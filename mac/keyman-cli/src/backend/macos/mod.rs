/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * macOS backend for the Keyman CLI. Glues together four subsystems:
 *
 *   - `prefs`   reads/writes Keyman's `NSUserDefaults` domain
 *               (`keyman.inputmethod.Keyman`).
 *   - `packages` reads `kmp.json` files on disk to map a keyboard id to
 *                its display name.
 *   - `tis`     drives Apple's Text Input Source Services API for the
 *               OS-level enable / select operations.
 *   - `ipc`     dispatches a `keyman:select?...` URL to the running
 *               IMK process via LaunchServices.
 */

pub mod ipc;
pub mod packages;
pub mod prefs;
pub mod tis;

use std::path::PathBuf;
use std::thread::sleep;
use std::time::Duration;

use crate::client::KeymanClient;
use crate::error::{CliError, Result};
use crate::keyboard::{ActivateOutcome, Keyboard, KeyboardId, SelectOutcome, Status};

pub const KEYMAN_BUNDLE_ID: &str = "keyman.inputmethod.Keyman";

const SELECT_VERIFY_TIMEOUT_MS: u64 = 1500;
const SELECT_VERIFY_POLL_MS: u64 = 50;

pub struct MacOsClient {
    bundle_id: String,
}

impl MacOsClient {
    pub fn new() -> Self {
        Self {
            bundle_id: KEYMAN_BUNDLE_ID.to_string(),
        }
    }

    fn build_keyboard(id: KeyboardId, selected_id: Option<&str>) -> Keyboard {
        let pkg = id.package().unwrap_or("").to_string();
        let stem = id.stem().unwrap_or("").to_string();
        let name =
            packages::display_name_for(&id, &keyboards_root()).unwrap_or_else(|| stem.clone());
        let selected = selected_id == Some(id.as_str());
        Keyboard {
            id,
            name,
            package: pkg,
            selected,
        }
    }
}

impl Default for MacOsClient {
    fn default() -> Self {
        Self::new()
    }
}

impl KeymanClient for MacOsClient {
    fn list_keyboards(&self) -> Result<Vec<Keyboard>> {
        let active = prefs::read_active_keyboards(&self.bundle_id)?;
        let selected = prefs::read_selected_keyboard(&self.bundle_id)?;
        let mut out: Vec<Keyboard> = active
            .into_iter()
            .map(|id| Self::build_keyboard(id, selected.as_deref()))
            .collect();
        out.sort_by(|a, b| a.id.as_str().cmp(b.id.as_str()));
        Ok(out)
    }

    fn status(&self) -> Result<Status> {
        let im_state = tis::query_state(&self.bundle_id)?;
        let selected_id = prefs_read_optional(&self.bundle_id);
        let selected_keyboard = match selected_id {
            Some(id_str) if !id_str.is_empty() => {
                let active = prefs::read_active_keyboards(&self.bundle_id).unwrap_or_default();
                let id = KeyboardId::from_canonical(&id_str);
                if active.iter().any(|k| k.as_str() == id_str) {
                    Some(Self::build_keyboard(id, Some(&id_str)))
                } else {
                    Some(Keyboard {
                        id: id.clone(),
                        name: id.stem().unwrap_or("").to_string(),
                        package: id.package().unwrap_or("").to_string(),
                        selected: true,
                    })
                }
            }
            _ => None,
        };
        Ok(Status {
            im_state,
            selected_keyboard,
        })
    }

    fn activate(&self) -> Result<ActivateOutcome> {
        let before = tis::query_state(&self.bundle_id)?;
        let after = tis::enable_and_select(&self.bundle_id)?;
        Ok(ActivateOutcome {
            im_registered_before: before.im_registered,
            im_selected_before: before.im_selected,
            im_registered_after: after.im_registered,
            im_selected_after: after.im_selected,
        })
    }

    fn select_keyboard(&self, id: &KeyboardId) -> Result<SelectOutcome> {
        let active = prefs::read_active_keyboards(&self.bundle_id)?;
        if !active.iter().any(|k| k.as_str() == id.as_str()) {
            return Err(CliError::KeyboardNotActive {
                resolved: id.to_string(),
            });
        }

        let previous =
            prefs::read_selected_keyboard(&self.bundle_id)?.map(KeyboardId::from_canonical);

        let im_state_before = tis::query_state(&self.bundle_id)?;
        let im_activated = !(im_state_before.im_registered && im_state_before.im_selected);
        if im_activated {
            tis::enable_and_select(&self.bundle_id)?;
        }

        if previous.as_ref().map(KeyboardId::as_str) == Some(id.as_str()) && !im_activated {
            return Ok(SelectOutcome {
                selected: Self::build_keyboard(id.clone(), Some(id.as_str())),
                im_activated: false,
                previous_selection: previous,
            });
        }

        ipc::send_select(id)?;

        let deadline = std::time::Instant::now() + Duration::from_millis(SELECT_VERIFY_TIMEOUT_MS);
        loop {
            prefs::synchronize(&self.bundle_id);
            let last_seen = prefs::read_selected_keyboard(&self.bundle_id)?;
            if last_seen.as_deref() == Some(id.as_str()) {
                break;
            }
            if std::time::Instant::now() >= deadline {
                return Err(CliError::SelectVerificationFailed {
                    expected: id.to_string(),
                    actual: last_seen,
                    timeout_ms: SELECT_VERIFY_TIMEOUT_MS,
                });
            }
            sleep(Duration::from_millis(SELECT_VERIFY_POLL_MS));
        }

        Ok(SelectOutcome {
            selected: Self::build_keyboard(id.clone(), Some(id.as_str())),
            im_activated,
            previous_selection: previous,
        })
    }
}

fn prefs_read_optional(bundle_id: &str) -> Option<String> {
    prefs::read_selected_keyboard(bundle_id).ok().flatten()
}

fn keyboards_root() -> PathBuf {
    let mut p = dirs_home();
    p.push("Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards");
    p
}

fn dirs_home() -> PathBuf {
    std::env::var_os("HOME").map_or_else(|| PathBuf::from("/"), PathBuf::from)
}
