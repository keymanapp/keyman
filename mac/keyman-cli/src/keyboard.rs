/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Platform-independent value types shared by every backend.
 *
 * The naming is deliberately neutral so that the same types fit Windows
 * TSF and Linux ibus / fcitx without rewording. For example we use
 * `im_registered` / `im_selected` rather than macOS-specific verbiage.
 */

use std::fmt;

/// Canonical Keyman keyboard identifier.
///
/// Format: a relative path starting with `/` of the form
/// `/<package>/<filename>.kmx`. This mirrors exactly the strings stored
/// in `KMActiveKeyboardsKey` / `KMSelectedKeyboardKey` in Keyman's
/// preferences, which is the form recognised by Keyman's IPC entry
/// points. All other input forms (filename only, package/keyboard,
/// etc.) are *resolved* to this canonical form before any backend call.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct KeyboardId(String);

impl KeyboardId {
    /// Wrap a string as a canonical id. The caller is responsible for
    /// having already canonicalised it (e.g. via the resolver). This
    /// constructor exists purely so backends can mint ids when reading
    /// from preferences.
    pub fn from_canonical(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The `<package>` segment of the id, useful for locating the
    /// containing `kmp.json` on disk.
    pub fn package(&self) -> Option<&str> {
        let trimmed = self.0.strip_prefix('/')?;
        let (pkg, _) = trimmed.split_once('/')?;
        if pkg.is_empty() {
            None
        } else {
            Some(pkg)
        }
    }

    /// The `<filename>.kmx` segment.
    pub fn filename(&self) -> Option<&str> {
        let trimmed = self.0.strip_prefix('/')?;
        let (_, file) = trimmed.split_once('/')?;
        if file.is_empty() {
            None
        } else {
            Some(file)
        }
    }

    /// The `<filename>` segment without `.kmx`.
    pub fn stem(&self) -> Option<&str> {
        self.filename().map(|f| f.strip_suffix(".kmx").unwrap_or(f))
    }
}

impl fmt::Display for KeyboardId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl AsRef<str> for KeyboardId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// A single keyboard surfaced by `keyman list`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Keyboard {
    pub id: KeyboardId,
    pub name: String,
    pub package: String,
    pub selected: bool,
}

/// Combined picture of input-method state on the host OS.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Status {
    pub im_state: ImState,
    pub selected_keyboard: Option<Keyboard>,
}

/// Input-method-level state. Field meanings:
///
/// * `im_registered` — Keyman is enabled in the OS as a usable input
///   source (macOS: present in `AppleEnabledInputSources`; Windows TSF:
///   listed in registered text services; Linux: present in
///   `ibus list-engine` or fcitx equivalent).
/// * `im_selected` — Keyman is currently the active input source.
/// * `im_process_running` — the platform's input-method server process
///   for Keyman is alive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImState {
    pub im_registered: bool,
    pub im_selected: bool,
    pub im_process_running: bool,
}

/// What `activate` did. Backends report all transitions they performed
/// so that `--json` consumers can verify exactly what changed.
#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ActivateOutcome {
    pub im_registered_before: bool,
    pub im_selected_before: bool,
    pub im_registered_after: bool,
    pub im_selected_after: bool,
}

impl ActivateOutcome {
    pub fn is_noop(self) -> bool {
        self.im_registered_before
            && self.im_selected_before
            && self.im_registered_after
            && self.im_selected_after
    }
}

/// What `select` did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectOutcome {
    pub selected: Keyboard,
    pub im_activated: bool,
    pub previous_selection: Option<KeyboardId>,
}
