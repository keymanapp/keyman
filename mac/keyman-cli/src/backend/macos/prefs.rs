/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Read Keyman's `NSUserDefaults` from another process via the
 * `CFPreferences*` C API.
 *
 * The Keyman input-method process persists its current selection and
 * its active-keyboards list under the bundle id
 * `keyman.inputmethod.Keyman`. The CLI reads the same domain to
 * answer `list` / `status` / to verify `select` round-trips, and
 * never writes — the IMK process owns the writes via its IPC handler.
 *
 * Notes on volatility: the running IMK process keeps an in-memory
 * cache of `KMSelectedKeyboardKey` and does not observe changes
 * (verified at mac/Keyman4MacIM/Keyman4MacIM/KMInputMethodAppDelegate.m;
 * no `addObserver:` on user-defaults / `NSUserDefaultsDidChange…`).
 * After a successful `select` IPC, we still re-read via
 * `CFPreferencesAppSynchronize` to make sure our own process picks up
 * the latest on-disk value.
 */

use core_foundation::array::CFArray;
use core_foundation::base::{CFType, CFTypeRef, TCFType};
use core_foundation::propertylist::CFPropertyList;
use core_foundation::string::{CFString, CFStringRef};
use core_foundation_sys::base::Boolean;

use crate::error::{CliError, Result};
use crate::keyboard::KeyboardId;

const KEY_SELECTED: &str = "KMSelectedKeyboardKey";
const KEY_ACTIVE: &str = "KMActiveKeyboardsKey";
const KEY_DATA_MODEL_VERSION: &str = "KMDataModelVersion";

extern "C" {
    fn CFPreferencesCopyAppValue(key: CFStringRef, application_id: CFStringRef) -> CFTypeRef;
    fn CFPreferencesAppSynchronize(application_id: CFStringRef) -> Boolean;
}

pub fn synchronize(bundle_id: &str) {
    let app = CFString::new(bundle_id);
    unsafe {
        let _ = CFPreferencesAppSynchronize(app.as_concrete_TypeRef());
    }
}

pub fn settings_exist(bundle_id: &str) -> bool {
    copy_pref(bundle_id, KEY_DATA_MODEL_VERSION).is_some()
        || copy_pref(bundle_id, KEY_ACTIVE).is_some()
        || copy_pref(bundle_id, KEY_SELECTED).is_some()
}

pub fn read_selected_keyboard(bundle_id: &str) -> Result<Option<String>> {
    match copy_pref(bundle_id, KEY_SELECTED) {
        Some(value) => {
            if let Some(s) = value.downcast::<CFString>() {
                Ok(Some(s.to_string()))
            } else {
                Ok(None)
            }
        }
        None => {
            if settings_exist(bundle_id) {
                Ok(None)
            } else {
                Err(CliError::PreferencesDomainMissing {
                    domain: bundle_id.to_string(),
                })
            }
        }
    }
}

pub fn read_active_keyboards(bundle_id: &str) -> Result<Vec<KeyboardId>> {
    let Some(value) = copy_pref(bundle_id, KEY_ACTIVE) else {
        if settings_exist(bundle_id) {
            return Ok(Vec::new());
        }
        return Err(CliError::PreferencesDomainMissing {
            domain: bundle_id.to_string(),
        });
    };

    let mut out: Vec<KeyboardId> = Vec::new();
    if let Some(arr) = value.downcast::<CFArray>() {
        for raw in arr.iter() {
            let raw_ptr: CFTypeRef = *raw;
            if raw_ptr.is_null() {
                continue;
            }
            let cf = unsafe { CFType::wrap_under_get_rule(raw_ptr) };
            if let Some(s) = cf.downcast::<CFString>() {
                out.push(KeyboardId::from_canonical(s.to_string()));
            }
        }
    }
    Ok(out)
}

fn copy_pref(bundle_id: &str, key: &str) -> Option<CFPropertyList> {
    let cf_key = CFString::new(key);
    let cf_app = CFString::new(bundle_id);
    unsafe {
        let raw =
            CFPreferencesCopyAppValue(cf_key.as_concrete_TypeRef(), cf_app.as_concrete_TypeRef());
        if raw.is_null() {
            None
        } else {
            Some(CFPropertyList::wrap_under_create_rule(raw))
        }
    }
}
