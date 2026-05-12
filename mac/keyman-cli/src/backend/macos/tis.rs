/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Carbon Text Input Source Services bindings (just the pieces we use).
 *
 * The CLI drives the OS-level "enable + select" via this API; it is
 * the same mechanism `mac/setup/textinputsource/main.c` already uses
 * in this repo, except we filter on `kTISPropertyBundleID` (more
 * reliable than the localized display name). No special entitlements
 * are required; a normal user-session process can call these APIs.
 */

use core_foundation::array::{CFArray, CFArrayRef};
use core_foundation::base::{CFType, CFTypeRef, TCFType};
use core_foundation::boolean::CFBoolean;
use core_foundation::dictionary::{CFDictionary, CFDictionaryRef};
use core_foundation::string::{CFString, CFStringRef};
use core_foundation_sys::base::{Boolean, OSStatus};

use crate::error::{CliError, Result};
use crate::keyboard::ImState;

pub type TISInputSourceRef = CFTypeRef;

#[link(name = "Carbon", kind = "framework")]
extern "C" {
    fn TISCreateInputSourceList(
        properties: CFDictionaryRef,
        includeAllInstalled: Boolean,
    ) -> CFArrayRef;
    fn TISCopyCurrentKeyboardInputSource() -> TISInputSourceRef;
    fn TISGetInputSourceProperty(
        input_source: TISInputSourceRef,
        property_key: CFStringRef,
    ) -> CFTypeRef;
    fn TISEnableInputSource(input_source: TISInputSourceRef) -> OSStatus;
    fn TISSelectInputSource(input_source: TISInputSourceRef) -> OSStatus;

    static kTISPropertyBundleID: CFStringRef;
    static kTISPropertyInputSourceID: CFStringRef;
    static kTISPropertyInputSourceIsEnabled: CFStringRef;
}

pub fn query_state(bundle_id: &str) -> Result<ImState> {
    let sources = list_sources_for_bundle(bundle_id);
    if sources.is_empty() {
        return Ok(ImState {
            im_registered: false,
            im_selected: false,
            im_process_running: false,
        });
    }

    let im_registered = sources
        .iter()
        .any(|s| bool_prop(*s, unsafe { kTISPropertyInputSourceIsEnabled }));

    let current = current_source_bundle_id();
    let im_selected = current.as_deref() == Some(bundle_id);

    let im_process_running = im_selected || is_process_running(bundle_id);

    Ok(ImState {
        im_registered,
        im_selected,
        im_process_running,
    })
}

pub fn enable_and_select(bundle_id: &str) -> Result<ImState> {
    let sources = list_sources_for_bundle(bundle_id);
    if sources.is_empty() {
        return Err(CliError::InputMethodNotInstalled {
            bundle_id: bundle_id.to_string(),
        });
    }

    let main = pick_main_source(&sources, bundle_id);

    for src in &sources {
        if !bool_prop(*src, unsafe { kTISPropertyInputSourceIsEnabled }) {
            let status = unsafe { TISEnableInputSource(*src) };
            if status != 0 {
                return Err(CliError::Carbon {
                    op: "TISEnableInputSource",
                    status,
                });
            }
        }
    }

    let status = unsafe { TISSelectInputSource(main) };
    if status != 0 {
        return Err(CliError::Carbon {
            op: "TISSelectInputSource",
            status,
        });
    }

    let im_registered = sources
        .iter()
        .all(|s| bool_prop(*s, unsafe { kTISPropertyInputSourceIsEnabled }));
    let im_selected = current_source_bundle_id().as_deref() == Some(bundle_id);

    Ok(ImState {
        im_registered,
        im_selected,
        im_process_running: true,
    })
}

fn pick_main_source(sources: &[TISInputSourceRef], bundle_id: &str) -> TISInputSourceRef {
    if let Some(found) = sources.iter().find(|s| {
        let id = string_prop(**s, unsafe { kTISPropertyInputSourceID }).unwrap_or_default();
        id == bundle_id
    }) {
        return *found;
    }
    sources[0]
}

fn list_sources_for_bundle(bundle_id: &str) -> Vec<TISInputSourceRef> {
    let key = unsafe { CFString::wrap_under_get_rule(kTISPropertyBundleID) };
    let value = CFString::new(bundle_id);
    let pairs: Vec<(CFString, CFType)> = vec![(key.clone(), value.as_CFType())];
    let dict = CFDictionary::from_CFType_pairs(&pairs);

    unsafe {
        let raw = TISCreateInputSourceList(dict.as_concrete_TypeRef(), 1);
        if raw.is_null() {
            return Vec::new();
        }
        let arr = CFArray::<CFType>::wrap_under_create_rule(raw);
        let cap = usize::try_from(arr.len()).unwrap_or(0);
        let mut out: Vec<TISInputSourceRef> = Vec::with_capacity(cap);
        for elem in arr.iter() {
            out.push(elem.as_CFTypeRef());
        }
        std::mem::forget(arr);
        out
    }
}

fn current_source_bundle_id() -> Option<String> {
    unsafe {
        let raw = TISCopyCurrentKeyboardInputSource();
        if raw.is_null() {
            return None;
        }
        let id = string_prop(raw, kTISPropertyBundleID);
        core_foundation_sys::base::CFRelease(raw);
        id
    }
}

fn bool_prop(source: TISInputSourceRef, key: CFStringRef) -> bool {
    unsafe {
        let raw = TISGetInputSourceProperty(source, key);
        if raw.is_null() {
            return false;
        }
        let b = CFBoolean::wrap_under_get_rule(raw.cast());
        Into::<bool>::into(b)
    }
}

fn string_prop(source: TISInputSourceRef, key: CFStringRef) -> Option<String> {
    unsafe {
        let raw = TISGetInputSourceProperty(source, key);
        if raw.is_null() {
            return None;
        }
        let s = CFString::wrap_under_get_rule(raw.cast());
        Some(s.to_string())
    }
}

fn is_process_running(bundle_id: &str) -> bool {
    use std::process::Command;
    let output = Command::new("/usr/bin/pgrep")
        .arg("-f")
        .arg(format!(
            "{}.app/Contents/MacOS/",
            bundle_friendly_name(bundle_id)
        ))
        .output();
    match output {
        Ok(o) => o.status.success() && !o.stdout.is_empty(),
        Err(_) => false,
    }
}

fn bundle_friendly_name(bundle_id: &str) -> &str {
    bundle_id.rsplit('.').next().unwrap_or(bundle_id)
}
