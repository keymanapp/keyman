/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Wire-format types for `--json` output. The shape of these structs is
 * the documented JSON schema; field names and types here are stable.
 * Adding fields is allowed; renaming or removing is a breaking change.
 */

use serde::Serialize;

use crate::keyboard::{ActivateOutcome, ImState, Keyboard, KeyboardId, SelectOutcome, Status};

#[derive(Debug, Serialize)]
pub struct KeyboardJson {
    pub id: String,
    pub name: String,
    pub package: String,
    pub selected: bool,
}

impl From<&Keyboard> for KeyboardJson {
    fn from(k: &Keyboard) -> Self {
        Self {
            id: k.id.as_str().to_string(),
            name: k.name.clone(),
            package: k.package.clone(),
            selected: k.selected,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ListJson {
    pub keyboards: Vec<KeyboardJson>,
}

#[derive(Debug, Serialize)]
pub struct StatusJson {
    pub im_registered: bool,
    pub im_selected: bool,
    pub im_process_running: bool,
    pub selected_keyboard: Option<KeyboardJson>,
}

impl StatusJson {
    pub fn from_status(s: &Status) -> Self {
        Self {
            im_registered: s.im_state.im_registered,
            im_selected: s.im_state.im_selected,
            im_process_running: s.im_state.im_process_running,
            selected_keyboard: s.selected_keyboard.as_ref().map(KeyboardJson::from),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct SelectJson {
    pub selected: KeyboardJson,
    pub im_activated: bool,
    pub previous_selection: Option<String>,
}

impl From<&SelectOutcome> for SelectJson {
    fn from(o: &SelectOutcome) -> Self {
        Self {
            selected: KeyboardJson::from(&o.selected),
            im_activated: o.im_activated,
            previous_selection: o.previous_selection.as_ref().map(KeyboardId::to_string),
        }
    }
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Serialize)]
pub struct ActivateJson {
    pub im_registered_before: bool,
    pub im_selected_before: bool,
    pub im_registered_after: bool,
    pub im_selected_after: bool,
    pub changed: bool,
}

impl From<&ActivateOutcome> for ActivateJson {
    fn from(o: &ActivateOutcome) -> Self {
        let changed = (o.im_registered_before != o.im_registered_after)
            || (o.im_selected_before != o.im_selected_after);
        Self {
            im_registered_before: o.im_registered_before,
            im_selected_before: o.im_selected_before,
            im_registered_after: o.im_registered_after,
            im_selected_after: o.im_selected_after,
            changed,
        }
    }
}

pub fn format_im_state(s: ImState) -> String {
    format!(
        "input-method: registered={} selected={} process-running={}",
        yn(s.im_registered),
        yn(s.im_selected),
        yn(s.im_process_running)
    )
}

fn yn(b: bool) -> &'static str {
    if b {
        "yes"
    } else {
        "no"
    }
}
