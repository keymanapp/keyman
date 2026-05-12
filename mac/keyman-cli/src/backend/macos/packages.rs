/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Read keyboard display names from `kmp.json` package descriptors.
 *
 * Each package lives at
 * `~/Library/Application Support/keyman.inputmethod.Keyman/Keyman-Keyboards/<package>/`.
 * The descriptor `kmp.json` contains a `keyboards` array; each entry
 * carries an `id` (matches the .kmx stem) and a `name` (the human-
 * readable display name shown in Keyman's menu). If we cannot read or
 * parse the file we fall through silently and the caller uses the
 * keyboard stem as a fallback — this is a "best effort" path, not a
 * correctness path.
 */

use std::fs;
use std::path::Path;

use serde::Deserialize;

use crate::keyboard::KeyboardId;

#[derive(Debug, Deserialize)]
struct KmpJson {
    #[serde(default)]
    keyboards: Vec<KmpKeyboard>,
    #[serde(default)]
    info: Option<KmpInfo>,
}

#[derive(Debug, Deserialize)]
struct KmpKeyboard {
    #[serde(default)]
    id: Option<String>,
    #[serde(default)]
    name: Option<String>,
}

#[derive(Debug, Deserialize)]
struct KmpInfo {
    #[serde(default)]
    name: Option<KmpInfoEntry>,
}

#[derive(Debug, Deserialize)]
struct KmpInfoEntry {
    #[serde(default)]
    description: Option<String>,
}

pub fn display_name_for(id: &KeyboardId, root: &Path) -> Option<String> {
    let pkg = id.package()?;
    let stem = id.stem()?;
    let kmp_path = root.join(pkg).join("kmp.json");
    let bytes = fs::read(&kmp_path).ok()?;
    let parsed: KmpJson = serde_json::from_slice(&bytes).ok()?;
    let from_keyboards = parsed.keyboards.iter().find_map(|kb| {
        let kb_id = kb.id.as_deref()?;
        if kb_id.eq_ignore_ascii_case(stem) {
            kb.name.clone().filter(|n| !n.is_empty())
        } else {
            None
        }
    });
    from_keyboards.or_else(|| {
        parsed
            .info
            .and_then(|i| i.name)
            .and_then(|n| n.description)
            .filter(|s| !s.is_empty())
    })
}
