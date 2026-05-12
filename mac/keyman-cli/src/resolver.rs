/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Free-form user-supplied keyboard identifier -> canonical
 * `/<package>/<file>.kmx` resolution.
 *
 * Resolution rules (mirrored in the README):
 *
 *   1. Exact canonical-id match wins. `/sil_euro_latin/sil_euro_latin.kmx`
 *      maps to itself if active.
 *   2. `package/keyboard[.kmx]` matches if exactly one active keyboard
 *      has that package and stem.
 *   3. Filename-only (with or without `.kmx`) matches if exactly one
 *      active keyboard has that filename.
 *   4. Otherwise the input is treated as a stem and matched against
 *      `<file>.kmx`-without-extension for each active keyboard.
 *
 * Multiple matches at the same priority level produce
 * `ResolveError::Ambiguous`. No match at any level produces
 * `ResolveError::Unknown`.
 */

use crate::keyboard::KeyboardId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    Unknown(String),
    Ambiguous {
        input: String,
        matches: Vec<KeyboardId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveOk {
    pub id: KeyboardId,
}

pub fn resolve_keyboard(input: &str, active: &[KeyboardId]) -> Result<ResolveOk, ResolveError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(ResolveError::Unknown(input.to_string()));
    }

    if let Some(hit) = active.iter().find(|k| k.as_str() == trimmed) {
        return Ok(ResolveOk { id: hit.clone() });
    }

    let normalized_with_kmx = ensure_kmx(trimmed);
    let normalized_no_kmx = strip_kmx(trimmed);

    if normalized_with_kmx.contains('/') {
        let needle_with = normalized_with_kmx.trim_start_matches('/');
        let matches: Vec<&KeyboardId> = active
            .iter()
            .filter(|id| {
                id.as_str()
                    .trim_start_matches('/')
                    .eq_ignore_ascii_case(needle_with)
            })
            .collect();
        match matches.len() {
            1 => {
                return Ok(ResolveOk {
                    id: matches[0].clone(),
                })
            }
            n if n > 1 => {
                return Err(ResolveError::Ambiguous {
                    input: input.to_string(),
                    matches: matches.into_iter().cloned().collect(),
                });
            }
            _ => {}
        }
    } else {
        let needle_filename = normalized_with_kmx.as_str();
        let needle_stem = normalized_no_kmx.as_str();

        let matches: Vec<&KeyboardId> = active
            .iter()
            .filter(|id| {
                id.filename()
                    .is_some_and(|f| f.eq_ignore_ascii_case(needle_filename))
                    || id
                        .stem()
                        .is_some_and(|s| s.eq_ignore_ascii_case(needle_stem))
            })
            .collect();
        match matches.len() {
            1 => {
                return Ok(ResolveOk {
                    id: matches[0].clone(),
                })
            }
            n if n > 1 => {
                return Err(ResolveError::Ambiguous {
                    input: input.to_string(),
                    matches: matches.into_iter().cloned().collect(),
                });
            }
            _ => {}
        }
    }

    Err(ResolveError::Unknown(input.to_string()))
}

fn ensure_kmx(s: &str) -> String {
    if has_kmx_suffix(s) {
        s.to_string()
    } else {
        format!("{s}.kmx")
    }
}

fn strip_kmx(s: &str) -> String {
    if has_kmx_suffix(s) {
        s[..s.len() - 4].to_string()
    } else {
        s.to_string()
    }
}

fn has_kmx_suffix(s: &str) -> bool {
    s.len() >= 4 && s[s.len() - 4..].eq_ignore_ascii_case(".kmx")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keyboard::KeyboardId;

    fn fixture() -> Vec<KeyboardId> {
        vec![
            KeyboardId::from_canonical("/sil_euro_latin/sil_euro_latin.kmx"),
            KeyboardId::from_canonical("/sil_ipa/sil_ipa.kmx"),
            KeyboardId::from_canonical("/qpolish/qpolish.kmx"),
            KeyboardId::from_canonical("/qrussian/qrussian.kmx"),
            KeyboardId::from_canonical("/cgreek/cgreek.kmx"),
            KeyboardId::from_canonical("/oldenglish/oldenglish.kmx"),
            KeyboardId::from_canonical("/hebrew_phonetic_arabic/hebrew_phonetic_arabic.kmx"),
        ]
    }

    #[test]
    fn canonical_match() {
        let r = resolve_keyboard("/sil_ipa/sil_ipa.kmx", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/sil_ipa/sil_ipa.kmx");
    }

    #[test]
    fn filename_with_extension() {
        let r = resolve_keyboard("qpolish.kmx", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/qpolish/qpolish.kmx");
    }

    #[test]
    fn filename_without_extension() {
        let r = resolve_keyboard("qpolish", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/qpolish/qpolish.kmx");
    }

    #[test]
    fn package_slash_keyboard_form() {
        let r = resolve_keyboard("sil_euro_latin/sil_euro_latin", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/sil_euro_latin/sil_euro_latin.kmx");
    }

    #[test]
    fn package_slash_keyboard_with_kmx() {
        let r = resolve_keyboard("cgreek/cgreek.kmx", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/cgreek/cgreek.kmx");
    }

    #[test]
    fn case_insensitive() {
        let r = resolve_keyboard("QPolish.KMX", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/qpolish/qpolish.kmx");
    }

    #[test]
    fn unknown_id_errors() {
        let err = resolve_keyboard("does-not-exist", &fixture()).unwrap_err();
        assert_eq!(err, ResolveError::Unknown("does-not-exist".to_string()));
    }

    #[test]
    fn empty_input_is_unknown() {
        let err = resolve_keyboard("   ", &fixture()).unwrap_err();
        match err {
            ResolveError::Unknown(_) => {}
            ResolveError::Ambiguous { .. } => panic!("expected Unknown, got Ambiguous"),
        }
    }

    #[test]
    fn ambiguous_collision_across_packages() {
        let active = vec![
            KeyboardId::from_canonical("/pkg_a/shared.kmx"),
            KeyboardId::from_canonical("/pkg_b/shared.kmx"),
        ];
        let err = resolve_keyboard("shared", &active).unwrap_err();
        match err {
            ResolveError::Ambiguous { input, matches } => {
                assert_eq!(input, "shared");
                assert_eq!(matches.len(), 2);
            }
            ResolveError::Unknown(_) => panic!("expected Ambiguous, got Unknown"),
        }
    }

    #[test]
    fn package_slash_keyboard_disambiguates() {
        let active = vec![
            KeyboardId::from_canonical("/pkg_a/shared.kmx"),
            KeyboardId::from_canonical("/pkg_b/shared.kmx"),
        ];
        let r = resolve_keyboard("pkg_a/shared", &active).unwrap();
        assert_eq!(r.id.as_str(), "/pkg_a/shared.kmx");
    }

    #[test]
    fn leading_slash_in_partial_id_accepted() {
        let r = resolve_keyboard("/qpolish/qpolish.kmx", &fixture()).unwrap();
        assert_eq!(r.id.as_str(), "/qpolish/qpolish.kmx");
    }

    #[test]
    fn unknown_when_package_does_not_exist() {
        let err = resolve_keyboard("foo/qpolish", &fixture()).unwrap_err();
        match err {
            ResolveError::Unknown(_) => {}
            ResolveError::Ambiguous { .. } => panic!("expected Unknown, got Ambiguous"),
        }
    }
}
