/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Typed errors for the Keyman CLI. We use `thiserror` for the library
 * surface; the binary turns these into user-readable stderr text and an
 * appropriate exit code in `main.rs`.
 *
 * Error variants are deliberately platform-agnostic so the same enum can
 * be returned by future Windows / Linux backends.
 */

use std::path::PathBuf;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, CliError>;

#[derive(Debug, Error)]
pub enum CliError {
    /// Keyman's preferences domain does not exist on this user account.
    /// Usually means Keyman was never installed or never launched.
    #[error("Keyman preferences not found on this account (the {domain} defaults domain is missing). Has Keyman ever been launched?")]
    PreferencesDomainMissing { domain: String },

    /// The user-supplied keyboard identifier did not match any active keyboard.
    #[error("Unknown keyboard: '{input}'. Run `keyman list` to see available keyboards.")]
    UnknownKeyboard { input: String },

    /// The user-supplied identifier matched more than one active keyboard.
    #[error("Ambiguous keyboard '{input}'. Matches: {}. Pass the canonical id (e.g. `/<package>/<file>.kmx`) to disambiguate.", .matches.join(", "))]
    AmbiguousKeyboard { input: String, matches: Vec<String> },

    /// The resolved keyboard is on disk but not in `KMActiveKeyboardsKey`.
    /// The CLI does not (yet) install or enable keyboards.
    #[error("Keyboard '{resolved}' is not in the active keyboards list. The CLI cannot enable disabled keyboards yet; enable it from Keyman's Configuration window first.")]
    KeyboardNotActive { resolved: String },

    /// Keyman input source could not be found in the Text Input Source list.
    #[error(
        "Keyman input source ('{bundle_id}') is not installed on this system; cannot activate."
    )]
    InputMethodNotInstalled { bundle_id: String },

    /// A Carbon `TIS*` call returned a non-zero `OSStatus`.
    #[error("Text Input Source API error in {op}: OSStatus {status}")]
    Carbon { op: &'static str, status: i32 },

    /// We could not get the running Keyman process to acknowledge the new
    /// selection within the verification window. Either the IPC handler
    /// is not present (older Keyman) or the process is not responding.
    #[error("Selected '{expected}' but Keyman did not switch keyboards within {timeout_ms}ms. The currently-selected keyboard is {}. This usually means the running Keyman.app does not yet support the `keyman:select` URL action; rebuild the IM and reinstall.", .actual.as_deref().unwrap_or("unknown"))]
    SelectVerificationFailed {
        expected: String,
        actual: Option<String>,
        timeout_ms: u64,
    },

    /// Failed to parse a kmp.json package descriptor.
    #[error("Failed to read package metadata at {path}: {reason}")]
    PackageMetadataParse { path: PathBuf, reason: String },

    /// `open(1)` returned a non-zero exit while dispatching the URL.
    #[error("Failed to dispatch URL to Keyman (`open` exited with status {status:?}): {reason}")]
    UrlDispatchFailed { reason: String, status: Option<i32> },

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl CliError {
    /// CLI exit code for this error. Documented in the README.
    pub fn exit_code(&self) -> i32 {
        match self {
            CliError::UnknownKeyboard { .. } => 3,
            CliError::AmbiguousKeyboard { .. } => 4,
            CliError::KeyboardNotActive { .. } => 5,
            CliError::PreferencesDomainMissing { .. } => 6,
            CliError::InputMethodNotInstalled { .. } => 7,
            CliError::Carbon { .. } => 8,
            CliError::SelectVerificationFailed { .. } => 9,
            CliError::UrlDispatchFailed { .. } => 10,
            CliError::PackageMetadataParse { .. } => 11,
            CliError::Io(_) => 1,
        }
    }
}
