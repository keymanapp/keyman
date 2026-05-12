/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Fallback backend for non-macOS targets. Every operation returns the
 * same "not yet implemented" error so the crate still builds on Linux
 * and Windows (useful for CI on contributor machines that don't run
 * macOS). Real implementations will land in their own modules.
 */

use crate::client::KeymanClient;
use crate::error::{CliError, Result};
use crate::keyboard::{ActivateOutcome, Keyboard, KeyboardId, SelectOutcome, Status};

pub struct UnsupportedClient;

impl UnsupportedClient {
    pub fn new() -> Self {
        Self
    }
}

impl Default for UnsupportedClient {
    fn default() -> Self {
        Self::new()
    }
}

impl KeymanClient for UnsupportedClient {
    fn list_keyboards(&self) -> Result<Vec<Keyboard>> {
        Err(unsupported())
    }

    fn status(&self) -> Result<Status> {
        Err(unsupported())
    }

    fn activate(&self) -> Result<ActivateOutcome> {
        Err(unsupported())
    }

    fn select_keyboard(&self, _id: &KeyboardId) -> Result<SelectOutcome> {
        Err(unsupported())
    }
}

fn unsupported() -> CliError {
    CliError::Io(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "Keyman CLI: only macOS is supported in stage 1. Windows / Linux backends are planned.",
    ))
}
