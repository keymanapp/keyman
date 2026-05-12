/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Library entry point for the Keyman CLI crate. Re-exports the public
 * surface used by the binary and by integration tests.
 */

#![forbid(unsafe_op_in_unsafe_fn)]

pub mod backend;
pub mod client;
pub mod error;
pub mod keyboard;
pub mod output;
pub mod resolver;

pub use client::KeymanClient;
pub use error::{CliError, Result};
pub use keyboard::{ActivateOutcome, ImState, Keyboard, KeyboardId, SelectOutcome, Status};
pub use resolver::{resolve_keyboard, ResolveError, ResolveOk};
