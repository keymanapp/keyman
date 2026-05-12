/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Compile-time backend dispatch. Each supported target OS provides one
 * `KeymanClient` implementation; `new_client()` returns the right one
 * for the current build target.
 */

#[cfg(target_os = "macos")]
pub mod macos;

#[cfg(not(target_os = "macos"))]
pub mod unsupported;

use crate::client::KeymanClient;

#[cfg(target_os = "macos")]
pub fn new_client() -> Box<dyn KeymanClient> {
    Box::new(macos::MacOsClient::new())
}

#[cfg(not(target_os = "macos"))]
pub fn new_client() -> Box<dyn KeymanClient> {
    Box::new(unsupported::UnsupportedClient::new())
}
