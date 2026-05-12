/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * IPC: dispatch a `keyman:select?path=...` URL to the running Keyman
 * IMK process via LaunchServices (`open(1)`).
 *
 * The IMK app delegate registers as the `keyman:` URL handler in
 * `KMInputMethodAppDelegate -initCompletion` (Apple Events:
 * `kInternetEventClass:kAEGetURL`). We extend its `processURL:` to
 * understand a new `select` action with a `path` query parameter (see
 * the matching commit in `mac/Keyman4MacIM/`).
 *
 * URL form chosen to match the existing convention in `processURL:`:
 * `keyman:select?path=<percent-encoded canonical id>`. The leading
 * `keyman:` (single colon, no `//`) survives the rewrite step in the
 * existing handler and lands as a clean `keyman/select?path=...` for
 * `NSURL` to parse.
 */

use std::process::Command;

use crate::error::{CliError, Result};
use crate::keyboard::KeyboardId;

pub fn send_select(id: &KeyboardId) -> Result<()> {
    let url = format!("keyman:select?path={}", percent_encode_path(id.as_str()));
    let output = Command::new("/usr/bin/open").arg("-g").arg(&url).output()?;
    if !output.status.success() {
        return Err(CliError::UrlDispatchFailed {
            reason: String::from_utf8_lossy(&output.stderr).into_owned(),
            status: output.status.code(),
        });
    }
    Ok(())
}

fn percent_encode_path(s: &str) -> String {
    use std::fmt::Write as _;
    let mut out = String::with_capacity(s.len() * 3);
    for byte in s.as_bytes() {
        match *byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(*byte as char);
            }
            other => {
                let _ = write!(out, "%{other:02X}");
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percent_encoding_handles_slashes() {
        assert_eq!(
            percent_encode_path("/sil_euro_latin/sil_euro_latin.kmx"),
            "%2Fsil_euro_latin%2Fsil_euro_latin.kmx"
        );
    }

    #[test]
    fn percent_encoding_keeps_unreserved() {
        assert_eq!(percent_encode_path("abc-_~.0"), "abc-_~.0");
    }
}
