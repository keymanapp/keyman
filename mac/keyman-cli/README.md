# `keyman` — command-line client for Keyman (macOS, stage 1)

A standalone Rust CLI for controlling Keyman from the shell. Stage 1
supports **macOS 14+** only; the crate is shaped so that Windows (TSF)
and Linux (ibus / fcitx) backends can land later behind the same
`KeymanClient` trait without an API break.

```
keyman list                      list installed Keyman keyboards
keyman status                    inspect Keyman's input-method state
keyman activate                  make Keyman the active OS input method
keyman select <keyboard>         switch Keyman to a specific keyboard
                                 (also activates Keyman if needed)
```

Each command accepts `--json` for machine-readable output (see
[JSON schema](#json-schema) below).

## What's in scope for stage 1

| In scope | Out of scope (future PRs) |
|---|---|
| Read `KMSelectedKeyboardKey` / `KMActiveKeyboardsKey` | Install / `enable` / `disable` keyboards |
| TIS-API activation of Keyman as the OS input source | Embedding the CLI in `Keyman.app` |
| Live keyboard switching via the existing `keyman://` URL scheme | Build-pipeline / `.pkg` / signing changes |
| Display names from `kmp.json` | AppleScript scripting dictionary |
| `--json` outputs with a documented stable schema | Windows / Linux backends |

## Build & install

```bash
cd mac/keyman-cli
cargo build --release
# binary is at target/release/keyman
```

The macOS system linker emits an ad-hoc Mach-O signature by default,
which is sufficient for local execution on Apple Silicon — **no
explicit `codesign` step is required**. There is no installer
integration in stage 1; the binary is meant to be run from
`target/release/keyman` or symlinked manually wherever the user
prefers.

MSRV: Rust 1.75.

## Prerequisites at run-time

* Keyman 18.x or 19.x installed (the IM bundle id must resolve to
  `keyman.inputmethod.Keyman`).
* `keyman select` requires a Keyman build that understands the
  `keyman:select?path=...` URL action. The matching IMK source change
  ships in the same PR as this CLI; older Keyman installs will accept
  the URL but ignore it, and `keyman select` will exit `9`
  (`SelectVerificationFailed`) after a 1.5 s grace window.

## IPC design for `select` (and why)

`activate` runs entirely from the CLI process via the Carbon TIS API.
`select` is harder: the running IMK process keeps the currently-loaded
keyboard cached in memory and does **not** observe `NSUserDefaults`
changes (verified in
`mac/Keyman4MacIM/Keyman4MacIM/KMInputMethodAppDelegate.m`). Some form
of IPC is therefore required for a live switch without a Keyman
restart.

The options we considered:

| Option | Verdict | Reason |
|---|---|---|
| **Extend `keyman://` URL scheme** | ✅ chosen | Keyman already registers `keyman:` (`Info.plist` `CFBundleURLSchemes`) and dispatches Apple Events via `kAEGetURL` in `initCompletion`. One new `else if` in `processURL:` reuses the existing surface; LaunchServices handles the app-not-running case for free. |
| Distributed notifications (`NSDistributedNotificationCenter`) | Rejected | Would need a new observer in the AppDelegate plus a new notification name; doesn't compose with LaunchServices auto-launch when Keyman is not running. |
| XPC service | Rejected | Robust but expensive: new bundle target, entitlements, signing changes — overkill for v1. Worth revisiting if a richer remote-control surface emerges. |
| Mach port + bootstrap server | Rejected | Same overkill concern, lower-level than XPC. |
| Apple Events / AppleScript | Rejected | No `.sdef` currently shipped; would need a scripting dictionary. |
| Write the pref + signal the IMK to re-read | Rejected | Fragile; entangles two mechanisms. |
| Accessibility-API menu click | Rejected | Would require the user to grant the CLI Accessibility permission — terrible UX. |

The matching IMK-side change is intentionally minimal (~25 lines in
`processURL:`): parse the `path` query parameter, look up the matching
index in `self.activeKeyboards`, and dispatch
`selectKeyboardFromMenu:` on the main queue — i.e. the exact same code
path the user triggers from Keyman's menu bar.

## Exit codes

| Code | Meaning |
|---|---|
| 0 | Success |
| 1 | Generic I/O or unexpected error (see stderr) |
| 2 | Usage / argument parse error (emitted by clap) |
| 3 | Unknown keyboard id |
| 4 | Ambiguous keyboard id (matches multiple active keyboards) |
| 5 | Keyboard is on disk but not in the active list — enable it from Keyman's Configuration window first |
| 6 | Keyman preferences domain missing (Keyman never launched on this user) |
| 7 | Keyman input source is not installed in macOS Text Input Sources |
| 8 | Carbon TIS API returned a non-zero `OSStatus` |
| 9 | `select` completed but the running Keyman did not switch within 1.5 s (usually means the running Keyman lacks the matching IMK patch — rebuild and reinstall the input method) |
| 10 | LaunchServices `open(1)` failed to dispatch the IPC URL |
| 11 | `kmp.json` package descriptor failed to parse |

## Keyboard-id resolution

`keyman select <keyboard>` accepts any of:

* canonical id: `/sil_euro_latin/sil_euro_latin.kmx`
* filename with `.kmx`: `sil_euro_latin.kmx`
* filename without `.kmx`: `sil_euro_latin`
* `package/keyboard[.kmx]`: `sil_euro_latin/sil_euro_latin`

Resolution rules (case-insensitive):

1. Exact canonical-id match wins.
2. Otherwise, `package/keyboard[.kmx]` matches if exactly one active
   keyboard has that package and stem.
3. Otherwise, filename/stem matches if exactly one active keyboard
   has that filename.
4. Multiple matches at the same priority → exit 4 with a list of
   candidates and a "specify the canonical id to disambiguate"
   message.
5. No match at any priority → exit 3.

## JSON schema

`--json` output is stable. Adding fields is non-breaking; renaming or
removing fields is a breaking change and will bump the crate's major
version.

### `keyman list --json`

```json
{
  "keyboards": [
    {
      "id": "/sil_euro_latin/sil_euro_latin.kmx",
      "name": "EuroLatin (SIL)",
      "package": "sil_euro_latin",
      "selected": true
    }
  ]
}
```

* `id` — canonical id, the path string also stored in
  `KMActiveKeyboardsKey`.
* `name` — display name from `kmp.json`'s `keyboards[].name`, falling
  back to the keyboard stem if `kmp.json` is missing or unparseable.
* `package` — the `<package>` segment of the id.
* `selected` — true if this is the keyboard pointed to by
  `KMSelectedKeyboardKey`.

### `keyman status --json`

```json
{
  "im_registered": true,
  "im_selected": false,
  "im_process_running": true,
  "selected_keyboard": {
    "id": "/sil_euro_latin/sil_euro_latin.kmx",
    "name": "EuroLatin (SIL)",
    "package": "sil_euro_latin",
    "selected": true
  }
}
```

* `im_registered` — Keyman is enabled in `AppleEnabledInputSources`
  under `com.apple.HIToolbox`.
* `im_selected` — Keyman is the currently active OS-level input
  source.
* `im_process_running` — the IMK server process is alive.
* `selected_keyboard` — `null` if no keyboard is selected, otherwise
  the same shape as a `list` entry.

### `keyman activate --json`

```json
{
  "im_registered_before": true,
  "im_selected_before": false,
  "im_registered_after": true,
  "im_selected_after": true,
  "changed": true
}
```

The four `_before` / `_after` fields make the actual state transition
auditable. `changed` is true if either pair differs.

### `keyman select <id> --json`

```json
{
  "selected": {
    "id": "/qpolish/qpolish.kmx",
    "name": "Quick Polish",
    "package": "qpolish",
    "selected": true
  },
  "im_activated": false,
  "previous_selection": "/sil_euro_latin/sil_euro_latin.kmx"
}
```

* `selected` — the keyboard now selected (same shape as `list`).
* `im_activated` — true if `select` also flipped Keyman to be the
  active OS-level input source (i.e. it wasn't already).
* `previous_selection` — canonical id of the previously-selected
  keyboard, or `null` if none.

## Cross-platform plan

`KeymanClient` (see `src/client.rs`) is the seam between the
dispatcher and per-OS backends. The trait and the value types
(`Keyboard`, `KeyboardId`, `Status`, `ImState`, `ActivateOutcome`,
`SelectOutcome`) are deliberately platform-neutral — for example
`im_registered` rather than "in `AppleEnabledInputSources`".

Future backends:

* **Windows.** Selection state lives in the registry / TSF profiles.
  `activate` maps to TSF `ITfInputProcessorProfileMgr::ActivateProfile`,
  `select` likely to a profile GUID exchange with the running Keyman
  Engine. No-op for stage 1.
* **Linux.** `ibus` and `fcitx` both expose D-Bus interfaces; the trait
  maps cleanly onto them. The current `keyman://` URL approach has no
  direct analogue, but D-Bus signals or a small named socket inside
  the Keyman engine process would fit. No-op for stage 1.

Non-macOS targets compile against `backend/unsupported.rs`, which
returns a clear "not yet implemented on this platform" error.

## Smoke test

See the matching PR description for the full transcript. The CLI
covers everything that can be verified through pure shell calls
(`list`, `status`, `activate`, error paths, JSON shape). Live keyboard
switching in `TextEdit` and the menu-bar checkmark move are visual
checks that need a human; the PR includes a copy-pasteable script for
those steps.
