# Raw input delivery and tag survival — measured 2026-08-28

**This is a measurement.** No production code changed to obtain it. It is the only
evidence for the claim the user-held-modifier signal rests on: that a `WM_INPUT`
feed on the serializer's own worker thread keeps reporting while the low level
hook does not, and that Keyman's own tag can still be read off those records.

Captured with [`host32/rawinput-probe.cpp`](../host32/rawinput-probe.cpp).

| | |
|---|---|
| OS | Windows 10.0 build 26200 |
| probe bitness | 32-bit |
| session is remote (`SM_REMOTESESSION`) | no |
| runs | 16:09:45, 16:10:24 and 16:36:01 local, 2026-08-28 |
| `keyman.exe` running | yes, Session 1 / Console |

## The policy the probe was written to test

The discriminator is **tag equality** — identical to
`IsKeymanInjectedKeyEvent(MakeCode, ExtraInformation)` — and never `hDevice` or an
injected-versus-physical filter. That is refuted before the run rather than after
it, because genuine user input from Remote Desktop and from the Keyman on-screen
keyboard arrives as OS-injected events: an injection filter would silently drop a
modifier the user is really holding. The probe records the `hDevice` column for
completeness, and the function that ships does not take an `hDevice` argument, so
the refuted policy is not expressible.

## The measured legs

| leg | result |
|---|---|
| registration: `RIDEV_INPUTSINK` against a message-only window on a **worker** thread | **succeeds**, `GetLastError` 0 |
| `WM_INPUT` delivered while the process is **unfocused** | **DELIVERED** — 164 raw keyboard records, foreground owned by another process throughout |
| `WM_INPUT` delivered while the **main thread is stalled** | **DELIVERED** — 42 records during an 8000 ms busy-block, with `fakefreeze` stalling `keyman.exe`'s main thread as well |
| the tag survives `SendInput` → raw input, scan `0xFF` | **survives** — `ExtraInformation` `0x4B4D0001`, `MakeCode` `0x00FF` |
| the tag survives for the Right Shift shape | **survives** — `ExtraInformation` `0x4B4D0001`, `MakeCode` `0x0036` |

### Why the stalled leg is the load-bearing one

The low level hook marshals every event to the thread that installed it —
`keyman.exe`'s **main** thread (`keyman32.cpp:275-280`) — which is the thread whose
stall causes Windows to evict the hook. Raw input queues on the **registering**
thread instead. 42 records arriving during an 8000 ms block of that main thread,
with `fakefreeze` holding `keyman.exe` as well, is a direct measurement that the
feed survives exactly the window in which the hook does not. It is not an argument
from documentation.

### Why the Right Shift row matters

`do_keybd_event` overwrites `SCAN_FLAG_KEYMAN_KEY_EVENT` with `SCANCODE_RSHIFT`
for `VK_RSHIFT`, so an injected Right Shift is byte-identical at the scan code to a
physical one. Row (c) is that shape, and the tag came back intact: the
`dwExtraInfo` arm is the only cover for it, on the raw-input path as on the hook
path.

## Carried forward as unverified, with reasons

Recorded as unrun, with the reason, and never written up as a pass.

- **The physical-keystroke column baseline.** Not measured: no physical keystroke
  arrived inside the wait window on the decisive capture, and the row is left
  blank rather than inferred. It is not a clause of the decision rule — the rule
  asks whether the tag survives `SendInput` to raw input, and the two shapes that
  answer it are injected by construction. Two things stand in for it: the delivery
  legs above carried **206 physical-key records** in total, so physical delivery to
  the message-only worker window is measured rather than assumed, and
  `IS_KEYMAN_INJECTED_KEY_EVENT.PhysicalKeystrokesAreNotKeymans` pins the
  classification side in the unit suite.
- **Remote Desktop.** Deliberately not run — operator decision. Admissible because
  the policy is tag equality: RDP input carries mstsc's own `dwExtraInfo` and is
  classified by the same test as any other input, so there is no RDP-specific code
  path to get wrong. `IS_KEYMAN_INJECTED_KEY_EVENT.RemoteDesktopInputIsNotKeymans`
  pins that population in the suite. **This leg is unverified, not satisfied**, and
  it is why `TEST_REMOTE_DESKTOP` is in the user testing list.
- **Keyman on-screen keyboard, higher-integrity focus, secure desktop.** Not run in
  this pass. `TheOnScreenKeyboardIsNotKeymans` pins the OSK population in the
  suite, and the secure-desktop case is the empirical counterpart to the "active
  desktop is not the user's → poison" rule, which the suite tests. The requirement
  is not unpinned, but the *field* confirmation is outstanding.

## Verdict

The decision rule was written before the run so the result could not be
rationalised afterwards. Both of its clauses are measured YES: the tag survives to
raw input in both shapes, and `WM_INPUT` reaches a message-only worker-thread
window both unfocused and with the main thread stalled.

Neither refutation branch applies. Delivery did not fail in the stalled or
unfocused case, and the tag was not zeroed or rewritten on the raw-input path.
