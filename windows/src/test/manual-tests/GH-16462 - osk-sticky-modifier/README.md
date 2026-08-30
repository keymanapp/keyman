# GH-16462: on-screen keyboard sticky modifier

Manual end-to-end test for the **on-screen keyboard half** of
[#16462](https://github.com/keymanapp/keyman/issues/16462), *bug: Closing /navigating OSK strands a sticky modifier*.
Related, but a different producer and a different fix: [#8064](https://github.com/keymanapp/keyman/issues/8064), *bug(windows): modifier key
occasionally is "stuck on"*.

A modifier "clicked sticky" on the on-screen keyboard is held down with a real,
chiral `keybd_event` KEYDOWN and **no matching KEYUP queued anywhere**. Nothing but
Keyman itself will ever send that KEYUP. So every way the OSK can fail to send it —
missing the cleanup path entirely, sending it for the wrong chiral key, or declining
to send it — leaves a modifier asserted **machine-wide**, in every application, until
the user presses that same physical key themselves or reboots. On a compact or 60%
layout with no physical Right Ctrl there is no in-session recovery at all.

## Why this is a manual test

There is no Delphi unit-test project anywhere under `windows/src/engine` — no DUnit,
no DUnitX, no `.dpr` test host. The engine's only automated tests are the C++
googletest projects under `windows/src/engine/keyman32/tests/`, and none of this
change is in C++. Everything below also depends on machine-wide modifier state and on
`keybd_event` reaching the whole desktop, which is not something an in-process test
harness can observe.

So this follows what the repo already does for
[`GH-140 - shift states`](../GH-140%20-%20shift%20states/) and
[`test_i3762 modifier tests`](../test_i3762%20modifier%20tests/): a documented manual
sequence, plus a **scripted oracle** so the result does not depend on the tester
typing anything (a stuck Ctrl makes the keyboard unusable, which is exactly the
moment measurement is needed), plus a **recorded red baseline** so the failures are
on the record rather than asserted.

## The oracle

[`watch-modifiers.ps1`](./watch-modifiers.ps1) polls the nine modifier virtual keys
every 60 ms with `GetAsyncKeyState` and logs only state *changes*, to
`osk-watch.txt` beside itself:

```powershell
powershell -ExecutionPolicy Bypass -File ".\watch-modifiers.ps1" -Minutes 45
```

It reads all nine VKs (`VK_SHIFT`, `VK_CONTROL`, `VK_MENU` and the six chiral ones),
not just the chiral six, because `do_keybd_event` injects the side-agnostic VK — a
phantom press is not necessarily an `0xA0..0xA5` event.

**Calibrate it before trusting a null result.** Press and release physical Left Shift
and confirm a `HELD: SHIFT,LSHIFT` / `ALL CLEAR` pair appears. An oracle that is not
running produces the same silence as a passing test.

For the cases that need to see the *injected event* rather than its effect, rebuild
`keyman.exe` with `{$DEFINE KLOGGING}` in `common/windows/delphi/general/klog.pas:26`
— shipped builds have the `$` missing, so `KL.Log` compiles away — and capture
`OutputDebugString` via the DBWIN protocol.

## Preconditions before each test

- A real Keyman for Windows install, running, with a **Keyman** keyboard active.
- The on-screen keyboard visible (Keyman menu → *On Screen Keyboard*).
- `watch-modifiers.ps1` running and calibrated.
- No modifier physically held at the start (`ALL CLEAR` in the log), except where a
  case says otherwise.
- For the chiral cases, a keyboard whose visual keyboard sets `<usealtgr/>`, so
  `UpdateKeyboard` sets `kbd.LRShift := True` and the OSK draws separate
  *L Ctrl* / *R Ctrl* keys. `sil_cameroon_qwerty` is the one used for the recorded
  baseline; `common/test/resources/keyboards/test_chirality.kmx` also works and
  additionally echoes which chiral modifier was live.

## Test cases

- **TEST_OSK-1**: cleanup is reached on every dismissal path
  - click `Ctrl` on the on-screen keyboard so it latches
  - confirm the oracle logs `HELD: CTRL,...`
  - dismiss the OSK from the Keyman tray menu — *not* from the OSK's own X button
  - repeat for: a tray icon double click; the Keyman hotkey; switching to the
    Character Map tab; and exiting Keyman entirely while the modifier is held

  Expected result:
  - `ALL CLEAR` in the oracle log after each dismissal
  - typing produces unmodified characters

  Observed on a shipped build: **FAILURE.** Every path except the OSK's own X button
  and the tab switch dismisses the visual keyboard with `Release`/`FreeAndNil`, which
  runs `OnDestroy` and never `OnClose` — and cleanup hung off `FormClose` alone. The
  oracle stays at `HELD: CTRL,...` indefinitely; Ctrl is held machine-wide.

- **TEST_OSK-2**: a sticky modifier is released with the chirality it was pressed with
  - with a `<usealtgr/>` keyboard active, click `R Ctrl` on the OSK so it latches
  - confirm the oracle logs `HELD: CTRL,RCTRL`
  - switch to a keyboard *without* `<usealtgr/>` (e.g.
    `common/test/keyboards/baseline/k_0301___multiple_deadkeys.kmx`), then switch back
  - click the `Ctrl` key on the OSK again, to toggle it off

  Expected result:
  - `ALL CLEAR`: the release goes out as `VK_RCONTROL` with `KEYEVENTF_EXTENDEDKEY`,
    the exact key that was pressed

  Observed on a shipped build: **FAILURE.** The keyboard switch runs `SetLRShift`,
  which collapses `essRCtrl` in `kbd.ShiftState` to the generic `essCtrl`, so the
  release branch — which took its VK from the *current* `kbd.LRShift` regime — emitted
  an unextended generic `VK_CONTROL` KEYUP. That does not clear an extended
  `VK_RCONTROL`. The oracle drops `CTRL` and keeps `RCTRL`. On hardware with no
  physical Right Ctrl key, nothing the user can type clears it.

- **TEST_OSK-3**: teardown never *presses* a modifier
  - click a modifier on the OSK so it latches
  - click the same modifier again to unlatch it
  - immediately — inside the 50 ms `UpdateShiftStates` resync window — dismiss the OSK
  - read the `KLOGGING` capture

  Expected result:
  - every `keybd_event` emitted by the teardown carries `KEYEVENTF_KEYUP`
    (`flags=2` or `flags=3`); no KEYDOWN appears

  Observed on a shipped build: **FAILURE.** `ResetShiftStates` routed its cleanup
  through `ShiftStateChange`'s `PrepState`, which emits a KEYDOWN whenever a modifier
  is in one shift-state set and not the other. A modifier-off click mutates
  `kbd.ShiftState` without touching `FShiftState`, so until the next resync tick
  equalised them the cleanup *pressed* a modifier — chiral, so potentially Right
  Control, with no KEYUP behind it. This is the phantom KEYDOWN, arriving from the
  very function that exists to clean up.

- **TEST_OSK-4**: teardown must not release a modifier the user is physically holding
  - press and **hold** physical Left Shift, and keep holding it throughout
  - click `R Ctrl` on the OSK so it latches; the oracle logs
    `HELD: SHIFT,CTRL,LSHIFT,RCTRL`
  - dismiss the OSK **with the mouse**, still holding Shift

  Expected result:
  - the oracle logs `HELD: SHIFT,LSHIFT` — Ctrl released, the user's Shift untouched
  - `ALL CLEAR` only once the tester releases Shift
  - typing before that release produces capitals

  Observed: **FAILURE** (measured 2026-08-27, see
  [`evidence/baseline-shipped-build.md`](./evidence/baseline-shipped-build.md)). The
  dismissal took *both*. `UpdateShiftStates` runs on a 50 ms timer and ends with
  `kbd.ShiftState := GetAsyncShiftState`, so `kbd.ShiftState` continuously carries
  physically-held modifiers; the cache was assigned from it wholesale, so a click made
  while Shift was held cached `essShift` alongside the key actually clicked, and
  teardown released the user's own Shift. This is I2177 recurring. A
  `GetAsyncKeyState` gate cannot catch it: a physically-held key genuinely *is* down,
  so the gate passes.

  Note the measurement protocol: pre-type the message and send it with the mouse. Two
  earlier attempts were *inconclusive*, not passes, because pressing Enter meant
  releasing Shift at the critical moment, and the log cannot then distinguish "the
  teardown released your Shift" from "the tester let go".

- **TEST_OSK-5**: Keyman's own injected Right Shift is not mistaken for the user's
  - click `Shift` on the OSK so it latches
  - type enough through a Keyman keyboard that the engine injects output several
    times — the serializer's release/restore halves inject `VK_RSHIFT`
  - dismiss the OSK

  Expected result:
  - `ALL CLEAR`; the clicked Shift is released

  Observed with a scan-code-based provenance test: **FAILURE.**
  `do_keybd_event` in `keyman32/keybd_shift.cpp` overwrites
  `SCAN_FLAG_KEYMAN_KEY_EVENT` with `SCANCODE_RSHIFT` for `VK_RSHIFT`, so an injected
  Right Shift reaches the OSK's hook feed as `vk = VK_SHIFT, scan = 0x36` —
  byte-identical to a physical press. A scan-code test therefore reads Keyman's own
  Right Shift as the user's, marks `essShift` user-held, and teardown declines to
  release the Shift the OSK clicked. Shift stays stuck: the exact outcome
  TEST_OSK-4's mechanism exists to avoid, arriving from the other direction.

## Baseline

[`evidence/baseline-shipped-build.md`](./evidence/baseline-shipped-build.md) records
the red measurements these cases were written against.

## Not covered here

`keyman.exe` killed or crashed while an OSK sticky modifier is held. Nothing
in-process can run after `TerminateProcess`, so no change in this area closes it. It
is written up in
[`issues/issue-osk-sticky-stranded-by-kill.md`](./issues/issue-osk-sticky-stranded-by-kill.md).
