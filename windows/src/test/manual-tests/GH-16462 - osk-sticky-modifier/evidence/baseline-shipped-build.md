# Red baseline — the OSK sticky-modifier failures as measured

Trimmed to the **failing** observations only. The passing verification runs against
the fixed engine are not reproduced here; this file exists so the defects are on the
record as measurements rather than as assertions.

**Instrument.** [`../watch-modifiers.ps1`](../watch-modifiers.ps1) — a 60 ms poller
over the nine modifier VKs, logging only state *changes*. Continuous polling is
deliberate: a stuck Ctrl makes the keyboard unusable, so nothing can be typed at the
moment of measurement. It reads all nine VKs rather than the six chiral slots, because
`do_keybd_event` injects the side-agnostic VK, so a phantom press is never
necessarily an `0xA0..0xA5` event.

**Calibration, before any null result was trusted** — a physical Left Shift press and
release, confirmed visible to the poller:

```
[09:05:01.133] HELD: SHIFT,LSHIFT
[09:05:05.253] ALL CLEAR
```

**Keyboard.** `sil_cameroon_qwerty`. Its `.kvk` header carries `<usealtgr/>`, so
`UpdateKeyboard` sets `kbd.LRShift := True` and the OSK draws separate *L Ctrl* /
*R Ctrl* keys. That is the chiral regime the chirality cases require.

---

## TEST_OSK-4 — teardown released a modifier the user was physically holding

**Measured, 2026-08-27, 09:11.** Hold physical Left Shift, click `R Ctrl` on the OSK,
dismiss:

```
[09:11:22.244] HELD: SHIFT,LSHIFT
[09:11:28.153] HELD: SHIFT,CTRL,LSHIFT,RCTRL
[09:11:35.396] ALL CLEAR                        <-- dismissal took BOTH
```

The dismissal was supposed to release only what the OSK had clicked. It cleared the
tester's own Shift as well.

**Cause**, found by reading the source rather than inferred from the log:
`UpdateShiftStates` runs on a 50 ms timer and ends with
`kbd.ShiftState := GetAsyncShiftState`, so `kbd.ShiftState` continuously carries
physically-held modifiers. `kbdShiftChange` then assigned it wholesale into
`FCachedShiftState`, so a click made while Shift was held cached `essShift` alongside
the key actually clicked, and the teardown released it.

The `GetAsyncKeyState` gate on the release cannot catch this: a physically-held key
genuinely *is* down, so the gate passes. The pre-existing comment argued that the
50 ms resync could not contaminate the cache because `FCachedShiftState` is written
"from a click and only from a click" — true of *when* it is written, irrelevant to
*what* it captures. The hazard travels in the value, not in the call path.

This is I2177 recurring, and it is why the cache carries an explicit invariant: a
release path may **read** `FCachedShiftState` and **remove** from it, and must never
**write** into it.

### Measurement protocol note

The first two attempts at this case were **INCONCLUSIVE, not failures**: sending the
message required pressing Enter, which meant releasing Shift at the critical moment,
and the log cannot distinguish "the teardown released your Shift" from "the tester let
go". The protocol was changed to pre-type the message and send it with the mouse, so
the held key never has to come up. Recorded here because the null result from the
earlier protocol was not evidence of anything.

For contrast, the shape a *pass* has under the corrected protocol (measured 09:35 on
the fixed build, and reproduced only to show what the log looks like when the case
passes):

```
[09:35:28.344] HELD: SHIFT,CTRL,LSHIFT,RCTRL    Shift held, R Ctrl clicked
[09:35:48.321] HELD: CTRL,RCTRL                 tester shifted grip
[09:36:00.108] HELD: SHIFT,CTRL,LSHIFT,RCTRL    re-held
[09:36:17.269] HELD: SHIFT,LSHIFT               <-- DISMISSAL: Ctrl released, Shift kept
[09:36:27.438] ALL CLEAR                        tester released, 10 s later
```

A tester releasing Shift produces `CTRL,RCTRL`, as at 09:35:48; it can never produce
`SHIFT,LSHIFT`. Ctrl going away while Shift stays can only have come from the
teardown.

---

## TEST_OSK-2 — the release went out with the wrong chirality

**Measured, 2026-08-27, 15:20, with `{$DEFINE KLOGGING}` so the injected event itself
is on the record rather than inferred from polling.** Press `R Ctrl` under a
`<usealtgr/>` keyboard, switch to one without it, then click the modifier off:

```
[15:20:17.381] keybd_event vk=A3 scan=0 flags=1              press, under cameroon
[15:20:31.256] UpdateKeyboard: VKI<>nil ... [akan]           switch: LRShift True->False
[15:20:40.911] ShiftStateChange: kbdShift= asyncShift=essCtrl
[15:20:40.915] keybd_event vk=A3 scan=0 flags=3              release
```

The `asyncShift=essCtrl` on the third line is the failure condition made visible: by
the time of the release, the engine's own view had **collapsed** to the generic
`essCtrl`. `SetLRShift` had rewritten `essRCtrl` away on the keyboard switch, so
neither `kbd.ShiftState` nor `kbd.LRShift` could still name the VK that was down.

**The shipped behaviour at that point emits `vk=11` unextended** — a generic,
left-resolving `VK_CONTROL` KEYUP — against a press that went out as `vk=A3` extended.
An unextended `VK_CONTROL` keyup does not clear an extended `VK_RCONTROL`. The poller
correspondingly drops `CTRL` and keeps `RCTRL`.

The `vk=A3 ... flags=3` above is the *fixed* build releasing the identity it actually
injected; it is quoted because it is the line that shows the collapse had happened and
the release survived it anyway.

**Recovery, when this was hit for real during the run**, on a keyboard with no
physical Right Ctrl:

- the OSK could not clear it — the OSK's own click-off is the path carrying the
  defect, so the obvious remedy is the one that does not work;
- the physical key could not clear it — it does not exist;
- every keystroke was meanwhile swallowed as a Ctrl chord, so the machine could not
  be driven by keyboard to fix itself, including to type a recovery script.

Recovery required an external tool injecting the matching event shape —
`keybd_event(VK_CONTROL, 0x1D, KEYUP | EXTENDEDKEY)`, side-agnostic VK with the
extended bit, mirroring what `do_keybd_event` sent going down. Absent such a tool the
realistic user remedy is a reboot. Compact and 60% layouts commonly ship without a
right Ctrl, so this is not a rare hardware configuration.

---

## TEST_OSK-1 — cleanup was never reached

Not a poller trace but a structural fact, stated here because it is what the poller
shows as an *unbounded* `HELD:` block with no terminating `ALL CLEAR`:
`ResetShiftStates` was invoked from `FormClose` and from the tab switch. Every other
dismissal path — the tray menu, the tray double click, the Keyman hotkey, exiting
Keyman — dismisses the visual keyboard with `Release`/`FreeAndNil`, which runs
`OnDestroy` and never `OnClose`. Those paths released nothing at all.

---

## TEST_OSK-3 and TEST_OSK-5 — reasoned from source, not separately measured

Recorded honestly as such.

**TEST_OSK-3** requires landing a dismissal inside the 50 ms `UpdateShiftStates`
resync window, which was not arranged deliberately during these runs. The defect is
read from the source: `ResetShiftStates` routed its cleanup through
`ShiftStateChange`'s `PrepState`, whose first branch emits a **KEYDOWN**. What the
runs do establish is the post-fix half — the `KLOGGING` traces show the teardown
emitting only `flags=2`/`flags=3` (KEYUP), never a press, in every recorded run, and
after the rewrite the press branch is not on this function's code path at all, so the
failure mode is structurally impossible rather than timing-avoided.

**TEST_OSK-5** was found by reading `keybd_shift.cpp` while auditing an earlier
scan-code-based provenance test, not by a field reproduction:
`do_keybd_event` overwrites `SCAN_FLAG_KEYMAN_KEY_EVENT` with `SCANCODE_RSHIFT` for
`VK_RSHIFT`, so an injected Right Shift is byte-identical to a physical one at this
feed. The consequence — a declined release, and a stuck Shift — follows from that
directly.

---

## An unattributed observation, deliberately not written off

The 60 ms poller recorded clear-and-reassert transients during several runs that the
engine log does not account for: the modifier reads up, then down again, with no
intervening `keybd_event` from `keyman.exe`. Example, where the engine log records
exactly one press and one release:

```
[15:19:07.848] HELD: CTRL,RCTRL
[15:19:10.667] ALL CLEAR          <-- no injection logged here
[15:19:12.046] HELD: CTRL,RCTRL   <-- nor here
[15:19:12.118] ALL CLEAR
```

What this rules out is useful: `keyman.exe` did not inject them. What it does **not**
rule out is `keyman32.dll`, which runs inside every hooked process and logs through
the C++ `SendDebugMessage`/ETW path that an `OutputDebugString` capture of
`keyman.exe` does not see. That blind spot is a property of the instrument, not
evidence of absence.

One correlation, on three observations and offered as a hypothesis rather than a
finding: the transient appeared in the runs involving the tray menu and/or a keyboard
switch, and not in the runs without either. Entering a shell notification-area menu
runs a modal message loop, and Windows normalising modifier state around that would
fit an artifact with no Keyman injection behind it. Testable by repeating the tray
dismissal several times and seeing whether it tracks.

The verdicts above are unaffected — every injected pair is matched, correctly chiral,
and the final state is clean — but an unattributed modifier producer should not be
closed on the basis that the outcomes happened to be right.
