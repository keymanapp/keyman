# fix(windows): on-screen keyboard sticky modifiers, for #16462

Fixes: #16462
Related: #8064

> [!IMPORTANT]
> **This PR requires the engine PR (`fix/windows/8064-stuck-modifier`) to have landed
> first.** The user-held signal added here depends on
> `KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED`, a flag that PR introduces in
> `keyman32/k32_visualkeyboardinterface.h` and sets in
> `keyman32/k32_lowlevelkeyboardhook.cpp`. Nothing in this branch touches
> `windows/src/engine/keyman32/` at all.
>
> This branch *compiles* standalone, because the Delphi side redeclares the flag as a
> literal constant rather than importing it. That is deliberate and it is also the
> trap: without the engine change nothing ever sets the bit, so
> `UpdateUserHeldModifiers` silently attributes Keyman's own injected modifiers to the
> user, and the release it then declines is the stuck Shift of
> `TEST_OSK_RIGHT_SHIFT_STICKY`. Merging this first would compile clean and behave
> worse than shipping. Merge order is a correctness requirement here, not a
> convenience.

## What this is

[#16462](https://github.com/keymanapp/keyman/issues/16462), *bug: Closing /navigating OSK strands a sticky modifier*.
Related, but a different producer and a different fix: [#8064](https://github.com/keymanapp/keyman/issues/8064), *bug(windows): modifier key
occasionally is "stuck on"*, has two independent producers. This PR is the
**on-screen-keyboard half**. The serializer half — `keybd_shift`'s modifier cache in
`keyman32` — is the other PR, and the two are separable: they share the symptom and
nothing else.

The mechanism specific to the OSK: a modifier "clicked sticky" on the on-screen
keyboard is held with a real, chiral `keybd_event` KEYDOWN and **no matching KEYUP
queued anywhere**. Nothing but Keyman will ever send that KEYUP. So the result of any
mistake here is not a Keyman typing glitch — it is a modifier asserted
**machine-wide**, in every application and on every layout, until the user presses
that same physical key themselves. On a compact or 60% layout with no physical Right
Ctrl, that key does not exist and the realistic remedy is a reboot; meanwhile every
keystroke is swallowed as a Ctrl chord, so the machine cannot be driven by keyboard to
fix itself.

## What changed

Four defects, in two files, both under `windows/src/engine/keyman/viskbd/`.

**Cleanup was not reached on most dismissal paths.** `ResetShiftStates` hung off
`FormClose` and the tab switch. Every other dismissal — tray menu, tray double click,
the Keyman hotkey, exiting Keyman — goes through `Release`/`FreeAndNil`, which run
`OnDestroy` and never `OnClose`, and released nothing at all.
`UfrmVisualKeyboard.FormDestroy` now calls it. Idempotent, so the X-button path
reaching it twice is harmless.

**The release used the wrong chirality.** A keyboard switch runs `SetLRShift`, which
collapses `kbd.ShiftState`'s chiral Ctrl/Alt entries to generic ones — after which
neither it nor `kbd.LRShift` can name the VK that is down. Both release paths derived
their VK from the current regime, so a sticky R Ctrl was released as an unextended
`VK_CONTROL` while the extended `VK_RCONTROL` stayed held. Both now take the identity
from `FCachedShiftState`, which survives the collapse.

**The cleanup could itself press a modifier.** `ResetShiftStates` routed its release
through `ShiftStateChange`'s `PrepState`, whose first branch emits a KEYDOWN. The new
`ReleaseCached` helper only ever emits `KEYEVENTF_KEYUP`, so the phantom press is
structurally impossible rather than timing-avoided.

**Teardown could cancel a modifier the user is holding.** `FCachedShiftState` records
that the OSK *pressed* a modifier; it does not record that the OSK is the *only* thing
holding it, and Windows keeps one down state per key, not one per holder. Two things
close that: the cache no longer absorbs physically-held modifiers from
`UpdateShiftStates`' 50 ms resync (which is the I2177 defect, and it recurred during
this work — see the baseline), and a new `FUserHeldShiftState`, fed by
`UpdateUserHeldModifiers` from the hook's modifier event, carries the missing fact.

Polling cannot supply that fact: by the time the question is asked the key is already
down, and a poll cannot say how many holders are behind it. The hook feed is the only
place the OSK can learn that a modifier it is already holding has *also* been pressed
by the user.

The asymmetry decides the policy. When the user is also holding the key, declining to
release costs nothing — their own physical KEYUP clears the single shared down state
and takes the OSK's press with it. Releasing is unrecoverable without user action. So
an overlap suppresses the KEYUP, matched by the exact key it lands on and never by
family: an OSK-held Right Ctrl must not be suppressed by a user-held Left Ctrl,
because Right Ctrl is the identity a user without that physical key cannot clear.

Provenance is decided by an explicit ledger and by the engine's flag, **not** by scan
code. A real scan code does not reliably mean "the user" — Windows was measured
propagating `bScan = 0` untouched for some events and substituting a real scan code
for others in the same run — and for `VK_RSHIFT` it is actively wrong:
`keybd_shift.cpp` overwrites the Keyman scan flag with `SCANCODE_RSHIFT`, so an
injected Right Shift arrives byte-identical to a physical one. One scan-code test
legitimately remains, for the one event Windows itself manufactures and nobody holds:
the AltGr compatibility left Control.

## Evidence, stated honestly

The OSK path is **measured on a live engine**, not reasoned about: a debug
`keyman.exe` built from this work, driven by hand, with a 60 ms nine-VK modifier
poller (`watch-modifiers.ps1`) as the oracle, and for the chirality cases a
`{$DEFINE KLOGGING}` build so the injected `vk`/`scan`/`flags` are on the record
rather than inferred from the effect. The instrument was calibrated against a physical
key press before any null result was trusted, and one measurement protocol was
corrected mid-run and the discarded attempts recorded rather than dropped.

**There is no automated test, and none is possible in this repo today.** There is no
DUnit or DUnitX project anywhere under `windows/src/engine` — the engine's only
automated tests are the C++ googletest projects under `keyman32/tests/`, and none of
this change is in C++. The behaviour is also machine-wide modifier state reached
through `keybd_event`, which no in-process harness observes. So this ships as a manual
test with a scripted oracle and a recorded red baseline, matching what the repo
already does for `GH-140 - shift states` and `test_i3762 modifier tests`. It is not
compiled by CI either: Delphi is IDE-built here.

Two of the five documented cases (`TEST_OSK-3`, the phantom press, and `TEST_OSK-5`,
the Right Shift misattribution) are **reasoned from source rather than separately
reproduced**, and the baseline says so at the point it says it. The other three were
measured.

**One producer remains UNMITIGATED, on every build including this one:** `keyman.exe`
killed or crashed while an OSK sticky modifier is held. Nothing in-process can run
after `TerminateProcess`, and there is no persisted record of the outstanding modifier
and no watchdog to reconcile it at next launch. Releasing all managed modifiers
unconditionally at startup was considered and rejected — it would release a modifier
the user is genuinely holding as Keyman launches, which is a new bug in the same
family. It is drafted in
[`issues/issue-osk-sticky-stranded-by-kill.md`](./issues/issue-osk-sticky-stranded-by-kill.md).
So the honest verdict for #16462 is **mitigated, with one producer left
open**, not closed.

## Files

- `windows/src/engine/keyman/viskbd/UfrmVisualKeyboard.pas`
- `windows/src/engine/keyman/viskbd/UfrmOSKOnScreenKeyboard.pas`
- `windows/src/test/manual-tests/GH-16462 - osk-sticky-modifier/` — the manual test, the
  oracle, the red baseline, and three paste-ready issue drafts (two fixed here but live
  in released builds, one unfixed).

# User Testing

TEST_OSK_MODIFIERS_BASELINE: Open the on screen keyboard in Keyman for Windows. Click each modifier key in turn to set it, and verify that clicking a character key produces the modified output, and that pressing a character on the hardware keyboard produces it too. Do this for each modifier state. This passes on a released build and must still pass here.

TEST_RIGHT_SHIFT: Open the on screen keyboard and click the Left Shift key. Then press and release the Right Shift key on the hardware keyboard. Verify the shift state is reset in the OSK and that the next key typed shows unmodified output. This is the behaviour added by #16361 and it must be unchanged.

TEST_RELEASE_MODIFIERS_OPTION: Turn on "Release Shift/Ctrl/Alt on On Screen Keyboard after clicking a key" in the OSK options. Click a modifier, then click a character key, and verify the modifier is released afterwards and the output is correct. Turn the option off and verify the modifier stays set instead.

TEST_OSK_TEARDOWN_PATHS: Open the on screen keyboard, click Ctrl so it is held, then dismiss the OSK using the Keyman tray menu rather than the OSK's own close button. On a released build Ctrl stays held machine-wide, because cleanup only ran on the close button and tab switch paths. With this change Ctrl is released. Repeat for a tray double click, for the Keyman hotkey, and for exiting Keyman entirely while the modifier is held.

TEST_OSK_CHIRAL_RELEASE: Install common/test/resources/keyboards/test_chirality.kmx, whose visual keyboard sets <usealtgr/> so the OSK draws separate Left Ctrl and Right Ctrl keys, and whose rules echo which chiral modifier was live — RC_a for Right Ctrl, LC_a for Left Ctrl, DF_a for none. Open the on screen keyboard, click Right Ctrl so it is held, and click a; verify RC_a. Now switch to any keyboard without <usealtgr/>, such as common/test/keyboards/baseline/k_0301___multiple_deadkeys.kmx, switch back, and click the Ctrl key off. On a released build the OSK releases a generic Ctrl while the extended Right Ctrl stays held, so clicking a still gives RC_a and nothing clears it on hardware with no physical Right Ctrl. With this change the exact key that was pressed is the one released, and a gives DF_a.

TEST_OSK_USER_HELD_MODIFIER: Open the on screen keyboard and click Shift so it is held. Now press and hold Shift on the hardware keyboard as well, and while still holding it dismiss the OSK. Verify that the physical Shift you are holding is still held afterwards — typing a character must produce modified output until you release it yourself — and that once you release it, no modifier remains stuck.

TEST_OSK_RIGHT_SHIFT_STICKY: Open the on screen keyboard and click Shift so it is held. Type enough through a Keyman keyboard that the engine injects output several times, then dismiss the OSK. On a released build the OSK can mistake its own injected Right Shift for one you are holding and decline to release the Shift it clicked, leaving Shift stuck. With this change Shift is released on dismissal.
