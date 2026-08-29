# fix(windows): stop the serializer stranding a modifier key (#8064)

Fixes: #8064
Related: #16462

Closes [#8064](https://github.com/keymanapp/keyman/issues/8064), *bug(windows): modifier
key occasionally is "stuck on"*. That report is entirely about physical typing -- its
reproduction is a hardware Right Shift released a fraction late -- and neither its body nor
any of its comments mentions the on-screen keyboard. The OSK reaches the same symptom by a
different producer and is tracked separately as
[#16462](https://github.com/keymanapp/keyman/issues/16462).

## What #8064 is

A modifier key stuck down **machine-wide**. Not a Keyman typing glitch: every
application on the machine behaves as though Ctrl, Alt or Shift were held, on
every keyboard layout, until the exact matching KEYUP arrives. Ordinary typing
usually produces that KEYUP by accident, which is why the bug appears to "fix
itself" and why it has been so hard to catch in the act. On hardware with no
physical Right Ctrl key the user cannot produce it at all, and only a restart
clears it — which is the shape of the worst field reports.

## The two producers

**The serializer.** `SerialKeyEventServer` keeps its own copy of the modifier
state. It is seeded from the OS once, in `InitThread`, and thereafter fed only by
messages posted from the low level keyboard hook. Windows bypasses a low level
hook that does not return within `LowLevelHooksTimeout`, so a modifier KEYUP can
be dropped, and nothing has re-derived that cache since 2018-10-10, when
`738e1946a6` deleted the per-batch `GetKeyboardState` from `keybd_shift_release`.
One stale byte then survives for the life of the process, and `keybd_shift_reset`
presses that modifier for real ahead of every injected batch — a KEYDOWN with no
matching KEYUP.

**The on-screen keyboard.** It holds a sticky modifier with a real chiral
KEYDOWN, runs its cleanup on only two of its dismissal paths, and releases by the
*current* `kbd.LRShift` regime rather than the identity it injected. That is a
separate PR, described below.

This PR is the serializer half.

## What changed

- **The cache is reconciled against live state before every batch.**
  `ReconcileModifierCache` clears any cached byte the OS reports up. It clears and
  never sets: live state cannot tell the user's finger from Keyman's own press one
  message ago, so setting from it would manufacture the very unmatched press this
  work exists to remove.
- **The release half releases the union of cache-held and OS-held modifiers.**
  Clearing alone would let a modifier the OS holds but the cache never recorded
  survive into the batch's own output keys.
- **Keyman's own injected modifiers no longer feed the cache.** They are tagged
  with `EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP` at injection and filtered at the
  hook. `dwExtraInfo` and not a scan code, because `do_keybd_event` rewrites an
  injected Right Shift's scan code to `SCANCODE_RSHIFT`, making it byte-identical
  at the hook to a physical one.
- **The low level hook no longer eats a key event before the handoff is
  confirmed.** It returned 1 unconditionally, discarding `PostMessage`'s result and
  dereferencing the server window with no null check. A destroyed modifier KEYUP is
  #8064 arriving from a second direction.
- **A post-batch verification pass** catches a release that raced a batch in
  flight, and releases only modifiers named in that batch's own restore mask. It is
  posted rather than run inline, so the hook's earlier posts have reached the cache
  by the time it reads it.
- **A user-held signal fed from `WM_INPUT`** on the serializer's own thread lets a
  hold the cache never saw be restored. It reports *unknown* rather than stale, is
  never authoritative alone, and widens what the cache justifies without ever
  vetoing it.
- **Tests.** `keybd_shift.tests.cpp` on the default target, and a separate opt-in
  `test-interactive` action for four probes that inject real keyboard input — which
  a Session-0 CI account cannot observe, so on CI they could only have reported
  PASSED without asserting anything.

## Evidence, tiered

**Measured end to end.** The defect reproduced and fixed on the same machine with
one variable changed: `host32.exe --probe 1x2x3x --iterations 5`, Left Shift held
and released 1500 ms into a five second stall, Windows 11 Pro 26200. Shipped
engine **5 of 5 wedged**; fixed engine **0 of 5**. All preconditions confirmed on
every iteration. Recorded in
`windows/src/test/manual-tests/GH-8064 - stuck-modifier-phantom-keydown/evidence/baseline-shipped-build.txt`.

**Measured, separately.** That `WM_INPUT` reaches a message-only window on a
worker thread while the process is unfocused (164 records) and while the main
thread is stalled (42 records during an 8000 ms block, with `fakefreeze` holding
`keyman.exe` too), and that the Keyman tag survives `SendInput` to raw input in
both the `0xFF` and the `SCANCODE_RSHIFT` shapes. That is the only evidence the
user-held signal rests on, and it is in `evidence/rawinput-delivery.md`.

**Source-reasoned only.** The residual serializer paths added after the end-to-end
run — the verification pass, the short-`SendInput` boundary, the diagnostic codes
— are covered by unit tests and a clean build, but were not re-run end to end.

**Carried forward as unverified, with reasons.** Not dropped silently:

- **ARM64.** No build agent, in line with #15065; the `test` and `test-interactive`
  actions have no arm64 leg for that reason.
- **The physical-keystroke column baseline** of the raw-input probe. No physical
  keystroke arrived inside the wait window on the decisive capture, and the row is
  left blank rather than inferred. 206 physical-key records in the delivery legs
  stand in for delivery; the classification side is pinned in the suite.
- **The RDP, OSK, higher-integrity-focus and secure-desktop legs** of that probe.
  Not run. Each population is pinned in the unit suite, and the policy is tag
  equality rather than an `hDevice` or injected-versus-physical filter — so there
  is no RDP-specific code path to get wrong — but these are unverified, not
  satisfied. `TEST_REMOTE_DESKTOP` below exists because of this.

## The OSK half is a separate PR

It requires this one to land first: the hook change here sets
`KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED` on `WM_KEYMAN_OSK_MODIFIER_EVENT`, and
the Delphi side that reads it is in that PR. Until both land, two producers stay
unmitigated on every build — `keyman.exe` killed while an OSK sticky modifier is
held, and `PostKeys` pair-splitting under queue truncation — so a stuck modifier
reported after this ships is triaged through `TRIAGE.md` rather than assumed to be
a regression. Drafts for both are in `issues/`.


## Running the regression gate

`windows/src/test/manual-tests/GH-8064 - stuck-modifier-phantom-keydown/stuck-mod-test.ps1 -Gate`
is a three-arm controlled test: it holds the stimulus constant and varies only the active keyboard,
so a wedge on the Keyman arm that does not appear on the Microsoft arms indicts Keyman rather than
the layout, Windows, or the harness. It exits 0 PASS, 1 FAIL, 2 INCONCLUSIVE, 3 setup error, and is
expected to FAIL on a released build and PASS on this one.

It needs the `sil_cameroon_qwerty` keyboard and any Microsoft English QWERTY installed, an
interactive desktop, and a running `keyman.exe`. It injects real keystrokes, so the machine must be
left alone while it runs.


## Measured, end to end

One machine, one session, one harness, one command. Only the engine differs.

| | engine | freeze confirmed | batch assembled | modifier stuck | |
|---|---|---|---|---|---|
| before | Keyman 19.0.276 Release, clean installer | 5/5 | 5/5 | **5/5** | FAIL |
| after | this branch, Win32/Debug | 5/5 | 5/5 | **0/5** | PASS |

Both halves are measurements rather than absences. Each iteration confirmed the freeze
live rather than assuming it after a fixed delay, and confirmed the probe actually
transformed, which is what proves a batch was assembled and the code under test ran. An
earlier attempt the same day returned INCONCLUSIVE with `text transformed 0` — five
iterations, no stuck modifier, and it proved nothing, because no rule fired. The harness
refuses to call that a pass, and that refusal is what makes the `0/5` meaningful.

Full provenance, including the preconditions that had to hold and the confounds that
remain open, is in
[`evidence/baseline-shipped-build.txt`](evidence/baseline-shipped-build.txt) and
[`evidence/run-after-branch-2026-08-29.txt`](evidence/run-after-branch-2026-08-29.txt).

One constraint is worth repeating here, because it can silently turn a broken build
green:

- **The host must be 32-bit.** `serialkeyeventserver.cpp` is `#ifndef _WIN64`, so the
  cache under test does not exist in a 64-bit process. Windows 11 Notepad is 64-bit — both
  `notepad.exe` and `SysWOW64\notepad.exe` report `IsWow64Process` false — which is why
  `host32/` exists.

# User Testing

TEST_MODIFIERS_BASELINE: Select a Keyman keyboard with rules that transform modified keystrokes, in a 32-bit application. Hold each modifier in turn on the hardware keyboard — Left Shift, Right Shift, Left Ctrl, Right Ctrl, Left Alt, Right Alt — and type a character the keyboard transforms. Verify the modified output is correct in every case, and that releasing the modifier returns to unmodified output. This passes on a released build and must still pass here.

TEST_STUCK_MODIFIER_REPRO: Follow README.md in windows/src/test/manual-tests/GH-8064 - stuck-modifier-phantom-keydown/ to build fakefreeze and run run-8064-test.ps1 against a 32-bit host application with a Keyman keyboard selected. The script holds a modifier, stalls Keyman past LowLevelHooksTimeout so Windows drops the hook, releases the modifier during the stall, and then makes Keyman produce output. On a released build this reports FAIL on all five iterations, with the modifier still reading down after the run. With this change it reports PASS on all five. A result of INCONCLUSIVE means the preconditions were not met and is not a pass — the script says which one.

TEST_STUCK_MODIFIER_MANUAL: The same defect by hand, for a machine without the harness. In a 32-bit application with a Keyman keyboard selected, hold Right Ctrl down, trigger the five second stall with fakefreeze, release Right Ctrl while the stall is still running, then type a character the keyboard transforms. On a released build Right Ctrl is now stuck down machine-wide — every application behaves as though Ctrl were held, and on hardware without a physical Right Ctrl nothing clears it. With this change typing continues normally and no modifier is left held.

TEST_MODIFIER_HELD_BEFORE_KEYMAN_STARTS: Exit Keyman. Hold Left Ctrl down, and while still holding it start Keyman, then release Left Ctrl before typing anything. Select a Keyman keyboard and type a character the keyboard transforms. On a released build the modifier state Keyman captured at startup is now stale and the first injected output can carry a phantom Ctrl. With this change the output is unmodified and correct.

TEST_NO_LOST_KEYSTROKES: In a 32-bit application with a Keyman keyboard selected, type continuously while triggering repeated fakefreeze stalls. Verify that no characters are silently lost — output may arrive late or out of order during a stall, but every keystroke must appear. This exercises the change that stops the low level hook suppressing a key event before it has confirmed the event was handed off.

TEST_REMOTE_DESKTOP: Connect to the machine over Remote Desktop, select a Keyman keyboard and type, using modifiers. Verify that typing and modified output work normally. Remote Desktop stamps its own marker on genuine user input, and this change reads that field to tell Keyman's own injected events from the user's, so this confirms real remote input is not misread as Keyman's own.
