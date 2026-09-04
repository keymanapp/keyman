# GH-8064: stuck modifier, phantom KEYDOWN

Manual end-to-end test for [#8064](https://github.com/keymanapp/keyman/issues/8064),
*bug(windows): modifier key occasionally is "stuck on"*.

There is no app to build here. The tools this test needs already exist in the
tree, and the pass/fail oracle is two PowerShell snippets given below.

[TRIAGE.md](./TRIAGE.md) is the companion document: how to tell the serializer
path from the on-screen keyboard path when a stuck modifier is reported in the
field. Read it before concluding that a report after this ships is a regression.

## The defect

`SerialKeyEventServer` keeps its own copy of the modifier state,
`m_ModifierKeyboardState`. It is seeded from the OS once, in `InitThread`, and
thereafter fed only by messages posted from the low level keyboard hook. Windows
bypasses a low level hook that does not return within `LowLevelHooksTimeout`, so
a modifier KEYUP can be lost, and nothing re-derives the cache — so that one
stale byte survives for the life of the process. `keybd_shift_reset` then presses
that modifier for real, with no matching KEYUP, ahead of every injected batch.

The cache has not been re-derived since 2018-10-10, when `738e1946a6` deleted the
per-batch `GetKeyboardState` from `keybd_shift_release`.

The seed is real, and that is worth stating because it is easy to assume
otherwise. `InitThread` calls `GetKeyboardState`, which reports the *calling
thread's* processed input queue rather than live hardware state — so the natural
reading is that a worker thread which has never pumped input gets nothing useful.
Measured, it gets the opposite.
`KEYBD_SHIFT.FreshThreadKeyboardStateReflectsLiveModifiers` in
`windows/src/engine/keyman32/tests/keybd_shift.interactive.tests.cpp` holds Left
Shift down and reads both threads:

```
this thread : GetKeyboardState ok=1 byte=0x00, GetAsyncKeyState=0x8001
fresh thread: GetKeyboardState ok=1 byte=0x81, GetAsyncKeyState=0x8000
```

A thread that has never pumped input reports the key **held** (high bit set); the
process main thread, whose queue the event never reached, reports it **up**.
`InitThread` reads the state before it calls `RegisterClass` or `CreateWindow`, so
it is exactly that queue-less case and the seed reflects whatever is genuinely
held at launch.

That gives the cache a second way to go stale, independent of any dropped KEYUP:
a modifier held as Keyman starts is seeded into the cache, and if the user
releases it before the hook is feeding events, nothing clears it.
`ReconcileModifierCache` closes both routes, since it compares the cache against
live state without caring how the two came to disagree.

The result is not a Keyman typing glitch. It is a modifier stuck down
**machine-wide**, in every application and on every keyboard layout, until the
exact matching KEYUP arrives.

The automated counterpart is `KEYBD_SHIFT.*` and `RECONCILE_MODIFIER_CACHE.*` in
`tests/keybd_shift.tests.cpp`, on the default CI target. Those construct the stale
byte directly. This manual test is the only one that exercises the real path: a
genuinely stalled hook, a genuinely dropped event, and `SendInput` reaching the
whole machine.

## Why an ordinary smoke test never finds it

Two things have to coincide, and on an idle or merely-busy machine they do not:

1. keyman.exe's main thread has to stall past `LowLevelHooksTimeout`, and
2. the event lost to that stall has to be a **modifier KEYUP**.

CPU load alone does not do it. Step 3 of the procedure below, releasing the
modifier *during* the stall, is the step that has to be arranged deliberately, and
it is the step no ordinary test performs. Nor does the defect announce itself in
the test document: a stuck Ctrl or Alt swallows keys and produces no case change
at all, so a smoke test that reads the text scores a genuinely wedged machine as
clean.

## Preconditions

- A real Keyman for Windows install, running, with a **Keyman** keyboard active:
  not a Microsoft or MSKLC layout of the same language. Confirm the active layout
  from the focused control, not the top-level window: Windows 11 Notepad's frame
  window sits on a thread pinned to its original input locale while the focused
  edit control tracks the real one.
- `Flag_ShouldSerializeInput` not disabled (it defaults to on).
- A host application with a text field. Bitness is **not** a precondition -- see
  "Host bitness" below. `host32/` is the recommended host; `stuck-mod-test.ps1`
  uses 64-bit Notepad and reproduces the defect there.
- Notepad open. Nothing more elaborate is needed, and nothing that holds real
  data should be used. See Hazards.

## Tools

| tool | role |
|---|---|
| `windows/src/support/fakefreeze` | the stimulus. Posts `KMC_WATCHDOG_FAKEFREEZE` to keyman.exe, which pauses for five seconds. Build with `./windows/src/support/fakefreeze/build.sh --debug build:x86` |
| `host32/host32.cpp` | the 32-bit host, which drives the whole sequence itself |
| `host32/rawinput-probe.cpp` | a measurement, not a fix: it changes no production file, and answers one question -- does a `WM_INPUT` feed on a worker thread keep delivering, with Keyman's `dwExtraInfo` tag intact, while the thread the low level hook was installed on is stalled. Its result is committed as [`evidence/rawinput-delivery.md`](evidence/rawinput-delivery.md). Build: `cl /nologo /W4 /EHsc /MT /DUNICODE /D_UNICODE rawinput-probe.cpp /link /SUBSYSTEM:WINDOWS user32.lib gdi32.lib /OUT:rawinput-probe.exe` |
| `run-8064-test.ps1` | the automated harness |
| `windows/src/test/manual-tests/keyboard_ll_identifier` | the wire logger. A `WH_KEYBOARD_LL` hook that logs `vkCode scanCode flags` for the nine modifier virtual keys. Delphi, and no binary is committed, so it has to be built |
| the snippets below | the pass/fail oracle, and the recovery |
| [`stuck-mod-test.ps1`](./stuck-mod-test.ps1) | the three-arm regression gate. `-Gate` returns 0 PASS / 1 FAIL / 2 INCONCLUSIVE / 3 setup error, and reports engine logging state. Needs `sil_cameroon_qwerty` and any Microsoft English QWERTY |

> `keyboard_ll_identifier` installs a **global** low level hook and logs every
> modifier keystroke on the machine while it runs. It does not log character keys,
> but close it before typing anything sensitive.

`rawinput-probe` is the one entry above that no step of this test invokes: it was
run once, to settle a design question, and is listed so it is not mistaken for a
repro step.

## Automated harness

`run-8064-test.ps1` performs steps 3 to 7 of the manual procedure: it holds the
modifier, posts the freeze, releases inside the stall, types a probe string, reads
the oracle, and clears any modifier it left asserted.

It requires `-HostApp`, a path to an application with a text field, and verifies
that it presents a real window rather than assuming it. There is no default,
because on Windows 11 both `notepad.exe` and the `SysWOW64` copy resolve to the
same 64-bit packaged Notepad, whose top-level frame brings complications of its
own (see the input-locale note under Preconditions).

### Host bitness

Until 2026-08-31 this harness *required* a 32-bit host and aborted on anything
else, on the reasoning that `serialkeyeventserver.cpp` being `#ifndef _WIN64`
makes a 64-bit host immune. That does not follow, and the requirement has been
downgraded to a reported value.

The server, the low level keyboard hook and the modifier cache are indeed compiled
only into the 32-bit engine and run in 32-bit `keyman.exe`. But `WH_KEYBOARD_LL`
is system-wide, and `serialkeyeventclient.cpp` carries no `_WIN64` guard: a 64-bit
client reaches that same single server by unsuffixed global name through a
memory-mapped file (`GLOBAL_FILE_MAPPING_NAME`, `serialkeyeventcommon.h:59`). One
server per session, serving both bitnesses. `stuck-mod-test.ps1` demonstrates the
consequence -- it wedges 64-bit Windows 11 Notepad and has never had a bitness
check.

`host32/` remains the recommended host, because it is the one behind the recorded
before/after pair in `evidence/` and it removes Notepad's packaged-app and
frame-window complications. A 64-bit run through *this* script is not yet backed
by a recorded measurement, so the script warns and asks you to save the output.

### host32 is a separate harness, not a host for this script

`host32.exe` **requires** `--fakefreeze PATH` and exits without it (`host32.cpp:598`).
It drives the whole sequence itself -- hold, freeze, release, type, read the oracle --
and is what produced the recorded pair in `evidence/`. Passing it to
`run-8064-test.ps1` cannot work: it exits immediately and the script has nothing to
type into. Run it directly instead:

```
./host32/host32.exe --fakefreeze <fakefreeze.exe> --probe 1x2x3x --iterations 5 --wait-for-rule 30
```

`run-8064-test.ps1` wants an ordinary passive application with an Edit control.
`C:\Windows\SysWOW64\charmap.exe` is one that ships with Windows and is 32-bit at
runtime; most stock alternatives are not (see Host bitness above).

`host32/` itself is a minimal 32-bit window with a single Edit control,
a fixed class and title, and it publishes its active keyboard layout in that title
because `GetKeyboardLayout(idThread)` returns 0 for a thread in another process
and the harness cannot otherwise tell which layout is selected. Build it with the
Keyman build environment sourced:

```
cl /nologo /W4 /EHsc /MT /DUNICODE /D_UNICODE host32.cpp \
   /link /SUBSYSTEM:WINDOWS user32.lib gdi32.lib /OUT:host32.exe
```

`host32` drives the sequence itself rather than being driven from the harness, and
that is a Windows constraint rather than a preference. A background process cannot
reliably grant another process's window keyboard focus: `SetForegroundWindow`
succeeds and `GetForegroundWindow` confirms the host is foreground, yet `GetFocus`
in the host's thread stays 0 and `SendInput` keystrokes go nowhere, because
`SetFocus` needs the calling thread to own the active window. A process that owns
its own window has focus by construction, the same way a person pressing keys
does.

The harness reports **INCONCLUSIVE** rather than PASS unless it confirms all three
of: the freeze took effect, a Keyman TIP is selected in a host it actually brought
to the foreground, and Keyman transformed the probe text. Host bitness is reported
on the result line but is not one of them. A precondition that is merely plausible produces a
false PASS, and a false PASS on this defect is worse than no test.

```
# An ordinary app with a text field -- NOT host32.exe, see below.
./run-8064-test.ps1 -HostApp <editor.exe> -Control    # harness sanity check
./run-8064-test.ps1 -HostApp <editor.exe>
./run-8064-test.ps1 -HostApp <editor.exe> -Modifier RSHIFT
```

`-Modifier RSHIFT` is the interesting case: Right Shift is the one modifier whose
`SCAN_FLAG_KEYMAN_KEY_EVENT` is overwritten with `SCANCODE_RSHIFT`, so only the
`dwExtraInfo` arm of the provenance gate covers it.

### Why the provenance gate keeps a scan-code arm

`IsKeymanInjectedKeyEvent` (`keybd_shift.cpp`) tests two tags, and the older of the
two is the scan code: `SCAN_FLAG_KEYMAN_KEY_EVENT`, `0xFF`, written into the scan
byte of the events Keyman injects. `keyman64.h` has carried a TODO to deprecate that
overload in favour of `dwExtraInfo` since long before this branch, and nothing in the
API blocks it: `keybd_event`'s fourth parameter *is* `dwExtraInfo`, and all five
direct callers already pass it, as `0` -- `keyman32.cpp:924-925`,
`kmhook_keyboard.cpp:147`, `kmprocessactions.cpp:101-102`. Retiring the scan arm is
therefore a matter of tagging those five call sites, not of opening a new channel.
*Those line numbers are a snapshot, checked at `d2a57b42f1` on 2026-09-03, and are
not maintained.*

This branch does not do it -- a bugfix is the wrong change to carry a deprecation --
so the gate keeps both arms: the scan arm, which is the only cover those five
untagged injections have, and `EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP`, which is the
only cover for the Right Shift shape above.

## Procedure, by hand

1. Start `keyboard_ll_identifier`. Press and release Left Shift once and confirm a
   matched KEYDOWN/KEYUP pair appears.
2. Focus the host and type a few characters. Confirm the Keyman keyboard is
   producing its own output, not the base layout's.
3. **Press and hold Left Shift.**
4. With Shift still held, run `fakefreeze.exe`. It prints `Sleeping 5 seconds...`.
5. **Release Left Shift while the freeze is still running.** This is the whole
   test. Releasing before or after proves nothing.
6. When `fakefreeze` reports Keyman is responsive again, type a key sequence that
   matches a rule in the active keyboard, so that Keyman produces output and an
   injected batch is assembled.
7. Run the oracle snippet below **without touching the keyboard**.

## The oracle

Not the text in the editor. A stuck Ctrl or Alt swallows keys and produces no case
change at all, so a text-only check scores a genuinely wedged machine as clean.
Read the modifier state instead:

```ps1
Add-Type -Name Km8064 -Namespace Probe -MemberDefinition '[DllImport("user32.dll")] public static extern short GetAsyncKeyState(int vKey);'
$mods = [ordered]@{ SHIFT=0x10; CTRL=0x11; ALT=0x12; LSHIFT=0xA0; RSHIFT=0xA1; LCTRL=0xA2; RCTRL=0xA3; LALT=0xA4; RALT=0xA5 }
foreach ($m in $mods.GetEnumerator()) {
  $down = [Probe.Km8064]::GetAsyncKeyState($m.Value) -lt 0
  '{0,-7} {1}' -f $m.Key, $(if ($down) { 'DOWN  <-- held' } else { 'up' })
}
```

All nine, not the six cache slots. `do_keybd_event` maps every modifier to the
side-agnostic virtual key before injecting it — `VK_LSHIFT` and `VK_RSHIFT` both
leave as `VK_SHIFT`, `VK_LCONTROL` and `VK_RCONTROL` as `VK_CONTROL`, `VK_LMENU`
and `VK_RMENU` as `VK_MENU` — so the phantom press is never an `0xA0`..`0xA5`
event. Reading only those six can report everything `up` on a machine that is
holding `VK_SHIFT` down, which is a false PASS. The wedge in the recorded baseline
was reported as `SHIFT, LSHIFT`: both the side-agnostic and the chiral VK.

**FAIL**, the defect reproduced, is any modifier reported `DOWN` while nothing is
physically pressed, together with, in `keyboard_ll_identifier`:

- a `VK_SHIFT` KEYDOWN carrying `scanCode = 0xFF`
  (`SCAN_FLAG_KEYMAN_KEY_EVENT`, i.e. Keyman synthesized it), and
- **no matching KEYUP** anywhere after it.

**PASS**, with the fix in place, is all nine reported `up`, and no unmatched
synthesized KEYDOWN in the log. `ReconcileModifierCache` cleared the stale byte at
the top of the batch, before `keybd_shift_reset` could act on it.

Note what the fix does and does not do. It prevents the latch from ever forming,
because it runs before the first phantom press. It cannot *recover* a process that
has already latched: once the phantom KEYDOWN has been sent the modifier is
genuinely held at the OS, cache and OS agree, and there is nothing left for a
`GetAsyncKeyState` check to see.

## Recorded result

Reproduced and fixed, measured on the same machine with one variable changed:

| engine | freeze active | rules firing | modifier wedged |
|---|---|---|---|
| shipped build | 5/5 | 5/5 | **5/5 FAIL** |
| fixed build | 5/5 | 5/5 | **0/5 PASS** |

`host32.exe --probe 1x2x3x --wait-for-rule 120 --iterations 5`, Left Shift held and
released 1500 ms into the stall, Windows 11 Pro 26200. The fixed half of the pair
is recorded in [`evidence/host32-fixed-19.0.277.txt`](evidence/host32-fixed-19.0.277.txt).

## Recovery

A plain KEYUP clears it, and no press is needed — but it has to be the *same* event
shape Keyman injected, so this mirrors `do_keybd_event`: side-agnostic virtual key,
`KEYEVENTF_EXTENDEDKEY` for right Ctrl and right Alt, and `SCANCODE_RSHIFT` for
right Shift.

```ps1
Add-Type -Name Recover8064 -Namespace Probe -MemberDefinition '[DllImport("user32.dll")] public static extern void keybd_event(byte bVk, byte bScan, uint dwFlags, System.UIntPtr dwExtraInfo);'
$KEYEVENTF_EXTENDEDKEY = 0x01; $KEYEVENTF_KEYUP = 0x02
# vk, scan, extended
$ups = @(
  @(0x10, 0x2A, $false),  # VK_SHIFT   + SCANCODE_LSHIFT
  @(0x10, 0x36, $false),  # VK_SHIFT   + SCANCODE_RSHIFT
  @(0x11, 0x00, $false),  # VK_CONTROL            = left
  @(0x11, 0x00, $true),   # VK_CONTROL + extended = right
  @(0x12, 0x00, $false),  # VK_MENU               = left
  @(0x12, 0x00, $true)    # VK_MENU    + extended = right
)
foreach ($u in $ups) {
  $flags = $KEYEVENTF_KEYUP
  if ($u[2]) { $flags = $flags -bor $KEYEVENTF_EXTENDEDKEY }
  [Probe.Recover8064]::keybd_event([byte]$u[0], [byte]$u[1], [uint32]$flags, [UIntPtr]::Zero)
}
```

Ordinary physical typing does the same, which is why the bug appears to "fix
itself" once a user starts interacting, and why it has been so hard to catch in
the act. Restarting Keyman is the fallback.

The *typing* workaround is not available for a modifier the keyboard does not
physically have. `keybd_shift` emits a latched Right Ctrl as
`VK_CONTROL | KEYEVENTF_EXTENDEDKEY`, and only the exact matching KEYUP clears it;
tapping Left Ctrl does not. On a keyboard with no Right Ctrl key the user cannot
produce that event at all, which is the shape of the field reports that say the
symptom persists until a restart. The snippet above is not limited that way —
synthesizing the extended KEYUP is exactly what it is for.

## Hazards

Each of these has already produced a false result.

- **Do not test with a bare Alt press and release.** That is the Windows
  menu-activation gesture and gives a near-perfect impersonation of this bug with
  Keyman uninvolved. Use Left Shift.
- **Do not clear the test field with keystrokes.** `Ctrl+A` then `Delete` works on
  a clean machine and fails silently the instant the wedge fires: with Shift
  latched it arrives as `Ctrl+Shift+A` and `Shift+Delete`, the field is never
  emptied, and every later reading includes the whole accumulated buffer. Select
  and retype, or restart the host.
- **Compare output case-sensitively** if you do read the text as a secondary
  signal. PowerShell's `-eq`, `-ne` and `-match` are case-insensitive, so wedged
  output compares equal to clean output; the symptom *is* a case change. Use
  `-ceq` / `-cne`.
- **Do not run this against an application holding real data.** The host32 window
  is all the repro needs. Injected navigation keys sent without
  `KEYEVENTF_EXTENDEDKEY` insert characters instead of moving the caret, which has
  corrupted real documents during this investigation.
- **A negative result is not proof of a fix.** If step 5 was mistimed the stall and
  the KEYUP never coincided, and the test simply did not run. Repeat until
  `keyboard_ll_identifier` shows the Shift KEYUP genuinely missing from the log
  during the freeze window.

## Release step: run the opt-in interactive tests

A **named step of the release manual-test pass for Windows**, run alongside the
procedure above. It has to be run by a person, signed in at a real interactive
desktop. It is deliberately not part of the `test` action and is never run by CI:
a Session-0 CI service account has no input desktop, so the only thing CI could
report for these tests is a skip dressed up as a pass.

```
./windows/src/engine/keyman32/build.sh --debug test-interactive:x86
./windows/src/engine/keyman32/build.sh --debug test-interactive:x64
```

Run both architectures. Expected result is four tests PASSED on each.

### Preconditions for the run

- An **interactive logon session** — your own desktop. Not Session 0, not a service
  account, not a headless build agent, not an RDP session that has been
  disconnected.
- **No modifier key physically held** when the run starts, and no sticky or latched
  modifier left over from earlier work. A modifier that already reads down makes
  these tests **FAIL by design**, and the failure message says to let go and
  re-run. That is intended: a run that starts with Shift down cannot tell an
  injected press from the one already there, so passing would have told you
  nothing.

> [WARN] These tests inject real keystrokes into the desktop you are sitting at,
> for the duration of the run. Do not type and do not click into another window
> until they finish. Keystrokes you contribute land in the same input queue the
> tests are measuring.

### What each test measures

Each one measures a property of Windows that a comment or a test elsewhere in the
engine rests on. A red is a statement about what Windows now does, not merely that
something broke.

| `KEYBD_SHIFT.<name>` | what it measures |
|---|---|
| `FreshThreadKeyboardStateReflectsLiveModifiers` | what `InitThread`'s `GetKeyboardState` seed leaves in the modifier cache on a thread that has never pumped input |
| `ReconcileDoesNotRaceItsOwnInjectedRestorePress` | whether `SendInput` returns before the injected press is visible to `GetAsyncKeyState` — i.e. whether the reconcile can clear a byte whose press is still in flight |
| `DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot` | whether `dwExtraInfo` survives `SendInput` to a low level keyboard hook, and that the scan code cannot identify an injected Right Shift, because `do_keybd_event` rewrites it to `SCANCODE_RSHIFT` |
| `GenericShiftSendInputReflectsInBothAsyncKeyStates` | whether Windows re-chiralises a generic `SendInput(wVk=VK_SHIFT, wScan=0)` before the hook sees it, and whether `GetAsyncKeyState` agrees for the chiral VK |

### How to triage a red

**Re-run the failing test in isolation first, before reading anything else into
it.** `build.sh` does not forward gtest arguments, so run the built executable
directly. `Win32` for the x86 build, `x64` for the x64 build; `Debug` matches
`--debug`, `Release` without it:

```
./windows/src/engine/keyman32/tests/bin/Win32/Debug/keyman32.interactive.tests.exe --gtest_filter=KEYBD_SHIFT.<name>
```

All four have been observed going **red together on one run and passing in
isolation immediately afterwards**. Another process disturbing the input queue or
the hook round trip is enough to do that, and from a log alone it is
indistinguishable from a real regression. So:

1. Re-run the failure in isolation. Passes in isolation, and the whole target
   passes on a re-run: it was interference. Note it and move on.
2. Still red in isolation, repeatably: treat it as a real finding. Read that test's
   own comment for what has changed, and what in the engine was resting on it.
3. Red on the precondition check — a modifier already down — is not either of those.
   Let go of the keys and re-run.

### Why this target exists at all

These four used to sit on the default target in `tests/keybd_shift.tests.cpp`.
gtest 1.8.1 has no `GTEST_SKIP()`, so an absent capability could only be
`SUCCEED()` plus a warning log, and the four therefore reported **PASSED on every
CI run without asserting anything**. Moving them to their own target is what allows
an absent capability to be `FAIL()` instead, honest only because the target is
invoked where the capability is supposed to exist.

[NOTE] Adding these tests back to `keyman32.tests.vcxproj`, or wiring
`test-interactive` into the `test` action, would undo exactly that and put the
silent pass back. If CI coverage of these properties is ever wanted, it needs an
interactive test runner, not a target change.
