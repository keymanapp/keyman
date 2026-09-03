# Triage: which path stuck this modifier?

For [#8064](https://github.com/keymanapp/keyman/issues/8064). Companion to
[README.md](./README.md).

A user reports a modifier stuck down machine-wide. **Do not assume it is a
regression of the #8064 fix.** #8064's own repro is contrived, and the on-screen
keyboard can produce the identical symptom — including the unclearable Right
Control case that is the worst field report. This file tells the two apart.

Two producers stay unmitigated on every build, this one included, so a report
after this fix ships is triaged here rather than treated as a regression:
`keyman.exe` killed or crashed while an OSK sticky modifier is held, and
`PostKeys` pair-splitting under queue truncation. Drafts for both are in
[`issues/`](./issues/).

**Before either path, confirm the modifier is stuck at all.** A modifier that
went *dead* while the user was holding it is the opposite failure and draws the
same sentence out of a user — "my Ctrl key is broken". The oracle below answers
the stuck question and is silent on the other. A dead-while-held modifier works
again on the very next press, needs no recovery, and emits
`#8064 dropped hold:` naming the VK; a stuck one persists with no further input
at all and emits nothing. Dead-while-held is a deliberate accepted cost of the
batch's release/restore asymmetry, not a defect to file.

**Triage against the build the user has, not against this tree.** The OSK
behaves differently on a released build than on the branch, and several rows
below turn on that difference:

| | released builds, including 18.0.249 | with the OSK half landed |
|---|---|---|
| does any OSK dismissal run cleanup? | only the X button and a tab switch | every path |
| does cleanup release the right chiral VK? | no — by the current `kbd.LRShift` regime | yes — by the identity that was injected |
| does a manual click-off release the right chiral VK? | no | yes |
| `keyman.exe` killed while a sticky modifier is held | stranded | stranded |

**How far to trust each row.** `[measured]` verdicts come from an executed wedge
on a live engine. `[source-derived]` verdicts are what the source predicts, with
nothing yet checked against a real occurrence.

**Read the two warnings before you run anything.** *Turning the engine log on*
below carries both, and they cost a day between them: enabling the log can **stop
the defect reproducing**, and a signal looked for in the wrong function reads
exactly like a signal that is not there.

## First, before anything else

Confirm the symptom, and recover:

```powershell
# Oracle. Reads all nine, not just the six chiral VKs.
Add-Type -Name Km8064 -Namespace Probe -MemberDefinition '[DllImport("user32.dll")] public static extern short GetAsyncKeyState(int vKey);'
$mods = [ordered]@{ SHIFT=0x10; CTRL=0x11; ALT=0x12; LSHIFT=0xA0; RSHIFT=0xA1; LCTRL=0xA2; RCTRL=0xA3; LALT=0xA4; RALT=0xA5 }
foreach ($m in $mods.GetEnumerator()) {
  '{0,-7} {1}' -f $m.Key, $(if ([Probe.Km8064]::GetAsyncKeyState($m.Value) -lt 0) { 'DOWN  <-- held' } else { 'up' })
}
```

Record **which** modifier and whether it is chiral before you clear it — that is
signal, and the recovery sweep destroys it.

## The signals

| signal | serializer path | OSK path | discriminating? |
|---|---|---|---|
| **Is the OSK open right now?** | irrelevant | required at the time of the wedge | **yes** `[source-derived]`. Window class `TfrmVisualKeyboard` exists and is visible iff the OSK is open (`k32_visualkeyboardinterface.cpp:46-48`). But see the trap below |
| **Has the OSK been open earlier this session?** | — | — | **no, and it cannot be made to.** The form is *freed* on every dismissal path, so the window class disappears and nothing in the process records that it once existed. Structural, not a prediction |
| **`KL.Log` lines from `do_keybd_event` / `ShiftStateChange` / `tmrCheckTimer`** | absent — the serializer is C++ and logs through `SendDebugMessageFormat`/ETW, not `KL.Log` | **present and decisive, in a `KLOGGING` build only** | **yes on a rebuild, unavailable in the field** `[measured]`. `common/windows/delphi/general/klog.pas:26` reads `{DEFINE KLOGGING}` — a comment, the `$` is missing — so in any build you can download every `Log` body compiles to an empty procedure. A `keyman.exe` rebuilt with `{$DEFINE KLOGGING}` emits `ShiftStateChange:`, `ResetShiftStates:`, `kbdKeyPressed: keybd_event vk=%x scan=%x flags=%x` and `UpdateKeyboard: VKI…`. **Blind spot:** it instruments `keyman.exe` only. `keyman32.dll` injects from inside every hooked process through the C++ ETW path and is invisible to an `OutputDebugString` capture of `keyman.exe` |
| **`SendDebugMessageFormat` from `keybd_shift`, and `"cache says held but OS says up, clearing vkey=…"`** | present, naming the exact VK | absent — the OSK emits no `SendDebugMessage*` at all | **yes** `[measured]`, once enabled. 4 lines on the branch, 0 on the shipped build, each naming `['?LShift' 0xa0]` — the exact wedged VK — about 8 s after the phantom press |
| **Scan code of the injected modifier, as seen by the hook** | `0xFF` (`SCAN_FLAG_KEYMAN_KEY_EVENT`, from `keybd_shift_reset` and the release path) | `0` — the OSK passes `bScan = 0` on every call | **yes for five of the six, and the practical replacement for `KL.Log` in the field** `[source-derived]`. **Right Shift is the exception, and the signal is absent for it:** `do_keybd_event` overwrites `SCAN_FLAG_KEYMAN_KEY_EVENT` with `SCANCODE_RSHIFT`, because `0x36` is what tells the receiving app which Shift it was, so an injected Right Shift is byte-identical at the hook to a physical one. Second caveat, **half-answered** `[measured]`: Windows *can* propagate `bScan = 0` untouched, but does not always. So read it one way only: **`scan == 0` means a `bScan = 0` injector, which on a released build means the OSK; `scan != 0` exculpates nobody** |
| **`SendDebugMessageFormat` from the hook's modifier cache feed** | present: `"Modifier cache feed posted/failed/skipped [...]"`, distinguishing a successful post, a failed `PostMessage`, a `NULL` server window, and a Keyman-own event filtered out | absent | **yes** `[measured]`. 17 posts on the branch, 0 on the shipped build; every one carried `isUp` and a chiral `vkCode`, and all 17 matched the serializer's own view of the same event one-to-one. Only with this change |
| **`SendDebugMessageFormat("verification: OS holds vkey=%s that the cache says nobody holds, correcting", …)`** | present only when the post-batch verification pass actually corrects a disagreement | absent — the OSK does not reach this code path | **yes, but rare by design** `[source-derived, rare by design]`. **Looked for and not found**: 0 lines in 5 logged iterations, alongside 4 reconcile-clears in the same run — the expected shape, not a failure to fire. Its *absence* is uninformative; its *presence* is strong evidence the serializer's residual race fired. Only with this change |
| **`SerialKeyEventServer::WndProc` — `hwnd=… msg=… wParam=… lParam=… m_ModifierKeyboardState=[LS:… LC:… LA:… RS:… RC:… RA:…]`** | present, one line per key or modifier event reaching the re-injection, printing the VK, the flags word, **and the modifier cache itself** | absent — the OSK never reaches the serializer | **yes, and it is the one serializer-side signal a released build has** `[measured]`. It works on the build the user actually has, unlike every other row in this table. 92 passes in the shipped capture, 59 in the branch one. **Read `m_ModifierKeyboardState` directly:** a byte stuck at `80` across many consecutive events, with no matching KEYUP for it in the same log, *is* the wedge. x86 only (`serialkeyeventserver.cpp` is `#ifndef _WIN64`) |
| **Which modifier is stuck** | any of the six | chirally constrained by `kbd.LRShift`: `VK_SHIFT` (to Left Shift) plus, when `LRShift`, the four chiral Ctrl/Alt VKs; when not, `VK_CONTROL`/`VK_MENU` (both to left). **`kbd.LRShift` does not always follow the keyboard** `[measured]`: `UpdateKeyboard` pins it `True` whenever the OSK has no visual keyboard loaded (`VKI=nil`), a state that does not self-heal | **weak.** A wedged **Right Ctrl** does not point at the serializer: the OSK emits extended `VK_RCONTROL` directly, and `SetLRShift`'s chirality collapse can strand one even on hardware that has the key. Corroboration only, never alone |
| **Does closing the OSK clear it?** | no | **yes once the OSK half lands — every dismissal path, releasing the right chiral VK** | **yes** `[measured]`. Tray-menu dismiss, Character Map tab (`FormDestroy`) and the X button each emitted the matching KEYUP. The sharper question is not *whether* cleanup runs but *what* it releases — see the trap below |

### The trap: "closing the OSK didn't clear it" does not exculpate the OSK

Two separate things have to hold for a dismissal to clear a sticky modifier —
cleanup has to *run*, and it has to release the *right chiral VK* — and on a
released build neither does reliably.

**Reachability.** `ResetShiftStates` is reached only from
`TfrmVisualKeyboard.FormClose`, so only the X button and a tab switch run
cleanup; the tray menu, tray double-click, `KMC_ONSCREENKEYBOARD` and Keyman
shutdown all bypass it.

**Correctness.** Both release paths derive the VK from the *current*
`kbd.LRShift`, so after a `SetLRShift` collapse they release unextended `vk=11`
while the key actually held is extended `VK_RCONTROL`. The decisive trace is a
press of `vk=A3 flags=1` under an AltGr keyboard, a keyboard switch collapsing
`LRShift` True→False, and a release that still goes out as `vk=A3 flags=3` while
the engine's own view already reads the generic `essCtrl`.

Both are fixed by the OSK half, which is a separate PR.

So if the user reports "I closed the OSK and it stayed stuck", ask **how** they
closed it and **whether they clicked the modifier off by hand first**. On a
released build a manual click-off can leave the wrong chiral VK stuck even though
the later dismissal correctly tears down whatever `FCachedShiftState` still names
— the click already emptied or mislabelled it.

Two states survive teardown on **every** build: a `keyman.exe` killed or crashed
while a sticky modifier is held, and an OSK whose `VKI` has gone nil, which pins
`kbd.LRShift` True and can make the whole chirality story read backwards.

**Do not use a Keyman-only restart as a test. It is unmeasured, and it is
actively confusable.** The expectation is that a restart clears neither an
OSK-stranded modifier (nothing is persisted, there is no restore-on-start
reconciliation) nor a serializer-stranded one (the cache dies with the process,
but the OS still holds the key the dead process pressed). The trap is that
restarting `keyman.exe` *does* recover the separate `VKI=nil` fault, so a
responder can restart, watch something improve, and conclude the stuck modifier
cleared having measured a different fault entirely.

The test:

1. Switch the OSK to the **Character Map tab** (`UpdatePanels` calls
   `ResetShiftStates`), or
2. Close the OSK with its **own X button** (`MnuOSK_Close`), or
3. Dismiss it any other way (tray menu, tray double-click,
   `KMC_ONSCREENKEYBOARD`, Keyman quit).

If any of these clears the modifier, it was the OSK.

## Turning the engine log on

`SendDebugMessageFormat` is gated on `ShouldDebug()` (`keymanengine.h` to
`k32_dbg.cpp`) and routed to **ETW** by `Keyman_WriteDebugEvent2W`
(`DebugEventTrace.cpp`), which early-returns unless debug logging is enabled.

- `HKCU\Software\Keyman\Keyman Engine`, value `debug` = `1` (`REGSZ_Debug`) —
  enables the log.
- Same key, `debug to console` = `1` — also emits via `OutputDebugStringW`, which
  is far easier to capture.

Collect with `windows/src/support/etl2log`, or any `OutputDebugString` viewer.
The capture must be **process-wide**: `keyman32.dll` logs from inside every hooked
process, not just from `keyman.exe`.

The flags are read **once**, at `Keyman_Initialise`, so **Keyman must be restarted
after arming them.** Arming them under a running Keyman leaves `debug=FALSE` in
`.SHARDATA` and the engine logs nothing — a silence indistinguishable from a
signal that did not fire.

> ### The log can stop the defect reproducing `[measured]`
>
> Same shipped build, same machine, same harness, one variable — the log:
>
> | run | engine log | wedged |
> |---|---|---|
> | A1 | off | **5/5** |
> | A2 | off | **5/5** |
> | A3 | **on** | **0/5** |
>
> Turning the log on closes the race window.
> Two consequences for a responder:
>
> - **A wedge that stops reproducing once you enable the log has not been
>   diagnosed.** It is the most likely outcome for a serializer-path wedge, and it
>   is not evidence about the fix in either direction.
> - If you need to establish *whether* it wedges, run **unlogged** and read
>   `GetAsyncKeyState`. The log is for finding out *which path*, on a wedge you can
>   already produce.

**An absent log line is not proof of an absent event.** Three separate ways it
lies:

- `ShouldDebug_1` and `Keyman_WriteDebugEvent2W` both bail out when
  `ThreadGlobals()` is NULL.
- Three of the rows above exist **only with this change**. Absent on a released
  build means the code is not there, not that the event did not happen.
- **You may be looking in the wrong function.** One path was recorded as "not
  loggable" after reading `UpdateLocalModifierState`, which emits nothing — while
  the same captures already held 151 lines logging every pass through it, from the
  top of the enclosing `WndProc` 112 lines earlier. Check the callers before
  writing "absent".

## Procedure

1. Run the oracle. Record which modifier, and whether it is chiral.
2. Check for a visible `TfrmVisualKeyboard`. **OSK open** puts the OSK path in
   play; **not open** does not rule it out, because the form is freed on close.
3. Ask which build they are on, how the OSK was last dismissed, and whether they
   clicked the stuck modifier off by hand before dismissing. Also ask whether
   `keyman.exe` was killed, crashed, or restarted while the OSK was open.
4. If the OSK is open: switch it to the Character Map tab, or dismiss it any other
   way. If the modifier clears, it was the OSK. If it does not clear, do not
   conclude "not the OSK" without also checking step 3.
5. Otherwise enable the engine log — **reading the warning box in *Turning the
   engine log on* first; the log can stop the wedge reproducing** — reproduce, and
   read the scan code of the phantom KEYDOWN: `0xFF` means the serializer, `0`
   means the OSK, and `scan != 0` on its own rules nothing out. If you can
   rebuild, a `keyman.exe` compiled with `{$DEFINE KLOGGING}` is the better
   instrument for anything OSK-shaped.

   **On a released build, go straight to `SerialKeyEventServer::WndProc`.** It is
   the only serializer-side signal a shipped build has, and it prints the modifier
   cache on every pass. A byte pinned at `80` across consecutive events, with no
   KEYUP for it anywhere in the log, is the wedge — no rebuild required. x86 only.
6. Look for `"cache says held but OS says up, clearing vkey=…"`. Present means the
   serializer's reconcile is running and doing its job. Also look for
   `"verification: OS holds vkey=... correcting"` — present means the post-batch
   verification pass caught the OS still holding a modifier the cache had already
   cleared, which is a distinct, narrower signal than the reconcile line. It is
   rare by design and its absence tells you nothing.
7. Recover with the sweep in [README.md](./README.md).

## If it is the serializer path after all

That is the path this fix owns, and it is pinned by
`PREPARE_INJECTED_INPUT_BATCH.*` and
`PREPARE_MODIFIER_VERIFICATION_CORRECTION.*` in
`windows/src/engine/keyman32/tests/keybd_shift.tests.cpp` — those go red if the
reconcile or the verification pass is removed. So a genuine serializer-path
recurrence means a case neither test class models. Capture the scan code, the
modifier, and the log line — including whether the
`"verification: OS holds vkey=..."` line fired — and add the case before changing
any code.
