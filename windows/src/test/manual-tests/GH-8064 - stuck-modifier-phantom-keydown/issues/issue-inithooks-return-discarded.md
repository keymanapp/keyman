# `InitHooks()`'s return value is discarded at startup (the feed-integrity gap already flagged in `serialkeyeventcommon.h:209-217`)

Deliberately unfiled. Not one of the producers named in [`../TRIAGE.md`](../TRIAGE.md)
— this path emits nothing itself. It is the first of the three ways
`serialkeyeventcommon.h:209-217` names for the modifier cache's feed to be dead
while `flag_ShouldSerializeInput` still reads TRUE, so it sits behind that
gap's mitigation rather than beside it. It ships as text so the evidence travels with
the branch that found it rather than depending on a tracker round trip; filing stays
open to any maintainer, who can lift the body below verbatim. Nothing here is waiting
on that. See [README.md](../README.md).

---

**Title:** `Keyman_Initialise` discards `InitHooks()`'s return value and reports success, so a failed hook installation is silent and the serializer's modifier cache can run with no feed

**Body:**

`Keyman_Initialise` installs Keyman's hooks by calling `InitHooks()` at
`windows/src/engine/keyman32/keyman32.cpp:401` and ignores what it returns.
`InitHooks` (`keyman32.cpp:293-310`) is written to be checked: it installs
`WH_GETMESSAGE` (`:297`), `WH_CALLWNDPROC` (`:298`) and, on x86, the
`WH_KEYBOARD_LL` hook via `InitLowLevelHook` (`:300`, defined at `:276-281`), and
returns TRUE only when all three handles are non-NULL (`:303-309`). Nobody reads
that value. `Keyman_Initialise` continues to `*Globals::Keyman_Initialised() = TRUE`
(`:414`) and returns TRUE (`:419`) whether three hooks installed, one, or none.

The same discard occurs a second time in `Keyman_RestartEngine`
(`keyman32.cpp:474-475`), which calls `UninitHooks()` then `InitHooks()` and
returns TRUE unconditionally at `:479`.

**What is left behind on failure: only a debug log.** `keyman32.cpp:403-410`
prints the hook handles and a `GetLastError`, but that is `SendDebugMessageFormat`
output, absent unless debugging is enabled, and the `GetLastError` read at `:403`
comes after all three installs so it does not necessarily belong to the one that
failed. Nothing calls `SetLastError`, nothing posts to the master controller, and
`Keyman_GetInitialised` (`keyman32.cpp:271-274`) returns
`Globals::get_Keyman_Initialised()` — TRUE. Every in-process consumer of "is
Keyman initialised" therefore reads TRUE with hooks missing;
`RestartLowLevelHook` (`keyman32.cpp:482-486`) is itself one of them and gates on
exactly that flag.

**Why this matters for the modifier cache specifically: the low level keyboard
hook is its only feed.** The serializer's modifier cache is
`SerialKeyEventServer::m_ModifierKeyboardState`
(`windows/src/engine/keyman32/serialkeyeventserver.cpp:51`). It is seeded once, at
thread start, from `GetKeyboardState` (`:254`), and thereafter changed only by
`UpdateLocalModifierState` (`:581-585`), which is reached only from the three call
sites at `:535`, `:541` and `:562` — all inside the handler guarded by
`msg == WM_KEYMAN_KEY_EVENT || msg == WM_KEYMAN_MODIFIER_EVENT` (`:467`). In
production those two messages have exactly two posters, and both are inside the
low level keyboard hook procedure: `WM_KEYMAN_MODIFIER_EVENT` at
`windows/src/engine/keyman32/k32_lowlevelkeyboardhook.cpp:214`, and
`WM_KEYMAN_KEY_EVENT` from `PostKeyEventAndDecideEat`
(`windows/src/engine/keyman32/keybd_shift.cpp:522`), called at
`k32_lowlevelkeyboardhook.cpp:288`. No other production code posts either message.

So if `InitLowLevelHook` fails and nobody notices, the cache keeps its launch seed
and is never fed again — while `flag_ShouldSerializeInput` still reads TRUE
(default TRUE at `keyman32.cpp:231`, initialiser at
`windows/src/engine/keyman32/k32_globals.cpp:90-91`), and that flag is what
`PrepareInjectedInput` hands `PrepareInjectedInputBatch` as `feedIsConfigured`
(`serialkeyeventserver.cpp:403`). The parameter's purpose is to say whether the
cache feed is on, and it is being told "yes" when the feed does not exist. Its own
declaration comment already records this: `serialkeyeventcommon.h:209-217` names
`keyman32.cpp:401` as one of three same-process ways the feed can be dead with the
flag still TRUE.

**The concrete failure shape.** With `feedIsConfigured` TRUE,
`PrepareInjectedInputBatch` builds the release half of every injected batch from
the **live** OS state (`keybd_shift.cpp:311`) and the restore half from the
**cache** (`keybd_shift.cpp:324`). `ReconcileModifierCache`
(`keybd_shift.cpp:252-265`) clears but never sets. With no feed the cache can
therefore only decay towards empty, so a modifier the user is physically holding
is released by the release half and never re-pressed by the restore half: the hold
silently stops applying to anything Keyman injects until the user lets go and
presses it again. That is the opposite direction to #8064's own latch — an
unmatched KEYUP, not an unmatched KEYDOWN — and it is precisely the outcome
`feedIsConfigured = FALSE` exists to avoid, by making both halves read the same
(stale) cache. The batch path is reached independently of the low level hook: it
is signalled from the app-side output path,
`windows/src/engine/keyman32/appint/aiWin2000Unicode.cpp:235`, so batches do still
run in this state.

**Established from source, and no more than that.** What the code establishes is
the mechanism above: the discarded return value, the single feed, the seed-only
cache, and the `feedIsConfigured = TRUE` inconsistency that follows. What it does
not establish is a frequency or a field report. No recorded run caught `InitHooks`
returning FALSE, and no test forces it, so how often a hook install actually fails
in the field is not known here. Read this as a missing failure signal with a
demonstrable consequence, not as a measured user-facing defect.

**A partial recovery already exists, and its limits are the point.**
`LowLevelHookWatchDog` (`windows/src/engine/keyman32/LowLevelHookWatchDog.cpp`)
covers the never-installed case as well as the silent-removal case it was written
for: `LastLowLevelEventTick` starts at 0 (`:50`), so the first `WM_KEYDOWN` seen by
the `WH_GETMESSAGE` hook (`windows/src/engine/keyman32/kmhook_getmessage.cpp:155`
→ `windows/src/engine/keyman/UfrmKeyman7Main.pas:858` → `Keyman_WatchDogKeyEvent`,
`LowLevelHookWatchDog.cpp:103-106`) makes `CheckIfHookIsAlive` (`:72-79`) exceed
`WATCHDOG_THRESHOLD` (`:48`) and triggers `ReinstallHook` (`:81-100`). But:

- it repairs only the low level hook, never `WH_GETMESSAGE` or `WH_CALLWNDPROC`;
- it depends on the `WH_GETMESSAGE` hook having installed, so the case where
  `InitHooks` failed for all three is not covered at all;
- the window between startup and the first keystroke in a hooked thread is
  unbounded, and the cache is unfed throughout it;
- it cannot help where the install fails by construction rather than transiently.
  `InitLowLevelHook` passes `Globals::get_FSingleThread()` as
  `SetWindowsHookExW`'s thread id (`keyman32.cpp:279`), which is non-zero whenever
  `Keyman_Initialise` was called with `FSingleApp` TRUE (`keyman32.cpp:367`), and
  `WH_KEYBOARD_LL` can only be installed globally. `RestartLowLevelHook` calls the
  same function with the same argument, so it fails the same way on every attempt;
- and in every one of these cases the *initial* failure is still silent, which is
  what this issue is about. The watchdog can turn a permanent breakage into an
  intermittent one; it reports nothing at startup.

**Scope for a fix.** Smallest honest version: check `InitHooks()`'s return value at
`keyman32.cpp:401` and make the failure visible — `SetLastError` plus a master
controller notification in the shape `RestartLowLevelHook` already uses for
`WHR_INIT_FAILURE` (`keyman32.cpp:498`), so the existing reporting path is reused
rather than duplicated. Whether `Keyman_Initialise` should then *fail* is a product
decision and deliberately not proposed here: refusing to initialise because one
hook did not install may be worse for the user than running degraded, and settling
that needs a maintainer's judgement about which hooks are load-bearing. Worth
deciding in the same pass: whether `flag_ShouldSerializeInput` is the right thing
to hand `PrepareInjectedInputBatch` as `feedIsConfigured`, given that the flag
describes configuration and is being read as capability.

**Why no fix is attempted here.** The #8064 fix is scoped to paths that can emit an
unmatched modifier KEYDOWN, and this is not one of them — it emits nothing. It is
an initialisation-error-handling defect that predates that work, its remedy needs
the product decision above, and both candidate remedies change
`Keyman_Initialise`'s success contract, which every caller of the engine depends
on. Making that change alongside a modifier-latch fix would put an unrelated
regression risk into the same change.

**Ask:** decide whether a failed hook installation should be reported, should fail
initialisation, or both; and confirm whether passing `flag_ShouldSerializeInput` to
`PrepareInjectedInputBatch` as `feedIsConfigured` is intended, given the
distinction that parameter's own doc comment draws between "the feed is
configured" and "the feed is working".
