#pragma once

#include <windows.h>

// We permit up to 256 input events in a single transaction
// This allows roughly 120 characters to be output from a single
// Keyman rule, less a bit of space for modifier shenanigans
#define MAX_KEYEVENT_INPUTS 256

// Length of the KeymanModifierVks table (keymanengine.h).
#define KEYMAN_MODIFIER_VK_COUNT 6

// We need to reserve space for up to KEYMAN_MODIFIER_VK_COUNT modifier key events + 2 prefix
// key events at the end of the buffer in order to make sure that we can reset the modifier
// state at the end of the output. This value depends on keybd_shift behaviour.
#define MAX_KEYEVENT_INPUTS_MODIFIERS (KEYMAN_MODIFIER_VK_COUNT + 2)

// #8064 Truncation policy for one injected batch. MAX_KEYEVENT_INPUTS is the whole buffer and
// MAX_KEYEVENT_INPUTS_MODIFIERS is a reserve carved off the end of it, not a second buffer.
// PrepareInjectedInputBatch (keybd_shift.cpp) lays a batch down in three parts, in this order:
//
//   1. release half -- keybd_shift(FALSE): up to 6 modifier KEYUPs + a prefix down/up, so <= 8
//   2. output keys  -- copied from SerialKeyEventSharedData::inputs, in shared-buffer order
//   3. restore half -- keybd_shift(TRUE): up to 6 modifier KEYDOWNs + a prefix down/up, so <= 8
//
// Parts 1 and 2 share the first MAX_KEYEVENT_INPUTS - MAX_KEYEVENT_INPUTS_MODIFIERS slots
// (256 - 8 = 248); the last 8 belong to part 3 alone. The output-key loop's bound is on the
// running total, never on the output count, so the release half spends from that same 248: with
// all six managed modifiers held it emits 8 events and only 240 output keys fit. Output keys that
// do not fit are dropped TAIL-FIRST -- the loop walks the shared buffer from index 0 and stops the
// moment the total reaches 248, so what is sent is always a prefix of what the keyboard asked for:
// never a hole in the middle, never a reordering. Tail-first is the deliberate choice, not an
// accident of the loop shape. A short output is recoverable; a short wrap is the bug. Dropping the
// tail leaves the release and restore halves paired, which is the whole invariant of #8064, while
// dropping or shortening part 3 to make room would strand a modifier down -- exactly the stuck
// modifier this work exists to remove.
//
// So the reserve is never encroached, and the worst case fills the buffer EXACTLY: 248 + 8 = 256.
// That exactness means an off-by-one in the loop bound is a heap overrun rather than a failing
// test, so it is pinned twice in keybd_shift.tests.cpp:
//
//   NeverWritesPastTheBufferWhenTheSharedBufferOverflows -- drives the worst case and asserts
//     n == MAX_KEYEVENT_INPUTS.
//   WorstCaseBatchFillsTheBufferToItsLastSlotAndNotOneFurther -- asserts the same total against
//     literals (8 + 240 + 8), so it does not move when the constants do, and writes into a
//     guarded over-allocation so that a too-small reserve is RECORDED rather than committed.
//     The older test writes into an exactly-256 array, so under that mutation it would itself
//     overrun before it could report -- which is why the boundary is pinned from both sides.
//
// Truncation at 248 is silent: no debug message, and no way for the caller to tell. The only
// logged clamp is the earlier one in SerialKeyEventClient::SignalServer (serialkeyeventclient.cpp),
// and it caps at 256, so a batch of 249..256 output keys is truncated here and reported nowhere.
// The min(pSharedData->nInputs, MAX_KEYEVENT_INPUTS) at the top of PrepareInjectedInputBatch is not
// this policy either: it guards the READ of pSharedData->inputs[i], since nInputs lives in shared
// memory and an over-large value would walk off the end of a 256-element array.

#define KEYEVENT_WINDOW_CLASS "Keyman_KeyEventConsumerWnd"

#define GLOBAL_FILE_MAPPING_NAME "KeymanEngine_KeyEvent_FileMapping"
#define GLOBAL_KEY_EVENT_NAME "KeymanEngine_KeyEvent"
#define GLOBAL_KEY_MUTEX_NAME "KeymanEngine_KeyMutex"

/**
WM_USER private messages -- used only for communication 
between low level keyboard hook and serial key event server
*/
#define WM_KEYMAN_KEY_EVENT (WM_USER + 1)
#define WM_KEYMAN_MODIFIER_EVENT (WM_USER + 2)

/**
  #8064 Posted by the serial key event server to itself after a batch's SendInput returns, when the
  restore half pressed at least one modifier. wParam is PrepareInjectedInputBatch's
  pRestorePressedMask, consumed by PrepareModifierVerificationCorrection.

  Posted, never an inline check after SendInput: posted messages are FIFO, so by the time this is
  dispatched, every modifier event the hook posted earlier -- including a user release that raced
  the batch -- has reached the cache. Inline, those are still undispatched in this thread's queue,
  since nothing pumps it until the current DispatchMessage returns.
*/
#define WM_KEYMAN_VERIFY_MODIFIER_EVENT (WM_USER + 3)

/**
  The INPUT structure and the KEYBDINPUT structure both vary in size between x86 and x64
  because of the presence of the ULONG_PTR member dwExtraInfo. Thus we need to maintain an
  equal sized structure between the two platforms for shared memory, and copy into INPUT
  structures before sending the input.
*/
struct CSDINPUT {
  WORD      wVk;
  WORD      wScan;
  DWORD     dwFlags;
  DWORD     time;
  ULONGLONG extraInfo;
};

struct SerialKeyEventSharedData {
  DWORD nInputs;
  CSDINPUT inputs[MAX_KEYEVENT_INPUTS];
};

// Live modifier-state reader. A function pointer, not a mock: gmock is not linked into
// keyman32.tests.vcxproj, so the tests bind a file-local stub.
typedef SHORT (WINAPI *PGETASYNCKEYSTATE)(int vKey);

// #8064 The message-post seam. Same shape and same reason as PGETASYNCKEYSTATE above: a plain
// function pointer, because gmock is not linked into keyman32.tests.vcxproj. Production passes
// PostMessage; the suite binds a file-local stub that records the post and can be made to fail, so
// PostKeyEventAndDecideEat's eat-only-on-a-successful-post decision is exercisable without a
// message queue, a window or an interactive desktop.
typedef BOOL (WINAPI *PPOSTMESSAGEFN)(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

/**
  #8064 FR-002 / FR-006 -- what a batch reports about a hold it could not keep.

  FR-001 governs the behaviour: absent a signal that says what the USER holds, the release and
  restore halves do not change, so some holds are still dropped. Both live-state-only alternatives
  are refuted in the spec. What FR-002 and FR-006 add is that a dropped hold stops being SILENT:
  the batch names it, so a field report has something to match against instead of a user sentence.

  A CODE, NOT A FORMAT STRING, and that is the whole point of the type. The suite asserts on a
  value, so the wording of the message can be rewritten by anyone at any time without silently
  turning the assertion into a tautology. A test that greps prose is a test that a later edit
  disarms without anyone noticing.

  A plain function pointer, same shape and same reason as PGETASYNCKEYSTATE and PPOSTMESSAGEFN
  above: gmock is not linked into keyman32.tests.vcxproj. Production binds a thunk over
  SendDebugMessageFormat (serialkeyeventserver.cpp); the suite binds a recorder, because
  SendDebugMessageFormat resolves to ETW (K32_DBG.CPP:189) and the suite has no way to read that
  back. So FR-002 and FR-006 are both assertable with NO machine, no desktop and no timing -- which
  is the reason the seam exists rather than a debug string being good enough.
*/
enum ModifierDiagnosticCode {
  /**
    FR-002. The release half is about to release a modifier that the LIVE state says is held and
    the CACHE does not claim -- so the restore half, which reads the cache, will not press it back.
    That is a hold this batch drops, and it is the residue outside any signal's reach: a console
    window or the secure desktop, where the cache feed never saw the KEYDOWN.

    Emitted once per such VK, naming it. Only meaningful when feedIsConfigured is TRUE, because with
    the feed off both halves read the same kbd and the condition cannot arise.
  */
  ReleasedWithoutCacheClaim = 1,

  /**
    FR-006 / FR-007. All six managed modifiers read UP live while the cache claims two or more
    held. The reconcile is about to clear every one of them and the restore half will press nothing,
    so every one of those holds is dropped at once. Two or more, not one, is deliberate: exactly one
    modifier held at launch and released before the feed was live is the normal launch-seed case
    002 exists to clear, and firing there would cry wolf on every session.

    Emitted ONCE PER BATCH, with vk = 0. Not once per lost key, and the reason is worth stating
    because the per-key form looks more useful and is not: ReconcileModifierCache already logs
    "cache says held but OS says up, clearing vkey=..." for each byte it clears, and in this state it
    clears exactly the claimed set. So the per-key detail is in the log either way, immediately
    after this line, and emitting it twice would only make the batch-level condition harder to find.
    This code says "the batch as a whole looks like a desktop switch"; the lines under it say which
    keys.

    THE RECONCILE STILL RUNS -- see FR-007a at the reconcile call in keybd_shift.cpp. This is a
    report, never a suppression.
  */
  PossibleDesktopSwitch = 2,
};

typedef void (*PMODIFIERDIAGNOSTIC)(ModifierDiagnosticCode code, BYTE vk);

/**
  #8064 W5 / FR-100a, FR-101, FR-104. What a source that is NOT KEYMAN'S OWN last said about each
  managed modifier.

  A SECOND SHADOW, NOT A SECOND CACHE, and the distinction is the whole design. The modifier cache
  (kbd) records what Keyman's own hook feed saw. This records what a raw-input feed saw. Neither is
  the OS's live state, and that exclusion is deliberate: live state cannot tell "the user is holding
  it" from "we pressed it ourselves", which is why restoring from it reopens #8064 from a new
  direction (FR-102).

  Three properties, and every one of them is load-bearing:

  1. IT REPORTS UNKNOWN RATHER THAN STALE. `unknown[vk]` is set wherever the signal cannot currently
     speak for that key -- the feed is not established yet, the active desktop is not the user's, the
     session changed, or the registration was displaced. A stale "held" is the sharpest risk in this
     whole design: it manufactures an unmatched press, which is #8064 arriving from the one direction
     ReconcileModifierCache is structurally blind to, because cache and OS then AGREE.

  2. THE EFFECTIVE READ IS ALWAYS `held & ~unknown`. No consumer may read `held` alone. There is no
     accessor enforcing that -- it is a discipline, and it is stated here because breaking it is
     silent.

  3. IT IS NEVER AUTHORITATIVE ALONE. The restore set is `cache OR (held & ~unknown)`; the signal
     only ever WIDENS what the cache already justifies. It can add a press the cache missed. It can
     never veto one the cache asserts.

  Poison is PER KEY and clears ONLY on a fresh observation of that key -- never on a timer. A timer
  would decide that a key is knowable again because time passed, which is exactly the reasoning that
  makes a stale shadow dangerous. See PoisonUserHeldKeys.

  256 bytes each rather than KEYMAN_MODIFIER_VK_COUNT, so a VK indexes directly and no consumer has
  to map through KeymanModifierVks to read it -- the same shape, and the same reason, as kbd.
*/
struct UserHeldModifierSignal {
  BYTE held[256];    // 0x80 where the signal last observed this key DOWN
  BYTE unknown[256]; // 0x80 where the signal CANNOT currently report on this key
};

// Defined in keybd_shift.cpp, outside any _WIN64 guard so both architectures and the gtest project
// can reach it. #8064 added feedIsConfigured and pRestorePressedMask, both defaulted so existing
// call sites are unaffected; see the doc comment there.
//
// feedIsConfigured means "the hook's cache feed is configured on". It does NOT mean "the feed is
// working", and nothing here can tell the caller that it is. Production passes
// flag_ShouldSerializeInput, which reads TRUE while the feed is dead in at least three same-process
// ways: InitHooks()'s return value is discarded (keyman32.cpp:401), FSingleApp=TRUE makes the
// global-only LL install structurally fail (keyman32.cpp:367,279), and Windows' silent hook removal
// at 200 ms is not detected until a later keystroke sees a >=1000 ms gap (LowLevelHookWatchDog.cpp).
// So TRUE is a claim about configuration only; read as "the cache is current" it will justify
// trusting a cache that has not been updated since its launch seed. FALSE is the safe degradation,
// not a diagnosis: it makes the release and restore halves read the same kbd, stale or not.
int PrepareInjectedInputBatch(
  LPINPUT pInputs,
  LPBYTE const kbd,
  const SerialKeyEventSharedData *pSharedData,
  PGETASYNCKEYSTATE pfnGetAsyncKeyState,
  BOOL feedIsConfigured = TRUE,
  DWORD *pRestorePressedMask = NULL,
  PMODIFIERDIAGNOSTIC pfnDiagnostic = NULL,
  int *pRestoreEventIndex = NULL,
  const UserHeldModifierSignal *pUserHeld = NULL);

// #8064. Defined in keybd_shift.cpp; see its doc comment and WM_KEYMAN_VERIFY_MODIFIER_EVENT above.
int PrepareModifierVerificationCorrection(
  LPINPUT pInputs,
  LPBYTE const kbd,
  DWORD restorePressedMask,
  PGETASYNCKEYSTATE pfnGetAsyncKeyState,
  const UserHeldModifierSignal *pUserHeld = NULL);


