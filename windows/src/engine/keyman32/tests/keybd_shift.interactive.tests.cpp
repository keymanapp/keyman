#include "pch.h"
#include "kbd.h"                  // SCANCODE_RSHIFT
#include "serialkeyeventcommon.h" // MAX_KEYEVENT_INPUTS

/*
  #8064 FR-023 -- THE OPT-IN INTERACTIVE TARGET. The four probes in this file inject real keyboard
  input and read it back. They need an interactive input desktop, and two of them additionally need
  that desktop to route keyboard messages to a pumped, hooked message queue. A Session-0 service
  account -- which is what CI runs as -- has none of that.

  They used to live in keybd_shift.tests.cpp, on the default target, where an absent capability was
  SUCCEED() plus a WARNING log: gtest 1.8.1 has no GTEST_SKIP(), so that was the only skip available.
  The cost was that the four reported PASSED on every CI run without asserting anything, and the
  tally could not tell a real pass from a skip. FR-022 and SC-005 forbid exactly that.

  So this file is a separate binary (keyman32.interactive.tests.vcxproj) built and run only by
  `build.sh test-interactive:x86` / `test-interactive:x64`, which is deliberately NOT part of the
  `test` action. See the README of
  windows/src/test/manual-tests/GH-8064 - stuck-modifier-phantom-keydown/ for the release step that
  names it.

  THE RULE THAT MAKES THIS FILE WORTH SEPARATING -- an absent capability here is FAIL(), not
  SUCCEED(). That inversion is only correct because of where this target is invoked: a person runs
  it on a desktop where the capability is supposed to exist, so "the capability was not there" is a
  real result about this machine and not an environment the test has to tolerate. The same rule
  covers an unmet precondition (a modifier already reading down, i.e. the operator is holding a
  key): failing tells them to let go and re-run, where a pass would have told them nothing.

  Nothing in this file is a unit test. Each one measures a platform property that a comment or a
  test elsewhere rests on, and says in its own comment what has changed if it ever goes red. Triage
  a red here by re-running the failures in isolation first: another process disturbing the input
  queue or the hook round trip is enough to turn all four red at once (observed 2026-08-27), and
  from a log alone that is indistinguishable from a real regression.

  The fixture is a lean copy of keybd_shift.tests.cpp's KEYBD_SHIFT, deliberately: the name is kept
  so each test's full name survives the move unchanged, and only the members these four use are
  carried over.
*/

// Globals::InitSettings (k32_globals.cpp) seeds the prefix virtual key from _VK_PREFIX_DEFAULT
// (appint/aiTIP.h) or from the registry, and Globals_InitProcess does not call it. Pin it here
// rather than depend on the machine's registry. Same value as keybd_shift.tests.cpp.
static const BYTE PREFIX_VK = 0x0E;

class KEYBD_SHIFT : public ::testing::Test {
public:
  KEYBD_SHIFT() {}

  void
  SetUp() {
    // keybd_shift's SendDebugEntry / SendDebugMessageFormat macros reach ThreadGlobals(), which
    // enters csGlobals; that critical section is only ever initialised by Globals_InitProcess.
    Globals_InitProcess();
    Globals::set_vk_prefix(PREFIX_VK);

    Rewind();
  }

  void
  TearDown() {
    Globals_UninitProcess();
  }

  ~KEYBD_SHIFT() {}

protected:
  BYTE kbd[256];

  void
  Rewind() {
    memset(kbd, 0, sizeof(kbd));
  }
};

namespace {
struct SeedProbeResult {
  BOOL getKeyboardStateOk;
  BYTE keyboardStateByte; // GetKeyboardState's byte for VK_LSHIFT, from the fresh thread
  SHORT asyncState;       // GetAsyncKeyState's answer for the same key, from the same thread
};

// A thread that has never pumped input -- the situation InitThread seeds the cache from.
DWORD WINAPI
SeedProbeThread(LPVOID param) {
  SeedProbeResult *r = (SeedProbeResult *)param;
  BYTE state[256];

  // 0xCC so a GetKeyboardState that writes nothing is distinguishable from one that writes zeroes.
  memset(state, 0xCC, sizeof(state));

  r->getKeyboardStateOk = GetKeyboardState(state);
  r->keyboardStateByte  = state[VK_LSHIFT];
  r->asyncState         = GetAsyncKeyState(VK_LSHIFT);
  return 0;
}
} // namespace

/*
  PROBE CAPABILITY -- shared by the four tests below, and referenced by name from each.

  Each one checks for the capability it needs with its own real mechanism, not with a static
  OpenInputDesktop-style proxy: nothing short of the round trip itself proves the round trip works
  from this process. What changed with the move to this target is what happens when the check comes
  back negative. On the default target it was SUCCEED() plus a WARNING, because a Session-0 CI
  account genuinely cannot do this and a red there would have been noise. Here it is FAIL(): this
  target is invoked by a person on a desktop where the capability is supposed to exist, so a
  negative check is a finding about that machine and the tally must say so.

  One is not quite its own mechanism: FreshThreadKeyboardStateReflectsLiveModifiers probes on the
  calling thread and asserts on a spawned one. That holds because GetAsyncKeyState is desktop-global,
  unlike GetKeyboardState/GetKeyState which read the calling thread's processed queue -- so seeing
  the injected press here is sufficient evidence the fresh thread would see it too.

  Every capability check releases what it injected before reporting, whichever way it goes, so a
  FAIL() never leaves a modifier asserted machine-wide. That was true when the check skipped and it
  has to stay true now that it fails.

  The check covers an ABSENT capability, not an intermittently disturbed one, and that distinction is
  the whole triage procedure. Observed on 2026-08-27: these four went red together on one x64 run,
  passed in isolation immediately after, and the whole suite passed on the next run -- another
  process disturbing the input queue or the hook round trip is enough. Re-run a red in isolation
  before believing it.
*/

/*
  Measures what InitThread's GetKeyboardState seed leaves in the cache. It reads the calling
  thread's processed queue, so on a thread that has never pumped input it looks like it should
  return nothing. It returns live state:

    this thread : GetKeyboardState ok=1 byte=0x00, GetAsyncKeyState=0x8001
    fresh thread: GetKeyboardState ok=1 byte=0x81, GetAsyncKeyState=0x8000

  See PROBE CAPABILITY. If a future Windows stops seeding a fresh thread from live state,
  InitThread's seed is a no-op again and a modifier held at launch is invisible to the cache until
  the user's next press or release of it. That does not reopen #8064 -- ReconcileModifierCache only
  ever clears -- but the cache's launch-time state is wrong until the first real event.
*/
TEST_F(KEYBD_SHIFT, FreshThreadKeyboardStateReflectsLiveModifiers) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0) {
    FAIL() << "Left Shift already reads down, so this run cannot tell an injected press from the "
           << "one already there. Let go of every modifier and run this target again.";
    return;
  }

  // Assert Left Shift for real, so there is something for the seed to find. This doubles as the
  // capability probe below: if it is not observable here, nothing downstream can be measured.
  keybd_event(VK_LSHIFT, 0, 0, 0);
  Sleep(150);

  const SHORT callerAsync = GetAsyncKeyState(VK_LSHIFT);

  if (callerAsync >= 0) {
    // The environment cannot do what this test needs -- most likely no interactive input desktop
    // (a Session-0 CI service account), possibly something else entirely. Either way, continuing
    // would fail EXPECT_LT(r.asyncState, 0) below for a reason that has nothing to do with whether
    // GetKeyboardState's seed behaviour has regressed, which is a false failure and worse than no
    // coverage. Release defensively (harmless whether or not anything actually landed) and skip.
    keybd_event(VK_LSHIFT, 0, KEYEVENTF_KEYUP, 0);
    FAIL() << "keybd_event's injected press was not observable via GetAsyncKeyState in this "
           << "process, so nothing below could be measured. On the default target this was a skip; "
           << "on this target it is a failure, because this target is only invoked where an "
           << "interactive input desktop is supposed to exist. See PROBE CAPABILITY.";
    return;
  }

  BYTE callerState[256];
  memset(callerState, 0xCC, sizeof(callerState));
  const BOOL callerOk = GetKeyboardState(callerState);

  SeedProbeResult r;
  memset(&r, 0, sizeof(r));
  HANDLE hThread = CreateThread(NULL, 0, SeedProbeThread, &r, 0, NULL);
  ASSERT_NE(hThread, (HANDLE)NULL);
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);

  // Release it again in the same run, whatever the measurement showed.
  keybd_event(VK_LSHIFT, 0, KEYEVENTF_KEYUP, 0);
  Sleep(150);

  printf("SEED PROBE  this thread : GetKeyboardState ok=%d byte=0x%02X, GetAsyncKeyState=0x%04X\n",
         (int)callerOk, (int)callerState[VK_LSHIFT], (unsigned short)callerAsync);
  printf("SEED PROBE  fresh thread: GetKeyboardState ok=%d byte=0x%02X, GetAsyncKeyState=0x%04X\n",
         (int)r.getKeyboardStateOk, (int)r.keyboardStateByte, (unsigned short)r.asyncState);
  printf("SEED PROBE  after release: GetAsyncKeyState=0x%04X\n", (unsigned short)GetAsyncKeyState(VK_LSHIFT));

  EXPECT_TRUE(r.getKeyboardStateOk) << "GetKeyboardState failed on the fresh thread";
  EXPECT_LT(r.asyncState, 0) << "GetAsyncKeyState on the fresh thread did not see the held key; "
                             << "the injection did not land and nothing was measured";
  // The measured result, not the hypothesis this probe was written to confirm. If this ever fires,
  // GetKeyboardState's behaviour for a queue-less thread has changed and the documentation this
  // probe backs -- the README's seed paragraph and the InitThread comment -- must be re-measured.
  EXPECT_NE(r.keyboardStateByte & 0x80, 0)
      << "GetKeyboardState on a thread that has never pumped input reported Left Shift up. "
      << "That was the original expectation and it is not what this machine does; if it is now "
      << "true, the seed is a no-op after all and the seed documentation needs revisiting.";

  EXPECT_EQ(callerState[VK_LSHIFT] & 0x80, 0)
      << "the calling thread's GetKeyboardState saw the injected key, so this run does not "
      << "demonstrate the queue-dependence the fresh-thread reading is being compared against";

  EXPECT_GE(GetAsyncKeyState(VK_LSHIFT), 0) << "the probe left Left Shift asserted machine-wide";
}

namespace {
// Injects one batch the way the restore half does: fillerCount inert events, then a KEYDOWN for
// vk, in a single SendInput call, so vk's press is queued behind the filler exactly as a real
// batch queues it behind the release half and the output keys.
bool
InjectRestorePress(BYTE vk, int fillerCount) {
  INPUT batch[MAX_KEYEVENT_INPUTS];
  int m = 0;

  memset(batch, 0, sizeof(batch));

  // KEYUPs for unassigned function keys that are not down: queue depth without side effects.
  for (int i = 0; i < fillerCount; i++, m++) {
    batch[m].type       = INPUT_KEYBOARD;
    batch[m].ki.wVk     = (WORD)(VK_F13 + (i % 8));
    batch[m].ki.wScan   = SCAN_FLAG_KEYMAN_KEY_EVENT;
    batch[m].ki.dwFlags = KEYEVENTF_KEYUP;
  }

  batch[m].type     = INPUT_KEYBOARD;
  batch[m].ki.wVk   = vk;
  batch[m].ki.wScan = SCAN_FLAG_KEYMAN_KEY_EVENT;
  m++;

  return SendInput(m, batch, sizeof(INPUT)) == (UINT)m;
}

// Returns the number of spins the release took to become visible, or kSettleSpinLimit if it never
// did. The count is reported, because a settle that consistently runs to the limit is the difference
// between "the measurement is slow" and "the injected release is not landing at all" -- and without
// it a slow run and a wedged one look identical.
const int kSettleSpinLimit = 200000;

int
ReleaseAndSettle(BYTE vk) {
  int i = 0;
  keybd_event(vk, 0, KEYEVENTF_KEYUP, 0);
  for (; i < kSettleSpinLimit && GetAsyncKeyState(vk) < 0; i++) {
    Sleep(0);
  }
  return i;
}
} // namespace

/*
  Can ReconcileModifierCache race a press its own previous batch injected? If the restore KEYDOWN
  had not yet reached GetAsyncKeyState, the reconcile would clear a byte the user genuinely holds.
  The race itself, not a proxy: production function, production reader, no delay after a SendInput
  that queues the press behind filler -- tighter than any real batch sequence.

  It does not happen. SendInput does not return until the press is visible (0x8001), at every batch
  depth reached. Measured 0 races and 0 stale reads in every trial that has run, on Windows 11 Pro
  26200, debug x86 and x64, with Keyman running so its WH_KEYBOARD_LL hook was in the chain
  throughout.

  HOW MANY TRIALS IS NOT FIXED, AND THE PRINTED N IS THE REAL ANSWER. An earlier form of this test
  asked for a flat 100 trials at each of three depths and, on the machine above, never finished: it
  ran past 26 minutes without printing one line. That was not a wedge. SendInput through a loaded
  low level hook chain costs roughly 110ms PER EVENT IN THE BATCH on that machine -- 121ms measured
  at depth 1, 3.57s at depth 33 -- so 100 trials at depth 201 is about six hours of wall clock, and
  the flat count was never achievable there. The cost is a property of the hook chain, not of
  SendInput: it is the same round trip the engine pays for every injected batch.

  So each depth now gets a wall-clock BUDGET and runs as many trials as fit, reporting the count it
  achieved. A budget-bound N is an honest statistical statement; a hang is not a statement at all,
  and a fixed N nobody can reach is a test that never runs. The test fails if a depth cannot manage
  kMinTrials, because that means nothing was measured there -- and it says so in those words rather
  than blaming the platform.

  An oracle, not just a measurement: it goes red if a future Windows returns from SendInput before
  the state is visible. The consequence then is a modifier dropped for one batch, self-correcting on
  the user's next physical press; the fix would be to skip the reconcile for a modifier this
  process's own previous batch pressed, capped at one consecutive skip per VK.

  Minutes long and timing-sensitive, unlike anything else in this file, and it covers something no
  other test here can: every other test drives CaptureLiveModifierState and ReconcileModifierCache
  through the stub reader, which cannot express a timing race at all. See PROBE CAPABILITY.

*/
TEST_F(KEYBD_SHIFT, ReconcileDoesNotRaceItsOwnInjectedRestorePress) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0) {
    FAIL() << "Left Shift already reads down, so every trial below would start from the wrong "
           << "state. Let go of every modifier and run this target again.";
    return;
  }

  // Capability probe: one trial of the exact mechanism the measurement below depends on, checked
  // immediately, before committing to the rest. Not a static desktop check -- the same
  // reasoning as FreshThreadKeyboardStateReflectsLiveModifiers's probe applies here too.
  {
    const bool injected = InjectRestorePress(VK_LSHIFT, 0);
    const bool landed   = injected && GetAsyncKeyState(VK_LSHIFT) < 0;
    ReleaseAndSettle(VK_LSHIFT); // harmless whether or not anything actually landed

    if (!landed) {
      FAIL() << "SendInput's injected press was not observable via GetAsyncKeyState in this "
             << "process, so the race this test measures could not be measured at all. This target "
             << "is only invoked where an interactive input desktop is supposed to exist. See "
             << "PROBE CAPABILITY.";
      return;
    }
  }

  LARGE_INTEGER freq;
  QueryPerformanceFrequency(&freq);

  // An empty batch, a typical one, and a nearly full one. The depths are the point -- the question
  // is whether a press queued behind filler is visible by the time SendInput returns -- so the
  // depths are fixed and the trial count is what gives way when the machine is slow.
  const int fillers[] = { 0, 32, 200 };

  // Per depth: at most kMaxTrials, for at most kDepthBudgetMs, and never fewer than kMinTrials or
  // that depth measured nothing and the test fails. See this test's comment for why the count is
  // budget-bound and not fixed.
  const int kMaxTrials       = 100;
  const int kMinTrials       = 3;
  const DWORD kDepthBudgetMs = 100000;

  // Unbuffered, so a run that stops mid-measurement still says where it stopped. Buffered printf is
  // part of why the original overrun presented as total silence.
  setvbuf(stdout, NULL, _IONBF, 0);

  bool starvedDepth = false;

  for (int f = 0; f < _countof(fillers) && !starvedDepth; f++) {
    int races = 0, staleReads = 0;
    int settleSpinsMax = 0, settleSpinsAtLimit = 0;
    double sendUs = 0.0;
    int done = 0;

    const DWORD depthStartedAt = GetTickCount();

    for (int i = 0; i < kMaxTrials; i++) {
      LARGE_INTEGER t0, t1;

      if (done >= kMinTrials && GetTickCount() - depthStartedAt > kDepthBudgetMs) {
        break;
      }

      Rewind();
      kbd[VK_LSHIFT] = 0x80; // as a hook KEYDOWN left it, with the user still holding

      QueryPerformanceCounter(&t0);
      ASSERT_TRUE(InjectRestorePress(VK_LSHIFT, fillers[f])) << "SendInput did not queue the batch";
      QueryPerformanceCounter(&t1);
      sendUs += (double)(t1.QuadPart - t0.QuadPart) * 1e6 / (double)freq.QuadPart;

      // What the OS reports the instant SendInput returns, before the reconcile reads anything.
      if (GetAsyncKeyState(VK_LSHIFT) >= 0) {
        staleReads++;
      }

      // The production question, asked with no delay: does the reconcile clear a byte whose press
      // is still in flight?
      BYTE live[256];
      CaptureLiveModifierState(live, GetAsyncKeyState);
      if (ReconcileModifierCache(kbd, live)) {
        races++;
      }

      const int spins = ReleaseAndSettle(VK_LSHIFT);
      if (spins > settleSpinsMax) {
        settleSpinsMax = spins;
      }
      if (spins >= kSettleSpinLimit) {
        settleSpinsAtLimit++;
      }
      done++;

      if (done == 1 || done % 10 == 0) {
        printf("RACE PROBE  filler=%3d  %d trials, %ums in this depth, settle spins max=%d at-limit=%d\n",
               fillers[f], done, (unsigned)(GetTickCount() - depthStartedAt), settleSpinsMax,
               settleSpinsAtLimit);
      }
    }

    // The per-EVENT cost, not the per-call cost: it is what makes the trial count small, and it is
    // the number to compare across machines. filler+1 events go out per trial.
    const double meanCallUs  = done ? sendUs / (double)done : 0.0;
    const double meanEventUs = meanCallUs / (double)(fillers[f] + 1);

    printf("RACE PROBE  filler=%3d  races=%d/%d  staleReads=%d/%d  SendInput mean=%.0fus/call "
           "%.0fus/event  settle spins max=%d at-limit=%d/%d\n",
           fillers[f], races, done, staleReads, done, meanCallUs, meanEventUs, settleSpinsMax,
           settleSpinsAtLimit, done);

    if (done < kMinTrials) {
      starvedDepth = true;
      break;
    }

    EXPECT_EQ(staleReads, 0)
        << "GetAsyncKeyState did not see the injected press by the time SendInput returned. "
        << "SendInput is no longer synchronous with respect to the async key state, so the "
        << "reconcile can now clear a modifier the user is holding. See this test's comment";
    EXPECT_EQ(races, 0)
        << "ReconcileModifierCache cleared a byte whose press its own batch had just injected. "
        << "That modifier is now dropped, not latched. See this test's comment";
  }

  // Release unconditionally before reporting, so a FAIL never leaves Left Shift asserted
  // machine-wide -- the same obligation every capability check in this file carries.
  ReleaseAndSettle(VK_LSHIFT);

  if (starvedDepth) {
    FAIL() << "a batch depth could not manage " << kMinTrials << " trials inside its "
           << kDepthBudgetMs << "ms budget, so that depth measured nothing. Read the us/event "
           << "figure printed above: if it is in the hundreds of microseconds this machine is "
           << "merely loaded, and if it is over 100000 the low level hook chain is costing about "
           << "0.1s per injected event, which is what made the fixed-count form of this test "
           << "unrunnable. Either way this is a fact about the machine, not a platform property "
           << "having changed. Re-run in isolation before believing anything else here. The "
           << "measured figures and the decision to keep this test are recorded in "
           << "specs/003-8064-audit-closeout/research.md R-13.";
    return;
  }

  EXPECT_GE(GetAsyncKeyState(VK_LSHIFT), 0) << "the probe left Left Shift asserted machine-wide";
}

namespace {
struct HookObservation {
  DWORD vkCode;
  DWORD scanCode;
  ULONG_PTR dwExtraInfo;
  DWORD flags;
};

HookObservation g_observed[64];
int g_observedCount = 0;
HHOOK g_probeHook = NULL;

LRESULT CALLBACK
ProvenanceProbeHook(int nCode, WPARAM wParam, LPARAM lParam) {
  if (nCode == HC_ACTION && g_observedCount < _countof(g_observed)) {
    const KBDLLHOOKSTRUCT *hs = (const KBDLLHOOKSTRUCT *)lParam;
    g_observed[g_observedCount].vkCode      = hs->vkCode;
    g_observed[g_observedCount].scanCode    = hs->scanCode;
    g_observed[g_observedCount].dwExtraInfo = hs->dwExtraInfo;
    g_observed[g_observedCount].flags       = hs->flags;
    g_observedCount++;
  }
  return CallNextHookEx(g_probeHook, nCode, wParam, lParam);
}

// Injects one modifier event the way do_keybd_event does, with an explicit dwExtraInfo.
void
InjectTaggedModifier(BYTE vk, BYTE scan, DWORD flags, ULONG_PTR extraInfo) {
  INPUT input;

  memset(&input, 0, sizeof(input));
  input.type = INPUT_KEYBOARD;

  // The same collapse do_keybd_event performs, including the Right Shift scan code rewrite.
  switch (vk) {
  case VK_RSHIFT:
    scan = SCANCODE_RSHIFT;
    /*fallthrough*/
  case VK_LSHIFT:
    input.ki.wVk = VK_SHIFT;
    break;
  default:
    input.ki.wVk = vk;
    break;
  }

  input.ki.wScan       = scan;
  input.ki.dwFlags     = flags;
  input.ki.dwExtraInfo = extraInfo;

  SendInput(1, &input, sizeof(INPUT));
}

// WH_KEYBOARD_LL delivers through the installing thread's message queue.
void
PumpFor(DWORD ms) {
  const DWORD until = GetTickCount() + ms;
  MSG msg;
  while (GetTickCount() < until) {
    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
    Sleep(1);
  }
}

// The first observation for vkCode with the given up/down direction, or NULL.
const HookObservation *
FindObservation(DWORD vkCode, bool isUp) {
  for (int i = 0; i < g_observedCount; i++) {
    if (g_observed[i].vkCode == vkCode && (((g_observed[i].flags & LLKHF_UP) != 0) == isUp)) {
      return &g_observed[i];
    }
  }
  return NULL;
}
} // namespace

/*
  Measures whether dwExtraInfo survives SendInput to a low level keyboard hook, and compares it
  against the scan code as a provenance signal -- i.e. whether the hook's modifier post can be gated
  on provenance at all, as keyman64.h's scan-code TODO wants.

  Right Shift is the case that matters: do_keybd_event overwrites SCAN_FLAG_KEYMAN_KEY_EVENT with
  SCANCODE_RSHIFT for it, so the scan code cannot identify an injected Right Shift, while
  dwExtraInfo passes straight through untouched.

  See PROBE CAPABILITY; this test's requirement is the strictest, since a hook callback needs the
  desktop to route keyboard messages to the installing thread's queue. If dwExtraInfo ever stopped
  surviving, IsKeymanInjectedKeyEvent's second arm would always read 0 and Keyman's own Right Shift
  wrap events would feed the cache again -- and TheGateCoversRightShiftThroughDwExtraInfo would not
  notice, since it never round-trips the OS.
*/
TEST_F(KEYBD_SHIFT, DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0 || GetAsyncKeyState(VK_RSHIFT) < 0) {
    FAIL() << "a Shift key already reads down, so an injected Shift cannot be distinguished from "
           << "it. Let go of both Shift keys and run this target again.";
    return;
  }

  g_observedCount = 0;
  g_probeHook     = SetWindowsHookEx(WH_KEYBOARD_LL, ProvenanceProbeHook, GetModuleHandle(NULL), 0);
  ASSERT_NE(g_probeHook, (HHOOK)NULL) << "could not install the probe hook";

  // Left Shift and Right Shift, down then up, all tagged as Keyman's own.
  InjectTaggedModifier(VK_LSHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT);
  InjectTaggedModifier(VK_LSHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT);
  InjectTaggedModifier(VK_RSHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, 0, EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT);
  InjectTaggedModifier(VK_RSHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, KEYEVENTF_KEYUP, EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT);

  PumpFor(400);

  UnhookWindowsHookEx(g_probeHook);
  g_probeHook = NULL;

  for (int i = 0; i < g_observedCount; i++) {
    printf("PROVENANCE  vk=0x%02X scan=0x%02X extra=0x%08X flags=0x%02X (injected=%d extended=%d up=%d)\n",
           (unsigned)g_observed[i].vkCode, (unsigned)g_observed[i].scanCode,
           (unsigned)g_observed[i].dwExtraInfo, (unsigned)g_observed[i].flags,
           (g_observed[i].flags & LLKHF_INJECTED) ? 1 : 0,
           (g_observed[i].flags & LLKHF_EXTENDED) ? 1 : 0,
           (g_observed[i].flags & LLKHF_UP) ? 1 : 0);
  }

  if (g_observedCount == 0) {
    // Not "the injection did not land": this desktop is not routing keyboard messages to a hooked,
    // pumped queue. Both Shifts were sent down and up above regardless, so nothing is left
    // asserted machine-wide.
    FAIL() << "the probe hook observed nothing, so neither dwExtraInfo's survival nor the Right "
           << "Shift scan rewrite was measured. This target is only invoked where the desktop is "
           << "supposed to route keyboard messages to a pumped, hooked message queue; see PROBE "
           << "CAPABILITY for why this is checked with the hook itself rather than a lighter proxy.";
    return;
  }

  // Every observed event must carry the tag. This is the property the gate would rely on.
  for (int i = 0; i < g_observedCount; i++) {
    EXPECT_EQ(g_observed[i].dwExtraInfo, (ULONG_PTR)EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT)
        << "dwExtraInfo did not survive SendInput for vk "
        << Debug_VirtualKey((WORD)g_observed[i].vkCode)
        << ", so it cannot be used as a provenance signal";
  }

  // And the scan code must NOT identify the injected Right Shift, which is the hole being closed.
  // Both Shifts arrive as VK_SHIFT collapsed by do_keybd_event's mapping.
  const HookObservation *shiftDown = FindObservation(VK_SHIFT, false);
  if (shiftDown == NULL) {
    shiftDown = FindObservation(VK_LSHIFT, false);
  }
  ASSERT_NE(shiftDown, (const HookObservation *)NULL) << "no Shift KEYDOWN was observed";

  bool sawRightShiftScanCode = false;
  for (int i = 0; i < g_observedCount; i++) {
    if (g_observed[i].scanCode == SCANCODE_RSHIFT) {
      sawRightShiftScanCode = true;
      EXPECT_NE(g_observed[i].scanCode, (DWORD)SCAN_FLAG_KEYMAN_KEY_EVENT)
          << "an injected Right Shift carried the Keyman scan flag after all, which would mean "
          << "do_keybd_event no longer rewrites it";
    }
  }
  EXPECT_TRUE(sawRightShiftScanCode)
      << "no event carried SCANCODE_RSHIFT, so the Right Shift leg was not measured";

  EXPECT_GE(GetAsyncKeyState(VK_LSHIFT), 0) << "the probe left Left Shift asserted machine-wide";
  EXPECT_GE(GetAsyncKeyState(VK_RSHIFT), 0) << "the probe left Right Shift asserted machine-wide";
}

/*
  #8064 Live measurement: does GetAsyncKeyState(VK_LSHIFT) reflect a third party's
  SendInput(wVk=VK_SHIFT, wScan=0), as GenericVkEventReconcilesAgainstTheChiralLiveReading assumes?
  Reuses the probe-hook plumbing above so the observed vkCode is on record too -- "Windows
  re-chiralises before the hook" and "GetAsyncKeyState reflects the chiral VK" are separate claims
  argued from the same mechanism. See PROBE CAPABILITY. A pass confirms the mechanism on the machine
  it ran on, not that no future Windows or other injector can violate it.
*/
TEST_F(KEYBD_SHIFT, GenericShiftSendInputReflectsInBothAsyncKeyStates) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0 || GetAsyncKeyState(VK_SHIFT) < 0) {
    FAIL() << "Shift already reads down, so the injected generic VK_SHIFT cannot be distinguished "
           << "from it. Let go of both Shift keys and run this target again.";
    return;
  }

  g_observedCount = 0;
  g_probeHook     = SetWindowsHookEx(WH_KEYBOARD_LL, ProvenanceProbeHook, GetModuleHandle(NULL), 0);
  ASSERT_NE(g_probeHook, (HHOOK)NULL) << "could not install the probe hook";

  // The audited scenario itself: a generic VK, scan 0, exactly as a third party might inject it --
  // not through do_keybd_event, which never emits a generic VK_SHIFT with scan 0 in the first place.
  INPUT input;
  memset(&input, 0, sizeof(input));
  input.type     = INPUT_KEYBOARD;
  input.ki.wVk   = VK_SHIFT;
  input.ki.wScan = 0;
  SendInput(1, &input, sizeof(INPUT));

  PumpFor(400);

  UnhookWindowsHookEx(g_probeHook);
  g_probeHook = NULL;

  const SHORT genericAsync = GetAsyncKeyState(VK_SHIFT);
  const SHORT leftAsync    = GetAsyncKeyState(VK_LSHIFT);
  const SHORT rightAsync   = GetAsyncKeyState(VK_RSHIFT);

  printf("GENERIC VK PROBE  GetAsyncKeyState VK_SHIFT=0x%04X VK_LSHIFT=0x%04X VK_RSHIFT=0x%04X\n",
         (unsigned short)genericAsync, (unsigned short)leftAsync, (unsigned short)rightAsync);

  for (int i = 0; i < g_observedCount; i++) {
    printf("GENERIC VK PROBE  hook saw vk=0x%02X scan=0x%02X extra=0x%08X flags=0x%02X\n",
           (unsigned)g_observed[i].vkCode, (unsigned)g_observed[i].scanCode,
           (unsigned)g_observed[i].dwExtraInfo, (unsigned)g_observed[i].flags);
  }

  // Release for real before asserting anything, so a failed assertion below does not leave the key
  // asserted machine-wide.
  keybd_event(VK_LSHIFT, 0, KEYEVENTF_KEYUP, 0);
  Sleep(150);

  if (g_observedCount == 0) {
    // The capability check, same reasoning as DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot's:
    // this desktop is not routing keyboard messages to a hooked, pumped message queue. Already
    // released above, so nothing is left asserted machine-wide.
    FAIL() << "the probe hook observed nothing, so the re-chiralisation claim was not measured. "
           << "This target is only invoked where the desktop is supposed to route keyboard messages "
           << "to a pumped, hooked message queue. See PROBE CAPABILITY.";
    return;
  }

  // The claim CaptureLiveModifierState's comment rests on: the hook sees a chiral vkCode, not the
  // generic 0x10, because Windows resolved chirality from MapVirtualKey(scan 0) before delivery.
  EXPECT_NE(g_observed[0].vkCode, (DWORD)VK_SHIFT)
      << "the hook saw the generic VK_SHIFT undivided; Windows did not re-chiralise it, which is "
      << "the premise CaptureLiveModifierState's comment rests on";

  // The claim this test exists to add: that GetAsyncKeyState for the chiral VK the hook observed
  // agrees with the generic reading, so a live-state reader that only checks the six chiral VKs is
  // not blind to this injection.
  EXPECT_LT(genericAsync, (SHORT)0) << "GetAsyncKeyState(VK_SHIFT) did not see the injection at all; "
                                    << "nothing here was measured";
  EXPECT_TRUE(leftAsync < 0 || rightAsync < 0)
      << "GetAsyncKeyState(VK_SHIFT) reports the key held, but neither chiral VK does -- "
      << "CaptureLiveModifierState would miss this, and ReconcileModifierCache could erase a "
      << "correctly-set cache byte for it. See the hardening sketched in CaptureLiveModifierState's "
      << "comment if this ever fires";

  EXPECT_GE(GetAsyncKeyState(VK_LSHIFT), 0) << "the probe left Left Shift asserted machine-wide";
  EXPECT_GE(GetAsyncKeyState(VK_RSHIFT), 0) << "the probe left Right Shift asserted machine-wide";
}
