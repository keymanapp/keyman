#include "pch.h"
#include "kbd.h"                  // SCANCODE_RSHIFT
#include "serialkeyeventcommon.h" // MAX_KEYEVENT_INPUTS, MAX_KEYEVENT_INPUTS_MODIFIERS

/*
  Characterisation tests for the serial key event server's modifier cache. See #8064, and
  windows/src/test/manual-tests/GH-8064 - stuck-modifier-phantom-keydown/README.md for the full
  mechanism and the end-to-end test.

  A dropped modifier KEYUP has exactly one residue: one byte of the array keybd_shift reads. So
  these tests construct the residue directly, and need no stall, no thread and no message pump.
*/

// Globals::InitSettings (k32_globals.cpp) seeds the prefix virtual key from _VK_PREFIX_DEFAULT
// (appint/aiTIP.h) or from the registry, and Globals_InitProcess does not call it. Pin it here
// rather than depend on the machine's registry.
static const BYTE PREFIX_VK = 0x0E;

class KEYBD_SHIFT : public ::testing::Test {
public:
  KEYBD_SHIFT() {}

  void
  SetUp() {
    // keybd_shift's SendDebugEntry / SendDebugMessageFormat macros reach ThreadGlobals(), which
    // enters csGlobals; that critical section is only ever initialised by Globals_InitProcess.
    // Same pattern as kmprocessactions.tests.cpp.
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
  INPUT inputs[MAX_KEYEVENT_INPUTS];
  int n;

  // Number of queued events for wVk in one direction.
  int
  Count(WORD wVk, bool isUp) const {
    int count = 0;
    for (int i = 0; i < n; i++) {
      if (inputs[i].ki.wVk == wVk && (((inputs[i].ki.dwFlags & KEYEVENTF_KEYUP) != 0) == isUp)) {
        count++;
      }
    }
    return count;
  }

  // Index of the first queued event for wVk in one direction, or -1.
  int
  IndexOf(WORD wVk, bool isUp) const {
    for (int i = 0; i < n; i++) {
      if (inputs[i].ki.wVk == wVk && (((inputs[i].ki.dwFlags & KEYEVENTF_KEYUP) != 0) == isUp)) {
        return i;
      }
    }
    return -1;
  }

  void
  Rewind() {
    memset(kbd, 0, sizeof(kbd));
    memset(inputs, 0, sizeof(inputs));
    n = 0;
  }
};

/*
  One stale byte is enough: the modifier is pressed for real and no release is queued for it
  anywhere in the batch, so SendInput latches it machine-wide. This is #8064.
*/
TEST_F(KEYBD_SHIFT, ResetRepressesFromCache) {
  kbd[VK_LSHIFT] = 0x80;

  keybd_shift(inputs, &n, TRUE, kbd);

  ASSERT_EQ(n, 3);

  EXPECT_EQ(inputs[0].ki.wVk, (WORD)VK_SHIFT);
  EXPECT_EQ(inputs[0].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)0);
  EXPECT_EQ(inputs[0].ki.wScan, (WORD)SCAN_FLAG_KEYMAN_KEY_EVENT);

  EXPECT_EQ(inputs[1].ki.wVk, (WORD)PREFIX_VK);
  EXPECT_EQ(inputs[1].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)0);
  EXPECT_EQ(inputs[2].ki.wVk, (WORD)PREFIX_VK);
  EXPECT_EQ(inputs[2].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)KEYEVENTF_KEYUP);

  EXPECT_EQ(Count(VK_SHIFT, false), 1);
  EXPECT_EQ(Count(VK_SHIFT, true), 0) << "a modifier KEYDOWN with no matching KEYUP: #8064";
}

/*
  The prefix is what stops a bare Alt release activating the window menu, so its position relative
  to the keyups matters.
*/
TEST_F(KEYBD_SHIFT, ReleaseEmitsPrefixThenKeyups) {
  kbd[VK_LSHIFT] = 0x80;

  keybd_shift(inputs, &n, FALSE, kbd);

  ASSERT_EQ(n, 3);

  EXPECT_EQ(inputs[0].ki.wVk, (WORD)PREFIX_VK);
  EXPECT_EQ(inputs[0].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)0);
  EXPECT_EQ(inputs[1].ki.wVk, (WORD)PREFIX_VK);
  EXPECT_EQ(inputs[1].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)KEYEVENTF_KEYUP);

  EXPECT_EQ(inputs[2].ki.wVk, (WORD)VK_SHIFT);
  EXPECT_EQ(inputs[2].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)KEYEVENTF_KEYUP);

  // The release half is balanced by construction: it only ever emits keyups.
  EXPECT_EQ(Count(VK_SHIFT, false), 0);
}

/*
  The extended bit is the only thing separating the two sides. On hardware with no physical Right
  Ctrl key, a latch on that virtual key cannot be cleared by any keystroke the user can produce.
*/
TEST_F(KEYBD_SHIFT, RightControlCollapsesToExtendedControl) {
  kbd[VK_RCONTROL] = 0x80;
  keybd_shift(inputs, &n, TRUE, kbd);

  int i = IndexOf(VK_CONTROL, false);
  ASSERT_NE(i, -1);
  EXPECT_EQ(inputs[i].ki.dwFlags & KEYEVENTF_EXTENDEDKEY, (DWORD)KEYEVENTF_EXTENDEDKEY);
  EXPECT_EQ(Count(VK_RCONTROL, false), 0) << "VK_RCONTROL must never reach SendInput";

  Rewind();

  kbd[VK_LCONTROL] = 0x80;
  keybd_shift(inputs, &n, TRUE, kbd);

  i = IndexOf(VK_CONTROL, false);
  ASSERT_NE(i, -1);
  EXPECT_EQ(inputs[i].ki.dwFlags & KEYEVENTF_EXTENDEDKEY, (DWORD)0);
  EXPECT_EQ(Count(VK_LCONTROL, false), 0);
}

/*
  Shift's side is carried by the scan code alone, so do_keybd_event has to spend the 0xFF
  synthesized-key marker slot on SCANCODE_RSHIFT instead. See the file comment in keybd_shift.cpp.
*/
TEST_F(KEYBD_SHIFT, RightShiftCollapsesToShiftWithRightScanCode) {
  kbd[VK_RSHIFT] = 0x80;

  keybd_shift(inputs, &n, TRUE, kbd);

  int i = IndexOf(VK_SHIFT, false);
  ASSERT_NE(i, -1);
  EXPECT_EQ(inputs[i].ki.wScan, (WORD)SCANCODE_RSHIFT);
  EXPECT_NE(inputs[i].ki.wScan, (WORD)SCAN_FLAG_KEYMAN_KEY_EVENT)
      << "Right Shift is the one modifier that cannot carry Keyman's synthesized-key marker";
  EXPECT_EQ(inputs[i].ki.dwFlags & KEYEVENTF_EXTENDEDKEY, (DWORD)0)
      << "Shift's side comes from the scan code, never the extended flag";

  Rewind();

  kbd[VK_LSHIFT] = 0x80;
  keybd_shift(inputs, &n, TRUE, kbd);

  i = IndexOf(VK_SHIFT, false);
  ASSERT_NE(i, -1);
  EXPECT_EQ(inputs[i].ki.wScan, (WORD)SCAN_FLAG_KEYMAN_KEY_EVENT);
}

/*
  serialkeyeventcommon.h reserves MAX_KEYEVENT_INPUTS_MODIFIERS events at the end of the buffer,
  noting "This value depends on keybd_shift behaviour". Nothing enforced that dependency; this does.
*/
TEST_F(KEYBD_SHIFT, ModifierEventCountNeverExceedsReserve) {
  const BYTE allSix[6] = {VK_LMENU, VK_RMENU, VK_LCONTROL, VK_RCONTROL, VK_LSHIFT, VK_RSHIFT};
  for (int i = 0; i < (int)_countof(allSix); i++) {
    kbd[allSix[i]] = 0x80;
  }

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 8) << "6 modifier keydowns + prefix down + prefix up";
  EXPECT_LE(n, MAX_KEYEVENT_INPUTS_MODIFIERS);

  n = 0;
  keybd_shift(inputs, &n, FALSE, kbd);
  EXPECT_EQ(n, 8) << "prefix down + prefix up + 6 modifier keyups";
  EXPECT_LE(n, MAX_KEYEVENT_INPUTS_MODIFIERS);
}

/*
  DISABLED_ deliberately, and must stay so: re-pressing a genuinely held modifier is what
  keybd_shift is for, and from inside it a stale byte and a real one are indistinguishable.
  RECONCILE_MODIFIER_CACHE.ReconcileThenResetPressesNothing is this test with the fix applied.
*/
TEST_F(KEYBD_SHIFT, DISABLED_ResetDoesNotPressAKeyThatIsNotHeld) {
  // gtest 1.8.1 has no GTEST_SKIP(); bail out visibly rather than pass for the wrong reason.
  if (GetAsyncKeyState(VK_LSHIFT) < 0) {
    GTEST_LOG_(WARNING) << "Left Shift reads down; precondition unmet, not evaluated";
    SUCCEED();
    return;
  }

  kbd[VK_LSHIFT] = 0x80;

  keybd_shift(inputs, &n, TRUE, kbd);

  EXPECT_EQ(Count(VK_SHIFT, false), 0)
      << "keybd_shift_reset queued a KEYDOWN for VK_SHIFT while GetAsyncKeyState reports it up. "
      << "That unmatched KEYDOWN is #8064: SendInput latches Shift machine-wide until the exact "
      << "matching KEYUP arrives, which for Right Ctrl on hardware without the key is never.";
}

/*
  isModifierKey gates the cache feed, so it defines the set of keys that can be latched at all:
  nine accepted virtual keys collapsing to six cache slots. x86 only -- its enclosing region in
  k32_lowlevelkeyboardhook.cpp is #ifndef _WIN64.
*/
#ifndef _WIN64
extern BOOL isModifierKey(DWORD vkCode);

TEST(K32LowLevelKeyboardHook, IsModifierKeyAcceptsExactlyNineVks) {
  const DWORD accepted[9] = {
      VK_LCONTROL, VK_RCONTROL, VK_CONTROL, VK_LMENU, VK_RMENU, VK_MENU, VK_LSHIFT, VK_RSHIFT, VK_SHIFT,
  };

  int acceptedCount = 0;
  for (DWORD vk = 0; vk < 256; vk++) {
    if (isModifierKey(vk)) {
      acceptedCount++;
    }
  }
  EXPECT_EQ(acceptedCount, (int)_countof(accepted)) << "the accepted virtual key set changed; the cache's six slots depend on it";

  // No SCOPED_TRACE here: it grows gtest 1.8.1's internal trace stack, whose capacity is retained
  // after the scope exits, and gtest_main.cpp's _CrtMemDifference check reports that as a leak.
  for (int i = 0; i < (int)_countof(accepted); i++) {
    EXPECT_TRUE(isModifierKey(accepted[i])) << "vkCode " << accepted[i] << " is no longer a modifier";
  }

  EXPECT_FALSE(isModifierKey(VK_CAPITAL));
  EXPECT_FALSE(isModifierKey(VK_NUMLOCK));
  EXPECT_FALSE(isModifierKey(VK_SCROLL));
  EXPECT_FALSE(isModifierKey(VK_INSERT));
  EXPECT_FALSE(isModifierKey(VK_LWIN));
  EXPECT_FALSE(isModifierKey(VK_RWIN));
  EXPECT_FALSE(isModifierKey(VK_APPS));
}
#endif // !_WIN64

namespace {
// Stub live-modifier-state reader. A file-local array rather than a mock, because gmock is not
// linked into keyman32.tests.vcxproj -- which is why ReconcileModifierCache takes its reader.
BYTE g_stubAsyncState[256];

SHORT WINAPI
StubGetAsyncKeyState(int vKey) {
  // 0x8000 is negative as a SHORT, which is what the "< 0 means down" convention tests.
  return (vKey >= 0 && vKey < 256 && g_stubAsyncState[vKey]) ? (SHORT)0x8000 : (SHORT)0;
}
} // namespace

class RECONCILE_MODIFIER_CACHE : public KEYBD_SHIFT {
public:
  void
  SetUp() {
    KEYBD_SHIFT::SetUp();
    memset(g_stubAsyncState, 0, sizeof(g_stubAsyncState));
  }
};

/*
  Once the stale byte is cleared, reset emits nothing at all: no modifier KEYDOWN and, because
  needsPrefix stays FALSE, no prefix keystroke either.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ClearsCachedModifierTheOsReportsUp) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held
  g_stubAsyncState[VK_LSHIFT] = 0;    // OS: up

  EXPECT_TRUE(ReconcileModifierCache(kbd, StubGetAsyncKeyState));
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0);

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0) << "nothing to restore, so not even a prefix keystroke";
}

/*
  Restoring a genuinely held modifier is what keeps Alt+F from opening the window menu.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, KeepsCachedModifierTheOsReportsDown) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held
  g_stubAsyncState[VK_LSHIFT] = 0x80; // OS: agrees

  EXPECT_FALSE(ReconcileModifierCache(kbd, StubGetAsyncKeyState));
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80);

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(Count(VK_SHIFT, false), 1) << "a real hold must still be restored";
}

/*
  The asymmetry is deliberate: between this read and SendInput the user may release the key, and
  asserting it would create the very latch this is removing.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, NeverSetsAModifierTheCacheDoesNotHold) {
  kbd[VK_RCONTROL]              = 0;    // cache: up
  g_stubAsyncState[VK_RCONTROL] = 0x80; // OS: down

  EXPECT_FALSE(ReconcileModifierCache(kbd, StubGetAsyncKeyState));
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0);

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0);
}

TEST_F(RECONCILE_MODIFIER_CACHE, ClearsAllSixSlots) {
  const BYTE allSix[6] = {VK_LMENU, VK_RMENU, VK_LCONTROL, VK_RCONTROL, VK_LSHIFT, VK_RSHIFT};
  for (int i = 0; i < (int)_countof(allSix); i++) {
    kbd[allSix[i]] = 0x80;
  }

  EXPECT_TRUE(ReconcileModifierCache(kbd, StubGetAsyncKeyState));

  for (int i = 0; i < (int)_countof(allSix); i++) {
    EXPECT_EQ(kbd[allSix[i]], (BYTE)0) << "slot for vkCode " << (int)allSix[i] << " was not cleared";
  }

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0);
}

/*
  Nothing outside the six slots may be touched, whatever the OS reports. A stuck letter or toggle is
  a different defect.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, LeavesNonModifierBytesAlone) {
  kbd['A']         = 0x80;
  kbd[VK_CAPITAL]  = 0x01;
  kbd[VK_NUMLOCK]  = 0x01;
  kbd[VK_INSERT]   = 0x80;

  EXPECT_FALSE(ReconcileModifierCache(kbd, StubGetAsyncKeyState));

  EXPECT_EQ(kbd['A'], (BYTE)0x80);
  EXPECT_EQ(kbd[VK_CAPITAL], (BYTE)0x01);
  EXPECT_EQ(kbd[VK_NUMLOCK], (BYTE)0x01);
  EXPECT_EQ(kbd[VK_INSERT], (BYTE)0x80);
}

/*
  DISABLED_ResetDoesNotPressAKeyThatIsNotHeld, with the reconcile call inserted.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ReconcileThenResetPressesNothing) {
  kbd[VK_LSHIFT] = 0x80;

  ReconcileModifierCache(kbd, StubGetAsyncKeyState);
  keybd_shift(inputs, &n, TRUE, kbd);

  EXPECT_EQ(Count(VK_SHIFT, false), 0);
}

/*
  Batch-level assertions against PrepareInjectedInputBatch. The cases above pin
  ReconcileModifierCache; these pin the production call to it, which no test could reach before.

  No SCOPED_TRACE below: gtest 1.8.1 retains its trace-stack capacity after the scope exits and
  gtest_main.cpp's _CrtMemDifference check reports that as a leak.
*/
class PREPARE_INJECTED_INPUT_BATCH : public RECONCILE_MODIFIER_CACHE {
public:
  void
  SetUp() {
    RECONCILE_MODIFIER_CACHE::SetUp();
    memset(&sharedData, 0, sizeof(sharedData));
  }

protected:
  SerialKeyEventSharedData sharedData;

  // Append one output key event to the shared buffer, as the client side would.
  void
  AddOutputKey(WORD wVk) {
    sharedData.inputs[sharedData.nInputs].wVk     = wVk;
    sharedData.inputs[sharedData.nInputs].wScan   = SCAN_FLAG_KEYMAN_KEY_EVENT;
    sharedData.inputs[sharedData.nInputs].dwFlags = 0;
    sharedData.nInputs++;
  }

  void
  RunBatch() {
    n = PrepareInjectedInputBatch(inputs, kbd, &sharedData, StubGetAsyncKeyState);
  }
};

/*
  One stale cache byte, the OS reporting that modifier up, and no KEYDOWN for it anywhere in the
  batch. An unmatched modifier KEYDOWN latches machine-wide once SendInput runs.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, StaleCachedModifierYieldsNoKeydownInTheBatch) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held, the dropped-KEYUP residue
  g_stubAsyncState[VK_LSHIFT] = 0;    // OS: up
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(VK_SHIFT, false), 0)
      << "the batch pressed a modifier the OS reports up: that is the unmatched KEYDOWN of #8064";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the batch must reconcile the cache in place";
}

/*
  Right Ctrl is the worst field case: emitted as VK_CONTROL | KEYEVENTF_EXTENDEDKEY, so on hardware
  without that key no keystroke the user can produce will clear it.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, StaleRightControlYieldsNoExtendedKeydownInTheBatch) {
  kbd[VK_RCONTROL]              = 0x80;
  g_stubAsyncState[VK_RCONTROL] = 0;
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(VK_CONTROL, false), 0)
      << "a latched Right Ctrl is unclearable on hardware without the key";
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0);
}

/*
  Every managed slot, one at a time, so a reconcile that covers only some of the table fails here.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, NoStaleSlotProducesAKeydownInTheBatch) {
  const BYTE expected[6] = {VK_LMENU, VK_RMENU, VK_LCONTROL, VK_RCONTROL, VK_LSHIFT, VK_RSHIFT};

  for (int i = 0; i < (int)_countof(expected); i++) {
    Rewind();
    memset(&sharedData, 0, sizeof(sharedData));
    memset(g_stubAsyncState, 0, sizeof(g_stubAsyncState));

    kbd[expected[i]] = 0x80; // cache: held
    AddOutputKey('A');       // OS: up, g_stubAsyncState is clear

    RunBatch();

    // do_keybd_event collapses the chiral VKs, so count what actually reaches SendInput.
    EXPECT_EQ(Count(VK_SHIFT, false) + Count(VK_CONTROL, false) + Count(VK_MENU, false), 0)
        << "stale slot for vkCode " << (int)expected[i] << " produced a modifier KEYDOWN";
    EXPECT_EQ(kbd[expected[i]], (BYTE)0) << "slot for vkCode " << (int)expected[i] << " was not reconciled";
  }
}

/*
  The restore half must be last, so a truncated SendInput drops presses rather than releases.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, EventOrderIsReleaseThenOutputThenRestore) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held
  g_stubAsyncState[VK_LSHIFT] = 0x80; // OS: agrees, so both halves act
  AddOutputKey('A');

  RunBatch();

  const int release = IndexOf(VK_SHIFT, true);
  const int output  = IndexOf('A', false);
  const int restore = IndexOf(VK_SHIFT, false);

  ASSERT_NE(release, -1) << "a genuinely held modifier must be released before the output keys";
  ASSERT_NE(output, -1);
  ASSERT_NE(restore, -1) << "a genuinely held modifier must be restored after the output keys";

  EXPECT_LT(release, output) << "release half must precede the output keys";
  EXPECT_LT(output, restore) << "restore half must be last, so truncation drops presses not releases";
}

/*
  The worst case fills the buffer exactly, so an off-by-one in the output-key loop bound is a heap
  overrun rather than a failing test. An over-long shared buffer must be clamped by the callee.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, NeverWritesPastTheBufferWhenTheSharedBufferOverflows) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    kbd[KeymanModifierVks[i]]              = 0x80;
    g_stubAsyncState[KeymanModifierVks[i]] = 0x80; // all six genuinely held: both halves emit
  }

  // Deliberately larger than the buffer. Clamping is the callee's job, not the caller's.
  for (int i = 0; i < MAX_KEYEVENT_INPUTS; i++) {
    AddOutputKey('A');
  }
  sharedData.nInputs = MAX_KEYEVENT_INPUTS + 50;

  RunBatch();

  EXPECT_LE(n, MAX_KEYEVENT_INPUTS) << "the batch wrote past the end of a 256-entry buffer";
  EXPECT_EQ(n, MAX_KEYEVENT_INPUTS) << "worst case should fill the buffer exactly, 256 of 256";
}

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
  Measures what InitThread's GetKeyboardState seed leaves in the modifier cache. It reads the
  calling thread's processed queue, so on a thread that has never pumped input it looks like it
  should return nothing. It returns live state:

    this thread : GetKeyboardState ok=1 byte=0x00, GetAsyncKeyState=0x8001
    fresh thread: GetKeyboardState ok=1 byte=0x81, GetAsyncKeyState=0x8000

  DISABLED_ deliberately: it asserts a real modifier machine-wide briefly, so it is a demonstration
  artifact, not a CI gate. Run by hand with --gtest_also_run_disabled_tests, nothing else focused.
*/
TEST_F(KEYBD_SHIFT, DISABLED_FreshThreadKeyboardStateReflectsLiveModifiers) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0) {
    GTEST_LOG_(WARNING) << "Left Shift already reads down; precondition unmet, not evaluated";
    SUCCEED();
    return;
  }

  // Assert Left Shift for real, so there is something for the seed to find.
  keybd_event(VK_LSHIFT, 0, 0, 0);
  Sleep(150);

  const SHORT callerAsync = GetAsyncKeyState(VK_LSHIFT);
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
