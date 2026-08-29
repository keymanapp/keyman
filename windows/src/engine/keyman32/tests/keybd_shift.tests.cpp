#include "pch.h"
#include "kbd.h"                  // SCANCODE_RSHIFT
#include "serialkeyeventcommon.h" // MAX_KEYEVENT_INPUTS, MAX_KEYEVENT_INPUTS_MODIFIERS

/*
  Characterisation tests for the serial key event server's modifier cache (#8064). A dropped
  modifier KEYUP has exactly one residue -- one byte of the array keybd_shift reads -- so these
  construct that residue directly: no stall, no thread, no message pump. Full mechanism and the
  end-to-end test: manual-tests/GH-8064 - stuck-modifier-phantom-keydown/README.md.
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
// The simulated live modifier state. Functions that take a snapshot are handed this array directly;
// PrepareInjectedInputBatch takes a reader, so StubGetAsyncKeyState reads the same array. A file
// local rather than a mock, because gmock is not linked into keyman32.tests.vcxproj.
BYTE g_liveModifierState[256];

// Counts reader calls, so a batch's total can be pinned.
int g_readerCalls = 0;

SHORT WINAPI
StubGetAsyncKeyState(int vKey) {
  g_readerCalls++;
  // 0x8000 is negative as a SHORT, which is what the "< 0 means down" convention tests.
  return (vKey >= 0 && vKey < 256 && g_liveModifierState[vKey]) ? (SHORT)0x8000 : (SHORT)0;
}
} // namespace

// Every case below reads or writes the simulated live state, so reset it and the reader counter.
class LIVE_MODIFIER_STATE : public KEYBD_SHIFT {
public:
  void
  SetUp() {
    KEYBD_SHIFT::SetUp();
    memset(g_liveModifierState, 0, sizeof(g_liveModifierState));
    g_readerCalls = 0;
  }
};

class CAPTURE_LIVE_MODIFIER_STATE : public LIVE_MODIFIER_STATE {
protected:
  BYTE live[256];

  // True if vk is one of the modifiers Keyman manages.
  static bool
  IsManaged(int vk) {
    for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
      if (KeymanModifierVks[i] == vk) {
        return true;
      }
    }
    return false;
  }
};

/*
  The snapshot reports what the reader reported, for the managed set and nothing else.
*/
TEST_F(CAPTURE_LIVE_MODIFIER_STATE, SetsAByteForEachModifierTheOsHolds) {
  g_liveModifierState[VK_LSHIFT]   = 0x80;
  g_liveModifierState[VK_RCONTROL] = 0x80;

  CaptureLiveModifierState(live, StubGetAsyncKeyState);

  EXPECT_EQ(live[VK_LSHIFT], (BYTE)0x80);
  EXPECT_EQ(live[VK_RCONTROL], (BYTE)0x80);
  EXPECT_EQ(live[VK_RSHIFT], (BYTE)0) << "a modifier the OS does not hold must read clear";
}

/*
  Pre-fill with 0xFF: no caller stack residue may survive as a held modifier.
*/
TEST_F(CAPTURE_LIVE_MODIFIER_STATE, ZeroesTheWholeArrayFirst) {
  memset(live, 0xFF, sizeof(live));

  CaptureLiveModifierState(live, StubGetAsyncKeyState);

  for (int i = 0; i < 256; i++) {
    ASSERT_EQ(live[i], (BYTE)0) << "caller residue survived at vk " << i;
  }
}

/*
  Even with every key reading down, only the managed slots are written.
*/
TEST_F(CAPTURE_LIVE_MODIFIER_STATE, SetsNoByteOutsideTheManagedSet) {
  memset(g_liveModifierState, 0x80, sizeof(g_liveModifierState));

  CaptureLiveModifierState(live, StubGetAsyncKeyState);

  for (int i = 0; i < 256; i++) {
    if (!IsManaged(i)) {
      ASSERT_EQ(live[i], (BYTE)0) << "wrote outside the managed set at vk " << i;
    }
  }
}

/*
  One reading per managed modifier, not one per consumer. This is the property that makes the
  snapshot coherent: two readings of the same modifier could disagree if the user pressed or
  released between them.
*/
TEST_F(CAPTURE_LIVE_MODIFIER_STATE, TakesExactlyOneReadingPerManagedModifier) {
  memset(g_liveModifierState, 0x80, sizeof(g_liveModifierState));
  g_readerCalls = 0;

  CaptureLiveModifierState(live, StubGetAsyncKeyState);

  EXPECT_EQ(g_readerCalls, KEYMAN_MODIFIER_VK_COUNT);
}

class RECONCILE_MODIFIER_CACHE : public LIVE_MODIFIER_STATE {};

/*
  Once the stale byte is cleared, reset emits nothing at all: no modifier KEYDOWN and, because
  needsPrefix stays FALSE, no prefix keystroke either.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ClearsCachedModifierTheOsReportsUp) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held
  g_liveModifierState[VK_LSHIFT] = 0;    // OS: up

  EXPECT_TRUE(ReconcileModifierCache(kbd, g_liveModifierState));
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0);

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0) << "nothing to restore, so not even a prefix keystroke";
}

/*
  Restoring a genuinely held modifier is what keeps Alt+F from opening the window menu.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, KeepsCachedModifierTheOsReportsDown) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: agrees

  EXPECT_FALSE(ReconcileModifierCache(kbd, g_liveModifierState));
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
  g_liveModifierState[VK_RCONTROL] = 0x80; // OS: down

  EXPECT_FALSE(ReconcileModifierCache(kbd, g_liveModifierState));
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0);

  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0);
}

TEST_F(RECONCILE_MODIFIER_CACHE, ClearsAllSixSlots) {
  const BYTE allSix[6] = {VK_LMENU, VK_RMENU, VK_LCONTROL, VK_RCONTROL, VK_LSHIFT, VK_RSHIFT};
  for (int i = 0; i < (int)_countof(allSix); i++) {
    kbd[allSix[i]] = 0x80;
  }

  EXPECT_TRUE(ReconcileModifierCache(kbd, g_liveModifierState));

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

  EXPECT_FALSE(ReconcileModifierCache(kbd, g_liveModifierState));

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

  ReconcileModifierCache(kbd, g_liveModifierState);
  keybd_shift(inputs, &n, TRUE, kbd);

  EXPECT_EQ(Count(VK_SHIFT, false), 0);
}

/*
  Batch-level assertions against PrepareInjectedInputBatch, the production caller of the cases above.
  No SCOPED_TRACE: gtest 1.8.1 retains its trace-stack capacity after the scope exits, and
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

  // #8064: cacheIsFed and restorePressedMask both default, so the many existing call sites above
  // stay exactly as they are.
  DWORD restorePressedMask;

  void
  RunBatch(BOOL cacheIsFed = TRUE) {
    restorePressedMask = 0;
    n                  = PrepareInjectedInputBatch(inputs, kbd, &sharedData, StubGetAsyncKeyState, cacheIsFed, &restorePressedMask);
  }
};

/*
  One stale cache byte, the OS reporting that modifier up, and no KEYDOWN for it anywhere in the
  batch. An unmatched modifier KEYDOWN latches machine-wide once SendInput runs.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, StaleCachedModifierYieldsNoKeydownInTheBatch) {
  kbd[VK_LSHIFT]              = 0x80; // cache: held, the dropped-KEYUP residue
  g_liveModifierState[VK_LSHIFT] = 0;    // OS: up
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
  g_liveModifierState[VK_RCONTROL] = 0;
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
    memset(g_liveModifierState, 0, sizeof(g_liveModifierState));

    kbd[expected[i]] = 0x80; // cache: held
    AddOutputKey('A');       // OS: up, g_liveModifierState is clear

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
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: agrees, so both halves act
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
    g_liveModifierState[KeymanModifierVks[i]] = 0x80; // all six genuinely held: both halves emit
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

/*
  The mirror of #8064: the cache byte clear while the OS reports the modifier held. The release half
  reads only the cache and the reconcile only ever clears, so nothing emitted a KEYUP and the output
  keys went out with the modifier live.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, OsHeldModifierIsReleasedBeforeTheOutputKeys) {
  kbd[VK_LSHIFT]              = 0;    // cache: clear -- Keyman never saw the KEYDOWN
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: genuinely held right now
  AddOutputKey('A');

  RunBatch();

  const int release = IndexOf(VK_SHIFT, true);
  const int output  = IndexOf('A', false);

  ASSERT_NE(release, -1) << "no KEYUP was emitted for a modifier the OS reports held: the output "
                         << "keys are injected while Shift is physically down";
  ASSERT_NE(output, -1);
  EXPECT_LT(release, output) << "the release must precede the output keys to have any effect";
}

/*
  The restore half must read the cache alone. Re-pressing on the OS's word is unsafe: the user may
  let go before SendInput runs, which is #8064 inverted. Passes before the fix as well as after.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, OsHeldModifierIsNotRestoredAfterTheOutputKeys) {
  kbd[VK_LSHIFT]              = 0;    // cache: clear
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: held
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(VK_SHIFT, false), 0)
      << "the restore half pressed a modifier the cache never held. The user may release it "
      << "before SendInput runs, and that unmatched KEYDOWN is #8064 from the other direction";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the batch must not write the OS's view into the cache";
}

/*
  Where cache and OS agree, behaviour is exactly as it was.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, AgreementCasesAreUnchanged) {
  // Both held: released before the output keys, restored after, exactly as today.
  kbd[VK_LSHIFT]              = 0x80;
  g_liveModifierState[VK_LSHIFT] = 0x80;
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(VK_SHIFT, true), 1) << "a genuinely held modifier is released once";
  EXPECT_EQ(Count(VK_SHIFT, false), 1) << "and restored once";
  EXPECT_LT(IndexOf(VK_SHIFT, true), IndexOf(VK_SHIFT, false));

  // Both up: no modifier events at all, and no prefix keystroke either.
  Rewind();
  memset(&sharedData, 0, sizeof(sharedData));
  memset(g_liveModifierState, 0, sizeof(g_liveModifierState));
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(VK_SHIFT, true), 0);
  EXPECT_EQ(Count(VK_SHIFT, false), 0);
  EXPECT_EQ(Count(PREFIX_VK, false), 0) << "nothing to release, so not even a prefix keystroke";
  EXPECT_EQ(n, 1) << "the output key and nothing else";
}

/*
  KEYMAN_MODIFIER_VK_COUNT readings per batch and no more, whatever the cache holds. Before the
  snapshot was hoisted, the reconcile and the release set each read for themselves -- six to twelve
  readings, and two readings of the same modifier could disagree.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, BatchTakesOneLiveReadingPerManagedModifier) {
  // Cache empty, which is normal typing.
  AddOutputKey('A');
  g_readerCalls = 0;
  RunBatch();
  EXPECT_EQ(g_readerCalls, KEYMAN_MODIFIER_VK_COUNT) << "empty cache";

  // Cache holds all six and the OS agrees.
  Rewind();
  memset(&sharedData, 0, sizeof(sharedData));
  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    kbd[KeymanModifierVks[i]]                 = 0x80;
    g_liveModifierState[KeymanModifierVks[i]] = 0x80;
  }
  AddOutputKey('A');
  g_readerCalls = 0;
  RunBatch();
  EXPECT_EQ(g_readerCalls, KEYMAN_MODIFIER_VK_COUNT) << "cache and OS agree, all six held";

  // Cache holds all six and the OS holds none: twelve readings before the hoist.
  Rewind();
  memset(&sharedData, 0, sizeof(sharedData));
  memset(g_liveModifierState, 0, sizeof(g_liveModifierState));
  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    kbd[KeymanModifierVks[i]] = 0x80;
  }
  AddOutputKey('A');
  g_readerCalls = 0;
  RunBatch();
  EXPECT_EQ(g_readerCalls, KEYMAN_MODIFIER_VK_COUNT) << "all six stale, the pre-hoist worst case";
}

/*
  Every modifier event the batch wraps its output in must be identifiable as Keyman's own, or the
  gate at the hook cannot filter it and the cache is polluted again. Right Shift is the reason this
  checks dwExtraInfo and not just the scan code.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, EveryWrapEventIsIdentifiableAsKeymanInjected) {
  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    kbd[KeymanModifierVks[i]]                 = 0x80;
    g_liveModifierState[KeymanModifierVks[i]] = 0x80;
  }
  AddOutputKey('A');

  RunBatch();

  int wrapEvents = 0;
  for (int i = 0; i < n; i++) {
    // The output key is the one event that is not a wrap event.
    if (inputs[i].ki.wVk == 'A') {
      continue;
    }
    wrapEvents++;
    EXPECT_TRUE(IsKeymanInjectedKeyEvent(inputs[i].ki.wScan, inputs[i].ki.dwExtraInfo))
        << "wrap event " << i << " (vk 0x" << (int)inputs[i].ki.wVk << ", scan 0x"
        << (int)inputs[i].ki.wScan << ") is indistinguishable from a physical keystroke at the hook";
  }

  EXPECT_GT(wrapEvents, 0) << "no wrap events were emitted, so nothing was checked";
}

/*
  #8064 The verification pass scopes itself to restorePressedMask, so the mask must name exactly the
  VKs the restore half pressed. keybd_shift_reset never writes kbd, so it is just kbd's
  post-reconcile state as a bitmask; these pin that translation.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskIsZeroWhenNothingIsRestored) {
  AddOutputKey('A'); // cache empty, OS empty: nothing released, nothing restored

  RunBatch();

  EXPECT_EQ(restorePressedMask, (DWORD)0);
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskNamesExactlyTheVkTheRestoreHalfPressed) {
  kbd[VK_LSHIFT]                 = 0x80;
  g_liveModifierState[VK_LSHIFT] = 0x80; // agree, so the restore half actually presses it
  AddOutputKey('A');

  RunBatch();

  int index = -1;
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    if (KeymanModifierVks[i] == VK_LSHIFT) {
      index = i;
    }
  }
  ASSERT_NE(index, -1);

  EXPECT_EQ(restorePressedMask, (DWORD)(1u << index));
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskCoversAllSixWhenAllAreRestored) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    kbd[KeymanModifierVks[i]]                 = 0x80;
    g_liveModifierState[KeymanModifierVks[i]] = 0x80;
  }
  AddOutputKey('A');

  RunBatch();

  const DWORD allSix = (1u << KEYMAN_MODIFIER_VK_COUNT) - 1;
  EXPECT_EQ(restorePressedMask, allSix);
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskExcludesAModifierTheReconcileCleared) {
  kbd[VK_LSHIFT]                 = 0x80; // cache: held, the dropped-KEYUP residue
  g_liveModifierState[VK_LSHIFT] = 0;    // OS: up -- reconcile clears it before the restore half runs
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(restorePressedMask, (DWORD)0) << "a reconciled-away modifier must not appear in the mask, "
                                          << "or the verification pass would check a VK the restore half never touched";
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskExcludesAModifierReleasedOnlyOnTheOssWord) {
  kbd[VK_LSHIFT]                 = 0;    // cache: clear -- Keyman never saw the KEYDOWN
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: held, so the release half releases it (G1)
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(restorePressedMask, (DWORD)0) << "the restore half must not press this, so it must not "
                                          << "be in the mask either -- invariant I-6 restated";
}

/*
  #8064 With flag_ShouldSerializeInput off the hook never posts WM_KEYMAN_MODIFIER_EVENT, so kbd
  never changes after its launch seed. The union would then release a live-held modifier no later
  batch could restore -- a lost modifier the pre-#8064 code could not produce, since release and
  restore both read the same stale kbd there. cacheIsFed=FALSE must restore that symmetry.

  To watch it fail, make the cacheIsFed branch in PrepareInjectedInputBatch unconditional:
  Count(VK_SHIFT, true) becomes 1.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, CacheNotFedLeavesALiveHeldModifierUntouched) {
  kbd[VK_LSHIFT]                 = 0;    // cache: never learned it, because the feed is off
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: genuinely held
  AddOutputKey('A');

  RunBatch(FALSE);

  EXPECT_EQ(Count(VK_SHIFT, true), 0)
      << "the cache is not fed, so releasing this modifier can never be undone by a later batch: "
      << "that is a new, permanent lost-modifier regression the union must not introduce here";
  EXPECT_EQ(Count(VK_SHIFT, false), 0);
  EXPECT_EQ(restorePressedMask, (DWORD)0);
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, CacheNotFedStillReleasesAndRestoresWhatTheCacheAloneHolds) {
  kbd[VK_LSHIFT]                 = 0x80; // cache: held (from the launch-time seed, say)
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS agrees
  AddOutputKey('A');

  RunBatch(FALSE);

  EXPECT_EQ(Count(VK_SHIFT, true), 1) << "cache-held modifiers are still released, exactly as before G1";
  EXPECT_EQ(Count(VK_SHIFT, false), 1) << "and still restored, because release and restore agree again";
}

TEST_F(PREPARE_INJECTED_INPUT_BATCH, CacheNotFedStillReconciles) {
  kbd[VK_LSHIFT]                 = 0x80; // cache: stale-held
  g_liveModifierState[VK_LSHIFT] = 0;    // OS: up
  AddOutputKey('A');

  RunBatch(FALSE);

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the reconcile runs regardless of cacheIsFed -- it only ever "
                                     << "clears, which is safe whether or not the cache is kept current";
  EXPECT_EQ(Count(VK_SHIFT, false), 0);
}

/*
  ComputeModifierReleaseState at the function level rather than through the batch.
*/
class COMPUTE_MODIFIER_RELEASE_STATE : public RECONCILE_MODIFIER_CACHE {
protected:
  BYTE releaseState[256];

  void
  Fill(BYTE value) {
    memset(releaseState, value, sizeof(releaseState));
  }
};

/*
  Pre-fill with 0xFF and confirm every byte the function does not set comes back zero.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, ZeroesTheWholeArrayFirst) {
  Fill(0xFF);

  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);

  for (int vk = 0; vk < 256; vk++) {
    EXPECT_EQ(releaseState[vk], (BYTE)0) << "byte " << vk << " kept caller stack residue";
  }
}

/*
  The union, over each of the four cache/OS combinations.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, IsTheUnionOfCacheAndLiveState) {
  // cache only
  Fill(0xFF);
  kbd[VK_LSHIFT] = 0x80;
  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);
  EXPECT_EQ(releaseState[VK_LSHIFT], (BYTE)0x80) << "cache-held must be released, as today";

  // OS only -- the mirror direction
  Fill(0xFF);
  memset(kbd, 0, sizeof(kbd));
  g_liveModifierState[VK_LCONTROL] = 0x80;
  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);
  EXPECT_EQ(releaseState[VK_LCONTROL], (BYTE)0x80) << "OS-held must be released: this is G1";

  // both
  Fill(0xFF);
  kbd[VK_RMENU]              = 0x80;
  g_liveModifierState[VK_RMENU] = 0x80;
  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);
  EXPECT_EQ(releaseState[VK_RMENU], (BYTE)0x80);

  // neither
  Fill(0xFF);
  memset(kbd, 0, sizeof(kbd));
  memset(g_liveModifierState, 0, sizeof(g_liveModifierState));
  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    EXPECT_EQ(releaseState[KeymanModifierVks[i]], (BYTE)0) << "nothing held, nothing released";
  }
}

/*
  Nothing outside the managed set is set, whatever the OS reports. A stuck letter or toggle key is a
  different defect.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, SetsNoByteOutsideTheManagedSet) {
  Fill(0xFF);
  memset(g_liveModifierState, 0x80, sizeof(g_liveModifierState)); // OS: every key held
  memset(kbd, 0x80, sizeof(kbd));                          // cache: every key held

  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);

  for (int vk = 0; vk < 256; vk++) {
    bool managed = false;
    for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
      if (KeymanModifierVks[i] == vk) {
        managed = true;
      }
    }
    if (managed) {
      EXPECT_EQ(releaseState[vk], (BYTE)0x80) << "managed slot " << vk << " should be set";
    } else {
      EXPECT_EQ(releaseState[vk], (BYTE)0) << "byte " << vk << " is outside the managed set";
    }
  }
}

/*
  A reader of the cache, never a writer: writing the OS's view into kbd is what the restore half
  would then press.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, NeverModifiesTheCache) {
  BYTE before[256];
  for (int vk = 0; vk < 256; vk++) {
    kbd[vk] = (BYTE)(vk & 0xFF);
  }
  memcpy(before, kbd, sizeof(before));
  memset(g_liveModifierState, 0x80, sizeof(g_liveModifierState)); // OS: everything held

  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);

  EXPECT_EQ(memcmp(kbd, before, sizeof(before)), 0) << "the cache was modified";
}

/*
  The release set is a superset of the cache, so the release half never emits fewer releases than it
  does today.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, IsASupersetOfTheCache) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    kbd[KeymanModifierVks[i]] = 0x80; // cache: all six held
  }
  memset(g_liveModifierState, 0, sizeof(g_liveModifierState)); // OS: none held

  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);

  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    const BYTE vk = KeymanModifierVks[i];
    EXPECT_TRUE((kbd[vk] & 0x80) == 0 || (releaseState[vk] & 0x80) != 0)
        << "slot " << (int)vk << " is held in the cache but not in the release set";
  }
}

/*
  The reserve must hold for the union too. An all-zero cache with the OS reporting every modifier
  held is the widest the release half gets.
*/
TEST_F(COMPUTE_MODIFIER_RELEASE_STATE, ModifierEventCountNeverExceedsReserveForTheUnion) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    g_liveModifierState[KeymanModifierVks[i]] = 0x80; // OS: all six held
  }
  // kbd stays all zero: the cache holds nothing at all. This is the union's widest divergence.

  ComputeModifierReleaseState(kbd, releaseState, g_liveModifierState);

  keybd_shift(inputs, &n, FALSE, releaseState);
  EXPECT_EQ(n, 8) << "prefix down + prefix up + 6 modifier keyups, from the OS side alone";
  EXPECT_LE(n, MAX_KEYEVENT_INPUTS_MODIFIERS);

  n = 0;
  keybd_shift(inputs, &n, TRUE, kbd);
  EXPECT_EQ(n, 0) << "the restore half must not press what the cache never held";
}

/*
  #8064 residual gaps, Task 1: PrepareModifierVerificationCorrection, the post-batch verification
  pass. No SCOPED_TRACE below, for the same gtest 1.8.1 leak-detector reason as elsewhere in this
  file.
*/
class PREPARE_MODIFIER_VERIFICATION_CORRECTION : public LIVE_MODIFIER_STATE {
protected:
  // Bit for KeymanModifierVks[index]-shaped calls, matching PrepareInjectedInputBatch's mask.
  static DWORD
  MaskBit(BYTE vk) {
    for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
      if (KeymanModifierVks[i] == vk) {
        return 1u << i;
      }
    }
    return 0;
  }

  void
  RunCorrection(DWORD restorePressedMask) {
    n = PrepareModifierVerificationCorrection(inputs, kbd, restorePressedMask, StubGetAsyncKeyState);
  }
};

/*
  The scenario this exists for: a batch's own restore press outlived a user release that raced it.
  The cache correctly says up -- the release was the user's own event -- but the OS still reports it
  held, because Keyman's restore press is what latches it.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, CorrectsAModifierTheOsHoldsThatTheCacheSaysNobodyHolds) {
  kbd[VK_LSHIFT]                 = 0;    // cache: up -- the user's race-winning release reached it
  g_liveModifierState[VK_LSHIFT] = 0x80; // OS: still held -- this batch's own restore press

  RunCorrection(MaskBit(VK_LSHIFT));

  EXPECT_EQ(Count(VK_SHIFT, true), 1) << "the OS is holding a modifier nobody holds; this must correct it";
  EXPECT_EQ(Count(VK_SHIFT, false), 0) << "a correction is a release only, never a press";
}

/*
  Restrict the check to the VKs this batch pressed. A VK outside restorePressedMask is a different
  batch's problem, even if it happens to disagree right now.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, DoesNotTouchAVkOutsideTheRestorePressedMask) {
  kbd[VK_LSHIFT]                 = 0;    // would trigger a correction on its own merits
  g_liveModifierState[VK_LSHIFT] = 0x80;

  RunCorrection(0); // this batch's restore half pressed nothing

  EXPECT_EQ(n, 0) << "VK_LSHIFT was not in the mask, so it must not be corrected";
}

/*
  Agreement -- cache still says held -- means nothing latched on Keyman's own doing; leave it alone.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, DoesNotCorrectWhenTheCacheStillReportsItHeld) {
  kbd[VK_LSHIFT]                 = 0x80; // cache: still held, e.g. the user is genuinely holding it
  g_liveModifierState[VK_LSHIFT] = 0x80;

  RunCorrection(MaskBit(VK_LSHIFT));

  EXPECT_EQ(n, 0) << "cache and OS agree, so there is nothing to correct";
}

/*
  Cache up and OS also up: the restore press was never latched, or already resolved. Nothing to do.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, DoesNotCorrectWhenTheOsAlsoReportsItUp) {
  kbd[VK_LSHIFT]                 = 0;
  g_liveModifierState[VK_LSHIFT] = 0; // OS: also up

  RunCorrection(MaskBit(VK_LSHIFT));

  EXPECT_EQ(n, 0);
}

/*
  Right Ctrl is the field case that matters: emitted as VK_CONTROL | KEYEVENTF_EXTENDEDKEY, so on
  hardware without that key nothing the user can produce clears it if this pass gets it wrong.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, CorrectsRightControlAsExtendedControl) {
  kbd[VK_RCONTROL]                 = 0;
  g_liveModifierState[VK_RCONTROL] = 0x80;

  RunCorrection(MaskBit(VK_RCONTROL));

  int i = IndexOf(VK_CONTROL, true);
  ASSERT_NE(i, -1);
  EXPECT_EQ(inputs[i].ki.dwFlags & KEYEVENTF_EXTENDEDKEY, (DWORD)KEYEVENTF_EXTENDEDKEY);
  EXPECT_EQ(Count(VK_RCONTROL, true), 0) << "VK_RCONTROL must never reach SendInput";
}

/*
  An isolated Alt release opens the window menu, exactly as it would from keybd_shift_release
  directly -- this pass must get the same prefix protection, not a bespoke bare KEYUP.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, UsesThePrefixProtectionForAnIsolatedAltCorrection) {
  kbd[VK_LMENU]                 = 0;
  g_liveModifierState[VK_LMENU] = 0x80;

  RunCorrection(MaskBit(VK_LMENU));

  ASSERT_EQ(n, 3) << "prefix down + prefix up + the one correction keyup";
  EXPECT_EQ(inputs[0].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)0);
  EXPECT_EQ(inputs[1].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)KEYEVENTF_KEYUP);
  EXPECT_EQ(inputs[2].ki.wVk, (WORD)VK_MENU);
  EXPECT_EQ(inputs[2].ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)KEYEVENTF_KEYUP);
}

/*
  The correction must be tagged the same way keybd_shift_release always tags a wrap event, or it
  would feed straight back into the cache and the gate at the hook could not tell it apart from a
  physical keystroke.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, TheCorrectionIsIdentifiableAsKeymanInjected) {
  kbd[VK_LSHIFT]                 = 0;
  g_liveModifierState[VK_LSHIFT] = 0x80;

  RunCorrection(MaskBit(VK_LSHIFT));

  int i = IndexOf(VK_SHIFT, true);
  ASSERT_NE(i, -1);
  EXPECT_TRUE(IsKeymanInjectedKeyEvent(inputs[i].ki.wScan, inputs[i].ki.dwExtraInfo))
      << "an untagged correction would re-enter the cache through the hook's WM_KEYMAN_MODIFIER_EVENT post";
}

/*
  Every managed VK, one at a time, so a check that covers only some of restorePressedMask's bits
  fails here rather than in the field.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, CorrectsEveryManagedVkInTurn) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    memset(kbd, 0, sizeof(kbd));
    memset(g_liveModifierState, 0, sizeof(g_liveModifierState));

    const BYTE vk                 = KeymanModifierVks[i];
    kbd[vk]                       = 0;
    g_liveModifierState[vk]       = 0x80;

    RunCorrection(MaskBit(vk));

    EXPECT_GT(n, 0) << "vkCode " << (int)vk << " was not corrected";
  }
}

/*
  The cache is fed by every modifier event the hook posts, Keyman's own included, because the
  WM_KEYMAN_MODIFIER_EVENT post precedes the "generated by Keyman" pass-through. These pin what that
  costs when a real user event interleaves with a batch's release and restore.

  The helpers feed the collapsed VK_SHIFT form; the hook actually reports the re-chiralised VK
  (0xA0/0xA1, measured by DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot). Either resolves by
  scan code to the same kbd byte, so this keeps the VK_SHIFT branch covered -- read it as coverage,
  not as a claim about what the hook reports.
*/
class MODIFIER_CACHE_EVENT_ORDER : public KEYBD_SHIFT {
protected:
  // The user physically releases Left Shift. Real scan code, chiral VK, untagged.
  void
  UserReleasesLeftShift() {
    UpdateModifierCacheFromKeyEvent(kbd, VK_LSHIFT, FALSE, 0x2A, TRUE);
  }

  // The release half's KEYUP, as do_keybd_event emits it and the hook echoes it back.
  void
  KeymanReleasesLeftShift() {
    UpdateModifierCacheFromKeyEvent(kbd, VK_SHIFT, FALSE, SCAN_FLAG_KEYMAN_KEY_EVENT, TRUE);
  }

  // The restore half's KEYDOWN, likewise.
  void
  KeymanRestoresLeftShift() {
    UpdateModifierCacheFromKeyEvent(kbd, VK_SHIFT, FALSE, SCAN_FLAG_KEYMAN_KEY_EVENT, FALSE);
  }

  // What the hook now does: apply the event only if it is not Keyman's own. Mirrors the condition
  // at k32_lowlevelkeyboardhook.cpp's WM_KEYMAN_MODIFIER_EVENT post.
  void
  ApplyThroughTheGate(BYTE bVk, BYTE bScan, ULONG_PTR extraInfo, BOOL fIsUp) {
    if (!IsKeymanInjectedKeyEvent(bScan, extraInfo)) {
      UpdateModifierCacheFromKeyEvent(kbd, bVk, FALSE, bScan, fIsUp);
    }
  }

  void
  GatedUserReleasesLeftShift() {
    ApplyThroughTheGate(VK_LSHIFT, 0x2A, 0, TRUE);
  }

  void
  GatedKeymanReleasesLeftShift() {
    ApplyThroughTheGate(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, TRUE);
  }

  void
  GatedKeymanRestoresLeftShift() {
    ApplyThroughTheGate(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, FALSE);
  }

  // Right Shift is the case the scan arm cannot carry: do_keybd_event rewrites 0xFF to 0x36.
  void
  GatedKeymanRestoresRightShift() {
    ApplyThroughTheGate(VK_SHIFT, SCANCODE_RSHIFT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, FALSE);
  }
};

/*
  The ordering that matters: the user let go while the batch was being built, so their KEYUP was
  posted first and Keyman's two events followed. The restore press applies last, so the cache ends
  up reporting a modifier the user is no longer holding -- a stale byte of Keyman's own making.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, KeymanOwnRestorePressOutlivesTheUsersRealRelease) {
  kbd[VK_LSHIFT] = 0x80; // the hook posted the user's KEYDOWN before the batch started

  UserReleasesLeftShift();
  KeymanReleasesLeftShift();
  KeymanRestoresLeftShift();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80)
      << "if this is 0, the cache is no longer fed by Keyman's own injected modifiers and the "
      << "defect these cases characterise has been fixed";
}

/*
  The same three events with the user's release last -- they let go after the batch landed -- and
  the cache is correct. Ordering alone is the difference, which is why the defect is intermittent.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, TheUsersReleaseArrivingLastLeavesTheCacheCorrect) {
  kbd[VK_LSHIFT] = 0x80;

  KeymanReleasesLeftShift();
  KeymanRestoresLeftShift();
  UserReleasesLeftShift();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the user is not holding it, so the cache must not either";
}

/*
  A balanced batch with no user event cancels out, which is why this has stayed invisible: the
  release half's KEYUP and the restore half's KEYDOWN net back to the byte they started from.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, ABalancedBatchLeavesTheCacheUnchanged) {
  kbd[VK_LSHIFT] = 0x80;

  KeymanReleasesLeftShift();
  KeymanRestoresLeftShift();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80);
}

/*
  The same ordering through the gate production now uses. Keyman's own two events are filtered, so
  the cache follows the user and the stale byte is never created. This is the fix for #8064.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, TheGateLeavesTheCacheFollowingTheUserAlone) {
  kbd[VK_LSHIFT] = 0x80;

  GatedUserReleasesLeftShift();
  GatedKeymanReleasesLeftShift();
  GatedKeymanRestoresLeftShift();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0)
      << "the user released it and Keyman's own echo must not put it back";
}

/*
  The same, for Right Shift. Its wrap events reach the hook with SCANCODE_RSHIFT rather than the
  0xFF flag, so the scan arm of the gate cannot see them and the dwExtraInfo arm has to. If the tag
  is ever dropped from do_keybd_event's callers, this is the case that goes red.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, TheGateCoversRightShiftThroughDwExtraInfo) {
  kbd[VK_RSHIFT] = 0x80;

  ApplyThroughTheGate(VK_RSHIFT, SCANCODE_RSHIFT, 0, TRUE); // the user's physical release
  GatedKeymanRestoresRightShift();

  EXPECT_EQ(kbd[VK_RSHIFT], (BYTE)0)
      << "an injected Right Shift restore was applied to the cache; the scan code cannot identify "
      << "it, so dwExtraInfo must";
}

/*
  Why the stale byte the first case leaves is not self-healing. The cache says held and the OS says
  held too -- Keyman's own restore press latched it -- so the two agree and the reconcile has
  nothing to clear. The batch then releases and re-presses, reproducing the latch indefinitely.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, TheStaleByteSurvivesTheReconcileBecauseTheOsAgrees) {
  BYTE live[256];
  BYTE releaseState[256];

  // The state the first case leaves: cache held, and the OS holding it by Keyman's own doing.
  kbd[VK_LSHIFT] = 0x80;
  memset(live, 0, sizeof(live));
  live[VK_LSHIFT] = 0x80;

  EXPECT_FALSE(ReconcileModifierCache(kbd, live))
      << "cache and OS agree, so there is no disagreement to detect";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80) << "the stale byte survives";

  ComputeModifierReleaseState(kbd, releaseState, live);
  n = 0;
  keybd_shift(inputs, &n, TRUE, kbd);

  EXPECT_EQ(Count(VK_SHIFT, false), 1)
      << "the restore half presses it again, so the next batch latches it again";
}

/*
  #8064 isModifierKey also accepts generic VK_SHIFT/VK_CONTROL/VK_MENU, but CaptureLiveModifierState
  reads only the six chiral VKs. The concern: a third party's SendInput(wVk=VK_SHIFT, wScan=0) files
  into kbd[VK_LSHIFT] through the hook feed, and if Windows asserted only the generic async state,
  the reconcile would erase the byte that event had just set correctly.

  This pins the reconcile side, given a live reading that reports the chiral VK held.
  GenericShiftSendInputReflectsInBothAsyncKeyStates measures that assumption for real.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, GenericVkEventReconcilesAgainstTheChiralLiveReading) {
  BYTE live[256];

  // The hook feed, as it would look for a third party's generic SendInput(VK_SHIFT, scan=0):
  // UpdateModifierCacheFromKeyEvent's VK_SHIFT case collapses it to VK_LSHIFT, because scan 0 is
  // not SCANCODE_RSHIFT.
  UpdateModifierCacheFromKeyEvent(kbd, VK_SHIFT, FALSE, 0, FALSE);
  ASSERT_EQ(kbd[VK_LSHIFT], (BYTE)0x80) << "the generic event did not reach the chiral slot it should";

  // The live reading, if Windows re-chiralises the async state the way
  // GenericShiftSendInputReflectsInBothAsyncKeyStates measures.
  memset(live, 0, sizeof(live));
  live[VK_LSHIFT] = 0x80;

  EXPECT_FALSE(ReconcileModifierCache(kbd, live))
      << "the chiral live reading agrees with the cache, so the generic-VK press must survive "
      << "reconciliation intact";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80);
}

/*
  IsKeymanInjectedKeyEvent decides which events may feed the modifier cache. The mstsc and OSK rows
  are regression guards: filtering on LLKHF_INJECTED, or on dwExtraInfo != 0, would classify those
  as Keyman's and strip a modifier the user or the OSK genuinely holds.
*/
class IS_KEYMAN_INJECTED_KEY_EVENT : public ::testing::Test {};

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, TheScanFlagAloneIsEnough) {
  EXPECT_TRUE(IsKeymanInjectedKeyEvent(SCAN_FLAG_KEYMAN_KEY_EVENT, 0))
      << "keybd_event callers cannot set dwExtraInfo, so the scan arm has to stand alone";
}

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, TheWrapTagAloneIsEnough) {
  // This row is Right Shift: a real scan code, identified only by the tag.
  EXPECT_TRUE(IsKeymanInjectedKeyEvent(SCANCODE_RSHIFT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP));
}

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, PhysicalKeystrokesAreNotKeymans) {
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(0x2A, 0)) << "physical Left Shift";
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(SCANCODE_RSHIFT, 0)) << "physical Right Shift";
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(0x1D, 0)) << "physical Left Ctrl";
}

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, RemoteDesktopInputIsNotKeymans) {
  // mstsc stamps this on genuine remote user input. Equality on the tag, never extraInfo != 0.
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(0x2A, 0x4321DCBA))
      << "an RDP user's real modifier would be stripped by the next batch";
}

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, TheOnScreenKeyboardIsNotKeymans) {
  // The OSK injects via keybd_event with scan 0 and no extraInfo. Its sticky modifiers are meant
  // to be real machine-wide, so the cache must keep learning them.
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(0, 0))
      << "an OSK sticky modifier would be stripped by the next batch";
}

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, ReInjectedUserKeystrokesAreNotKeymans) {
  // The serializer re-injects the user's own keystrokes with its own tag. Those represent user
  // input, and the server applies them to the cache directly, so the echo is a harmless duplicate.
  EXPECT_FALSE(IsKeymanInjectedKeyEvent(0x2A, EXTRAINFO_FLAG_SERIALIZED_USER_KEY_EVENT));
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
  PROBE CAPABILITY -- shared by the four tests below that inject real modifiers, and referenced by
  name from each. They need an interactive input desktop (the hook ones also need keyboard messages
  routed to a pumped, hooked queue), which a Session-0 CI service account lacks. Rather than
  DISABLED_ forever, where the assumption would rot unchecked even on machines that could check it,
  each probes for the capability with its own real mechanism -- no static OpenInputDesktop-style
  proxy proves the round trip actually works from this process -- and skips when it is absent.
  gtest 1.8.1 has no GTEST_SKIP(), so a skip is SUCCEED() plus a WARNING log, as elsewhere here.

  One is not quite its own mechanism: FreshThreadKeyboardStateReflectsLiveModifiers probes on the
  calling thread and asserts on a spawned one. That holds because GetAsyncKeyState is desktop-global,
  unlike GetKeyboardState/GetKeyState which read the calling thread's processed queue -- so seeing
  the injected press here is sufficient evidence the fresh thread would see it too.

  The cost of this design, which every reader of a CI log needs: a skip reports as PASSED with a log
  line, not as a distinct SKIPPED status. Nothing that would have been asserted is asserted, so it is
  not a false pass -- but the tally alone cannot tell the two apart.

  The probe covers an ABSENT capability, not an intermittently disturbed one. Observed on 2026-08-27:
  these tests went red together on one x64 run, passed in isolation immediately after, and the whole
  suite passed on the next run -- another process disturbing the input queue or the hook round trip
  is enough. From a CI log alone that is indistinguishable from a real regression, so triage a red
  here by re-running the failures in isolation before believing them.
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
    GTEST_LOG_(WARNING) << "Left Shift already reads down; precondition unmet, not evaluated";
    SUCCEED();
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
    GTEST_LOG_(WARNING) << "SKIPPED: keybd_event's injected press was not observable via "
                        << "GetAsyncKeyState in this process. This test needs an interactive input "
                        << "desktop to inject and observe real keyboard state; see the test's own "
                        << "comment for why a static desktop check is not used instead.";
    SUCCEED();
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

void
ReleaseAndSettle(BYTE vk) {
  keybd_event(vk, 0, KEYEVENTF_KEYUP, 0);
  for (int i = 0; i < 200000 && GetAsyncKeyState(vk) < 0; i++) {
    Sleep(0);
  }
}
} // namespace

/*
  Can ReconcileModifierCache race a press its own previous batch injected? If the restore KEYDOWN
  had not yet reached GetAsyncKeyState, the reconcile would clear a byte the user genuinely holds.
  The race itself, not a proxy: production function, production reader, no delay after a SendInput
  that queues the press behind filler -- tighter than any real batch sequence.

  It does not happen. SendInput does not return until the press is visible (0x8001), at every batch
  depth. Measured 0 races and 0 stale reads in 300 attempts at depths 1, 33 and 201, on Windows 11
  Pro 26200, debug x86, with Keyman running, so its WH_KEYBOARD_LL hook was in the chain throughout.
  The SendInput timings printed are an upper bound on a loaded machine, not the cost of the call.

  An oracle, not just a measurement: it goes red if a future Windows returns from SendInput before
  the state is visible. The consequence then is a modifier dropped for one batch, self-correcting on
  the user's next physical press; the fix would be to skip the reconcile for a modifier this
  process's own previous batch pressed, capped at one consecutive skip per VK.

  Multi-second and timing-sensitive, unlike anything else in this file, and it covers something no
  other test here can: every other test drives CaptureLiveModifierState and ReconcileModifierCache
  through the stub reader, which cannot express a timing race at all. See PROBE CAPABILITY.
*/
TEST_F(KEYBD_SHIFT, ReconcileDoesNotRaceItsOwnInjectedRestorePress) {
  if (GetAsyncKeyState(VK_LSHIFT) < 0) {
    GTEST_LOG_(WARNING) << "Left Shift already reads down; precondition unmet, not evaluated";
    SUCCEED();
    return;
  }

  // Capability probe: one trial of the exact mechanism the 300-iteration measurement below depends
  // on, checked immediately, before committing to the rest. Not a static desktop check -- the same
  // reasoning as FreshThreadKeyboardStateReflectsLiveModifiers's probe applies here too.
  {
    const bool injected = InjectRestorePress(VK_LSHIFT, 0);
    const bool landed   = injected && GetAsyncKeyState(VK_LSHIFT) < 0;
    ReleaseAndSettle(VK_LSHIFT); // harmless whether or not anything actually landed

    if (!landed) {
      GTEST_LOG_(WARNING) << "SKIPPED: SendInput's injected press was not observable via "
                          << "GetAsyncKeyState in this process. This test needs an interactive "
                          << "input desktop to inject and observe real keyboard state.";
      SUCCEED();
      return;
    }
  }

  LARGE_INTEGER freq;
  QueryPerformanceFrequency(&freq);

  // An empty batch, a typical one, and a nearly full one.
  const int fillers[]  = { 0, 32, 200 };
  const int kIterations = 100;

  for (int f = 0; f < _countof(fillers); f++) {
    int races = 0, staleReads = 0;
    double sendUs = 0.0;

    for (int i = 0; i < kIterations; i++) {
      LARGE_INTEGER t0, t1;

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

      ReleaseAndSettle(VK_LSHIFT);
    }

    printf("RACE PROBE  filler=%3d  races=%d/%d  staleReads=%d/%d  SendInput mean=%.0fus\n",
           fillers[f], races, kIterations, staleReads, kIterations, sendUs / (double)kIterations);

    EXPECT_EQ(staleReads, 0)
        << "GetAsyncKeyState did not see the injected press by the time SendInput returned. "
        << "SendInput is no longer synchronous with respect to the async key state, so the "
        << "reconcile can now clear a modifier the user is holding. See this test's comment";
    EXPECT_EQ(races, 0)
        << "ReconcileModifierCache cleared a byte whose press its own batch had just injected. "
        << "That modifier is now dropped, not latched. See this test's comment";
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
    GTEST_LOG_(WARNING) << "a Shift key already reads down; precondition unmet, not evaluated";
    SUCCEED();
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
    GTEST_LOG_(WARNING) << "SKIPPED: the probe hook observed nothing. This test needs an "
                        << "interactive input desktop that routes keyboard messages to a pumped, "
                        << "hooked message queue; see the test's own comment for why this is "
                        << "checked with the hook itself rather than a lighter proxy.";
    SUCCEED();
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
    GTEST_LOG_(WARNING) << "Shift already reads down; precondition unmet, not evaluated";
    SUCCEED();
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
    GTEST_LOG_(WARNING) << "SKIPPED: the probe hook observed nothing. This test needs an "
                        << "interactive input desktop that routes keyboard messages to a pumped, "
                        << "hooked message queue.";
    SUCCEED();
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
