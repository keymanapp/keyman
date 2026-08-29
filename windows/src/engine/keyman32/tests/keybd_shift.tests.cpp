#include "pch.h"
#include "kbd.h"                  // SCANCODE_RSHIFT
#include "serialkeyeventcommon.h" // MAX_KEYEVENT_INPUTS, MAX_KEYEVENT_INPUTS_MODIFIERS

/*
  Characterisation tests for the serial key event server's modifier cache (#8064). A dropped
  modifier KEYUP has exactly one residue -- one byte of the array keybd_shift reads -- so these
  construct that residue directly: no stall, no thread, no message pump. Full mechanism and the
  end-to-end test: manual-tests/GH-8064 - stuck-modifier-phantom-keydown/README.md.
*/

/*
  #8064 FR-018 / SC-008 -- THE LEDGER. Every case in this file that passes with its corresponding
  fix reverted, and what was done about it. The verdicts marked MEASURED come from a 34-mutation
  sweep of keybd_shift.cpp and k32_lowlevelkeyboardhook.cpp run against this suite: each mutation
  reverts or widens one production decision, the suite is rebuilt, and the set that goes red is
  recorded. A case that no mutation can turn red is guaranteeing nothing.

  PAIRED -- a positive was added or strengthened, so the case now discriminates:

    CAPTURE_LIVE_MODIFIER_STATE.SetsAByteForEachModifierTheOsHolds
      StubGetAsyncKeyState answers 0x8000 or 0 and nothing else, so this holds whatever
      CaptureLiveModifierState tests: replacing its `< 0` with `!= 0` left all 73 cases green.
      Paired with OnlyTheSignBitCountsAsHeld, which answers 0x0001 and turns that mutation red.

    PREPARE_INJECTED_INPUT_BATCH.RestorePressedMaskIsZeroWhenNothingIsRestored
      Asserted zero against an out-param the harness had already zeroed, so it passed whether or
      not PrepareInjectedInputBatch ever wrote it. RunBatch now seeds kRestoreMaskUnwritten, which
      makes the same assertion demand the write as well. See RunBatch.

    PREPARE_INJECTED_INPUT_BATCH.OsHeldModifierIsNotRestoredAfterTheOutputKeys
      Its own comment conceded that it passes before the fix as well as after -- true of the
      reconcile, but not of the restore half it actually guards: pointing the restore half at live
      instead of kbd turns it red, and so does letting the reconcile set as well as clear. Paired
      by CacheNotFedLeavesALiveHeldModifierUntouched, which the same two mutations also turn red.

  DEFECT CHARACTERISATION -- green with the fix reverted, deliberately. Labelled structurally, on
  the fixture name, not in a comment a later reader has to interpret:

    DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER, all four cases. See that fixture.

  DISABLED BY DESIGN:

    KEYBD_SHIFT.DISABLED_ResetDoesNotPressAKeyThatIsNotHeld -- see its own comment. Its
    with-the-fix positive is RECONCILE_MODIFIER_CACHE.ReconcileThenResetPressesNothing.

  MOVED, AND WHERE TO -- the four interactive probes. They inject real input and need an interactive
  input desktop, so on this target they could only SUCCEED() without asserting anything, which is
  what FR-022 and SC-005 forbid. FR-023 moved them, unchanged except that an absent capability is now
  FAIL() instead of SUCCEED(), to keybd_shift.interactive.tests.cpp -- built by
  keyman32.interactive.tests.vcxproj and run by `build.sh test-interactive:x86` / `test-interactive:x64`,
  deliberately not part of the `test` action. Comments in this file still cite them by name, because
  each one measures a platform property a test here rests on:

    KEYBD_SHIFT.FreshThreadKeyboardStateReflectsLiveModifiers
    KEYBD_SHIFT.ReconcileDoesNotRaceItsOwnInjectedRestorePress
    KEYBD_SHIFT.DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot
    KEYBD_SHIFT.GenericShiftSendInputReflectsInBothAsyncKeyStates

  Not on the ledger, and worth saying why: KEYBD_SHIFT.ResetRepressesFromCache reads like
  characterisation -- its last assertion is labelled #8064 -- but it is a pin. It goes red when
  keybd_shift_reset's prefix protection is dropped, so it holds production behaviour and stays
  where it is.
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

  #8064 FR-018: on the ledger at the top of this file, under DISABLED BY DESIGN. It is not an
  unpaired negative that nobody got round to enabling.
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

// #8064 The same simulated state, reported through GetAsyncKeyState's other bit. The real API sets
// 0x0001 for "pressed since the last call" and leaves the sign bit clear once the key is back up,
// so this is the answer that separates held-now from was-held. Only OnlyTheSignBitCountsAsHeld
// uses it; the FR-018 ledger at the top of this file says why the 0x8000-or-0 stub above cannot do
// that job.
SHORT WINAPI
StubGetAsyncKeyStateToggledOnly(int vKey) {
  g_readerCalls++;
  return (vKey >= 0 && vKey < 256 && g_liveModifierState[vKey]) ? (SHORT)0x0001 : (SHORT)0;
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
  #8064 FR-018's positive for the case above, which cannot be it. SetsAByteForEachModifierTheOsHolds
  drives StubGetAsyncKeyState, whose only two answers are 0x8000 and 0 -- and both sort the same way
  under `< 0` as under `!= 0`. So it asserts the snapshot's contents while holding nothing about the
  test that produces them: measured, replacing CaptureLiveModifierState's `< 0` with `!= 0` leaves
  the whole suite green.

  0x0001 is the reading that separates the two, and it is not a contrivance. GetAsyncKeyState's low
  bit means the key was pressed since the last call, and it is set with the key already back up.
  Read as held, it fabricates a modifier the OS is not holding -- which costs a spurious KEYUP in
  the release half, and, worse, keeps a stale cache byte alive through ReconcileModifierCache, whose
  only clearing condition is the live reading being clear. A stale byte the reconcile can no longer
  reach is #8064 with its own repair disabled.

  Turns red when: the `< 0` in CaptureLiveModifierState becomes `!= 0`, or any other test that is
  not the sign bit.
*/
TEST_F(CAPTURE_LIVE_MODIFIER_STATE, OnlyTheSignBitCountsAsHeld) {
  memset(g_liveModifierState, 0x80, sizeof(g_liveModifierState)); // every VK answers 0x0001 below
  g_readerCalls = 0;

  CaptureLiveModifierState(live, StubGetAsyncKeyStateToggledOnly);

  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    EXPECT_EQ(live[KeymanModifierVks[i]], (BYTE)0)
        << "vkCode " << (int)KeymanModifierVks[i] << ": the reader answered 0x0001 -- pressed since "
        << "the last call, not held now -- and the snapshot recorded it as held";
  }

  EXPECT_EQ(g_readerCalls, KEYMAN_MODIFIER_VK_COUNT) << "the reading count must not move with the answer";
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
  FR-019's replacement contract, and the reason PrepareInjectedInputBatch can pass live straight to
  the release half. The reconcile clears every byte the cache holds while the OS reports it up, so
  afterwards kbd is a subset of live over the managed six -- which is exactly what makes
  kbd union live == live, and the union the batch used to compute redundant rather than merely
  unnecessary. Delete this and the comment at that keybd_shift call is a guess again.

  All four cache/OS combinations are present in the one array, so none can pass by never being
  exercised.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ReconcileLeavesTheCacheASubsetOfLiveState) {
  kbd[VK_LSHIFT]                 = 0x80; // cache holds, OS agrees
  g_liveModifierState[VK_LSHIFT] = 0x80;
  kbd[VK_LCONTROL]               = 0x80; // cache holds, OS says up -- the stale byte of #8064
  kbd[VK_RMENU]                  = 0;    // cache up, OS holds -- the mirror direction
  g_liveModifierState[VK_RMENU]  = 0x80;
  // VK_LMENU, VK_RCONTROL and VK_RSHIFT: neither side holds them.

  EXPECT_TRUE(ReconcileModifierCache(kbd, g_liveModifierState)) << "the stale VK_LCONTROL byte is a disagreement";

  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    const BYTE vk = KeymanModifierVks[i];
    EXPECT_TRUE(!(kbd[vk] & 0x80) || (g_liveModifierState[vk] & 0x80))
        << "slot " << (int)vk << " is held in the cache but not in the live state: kbd is not a subset of live";
    // The corollary the deletion rests on: the union carries nothing live does not already carry.
    EXPECT_EQ((BYTE)((kbd[vk] | g_liveModifierState[vk]) & 0x80), (BYTE)(g_liveModifierState[vk] & 0x80))
        << "slot " << (int)vk << ": the union of cache and live diverges from live";
  }
}

// #8064 FR-018. A value PrepareInjectedInputBatch cannot produce -- the mask is six bits wide --
// so PREPARE_INJECTED_INPUT_BATCH::RunBatch can seed the out-param with it and every mask case
// then fails if the write never happens. File scope, not a static class member: gtest binds its
// arguments by const reference, which would ODR-use a member with no out-of-line definition.
static const DWORD kRestoreMaskUnwritten = 0xFFFFFFFF;

// #8064 FR-015b. Same trick as kRestoreMaskUnwritten, one level down: a value
// PrepareInjectedInputBatch cannot produce for pRestorePressIndex -- it writes a buffer index or
// -1 -- so a case that reads an index is forced to demand the write rather than read back the
// harness's own initialisation.
static const int kRestoreEventIndexUnwritten = -2;

// do_keybd_event collapses each chiral modifier onto its generic VK, so an assertion about a queued
// event has to name the generic one. Mirrors that switch rather than restating its result, so the
// two cannot drift apart silently.
static WORD
CollapsedVk(BYTE vk) {
  switch (vk) {
  case VK_LCONTROL:
  case VK_RCONTROL:
    return VK_CONTROL;
  case VK_LMENU:
  case VK_RMENU:
    return VK_MENU;
  case VK_LSHIFT:
  case VK_RSHIFT:
    return VK_SHIFT;
  default:
    return vk;
  }
}

// #8064 FR-015b, expressed once so the test asserts the caller's rule and not a paraphrase of it:
// given a mask, the per-bit press indices and how many events SendInput actually sent, how many
// bits stand for a press that never left the buffer.
static int
DroppedBitCount(DWORD mask, const int *index, int sent) {
  int dropped = 0;
  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    if ((mask & (1u << i)) && index[i] >= sent) {
      dropped++;
    }
  }
  return dropped;
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

  // #8064: feedIsConfigured and restorePressedMask both default, so the many existing call sites
  // above stay exactly as they are.
  DWORD restorePressedMask;

  // #8064 FR-018. Seeded with this and never with 0. RestorePressedMaskIsZeroWhenNothingIsRestored
  // asserts the mask is zero, and while the harness pre-set it to zero that assertion passed
  // whether or not PrepareInjectedInputBatch ever wrote the out-param -- it was reading back its
  // own initialisation. A value no real mask can hold makes the same assertion demand the write
  // too, which is the positive the case was missing. 0xFFFFFFFF: the mask is six bits wide.
  // #8064 FR-015b. One int per mask bit; see pRestorePressIndex in keybd_shift.cpp.
  int restoreEventIndex[KEYMAN_MODIFIER_VK_COUNT];

  void
  RunBatch(BOOL feedIsConfigured = TRUE) {
    restorePressedMask = kRestoreMaskUnwritten;
    for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
      restoreEventIndex[i] = kRestoreEventIndexUnwritten;
    }
    n = PrepareInjectedInputBatch(
      inputs, kbd, &sharedData, StubGetAsyncKeyState, feedIsConfigured, &restorePressedMask, NULL,
      restoreEventIndex);
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
  FR-021's pin, and the reason the reserve gets a test rather than a comment. It is one number that
  has to be exactly right in both directions. One too small and the restore half writes past the end
  of the server's `new INPUT[MAX_KEYEVENT_INPUTS]` (serialkeyeventserver.cpp:249) -- a heap overrun,
  silent, in the process that owns the keyboard. One too large and output keys that would have fitted
  are dropped instead, silently as well: the truncation at 248 is logged nowhere (see the truncation
  policy comment in serialkeyeventcommon.h). Neither direction shows up as a failure anywhere else,
  so the boundary is pinned here from both sides.

  The expected numbers are derived from the literal worst case, NOT recomputed from the production
  expression `MAX_KEYEVENT_INPUTS - MAX_KEYEVENT_INPUTS_MODIFIERS`. Recomputing it would make the
  assertion move with the constant and pin nothing:

    release half   2 prefix events + 6 modifier KEYUPs                        =    8
    output keys    the loop bound is the running total, not the output count  =  240
    restore half   6 modifier KEYDOWNs + 2 prefix events                      =    8
                                                                                ----
                                                                                 256

  The 240 is worth stating, because "248 output keys + 8 restore events" is the arithmetic one
  reaches for and it is not reachable: the release half spends from the same 248 slots the output
  keys do. Nor can a batch dodge that by having an empty release half and a full restore half --
  ReconcileModifierCache leaves the cache a subset of live state, so the restore half can only be
  non-empty where the release half already was. 8 + 240 + 8 is the true ceiling.

  The batch goes into a buffer deliberately larger than MAX_KEYEVENT_INPUTS, pre-filled with a
  sentinel, so a reserve one too small is caught here as a written-guard-slot failure instead of
  corrupting this process's stack the way it would corrupt the server's heap.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, WorstCaseBatchFillsTheBufferToItsLastSlotAndNotOneFurther) {
  const int kPrefixEventsPerHalf   = 2;   // keybd_sendprefix emits one KEYDOWN and one KEYUP
  const int kModifierEventsPerHalf = 6;   // KeymanModifierVks, every one of them held
  const int kReleaseHalfEvents     = kPrefixEventsPerHalf + kModifierEventsPerHalf;
  const int kRestoreHalfEvents     = kModifierEventsPerHalf + kPrefixEventsPerHalf;
  const int kOutputKeysThatFit     = 240;
  const int kWorstCaseTotal        = kReleaseHalfEvents + kOutputKeysThatFit + kRestoreHalfEvents;

  // Room to record an overrun rather than commit one. 32 only has to exceed the largest overrun a
  // plausible error in the loop bound could produce; dropping the bound entirely yields 16 past.
  const int kGuardSlots      = 32;
  const DWORD kUntouchedSlot = 0xCDCDCDCD;
  INPUT guarded[MAX_KEYEVENT_INPUTS + kGuardSlots];
  memset(guarded, 0xCD, sizeof(guarded));

  // Both halves at full stretch: all six managed modifiers held by the OS, so the release half emits
  // its 8, and held in the cache too, so the reconcile clears none of them and the restore half
  // emits its 8 as well.
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    kbd[KeymanModifierVks[i]]                 = 0x80;
    g_liveModifierState[KeymanModifierVks[i]] = 0x80;
  }

  // More output keys than can fit, so the loop bound is what stops the copy. Exactly the shared
  // buffer's capacity: the over-large nInputs case is
  // NeverWritesPastTheBufferWhenTheSharedBufferOverflows above, and is a different guard.
  for (int i = 0; i < MAX_KEYEVENT_INPUTS; i++) {
    AddOutputKey('A');
  }
  ASSERT_EQ(sharedData.nInputs, (DWORD)MAX_KEYEVENT_INPUTS);

  DWORD restoreMask = 0;
  const int total = PrepareInjectedInputBatch(guarded, kbd, &sharedData, StubGetAsyncKeyState, TRUE, &restoreMask);

  // Fits -- the last slot of the real buffer was written.
  EXPECT_EQ(total, kWorstCaseTotal) << "the worst case no longer fills a " << MAX_KEYEVENT_INPUTS
                                    << "-entry buffer exactly";
  EXPECT_NE(guarded[kWorstCaseTotal - 1].type, kUntouchedSlot)
      << "slot " << (kWorstCaseTotal - 1) << " was never written: the reserve is one too large, and "
      << "an output key that would have fitted was dropped for nothing";

  // Exactly -- one more event would be past the end of the buffer the server actually allocates.
  EXPECT_EQ(kWorstCaseTotal, (int)MAX_KEYEVENT_INPUTS)
      << "the worst case and the allocation no longer agree; one of the two moved without the other";
  for (int i = kWorstCaseTotal; i < kWorstCaseTotal + kGuardSlots; i++) {
    ASSERT_EQ(guarded[i].type, kUntouchedSlot)
        << "slot " << i << " was written: in production that is " << (i - kWorstCaseTotal + 1)
        << " INPUT structure(s) past the end of new INPUT[MAX_KEYEVENT_INPUTS], on the heap";
  }

  // The composition, so a failure above says which of the three parts moved rather than only that
  // the total did.
  int outputKeys = 0, prefixEvents = 0, modifierUps = 0, modifierDowns = 0;
  for (int i = 0; i < total; i++) {
    const WORD vk   = guarded[i].ki.wVk;
    const bool isUp = (guarded[i].ki.dwFlags & KEYEVENTF_KEYUP) != 0;
    if (vk == 'A') {
      outputKeys++;
    } else if (vk == PREFIX_VK) {
      prefixEvents++;
    } else if (isUp) {
      modifierUps++;
    } else {
      modifierDowns++;
    }
  }

  EXPECT_EQ(outputKeys, kOutputKeysThatFit)
      << "the release half spends from the same slots the output keys do, so 240 fit, not 248";
  EXPECT_EQ(prefixEvents, kPrefixEventsPerHalf * 2) << "one prefix down/up pair per half";
  EXPECT_EQ(modifierUps, kModifierEventsPerHalf) << "the release half must emit all six KEYUPs";
  EXPECT_EQ(modifierDowns, kModifierEventsPerHalf) << "the restore half must emit all six KEYDOWNs";
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
  let go before SendInput runs, which is #8064 inverted.

  #8064 FR-018. This used to concede here that it passes before the fix as well as after, which is
  true of the reconcile and false of the thing it actually guards. Measured: it goes red when the
  restore half is pointed at live instead of kbd, and again when ReconcileModifierCache is made to
  set as well as clear -- the two production shapes that would press a modifier the cache never
  held. CacheNotFedLeavesALiveHeldModifierUntouched goes red under both as well, so the pair holds
  the same boundary from the other side.
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

  #8064 FR-018. The first of them makes two claims in one assertion, and it only ever made one of
  them until RunBatch stopped seeding zero: that the mask is empty when the restore half pressed
  nothing, and that the mask was written at all. Against a pre-zeroed out-param the second was free,
  so a PrepareInjectedInputBatch that never touched pRestorePressedMask passed here. The sentinel is
  what buys it. See RunBatch.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, RestorePressedMaskIsZeroWhenNothingIsRestored) {
  AddOutputKey('A'); // cache empty, OS empty: nothing released, nothing restored

  RunBatch();

  ASSERT_NE(restorePressedMask, kRestoreMaskUnwritten) << "the out-param was never written";
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
  restore both read the same stale kbd there. feedIsConfigured=FALSE must restore that symmetry.

  To watch it fail, make the feedIsConfigured branch in PrepareInjectedInputBatch unconditional:
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

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the reconcile runs regardless of feedIsConfigured -- it only ever "
                                     << "clears, which is safe whether or not the cache is kept current";
  EXPECT_EQ(Count(VK_SHIFT, false), 0);
}

/*
  The release half at the function level rather than through the batch. Post-FR-019 the release set
  IS the live snapshot: ComputeModifierReleaseState and its union are gone, on the theorem
  ReconcileLeavesTheCacheASubsetOfLiveState pins. The two properties that were never about the union
  -- the residue guard and the reserve-size guard -- are held here against live instead.

  First, what the union's memset defended, re-expressed on live: CaptureLiveModifierState zeroes all
  256 bytes before it writes, so caller stack residue in the release set cannot reach
  keybd_shift_release as a keyup.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ZeroesTheWholeArrayFirst) {
  BYTE live[256];
  memset(live, 0xFF, sizeof(live)); // caller stack residue, every byte reading held

  CaptureLiveModifierState(live, StubGetAsyncKeyState); // the OS holds nothing

  keybd_shift(inputs, &n, FALSE, live);

  EXPECT_EQ(n, 0) << "caller residue reached the release half -- with nothing held, not even a prefix is due";
}

/*
  The reserve must hold for the release set. An all-zero cache with the OS reporting every managed
  modifier held is the widest the release half gets, and post-FR-019 that release set is live
  itself. The reconcile runs first, exactly as PrepareInjectedInputBatch orders it; it has nothing
  to clear here, which is the point -- the width comes from the OS side alone.
*/
TEST_F(RECONCILE_MODIFIER_CACHE, ModifierEventCountNeverExceedsReserveForTheUnion) {
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    g_liveModifierState[KeymanModifierVks[i]] = 0x80; // OS: all six held
  }
  // kbd stays all zero: the cache holds nothing at all. This is the widest the two can diverge.

  ReconcileModifierCache(kbd, g_liveModifierState);

  keybd_shift(inputs, &n, FALSE, g_liveModifierState);
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
  #8064 FR-017 mutation 4's positive, and the case the one above cannot be. For Left Shift the scan
  code is still SCAN_FLAG_KEYMAN_KEY_EVENT when the correction is emitted, so
  IsKeymanInjectedKeyEvent's scan arm answers TRUE whatever dwExtraInfo carries: drop
  EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP from every event keybd_shift_release emits and that case stays
  green. It asserts the tag; it does not hold it.

  Right Shift is the one VK where the tag is load-bearing. do_keybd_event overwrites the 0xFF scan
  with SCANCODE_RSHIFT (keybd_shift.cpp), by design and unavoidably -- scan code is the only thing
  that tells the receiving app which Shift it was -- so the scan arm is blinded and dwExtraInfo is
  the only channel left. Untagged, this correction re-enters the cache through the hook's gate as
  though the user had pressed Right Shift, and the cache reacquires the very byte the correction
  exists to deny. That is the same shape as TheGateCoversRightShiftThroughDwExtraInfo, on the
  emitting side rather than the receiving one.

  Turns red when: EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP is dropped from the events
  keybd_shift_release emits. The ASSERT_EQ on the scan code is what keeps that honest -- it pins
  that this really is the event the scan arm cannot rescue, so the EXPECT below is testing the tag
  and nothing else.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, TheRightShiftCorrectionIsIdentifiableThroughDwExtraInfoAlone) {
  kbd[VK_RSHIFT]                 = 0;    // cache: nobody holds it
  g_liveModifierState[VK_RSHIFT] = 0x80; // OS: still held, by this batch's own restore press

  RunCorrection(MaskBit(VK_RSHIFT));

  int i = IndexOf(VK_SHIFT, true); // Right Shift is emitted as VK_SHIFT plus the right scan code
  ASSERT_NE(i, -1) << "nothing was corrected, so there is no tag to inspect";
  ASSERT_EQ(inputs[i].ki.wScan, (WORD)SCANCODE_RSHIFT)
      << "if this is SCAN_FLAG_KEYMAN_KEY_EVENT then the case has stopped testing what it claims to: "
      << "the scan arm would carry the identification and dwExtraInfo would be free to be wrong";

  EXPECT_EQ(inputs[i].ki.dwExtraInfo, (ULONG_PTR)EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP)
      << "the wrap tag is the only signal left once the scan code has been rewritten to 0x36";
  EXPECT_TRUE(IsKeymanInjectedKeyEvent(inputs[i].ki.wScan, inputs[i].ki.dwExtraInfo))
      << "an untagged Right Shift correction feeds the cache as though the user had pressed it";
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
  WM_KEYMAN_MODIFIER_EVENT post precedes the "generated by Keyman" pass-through. Every case on this
  fixture drives ShouldFeedModifierCache -- the production gate -- so every one of them pins the fix.
  The cases that characterise what the gate costs when it is absent are on
  DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER below, with the ungated helpers they need.

  The helpers feed the collapsed VK_SHIFT form; the hook actually reports the re-chiralised VK
  (0xA0/0xA1, measured by DwExtraInfoSurvivesSendInputWhereTheScanCodeDoesNot, in
  keybd_shift.interactive.tests.cpp). Either resolves by
  scan code to the same kbd byte, so this keeps the VK_SHIFT branch covered -- read it as coverage,
  not as a claim about what the hook reports.
*/
class MODIFIER_CACHE_EVENT_ORDER : public KEYBD_SHIFT {
protected:
  // #8064 What the hook does, calling the production decision rather than restating it.
  // ShouldFeedModifierCache (keybd_shift.cpp) is the same function k32_lowlevelkeyboardhook.cpp
  // calls before it posts WM_KEYMAN_MODIFIER_EVENT, so the cases below now fail when its
  // !IsKeymanInjectedKeyEvent term is removed. The test-local ApplyThroughTheGate mirror this
  // replaces did not: with the production term deleted, the whole suite stayed green (FR-016).
  //
  // serializeInput is TRUE throughout: these cases characterise the injected-event arm, and the
  // hook only reaches the post at all with the feed configured on.
  void
  FeedThroughTheGate(BYTE bVk, BYTE bScan, ULONG_PTR extraInfo, BOOL fIsUp) {
    if (ShouldFeedModifierCache(TRUE, bScan, extraInfo)) {
      UpdateModifierCacheFromKeyEvent(kbd, bVk, FALSE, bScan, fIsUp);
    }
  }

  void
  GatedUserReleasesLeftShift() {
    FeedThroughTheGate(VK_LSHIFT, 0x2A, 0, TRUE);
  }

  void
  GatedKeymanReleasesLeftShift() {
    FeedThroughTheGate(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, TRUE);
  }

  void
  GatedKeymanRestoresLeftShift() {
    FeedThroughTheGate(VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, FALSE);
  }

  // Right Shift is the case the scan arm cannot carry: do_keybd_event rewrites 0xFF to 0x36.
  void
  GatedKeymanRestoresRightShift() {
    FeedThroughTheGate(VK_SHIFT, SCANCODE_RSHIFT, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP, FALSE);
  }
};

/*
  #8064 FR-018 / SC-008 -- the one designated structural place. Every case on this fixture
  characterises the defect: each asserts what the *unfixed* path does, so each stays green when the
  fix is reverted. That is deliberate and it is the whole point of them. The fixture name is where
  that intent lives, so a reader who greps for what the suite actually guarantees can subtract these
  four without reading a comment and inferring it.

  Nothing here may be read as a pin on production behaviour. The pins for the same code paths are on
  MODIFIER_CACHE_EVENT_ORDER above, which drives ShouldFeedModifierCache itself and goes red when
  its !IsKeymanInjectedKeyEvent term is deleted. These four drive the ungated helpers below instead:
  the pre-fix feed, reconstructed on purpose, which is why those helpers live here and not on the
  base fixture where something pinning the fix could reach them.

  Measured, not asserted from the armchair: across the 34-mutation sweep behind the FR-018 ledger at
  the top of this file, no revert of any part of the #8064 fix turns any of these four red.

  If one of these ever fails, that is not a regression. It is the defect no longer being
  reproducible, and the case should be deleted rather than repaired.
*/
class DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER : public MODIFIER_CACHE_EVENT_ORDER {
protected:
  // The ungated feed: the cache as it was fed before ShouldFeedModifierCache existed, with Keyman's
  // own injected modifiers going in alongside the user's.

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
};

/*
  The ordering that matters: the user let go while the batch was being built, so their KEYUP was
  posted first and Keyman's two events followed. The restore press applies last, so the cache ends
  up reporting a modifier the user is no longer holding -- a stale byte of Keyman's own making.
*/
TEST_F(DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER, KeymanOwnRestorePressOutlivesTheUsersRealRelease) {
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
TEST_F(DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER, TheUsersReleaseArrivingLastLeavesTheCacheCorrect) {
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
TEST_F(DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER, ABalancedBatchLeavesTheCacheUnchanged) {
  kbd[VK_LSHIFT] = 0x80;

  KeymanReleasesLeftShift();
  KeymanRestoresLeftShift();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80);
}

/*
  The same ordering through the gate production uses -- ShouldFeedModifierCache itself, not a copy
  of it. Keyman's own two events are filtered, so the cache follows the user and the stale byte is
  never created. This is the fix for #8064, and this case is what holds it in place: remove the
  !IsKeymanInjectedKeyEvent term from ShouldFeedModifierCache and the restore press lands in the
  cache again, so this goes red.
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
  The same, for Right Shift, and likewise through the production gate. Its wrap events reach the
  hook with SCANCODE_RSHIFT rather than the 0xFF flag, so the scan arm of the gate cannot see them
  and the dwExtraInfo arm has to. If the tag is ever dropped from do_keybd_event's callers, or the
  !IsKeymanInjectedKeyEvent term from ShouldFeedModifierCache, this is the case that goes red.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, TheGateCoversRightShiftThroughDwExtraInfo) {
  kbd[VK_RSHIFT] = 0x80;

  FeedThroughTheGate(VK_RSHIFT, SCANCODE_RSHIFT, 0, TRUE); // the user's physical release
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
TEST_F(DEFECT_CHARACTERISATION_MODIFIER_CACHE_EVENT_ORDER, TheStaleByteSurvivesTheReconcileBecauseTheOsAgrees) {
  BYTE live[256];

  // The state the first case leaves: cache held, and the OS holding it by Keyman's own doing.
  kbd[VK_LSHIFT] = 0x80;
  memset(live, 0, sizeof(live));
  live[VK_LSHIFT] = 0x80;

  EXPECT_FALSE(ReconcileModifierCache(kbd, live))
      << "cache and OS agree, so there is no disagreement to detect";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80) << "the stale byte survives";

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
  GenericShiftSendInputReflectsInBothAsyncKeyStates, in keybd_shift.interactive.tests.cpp, measures
  that assumption for real.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, GenericVkEventReconcilesAgainstTheChiralLiveReading) {
  BYTE live[256];

  // The hook feed, as it would look for a third party's generic SendInput(VK_SHIFT, scan=0):
  // UpdateModifierCacheFromKeyEvent's VK_SHIFT case collapses it to VK_LSHIFT, because scan 0 is
  // not SCANCODE_RSHIFT.
  UpdateModifierCacheFromKeyEvent(kbd, VK_SHIFT, FALSE, 0, FALSE);
  ASSERT_EQ(kbd[VK_LSHIFT], (BYTE)0x80) << "the generic event did not reach the chiral slot it should";

  // The live reading, if Windows re-chiralises the async state the way
  // GenericShiftSendInputReflectsInBothAsyncKeyStates measures (interactive target).
  memset(live, 0, sizeof(live));
  live[VK_LSHIFT] = 0x80;

  EXPECT_FALSE(ReconcileModifierCache(kbd, live))
      << "the chiral live reading agrees with the cache, so the generic-VK press must survive "
      << "reconciliation intact";
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0x80);
}

/*
  #8064 FR-017 mutation 3's positive, and the pair to the case above. That one covers VK_SHIFT,
  whose chirality is carried by the scan code. Ctrl and Alt are the two whose chirality is carried
  by the extended bit instead, and before this case existed nothing in this file fed
  UpdateModifierCacheFromKeyEvent a VK_CONTROL or a VK_MENU at all -- so inverting that arm was
  survivable by the whole suite.

  Getting the hand wrong does not merely mislabel a byte, it deletes one. With the arm inverted, an
  extended Right Ctrl KEYDOWN files into kbd[VK_LCONTROL] while the OS holds VK_RCONTROL; the
  reconcile then finds a cache byte the live reading does not support and clears it, so the modifier
  the user is physically holding drops out of the cache and the restore half never re-presses it.
  That is A1's dropped-hold shape, manufactured by a swapped ternary -- and the mirror-image byte,
  the one the user is not holding, is the direction that latches.

  Turns red when: either extended-bit arm of UpdateModifierCacheFromKeyEvent is inverted --
  fIsExtendedKey ? VK_LCONTROL : VK_RCONTROL, or the same swap for the Alt pair. Each of the four
  EXPECT_EQ pairs discriminates on its own; the reconcile at the end is what it costs.
*/
TEST_F(MODIFIER_CACHE_EVENT_ORDER, ExtendedBitChiralisesControlAndAltIntoTheSlotTheLiveReadingUses) {
  BYTE live[256];

  // Right Ctrl reaches the hook as VK_CONTROL with the extended bit set; the scan code is 0x1D for
  // both hands, so the bit is the only signal there is.
  UpdateModifierCacheFromKeyEvent(kbd, VK_CONTROL, TRUE, 0x1D, FALSE);
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0x80) << "the extended bit means right";
  EXPECT_EQ(kbd[VK_LCONTROL], (BYTE)0) << "and it must not also, or instead, reach the left slot";

  // Left Ctrl: same VK, same scan code, bit clear.
  Rewind();
  UpdateModifierCacheFromKeyEvent(kbd, VK_CONTROL, FALSE, 0x1D, FALSE);
  EXPECT_EQ(kbd[VK_LCONTROL], (BYTE)0x80) << "no extended bit means left";
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0);

  // Alt, on the same rule and the same shared scan code.
  Rewind();
  UpdateModifierCacheFromKeyEvent(kbd, VK_MENU, TRUE, 0x38, FALSE);
  EXPECT_EQ(kbd[VK_RMENU], (BYTE)0x80) << "the extended bit means right for the Alt pair too";
  EXPECT_EQ(kbd[VK_LMENU], (BYTE)0);

  Rewind();
  UpdateModifierCacheFromKeyEvent(kbd, VK_MENU, FALSE, 0x38, FALSE);
  EXPECT_EQ(kbd[VK_LMENU], (BYTE)0x80);
  EXPECT_EQ(kbd[VK_RMENU], (BYTE)0);

  // What the collapse is for. CaptureLiveModifierState reads the six chiral VKs, so a cache byte
  // filed under the wrong hand is a cache byte the reconcile has grounds to erase.
  Rewind();
  UpdateModifierCacheFromKeyEvent(kbd, VK_CONTROL, TRUE, 0x1D, FALSE);
  memset(live, 0, sizeof(live));
  live[VK_RCONTROL] = 0x80; // the OS agrees, and names the same hand

  EXPECT_FALSE(ReconcileModifierCache(kbd, live))
      << "cache and OS name the same hand, so there is nothing here to reconcile";
  EXPECT_EQ(kbd[VK_RCONTROL], (BYTE)0x80)
      << "with the extended-bit arm inverted this byte was filed as VK_LCONTROL, unsupported by the "
      << "live reading, and the reconcile has just cleared it -- the user is still holding Right "
      << "Ctrl and the cache is not";
}

/*
  IsKeymanInjectedKeyEvent decides which events may feed the modifier cache. The mstsc and OSK rows
  are regression guards: filtering on LLKHF_INJECTED, or on dwExtraInfo != 0, would classify those
  as Keyman's and strip a modifier the user or the OSK genuinely holds.
*/
class IS_KEYMAN_INJECTED_KEY_EVENT : public ::testing::Test {};

TEST_F(IS_KEYMAN_INJECTED_KEY_EVENT, TheScanFlagAloneIsEnough) {
  // The scan arm stands alone because those events carry no tag today, not because keybd_event
  // lacks a channel for one: its fourth parameter is dwExtraInfo, and all five direct callers
  // already pass it, as 0 (keyman32.cpp:924-925, kmhook_keyboard.cpp:147,
  // kmprocessactions.cpp:101-102). Tagging those five is what would retire this arm -- which makes
  // the TODO this row overrides, keyman64.h's "Deprecate overloading of scancodes and use
  // dwExtraInfo instead", actionable again.
  EXPECT_TRUE(IsKeymanInjectedKeyEvent(SCAN_FLAG_KEYMAN_KEY_EVENT, 0))
      << "Keyman's five keybd_event callers pass dwExtraInfo 0 today, so the scan arm has to stand alone";
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
// #8064 The message-post seam, recorded. PostKeyEventAndDecideEat takes a PPOSTMESSAGEFN
// (serialkeyeventcommon.h) for the same reason PrepareInjectedInputBatch takes a PGETASYNCKEYSTATE:
// gmock is not linked into keyman32.tests.vcxproj, so the suite binds file-local stubs. Two of them,
// one per outcome, rather than one stub with a mutable result flag -- what these cases are about is
// which outcome the production code is looking at, so the outcome belongs in the binding.
int g_postCalls     = 0;
HWND g_postHwnd     = NULL;
UINT g_postMsg      = 0;
WPARAM g_postWParam = 0;
LPARAM g_postLParam = 0;

void
RecordPost(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) {
  g_postCalls++;
  g_postHwnd   = hWnd;
  g_postMsg    = Msg;
  g_postWParam = wParam;
  g_postLParam = lParam;
}

// The handoff succeeded: the serializer owns the event now and will re-inject it.
BOOL WINAPI
StubPostSucceeds(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) {
  RecordPost(hWnd, Msg, wParam, lParam);
  return TRUE;
}

// The handoff failed. Not hypothetical on this path: PostMessage returns FALSE on a full thread
// queue -- WM_USER messages count against the 10,000 limit, and a stalled serializer is exactly how
// it fills -- and on a UIPI refusal.
BOOL WINAPI
StubPostFails(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) {
  RecordPost(hWnd, Msg, wParam, lParam);
  return FALSE;
}
} // namespace

/*
  #8064 PostKeyEventAndDecideEat, the low level hook's eat decision, exercised as production code
  rather than mirrored: k32_lowlevelkeyboardhook.cpp is entirely inside #ifndef _WIN64 and the suite
  cannot link it, so the decision lives in keybd_shift.cpp and the hook calls it (:288).

  ::testing::Test rather than KEYBD_SHIFT, following IS_KEYMAN_INJECTED_KEY_EVENT above: the
  function is pure and logs nothing, so it needs neither Globals_InitProcess nor an event buffer.
*/
class POST_KEY_EVENT_AND_DECIDE_EAT : public ::testing::Test {
public:
  void
  SetUp() {
    RewindPostRecorder();
  }

protected:
  static void
  RewindPostRecorder() {
    g_postCalls  = 0;
    g_postHwnd   = NULL;
    g_postMsg    = 0;
    g_postWParam = 0;
    g_postLParam = 0;
  }

  // A stand-in for the serializer's message-only window. The stubs record it and never dereference
  // it, so this needs no window, no message queue and no interactive desktop.
  static HWND
  ServerWindow() {
    return reinterpret_cast<HWND>(static_cast<ULONG_PTR>(0x8064));
  }

  // A Left Shift KEYUP as the hook hands it over, already converted by
  // LLKHFFlagstoWMKeymanKeyEventFlags (k32_lowlevelkeyboardhook.cpp:83): scan code in the high word,
  // KEYEVENTF_KEYUP in the low. A modifier KEYUP because that is the event whose loss re-asserts
  // #8064 -- lose an output keystroke and the user retypes it; lose this one and Shift stays down.
  static DWORD
  LeftShiftKeyUpFlags() {
    return (0x2A << 16) | KEYEVENTF_KEYUP;
  }
};

/*
  #8064 FR-017 mutation 2's positive. The hook's return value decides whether the user's key event
  survives at all -- returning 1 eats it, and nothing else will ever deliver it -- so the eat is only
  safe once the handoff has actually succeeded. Eating on trust destroys a key event every time
  PostMessage fails, and for a modifier KEYUP that is exactly how #8064 re-asserts: the OS stays
  latched, the cache still says down, and the clear-only reconcile can never see a disagreement to
  clear. Unserialized beats destroyed.

  Turns red when: PostKeyEventAndDecideEat's success check is replaced so the event is eaten
  unconditionally -- ignore pfnPost's result and return TRUE. The control in the second half is what
  stops a hard-coded FALSE satisfying the first half instead.
*/
TEST_F(POST_KEY_EVENT_AND_DECIDE_EAT, AFailingPostPassesTheEventThroughInsteadOfEatingIt) {
  EXPECT_FALSE(PostKeyEventAndDecideEat(ServerWindow(), VK_LSHIFT, LeftShiftKeyUpFlags(), StubPostFails))
      << "the handoff failed, so eating this KEYUP would destroy it outright and leave Shift latched";
  EXPECT_EQ(g_postCalls, 1) << "the post must actually have been attempted -- a FALSE arriving from the "
                            << "NULL-window branch would pin nothing about the success check";

  // The control. Without it a hard-coded FALSE would satisfy the assertion above.
  RewindPostRecorder();

  EXPECT_TRUE(PostKeyEventAndDecideEat(ServerWindow(), VK_LSHIFT, LeftShiftKeyUpFlags(), StubPostSucceeds))
      << "a successful handoff must still eat: the serializer owns the event now and re-injects it";
  EXPECT_EQ(g_postCalls, 1);
}

/*
  #8064 FR-026 / SC-006: a real divergence route, end to end, on the default CI target -- no stall,
  no input injection, no interactive desktop, no timing. Both routes below run to completion in
  microseconds and answer the same way on every machine.

  Why these two and not 002's table. 002's launch-seed, NULL-window, destroyed-handoff and
  pass-through routes are indistinguishable to the reconcile: they all reduce to a cache byte the
  live reading does not support, which is the same two-line stub setup RECONCILE_MODIFIER_CACHE
  already covers. What was not reachable until PostKeyEventAndDecideEat was extracted out of
  k32_lowlevelkeyboardhook.cpp -- a file this suite cannot link, being wholly inside #ifndef _WIN64
  -- is the moment divergence is *created* rather than repaired: the hook deciding the fate of an
  event it could not hand off.

  The two failures are not the same failure. A failing post announces itself. A destroyed or
  not-yet-created serializer window does not: PostMessage to a NULL hwnd does not fail, it misroutes
  the message to the calling thread's own queue and returns success, so a caller that trusted the
  return value would eat an event that nothing will ever deliver, and would believe it had
  serialized it. That is why the NULL is tested before the post is attempted rather than left to it.

  Turns red when: either FALSE branch is removed. Drop the NULL check and the first half eats an
  event on the strength of a post that went nowhere; eat unconditionally and the second half eats
  one whose post failed. Either mutation destroys the user's KEYUP, and a destroyed modifier KEYUP
  is #8064 itself -- the OS stays latched, the cache still says down, and the clear-only reconcile
  has no disagreement to find. The successful-post control at the end is what stops a function that
  never eats from passing both halves.
*/
TEST_F(POST_KEY_EVENT_AND_DECIDE_EAT, NeitherHandoffFailureRouteEatsTheEvent) {
  // Route 1 -- the destroyed handoff. No serializer window, so no post is attempted at all. The
  // stub is the one that would have succeeded: the point is that its answer is never consulted.
  EXPECT_FALSE(PostKeyEventAndDecideEat(NULL, VK_LSHIFT, LeftShiftKeyUpFlags(), StubPostSucceeds))
      << "there is no serializer to hand off to, so this KEYUP must reach the OS unserialized";
  EXPECT_EQ(g_postCalls, 0)
      << "a post to a NULL hwnd does not fail -- it misroutes to this thread's own queue and reports "
      << "success -- so it must not be attempted, and its answer must not be believed";

  // Route 2 -- the handoff failure. The window is there, the post is made, and it fails.
  RewindPostRecorder();

  EXPECT_FALSE(PostKeyEventAndDecideEat(ServerWindow(), VK_LSHIFT, LeftShiftKeyUpFlags(), StubPostFails))
      << "the handoff failed, so this KEYUP must likewise pass through unserialized";
  ASSERT_EQ(g_postCalls, 1) << "the route is only real if the post was attempted";
  EXPECT_EQ(g_postHwnd, ServerWindow());
  EXPECT_EQ(g_postMsg, (UINT)WM_KEYMAN_KEY_EVENT) << "the serializer reads the event off this message";
  EXPECT_EQ(g_postWParam, (WPARAM)VK_LSHIFT) << "wParam is the virtual key";
  EXPECT_EQ(g_postLParam, (LPARAM)LeftShiftKeyUpFlags()) << "lParam is the flags the caller already converted";

  // And the route that is not a failure, so neither half above is satisfied by never eating.
  RewindPostRecorder();

  EXPECT_TRUE(PostKeyEventAndDecideEat(ServerWindow(), VK_LSHIFT, LeftShiftKeyUpFlags(), StubPostSucceeds))
      << "the serializer accepted the event and will re-inject it, so this one is eaten";
}

namespace {
/*
  #8064 FR-002 / FR-006 -- the diagnostic recorder the seam exists for.

  SendDebugMessageFormat resolves to ETW (K32_DBG.CPP:189) and nothing in this process can read it
  back, so a test that wanted to assert "the batch reported this" had no way to. Binding
  PMODIFIERDIAGNOSTIC to this recorder is what makes FR-002 and FR-006 assertable with NO machine:
  no interactive desktop, no injection, no timing, no desktop switch. That is the whole point of
  the seam, and it is why these live on the default target rather than in
  keybd_shift.interactive.tests.cpp.

  A file-local recorder rather than a mock, for the same reason as StubGetAsyncKeyState above:
  gmock is not linked into keyman32.tests.vcxproj.
*/
struct DiagnosticRecord {
  ModifierDiagnosticCode code;
  BYTE vk;
};

DiagnosticRecord g_diagnostics[16];
int g_diagnosticCount = 0;

void
RecordDiagnostic(ModifierDiagnosticCode code, BYTE vk) {
  if (g_diagnosticCount < _countof(g_diagnostics)) {
    g_diagnostics[g_diagnosticCount].code = code;
    g_diagnostics[g_diagnosticCount].vk   = vk;
    g_diagnosticCount++;
  }
}

int
CountDiagnostics(ModifierDiagnosticCode code) {
  int count = 0;
  for (int i = 0; i < g_diagnosticCount; i++) {
    if (g_diagnostics[i].code == code) {
      count++;
    }
  }
  return count;
}

bool
HasDiagnostic(ModifierDiagnosticCode code, BYTE vk) {
  for (int i = 0; i < g_diagnosticCount; i++) {
    if (g_diagnostics[i].code == code && g_diagnostics[i].vk == vk) {
      return true;
    }
  }
  return false;
}
} // namespace

/*
  #8064 FR-002 / FR-006. A batch that cannot keep a hold must NAME it.

  FR-001 governs what the batch does, and it does not change here: absent a signal that says what
  the user holds, the release and restore halves are untouched and some holds are still dropped.
  Both live-state-only alternatives are refuted in the spec. What changes is that a dropped hold
  stops being silent, so a field report has something to match instead of a user sentence.

  Every case here asserts the BEHAVIOUR is unchanged as well as that the report happened. A
  diagnostic that quietly altered the batch would be a worse bug than the silence it replaced.
*/
class MODIFIER_DIAGNOSTIC : public PREPARE_INJECTED_INPUT_BATCH {
public:
  void
  SetUp() {
    PREPARE_INJECTED_INPUT_BATCH::SetUp();
    g_diagnosticCount = 0;
    memset(g_diagnostics, 0, sizeof(g_diagnostics));
  }

protected:
  void
  RunBatchReporting(BOOL feedIsConfigured = TRUE) {
    restorePressedMask = kRestoreMaskUnwritten;
    n                  = PrepareInjectedInputBatch(
      inputs, kbd, &sharedData, StubGetAsyncKeyState, feedIsConfigured, &restorePressedMask, RecordDiagnostic);
  }
};

/*
  FR-002. The OS holds Left Control, the cache does not claim it, so the release half releases it
  and the restore half -- which reads the cache -- does not press it back. That is a hold this batch
  DROPS, and it is the residue FR-001 accepts: a console window or the secure desktop, where the
  cache feed never saw the KEYDOWN.

  The batch is right to do this; releasing on the cache alone reopens 002/FR-001's silent text
  destruction. What was wrong is that it happened without a word.
*/
TEST_F(MODIFIER_DIAGNOSTIC, ADroppedHoldIsNamedInTheDiagnostic) {
  kbd[VK_LCONTROL]                 = 0;    // cache: never saw the KEYDOWN
  g_liveModifierState[VK_LCONTROL] = 0x80; // OS: the user is holding it
  AddOutputKey('A');

  RunBatchReporting();

  // The behaviour FR-001 pins, asserted first: the hold IS dropped, deliberately.
  // do_keybd_event collapses VK_LCONTROL to VK_CONTROL, so the queued events name the generic VK
  // while the cache and live arrays stay chiral.
  EXPECT_EQ(Count(VK_CONTROL, true), 1) << "the release half must still release what the OS holds";
  EXPECT_EQ(Count(VK_CONTROL, false), 0)
      << "the restore half reads the cache, which does not claim it, so nothing presses it back -- "
      << "this is the accepted drop, not a defect to fix here";

  // And the report, which is what FR-002 adds.
  EXPECT_EQ(CountDiagnostics(ReleasedWithoutCacheClaim), 1)
      << "the batch dropped a hold and said nothing. That silence is FR-002's subject: a user "
      << "reports a modifier that went dead and there is nothing in the log to match it against";
  EXPECT_TRUE(HasDiagnostic(ReleasedWithoutCacheClaim, VK_LCONTROL))
      << "the report must NAME the modifier -- 'a hold was dropped' does not let anyone correlate "
      << "it with what the user was pressing";
}

/*
  FR-002's negative, and the reason the code is not simply emitted on every release. A modifier the
  cache DOES claim is released and pressed straight back, so no hold is lost and there is nothing
  to report. Without this case the diagnostic could fire on every ordinary batch and mean nothing.
*/
TEST_F(MODIFIER_DIAGNOSTIC, AHoldTheCacheClaimsIsRestoredAndNotReported) {
  kbd[VK_LSHIFT]                 = 0x80;
  g_liveModifierState[VK_LSHIFT] = 0x80;
  AddOutputKey('A');

  RunBatchReporting();

  ASSERT_EQ(Count(VK_SHIFT, true), 1) << "released by the release half";
  ASSERT_EQ(Count(VK_SHIFT, false), 1) << "and pressed back by the restore half";
  EXPECT_EQ(CountDiagnostics(ReleasedWithoutCacheClaim), 0)
      << "nothing was lost, so a report here would be noise -- and noise is what stops anyone "
      << "reading the ones that matter";
}

/*
  FR-002 with the feed off. Both halves read the same kbd, so the condition cannot arise: whatever
  the release half released, the restore half presses back. Asserted rather than assumed, because
  the obvious implementation -- compare live against kbd unconditionally -- would report a drop on
  every !feedIsConfigured batch that had any live modifier at all.
*/
TEST_F(MODIFIER_DIAGNOSTIC, WithTheFeedOffThereIsNoDropToReport) {
  kbd[VK_LSHIFT]                   = 0;
  g_liveModifierState[VK_LSHIFT]   = 0x80;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  AddOutputKey('A');

  RunBatchReporting(FALSE);

  EXPECT_EQ(Count(VK_SHIFT, true), 0) << "the release set is kbd here, which claims nothing";
  EXPECT_EQ(Count(VK_CONTROL, true), 0) << "nor Control, for the same reason";
  EXPECT_EQ(CountDiagnostics(ReleasedWithoutCacheClaim), 0)
      << "with the feed off the release half never releases what the cache does not claim, so "
      << "there is no dropped hold and nothing to say about one";
}

/*
  FR-006 / FR-007. All six managed modifiers read up live while the cache claims two held. The
  reconcile clears both, the release half (live) releases nothing, the restore half (the now-empty
  cache) presses nothing -- two holds gone at once, with cache and OS in perfect agreement
  afterwards. This is the shape a desktop switch leaves: the user held Ctrl+Shift, went to the
  secure desktop or a console, let go where the feed could not see it, and came back.

  ReconcileModifierCache is structurally blind to it forever, because it tests for
  cache-up-and-live-down and this state IS cache-up-and-live-down -- it is doing exactly its job.
  So the only thing that can be added is that it says so.
*/
TEST_F(MODIFIER_DIAGNOSTIC, TwoHoldsLostAtOnceLookLikeADesktopSwitchAndAreReportedAsOne) {
  kbd[VK_LSHIFT]   = 0x80; // cache: the user was holding both
  kbd[VK_LCONTROL] = 0x80;
  // and every live reading is up
  AddOutputKey('A');

  RunBatchReporting();

  // FR-007: the reconcile MUST still run, and this guard causes no press that would not otherwise
  // be emitted. Asserted before the report, because getting this wrong reopens #8064.
  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the reconcile must still clear -- suppressing it here is "
                                     << "what an earlier draft did, and it reintroduced #8064";
  EXPECT_EQ(kbd[VK_LCONTROL], (BYTE)0);
  EXPECT_EQ(Count(VK_SHIFT, false), 0) << "and no modifier KEYDOWN may be manufactured";
  EXPECT_EQ(Count(VK_CONTROL, false), 0);

  EXPECT_EQ(CountDiagnostics(PossibleDesktopSwitch), 1)
      << "two holds were lost in one batch with cache and OS agreeing afterwards, and nothing was "
      << "reported. FR-006 exists because that state is invisible to every other mechanism here";
  EXPECT_TRUE(HasDiagnostic(PossibleDesktopSwitch, 0))
      << "once per batch with vk = 0, per contracts/modifier-state.md section 4: the condition is a "
      << "property of the batch, and which keys were lost is already in ReconcileModifierCache's "
      << "own per-VK clearing lines. A per-key form here would bury the batch-level signal";
}

/*
  FR-006's threshold, and it is the load-bearing half of the requirement. EXACTLY ONE modifier held
  at launch and released before the feed was live is the NORMAL launch-seed case 002's reconcile
  exists to clear. Firing here would put a line in the log on an ordinary session, and a diagnostic
  that cries wolf is one nobody reads when it matters.
*/
TEST_F(MODIFIER_DIAGNOSTIC, OneLostHoldIsTheLaunchSeedCaseAndIsNotReportedAsADesktopSwitch) {
  kbd[VK_LSHIFT] = 0x80; // the launch seed caught it; the user let go before the feed was live
  AddOutputKey('A');

  RunBatchReporting();

  ASSERT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the reconcile still clears it -- that is 002's whole route";
  EXPECT_EQ(CountDiagnostics(PossibleDesktopSwitch), 0)
      << "one cleared modifier is the launch-seed case, not a desktop switch. Two or more is the "
      << "threshold precisely so this session does not get a warning it cannot act on";
}

/*
  #8064 FR-015b / A9. What the caller has to know when SendInput sends fewer events than it was
  given.

  serialkeyeventserver.cpp checks `SendInput(...) == 0`, and its own comment concedes that
  `!= m_nInputs` is the honest check. The excuse it gives -- "not a latch source, so left alone: the
  restore KEYDOWNs are last, so truncation drops presses, never releases" -- is true about latching
  and FALSE about the mask. The restore presses being last is precisely why a short send drops THEM,
  and the mask handed to the verification pass then names presses that never reached the OS. That
  pass corrects on cache-up-and-live-down; for a press that was never sent, live IS down, so it
  releases a modifier on the strength of an event that does not exist.

  The remedy has to be EXACT, not conservative. Clearing the whole mask on any short send would
  suppress the correction for the presses that did land -- a second dropped hold traded for the
  first. So the batch reports, per mask bit, the buffer index of the press that bit stands for, and
  the caller clears exactly the bits at or past the send boundary.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, EachRestorePressIsLocatableInTheBufferSoAShortSendIsExact) {
  kbd[VK_LSHIFT]                   = 0x80;
  kbd[VK_LCONTROL]                 = 0x80;
  g_liveModifierState[VK_LSHIFT]   = 0x80;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  AddOutputKey('A');

  RunBatch();

  ASSERT_NE(restorePressedMask, kRestoreMaskUnwritten) << "the mask out-param was never written";
  ASSERT_NE(restorePressedMask, (DWORD)0) << "this batch must restore something or it tests nothing";

  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    const bool bitSet = (restorePressedMask & (1u << i)) != 0;

    ASSERT_NE(restoreEventIndex[i], kRestoreEventIndexUnwritten)
        << "slot " << i << ": the index out-param was never written, so a caller facing a short "
        << "send has no way to tell which of its restore presses actually went out. That is A9: "
        << "the mask asserts presses the OS never received";

    if (!bitSet) {
      EXPECT_EQ(restoreEventIndex[i], -1)
          << "slot " << i << ": no press for this bit, so its index must say so explicitly rather "
          << "than hold a stale or plausible-looking buffer position";
      continue;
    }

    ASSERT_GE(restoreEventIndex[i], 0) << "slot " << i << ": bit set, so there is a press to locate";
    ASSERT_LT(restoreEventIndex[i], n) << "slot " << i << ": index past the end of the batch";

    // The index must point at the press it claims to, or clearing by index corrects the wrong bit.
    const INPUT &ev = inputs[restoreEventIndex[i]];
    EXPECT_EQ(ev.ki.dwFlags & KEYEVENTF_KEYUP, (DWORD)0)
        << "slot " << i << ": the index points at a KEYUP, not at a restore press";
    EXPECT_EQ(ev.ki.wVk, (WORD)CollapsedVk(KeymanModifierVks[i]))
        << "slot " << i << ": the index points at an event for a different key";
  }

  // And the property the whole mechanism exists for, stated the way the caller will use it. Two
  // boundaries, because "exact" means both directions: at the first restore press EVERY set bit is
  // identified as dropped, and one past the last one NONE is. A conservative
  // clear-the-whole-mask-on-any-short-send passes the first and fails the second.
  int firstPress = n, lastPress = -1, setBits = 0;
  for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
    if (!(restorePressedMask & (1u << i))) {
      continue;
    }
    setBits++;
    if (restoreEventIndex[i] < firstPress) {
      firstPress = restoreEventIndex[i];
    }
    if (restoreEventIndex[i] > lastPress) {
      lastPress = restoreEventIndex[i];
    }
  }
  ASSERT_EQ(setBits, 2) << "Left Shift and Left Control were both restored";

  EXPECT_EQ(DroppedBitCount(restorePressedMask, restoreEventIndex, firstPress), setBits)
      << "a send that stopped at the first restore press delivered none of them, so every bit must "
      << "be identified as dropped";
  EXPECT_EQ(DroppedBitCount(restorePressedMask, restoreEventIndex, lastPress + 1), 0)
      << "a send that delivered every restore press must clear no bit at all -- this is the half a "
      << "conservative whole-mask clear gets wrong, and getting it wrong drops a second hold";
}

/*
  #8064 FR-014 / A8. The verification correction's prefix keystroke goes somewhere.

  PrepareModifierVerificationCorrection emits through keybd_shift_release, and keybd_shift_release
  sends a dummy prefix down/up pair before the first modifier KEYUP. Inside a batch that is exactly
  right and must not change: the prefix sits between the keyboard's own output keys, and it exists
  because an isolated Alt release opens the window menu.

  A verification correction is NOT inside a batch. It is a standalone SendInput, fired from a posted
  message some time after the batch's own SendInput returned, into whatever has focus BY THEN. So
  the prefix down/up pair lands in the user's document, or in a control that has its own idea of
  what that key means. And the prefix VK is registry-overridable -- REGSZ_ZapVirtualKeyCode,
  k32_globals.cpp:378 -- so it cannot be assumed to be unassigned on every machine.

  The prefix exists for ONE reason: an isolated Alt release opens the window menu. A correction that
  releases only Shift, or only Ctrl, needs no protection from that and should send no prefix. So the
  rule is not "suppress the prefix" but "send it only when the correction set contains an Alt".
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, AShiftOnlyCorrectionSendsNoPrefixKeystroke) {
  kbd[VK_LSHIFT]                 = 0;
  g_liveModifierState[VK_LSHIFT] = 0x80;

  RunCorrection(MaskBit(VK_LSHIFT));

  ASSERT_EQ(Count(VK_SHIFT, true), 1) << "the correction itself must still happen";

  EXPECT_EQ(Count(PREFIX_VK, false), 0)
      << "a standalone correction that releases only Shift sent a prefix KEYDOWN into whatever has "
      << "focus now. The prefix exists to stop an isolated ALT release opening the window menu; "
      << "there is no Alt here, so this keystroke has no job and a destination nobody chose";
  EXPECT_EQ(Count(PREFIX_VK, true), 0) << "and its KEYUP likewise";
  EXPECT_EQ(n, 1) << "one KEYUP is the whole correction; anything more is stray input";
}

/*
  FR-014's positive, and the reason the prefix is made conditional rather than removed. An Alt-family
  VK in the correction set is the case the prefix was written for, so it must still be sent. Without
  this case the fix could delete the prefix outright and nothing would notice until an Alt
  correction popped a window menu in the field.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, AnAltCorrectionStillSendsThePrefixKeystroke) {
  kbd[VK_LMENU]                 = 0;
  g_liveModifierState[VK_LMENU] = 0x80;

  RunCorrection(MaskBit(VK_LMENU));

  ASSERT_EQ(Count(VK_MENU, true), 1) << "the correction itself";
  EXPECT_EQ(Count(PREFIX_VK, false), 1)
      << "an isolated Alt release opens the window menu, which is precisely what the prefix is for. "
      << "Suppressing it here would trade stray input for a visible menu";
  EXPECT_EQ(Count(PREFIX_VK, true), 1);
}

/*
  FR-014, the mixed set. Shift and Alt both need correcting, so the prefix is required -- the rule is
  about whether ANY Alt is present, not about whether the set is Alt-only. Stated as its own case
  because the obvious implementation, checking only the first VK in the set, gets this wrong: Shift
  sorts before Alt in KeymanModifierVks.
*/
TEST_F(PREPARE_MODIFIER_VERIFICATION_CORRECTION, ACorrectionContainingAnyAltSendsThePrefixKeystroke) {
  kbd[VK_LSHIFT]                = 0;
  kbd[VK_RMENU]                 = 0;
  g_liveModifierState[VK_LSHIFT] = 0x80;
  g_liveModifierState[VK_RMENU]  = 0x80;

  RunCorrection(MaskBit(VK_LSHIFT) | MaskBit(VK_RMENU));

  ASSERT_EQ(Count(VK_SHIFT, true), 1);
  ASSERT_EQ(Count(VK_MENU, true), 1);
  EXPECT_EQ(Count(PREFIX_VK, false), 1)
      << "an Alt is in the set, so the prefix is needed -- and it must be found wherever in the set "
      << "the Alt sits, not only when the set begins with one";
}

/*
  FR-014's boundary in the other direction, and the one that keeps the batch path honest. Inside
  PrepareInjectedInputBatch the release half's prefix is UNCHANGED: it is not stray input there, it
  is bracketed by the batch's own output keys, and removing it would reopen the window-menu problem
  for every Alt+key rule. The parameter defaults to TRUE precisely so this path is untouched.
*/
TEST_F(PREPARE_INJECTED_INPUT_BATCH, TheBatchReleaseHalfStillSendsItsPrefixForAShiftOnlyRelease) {
  kbd[VK_LSHIFT]                 = 0x80;
  g_liveModifierState[VK_LSHIFT] = 0x80;
  AddOutputKey('A');

  RunBatch();

  EXPECT_EQ(Count(PREFIX_VK, false), 2)
      << "one prefix pair from the release half and one from the restore half. FR-014 changes the "
      << "STANDALONE correction only; suppressing it here would be a behaviour change to every "
      << "batch that touches a modifier";
  EXPECT_EQ(Count(PREFIX_VK, true), 2);
}
/*
  #8064 W5 / FR-101 ... FR-105 -- the user-held signal.

  THE PROBLEM IT EXISTS FOR (A0). The modifier cache is fed only by Keyman's low level keyboard hook.
  Wherever that hook does not see a KEYDOWN -- a console window, the secure desktop, the pass-through
  paths -- the cache never learns that the user is holding a modifier. The batch's release half reads
  live OS state and releases it; the restore half reads the cache and does not press it back. The
  hold is dropped, and no amount of reconciling can recover it, because there is nothing in the cache
  to reconcile.

  THE SHAPE OF THE FIX, and why it is not "just read the OS". Live OS state cannot distinguish "the
  user is holding it" from "we pressed it ourselves one message ago", so restoring from it
  manufactures unmatched presses -- #8064 from a new direction (FR-102, and both live-state-only
  alternatives are refuted in the spec). The signal is a THIRD input: what a source that is not
  Keyman's own last said. It is fed from raw keyboard input, and it reports UNKNOWN rather than
  stale.

  THE RESTORE SET BECOMES `cache OR (held & ~unknown)`. Widened by a user-held observation only. The
  release half is untouched and still reads live.

  THE SHARPEST RISK IN THE WHOLE SPEC LIVES HERE: a stale shadow manufactures a press that nothing
  can detect, because cache and OS then AGREE and ReconcileModifierCache tests for disagreement. That
  is why FR-104a's per-key poisoning, FR-104b's displacement detection and FR-103a's signal-aware
  verification pass are all three required, and why every case below asserts the *unknown* half as
  well as the *held* half.
*/
class USER_HELD_SIGNAL : public PREPARE_INJECTED_INPUT_BATCH {
public:
  void
  SetUp() {
    PREPARE_INJECTED_INPUT_BATCH::SetUp();
    memset(&userHeld, 0, sizeof(userHeld));
  }

protected:
  UserHeldModifierSignal userHeld;

  void
  RunBatchWithSignal() {
    restorePressedMask = kRestoreMaskUnwritten;
    for (int i = 0; i < KEYMAN_MODIFIER_VK_COUNT; i++) {
      restoreEventIndex[i] = kRestoreEventIndexUnwritten;
    }
    n = PrepareInjectedInputBatch(
      inputs, kbd, &sharedData, StubGetAsyncKeyState, TRUE, &restorePressedMask, NULL,
      restoreEventIndex, &userHeld);
  }
};

/*
  FR-101, the whole point. The A0 window exactly: the OS reports Left Control held, the CACHE does
  not claim it -- the hook never saw the KEYDOWN, because it happened in a console window -- and the
  SIGNAL does, because raw input is not routed through that hook.

  Today the restore half reads the cache alone, so this hold is dropped. With the signal it is not.
*/
TEST_F(USER_HELD_SIGNAL, AHoldOnlyTheSignalKnowsAboutIsRestored) {
  kbd[VK_LCONTROL]                 = 0;    // the cache never saw it
  g_liveModifierState[VK_LCONTROL] = 0x80; // the OS knows it is down
  userHeld.held[VK_LCONTROL]       = 0x80; // and so does the signal, from raw input
  userHeld.unknown[VK_LCONTROL]    = 0;    // and it can currently speak for this key
  AddOutputKey('A');

  RunBatchWithSignal();

  EXPECT_EQ(Count(VK_CONTROL, true), 1)
      << "the release half is unchanged: it still releases what the OS reports held";
  EXPECT_EQ(Count(VK_CONTROL, false), 1)
      << "the signal says the USER is holding Left Control, so the restore half must press it back. "
      << "Without this the hold is dropped and the user's Ctrl goes dead mid-chord -- the A0 window, "
      << "which no reconcile can recover because the cache has nothing in it to reconcile";
}

/*
  FR-102, and it is the guard rail on the case above. The OS says held; NOTHING else does. That is
  indistinguishable from "Keyman pressed it one message ago", so it must NOT be restored -- restoring
  here is #8064 arriving from a new direction, and on hardware with no physical Right Ctrl the user
  cannot clear it at all.

  Green today and must stay green. It is the case that stops FR-101 being implemented as "restore
  from live state", which is the refuted alternative.
*/
TEST_F(USER_HELD_SIGNAL, ALiveOnlyHoldIsStillNotRestored) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80; // the OS says held
  // and neither the cache nor the signal claims it
  AddOutputKey('A');

  RunBatchWithSignal();

  EXPECT_EQ(Count(VK_CONTROL, true), 1) << "released, as the release half always has";
  EXPECT_EQ(Count(VK_CONTROL, false), 0)
      << "nothing that can tell a finger from an injection claims this key, so pressing it back "
      << "would manufacture an unmatched KEYDOWN. FR-102: the set widens by a USER-HELD signal "
      << "only, never by live OS state";
}

/*
  FR-104, the unknown half, and the reason the signal is two arrays and not one. A key the signal
  claims held but cannot currently SPEAK for is worth exactly nothing: the effective read is
  `held & ~unknown`, everywhere, with no exceptions.

  This is the UAC case in miniature -- hold Ctrl, release it on the secure desktop where the feed
  cannot see it, come back -- and it is the one that manufactures a press if it is got wrong.
*/
TEST_F(USER_HELD_SIGNAL, APoisonedKeyIsNotRestoredEvenThoughTheSignalSaysHeld) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  userHeld.held[VK_LCONTROL]       = 0x80; // the last thing the signal saw was a KEYDOWN
  userHeld.unknown[VK_LCONTROL]    = 0x80; // but it cannot speak for this key now
  AddOutputKey('A');

  RunBatchWithSignal();

  EXPECT_EQ(Count(VK_CONTROL, false), 0)
      << "the signal's last observation is stale and it says so. Restoring on a stale 'held' is the "
      << "sharpest risk in this design: it manufactures an unmatched press in the ONE state "
      << "ReconcileModifierCache is structurally blind to, because cache and OS then agree";
}

/*
  FR-101 with no signal at all. NULL means the cache alone -- FR-104's fallback, and the shape this
  code has if US0 is struck. Asserted rather than assumed: every existing test in this file calls
  the batch without a signal, so a fix that dereferenced the pointer unconditionally would take the
  whole suite down, but one that treated NULL as "everything held" would not, and that is the
  dangerous direction.
*/
TEST_F(USER_HELD_SIGNAL, NoSignalMeansTheCacheAloneAndNotAnEmptyOne) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  AddOutputKey('A');

  // deliberately the no-signal call
  restorePressedMask = kRestoreMaskUnwritten;
  n                  = PrepareInjectedInputBatch(
    inputs, kbd, &sharedData, StubGetAsyncKeyState, TRUE, &restorePressedMask);

  EXPECT_EQ(Count(VK_CONTROL, false), 0) << "no signal, so the cache alone decides the restore set";
  EXPECT_EQ(Count(VK_CONTROL, true), 1) << "and the release half is untouched by any of this";
}

/*
  FR-103. The mask has to cover the presses FR-101 added, or the verification pass cannot correct a
  release that raced the batch -- and FR-101's presses are precisely the ones most likely to race,
  since they are made on a signal about a key the user is actively holding.
*/
TEST_F(USER_HELD_SIGNAL, TheMaskCoversAPressMadeOnTheSignalAlone) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  userHeld.held[VK_LCONTROL]       = 0x80;
  AddOutputKey('A');

  RunBatchWithSignal();

  ASSERT_NE(restorePressedMask, kRestoreMaskUnwritten) << "the out-param was never written";

  DWORD expected = 0;
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    if (KeymanModifierVks[i] == VK_LCONTROL) {
      expected = 1u << i;
    }
  }
  ASSERT_NE(expected, (DWORD)0) << "VK_LCONTROL is not in KeymanModifierVks; the test is wrong";

  EXPECT_EQ(restorePressedMask & expected, expected)
      << "the restore half pressed Left Control on the signal's word, and the mask does not say so. "
      << "The verification pass is scoped by this mask, so a press it omits can never be corrected";
}

/*
  #8064 FR-105. The phantom case, UNCHANGED with the signal in place. This is the assertion that says
  US0 did not reopen what 002 closed.

  Two shapes, both of them #8064's own:

    - the stale cache byte: the cache claims held, live says up, the signal claims nothing. The
      reconcile clears it and nothing is pressed.
    - the live-only hold: live says held, neither the cache nor the signal claims it. Released, and
      NOT restored.

  If either of these ever restores, #8064 is back, and on hardware with no physical Right Ctrl the
  user cannot clear it.
*/
TEST_F(USER_HELD_SIGNAL, TheStaleCacheByteIsStillClearedAndNothingIsPressed) {
  kbd[VK_LSHIFT] = 0x80; // the dropped-KEYUP residue: this is #8064's own state
  // live says up, and the signal claims nothing
  AddOutputKey('A');

  RunBatchWithSignal();

  EXPECT_EQ(kbd[VK_LSHIFT], (BYTE)0) << "the reconcile must still clear the stale byte";
  EXPECT_EQ(Count(VK_SHIFT, false), 0)
      << "an unmatched modifier KEYDOWN latches machine-wide once SendInput runs. That is #8064, "
      << "and adding a third input must not create a new route to it";
  EXPECT_EQ(Count(VK_SHIFT, true), 0) << "and nothing was held, so nothing needed releasing either";
}

/*
  FR-105's other half, and the one the signal makes newly possible to get wrong: the signal claims a
  key the live state does NOT report held. That is a signal that has gone stale without being
  poisoned -- the user released the key and the release was not observed.

  The restore half presses on `held & ~unknown` alone, deliberately: the signal is the authority on
  what the USER holds, and live state cannot be used to second-guess it without reintroducing the
  ambiguity the signal exists to escape. So this DOES press, and that is correct -- but the
  verification pass is what catches it, which is why FR-103 and FR-103a are a pair. Asserted here so
  the behaviour is a decision on record rather than an accident.
*/
TEST_F(USER_HELD_SIGNAL, ASignalClaimNotBackedByLiveStateStillPressesAndIsCoveredByTheMask) {
  kbd[VK_LCONTROL]           = 0;
  userHeld.held[VK_LCONTROL] = 0x80; // the signal's last observation was a KEYDOWN
  // live says up: the user let go and the release was not observed
  AddOutputKey('A');

  RunBatchWithSignal();

  EXPECT_EQ(Count(VK_CONTROL, false), 1)
      << "the signal is the authority on what the user holds; second-guessing it with live state "
      << "reintroduces the ambiguity it exists to escape";

  DWORD bit = 0;
  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    if (KeymanModifierVks[i] == VK_LCONTROL) {
      bit = 1u << i;
    }
  }
  EXPECT_EQ(restorePressedMask & bit, bit)
      << "so this press MUST be in the mask -- it is exactly the press the verification pass exists "
      << "to correct, and a press the mask omits can never be corrected";
}

/*
  #8064 FR-103 + FR-103a, ASSERTED TOGETHER. Testing them apart proves nothing: FR-103 widens the
  mask so the pass can see FR-101's presses, and FR-103a stops the pass from immediately undoing
  them. Either one alone is worse than neither.

  The state below is the one every FR-101 restore creates inside the A0 window: the cache does not
  claim the key (it never saw the KEYDOWN), and live reports it held (because the restore press just
  landed). That is cache-up-and-live-down inverted -- exactly what the pass corrects on -- so without
  FR-103a the pass releases the modifier one message after the batch pressed it, and US0 delivers
  nothing at a cost of two injected events per batch.
*/
class VERIFICATION_WITH_SIGNAL : public PREPARE_MODIFIER_VERIFICATION_CORRECTION {
public:
  void
  SetUp() {
    PREPARE_MODIFIER_VERIFICATION_CORRECTION::SetUp();
    memset(&userHeld, 0, sizeof(userHeld));
  }

protected:
  UserHeldModifierSignal userHeld;

  void
  RunCorrectionWithSignal(DWORD restorePressedMask) {
    n = PrepareModifierVerificationCorrection(inputs, kbd, restorePressedMask, StubGetAsyncKeyState, &userHeld);
  }
};

TEST_F(VERIFICATION_WITH_SIGNAL, ThePassDoesNotUndoAPressItMadeOnTheSignalsWord) {
  kbd[VK_LCONTROL]                 = 0;    // the cache never saw the KEYDOWN -- the A0 window
  g_liveModifierState[VK_LCONTROL] = 0x80; // the restore press landed, so the OS holds it
  userHeld.held[VK_LCONTROL]       = 0x80; // and the signal says the user is still holding it
  userHeld.unknown[VK_LCONTROL]    = 0;

  RunCorrectionWithSignal(MaskBit(VK_LCONTROL));

  EXPECT_EQ(n, 0)
      << "the pass corrected a press the batch had just made on the signal's word. This is the "
      << "no-op FR-103a exists to prevent: widen the mask without making the pass signal-aware and "
      << "US0 delivers nothing while still costing two injected events every batch";
  EXPECT_EQ(Count(VK_CONTROL, true), 0) << "and no release may be emitted at all";
}

/*
  FR-103a's other side, which is what stops it being implemented as "never correct anything". A
  modifier the OS holds that NEITHER the cache NOR the signal claims is a genuine correction and must
  still be made -- this is the pass's original job, and the signal must not disable it.
*/
TEST_F(VERIFICATION_WITH_SIGNAL, ThePassStillCorrectsWhatNeitherTheCacheNorTheSignalClaims) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  // the signal claims nothing

  RunCorrectionWithSignal(MaskBit(VK_LCONTROL));

  EXPECT_EQ(Count(VK_CONTROL, true), 1)
      << "nobody claims this hold, so the OS is holding a modifier no user is. Declining to correct "
      << "it would leave the latch FR-103's pass was written to clear";
}

/*
  FR-103a with a POISONED claim. The signal says held, but it also says it cannot currently speak for
  the key -- so its claim carries no weight and the correction proceeds. Same effective read,
  `held & ~unknown`, as the restore half: if these two halves ever disagree about the same key, one
  presses and the other releases, every batch, forever.
*/
TEST_F(VERIFICATION_WITH_SIGNAL, APoisonedClaimDoesNotBlockACorrection) {
  kbd[VK_LCONTROL]                 = 0;
  g_liveModifierState[VK_LCONTROL] = 0x80;
  userHeld.held[VK_LCONTROL]       = 0x80;
  userHeld.unknown[VK_LCONTROL]    = 0x80; // stale, and it says so

  RunCorrectionWithSignal(MaskBit(VK_LCONTROL));

  EXPECT_EQ(Count(VK_CONTROL, true), 1)
      << "a poisoned claim is worth nothing, on both sides of the batch. The restore half would not "
      << "have pressed on this either, so the pass must not treat it as a press worth protecting";
}

namespace {
// RAWKEYBOARD::Flags values. Defined here rather than pulled from a header so the test states the
// wire format it is asserting about.
const USHORT kRiKeyMake  = 0x0000; // RI_KEY_MAKE
const USHORT kRiKeyBreak = 0x0001; // RI_KEY_BREAK
const USHORT kRiKeyE0    = 0x0002; // RI_KEY_E0
} // namespace

/*
  #8064 W5 / FR-100a, FR-104a -- UpdateUserHeldFromRawKeyboard.

  The two things this function has to get right, and both of them are the kind that fail silently:

  1. THE DISCRIMINATOR IS "NOT KEYMAN'S OWN", NEVER "NOT INJECTED". Genuine user input arrives
     OS-injected from Remote Desktop and from the Keyman OSK. A filter on injection -- or on
     hDevice -- drops those, so a hold made over RDP is one the restore half silently drops. The
     function does not even take hDevice, so the refuted policy is not expressible.

  2. POISON CLEARS ONLY ON A FRESH OBSERVATION OF THAT KEY, AND NEVER ON A TIMER. This is the UAC
     case: hold Ctrl, walk onto the secure desktop, release it there where nothing can see it, come
     back. Both FR-104 triggers have fired, so without per-key poisoning the signal reports a stale
     "held", FR-101 restores it, and FR-103a agrees not to correct it -- an unmatched press that
     nothing downstream can detect, because the cache and the OS then agree.
*/
class USER_HELD_FROM_RAW : public KEYBD_SHIFT {
public:
  void
  SetUp() {
    KEYBD_SHIFT::SetUp();
    memset(&signal, 0, sizeof(signal));
  }

protected:
  UserHeldModifierSignal signal;

  // The effective read, spelled out once: held & ~unknown. No consumer may read held alone.
  bool
  EffectivelyHeld(BYTE vk) const {
    return (signal.held[vk] & 0x80) != 0 && (signal.unknown[vk] & 0x80) == 0;
  }
};

TEST_F(USER_HELD_FROM_RAW, DerivesChiralityFromTheExtendedFlagAndTheMakeCode) {
  // Control and Alt: RI_KEY_E0 is the only chirality signal.
  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyMake, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_LCONTROL)) << "no E0 means left";
  EXPECT_FALSE(EffectivelyHeld(VK_RCONTROL));

  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyE0, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_RCONTROL)) << "E0 means right";

  UpdateUserHeldFromRawKeyboard(&signal, VK_MENU, 0x38, kRiKeyE0, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_RMENU));

  // Shift: the make code alone, exactly as UpdateModifierCacheFromKeyEvent does it. A disagreement
  // between the two would be silent, because both write a 256-byte array keyed by the chiral VK.
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, SCANCODE_RSHIFT, kRiKeyMake, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_RSHIFT)) << "the right-shift make code means right, with no E0";
  EXPECT_FALSE(EffectivelyHeld(VK_LSHIFT));

  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyMake, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_LSHIFT));
}

TEST_F(USER_HELD_FROM_RAW, ABreakFlagIsAReleaseAndNothingOutsideTheManagedSixIsWritten) {
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyMake, 0);
  ASSERT_TRUE(EffectivelyHeld(VK_LSHIFT));

  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyBreak, 0);
  EXPECT_FALSE(EffectivelyHeld(VK_LSHIFT)) << "RI_KEY_BREAK is a KEYUP";

  UpdateUserHeldFromRawKeyboard(&signal, 'A', 0x1E, kRiKeyMake, 0);
  UpdateUserHeldFromRawKeyboard(&signal, VK_CAPITAL, 0x3A, kRiKeyMake, 0);
  for (int i = 0; i < 256; i++) {
    bool managed = false;
    for (int k = 0; k < (int)_countof(KeymanModifierVks); k++) {
      if (KeymanModifierVks[k] == i) {
        managed = true;
      }
    }
    if (!managed) {
      EXPECT_EQ(signal.held[i], (BYTE)0) << "slot " << i << " is outside the managed six and was written";
    }
  }
}

TEST_F(USER_HELD_FROM_RAW, KeymansOwnEventsAreIgnoredButRdpAndOskInputIsNot) {
  // Keyman's own wrap event: ignored, because it says nothing about what the user is holding.
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyMake, EXTRAINFO_FLAG_KEYMAN_MODIFIER_WRAP);
  EXPECT_FALSE(EffectivelyHeld(VK_LSHIFT))
      << "Keyman's own injected modifier reached the user-held signal. The signal would then be "
      << "recording Keyman's own presses as evidence that the user is holding them, which is the "
      << "circularity the whole design exists to avoid";

  // Keyman's scan-flag form: likewise ignored.
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, SCAN_FLAG_KEYMAN_KEY_EVENT, kRiKeyMake, 0);
  EXPECT_FALSE(EffectivelyHeld(VK_LSHIFT));

  // Remote Desktop: OS-INJECTED, and genuinely the user. FR-100a.
  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyMake, 0x4321DCBA);
  EXPECT_TRUE(EffectivelyHeld(VK_LCONTROL))
      << "a hold made over Remote Desktop was dropped. RDP input is OS-injected and it is the user; "
      << "an injection-based or hDevice-based filter is refuted for exactly this reason";

  // The OSK: also OS-injected, also the user.
  UpdateUserHeldFromRawKeyboard(&signal, VK_MENU, 0x38, kRiKeyMake, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_LMENU));
}

/*
  FR-104a, THE UAC CASE, EXPLICITLY. The whole point is what does NOT happen: no elapsed time, no
  number of unrelated events, and no observation of a DIFFERENT key clears this key's poison. Only
  an event for this key does.
*/
TEST_F(USER_HELD_FROM_RAW, PoisonPersistsUntilAFreshObservationOfThatVeryKey) {
  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyMake, 0);
  ASSERT_TRUE(EffectivelyHeld(VK_LCONTROL)) << "the user pressed Left Control and we saw it";

  // The secure desktop, or a session change: we can no longer speak for this key.
  BYTE toPoison[1] = { VK_LCONTROL };
  PoisonUserHeldKeys(&signal, toPoison, 1);

  EXPECT_EQ(signal.held[VK_LCONTROL], (BYTE)0x80) << "poison suppresses the claim, it does not erase it";
  EXPECT_FALSE(EffectivelyHeld(VK_LCONTROL))
      << "the user may have released Left Control on the secure desktop, where nothing could see it. "
      << "Reporting the stale 'held' here is what manufactures an unmatched press, and cache and OS "
      << "would then AGREE -- the one state ReconcileModifierCache can never detect";

  // Events for OTHER keys do not rehabilitate this one.
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyMake, 0);
  UpdateUserHeldFromRawKeyboard(&signal, VK_MENU, 0x38, kRiKeyE0, 0);
  EXPECT_FALSE(EffectivelyHeld(VK_LCONTROL)) << "poison is PER KEY; another key being observed proves nothing";
  EXPECT_TRUE(EffectivelyHeld(VK_LSHIFT)) << "and those other keys are fine";

  // Only an event for this key does, and either direction counts as an observation.
  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyBreak, 0);
  EXPECT_EQ(signal.unknown[VK_LCONTROL], (BYTE)0) << "a fresh observation clears the poison";
  EXPECT_FALSE(EffectivelyHeld(VK_LCONTROL)) << "and it was a release, so the key is up";

  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyMake, 0);
  EXPECT_TRUE(EffectivelyHeld(VK_LCONTROL)) << "and it is back in service";
}

/*
  FR-104b. A displaced raw-input registration means the feed has been silently redirected, with no
  error surfaced anywhere. The signal has no standing to speak about ANY key, so every one is
  poisoned -- not the ones we happen to think are held, all of them.
*/
TEST_F(USER_HELD_FROM_RAW, ADisplacedRegistrationPoisonsEveryKey) {
  UpdateUserHeldFromRawKeyboard(&signal, VK_CONTROL, 0x1D, kRiKeyMake, 0);
  UpdateUserHeldFromRawKeyboard(&signal, VK_SHIFT, 0x2A, kRiKeyMake, 0);
  ASSERT_TRUE(EffectivelyHeld(VK_LCONTROL));
  ASSERT_TRUE(EffectivelyHeld(VK_LSHIFT));

  PoisonAllUserHeldKeys(&signal);

  for (int i = 0; i < (int)_countof(KeymanModifierVks); i++) {
    const BYTE vk = KeymanModifierVks[i];
    EXPECT_EQ(signal.unknown[vk], (BYTE)0x80)
        << "managed slot " << (int)vk << " was left speakable-for after the feed was displaced. "
        << "Every key, not only the ones currently claimed: a redirected feed can no longer observe "
        << "a press either, so a key that is up now may be held before anything notices";
    EXPECT_FALSE(EffectivelyHeld(vk));
  }
}
