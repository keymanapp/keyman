(*
  Name:             UfrmOSKOnScreenKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    8 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Support exporting and printing
                    04 Dec 2006 - mcdurdin - Fix shift states when pressing keys
                    12 Dec 2006 - mcdurdin - Localize strings according to product
                    04 Jan 2007 - mcdurdin - Fix shift state problems with L/R shift
                    15 Jan 2007 - mcdurdin - Fix L/R Ctrl/Alt deactivation
                    15 Jan 2007 - mcdurdin - Fix crash when viewing a keyboard that has no assignment for K_oE2
                    30 May 2007 - mcdurdin - I765 - Reset shift, ctrl, alt after clicking
                    30 May 2007 - mcdurdin - I765 - Caps, Bksp, Enter, Tab now usable on OSK
                    05 Jun 2007 - mcdurdin - I763? - Fix 102 key not displaying on UK keyboard
                    27 Mar 2008 - mcdurdin - Add HasVisualKeyboard(KeymanID) function
                    16 Jan 2009 - mcdurdin - I1144 - Reset shift state when OSK is closed or goes to another tab
                    12 Mar 2010 - mcdurdin - I2177 - Fix hotkeys leaving shift state incorrect when usage visible
                    29 Mar 2010 - mcdurdin - I2262 - Resizing OSK causes a lot of flicker
                    06 Apr 2010 - mcdurdin - I2284 - Change look and feel of OSK for Keyman 8
                    06 Apr 2010 - mcdurdin - I2262 - Reduce flicker with resize
                    06 Apr 2010 - mcdurdin - I2200 - Fix conflict between fixed OSK and mnemonic layouts
                    09 Apr 2010 - mcdurdin - I2295 - OSK can crash with mnemonic layouts and some shift states
                    11 Jan 2011 - mcdurdin - I764 - Fix Euro Layout issues
                    11 Jan 2011 - mcdurdin - I2603 - Simulated AltGr does not put OSK into AltGr state
                    25 Jan 2011 - mcdurdin - I2329 - Printing not working on x64 machines
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    28 Feb 2014 - mcdurdin - I4098 - V9.0 - OSK is still 8.0 style
                    03 Aug 2014 - mcdurdin - I4359 - V9.0 - OSK shows wrong base keyboard and doesn't refresh
                    04 Nov 2014 - mcdurdin - I4487 - Crash when saving OSK to file, changing keyboard midstream [CrashID:keyman.exe_9.0.473.0_2C59B75E_EAccessViolation]
                    08 Apr 2015 - mcdurdin - I4650 - V9.0 - On Screen keyboard translates keys wrongly for European keyboards
*)
unit UfrmOSKOnScreenKeyboard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmOSKPlugInBase, OnScreenKeyboard, ExtShiftState, UserMessages,
  VisualKeyboard, VisualKeyboardInfo, Menus, ExtCtrls;

type
  TfrmOSKOnScreenKeyboard = class(TfrmOSKPlugInBase)
    kbd: TOnScreenKeyboard;
    tmrCheck: TTimer;
    dlgSave: TSaveDialog;
    procedure kbdKeyPressed(Sender: TOnScreenKeyboard;
      Key: TOnScreenKeyboardKey);
    procedure kbdShiftChange(Sender: TObject);
    procedure tmrCheckTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdFadeWhenInactiveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    OldHandle: Integer;
    FShiftState: TExtShiftState;
    FSystemLayout: DWord;
    FPositional: Boolean;
    FUnicode: Boolean;
    VKI: TVisualKeyboardInfo;
    VisualKeyboards: TVisualKeyboardInfoList;
    FCachedShiftState: TExtShiftState;
    // #8064: what a source OTHER THAN THIS UNIT is currently holding down. FCachedShiftState says
    // the OSK pressed a modifier; it does NOT say the OSK is the only one holding it, and both
    // readers need that difference: ResetShiftStates, to decline a release that would cancel the
    // user's own hold, and kbdKeyPressed's FinalState, to know a suppressed hold is still owed its
    // restoring KEYDOWN. Read through UserHoldsModifier, never directly, for the chirality rules.
    //
    // Two writers. UpdateUserHeldModifiers maintains it from the low level hook feed and is the
    // only one that RETIRES an entry. kbdKeyPressed's NoteSuppressedUserHold also seeds it, from
    // the physical snapshot taken before PrepState suppresses a hold -- because that suppression
    // destroys the evidence a poll would need, and because the feed cannot know about a hold that
    // predates this window.
    FUserHeldShiftState: TExtShiftState;
    IsSimulatedLControlDown: Boolean;
    function GetAsyncShiftState: TExtShiftState;
    procedure UpdateUserHeldModifiers(VKCode, ScanCode, Flags: DWORD; IsUp: Boolean);
    function UserHoldsModifier(shift: TExtShiftStateValue): Boolean;
    procedure UpdateKeyboard(FLoading: Boolean);

    function SaveWebPage(vk: TVisualKeyboard; const s: string): Boolean;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure ShiftStateChange(kbdShift, asyncShift: TExtShiftState);
    procedure UpdateShiftStates;
  public
    { Public declarations }
    procedure RefreshKeyboards;
    procedure SelectKeyboard(KeymanID: Integer; Unicode: Boolean);   // I4359
    procedure ConstrainSizing(SizeDir: Integer; var Rect: TRect); override;

    procedure SaveAsWebPage;
    procedure PrintKeyboard;

    procedure ResetShiftStates; // I1144
    procedure OskModifierEvent(VKCode, Flags: DWORD);

    function HasVisualKeyboard: Boolean; overload;
    function HasVisualKeyboard(KeymanID: Integer): Boolean; overload;
  end;

implementation

uses
  KLog,
  kmint,
  MessageIdentifierConsts,
  messageidentifiers,
  Types,
  UfrmKeyman7Main,
  UfrmVisualKeyboard,
  utildir,
  utilexecute,
  VisualKeyboardExportHTML,
  VisualKeyboardExportXML,
  VisualKeyboardParameters, keymanapi_TLB;

{$R *.dfm}

(**
 * #8064: the scan codes that identify an injector at the low level hook. Hoisted to unit level
 * because UpdateUserHeldModifiers needs them as well as OskModifierEvent.
 *)
const
  /// The normal scan code for left Control key
  SCAN_LEFT_CONTROL = $1D;
  /// The scan code generated by Windows when it simulates pressing left Control, when AltGr is
  /// pressed on a European layout; this is generated by Windows for compatibility reasons.
  SCAN_LEFT_CONTROL_SIMULATED = $21D;
  /// KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED (keyman32/k32_visualkeyboardinterface.h): set by the
  /// low level hook when IsKeymanInjectedKeyEvent says the event is Keyman's own.
  ///
  /// This replaces an earlier scan-code test here, which was WRONG for exactly one key and in the
  /// unrecoverable direction. do_keybd_event (keybd_shift.cpp) overwrites SCAN_FLAG_KEYMAN_KEY_EVENT
  /// with SCANCODE_RSHIFT for VK_RSHIFT, so an injected Right Shift reaches this feed as
  /// vk = VK_SHIFT, scan = $36 -- byte-identical to a physical one. A scan test therefore read
  /// Keyman's own Right Shift as the user's, set essShift user-held, and made ResetShiftStates
  /// decline to release the OSK's clicked Shift: a STUCK modifier, which is the one outcome this
  /// whole design exists to avoid. manual-tests/GH-16462 - osk-sticky-modifier/README.md names Right Shift as this exception, and
  /// ShouldFeedModifierCache needs its second dwExtraInfo arm for the same reason.
  ///
  /// dwExtraInfo, which is what closes that gap, is not carried in this message, so the decision is
  /// made in the hook where both inputs are in hand and only the answer is sent.
  KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED = $00000010;

type
  /// #8064: the modifier families this unit injects into.
  TOskModifierFamily = (omfShift, omfCtrl, omfAlt);

var
  (**
   * #8064: injections this unit has made and not yet seen come back through the low level hook
   * feed, counted per family and per direction.
   *
   * WHY A LEDGER RATHER THAN A SCAN CODE TEST. Every event this unit injects also arrives at
   * OskModifierEvent, and the user-held signal must not count the OSK's own press as the user's.
   * Scan code alone cannot do that job: manual-tests/GH-16462 - osk-sticky-modifier/README.md records the measurement that `scan = 0` reliably
   * means a `bScan = 0` injector, but that the converse does NOT hold -- Windows propagated
   * `bScan = 0` untouched for some events in that capture and substituted a real scan code for
   * others in the same run. Reading a real scan code as "the user" would therefore let the OSK's
   * own echo mark the key user-held, and the release would be declined for a key nobody but the
   * OSK holds -- trading this bug for the stuck modifier it exists to prevent.
   *
   * Keyed by FAMILY, not by exact VK, because this unit injects generic VK_SHIFT / VK_CONTROL /
   * VK_MENU while the hook is free to report a chiral identity back (PostVisualKeyboardModifierEvent
   * documents chiral Ctrl/Alt), so an exact-VK ledger would fail to recognise its own echo. Family
   * resolution is all it needs: the ledger only ever answers "was this event ours".
   *
   * Degrades safely. A lost echo -- the hook feed only reaches here while the visual keyboard
   * window exists -- leaves a count standing, and the next genuine user event of that family is
   * eaten as the echo. That misses one user hold and falls back to the pre-#8064 behaviour; it
   * never invents one.
   *
   * Unit level and unguarded because do_keybd_event is a unit-level routine and the OSK is a
   * single-instance form on the one UI thread, which is the same reason do_keybd_event itself is
   * not a method.
   *)
  FOskPendingEcho: array[TOskModifierFamily, Boolean] of Integer;

/// #8064: maps a modifier VK, chiral or generic, onto the family the echo ledger counts. False for
/// anything that is not a modifier -- the character keys do_keybd_event also emits.
function OskModifierFamilyOf(vk: DWORD; var family: TOskModifierFamily): Boolean;
begin
  Result := True;
  case vk of
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT: family := omfShift;
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL: family := omfCtrl;
    VK_MENU, VK_LMENU, VK_RMENU: family := omfAlt;
  else
    Result := False;
  end;
end;

/// #8064: maps a modifier VK onto the TExtShiftState value naming that exact key. Note that
/// TExtShiftState has no chiral Shift values, so both Shift keys map to essShift.
function OskModifierShiftValueOf(vk: DWORD; var shift: TExtShiftStateValue): Boolean;
begin
  Result := True;
  case vk of
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT: shift := essShift;
    VK_CONTROL:  shift := essCtrl;
    VK_LCONTROL: shift := essLCtrl;
    VK_RCONTROL: shift := essRCtrl;
    VK_MENU:  shift := essAlt;
    VK_LMENU: shift := essLAlt;
    VK_RMENU: shift := essRAlt;
  else
    Result := False;
  end;
end;

/// #8064: is this hook event the echo of an injection this unit made? Consumes the ledger entry if
/// so, so each injection accounts for exactly one observation.
function ConsumeOskModifierEcho(vk: DWORD; isUp: Boolean): Boolean;
var
  family: TOskModifierFamily;
begin
  Result := False;
  if not OskModifierFamilyOf(vk, family) then
    Exit;
  if FOskPendingEcho[family, isUp] > 0 then
  begin
    Dec(FOskPendingEcho[family, isUp]);
    Result := True;
  end;
end;

procedure do_keybd_event(bVk: Byte; bScan: Byte; dwFlags, dwExtraInfo: DWORD);
var
  family: TOskModifierFamily;
begin
  KL.Log('kbdKeyPressed: keybd_event vk=%x scan=%x flags=%x', [bVk, bScan, dwFlags]);

  // #8064: recorded BEFORE the injection, so the echo can never arrive ahead of its own ledger
  // entry. Every modifier this unit injects goes through here, which is what makes this the one
  // place the ledger has to be written.
  if OskModifierFamilyOf(bVk, family) then
    Inc(FOskPendingEcho[family, (dwFlags and KEYEVENTF_KEYUP) <> 0]);

  keybd_event(bVk, bScan, dwFlags, dwExtraInfo);
end;

procedure TfrmOSKOnScreenKeyboard.kbdKeyPressed(Sender: TOnScreenKeyboard; Key: TOnScreenKeyboardKey);
var
  vk, scan: Integer;
  fkcss, ass: TExtShiftState;
  // #8064: frozen with fkcss/ass below. Re-reading kbd.LRShift for FinalState risks a keyboard
  // switch landing in between, selecting essCtrl/essAlt against snapshots that encode the chiral
  // values -- no branch then matches, and a PrepState suppression goes unrestored.
  LLRShift: Boolean;


  (**
   * #8064: PrepState is about to suppress a modifier that is down and that the OSK is not
   * claiming, so something other than this unit is holding it -- FCachedShiftState is masked by
   * kbd.ShiftState, so a modifier absent from `fkcss` cannot be an outstanding OSK press.
   *
   * Recorded BEFORE the suppressing KEYUP, because that KEYUP is what destroys the evidence: from
   * then on the live key state reads up whether or not the user is still holding the key, and no
   * poll can recover the difference. FinalState needs exactly that difference.
   *
   * Into FUserHeldShiftState, the field the hook feed maintains, because it is the same fact --
   * "a source other than this unit holds this key". Seeding it here is what covers a hold that
   * predates this window, which the feed never saw; and once seeded, the user's own physical KEYUP
   * retires it through the feed like any other entry.
   *)
  procedure NoteSuppressedUserHold(shift: TExtShiftStateValue);
  begin
    // The one down state nobody holds: Windows' compatibility LCtrl for AltGr. Its release reaches
    // the feed with SCAN_LEFT_CONTROL_SIMULATED, which UpdateUserHeldModifiers discards, so a seed
    // made from it would never be retired and would go on declining teardown releases for the rest
    // of the session. GetAsyncShiftState subtracts it in the chiral regime already; this covers the
    // generic one, where essCtrl is read straight from VK_CONTROL.
    if IsSimulatedLControlDown and (shift in [essCtrl, essLCtrl]) then
      Exit;

    Include(FUserHeldShiftState, shift);
  end;

  procedure PrepState(fkcss, ass: TExtShiftState; shift: TExtShiftStateValue; vk: Integer);
  var
    FExtended: Dword;
  begin
    if vk in [VK_RCONTROL, VK_RMENU] then FExtended := KEYEVENTF_EXTENDEDKEY else FExtended := 0;
    if (shift in fkcss) and not (shift in ass) then do_keybd_event(vk, 0, FExtended, 0)
    else if not (shift in fkcss) and (shift in ass) then
    begin
      NoteSuppressedUserHold(shift);
      do_keybd_event(vk, 0, FExtended or KEYEVENTF_KEYUP, 0);
    end;
  end;

  procedure FinalState(fkcss, ass: TExtShiftState; shift: TExtShiftStateValue; vk: Integer);
  var
    FExtended: Dword;
  begin
    if vk in [VK_RCONTROL, VK_RMENU] then FExtended := KEYEVENTF_EXTENDEDKEY else FExtended := 0;
    if (shift in fkcss) and not (shift in ass) then do_keybd_event(vk, 0, FExtended or KEYEVENTF_KEYUP, 0)
    else if not (shift in fkcss) and (shift in ass) then
    begin
      // #8064: `ass` is stale by now -- the character keys and the COM property get intervene --
      // so restoring blind can inject a KEYDOWN the user has already released: a down state with
      // no holder, which is the stuck modifier this whole area exists to avoid. Undoing
      // PrepState's own press (the branch above) needs no such check.
      //
      // The test is the user-held signal, NOT GetAsyncKeyState. A poll cannot answer this question
      // at all any more: PrepState's suppressing KEYUP has already made the live state read up, so
      // a live test reads "up" for the user's still-held key, declines every restoration, and
      // leaves the modifier dead in the user's hand -- the failure this branch exists to prevent,
      // arriving from the other direction. The signal has no such blind spot: this unit's own
      // injections are consumed as echoes by ConsumeOskModifierEcho and never clear a hold, while
      // a genuine KEYUP from the user does, so it still separates the two cases after the
      // suppression that a poll no longer can.
      if UserHoldsModifier(shift) then
        do_keybd_event(vk, 0, FExtended, 0)
      else
        KL.Log('kbdKeyPressed: not restoring %s -- the user has let go of it',
          [ExtShiftStateToString([shift])]);
    end;
  end;
begin
  KL.Log('kbdKeyPressed - Value: %s Key: %s USVKey=%x VKey=%x FPositional:%s', [Key.KeyValue, Key.ActiveKeyCap, Key.USVKey, Key.VKey, BoolToStr(FPositional, True)]);


  if not Assigned(VKI)
    then vk := Key.VKey
    else vk := Key.USVKey;   // I4650
  scan := Key.ScanCode;

  fkcss := kbd.ShiftState;
  ass := GetAsyncShiftState;
  LLRShift := kbd.LRShift;

  PrepState(fkcss, ass, essShift, VK_SHIFT);
  if LLRShift then
  begin
    PrepState(fkcss, ass, essLCtrl, VK_LCONTROL);
    PrepState(fkcss, ass, essLAlt, VK_LMENU);
    PrepState(fkcss, ass, essRCtrl, VK_RCONTROL);
    PrepState(fkcss, ass, essRAlt, VK_RMENU);
  end
  else
  begin
    PrepState(fkcss, ass, essCtrl, VK_CONTROL);
    PrepState(fkcss, ass, essAlt, VK_MENU);
  end;

  do_keybd_event(vk, scan, 0, 0);
  do_keybd_event(vk, scan, KEYEVENTF_KEYUP, 0);

  if kmcom.Options['koReleaseShiftKeysAfterKeyPress'].Value then
  begin
    kbd.ShiftState := [];
    kbdShiftChange(nil);
  end
  else
  begin
    FinalState(fkcss, ass, essShift, VK_SHIFT);
    if LLRShift then
    begin
      FinalState(fkcss, ass, essLCtrl, VK_LCONTROL);
      FinalState(fkcss, ass, essLAlt, VK_LMENU);
      FinalState(fkcss, ass, essRCtrl, VK_RCONTROL);
      FinalState(fkcss, ass, essRAlt, VK_RMENU);
    end
    else
    begin
      FinalState(fkcss, ass, essCtrl, VK_CONTROL);
      FinalState(fkcss, ass, essAlt, VK_MENU);
    end;
  end;
end;

procedure TfrmOSKOnScreenKeyboard.kbdShiftChange(Sender: TObject);
var
  ass, fkcss, FMask: TExtShiftState;
begin
  fkcss := kbd.ShiftState;
  ass := GetAsyncShiftState;

  // #8064: FCachedShiftState records only what the OSK has clicked outstanding, by exact chiral
  // identity, and is what ResetShiftStates releases from. It must never come to name what the user
  // is physically holding, or teardown releases the user's own key (I2177): `- ass` excludes that,
  // and accumulating covers earlier clicks that by now read as down. The snapshot above is taken
  // before ShiftStateChange injects anything, so the modifier just clicked is not physically down
  // yet and survives the subtraction. See manual-tests/GH-16462 - osk-sticky-modifier/README.md.
  //
  // ShiftStateChange first, deliberately, for two reasons: its release branch reads the pre-mask
  // cache for the chiral identity to release, and after a SetLRShift collapse the mask below would
  // already have stripped it (measured: the click-off released VK_CONTROL and left VK_RCONTROL
  // held); and it removes from the cache whatever it released, so running it first keeps that
  // removal and the accumulate below from fighting over the same field.
  ShiftStateChange(fkcss, ass);

  // Widen across the family before masking: the cache may name a modifier in a representation
  // SetLRShift has since collapsed, and a bare `* fkcss` would drop a still-held essRCtrl merely
  // because an unrelated modifier was clicked. Retains only, never adds, so I2177 stays fixed --
  // the additive term already excludes anything physically held.
  //
  // Conditional, and the guard is load-bearing: when fkcss carries nothing from that family the
  // family really is off, and the cache entry should go rather than be retained.
  FMask := fkcss;
  if fkcss * [essCtrl, essLCtrl, essRCtrl] <> [] then
    FMask := FMask + [essCtrl, essLCtrl, essRCtrl];
  if fkcss * [essAlt, essLAlt, essRAlt] <> [] then
    FMask := FMask + [essAlt, essLAlt, essRAlt];

  FCachedShiftState := (FCachedShiftState + (fkcss - ass)) * FMask;
end;

(**
 * Handles OSK modifier events received from
 * keyman32:PostVisualKeyboardModifierEvent in k32_visualkeyboardinterface.cpp.
 *
 * This expects the chiral VK_LCONTROL / VK_RCONTROL / VK_LMENU / VK_RMENU /
 * VK_LSHIFT / VK_RSHIFT virtual key codes (unlike most Windows contexts).
 *
 * This currently deals with two situations:
 *
 * 1. When Windows posts a simulated LControl key when AltGr is pressed on a
 *    European layout. See serialkeyeventserver.cpp:WndProc for a deeper
 *    discussion of the key events that are generated in this scenario.
 * 2. When the physical RShift key is released, the OSK needs to release the
 *    'clicked' LShift for consistency (#12611).
 *
 * @param VKCode virtual key code of the modifier key, chiral for Alt and Ctrl
 * @param Flags  as follows:
 *    KEYEVENTF_EXTENDEDKEY   0x00000001  - extended bit is set on the scan code
 *    KEYEVENTF_KEYUP         0x00000002  - key is being released
 *    scan code mask          0x0FFF0000  - 12 bits for scan code
 *    all other bits reserved
 *)
procedure TfrmOSKOnScreenKeyboard.OskModifierEvent(VKCode, Flags: DWORD);
var
  isUp: Boolean;
  scanCode: DWORD;
begin
  // not used: extended := (Flags and KEYEVENTF_EXTENDEDKEY) <> 0;
  isUp := (Flags and KEYEVENTF_KEYUP) <> 0;

  // In order to identify the Windows simulated left control scan code, we need
  // to match on more bits than the normal 8 bit scan code.
  scanCode := (Flags and $0FFF0000) shr 16;

  // #8064: unconditionally, and before the branches below, which each return early. This feed is
  // the only place the OSK can learn that a modifier it is already holding has ALSO been pressed
  // by the user -- polling cannot see it, because the key is already down.
  UpdateUserHeldModifiers(VKCode, scanCode, Flags, isUp);

  // If the Left Control key is simulated by Windows, then we want to ignore it
  // because in reality, AltGr has been pressed on a European keyboard. However,
  // if the Left Control key is being released, then we should reset the flag
  // whether it comes from the Windows simulation or from another source (real
  // or injected), so that we don't end up with a stuck left control key
  if (scanCode = SCAN_LEFT_CONTROL_SIMULATED) and not isUp then
    IsSimulatedLControlDown := True
  else if ((scanCode = SCAN_LEFT_CONTROL) or (scanCode = SCAN_LEFT_CONTROL_SIMULATED)) and isUp then
    IsSimulatedLControlDown := False
  else if (VKCode = VK_RSHIFT) and isUp then
  begin
    if (GetAsyncKeyState(VK_LSHIFT) and $8000) = $8000 then
    begin
      // #12611: The physical right shift key has just been released by the
      // user, but the user has previously clicked the left shift key in the
      // OSK.  We need to force a key up for left shift as well to clear, both
      // in the OSK and for apps that may differentiate L/R shift.
      do_keybd_event(VK_LSHIFT, 0, KEYEVENTF_KEYUP, 0);
    end;
  end;

  // TODO: in the future, we might be able to eliminate tmrCheck and make all
  // modifier updates go through the WH_KEYBOARD_LL hook.
end;

procedure TfrmOSKOnScreenKeyboard.ShiftStateChange(kbdShift, asyncShift: TExtShiftState);
  // #8064: the release branch below used to pick its VK from the CURRENT kbd.LRShift, so after a
  // SetLRShift collapse a click-off released generic VK_CONTROL/VK_MENU (Left) while the extended
  // right-hand key stayed held -- unclearable on hardware without that physical key. It now takes
  // the chiral identity from FCachedShiftState, as ResetShiftStates does.
  //
  // Reads and removals only, of FCachedShiftState. The reverted attempt *wrote* the cache here,
  // including from UpdateShiftStates' 50 ms resync, whose press branch fires for physically-held
  // modifiers, so teardown released the user's own keys (I2177). See
  // manual-tests/GH-16462 - osk-sticky-modifier/README.md, "The FCachedShiftState invariant". FUserHeldShiftState is a different field under a different rule
  // and the press branch below does retire entries from it; see UpdateUserHeldModifiers.
  procedure PrepState(fkcss, ass: TExtShiftState; shift: TExtShiftStateValue; vk: Integer);
  var
    FExtended, FReleaseExtended: Dword;
    FReleaseVk: Integer;
  begin
    if vk in [VK_RCONTROL, VK_RMENU] then FExtended := KEYEVENTF_EXTENDEDKEY else FExtended := 0;
    if (shift in fkcss) and not (shift in ass) then
    begin
      // #8064: the sticky press ResetShiftStates will later have to release. This branch's own
      // guard is proof that NOBODY holds this key right now -- it fired precisely because the live
      // read said up -- so it is the one authoritative point at which a stale user-held record can
      // be retired. Without it a missed KEYUP would suppress every future release of this key:
      // once the OSK presses it the live read is down, so ResetShiftStates' own self-heal can no
      // longer tell a stale record from a real hold.
      //
      // A generic entry carries family-wide evidence -- VK_CONTROL/VK_MENU reading up means both
      // hands are up -- so it retires the family. A chiral entry speaks only for its own key.
      if shift = essCtrl then
        FUserHeldShiftState := FUserHeldShiftState - [essCtrl, essLCtrl, essRCtrl]
      else if shift = essAlt then
        FUserHeldShiftState := FUserHeldShiftState - [essAlt, essLAlt, essRAlt]
      else
        Exclude(FUserHeldShiftState, shift);

      do_keybd_event(vk, 0, FExtended, 0);
    end
    else if not (shift in fkcss) and (shift in ass) then
    begin
      // Prefer the identity actually injected over the one the current regime implies. Falls back
      // to `vk` when the cache names nothing in this family -- i.e. when the OSK did not put the
      // key down, in which case the pre-existing behaviour is retained unchanged.
      FReleaseVk := vk;
      FReleaseExtended := FExtended;

      if shift in [essCtrl, essLCtrl, essRCtrl] then
      begin
        if essRCtrl in FCachedShiftState then
        begin
          FReleaseVk := VK_RCONTROL; FReleaseExtended := KEYEVENTF_EXTENDEDKEY;
        end
        else if essLCtrl in FCachedShiftState then
        begin
          FReleaseVk := VK_LCONTROL; FReleaseExtended := 0;
        end
        else if essCtrl in FCachedShiftState then
        begin
          FReleaseVk := VK_CONTROL; FReleaseExtended := 0;
        end;
      end
      else if shift in [essAlt, essLAlt, essRAlt] then
      begin
        if essRAlt in FCachedShiftState then
        begin
          FReleaseVk := VK_RMENU; FReleaseExtended := KEYEVENTF_EXTENDEDKEY;
        end
        else if essLAlt in FCachedShiftState then
        begin
          FReleaseVk := VK_LMENU; FReleaseExtended := 0;
        end
        else if essAlt in FCachedShiftState then
        begin
          FReleaseVk := VK_MENU; FReleaseExtended := 0;
        end;
      end;

      do_keybd_event(FReleaseVk, 0, FReleaseExtended or KEYEVENTF_KEYUP, 0);

      // Removal only. Drop the whole family, because the collapse this fix exists for means the
      // cache may name the released key in a different representation than `shift` arrived in.
      if shift in [essCtrl, essLCtrl, essRCtrl] then
        FCachedShiftState := FCachedShiftState - [essCtrl, essLCtrl, essRCtrl]
      else if shift in [essAlt, essLAlt, essRAlt] then
        FCachedShiftState := FCachedShiftState - [essAlt, essLAlt, essRAlt]
      else
        FCachedShiftState := FCachedShiftState - [shift];
    end;
  end;
begin
  KL.Log('ShiftStateChange: kbdShift=%s asyncShift=%s ', [ExtShiftStateToString(kbdShift), ExtShiftStateToString(asyncShift)]);

  PrepState(kbdShift, asyncShift, essShift, VK_SHIFT);
  if kbd.LRShift then
  begin
    PrepState(kbdShift, asyncShift, essLCtrl, VK_LCONTROL);
    PrepState(kbdShift, asyncShift, essLAlt, VK_LMENU);
    PrepState(kbdShift, asyncShift, essRCtrl, VK_RCONTROL);
    PrepState(kbdShift, asyncShift, essRAlt, VK_RMENU);
  end
  else
  begin
    PrepState(kbdShift, asyncShift, essCtrl, VK_CONTROL);
    PrepState(kbdShift, asyncShift, essAlt, VK_MENU);
  end;

  UpdateKeyboard(False);
end;

procedure TfrmOSKOnScreenKeyboard.PrintKeyboard;
var
  k: IKeymanKeyboardInstalled;
  n: Integer;
begin
  if not Assigned(VKI) then Exit;
  n := kmcom.Keyboards.IndexOf(VKI.KeymanName);
  if n = 0 then Exit;
  k := kmcom.Keyboards[n];
  //if Assigned(k.VisualKeyboard) then
  //(k.VisualKeyboard as IKeymanVisualKeyboard2).Print;  // I2329
  {$MESSAGE HINT 'Implement Print'}
end;

procedure TfrmOSKOnScreenKeyboard.RefreshKeyboards;
begin
  VKI := nil;
  VisualKeyboards.Load;
  UpdateKeyboard(True);
end;

procedure TfrmOSKOnScreenKeyboard.ResetShiftStates;
var
  FRemaining, FExpandedCache: TExtShiftState;

  // #8064: a keyboard switch runs SetLRShift, which collapses the chiral Ctrl/Alt entries in
  // kbd.ShiftState to generic ones, so neither it nor kbd.LRShift can still name the VK that is
  // down. FCachedShiftState survives the collapse, so release from it directly -- gated on live
  // state, so a key nobody holds is left alone and a second call is a no-op.
  //
  // The live check is NOT what keeps a physically-held modifier safe (I2177), and it never was:
  // once the OSK has pressed the key, the live read is down whether or not the user is also
  // holding it. FUserHeldShiftState is what carries that, via UserHoldsModifier.
  procedure ReleaseCached(shift: TExtShiftStateValue; vk: Integer; extended: DWord);
  begin
    if not (shift in FCachedShiftState) then
      Exit;

    if (GetAsyncKeyState(vk) and $8000) <> $8000 then
      Exit;

    if UserHoldsModifier(shift) then
    begin
      // Left latched on purpose. The user's own physical release clears the one shared down state
      // and takes this press with it; a KEYUP here would instead kill the hold in their hand.
      KL.Log('ResetShiftStates: not releasing %s -- the user is holding it too', [ExtShiftStateToString([shift])]);
      Exit;
    end;

    do_keybd_event(vk, 0, extended or KEYEVENTF_KEYUP, 0);
  end;

  // #8064: a hold the feed recorded but never saw released -- the feed only runs while the visual
  // keyboard window exists, so an event can be missed across a hide/show. A key that reads up live
  // is held by nobody, so the record is stale and saying so here is what stops a stale entry
  // suppressing releases for the rest of the session.
  procedure ForgetUserHoldIfKeyIsUp(shift: TExtShiftStateValue; vk: Integer);
  begin
    if (shift in FUserHeldShiftState) and ((GetAsyncKeyState(vk) and $8000) <> $8000) then
      Exclude(FUserHeldShiftState, shift);
  end;

begin
  ForgetUserHoldIfKeyIsUp(essShift, VK_SHIFT);
  ForgetUserHoldIfKeyIsUp(essCtrl,  VK_CONTROL);
  ForgetUserHoldIfKeyIsUp(essLCtrl, VK_LCONTROL);
  ForgetUserHoldIfKeyIsUp(essRCtrl, VK_RCONTROL);
  ForgetUserHoldIfKeyIsUp(essAlt,   VK_MENU);
  ForgetUserHoldIfKeyIsUp(essLAlt,  VK_LMENU);
  ForgetUserHoldIfKeyIsUp(essRAlt,  VK_RMENU);

  KL.Log('ResetShiftStates: FShiftState=%s Cache=%s UserHeld=%s kbd.ShiftState=%s', [ExtShiftStateToString(FShiftState), ExtShiftStateToString(FCachedShiftState), ExtShiftStateToString(FUserHeldShiftState), ExtShiftStateToString(kbd.ShiftState)]);

  ReleaseCached(essShift, VK_SHIFT,    0);
  ReleaseCached(essCtrl,  VK_CONTROL,  0);
  ReleaseCached(essLCtrl, VK_LCONTROL, 0);
  ReleaseCached(essRCtrl, VK_RCONTROL, KEYEVENTF_EXTENDEDKEY);
  ReleaseCached(essAlt,   VK_MENU,     0);
  ReleaseCached(essLAlt,  VK_LMENU,    0);
  ReleaseCached(essRAlt,  VK_RMENU,    KEYEVENTF_EXTENDEDKEY);

  // Makes a second call a no-op, and stops kbd.ShiftState claiming, for rendering, a modifier the
  // OSK is no longer holding. Widen to the family first: the cache may name essRCtrl where a
  // collapse has left kbd.ShiftState carrying essCtrl.
  FExpandedCache := FCachedShiftState;
  if FCachedShiftState * [essCtrl, essLCtrl, essRCtrl] <> [] then
    FExpandedCache := FExpandedCache + [essCtrl, essLCtrl, essRCtrl];
  if FCachedShiftState * [essAlt, essLAlt, essRAlt] <> [] then
    FExpandedCache := FExpandedCache + [essAlt, essLAlt, essRAlt];

  FRemaining := kbd.ShiftState - FExpandedCache;
  kbd.ShiftState    := FRemaining;
  FCachedShiftState := [];

  // #8064: FUserHeldShiftState is deliberately NOT cleared here. It records what the user is
  // physically holding, which teardown does not change, and the feed keeps it current for as long
  // as this form exists. Clearing the cache is enough to make a second call a no-op.

  UpdateKeyboard(False);   // I1144 // I2177
end;

procedure TfrmOSKOnScreenKeyboard.tmrCheckTimer(Sender: TObject);
begin
  if GetAsyncShiftState <> FShiftState then
  begin
    KL.Log('tmrCheckTimer: FShiftState=%s async=%s', [ExtShiftStateToString(FShiftState), ExtShiftStateToString(GetAsyncShiftState)]);
    UpdateShiftStates;
  end;
end;

procedure TfrmOSKOnScreenKeyboard.UpdateShiftStates;
var
  FNewShiftState: TExtShiftState;
  FChanged: Boolean;
begin
  FChanged := False;
  FNewShiftState := GetAsyncShiftState;

  if (FShiftState * [essLAlt, essRAlt] = [essLAlt, essRAlt]) and (FNewShiftState * [essLAlt, essRAlt] <> FShiftState * [essLAlt, essRAlt]) then
  begin
    FNewShiftState := FNewShiftState - [essLAlt, essRAlt];
    FChanged := True;
  end;

  if (FShiftState * [essLCtrl, essRCtrl] = [essLCtrl, essRCtrl]) and (FNewShiftState * [essLCtrl, essRCtrl] <> FShiftState * [essLCtrl, essRCtrl]) then
  begin
    FNewShiftState := FNewShiftState - [essLCtrl, essRCtrl];
    FChanged := True;
  end;

  if FChanged then
    ShiftStateChange(FNewShiftState, FShiftState);

  FShiftState := FNewShiftState;
  kbd.ShiftState := FShiftState;
  UpdateKeyboard(False);
end;

(**
 * #8064: maintains FUserHeldShiftState from the low level hook feed -- the modifiers a source
 * other than this unit is currently holding down.
 *
 * THE PROBLEM THIS EXISTS FOR. FCachedShiftState records that the OSK PRESSED a modifier. It does
 * not record that the OSK is the ONLY thing holding it, and those are different facts, because
 * Windows keeps one down state per key and not one per holder. ResetShiftStates used to treat
 * `shift in FCachedShiftState` plus a live `GetAsyncKeyState` down as proof that the down state
 * was the OSK's alone -- manual-tests/GH-16462 - osk-sticky-modifier/README.md says so in as many words. It is not proof. If the
 * user physically presses the same modifier after the sticky click, the live read is down for two
 * reasons and teardown's KEYUP cancels the user's hold along with the OSK's press, leaving the
 * modifier dead in the user's hand until they release and press it again.
 *
 * The asymmetry that decides the policy: when the user IS also holding the key, declining to
 * release costs nothing, because their own physical KEYUP clears the single shared down state and
 * takes the OSK's press with it. Releasing, by contrast, is unrecoverable without user action. So
 * an overlap suppresses the KEYUP and an ordinary teardown still emits it.
 *
 * "Other than this unit" is decided by ConsumeOskModifierEcho, an explicit ledger of injections,
 * NOT by reading the scan code -- see FOskPendingEcho for why the scan code cannot carry that.
 *
 * That the echoes are consumed here, and that an OSK KEYUP therefore never retires a hold, is also
 * what lets kbdKeyPressed's FinalState use this field: after PrepState's suppressing KEYUP a poll
 * reads "up" for a key the user is still holding, and this field does not.
 *
 * keyman32's own injections are excluded by KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED, decided in the
 * hook by IsKeymanInjectedKeyEvent and carried in the flags. This too was a scan-code test once, and
 * it was wrong for Right Shift in the unrecoverable direction -- see that constant.
 *
 * One scan code test remains, for the one event Windows itself manufactures and nobody holds:
 * SCAN_LEFT_CONTROL_SIMULATED, the AltGr compatibility left Control, which this unit already
 * discounts via IsSimulatedLControlDown and which GetAsyncShiftState already subtracts.
 *
 * Everything else counts as the user, deliberately including OS-injected input from RDP, the
 * Windows touch keyboard and remote-control tools: those deliver genuine user holds, and the same
 * reasoning is already recorded for the serializer's own signal. Over-attribution here costs a
 * declined release, which the user's release then clears; under-attribution costs a cancelled
 * hold, which it does not.
 *)
procedure TfrmOSKOnScreenKeyboard.UpdateUserHeldModifiers(VKCode, ScanCode, Flags: DWORD; IsUp: Boolean);
var
  shift: TExtShiftStateValue;
begin
  // This unit's own press or release, accounted for exactly once. Neither sets nor clears: an OSK
  // KEYUP is not evidence that the user let go of anything.
  if ConsumeOskModifierEcho(VKCode, IsUp) then
    Exit;

  // keyman32's own injected modifiers -- the serializer's release and restore halves. Not a holder,
  // and its restore press is the very event #8064's serializer half is about.
  //
  // Decided by the hook and carried in the flags, NOT by the scan code. See
  // KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED: for Right Shift the scan code cannot tell Keyman's own
  // injection from a physical press, and reading that one wrong strands the OSK's Shift.
  if (Flags and KEYMAN_OSK_MODIFIER_FLAG_KEYMAN_INJECTED) <> 0 then
    Exit;

  // Windows' compatibility LCtrl for AltGr. Not a key the user is holding; IsSimulatedLControlDown
  // tracks it, and GetAsyncShiftState already subtracts it.
  if ScanCode = SCAN_LEFT_CONTROL_SIMULATED then
    Exit;

  if not OskModifierShiftValueOf(VKCode, shift) then
    Exit;

  if IsUp then
    Exclude(FUserHeldShiftState, shift)
  else
    Include(FUserHeldShiftState, shift);

  KL.Log('UpdateUserHeldModifiers: vk=%x scan=%x isUp=%s UserHeld=%s',
    [VKCode, ScanCode, BoolToStr(IsUp, True), ExtShiftStateToString(FUserHeldShiftState)]);
end;

(**
 * #8064: does the USER -- any holder other than this unit -- hold the key that `shift` names?
 *
 * Two callers, asking the same question of the same field for opposite reasons. ResetShiftStates
 * asks whether a KEYUP it is about to emit would cancel a user hold as well as the OSK's press,
 * and declines the release if so. kbdKeyPressed's FinalState asks whether a hold it suppressed is
 * still owed its restoring KEYDOWN, and declines the press if not. Both sides of the same
 * asymmetry: acting on a hold the user does not have leaves a modifier down with nobody to
 * release it.
 *
 * Matched by the key the event actually lands on, never by family. A family test would be the
 * obvious shortcut and it would be a regression: with the OSK holding essRCtrl and the user
 * holding LCtrl, a family test suppresses the Right Ctrl release, and Right Ctrl is the one
 * identity a user on hardware without that physical key cannot clear themselves -- the "until
 * you reboot" class of report in manual-tests/GH-16462 - osk-sticky-modifier/README.md. Releasing Right Ctrl cannot touch Left Ctrl, so it
 * must not be suppressed by it.
 *
 * The generic entries are the exception, and only in one direction: VK_CONTROL and VK_MENU sent
 * unextended resolve to the LEFT key, so a left-hand or generic user hold is at risk from them
 * and a right-hand one is not.
 *)
function TfrmOSKOnScreenKeyboard.UserHoldsModifier(shift: TExtShiftStateValue): Boolean;
begin
  case shift of
    // TExtShiftState has no chiral Shift values, so essShift is every Shift hold there is.
    essShift: Result := essShift in FUserHeldShiftState;
    essLCtrl: Result := essLCtrl in FUserHeldShiftState;
    essRCtrl: Result := essRCtrl in FUserHeldShiftState;
    essLAlt:  Result := essLAlt in FUserHeldShiftState;
    essRAlt:  Result := essRAlt in FUserHeldShiftState;
    essCtrl:  Result := FUserHeldShiftState * [essCtrl, essLCtrl] <> [];
    essAlt:   Result := FUserHeldShiftState * [essAlt, essLAlt] <> [];
  else
    Result := False;
  end;
end;

procedure TfrmOSKOnScreenKeyboard.FormCreate(Sender: TObject);
var
  family: TOskModifierFamily;
begin
  inherited;

  // #8064: FOskPendingEcho is unit level, so it outlives the form. Any count still standing here
  // belongs to a previous instance and would otherwise be spent swallowing this instance's first
  // user event of that family.
  for family := Low(TOskModifierFamily) to High(TOskModifierFamily) do
  begin
    FOskPendingEcho[family, False] := 0;
    FOskPendingEcho[family, True]  := 0;
  end;

  VisualKeyboards := TVisualKeyboardInfoList.Create;
  VisualKeyboards.Load;

  kbd.DoubleBuffered := False;
  kbd.Transparent := False;
  kbd.DisplayUnderlyingChar := True;
  kbd.DisableExtendedKeys := False;
end;

procedure TfrmOSKOnScreenKeyboard.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(VisualKeyboards);
end;

procedure TfrmOSKOnScreenKeyboard.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := $f2f2f1; //clWhite;   // I4098
  Canvas.FillRect(ClientRect);
end;

procedure TfrmOSKOnScreenKeyboard.FormResize(Sender: TObject);
begin
  inherited;
  kbd.Update;
end;

function TfrmOSKOnScreenKeyboard.GetAsyncShiftState: TExtShiftState;
var
  lctrl: SHORT;
  ralt: SHORT;
begin
  Result := [];
  if (GetAsyncKeyState(VK_SHIFT) and $8000) = $8000 then Result := Result + [essShift];
  if kbd.LRShift then
  begin
    lctrl := GetAsyncKeyState(VK_LCONTROL);
    ralt := GetAsyncKeyState(VK_RMENU);

    if not IsSimulatedLControlDown and ((lctrl and $8000) = $8000) then
      Result := Result + [essLCtrl];
    if (GetAsyncKeyState(VK_RCONTROL) and $8000) = $8000 then Result := Result + [essRCtrl];
    if (GetAsyncKeyState(VK_LMENU) and $8000) = $8000 then Result := Result + [essLAlt];
    if (ralt and $8000) = $8000 then Result := Result + [essRAlt];
  end
  else
  begin
    if (GetAsyncKeyState(VK_CONTROL) and $8000) = $8000 then Result := Result + [essCtrl];
    if (GetAsyncKeyState(VK_MENU) and $8000) = $8000 then Result := Result + [essAlt];
  end;
end;

function TfrmOSKOnScreenKeyboard.HasVisualKeyboard(KeymanID: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to VisualKeyboards.Count - 1 do
    if VisualKeyboards[i].KeymanID = KeymanID then
    begin
      Result := VisualKeyboards[i].FileName <> '';
      Exit;
    end;
  Result := False;
end;

function TfrmOSKOnScreenKeyboard.HasVisualKeyboard: Boolean;
begin
  Result := Assigned(VKI) and Assigned(VKI.Keyboard);
end;

procedure TfrmOSKOnScreenKeyboard.UpdateKeyboard(FLoading: Boolean);
var
  k: TOnScreenKeyboardKey;
  i: Integer;
  FFont: TFont;
  ss: DWord;
  ssi: Integer;
begin
  kbd.BeginUpdate;
  try
    kbd.UnderlyingLayout := FSystemLayout;
    kbd.Keys.ClearValues;

    if FLoading then
    begin
      if not Assigned(VKI) or not Assigned(VKI.Keyboard) then
      begin
        kbd.Display102Key := kbd.EuroLayout;  // I764
        kbd.DisplayUnderlyingChar := True;
        kbd.LRShift := True;
        FPositional := False;
      end
      else
      begin
        kbd.Display102Key := kvkh102 in VKI.Keyboard.Header.Flags;
        kbd.DisplayUnderlyingChar := kvkhDisplayUnderlying in VKI.Keyboard.Header.Flags;
        kbd.LRShift := kvkhAltGr in VKI.Keyboard.Header.Flags;
        i := kmcom.Keyboards.IndexOf(VKI.KeymanName);
        if i > 0 then
          FPositional := kmcom.Keyboards[i].LayoutType = kltPositional
        else
          FPositional := True;
      end;
    end;

    if not Assigned(VKI) or not Assigned(VKI.Keyboard) then
      FFont := Self.Font
    else if FUnicode then
      FFont := VKI.Keyboard.Header.UnicodeFont
    else
      FFont := VKI.Keyboard.Header.ANSIFont;

    kbd.LargeCapFont := not Assigned(VKI) or not Assigned(VKI.Keyboard);
    kbd.DataFont := FFont;

    if not Assigned(VKI) then
      KL.Log('UpdateKeyboard: VKI=nil')
    else if not Assigned(VKI.Keyboard) then
      KL.Log('UpdateKeyboard: VKI<>nil, VKI.Keyboard=nil ['+VKI.KeymanName+']')
    else
      KL.Log('UpdateKeyboard: VKI<>nil, VKI.Keyboard<>nil ['+VKI.KeymanName+']');
    if not Assigned(VKI) or not Assigned(VKI.Keyboard) then Exit;

    KL.Log('UpdateKeyboard: FUnicode='+BoolToStr(FUnicode)+'; VKI.Keyboard.Keys.Count='+IntToStr(VKI.Keyboard.Keys.Count));

    if ((kbd.ShiftState * [essLCtrl, essLAlt]) = [essLCtrl, essLAlt]) and
        kmcom.Options['koAltGrCtrlAlt'].Value then
    begin
      // I2603
      ss := ExtShiftStateToVkShiftState(kbd.ShiftState - [essLCtrl, essLAlt] + [essRAlt]);
      ssi := ValidExtShiftStateIndex(kbd.ShiftState - [essLCtrl, essLAlt] + [essRAlt]);
    end
    else
    begin
      ss := ExtShiftStateToVkShiftState(kbd.ShiftState);
      ssi := ValidExtShiftStateIndex(kbd.ShiftState);
    end;

    for i := 0 to VKI.Keyboard.Keys.Count - 1 do
    begin
      if not FPositional then
      begin
        // Locate the key based on the key cap -- Ignore Alt and Alt+Shift states -- they are bogus!
        if (kbd.ShiftState * [essCtrl,essAlt] = [essAlt])
          then k := nil
          else k := kbd.Keys.ItemsByKeyCap[VKI.Keyboard.Keys[i].VKeyCap, ssi];
        if Assigned(k) then
        begin
          k.KeyValue := VKI.Keyboard.Keys[i].Text;
          k.KeyGlyph := VKI.Keyboard.Keys[i].Bitmap;
        end;
      end
      else if (VKI.Keyboard.Keys[i].Shift = ss) and
        ((FUnicode and (kvkkUnicode in VKI.Keyboard.Keys[i].Flags)) or
         (not FUnicode and not (kvkkUnicode in VKI.Keyboard.Keys[i].Flags))) then
      begin
        k := kbd.Keys.ItemsByUSVK[VKI.Keyboard.Keys[i].VKey];  // I764
        if Assigned(k) then
        begin
          //KL.Log('VisualKeyboard: Key '+k.KeyText+' = '+VKI.Keyboard.Keys[i].Text);
          k.KeyValue := VKI.Keyboard.Keys[i].Text;
          k.KeyGlyph := VKI.Keyboard.Keys[i].Bitmap;
        end;
      end;
    end;
  finally
    kbd.EndUpdate;
  end;
end;

procedure TfrmOSKOnScreenKeyboard.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TfrmOSKOnScreenKeyboard.cmdFadeWhenInactiveClick(Sender: TObject);
begin
  AttachThreadInput(GetWindowThreadProcessId(OldHandle, nil), GetCurrentThread, True);
  SetForegroundWindow(OldHandle);
  AttachThreadInput(GetWindowThreadProcessId(OldHandle, nil), GetCurrentThread, False);
  //FFadeVisualKeyboard := not FFadeVisualKeyboard;
end;

procedure TfrmOSKOnScreenKeyboard.ConstrainSizing(SizeDir: Integer; var Rect: TRect);
var
  rKbdNew, rWnd, rKbd: TRect;
  d: TPoint;
begin
  rWnd := GetParentForm(Self).BoundsRect;
  rKbd := kbd.BoundsRect;

  { Get the difference in size between the window and the keyboard }
  d := Point((rWnd.Right - rWnd.Left) - (rKbd.Right - rKbd.Left), (rWnd.Bottom - rWnd.Top) - (rKbd.Bottom - rKbd.Top));

  { Calculate the new keyboard size }
  rKbdNew := Types.Rect(rKbd.Left, rKbd.Top, Rect.Right - Rect.Left - d.x + rKbd.Left, Rect.Bottom - Rect.Top - d.y + rKbd.Top);

  { Adjust the keyboard size }

  //rKbdNew := Rect(Rect.Right-Rect.Left - (
  case SizeDir of
    WMSZ_BOTTOMRIGHT, WMSZ_RIGHT, WMSZ_LEFT, WMSZ_BOTTOMLEFT, WMSZ_TOPLEFT, WMSZ_TOPRIGHT:
      kbd.AdjustBoundsRect(rKbdNew, True);
    else
      kbd.AdjustBoundsRect(rKbdNew, False);
  end;

  case SizeDir of
    WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT, WMSZ_RIGHT: Rect.Bottom := Rect.Top + d.Y + (rKbdNew.Bottom - rKbdNew.Top);
    WMSZ_TOPRIGHT, WMSZ_TOPLEFT, WMSZ_LEFT: Rect.Top := Rect.Bottom - d.Y - (rKbdNew.Bottom - rKbdNew.Top);
    WMSZ_TOP: Rect.Left := Rect.Right - d.X - (rKbdNew.Right - rKbdNew.Left);
    WMSZ_BOTTOM: Rect.Right := Rect.Left + d.X + (rKbdNew.Right - rKbdNew.Left);
  end;
end;

procedure TfrmOSKOnScreenKeyboard.SaveAsWebPage;   // I4487
var
  vk: TVisualKeyboard;
begin
  if not Assigned(VKI.Keyboard) then
    Exit;
  vk := TVisualKeyboard.Create;
  try
    vk.LoadFromFile(VKI.FileName);
    if dlgSave.Execute then
    begin
      if not SaveWebPage(vk, dlgSave.FileName) then Exit;
      TUtilExecute.URL(dlgSave.FileName);  // I3349
    end;
  finally
    vk.Free;
  end;
end;

function TfrmOSKOnScreenKeyboard.SaveWebPage(vk: TVisualKeyboard; const s: string): Boolean;
begin
  Screen.Cursor := crHourglass;
  try
    with TVisualKeyboardExportHTML.Create(vk) do
    try
      ExportToFile(s);
    finally
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  Result := True;
end;

procedure TfrmOSKOnScreenKeyboard.SelectKeyboard(KeymanID: Integer; Unicode: Boolean);   // I4359
var
  i: Integer;
  FVKI: TVisualKeyboardInfo;
  FLoaded: Boolean;
  SystemLayout: DWord;
begin

  if KeymanID <> KEYMANID_NONKEYMAN
    then SystemLayout := kmcom.Options['koBaseLayout'].Value
    else SystemLayout := frmKeyman7Main.ActiveHKL;  {$MESSAGE HINT 'Why is this // INCORRECT!'}

  FLoaded := FSystemLayout <> SystemLayout;
  FSystemLayout := SystemLayout;
  FUnicode := Unicode;
  FVKI := nil;
  for i := 0 to VisualKeyboards.Count - 1 do
    if VisualKeyboards[i].KeymanID = KeymanID then
    begin
      FVKI := VisualKeyboards[i];
      if FVKI.FileName <> '' then
      begin
        if not Assigned(FVKI.Keyboard) then
        begin
          FLoaded := True;
          FVKI.Keyboard := TVisualKeyboard.Create;
          FVKI.Keyboard.LoadFromFile(FVKI.FileName);
        end;
      end;
      Break;
    end;
  if (FVKI <> VKI) or FLoaded then
  begin
    VKI := FVKI;
    UpdateKeyboard(True);
  end;
end;

end.
