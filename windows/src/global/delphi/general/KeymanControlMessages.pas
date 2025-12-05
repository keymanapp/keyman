(*
  Name:             KeymanControlMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    13 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - Initial version
                    20 Jul 2008 - mcdurdin - I1412 - Improve performance of tutorial
                    11 Dec 2009 - mcdurdin - I934 - x64 - platform comms - focus info
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
*)
unit KeymanControlMessages;

interface

const
  //KMC_KEYBOARDCHANGED = 1;   // I3949
  //KMC_CHANGEUISTATE = 2;
  KMC_GETLOADED = 3;
  KMC_REFRESH = 4;
  KMC_INTERFACEHOTKEY = 5;

  KMC_NOTIFYWELCOME = 6;
  KMC_NOTIFYWELCOME_EVENT = 7;    // From Keyman to welcome window

  KMC_ONSCREENKEYBOARD = 8;   // 7.0.248.0

  KMC_SETFOCUSINFO = 9; // 7.1.270.0
  KMC_GETLASTFOCUS = 10; // 7.1.270.0
  KMC_GETLASTACTIVE = 11; // 7.1.270.0

  KMC_GETLASTKEYMANID = 12; // 7.1.274.0
  KMC_GETLASTHKL = 13; // 7.1.274.0

  //KMC_HKLCHANGED = 15; // 8.0.290.0   // I3949

  KMC_KEYDOWN = 16;
  KMC_KEYUP = 17;

  KMC_PROFILECHANGED = 18;  // 9.0.426.0   // I3933

  KMC_HINTRESPONSE = 19;  // 14.0 HIWORD(wParam) = ModalResult, lParam = hint enum

  KMC_WATCHDOG_FAKEFREEZE = 20; // 19.0 - pause Keyman for 5 seconds for debug purposes to test stability
  KMC_WATCHDOG_KEYEVENT   = 21; // 19.0 - let the LowLevelHookWatchDog know that input has happened on another thread

  PC_UPDATE = 0;
  PC_UPDATE_LANGUAGESWITCH = 1;
  PC_HOTKEYCHANGE = 2;

  // KMC_KEYBOARDHOTKEY = 19;  // 9.0.459.0   // I4326 Deprecated in favour of language hotkeys
  //KMC_LANGUAGEHOTKEY = 20;

//TOUCH    KMC_CONTEXT = 19;

  // KMC_NOTIFYWELCOME commands
  NW_NOTIFYHANDLE = 0;
  NW_SENDBALLOON = 1;

  // NW_SENDBALLOON options
  NWB_IDENTIFYICON = 0;
  NWB_TUTORIALFINISHED = 1;
  NWB_KEYMANRUNNING = 2;

  // KMC_NOTIFYWELCOME_EVENT events
  NWE_ICONCLICKED = 0;
  NWE_MENUOPENED = 1;
  NWE_MENUCLOSED = 2;

implementation

end.
