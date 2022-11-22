/*
  Name:             keymancontrol
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    13 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add KMC_REFRESH and KMC_INTERFACEHOTKEY wm_keyman_control messages
                    11 Dec 2009 - mcdurdin - I934 - x64 platfrom comms add KMC_SETFOCUSINFO
                    06 Apr 2010 - mcdurdin - I2271 - Select Keyboard tidy up
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
*/

//#define KMC_KEYBOARDCHANGED	1   // I3949
//#define KMC_CHANGEUISTATE  	2
//#define KMC_GETLOADED	    	3
#define KMC_REFRESH			    4
#define KMC_INTERFACEHOTKEY	5

//#define KMC_NOTIFYWELCOME       6       // 7.0.245.0
//#define KMC_NOTIFYWELCOMEEVENT  7       // 7.0.245.0
#define KMC_ONSCREENKEYBOARD    8       // 7.0.248.0

#define KMC_SETFOCUSINFO     9       // 7.1.270.0
#define KMC_GETLASTFOCUS    10       // 7.1.270.0
#define KMC_GETLASTACTIVE   11

#define KMC_GETLASTKEYMANID 12     // 7.1.274.0
#define KMC_GETLASTHKL      13     // 7.1.274.0

//#define KMC_LANGUAGESWITCH  14      // 8.0.290.0
//#define KMC_HKLCHANGED 15    // 8.0.290.0   // I3949

#define KMC_KEYDOWN 16  // 8.0.291.0
#define KMC_KEYUP   17  // 8.0.291.0

#define KMC_PROFILECHANGED  18  // 9.0.426.0   // I3933

#define PC_UPDATE 0                   // Tell Keyman to update its display of active keyboard
#define PC_UPDATE_LANGUAGESWITCH 1    // Tell Keyman to update its display of active keyboard and then open language switch form
#define PC_HOTKEYCHANGE 2             // Tell Keyman that a hotkey was pressed to switch keyboard

//#define KMC_KEYBOARDHOTKEY  19  // 9.0.459.0   // I4326 Deprecated in favour of language hotkeys
#define KMC_LANGUAGEHOTKEY  20
//TOUCH  #define KMC_CONTEXT 19  // 9.0.450.0

#define RWM_KEYMAN_CONTROL "WM_KEYMAN_CONTROL"

#define khLanguageSwitch  8
