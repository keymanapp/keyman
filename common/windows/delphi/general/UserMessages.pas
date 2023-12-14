(*
  Name:             UserMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Jul 2012

  Modified Date:    6 Nov 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Jul 2012 - mcdurdin - I3390 - Font helper can crash when WM_FONTCHANGE received [CrashID:keyman.exe_8.0.350.0_2C53A3AE_EAccessViolation]
                    04 Nov 2012 - mcdurdin - I3519 - V9.0 - Merge of I3390 - Font helper can crash when WM_FONTCHANGE received
                    28 May 2014 - mcdurdin - I4242 - Crash when OSK closed/reopened without dismissing hint window [CrashID:keyman.exe_9.0.449.0_2C405C5D_EInvalidPointer]
                    23 Jun 2015 - mcdurdin - I4767 - Keyboard debugger does not always activate profile correctly
                    06 Nov 2015 - mcdurdin - I4918 - Text editor is not refreshing correctly with new theme
*)
unit UserMessages;

interface

uses
  Messages;

const
  // UfrmKeyman7Main
  WM_USER_Start = WM_USER+101;
  WM_USER_ParameterPass = WM_USER+100;
  WM_USER_SendFontChange = WM_USER+102;
  WM_USER_VisualKeyboardClosed = WM_USER+105;   // I4242

const
  // UfrmKeymanBase
  WM_USER_FireCommand = WM_USER + 111;
  WM_USER_FormShown = WM_USER + 112;
  WM_USER_ContentRender = WM_USER + 113;
  WM_USER_Step = WM_USER + 114;
  WM_USER_CheckFonts = WM_USER + 115;

const
  WC_COMMAND = 0;
  WC_HELP = 1;
  WC_OPENURL = 2;

  WM_USER_INPUTLANGCHANGE = WM_USER+123;

  WM_USER_Modified = WM_USER + 124;
  WM_USER_UpdateCaption = WM_USER + 125;   // I4918
  WM_USER_OpenFiles = WM_USER + 126;

const
  // SyntaxHighlight
  WM_USER_SYNTAXCOLOURCHANGE = WM_USER + 130;

const
  // UfrmOSKOnScreenKeyboard
  WM_USER_PrintKeyboard = WM_USER + 140;

const
  // UfrmOSKFontHelper
  WM_USER_FontChange = WM_USER + 150;  // I3390   // I3519

const
  // UframeTextEditor
  WM_USER_TextEditor_Command = WM_USER + 170;

const
  // UfrmDebug
  WM_USER_DebugEnd = WM_USER + 200;
  WM_USER_UpdateForceKeyboard = WM_USER + 201;   // I4767

const
  // KeymanTrayIcon
  WM_SYSTEM_TRAY_MESSAGE = WM_USER + 1;

implementation

end.

