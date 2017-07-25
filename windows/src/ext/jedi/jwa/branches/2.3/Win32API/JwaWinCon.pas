{******************************************************************************}
{                                                                              }
{ Console Applications API interface Unit for Object Pascal                    }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: wincon.h, released June 2000. The original Pascal      }
{ code is: WinCon.pas, released December 2000. The initial developer of the    }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// $Id: JwaWinCon.pas,v 1.12 2007/09/05 11:58:53 dezipaitor Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinCon;

{$WEAKPACKAGEUNIT}


{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinCon.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}

{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinBase, JwaWinType;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}


type
  PCOORD = ^COORD;
  {$EXTERNALSYM PCOORD}
  _COORD = record
    X: SHORT;
    Y: SHORT;
  end;
  {$EXTERNALSYM _COORD}
  COORD = _COORD;
  {$EXTERNALSYM COORD}
  TCoord = _COORD;

  PSMALL_RECT = ^SMALL_RECT;
  {$EXTERNALSYM PSMALL_RECT}
  _SMALL_RECT = record
    Left: SHORT;
    Top: SHORT;
    Right: SHORT;
    Bottom: SHORT;
  end;
  {$EXTERNALSYM _SMALL_RECT}
  SMALL_RECT = _SMALL_RECT;
  {$EXTERNALSYM SMALL_RECT}
  TSmallRect = SMALL_RECT;
  PSmallRect = PSMALL_RECT;

  TCharUnion = record
    case Integer of
      0: (UnicodeChar: WCHAR);
      1: (AsciiChar: AnsiChar);
  end;

  PKEY_EVENT_RECORD = ^KEY_EVENT_RECORD;
  {$EXTERNALSYM PKEY_EVENT_RECORD}
  _KEY_EVENT_RECORD = record
    bKeyDown: BOOL;
    wRepeatCount: WORD;
    wVirtualKeyCode: WORD;
    wVirtualScanCode: WORD;
    uChar: TCharUnion;
    dwControlKeyState: DWORD;
  end;
  {$EXTERNALSYM _KEY_EVENT_RECORD}
  KEY_EVENT_RECORD = _KEY_EVENT_RECORD;
  {$EXTERNALSYM KEY_EVENT_RECORD}
  TKeyEventRecord = KEY_EVENT_RECORD;
  PKeyEventRecord = PKEY_EVENT_RECORD;

//
// ControlKeyState flags
//

const
  RIGHT_ALT_PRESSED  = $0001; // the right alt key is pressed.
  {$EXTERNALSYM RIGHT_ALT_PRESSED}
  LEFT_ALT_PRESSED   = $0002; // the left alt key is pressed.
  {$EXTERNALSYM LEFT_ALT_PRESSED}
  RIGHT_CTRL_PRESSED = $0004; // the right ctrl key is pressed.
  {$EXTERNALSYM RIGHT_CTRL_PRESSED}
  LEFT_CTRL_PRESSED  = $0008; // the left ctrl key is pressed.
  {$EXTERNALSYM LEFT_CTRL_PRESSED}
  SHIFT_PRESSED      = $0010; // the shift key is pressed.
  {$EXTERNALSYM SHIFT_PRESSED}
  NUMLOCK_ON         = $0020; // the numlock light is on.
  {$EXTERNALSYM NUMLOCK_ON}
  SCROLLLOCK_ON      = $0040; // the scrolllock light is on.
  {$EXTERNALSYM SCROLLLOCK_ON}
  CAPSLOCK_ON        = $0080; // the capslock light is on.
  {$EXTERNALSYM CAPSLOCK_ON}
  ENHANCED_KEY       = $0100; // the key is enhanced.
  {$EXTERNALSYM ENHANCED_KEY}
  NLS_DBCSCHAR       = $00010000; // DBCS for JPN: SBCS/DBCS mode.
  {$EXTERNALSYM NLS_DBCSCHAR}
  NLS_ALPHANUMERIC   = $00000000; // DBCS for JPN: Alphanumeric mode.
  {$EXTERNALSYM NLS_ALPHANUMERIC}
  NLS_KATAKANA       = $00020000; // DBCS for JPN: Katakana mode.
  {$EXTERNALSYM NLS_KATAKANA}
  NLS_HIRAGANA       = $00040000; // DBCS for JPN: Hiragana mode.
  {$EXTERNALSYM NLS_HIRAGANA}
  NLS_ROMAN          = $00400000; // DBCS for JPN: Roman/Noroman mode.
  {$EXTERNALSYM NLS_ROMAN}
  NLS_IME_CONVERSION = $00800000; // DBCS for JPN: IME conversion.
  {$EXTERNALSYM NLS_IME_CONVERSION}
  NLS_IME_DISABLE    = $20000000; // DBCS for JPN: IME enable/disable.
  {$EXTERNALSYM NLS_IME_DISABLE}

type
  PMOUSE_EVENT_RECORD = ^MOUSE_EVENT_RECORD;
  {$EXTERNALSYM PMOUSE_EVENT_RECORD}
  _MOUSE_EVENT_RECORD = record
    dwMousePosition: COORD;
    dwButtonState: DWORD;
    dwControlKeyState: DWORD;
    dwEventFlags: DWORD;
  end;
  {$EXTERNALSYM _MOUSE_EVENT_RECORD}
  MOUSE_EVENT_RECORD = _MOUSE_EVENT_RECORD;
  {$EXTERNALSYM MOUSE_EVENT_RECORD}
  TMouseEventRecord = MOUSE_EVENT_RECORD;
  PMouseEventRecord = PMOUSE_EVENT_RECORD;

//
// ButtonState flags
//

const
  FROM_LEFT_1ST_BUTTON_PRESSED = $0001;
  {$EXTERNALSYM FROM_LEFT_1ST_BUTTON_PRESSED}
  RIGHTMOST_BUTTON_PRESSED     = $0002;
  {$EXTERNALSYM RIGHTMOST_BUTTON_PRESSED}
  FROM_LEFT_2ND_BUTTON_PRESSED = $0004;
  {$EXTERNALSYM FROM_LEFT_2ND_BUTTON_PRESSED}
  FROM_LEFT_3RD_BUTTON_PRESSED = $0008;
  {$EXTERNALSYM FROM_LEFT_3RD_BUTTON_PRESSED}
  FROM_LEFT_4TH_BUTTON_PRESSED = $0010;
  {$EXTERNALSYM FROM_LEFT_4TH_BUTTON_PRESSED}

//
// EventFlags
//

  MOUSE_MOVED   = $0001;
  {$EXTERNALSYM MOUSE_MOVED}
  DOUBLE_CLICK  = $0002;
  {$EXTERNALSYM DOUBLE_CLICK}
  MOUSE_WHEELED = $0004;
  {$EXTERNALSYM MOUSE_WHEELED}

type
  PWINDOW_BUFFER_SIZE_RECORD = ^WINDOW_BUFFER_SIZE_RECORD;
  {$EXTERNALSYM PWINDOW_BUFFER_SIZE_RECORD}
  _WINDOW_BUFFER_SIZE_RECORD = record
    dwSize: COORD;
  end;
  {$EXTERNALSYM _WINDOW_BUFFER_SIZE_RECORD}
  WINDOW_BUFFER_SIZE_RECORD = _WINDOW_BUFFER_SIZE_RECORD;
  {$EXTERNALSYM WINDOW_BUFFER_SIZE_RECORD}
  TWindowBufferSizeRecord = WINDOW_BUFFER_SIZE_RECORD;
  PWindowBufferSizeRecord = PWINDOW_BUFFER_SIZE_RECORD;

  PMENU_EVENT_RECORD = ^MENU_EVENT_RECORD;
  {$EXTERNALSYM PMENU_EVENT_RECORD}
  _MENU_EVENT_RECORD = record
    dwCommandId: UINT;
  end;
  {$EXTERNALSYM _MENU_EVENT_RECORD}
  MENU_EVENT_RECORD = _MENU_EVENT_RECORD;
  {$EXTERNALSYM MENU_EVENT_RECORD}
  TMenuEventRecord = MENU_EVENT_RECORD;
  PMenuEventRecord = PMENU_EVENT_RECORD;

  PFOCUS_EVENT_RECORD = ^FOCUS_EVENT_RECORD;
  {$EXTERNALSYM PFOCUS_EVENT_RECORD}
  _FOCUS_EVENT_RECORD = record
    bSetFocus: BOOL;
  end;
  {$EXTERNALSYM _FOCUS_EVENT_RECORD}
  FOCUS_EVENT_RECORD = _FOCUS_EVENT_RECORD;
  {$EXTERNALSYM FOCUS_EVENT_RECORD}
  TFocusEventRecord = FOCUS_EVENT_RECORD;
  PFocusEventRecord = PFOCUS_EVENT_RECORD;

  PINPUT_RECORD = ^INPUT_RECORD;
  {$EXTERNALSYM PINPUT_RECORD}
  _INPUT_RECORD = record
    EventType: WORD;
    case Integer of
      0: (KeyEvent: KEY_EVENT_RECORD);
      1: (MouseEvent: MOUSE_EVENT_RECORD);
      2: (WindowBufferSizeEvent: WINDOW_BUFFER_SIZE_RECORD);
      3: (MenuEvent: MENU_EVENT_RECORD);
      4: (FocusEvent: FOCUS_EVENT_RECORD);
  end;
  {$EXTERNALSYM _INPUT_RECORD}
  INPUT_RECORD = _INPUT_RECORD;
  {$EXTERNALSYM INPUT_RECORD}
  TInputRecord = INPUT_RECORD;
  PInputRecord = PINPUT_RECORD;

//
//  EventType flags:
//

const
  KEY_EVENT                = $0001; // Event contains key event record
  {$EXTERNALSYM KEY_EVENT}
  MOUSE_EVENT_             = $0002; // Event contains mouse event record
  
  WINDOW_BUFFER_SIZE_EVENT = $0004; // Event contains window change event record
  {$EXTERNALSYM WINDOW_BUFFER_SIZE_EVENT}
  MENU_EVENT               = $0008; // Event contains menu event record
  {$EXTERNALSYM MENU_EVENT}
  FOCUS_EVENT              = $0010; // event contains focus change
  {$EXTERNALSYM FOCUS_EVENT}

type
  PCHAR_INFO = ^CHAR_INFO;
  {$EXTERNALSYM PCHAR_INFO}
  _CHAR_INFO = record
    uChar: TCharUnion;
    Attributes: WORD;
  end;
  {$EXTERNALSYM _CHAR_INFO}
  CHAR_INFO = _CHAR_INFO;
  {$EXTERNALSYM CHAR_INFO}
  TCharInfo = CHAR_INFO;
  PCharInfo = PCHAR_INFO;

//
// Attributes flags:
//

const
  FOREGROUND_BLUE            = $0001; // text color contains blue.
  {$EXTERNALSYM FOREGROUND_BLUE}
  FOREGROUND_GREEN           = $0002; // text color contains green.
  {$EXTERNALSYM FOREGROUND_GREEN}
  FOREGROUND_RED             = $0004; // text color contains red.
  {$EXTERNALSYM FOREGROUND_RED}
  FOREGROUND_INTENSITY       = $0008; // text color is intensified.
  {$EXTERNALSYM FOREGROUND_INTENSITY}
  BACKGROUND_BLUE            = $0010; // background color contains blue.
  {$EXTERNALSYM BACKGROUND_BLUE}
  BACKGROUND_GREEN           = $0020; // background color contains green.
  {$EXTERNALSYM BACKGROUND_GREEN}
  BACKGROUND_RED             = $0040; // background color contains red.
  {$EXTERNALSYM BACKGROUND_RED}
  BACKGROUND_INTENSITY       = $0080; // background color is intensified.
  {$EXTERNALSYM BACKGROUND_INTENSITY}
  COMMON_LVB_LEADING_BYTE    = $0100; // Leading Byte of DBCS
  {$EXTERNALSYM COMMON_LVB_LEADING_BYTE}
  COMMON_LVB_TRAILING_BYTE   = $0200; // Trailing Byte of DBCS
  {$EXTERNALSYM COMMON_LVB_TRAILING_BYTE}
  COMMON_LVB_GRID_HORIZONTAL = $0400; // DBCS: Grid attribute: top horizontal.
  {$EXTERNALSYM COMMON_LVB_GRID_HORIZONTAL}
  COMMON_LVB_GRID_LVERTICAL  = $0800; // DBCS: Grid attribute: left vertical.
  {$EXTERNALSYM COMMON_LVB_GRID_LVERTICAL}
  COMMON_LVB_GRID_RVERTICAL  = $1000; // DBCS: Grid attribute: right vertical.
  {$EXTERNALSYM COMMON_LVB_GRID_RVERTICAL}
  COMMON_LVB_REVERSE_VIDEO   = $4000; // DBCS: Reverse fore/back ground attribute.
  {$EXTERNALSYM COMMON_LVB_REVERSE_VIDEO}
  COMMON_LVB_UNDERSCORE      = $8000; // DBCS: Underscore.
  {$EXTERNALSYM COMMON_LVB_UNDERSCORE}

  COMMON_LVB_SBCSDBCS        = $0300; // SBCS or DBCS flag.
  {$EXTERNALSYM COMMON_LVB_SBCSDBCS}

type
  PCONSOLE_SCREEN_BUFFER_INFO = ^CONSOLE_SCREEN_BUFFER_INFO;
  {$EXTERNALSYM PCONSOLE_SCREEN_BUFFER_INFO}
  _CONSOLE_SCREEN_BUFFER_INFO = record
    dwSize: COORD;
    dwCursorPosition: COORD;
    wAttributes: WORD;
    srWindow: SMALL_RECT;
    dwMaximumWindowSize: COORD;
  end;
  {$EXTERNALSYM _CONSOLE_SCREEN_BUFFER_INFO}
  CONSOLE_SCREEN_BUFFER_INFO = _CONSOLE_SCREEN_BUFFER_INFO;
  {$EXTERNALSYM CONSOLE_SCREEN_BUFFER_INFO}
  TConsoleScreenBufferInfo = CONSOLE_SCREEN_BUFFER_INFO;
  PConsoleScreenBufferInfo = PCONSOLE_SCREEN_BUFFER_INFO;

  PCONSOLE_CURSOR_INFO = ^CONSOLE_CURSOR_INFO;
  {$EXTERNALSYM PCONSOLE_CURSOR_INFO}
  _CONSOLE_CURSOR_INFO = record
    dwSize: DWORD;
    bVisible: BOOL;
  end;
  {$EXTERNALSYM _CONSOLE_CURSOR_INFO}
  CONSOLE_CURSOR_INFO = _CONSOLE_CURSOR_INFO;
  {$EXTERNALSYM CONSOLE_CURSOR_INFO}
  TConsoleCursorInfo = CONSOLE_CURSOR_INFO;
  PConsoleCursorInfo = PCONSOLE_CURSOR_INFO;

  _CONSOLE_FONT_INFO = record
    nFont: DWORD;
    dwFontSize: COORD;
  end;
  {$EXTERNALSYM _CONSOLE_FONT_INFO}
  CONSOLE_FONT_INFO = _CONSOLE_FONT_INFO;
  {$EXTERNALSYM CONSOLE_FONT_INFO}
  PCONSOLE_FONT_INFO = ^CONSOLE_FONT_INFO;
  {$EXTERNALSYM PCONSOLE_FONT_INFO}
  TConsoleFontInfo = CONSOLE_FONT_INFO;
  PConsoleFontInfo = PCONSOLE_FONT_INFO;

  _CONSOLE_SELECTION_INFO = record
    dwFlags: DWORD;
    dwSelectionAnchor: COORD;
    srSelection: SMALL_RECT;
  end;
  {$EXTERNALSYM _CONSOLE_SELECTION_INFO}
  CONSOLE_SELECTION_INFO = _CONSOLE_SELECTION_INFO;
  {$EXTERNALSYM CONSOLE_SELECTION_INFO}
  PCONSOLE_SELECTION_INFO = ^CONSOLE_SELECTION_INFO;
  {$EXTERNALSYM PCONSOLE_SELECTION_INFO}
  TConsoleSelectionInfo = CONSOLE_SELECTION_INFO;
  PConsoleSelectionInfo = PCONSOLE_SELECTION_INFO;  

//
// Selection flags
//

const
  CONSOLE_NO_SELECTION          = $0000;
  {$EXTERNALSYM CONSOLE_NO_SELECTION}
  CONSOLE_SELECTION_IN_PROGRESS = $0001;   // selection has begun
  {$EXTERNALSYM CONSOLE_SELECTION_IN_PROGRESS}
  CONSOLE_SELECTION_NOT_EMPTY   = $0002;   // non-null select rectangle
  {$EXTERNALSYM CONSOLE_SELECTION_NOT_EMPTY}
  CONSOLE_MOUSE_SELECTION       = $0004;   // selecting with mouse
  {$EXTERNALSYM CONSOLE_MOUSE_SELECTION}
  CONSOLE_MOUSE_DOWN            = $0008;   // mouse is down
  {$EXTERNALSYM CONSOLE_MOUSE_DOWN}

//
// typedef for ctrl-c handler routines
//

type
  PHANDLER_ROUTINE = function(CtrlType: DWORD): BOOL; stdcall;
  {$EXTERNALSYM PHANDLER_ROUTINE}
  THandlerRoutine = PHANDLER_ROUTINE;

const
  CTRL_C_EVENT        = 0;
  {$EXTERNALSYM CTRL_C_EVENT}
  CTRL_BREAK_EVENT    = 1;
  {$EXTERNALSYM CTRL_BREAK_EVENT}
  CTRL_CLOSE_EVENT    = 2;
  {$EXTERNALSYM CTRL_CLOSE_EVENT}
  // 3 is reserved!
  // 4 is reserved!
  CTRL_LOGOFF_EVENT   = 5;
  {$EXTERNALSYM CTRL_LOGOFF_EVENT}
  CTRL_SHUTDOWN_EVENT = 6;
  {$EXTERNALSYM CTRL_SHUTDOWN_EVENT}

//
//  Input Mode flags:
//

  ENABLE_PROCESSED_INPUT = $0001;
  {$EXTERNALSYM ENABLE_PROCESSED_INPUT}
  ENABLE_LINE_INPUT      = $0002;
  {$EXTERNALSYM ENABLE_LINE_INPUT}
  ENABLE_ECHO_INPUT      = $0004;
  {$EXTERNALSYM ENABLE_ECHO_INPUT}
  ENABLE_WINDOW_INPUT    = $0008;
  {$EXTERNALSYM ENABLE_WINDOW_INPUT}
  ENABLE_MOUSE_INPUT     = $0010;
  {$EXTERNALSYM ENABLE_MOUSE_INPUT}

//
// Output Mode flags:
//

  ENABLE_PROCESSED_OUTPUT   = $0001;
  {$EXTERNALSYM ENABLE_PROCESSED_OUTPUT}
  ENABLE_WRAP_AT_EOL_OUTPUT = $0002;
  {$EXTERNALSYM ENABLE_WRAP_AT_EOL_OUTPUT}

//
// direct API definitions.
//

function PeekConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM PeekConsoleInputA}
function PeekConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM PeekConsoleInputW}
function PeekConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM PeekConsoleInput}

function ReadConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleInputA}
function ReadConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleInputW}
function ReadConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleInput}

function WriteConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleInputA}
function WriteConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleInputW}
function WriteConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleInput}

function ReadConsoleOutputA(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputA}
function ReadConsoleOutputW(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputW}
function ReadConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutput}

function WriteConsoleOutputA(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputA}
function WriteConsoleOutputW(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputW}
function WriteConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutput}

function ReadConsoleOutputCharacterA(hConsoleOutput: HANDLE; lpCharacter: LPSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputCharacterA}
function ReadConsoleOutputCharacterW(hConsoleOutput: HANDLE; lpCharacter: LPWSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputCharacterW}
function ReadConsoleOutputCharacter(hConsoleOutput: HANDLE; lpCharacter: LPTSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputCharacter}

function ReadConsoleOutputAttribute(hConsoleOutput: HANDLE;
  var lpAttribute: DWORD; nLength: DWORD; dwReadCoord: COORD;
  var lpNumberOfAttrsRead: DWORD): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleOutputAttribute}

function WriteConsoleOutputCharacterA(hConsoleOutput: HANDLE;
  lpCharacter: LPCSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputCharacterA}
function WriteConsoleOutputCharacterW(hConsoleOutput: HANDLE;
  lpCharacter: LPCWSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputCharacterW}
function WriteConsoleOutputCharacter(hConsoleOutput: HANDLE;
  lpCharacter: LPCTSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputCharacter}

function WriteConsoleOutputAttribute(hConsoleOutput: HANDLE; lpAttribute: PWORD;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfAttrsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleOutputAttribute}

function FillConsoleOutputCharacterA(hConsoleOutput: HANDLE; cCharacter: AnsiChar;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM FillConsoleOutputCharacterA}
function FillConsoleOutputCharacterW(hConsoleOutput: HANDLE; cCharacter: WCHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM FillConsoleOutputCharacterW}
function FillConsoleOutputCharacter(hConsoleOutput: HANDLE; cCharacter: TCHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM FillConsoleOutputCharacter}

function FillConsoleOutputAttribute(hConsoleOutput: HANDLE; wAttribute: WORD;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfAttrsWritten: DWORD): BOOL; stdcall;
{$EXTERNALSYM FillConsoleOutputAttribute}
function GetConsoleMode(hConsoleHandle: HANDLE; var lpMode: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetConsoleMode}
function GetNumberOfConsoleInputEvents(hConsoleInput: HANDLE;
  var lpNumberOfEvents: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetNumberOfConsoleInputEvents}
function GetConsoleScreenBufferInfo(hConsoleOutput: HANDLE;
  var lpConsoleScreenBufferInfo: CONSOLE_SCREEN_BUFFER_INFO): BOOL; stdcall;
{$EXTERNALSYM GetConsoleScreenBufferInfo}
function GetLargestConsoleWindowSize(hConsoleOutput: HANDLE): COORD; stdcall;
{$EXTERNALSYM GetLargestConsoleWindowSize}
function GetConsoleCursorInfo(hConsoleOutput: HANDLE;
  var lpConsoleCursorInfo: CONSOLE_CURSOR_INFO): BOOL; stdcall;
function GetCurrentConsoleFont(hConsoleOutput: HANDLE; bMaximumWindow: BOOL;
  var lpConsoleCurrentFont: CONSOLE_FONT_INFO): BOOL; stdcall;
{$EXTERNALSYM GetCurrentConsoleFont}
function GetConsoleFontSize(hConsoleOutput: HANDLE; nFont: DWORD): COORD; stdcall;
{$EXTERNALSYM GetConsoleFontSize}
function GetConsoleSelectionInfo(var lpConsoleSelectionInfo: CONSOLE_SELECTION_INFO): BOOL; stdcall;
{$EXTERNALSYM GetConsoleSelectionInfo}
{$EXTERNALSYM GetConsoleCursorInfo}
function GetNumberOfConsoleMouseButtons(var lpNumberOfMouseButtons: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetNumberOfConsoleMouseButtons}
function SetConsoleMode(hConsoleHandle: HANDLE; dwMode: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetConsoleMode}
function SetConsoleActiveScreenBuffer(hConsoleOutput: HANDLE): BOOL; stdcall;
{$EXTERNALSYM SetConsoleActiveScreenBuffer}
function FlushConsoleInputBuffer(hConsoleInput: HANDLE): BOOL; stdcall;
{$EXTERNALSYM FlushConsoleInputBuffer}
function SetConsoleScreenBufferSize(hConsoleOutput: HANDLE; dwSize: COORD): BOOL; stdcall;
{$EXTERNALSYM SetConsoleScreenBufferSize}
function SetConsoleCursorPosition(hConsoleOutput: HANDLE; dwCursorPosition: COORD): BOOL; stdcall;
{$EXTERNALSYM SetConsoleCursorPosition}
function SetConsoleCursorInfo(hConsoleOutput: HANDLE;
  var lpConsoleCursorInfo: CONSOLE_CURSOR_INFO): BOOL; stdcall;
{$EXTERNALSYM SetConsoleCursorInfo}

function ScrollConsoleScreenBufferA(hConsoleOutput: HANDLE;
  const lpScrollRectangle: SMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$EXTERNALSYM ScrollConsoleScreenBufferA}
function ScrollConsoleScreenBufferW(hConsoleOutput: HANDLE;
  const lpScrollRectangle: PSMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$EXTERNALSYM ScrollConsoleScreenBufferW}
function ScrollConsoleScreenBuffer(hConsoleOutput: HANDLE;
  const lpScrollRectangle: PSMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$EXTERNALSYM ScrollConsoleScreenBuffer}

function SetConsoleWindowInfo(hConsoleOutput: HANDLE; bAbsolute: BOOL;
  const lpConsoleWindow: SMALL_RECT): BOOL; stdcall;
{$EXTERNALSYM SetConsoleWindowInfo}
function SetConsoleTextAttribute(hConsoleOutput: HANDLE; wAttributes: WORD): BOOL; stdcall;
{$EXTERNALSYM SetConsoleTextAttribute}
function SetConsoleCtrlHandler(HandlerRoutine: PHANDLER_ROUTINE; Add: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetConsoleCtrlHandler}
function GenerateConsoleCtrlEvent(dwCtrlEvent: DWORD; dwProcessGroupId: DWORD): BOOL; stdcall;
{$EXTERNALSYM GenerateConsoleCtrlEvent}
function AllocConsole: BOOL; stdcall;
{$EXTERNALSYM AllocConsole}
function FreeConsole: BOOL; stdcall;
{$EXTERNALSYM FreeConsole}
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall;
{$EXTERNALSYM AttachConsole}

const
  ATTACH_PARENT_PROCESS = DWORD(-1);
  {$EXTERNALSYM ATTACH_PARENT_PROCESS}

function GetConsoleTitleA(lpConsoleTitle: LPSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleTitleA}
function GetConsoleTitleW(lpConsoleTitle: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleTitleW}
function GetConsoleTitle(lpConsoleTitle: LPTSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleTitle}

function SetConsoleTitleA(lpConsoleTitle: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SetConsoleTitleA}
function SetConsoleTitleW(lpConsoleTitle: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM SetConsoleTitleW}
function SetConsoleTitle(lpConsoleTitle: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SetConsoleTitle}

function ReadConsoleA(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleA}
function ReadConsoleW(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM ReadConsoleW}
function ReadConsole(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM ReadConsole}

function WriteConsoleA(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleA}
function WriteConsoleW(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM WriteConsoleW}
function WriteConsole(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$EXTERNALSYM WriteConsole}

const
  CONSOLE_TEXTMODE_BUFFER = 1;
  {$EXTERNALSYM CONSOLE_TEXTMODE_BUFFER}

function CreateConsoleScreenBuffer(dwDesiredAccess: DWORD; dwShareMode: DWORD;
  lpSecurityAttributes: PSECURITY_ATTRIBUTES; dwFlags: DWORD;
  lpScreenBufferData: LPVOID): HANDLE; stdcall;
{$EXTERNALSYM CreateConsoleScreenBuffer}
function GetConsoleCP: UINT; stdcall;
{$EXTERNALSYM GetConsoleCP}
function SetConsoleCP(wCodePageID: UINT): BOOL; stdcall;
{$EXTERNALSYM SetConsoleCP}
function GetConsoleOutputCP: UINT; stdcall;
{$EXTERNALSYM GetConsoleOutputCP}
function SetConsoleOutputCP(wCodePageID: UINT): BOOL; stdcall;
{$EXTERNALSYM SetConsoleOutputCP}

const
  CONSOLE_FULLSCREEN = 1;            // fullscreen console
  {$EXTERNALSYM CONSOLE_FULLSCREEN}
  CONSOLE_FULLSCREEN_HARDWARE = 2;   // console owns the hardware
  {$EXTERNALSYM CONSOLE_FULLSCREEN_HARDWARE}

function GetConsoleDisplayMode(var lpModeFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetConsoleDisplayMode}

function GetConsoleWindow: HWND; stdcall;
{$EXTERNALSYM GetConsoleWindow}

function GetConsoleProcessList(var lpdwProcessList: LPDWORD; dwProcessCount: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleProcessList}

//
// Aliasing apis.
//

function AddConsoleAliasA(Source, Target, ExeName: LPSTR): BOOL; stdcall;
{$EXTERNALSYM AddConsoleAliasA}
function AddConsoleAliasW(Source, Target, ExeName: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM AddConsoleAliasW}
function AddConsoleAlias(Source, Target, ExeName: LPTSTR): BOOL; stdcall;
{$EXTERNALSYM AddConsoleAlias}

function GetConsoleAliasA(Source, TargetBuffer: LPSTR; TargetBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasA}
function GetConsoleAliasW(Source, TargetBuffer: LPWSTR; TargetBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasW}
function GetConsoleAlias(Source, TargetBuffer: LPTSTR; TargetBufferLength: DWORD; ExeName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAlias}

function GetConsoleAliasesLengthA(ExeName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasesLengthA}
function GetConsoleAliasesLengthW(ExeName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasesLengthW}
function GetConsoleAliasesLength(ExeName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasesLength}

function GetConsoleAliasExesLengthA: DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExesLengthA}
function GetConsoleAliasExesLengthW: DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExesLengthW}
function GetConsoleAliasExesLength: DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExesLength}

function GetConsoleAliasesA(AliasBuffer: LPSTR; AliasBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasesA}
function GetConsoleAliasesW(AliasBuffer: LPWSTR; AliasBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasesW}
function GetConsoleAliases(AliasBuffer: LPTSTR; AliasBufferLength: DWORD; ExeName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliases}

function GetConsoleAliasExesA(ExeNameBuffer: LPSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExesA}
function GetConsoleAliasExesW(ExeNameBuffer: LPWSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExesW}
function GetConsoleAliasExes(ExeNameBuffer: LPTSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetConsoleAliasExes}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  kernel32 = 'kernel32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _PeekConsoleInputA: Pointer;

function PeekConsoleInputA;
begin
  GetProcedureAddress(_PeekConsoleInputA, kernel32, 'PeekConsoleInputA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PeekConsoleInputA]
  end;
end;

var
  _PeekConsoleInputW: Pointer;

function PeekConsoleInputW;
begin
  GetProcedureAddress(_PeekConsoleInputW, kernel32, 'PeekConsoleInputW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PeekConsoleInputW]
  end;
end;

var
  _PeekConsoleInput: Pointer;

function PeekConsoleInput;
begin
  GetProcedureAddress(_PeekConsoleInput, kernel32, 'PeekConsoleInput' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PeekConsoleInput]
  end;
end;

var
  _ReadConsoleInputA: Pointer;

function ReadConsoleInputA;
begin
  GetProcedureAddress(_ReadConsoleInputA, kernel32, 'ReadConsoleInputA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleInputA]
  end;
end;

var
  _ReadConsoleInputW: Pointer;

function ReadConsoleInputW;
begin
  GetProcedureAddress(_ReadConsoleInputW, kernel32, 'ReadConsoleInputW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleInputW]
  end;
end;

var
  _ReadConsoleInput: Pointer;

function ReadConsoleInput;
begin
  GetProcedureAddress(_ReadConsoleInput, kernel32, 'ReadConsoleInput' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleInput]
  end;
end;

var
  _WriteConsoleInputA: Pointer;

function WriteConsoleInputA;
begin
  GetProcedureAddress(_WriteConsoleInputA, kernel32, 'WriteConsoleInputA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleInputA]
  end;
end;

var
  _WriteConsoleInputW: Pointer;

function WriteConsoleInputW;
begin
  GetProcedureAddress(_WriteConsoleInputW, kernel32, 'WriteConsoleInputW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleInputW]
  end;
end;

var
  _WriteConsoleInput: Pointer;

function WriteConsoleInput;
begin
  GetProcedureAddress(_WriteConsoleInput, kernel32, 'WriteConsoleInput' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleInput]
  end;
end;

var
  _ReadConsoleOutputA: Pointer;

function ReadConsoleOutputA;
begin
  GetProcedureAddress(_ReadConsoleOutputA, kernel32, 'ReadConsoleOutputA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputA]
  end;
end;

var
  _ReadConsoleOutputW: Pointer;

function ReadConsoleOutputW;
begin
  GetProcedureAddress(_ReadConsoleOutputW, kernel32, 'ReadConsoleOutputW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputW]
  end;
end;

var
  _ReadConsoleOutput: Pointer;

function ReadConsoleOutput;
begin
  GetProcedureAddress(_ReadConsoleOutput, kernel32, 'ReadConsoleOutput' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutput]
  end;
end;

var
  _WriteConsoleOutputA: Pointer;

function WriteConsoleOutputA;
begin
  GetProcedureAddress(_WriteConsoleOutputA, kernel32, 'WriteConsoleOutputA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputA]
  end;
end;

var
  _WriteConsoleOutputW: Pointer;

function WriteConsoleOutputW;
begin
  GetProcedureAddress(_WriteConsoleOutputW, kernel32, 'WriteConsoleOutputW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputW]
  end;
end;

var
  _WriteConsoleOutput: Pointer;

function WriteConsoleOutput;
begin
  GetProcedureAddress(_WriteConsoleOutput, kernel32, 'WriteConsoleOutput' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutput]
  end;
end;

var
  _ReadConsoleOutputCharacterA: Pointer;

function ReadConsoleOutputCharacterA;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacterA, kernel32, 'ReadConsoleOutputCharacterA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputCharacterA]
  end;
end;

var
  _ReadConsoleOutputCharacterW: Pointer;

function ReadConsoleOutputCharacterW;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacterW, kernel32, 'ReadConsoleOutputCharacterW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputCharacterW]
  end;
end;

var
  _ReadConsoleOutputCharacter: Pointer;

function ReadConsoleOutputCharacter;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacter, kernel32, 'ReadConsoleOutputCharacter' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputCharacter]
  end;
end;

var
  _ReadConsoleOutputAttribute: Pointer;

function ReadConsoleOutputAttribute;
begin
  GetProcedureAddress(_ReadConsoleOutputAttribute, kernel32, 'ReadConsoleOutputAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleOutputAttribute]
  end;
end;

var
  _WriteConsoleOutputCharacterA: Pointer;

function WriteConsoleOutputCharacterA;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacterA, kernel32, 'WriteConsoleOutputCharacterA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputCharacterA]
  end;
end;

var
  _WriteConsoleOutputCharacterW: Pointer;

function WriteConsoleOutputCharacterW;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacterW, kernel32, 'WriteConsoleOutputCharacterW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputCharacterW]
  end;
end;

var
  _WriteConsoleOutputCharacter: Pointer;

function WriteConsoleOutputCharacter;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacter, kernel32, 'WriteConsoleOutputCharacter' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputCharacter]
  end;
end;

var
  _WriteConsoleOutputAttribute: Pointer;

function WriteConsoleOutputAttribute;
begin
  GetProcedureAddress(_WriteConsoleOutputAttribute, kernel32, 'WriteConsoleOutputAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleOutputAttribute]
  end;
end;

var
  _FillConsoleOutputCharacterA: Pointer;

function FillConsoleOutputCharacterA;
begin
  GetProcedureAddress(_FillConsoleOutputCharacterA, kernel32, 'FillConsoleOutputCharacterA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FillConsoleOutputCharacterA]
  end;
end;

var
  _FillConsoleOutputCharacterW: Pointer;

function FillConsoleOutputCharacterW;
begin
  GetProcedureAddress(_FillConsoleOutputCharacterW, kernel32, 'FillConsoleOutputCharacterW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FillConsoleOutputCharacterW]
  end;
end;

var
  _FillConsoleOutputCharacter: Pointer;

function FillConsoleOutputCharacter;
begin
  GetProcedureAddress(_FillConsoleOutputCharacter, kernel32, 'FillConsoleOutputCharacter' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FillConsoleOutputCharacter]
  end;
end;

var
  _FillConsoleOutputAttribute: Pointer;

function FillConsoleOutputAttribute;
begin
  GetProcedureAddress(_FillConsoleOutputAttribute, kernel32, 'FillConsoleOutputAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FillConsoleOutputAttribute]
  end;
end;

var
  _GetConsoleMode: Pointer;

function GetConsoleMode;
begin
  GetProcedureAddress(_GetConsoleMode, kernel32, 'GetConsoleMode');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleMode]
  end;
end;

var
  _GetNumberOfConsoleInputEvents: Pointer;

function GetNumberOfConsoleInputEvents;
begin
  GetProcedureAddress(_GetNumberOfConsoleInputEvents, kernel32, 'GetNumberOfConsoleInputEvents');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetNumberOfConsoleInputEvents]
  end;
end;

var
  _GetConsoleScreenBufferInfo: Pointer;

function GetConsoleScreenBufferInfo;
begin
  GetProcedureAddress(_GetConsoleScreenBufferInfo, kernel32, 'GetConsoleScreenBufferInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleScreenBufferInfo]
  end;
end;

var
  _GetLargestConsoleWindowSize: Pointer;

function GetLargestConsoleWindowSize;
begin
  GetProcedureAddress(_GetLargestConsoleWindowSize, kernel32, 'GetLargestConsoleWindowSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLargestConsoleWindowSize]
  end;
end;

var
  _GetConsoleCursorInfo: Pointer;

function GetConsoleCursorInfo;
begin
  GetProcedureAddress(_GetConsoleCursorInfo, kernel32, 'GetConsoleCursorInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleCursorInfo]
  end;
end;

var
  _GetCurrentConsoleFont: Pointer;

function GetCurrentConsoleFont;
begin
  GetProcedureAddress(_GetCurrentConsoleFont, kernel32, 'GetCurrentConsoleFont');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetCurrentConsoleFont]
  end;
end;

var
  _GetConsoleFontSize: Pointer;

function GetConsoleFontSize;
begin
  GetProcedureAddress(_GetConsoleFontSize, kernel32, 'GetConsoleFontSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleFontSize]
  end;
end;

var
  _GetConsoleSelectionInfo: Pointer;

function GetConsoleSelectionInfo;
begin
  GetProcedureAddress(_GetConsoleSelectionInfo, kernel32, 'GetConsoleSelectionInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleSelectionInfo]
  end;
end;

var
  _GetNumberOfConsoleMouseButtons: Pointer;

function GetNumberOfConsoleMouseButtons;
begin
  GetProcedureAddress(_GetNumberOfConsoleMouseButtons, kernel32, 'GetNumberOfConsoleMouseButtons');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetNumberOfConsoleMouseButtons]
  end;
end;

var
  _SetConsoleMode: Pointer;

function SetConsoleMode;
begin
  GetProcedureAddress(_SetConsoleMode, kernel32, 'SetConsoleMode');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleMode]
  end;
end;

var
  _SetConsoleActiveScreenBuffer: Pointer;

function SetConsoleActiveScreenBuffer;
begin
  GetProcedureAddress(_SetConsoleActiveScreenBuffer, kernel32, 'SetConsoleActiveScreenBuffer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleActiveScreenBuffer]
  end;
end;

var
  _FlushConsoleInputBuffer: Pointer;

function FlushConsoleInputBuffer;
begin
  GetProcedureAddress(_FlushConsoleInputBuffer, kernel32, 'FlushConsoleInputBuffer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FlushConsoleInputBuffer]
  end;
end;

var
  _SetConsoleScreenBufferSize: Pointer;

function SetConsoleScreenBufferSize;
begin
  GetProcedureAddress(_SetConsoleScreenBufferSize, kernel32, 'SetConsoleScreenBufferSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleScreenBufferSize]
  end;
end;

var
  _SetConsoleCursorPosition: Pointer;

function SetConsoleCursorPosition;
begin
  GetProcedureAddress(_SetConsoleCursorPosition, kernel32, 'SetConsoleCursorPosition');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleCursorPosition]
  end;
end;

var
  _SetConsoleCursorInfo: Pointer;

function SetConsoleCursorInfo;
begin
  GetProcedureAddress(_SetConsoleCursorInfo, kernel32, 'SetConsoleCursorInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleCursorInfo]
  end;
end;

var
  _ScrollConsoleScreenBufferA: Pointer;

function ScrollConsoleScreenBufferA;
begin
  GetProcedureAddress(_ScrollConsoleScreenBufferA, kernel32, 'ScrollConsoleScreenBufferA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScrollConsoleScreenBufferA]
  end;
end;

var
  _ScrollConsoleScreenBufferW: Pointer;

function ScrollConsoleScreenBufferW;
begin
  GetProcedureAddress(_ScrollConsoleScreenBufferW, kernel32, 'ScrollConsoleScreenBufferW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScrollConsoleScreenBufferW]
  end;
end;

var
  _ScrollConsoleScreenBuffer: Pointer;

function ScrollConsoleScreenBuffer;
begin
  GetProcedureAddress(_ScrollConsoleScreenBuffer, kernel32, 'ScrollConsoleScreenBuffer' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScrollConsoleScreenBuffer]
  end;
end;

var
  _SetConsoleWindowInfo: Pointer;

function SetConsoleWindowInfo;
begin
  GetProcedureAddress(_SetConsoleWindowInfo, kernel32, 'SetConsoleWindowInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleWindowInfo]
  end;
end;

var
  _SetConsoleTextAttribute: Pointer;

function SetConsoleTextAttribute;
begin
  GetProcedureAddress(_SetConsoleTextAttribute, kernel32, 'SetConsoleTextAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleTextAttribute]
  end;
end;

var
  _SetConsoleCtrlHandler: Pointer;

function SetConsoleCtrlHandler;
begin
  GetProcedureAddress(_SetConsoleCtrlHandler, kernel32, 'SetConsoleCtrlHandler');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleCtrlHandler]
  end;
end;

var
  _GenerateConsoleCtrlEvent: Pointer;

function GenerateConsoleCtrlEvent;
begin
  GetProcedureAddress(_GenerateConsoleCtrlEvent, kernel32, 'GenerateConsoleCtrlEvent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GenerateConsoleCtrlEvent]
  end;
end;

var
  _AllocConsole: Pointer;

function AllocConsole;
begin
  GetProcedureAddress(_AllocConsole, kernel32, 'AllocConsole');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AllocConsole]
  end;
end;

var
  _FreeConsole: Pointer;

function FreeConsole;
begin
  GetProcedureAddress(_FreeConsole, kernel32, 'FreeConsole');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeConsole]
  end;
end;

var
  _AttachConsole: Pointer;

function AttachConsole;
begin
  GetProcedureAddress(_AttachConsole, kernel32, 'AttachConsole');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AttachConsole]
  end;
end;

var
  _GetConsoleTitleA: Pointer;

function GetConsoleTitleA;
begin
  GetProcedureAddress(_GetConsoleTitleA, kernel32, 'GetConsoleTitleA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleTitleA]
  end;
end;

var
  _GetConsoleTitleW: Pointer;

function GetConsoleTitleW;
begin
  GetProcedureAddress(_GetConsoleTitleW, kernel32, 'GetConsoleTitleW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleTitleW]
  end;
end;

var
  _GetConsoleTitle: Pointer;

function GetConsoleTitle;
begin
  GetProcedureAddress(_GetConsoleTitle, kernel32, 'GetConsoleTitle' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleTitle]
  end;
end;

var
  _SetConsoleTitleA: Pointer;

function SetConsoleTitleA;
begin
  GetProcedureAddress(_SetConsoleTitleA, kernel32, 'SetConsoleTitleA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleTitleA]
  end;
end;

var
  _SetConsoleTitleW: Pointer;

function SetConsoleTitleW;
begin
  GetProcedureAddress(_SetConsoleTitleW, kernel32, 'SetConsoleTitleW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleTitleW]
  end;
end;

var
  _SetConsoleTitle: Pointer;

function SetConsoleTitle;
begin
  GetProcedureAddress(_SetConsoleTitle, kernel32, 'SetConsoleTitle' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleTitle]
  end;
end;

var
  _ReadConsoleA: Pointer;

function ReadConsoleA;
begin
  GetProcedureAddress(_ReadConsoleA, kernel32, 'ReadConsoleA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleA]
  end;
end;

var
  _ReadConsoleW: Pointer;

function ReadConsoleW;
begin
  GetProcedureAddress(_ReadConsoleW, kernel32, 'ReadConsoleW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsoleW]
  end;
end;

var
  _ReadConsole: Pointer;

function ReadConsole;
begin
  GetProcedureAddress(_ReadConsole, kernel32, 'ReadConsole' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadConsole]
  end;
end;

var
  _WriteConsoleA: Pointer;

function WriteConsoleA;
begin
  GetProcedureAddress(_WriteConsoleA, kernel32, 'WriteConsoleA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleA]
  end;
end;

var
  _WriteConsoleW: Pointer;

function WriteConsoleW;
begin
  GetProcedureAddress(_WriteConsoleW, kernel32, 'WriteConsoleW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsoleW]
  end;
end;

var
  _WriteConsole: Pointer;

function WriteConsole;
begin
  GetProcedureAddress(_WriteConsole, kernel32, 'WriteConsole' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteConsole]
  end;
end;

var
  _CreateConsoleScreenBuffer: Pointer;

function CreateConsoleScreenBuffer;
begin
  GetProcedureAddress(_CreateConsoleScreenBuffer, kernel32, 'CreateConsoleScreenBuffer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateConsoleScreenBuffer]
  end;
end;

var
  _GetConsoleCP: Pointer;

function GetConsoleCP;
begin
  GetProcedureAddress(_GetConsoleCP, kernel32, 'GetConsoleCP');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleCP]
  end;
end;

var
  _SetConsoleCP: Pointer;

function SetConsoleCP;
begin
  GetProcedureAddress(_SetConsoleCP, kernel32, 'SetConsoleCP');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleCP]
  end;
end;

var
  _GetConsoleOutputCP: Pointer;

function GetConsoleOutputCP;
begin
  GetProcedureAddress(_GetConsoleOutputCP, kernel32, 'GetConsoleOutputCP');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleOutputCP]
  end;
end;

var
  _SetConsoleOutputCP: Pointer;

function SetConsoleOutputCP;
begin
  GetProcedureAddress(_SetConsoleOutputCP, kernel32, 'SetConsoleOutputCP');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetConsoleOutputCP]
  end;
end;

var
  _GetConsoleDisplayMode: Pointer;

function GetConsoleDisplayMode;
begin
  GetProcedureAddress(_GetConsoleDisplayMode, kernel32, 'GetConsoleDisplayMode');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleDisplayMode]
  end;
end;

var
  _GetConsoleWindow: Pointer;

function GetConsoleWindow;
begin
  GetProcedureAddress(_GetConsoleWindow, kernel32, 'GetConsoleWindow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleWindow]
  end;
end;

var
  _GetConsoleProcessList: Pointer;

function GetConsoleProcessList;
begin
  GetProcedureAddress(_GetConsoleProcessList, kernel32, 'GetConsoleProcessList');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleProcessList]
  end;
end;

var
  _AddConsoleAliasA: Pointer;

function AddConsoleAliasA;
begin
  GetProcedureAddress(_AddConsoleAliasA, kernel32, 'AddConsoleAliasA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddConsoleAliasA]
  end;
end;

var
  _AddConsoleAliasW: Pointer;

function AddConsoleAliasW;
begin
  GetProcedureAddress(_AddConsoleAliasW, kernel32, 'AddConsoleAliasW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddConsoleAliasW]
  end;
end;

var
  _AddConsoleAlias: Pointer;

function AddConsoleAlias;
begin
  GetProcedureAddress(_AddConsoleAlias, kernel32, 'AddConsoleAlias' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddConsoleAlias]
  end;
end;

var
  _GetConsoleAliasA: Pointer;

function GetConsoleAliasA;
begin
  GetProcedureAddress(_GetConsoleAliasA, kernel32, 'GetConsoleAliasA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasA]
  end;
end;

var
  _GetConsoleAliasW: Pointer;

function GetConsoleAliasW;
begin
  GetProcedureAddress(_GetConsoleAliasW, kernel32, 'GetConsoleAliasW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasW]
  end;
end;

var
  _GetConsoleAlias: Pointer;

function GetConsoleAlias;
begin
  GetProcedureAddress(_GetConsoleAlias, kernel32, 'GetConsoleAlias' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAlias]
  end;
end;

var
  _GetConsoleAliasesLengthA: Pointer;

function GetConsoleAliasesLengthA;
begin
  GetProcedureAddress(_GetConsoleAliasesLengthA, kernel32, 'GetConsoleAliasesLengthA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasesLengthA]
  end;
end;

var
  _GetConsoleAliasesLengthW: Pointer;

function GetConsoleAliasesLengthW;
begin
  GetProcedureAddress(_GetConsoleAliasesLengthW, kernel32, 'GetConsoleAliasesLengthW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasesLengthW]
  end;
end;

var
  _GetConsoleAliasesLength: Pointer;

function GetConsoleAliasesLength;
begin
  GetProcedureAddress(_GetConsoleAliasesLength, kernel32, 'GetConsoleAliasesLength' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasesLength]
  end;
end;

var
  _GetConsoleAliasExesLengthA: Pointer;

function GetConsoleAliasExesLengthA;
begin
  GetProcedureAddress(_GetConsoleAliasExesLengthA, kernel32, 'GetConsoleAliasExesLengthA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExesLengthA]
  end;
end;

var
  _GetConsoleAliasExesLengthW: Pointer;

function GetConsoleAliasExesLengthW;
begin
  GetProcedureAddress(_GetConsoleAliasExesLengthW, kernel32, 'GetConsoleAliasExesLengthW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExesLengthW]
  end;
end;

var
  _GetConsoleAliasExesLength: Pointer;

function GetConsoleAliasExesLength;
begin
  GetProcedureAddress(_GetConsoleAliasExesLength, kernel32, 'GetConsoleAliasExesLength' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExesLength]
  end;
end;

var
  _GetConsoleAliasesA: Pointer;

function GetConsoleAliasesA;
begin
  GetProcedureAddress(_GetConsoleAliasesA, kernel32, 'GetConsoleAliasesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasesA]
  end;
end;

var
  _GetConsoleAliasesW: Pointer;

function GetConsoleAliasesW;
begin
  GetProcedureAddress(_GetConsoleAliasesW, kernel32, 'GetConsoleAliasesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasesW]
  end;
end;

var
  _GetConsoleAliases: Pointer;

function GetConsoleAliases;
begin
  GetProcedureAddress(_GetConsoleAliases, kernel32, 'GetConsoleAliases' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliases]
  end;
end;

var
  _GetConsoleAliasExesA: Pointer;

function GetConsoleAliasExesA;
begin
  GetProcedureAddress(_GetConsoleAliasExesA, kernel32, 'GetConsoleAliasExesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExesA]
  end;
end;

var
  _GetConsoleAliasExesW: Pointer;

function GetConsoleAliasExesW;
begin
  GetProcedureAddress(_GetConsoleAliasExesW, kernel32, 'GetConsoleAliasExesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExesW]
  end;
end;

var
  _GetConsoleAliasExes: Pointer;

function GetConsoleAliasExes;
begin
  GetProcedureAddress(_GetConsoleAliasExes, kernel32, 'GetConsoleAliasExes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetConsoleAliasExes]
  end;
end;

{$ELSE}

function PeekConsoleInputA; external kernel32 name 'PeekConsoleInputA';
function PeekConsoleInputW; external kernel32 name 'PeekConsoleInputW';
function PeekConsoleInput; external kernel32 name 'PeekConsoleInput' + AWSuffix;
function ReadConsoleInputA; external kernel32 name 'ReadConsoleInputA';
function ReadConsoleInputW; external kernel32 name 'ReadConsoleInputW';
function ReadConsoleInput; external kernel32 name 'ReadConsoleInput' + AWSuffix;
function WriteConsoleInputA; external kernel32 name 'WriteConsoleInputA';
function WriteConsoleInputW; external kernel32 name 'WriteConsoleInputW';
function WriteConsoleInput; external kernel32 name 'WriteConsoleInput' + AWSuffix;
function ReadConsoleOutputA; external kernel32 name 'ReadConsoleOutputA';
function ReadConsoleOutputW; external kernel32 name 'ReadConsoleOutputW';
function ReadConsoleOutput; external kernel32 name 'ReadConsoleOutput' + AWSuffix;
function WriteConsoleOutputA; external kernel32 name 'WriteConsoleOutputA';
function WriteConsoleOutputW; external kernel32 name 'WriteConsoleOutputW';
function WriteConsoleOutput; external kernel32 name 'WriteConsoleOutput' + AWSuffix;
function ReadConsoleOutputCharacterA; external kernel32 name 'ReadConsoleOutputCharacterA';
function ReadConsoleOutputCharacterW; external kernel32 name 'ReadConsoleOutputCharacterW';
function ReadConsoleOutputCharacter; external kernel32 name 'ReadConsoleOutputCharacter' + AWSuffix;
function ReadConsoleOutputAttribute; external kernel32 name 'ReadConsoleOutputAttribute';
function WriteConsoleOutputCharacterA; external kernel32 name 'WriteConsoleOutputCharacterA';
function WriteConsoleOutputCharacterW; external kernel32 name 'WriteConsoleOutputCharacterW';
function WriteConsoleOutputCharacter; external kernel32 name 'WriteConsoleOutputCharacter' + AWSuffix;
function WriteConsoleOutputAttribute; external kernel32 name 'WriteConsoleOutputAttribute';
function FillConsoleOutputCharacterA; external kernel32 name 'FillConsoleOutputCharacterA';
function FillConsoleOutputCharacterW; external kernel32 name 'FillConsoleOutputCharacterW';
function FillConsoleOutputCharacter; external kernel32 name 'FillConsoleOutputCharacter' + AWSuffix;
function FillConsoleOutputAttribute; external kernel32 name 'FillConsoleOutputAttribute';
function GetConsoleMode; external kernel32 name 'GetConsoleMode';
function GetNumberOfConsoleInputEvents; external kernel32 name 'GetNumberOfConsoleInputEvents';
function GetConsoleScreenBufferInfo; external kernel32 name 'GetConsoleScreenBufferInfo';
function GetLargestConsoleWindowSize; external kernel32 name 'GetLargestConsoleWindowSize';
function GetConsoleCursorInfo; external kernel32 name 'GetConsoleCursorInfo';
function GetCurrentConsoleFont; external kernel32 name 'GetCurrentConsoleFont';
function GetConsoleFontSize; external kernel32 name 'GetConsoleFontSize';
function GetConsoleSelectionInfo; external kernel32 name 'GetConsoleSelectionInfo';
function GetNumberOfConsoleMouseButtons; external kernel32 name 'GetNumberOfConsoleMouseButtons';
function SetConsoleMode; external kernel32 name 'SetConsoleMode';
function SetConsoleActiveScreenBuffer; external kernel32 name 'SetConsoleActiveScreenBuffer';
function FlushConsoleInputBuffer; external kernel32 name 'FlushConsoleInputBuffer';
function SetConsoleScreenBufferSize; external kernel32 name 'SetConsoleScreenBufferSize';
function SetConsoleCursorPosition; external kernel32 name 'SetConsoleCursorPosition';
function SetConsoleCursorInfo; external kernel32 name 'SetConsoleCursorInfo';
function ScrollConsoleScreenBufferA; external kernel32 name 'ScrollConsoleScreenBufferA';
function ScrollConsoleScreenBufferW; external kernel32 name 'ScrollConsoleScreenBufferW';
function ScrollConsoleScreenBuffer; external kernel32 name 'ScrollConsoleScreenBuffer' + AWSuffix;
function SetConsoleWindowInfo; external kernel32 name 'SetConsoleWindowInfo';
function SetConsoleTextAttribute; external kernel32 name 'SetConsoleTextAttribute';
function SetConsoleCtrlHandler; external kernel32 name 'SetConsoleCtrlHandler';
function GenerateConsoleCtrlEvent; external kernel32 name 'GenerateConsoleCtrlEvent';
function AllocConsole; external kernel32 name 'AllocConsole';
function FreeConsole; external kernel32 name 'FreeConsole';
function AttachConsole; external kernel32 name 'AttachConsole';
function GetConsoleTitleA; external kernel32 name 'GetConsoleTitleA';
function GetConsoleTitleW; external kernel32 name 'GetConsoleTitleW';
function GetConsoleTitle; external kernel32 name 'GetConsoleTitle' + AWSuffix;
function SetConsoleTitleA; external kernel32 name 'SetConsoleTitleA';
function SetConsoleTitleW; external kernel32 name 'SetConsoleTitleW';
function SetConsoleTitle; external kernel32 name 'SetConsoleTitle' + AWSuffix;
function ReadConsoleA; external kernel32 name 'ReadConsoleA';
function ReadConsoleW; external kernel32 name 'ReadConsoleW';
function ReadConsole; external kernel32 name 'ReadConsole' + AWSuffix;
function WriteConsoleA; external kernel32 name 'WriteConsoleA';
function WriteConsoleW; external kernel32 name 'WriteConsoleW';
function WriteConsole; external kernel32 name 'WriteConsole' + AWSuffix;
function CreateConsoleScreenBuffer; external kernel32 name 'CreateConsoleScreenBuffer';
function GetConsoleCP; external kernel32 name 'GetConsoleCP';
function SetConsoleCP; external kernel32 name 'SetConsoleCP';
function GetConsoleOutputCP; external kernel32 name 'GetConsoleOutputCP';
function SetConsoleOutputCP; external kernel32 name 'SetConsoleOutputCP';
function GetConsoleDisplayMode; external kernel32 name 'GetConsoleDisplayMode';
function GetConsoleWindow; external kernel32 name 'GetConsoleWindow';
function GetConsoleProcessList; external kernel32 name 'GetConsoleProcessList';
function AddConsoleAliasA; external kernel32 name 'AddConsoleAliasA';
function AddConsoleAliasW; external kernel32 name 'AddConsoleAliasW';
function AddConsoleAlias; external kernel32 name 'AddConsoleAlias' + AWSuffix;
function GetConsoleAliasA; external kernel32 name 'GetConsoleAliasA';
function GetConsoleAliasW; external kernel32 name 'GetConsoleAliasW';
function GetConsoleAlias; external kernel32 name 'GetConsoleAlias' + AWSuffix;
function GetConsoleAliasesLengthA; external kernel32 name 'GetConsoleAliasesLengthA';
function GetConsoleAliasesLengthW; external kernel32 name 'GetConsoleAliasesLengthW';
function GetConsoleAliasesLength; external kernel32 name 'GetConsoleAliasesLength' + AWSuffix;
function GetConsoleAliasExesLengthA; external kernel32 name 'GetConsoleAliasExesLengthA';
function GetConsoleAliasExesLengthW; external kernel32 name 'GetConsoleAliasExesLengthW';
function GetConsoleAliasExesLength; external kernel32 name 'GetConsoleAliasExesLength' + AWSuffix;
function GetConsoleAliasesA; external kernel32 name 'GetConsoleAliasesA';
function GetConsoleAliasesW; external kernel32 name 'GetConsoleAliasesW';
function GetConsoleAliases; external kernel32 name 'GetConsoleAliases' + AWSuffix;
function GetConsoleAliasExesA; external kernel32 name 'GetConsoleAliasExesA';
function GetConsoleAliasExesW; external kernel32 name 'GetConsoleAliasExesW';
function GetConsoleAliasExes; external kernel32 name 'GetConsoleAliasExes' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
