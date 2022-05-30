(*
  Name:             hh
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 May 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
{*******************************************************}
{                                                       }
{       HTML Help API Interface Unit                    }
{                                                       }
{       Copyright (c) 1999 The Helpware Group           }
{                                                       }
{*******************************************************}

{
========================================================
  hh.pas
  Version: 1.5
  HTML Help API Unit

  htmlhelp.h ported to The Helpware Group
  Copyright (c) 1999 The Helpware Group
  Email: support@helpware.net
  Web: http://www.helpware.net
  Platform: Delphi 2, 3, 4, 5, ...

  Notes:
    htmlhelp.h is distributed with HH Workshop.
    A free download from
    http://msdn.microsoft.com/library/tools/htmlhelp/chm/HH1Start.htm

    The name hh.pas was used instead of htmlhelp.pas
    to avoid a name clash with API function htmlhelp()

  Changes Notes: See hh_doc.txt
========================================================
}

unit hh;

interface

uses
  Windows,   //This line will not compile under Delphi 1 -- D1 is not supported
  SysUtils,
  ErrorControlledRegistry;

{ >> Create conditional symbols.
     Note: This module is Delphi 2/3/4/5/.. compatible

     VER90     - Predefined by Delphi 2 compiler.
     VER100    - Predefined by Delphi 3 compiler.
     D4PLUS    - We defined this if Compiler is Delphi 4 or greater
}

{$DEFINE D4PLUS}

{$IFDEF VER90}        //Dephi 2
  {$UNDEF D4PLUS}
{$ENDIF}

{$IFDEF VER100}       //Dephi 3
  {$UNDEF D4PLUS}
{$ENDIF}



var
  //0 if hhctrl.ocx could not be loaded
  HHCtrlHandle: THandle = 0;

  //You can set this to false to override the default load API load on module initialization
  AutoLoadAPI: Boolean = TRUE;

const
  hhctrlLib  = 'hhctrl.ocx';

{exports}
  function GetPathToHHCtrlOCX: string;
  procedure LoadHtmlHelp;
  procedure UnloadHtmlHelp;


{ Externals from HHCTRL.OCX }

var                            //functions are invalid if HHCtrlHandle = 0
  HtmlHelpA: function(hwndCaller: HWND; pszFile: PAnsiChar;
    uCommand: UInt; dwData: DWORD): HWND; stdcall;

  HtmlHelpW: function(hwndCaller: HWND; pszFile: PWideChar;
    uCommand: UInt; dwData: DWORD): HWND; stdcall;

  HtmlHelp: function(hwndCaller: HWND; pszFile: PChar;
    uCommand: UInt; dwData: DWORD): HWND; stdcall;


  { Use the following for GetProcAddress to load from hhctrl.ocx }

const
  ATOM_HTMLHELP_API_ANSI    = 14;
  ATOM_HTMLHELP_API_UNICODE = 15;


  { Commands to pass to HtmlHelp() }

const
  HH_DISPLAY_TOPIC        = $0000;  {**}
  HH_HELP_FINDER          = $0000;  // WinHelp equivalent
  HH_DISPLAY_TOC          = $0001;  // not currently implemented
  HH_DISPLAY_INDEX        = $0002;  // not currently implemented
  HH_DISPLAY_SEARCH       = $0003;  // not currently implemented
  HH_SET_WIN_TYPE         = $0004;
  HH_GET_WIN_TYPE         = $0005;
  HH_GET_WIN_HANDLE       = $0006;
  HH_ENUM_INFO_TYPE       = $0007;  // Get Info type name, call repeatedly to enumerate, -1 at end
  HH_SET_INFO_TYPE        = $0008;  // Add Info type to filter.
  HH_SYNC                 = $0009;
  HH_RESERVED1            = $000A;
  HH_RESERVED2            = $000B;
  HH_RESERVED3            = $000C;
  HH_KEYWORD_LOOKUP       = $000D;
  HH_DISPLAY_TEXT_POPUP   = $000E;  // display string resource id or text in a popup window
  HH_HELP_CONTEXT         = $000F;  {**}// display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU  = $0010;  // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP      = $0011;  // text popup help, same as WinHelp HELP_WM_HELP
  HH_CLOSE_ALL            = $0012;  // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP         = $0013;  // ALink version of HH_KEYWORD_LOOKUP
  HH_GET_LAST_ERROR       = $0014;  // not currently implemented // See HHERROR.h
  HH_ENUM_CATEGORY        = $0015;	// Get category name, call repeatedly to enumerate, -1 at end
  HH_ENUM_CATEGORY_IT     = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
  HH_RESET_IT_FILTER      = $0017;  // Clear the info type filter of all info types.
  HH_SET_INCLUSIVE_FILTER = $0018;  // set inclusive filtering method for untyped topics to be included in display
  HH_SET_EXCLUSIVE_FILTER = $0019;  // set exclusive filtering method for untyped topics to be excluded from display
  HH_INITIALIZE           = $001C;  // Initializes the help system.
  HH_UNINITIALIZE         = $001D;  // Uninitializes the help system.
  HH_PRETRANSLATEMESSAGE  = $00fd;  // Pumps messages. (NULL, NULL, MSG*).
  HH_SET_GLOBAL_PROPERTY  = $00fc;  // Set a global property. (NULL, NULL, HH_GPROP)

  { window properties }

const
  HHWIN_PROP_TAB_AUTOHIDESHOW = $00000001;  // (1 << 0)  Automatically hide/show tri-pane window
  HHWIN_PROP_ONTOP            = $00000002;  // (1 << 1)  Top-most window
  HHWIN_PROP_NOTITLEBAR       = $00000004;  // (1 << 2)  no title bar
  HHWIN_PROP_NODEF_STYLES     = $00000008;  // (1 << 3)  no default window styles (only HH_WINTYPE.dwStyles)
  HHWIN_PROP_NODEF_EXSTYLES   = $00000010;  // (1 << 4)  no default extended window styles (only HH_WINTYPE.dwExStyles)
  HHWIN_PROP_TRI_PANE         = $00000020;  // (1 << 5)  use a tri-pane window
  HHWIN_PROP_NOTB_TEXT        = $00000040;  // (1 << 6)  no text on toolbar buttons
  HHWIN_PROP_POST_QUIT        = $00000080;  // (1 << 7)  post WM_QUIT message when window closes
  HHWIN_PROP_AUTO_SYNC        = $00000100;  // (1 << 8)  automatically ssync contents and index
  HHWIN_PROP_TRACKING         = $00000200;  // (1 << 9)  send tracking notification messages
  HHWIN_PROP_TAB_SEARCH       = $00000400;  // (1 << 10) include search tab in navigation pane
  HHWIN_PROP_TAB_HISTORY      = $00000800;  // (1 << 11) include history tab in navigation pane
  HHWIN_PROP_TAB_FAVORITES    = $00001000;  // (1 << 12) include favorites tab in navigation pane
  HHWIN_PROP_CHANGE_TITLE     = $00002000;  // (1 << 13) Put current HTML title in title bar
  HHWIN_PROP_NAV_ONLY_WIN     = $00004000;  // (1 << 14) Only display the navigation window
  HHWIN_PROP_NO_TOOLBAR       = $00008000;  // (1 << 15) Don't display a toolbar
  HHWIN_PROP_MENU             = $00010000;  // (1 << 16) Menu
  HHWIN_PROP_TAB_ADVSEARCH    = $00020000;  // (1 << 17) Advanced FTS UI.
  HHWIN_PROP_USER_POS         = $00040000;  // (1 << 18) After initial creation, user controls window size/position
  HHWIN_PROP_TAB_CUSTOM1      = $00080000;  // (1 << 19) Use custom tab #1
  HHWIN_PROP_TAB_CUSTOM2      = $00100000;  // (1 << 20) Use custom tab #2
  HHWIN_PROP_TAB_CUSTOM3      = $00200000;  // (1 << 21) Use custom tab #3
  HHWIN_PROP_TAB_CUSTOM4      = $00400000;  // (1 << 22) Use custom tab #4
  HHWIN_PROP_TAB_CUSTOM5      = $00800000;  // (1 << 23) Use custom tab #5
  HHWIN_PROP_TAB_CUSTOM6      = $01000000;  // (1 << 24) Use custom tab #6
  HHWIN_PROP_TAB_CUSTOM7      = $02000000;  // (1 << 25) Use custom tab #7
  HHWIN_PROP_TAB_CUSTOM8      = $04000000;  // (1 << 26) Use custom tab #8
  HHWIN_PROP_TAB_CUSTOM9      = $08000000;  // (1 << 27) Use custom tab #9
  HHWIN_TB_MARGIN             = $10000000;  // (1 << 28) the window type has a margin

  { window parameters }

const
  HHWIN_PARAM_PROPERTIES      = $00000002;  // (1 << 1)  valid fsWinProperties
  HHWIN_PARAM_STYLES          = $00000004;  // (1 << 2)  valid dwStyles
  HHWIN_PARAM_EXSTYLES        = $00000008;  // (1 << 3)  valid dwExStyles
  HHWIN_PARAM_RECT            = $00000010;  // (1 << 4)  valid rcWindowPos
  HHWIN_PARAM_NAV_WIDTH       = $00000020;  // (1 << 5)  valid iNavWidth
  HHWIN_PARAM_SHOWSTATE       = $00000040;  // (1 << 6)  valid nShowState
  HHWIN_PARAM_INFOTYPES       = $00000080;  // (1 << 7)  valid apInfoTypes
  HHWIN_PARAM_TB_FLAGS        = $00000100;  // (1 << 8)  valid fsToolBarFlags
  HHWIN_PARAM_EXPANSION       = $00000200;  // (1 << 9)  valid fNotExpanded
  HHWIN_PARAM_TABPOS          = $00000400;  // (1 << 10) valid tabpos
  HHWIN_PARAM_TABORDER        = $00000800;  // (1 << 11) valid taborder
  HHWIN_PARAM_HISTORY_COUNT   = $00001000;  // (1 << 12) valid cHistory
  HHWIN_PARAM_CUR_TAB         = $00002000;  // (1 << 13) valid curNavType

  { button constants }

const
  HHWIN_BUTTON_EXPAND         = $00000002;  // (1 << 1)  Expand/contract button
  HHWIN_BUTTON_BACK           = $00000004;  // (1 << 2)  Back button
  HHWIN_BUTTON_FORWARD        = $00000008;  // (1 << 3)  Forward button
  HHWIN_BUTTON_STOP           = $00000010;  // (1 << 4)  Stop button
  HHWIN_BUTTON_REFRESH        = $00000020;  // (1 << 5)  Refresh button
  HHWIN_BUTTON_HOME           = $00000040;  // (1 << 6)  Home button
  HHWIN_BUTTON_BROWSE_FWD     = $00000080;  // (1 << 7)  not implemented
  HHWIN_BUTTON_BROWSE_BCK     = $00000100;  // (1 << 8)  not implemented
  HHWIN_BUTTON_NOTES          = $00000200;  // (1 << 9)  not implemented
  HHWIN_BUTTON_CONTENTS       = $00000400;  // (1 << 10) not implemented
  HHWIN_BUTTON_SYNC           = $00000800;  // (1 << 11) Sync button
  HHWIN_BUTTON_OPTIONS        = $00001000;  // (1 << 12) Options button
  HHWIN_BUTTON_PRINT          = $00002000;  // (1 << 13) Print button
  HHWIN_BUTTON_INDEX          = $00004000;  // (1 << 14) not implemented
  HHWIN_BUTTON_SEARCH         = $00008000;  // (1 << 15) not implemented
  HHWIN_BUTTON_HISTORY        = $00010000;  // (1 << 16) not implemented
  HHWIN_BUTTON_FAVORITES      = $00020000;  // (1 << 17) not implemented
  HHWIN_BUTTON_JUMP1          = $00040000;  // (1 << 18)
  HHWIN_BUTTON_JUMP2          = $00080000;  // (1 << 19)
  HHWIN_BUTTON_ZOOM           = $00100000;  // (1 << 20)
  HHWIN_BUTTON_TOC_NEXT       = $00200000;  // (1 << 21)
  HHWIN_BUTTON_TOC_PREV       = $00400000;  // (1 << 22)

  HHWIN_DEF_BUTTONS           = (HHWIN_BUTTON_EXPAND
                                 OR HHWIN_BUTTON_BACK
                                 OR HHWIN_BUTTON_OPTIONS
                                 OR HHWIN_BUTTON_PRINT);


  { Button IDs }

const
  IDTB_EXPAND             = 200;
  IDTB_CONTRACT           = 201;
  IDTB_STOP               = 202;
  IDTB_REFRESH            = 203;
  IDTB_BACK               = 204;
  IDTB_HOME               = 205;
  IDTB_SYNC               = 206;
  IDTB_PRINT              = 207;
  IDTB_OPTIONS            = 208;
  IDTB_FORWARD            = 209;
  IDTB_NOTES              = 210; // not implemented
  IDTB_BROWSE_FWD         = 211;
  IDTB_BROWSE_BACK        = 212;
  IDTB_CONTENTS           = 213; // not implemented
  IDTB_INDEX              = 214; // not implemented
  IDTB_SEARCH             = 215; // not implemented
  IDTB_HISTORY            = 216; // not implemented
  IDTB_FAVORITES          = 217; // not implemented
  IDTB_JUMP1              = 218;
  IDTB_JUMP2              = 219;
  IDTB_CUSTOMIZE          = 221;
  IDTB_ZOOM               = 222;
  IDTB_TOC_NEXT           = 223;
  IDTB_TOC_PREV           = 224;


  { Notification codes }

const
  HHN_FIRST       = (0-860);
  HHN_LAST        = (0-879);

  HHN_NAVCOMPLETE   = (HHN_FIRST-0);
  HHN_TRACK         = (HHN_FIRST-1);
  HHN_WINDOW_CREATE = (HHN_FIRST-2);


type
  {*** Used by command HH_GET_LAST_ERROR
   NOTE: Not part of the htmlhelp.h but documented in HH Workshop help
         You must call SysFreeString(xx.description) to free BSTR
  }
  tagHH_LAST_ERROR = packed record
    cbStruct:      Integer;     // sizeof this structure
    hr:            Integer;     // Specifies the last error code.
    description:   PWideChar;   // (BSTR) Specifies a Unicode string containing a description of the error.
  end;
  HH_LAST_ERROR = tagHH_LAST_ERROR;
  THHLastError = tagHH_LAST_ERROR;


type
  {*** Notify event info for HHN_NAVCOMPLETE, HHN_WINDOW_CREATE }
  PHHNNotify = ^THHNNotify;
  tagHHN_NOTIFY = packed record
    hdr:    TNMHdr;
    pszUrl: PChar;              //PCSTR: Multi-byte, null-terminated string
  end;
  HHN_NOTIFY = tagHHN_NOTIFY;
  THHNNotify = tagHHN_NOTIFY;

  {** Use by command HH_DISPLAY_TEXT_POPUP}
  PHHPopup = ^THHPopup;
  tagHH_POPUP = packed record
    cbStruct:      Integer;     // sizeof this structure
    hinst:         HINST;       // instance handle for string resource
    idString:      cardinal;    // string resource id, or text id if pszFile is specified in HtmlHelp call
    pszText:       PChar;       // used if idString is zero
    pt:            TPOINT;      // top center of popup window
    clrForeground: COLORREF;    // use -1 for default
    clrBackground: COLORREF;    // use -1 for default
    rcMargins:     TRect;       // amount of space between edges of window and text, -1 for each member to ignore
    pszFont:       PChar;       // facename, point size, char set, BOLD ITALIC UNDERLINE
  end;
  HH_POPUP = tagHH_POPUP;
  THHPopup = tagHH_POPUP;

  {** Use by commands - HH_ALINK_LOOKUP, HH_KEYWORD_LOOKUP}
  PHHAKLink = ^THHAKLink;
  tagHH_AKLINK = packed record
    cbStruct:      integer;     // sizeof this structure
    fReserved:     BOOL;        // must be FALSE (really!)
    pszKeywords:   PChar;       // semi-colon separated keywords
    pszUrl:        PChar;       // URL to jump to if no keywords found (may be NULL)
    pszMsgText:    PChar;       // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    pszMsgTitle:   PChar;       // Message text to display in MessageBox if pszUrl is NULL and no keyword match
    pszWindow:     PChar;       // Window to display URL in
    fIndexOnFail:  BOOL;        // Displays index if keyword lookup fails.
  end;
  HH_AKLINK = tagHH_AKLINK;
  THHAKLink = tagHH_AKLINK;


const
  HHWIN_NAVTYPE_TOC          = 0;
  HHWIN_NAVTYPE_INDEX        = 1;
  HHWIN_NAVTYPE_SEARCH       = 2;
  HHWIN_NAVTYPE_FAVORITES    = 3;
  HHWIN_NAVTYPE_HISTORY      = 4;   // not implemented
  HHWIN_NAVTYPE_AUTHOR       = 5;
  HHWIN_NAVTYPE_CUSTOM_FIRST = 11;


const
  IT_INCLUSIVE = 0;
  IT_EXCLUSIVE = 1;
  IT_HIDDEN    = 2;

type
  PHHEnumIT = ^THHEnumIT;
  tagHH_ENUM_IT = packed record                  //tagHH_ENUM_IT, HH_ENUM_IT, *PHH_ENUM_IT
    cbStruct:           Integer;     // size of this structure
    iType:              Integer;     // the type of the information type ie. Inclusive, Exclusive, or Hidden
    pszCatName:         PAnsiChar;   // Set to the name of the Category to enumerate the info types in a category; else NULL
    pszITName:          PAnsiChar;   // volitile pointer to the name of the infotype. Allocated by call. Caller responsible for freeing
    pszITDescription:   PAnsiChar;   // volitile pointer to the description of the infotype.
  end;
  THHEnumIT = tagHH_ENUM_IT;


type
  PHHEnumCat = ^THHEnumCat;
  tagHH_ENUM_CAT = packed record                 //tagHH_ENUM_CAT, HH_ENUM_CAT, *PHH_ENUM_CAT
    cbStruct:           Integer;     // size of this structure
    pszCatName:         PAnsiChar;   // volitile pointer to the category name
    pszCatDescription:  PAnsiChar;   // volitile pointer to the category description
  end;
  THHEnumCat = tagHH_ENUM_CAT;


type
  PHHSetInfoType = ^THHSetInfoType;
  tagHH_SET_INFOTYPE = packed record             //tagHH_SET_INFOTYPE, HH_SET_INFOTYPE, *PHH_SET_INFOTYPE
    cbStruct:           Integer;     // the size of this structure
    pszCatName:         PAnsiChar;   // the name of the category, if any, the InfoType is a member of.
    pszInfoTypeName:    PAnsiChar;   // the name of the info type to add to the filter
  end;
  THHSetInfoType = tagHH_SET_INFOTYPE;


type
  HH_INFOTYPE = DWORD;
  THHInfoType = HH_INFOTYPE;
  PHHInfoType = ^THHInfoType;        //PHH_INFOTYPE


const
  HHWIN_NAVTAB_TOP    = 0;
  HHWIN_NAVTAB_LEFT   = 1;
  HHWIN_NAVTAB_BOTTOM = 2;

const
  HH_MAX_TABS  = 19;                 // maximum number of tabs
const
  HH_TAB_CONTENTS     = 0;
  HH_TAB_INDEX        = 1;
  HH_TAB_SEARCH       = 2;
  HH_TAB_FAVORITES    = 3;
  HH_TAB_HISTORY      = 4;
  HH_TAB_AUTHOR       = 5;
  HH_TAB_CUSTOM_FIRST = 11;
  HH_TAB_CUSTOM_LAST  = HH_MAX_TABS;

  HH_MAX_TABS_CUSTOM = (HH_TAB_CUSTOM_LAST - HH_TAB_CUSTOM_FIRST + 1);



  { HH_DISPLAY_SEARCH Command Related Structures and Constants }

const
  HH_FTS_DEFAULT_PROXIMITY = (-1);

type
  {** Used by command HH_DISPLAY_SEARCH}
  PHHFtsQuery = ^THHFtsQuery;
  tagHH_FTS_QUERY = packed record          //tagHH_FTS_QUERY, HH_FTS_QUERY
    cbStruct:          integer;      // Sizeof structure in bytes.
    fUniCodeStrings:   BOOL;         // TRUE if all strings are unicode.
    pszSearchQuery:    PChar;        // String containing the search query.
    iProximity:        LongInt;      // Word proximity.
    fStemmedSearch:    Bool;         // TRUE for StemmedSearch only.
    fTitleOnly:        Bool;         // TRUE for Title search only.
    fExecute:          Bool;         // TRUE to initiate the search.
    pszWindow:         PChar;        // Window to display in
  end;
  THHFtsQuery = tagHH_FTS_QUERY;


  { HH_WINTYPE Structure }

type
  {** Used by commands HH_GET_WIN_TYPE, HH_SET_WIN_TYPE}
  PHHWinType = ^THHWinType;
  tagHH_WINTYPE = packed record             //tagHH_WINTYPE, HH_WINTYPE, *PHH_WINTYPE;
    cbStruct:          Integer;      // IN: size of this structure including all Information Types
    fUniCodeStrings:   BOOL;         // IN/OUT: TRUE if all strings are in UNICODE
    pszType:           PChar;        // IN/OUT: Name of a type of window
    fsValidMembers:    DWORD;        // IN: Bit flag of valid members (HHWIN_PARAM_)
    fsWinProperties:   DWORD;        // IN/OUT: Properties/attributes of the window (HHWIN_)

    pszCaption:        PChar;        // IN/OUT: Window title
    dwStyles:          DWORD;        // IN/OUT: Window styles
    dwExStyles:        DWORD;        // IN/OUT: Extended Window styles
    rcWindowPos:       TRect;        // IN: Starting position, OUT: current position
    nShowState:        Integer;      // IN: show state (e.g., SW_SHOW)

    hwndHelp:          HWND;         // OUT: window handle
    hwndCaller:        HWND;         // OUT: who called this window

    paInfoTypes:       PHHInfoType;  // IN: Pointer to an array of Information Types

    { The following members are only valid if HHWIN_PROP_TRI_PANE is set }

    hwndToolBar:       HWND;         // OUT: toolbar window in tri-pane window
    hwndNavigation:    HWND;         // OUT: navigation window in tri-pane window
    hwndHTML:          HWND;         // OUT: window displaying HTML in tri-pane window
    iNavWidth:         Integer;      // IN/OUT: width of navigation window
    rcHTML:            TRect;        // OUT: HTML window coordinates

    pszToc:            PChar;        // IN: Location of the table of contents file
    pszIndex:          PChar;        // IN: Location of the index file
    pszFile:           PChar;        // IN: Default location of the html file
    pszHome:           PChar;        // IN/OUT: html file to display when Home button is clicked
    fsToolBarFlags:    DWORD;        // IN: flags controling the appearance of the toolbar (HHWIN_BUTTON_)
    fNotExpanded:      BOOL;         // IN: TRUE/FALSE to contract or expand, OUT: current state
    curNavType:        Integer;      // IN/OUT: UI to display in the navigational pane
    tabpos:            Integer;      // IN/OUT: HHWIN_NAVTAB_TOP, HHWIN_NAVTAB_LEFT, or HHWIN_NAVTAB_BOTTOM
    idNotify:          Integer;      // IN: ID to use for WM_NOTIFY messages
    tabOrder: packed array[0..HH_MAX_TABS] of Byte;  // IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs
    cHistory:          Integer;       // IN/OUT: number of history items to keep (default is 30)
    pszJump1:          PChar;         // Text for HHWIN_BUTTON_JUMP1
    pszJump2:          PChar;         // Text for HHWIN_BUTTON_JUMP2
    pszUrlJump1:       PChar;         // URL for HHWIN_BUTTON_JUMP1
    pszUrlJump2:       PChar;         // URL for HHWIN_BUTTON_JUMP2
    rcMinSize:         TRect;         // Minimum size for window (ignored in version 1)

    cbInfoTypes:       Integer;       // size of paInfoTypes;
    pszCustomTabs:     PChar;         // multiple zero-terminated strings
  end;
  HH_WINTYPE = tagHH_WINTYPE;
  THHWinType = tagHH_WINTYPE;

const
  HHACT_TAB_CONTENTS   = 0;
  HHACT_TAB_INDEX      = 1;
  HHACT_TAB_SEARCH     = 2;
  HHACT_TAB_HISTORY    = 3;
  HHACT_TAB_FAVORITES  = 4;

  HHACT_EXPAND         = 5;
  HHACT_CONTRACT       = 6;
  HHACT_BACK           = 7;
  HHACT_FORWARD        = 8;
  HHACT_STOP           = 9;
  HHACT_REFRESH        = 10;
  HHACT_HOME           = 11;
  HHACT_SYNC           = 12;
  HHACT_OPTIONS        = 13;
  HHACT_PRINT          = 14;
  HHACT_HIGHLIGHT      = 15;
  HHACT_CUSTOMIZE      = 16;
  HHACT_JUMP1          = 17;
  HHACT_JUMP2          = 18;
  HHACT_ZOOM           = 19;
  HHACT_TOC_NEXT       = 20;
  HHACT_TOC_PREV       = 21;
  HHACT_NOTES          = 22;

  HHACT_LAST_ENUM      = 23;


type
  {*** Notify event info for HHN_TRACK }
  PHHNTrack = ^THHNTrack;
  tagHHNTRACK = packed record                  //tagHHNTRACK, HHNTRACK;
    hdr:               TNMHdr;
    pszCurUrl:         PChar;                  // Multi-byte, null-terminated string  
    idAction:          Integer;                // HHACT_ value
    phhWinType:        PHHWinType;             // Current window type structure
  end;
  HHNTRACK = tagHHNTRACK;
  THHNTrack = tagHHNTRACK;


///////////////////////////////////////////////////////////////////////////////
//
// Global Control Properties.
//
const
  HH_GPROPID_SINGLETHREAD     = 1;      // VARIANT_BOOL: True for single thread
  HH_GPROPID_TOOLBAR_MARGIN   = 2;      // long: Provides a left/right margin around the toolbar.
  HH_GPROPID_UI_LANGUAGE      = 3;      // long: LangId of the UI.
  HH_GPROPID_CURRENT_SUBSET   = 4;      // BSTR: Current subset.
  HH_GPROPID_CONTENT_LANGUAGE = 5;      // long: LandId for desired content.

type
  tagHH_GPROPID = HH_GPROPID_SINGLETHREAD..HH_GPROPID_CONTENT_LANGUAGE;                //tagHH_GPROPID, HH_GPROPID
  HH_GPROPID = tagHH_GPROPID;
  THHGPropID = HH_GPROPID;

///////////////////////////////////////////////////////////////////////////////
//
// Global Property structure
//
type
  PHHGlobalProperty = ^THHGlobalProperty;
  tagHH_GLOBAL_PROPERTY = record                  //tagHH_GLOBAL_PROPERTY, HH_GLOBAL_PROPERTY
    id:                THHGPropID;
    Dummy:             Integer;                  // Added to enforce 8-byte packing
    var_:              VARIANT;
  end;
  HH_GLOBAL_PROPERTY = tagHH_GLOBAL_PROPERTY;
  THHGlobalProperty = tagHH_GLOBAL_PROPERTY;



implementation


const hhPathRegKey = 'CLSID\{adb880a6-d8ff-11cf-9377-00aa003b7a11}\InprocServer32';

{ Returns full path to hhctrl.ocx.
  Returns empty string if file or registry entry not found.
  Note: hhctrl.ocx may not be in the path. Consider the case where
        the ocx has been downloaded to the windows ocx cache via the net.
        So.. best to get the path from the registry.}

function GetPathToHHCtrlOCX: string;
var Reg: TRegistryErrorControlled;  // I2890
begin
  result := '';  //default return
  Reg := TRegistryErrorControlled.Create;  // I2890
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if Reg.OpenKeyReadOnly(hhPathRegKey) then
  begin
    result := Reg.ReadString('');  //default value
    Reg.CloseKey;
    if (result <> '') and (not FileExists(result)) then  //final check - file must exist
      result := '';
  end;
  Reg.Free;
end;


{setup HTML Help API function interface
 sets HHCtrlHandle = 0 if API function not available }
procedure LoadHtmlHelp;
var OcxPath: string;
begin
  if HHCtrlHandle = 0 then
  begin
    OcxPath := GetPathToHHCtrlOCX;
    if (OcxPath <> '') and FileExists(OcxPath) then
    begin
      HHCtrlHandle := LoadLibrary(PChar(OcxPath));
      if HHCtrlHandle <> 0 then
      begin
        @HtmlHelpA := GetProcAddress(HHCtrlHandle, 'HtmlHelpA');
        @HtmlHelpW := GetProcAddress(HHCtrlHandle, 'HtmlHelpW');
        @HtmlHelp := GetProcAddress(HHCtrlHandle, 'HtmlHelpA');
      end;
    end;
  end;
end;

procedure UnloadHtmlHelp;
begin
  if HHCtrlHandle <> 0 then
  begin
    FreeLibrary(HHCtrlHandle);
    @HtmlHelpA := nil;
    @HtmlHelpW := nil;
    @HtmlHelp := nil;
    HHCtrlHandle := 0;
  end;
end;

//itialization

end.



