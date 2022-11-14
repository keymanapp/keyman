(*
  Name:             custinterfaces
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    4 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add IKeymanCustomisationOptions properties and IKeymanCustomisationMenuItem properties
                    14 Sep 2006 - mcdurdin - Add OSK actions and locations to menu items
                    14 Sep 2006 - mcdurdin - Add SupportsCharacterMap property
                    14 Sep 2006 - mcdurdin - Add SplashFileName and Welcome* size properties
                    04 Dec 2006 - mcdurdin - Add localization, remove welcome page size
                    12 Dec 2006 - mcdurdin - Add GetDialogParameters functoin
                    04 Jan 2007 - mcdurdin - Add miaOpenKeyboardHelp
                    23 Aug 2007 - mcdurdin - I956 - Add GetLocalePathForLocale function
                    27 Mar 2008 - mcdurdin - I1373 - View keyboard usage
                    27 Mar 2008 - mcdurdin - I1374 - View font helper
                    27 Mar 2008 - mcdurdin - I1248 - Tutorial
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    12 Mar 2010 - mcdurdin - I2226 - Add Text Editor to menu
                    12 Mar 2010 - mcdurdin - I2228 - Add column break to menu
                    11 Jan 2011 - mcdurdin - I2633 - Wrong icons in OSK toolbar (more keyboards icon)
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
                    04 Mar 2015 - mcdurdin - I4606 - V9.0 - Support single keyboard buttons on OSK toolbar
*)
unit custinterfaces;

interface

uses
  ActiveX, keymanapi_TLB;

type
  TCustomisationMenuItemAction = TOLEEnum;

const
  miaOpenKeymanConfiguration = $00000001;
  miaOpenProductHelp         = $00000002;
  miaOpenProductAbout        = $00000003;
  miaToggleVisualKeyboard    = $00000004;
  miaExitProduct             = $00000005;
  miaRunProgram              = $00000006;
  _miaSwitchKeymanOff         = $00000007;   // I3614
  miaNull                    = $00000008;

  miaOSK_Fade                = $00000009;
  miaOSK_ToggleToolbar       = $0000000A;
  miaOSK_SaveAsWebPage       = $0000000B;
  miaOSK_Print               = $0000000C;

  miaOSK_ViewKeyboard        = $0000000D;
  miaOSK_ViewCharMap         = $0000000E;
  miaOSK_ViewEntryHelper     = $0000000F;
  miaOSK_ViewKeyboardUsage   = $00000010;

  miaOSK_Close               = $00000011;

  miaOpenKeyboardHelp        = $00000012;
  miaOSK_ViewFontHelper      = $00000013;
  miaOpenTextEditor          = $00000014;

  miaSelectKeyboard          = $00000015;   // I4606
  //miaOSK_MoreKeyboards       = $00000015;  // I2633   // I3614

  mia_Max = $00000015;

type
  TCustomisationMenuItemType = TOLEEnum;

const
  mitText          = $00000001;
  mitGraphic       = $00000002;
  mitSeparator     = $00000003;
  _mitKeyboardsList = $00000004;   // I3614
  mitBreak         = $00000005;

  mit_Max = $00000005;

type
  TCustomisationMenuItemLocation = TOLEEnum;

const
  milLeft           = $00000001;
  milRight          = $00000002;
  milOSKToolbar     = $00000003;
  milOSKContextMenu = $00000004;

  mil_Max =  $00000004;

type
  IKeymanCustomisationMessages = interface
    ['{E2297A1A-F559-4804-87A5-6CAC1B314C3C}']
    function MessageFromID(const ID: WideString): WideString; overload; safecall;
    function MessageFromID(const ID, LanguageCode: WideString): WideString; overload; safecall;
    function GetLocalePath: WideString; safecall;
    function GetAvailableLanguages: WideString; safecall;
    function GetLanguageCode: WideString; safecall;
    procedure GetDialogParameters(const DialogName: WideString; var FWidth, FHeight: Integer); stdcall;
    procedure SetLanguageCode(const Value: WideString); safecall;
    function GetLocalePathForLocale(const Locale: WideString): WideString; safecall;
    procedure Load; safecall;
    procedure Refresh; safecall;

    property LanguageCode: WideString read GetLanguageCode write SetLanguageCode;
  end;

  IKeymanCustomisationMenuItem = interface
    ['{89289C95-04AD-4A1B-8791-061782DD43F3}']
    function Get_Location: TCustomisationMenuItemLocation; safecall;
    function Get_ItemType: TCustomisationMenuItemType; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Action: TCustomisationMenuItemAction; safecall;
    function Get_CmdLine: WideString; safecall;
    function Get_Icon: IPictureDisp; safecall;
    function Get_PictureDefault: IPictureDisp; safecall;
    function Get_PictureSelected: IPictureDisp; safecall;
    function Get_Hotkey: Word; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_Index: Integer; safecall;

    property Location: TCustomisationMenuItemLocation read Get_Location;
    property ItemType: TCustomisationMenuItemType read Get_ItemType;
    property Caption: WideString read Get_Caption;
    property Action: TCustomisationMenuItemAction read Get_Action;
    property CmdLine: WideString read Get_CmdLine;
    property Icon: IPictureDisp read Get_Icon;
    property Index: Integer read Get_Index;
    property PictureDefault: IPictureDisp read Get_PictureDefault;
    property PictureSelected: IPictureDisp read Get_PictureSelected;
    property Hotkey: Word read Get_Hotkey;
    property Font: IFontDisp read Get_Font;
  end;

  IKeymanCustomisationMenuItems = interface
    ['{047D772B-5B6F-4718-8BBB-C65436F6FDB2}']
    function Get_Item(const Index: Integer): IKeymanCustomisationMenuItem; safecall;
    function Get_Count: Integer; safecall;
    property Items[const Index: Integer]: IKeymanCustomisationMenuItem read Get_Item;
    property Count: Integer read Get_Count;
  end;

  IKeymanCustomisation = interface
    ['{22D9F032-2362-4EF9-B8B4-F821585EE7A9}']
    function Get_CustMessages: IKeymanCustomisationMessages; safecall;
    function Get_CustMenuItems: IKeymanCustomisationMenuItems; safecall;
    function Get_CustFile(const FileName: WideString): IStream; safecall;
    property CustMessages: IKeymanCustomisationMessages read Get_CustMessages;
    property CustMenuItems: IKeymanCustomisationMenuItems read Get_CustMenuItems;
    property CustFile[const FileName: WideString]: IStream read Get_CustFile;
    procedure Refresh; safecall;
  end;

  IKeymanCustomisationAccess = interface
    ['{CE1EEB3F-3CF3-46C0-B7AF-C64040F6CFD9}']
    function KeymanCustomisation: IKeymanCustomisation; safecall;
  end;

const
  CustomisationMenuItemActionNames: array [1 .. mia_Max] of string = (
    'Open Product Configuration', 'Open Product Help', 'Open Product About', 'Toggle Visual Keyboard',
    'Exit Product', 'Run Program', 'Switch Keyman Off', 'No action',
    'On Screen Keyboard - Toggle Fade',
    'On Screen Keyboard - Toggle Toolbar',
    'On Screen Keyboard - Save as Web Page',
    'On Screen Keyboard - Print',
    'On Screen Keyboard - View Keyboard',
    'On Screen Keyboard - View Char Map',
    'On Screen Keyboard - View Entry Helper',
    'On Screen Keyboard - View Welcome',
    'On Screen Keyboard - Close',
    'Open Keyboard Help',
    'On Screen Keyboard - View Font Helper',
    'Open Text Editor',
    'On Screen Keyboard - Keyboard by name'); // I2633   // I4606

  CustomisationMenuItemTypeNames: array [1 .. mit_Max] of string = (
    'Text', 'Graphic', 'Separator', 'Keyboards List', 'Break');

  CustomisationMenuItemLocationNames: array [1 .. mil_Max] of WideString = (
    'Tool Tray Left Button Menu', 'Tool Tray Right Button Menu',
    'On Screen Keyboard Toolbar', 'On Screen Keyboard Context Menu');

  CustomisationMenuItemLocationIsToolbar: array [1 .. mil_Max] of Boolean = (
    False, False, True, False);

implementation

end.
