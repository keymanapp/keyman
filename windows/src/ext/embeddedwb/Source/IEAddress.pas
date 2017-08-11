{*******************************************************}
{                  IEAddress Component                  }
{                       STAGE 3                         }
{                    For Delphi 5 to XE                 }
{                Freeware Component                     }
{                                                       }
{     CONTRIBUTORS:                                     }
{      Eran Bodankin (bsalsa)                           }
{      Per Lindsø Larsen                                }
{      Peter Morris                                     }
{      Thomas Stutz                                     }
{                                                       }
{                       Enjoy!                          }
{   UPDATES:                                            }
{               http://www.bsalsa.com                   }
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}

{ Remove the dot from the define below to enable support for TFlatComboBox.
 (FlatStyle Components, All Components look like MS Money und MS Encarta)
  http://www.torry.net/vcl/packs/interfacemiddle/flatstyl.zip}

{.$DEFINE USE_TFlatComboBox}

unit IEAddress;

interface

{$I EWB.inc}

uses
  ActiveX, ComCtrls, ShlObj, Windows, Messages, Classes, Controls, StdCtrls,
  Graphics, EmbeddedWB, Dialogs{$IFDEF USE_TFlatComboBox}, TFlatComboBoxUnit{$ENDIF}; //By Smot

type
  IObjMgr = interface(IUnknown)
    ['{00BB2761-6A77-11D0-A535-00C04FD7D062}']
    function Append(punk: IUnknown): HResult; stdcall;
    function Remove(punk: IUnknown): HResult; stdcall;
  end;

  IACList = interface(IUnknown)
    ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand: POLESTR): HResult; stdcall;
  end;

  IACList2 = interface(IACList)
    ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(var pdwFlag: DWORD): HResult; stdcall;
  end;
  IAutoComplete = interface(IUnknown)
    ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: HWND; const punkACL: IUnknown; pwszRegKeyPath,
      pwszQuickComplete: POLESTR): HResult; stdcall;
    function Enable(fEnable: BOOL): HResult; stdcall;
  end;
type
  IAutoComplete2 = interface(IAutoComplete)
    ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings: TStringList;
    FCurrIndex: integer;
  public
    {IEnumString}
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
    {VCL}
    constructor Create;
    destructor Destroy; override;
  end;

  TAutoComplete2Option = (acoNone, acoAutoSuggest, acoAutoAppend, acoSearch, acoFilterPrefix,
    acoUseTab, acoUpDownKeyDropList, acoRtlReading, acoWordFilter,
    acoNoPrefixFiltering);
  TAutoComplete2Options = set of TAutoComplete2Option;
  TACSource = (acsList, acsHistory, acsMRU, acsShell);
  TAutoComplete2Source = set of TACSource;
  TAComp = (AutoComplete1, AutoComplete2);
  TAutoCompleteOption = (acAutoSuggest, acAutoAppend, acFileSystem, acUrlHistory,
    acUrlMRU, acUrlAll, acUseTab);
  TAutoCompleteOptions = set of TAutoCompleteOption;
  TIEAddressState = set of (csButtonPressed, csMouseCaptured);
  TThemes = (tmNone, tmXP, tmSilver, tmSoil);
  TGUI = (gsCombobox, gsThemes);
  TTypedUrlsMethod = (tuCommon, tuExtended);
  TTextAfterNav = (anLocationUrl, anLocationName);
  TTextOnLoad = (tlIELastVisited, tlIEHomePage, tlBlank, tlUserDefine);
  TOnUrlSelectedEvent = procedure(Sender: TObject; Url: WideString; var Cancel: boolean) of object;
  TOnGetFaviconEvent = procedure(Sender: TObject; Favicon, SiteUrl: WideString; var Cancel: boolean; Icon: TIcon) of object;
  TOnGetAppIconEvent = procedure(Sender: TObject; var Cancel: boolean; Icon: TIcon) of object;
  TOnGetIconEvent = procedure(Sender: TObject; Ext: string; hIco: hIcon) of object;
  TOnUpdateRegistryEvent = procedure(Sender: TObject; Url: WideString; var Cancel: boolean) of object;
  TOnUpdateTextEvent = procedure(Sender: TObject; OldUrl, NewUrl: WideString; var Cancel: boolean) of object;
  TOnPaintEvent = procedure(Sender: TObject; var Continue: boolean) of object;

type
{$IFDEF USE_TFlatComboBox} //By Smot
  TComboBoxType = TFlatComboBox;
{$ELSE}
  TComboBoxType = TCustomComboBox;
{$ENDIF}

type
  TCustomIEAddress = class(TComboBoxType)
  private
    dllVer: Extended;
    FAbout: string;
    FArrowColor: TColor;
    FBorderColor: TColor;
    FButtonColor: TColor;
    FButtonPressedColor: TColor;
    FButtonWidth: Integer;
    FCanvas: TControlCanvas;
    FCustomProperty: string;
    FEditState: TIEAddressState;
    FEmbeddedWB: TEmbeddedWB;
    FFlat: Boolean;
    FGUI: TGUI;
    FHasBorder: Boolean;
    FHasDropDown: Boolean;
    FHintColor: TColor;
    FIconLeft: Integer;
    FIconTop: Integer;
    FImageIndex: Integer;
    FImageList: TImageList;
    FImageSize: Integer;
    FModified: Boolean;
    FMouseActive: Boolean;
    FNavOnDblClk: Boolean;
    FNavOnEnterKey: Boolean;
    FNavOnLoad: Boolean;
    FNavOnSelected: Boolean;
    FOldBGColor: TColor;
    FOldHintColor: TColor;
    FOnGetAppIcon: TOnGetAppIconEvent;
    FOnGetFavicon: TOnGetFaviconEvent;
    FOnGetIcon: TOnGetIconEvent;
    FOnPaint: TOnPaintEvent;
    FOnUpdateRegistry: TOnUpdateRegistryEvent;
    FOnUpdateText: TOnUpdateTextEvent;
    FOnUrlSelected: TOnUrlSelectedEvent;
    FSecureSiteBG: TColor;
    FSelImageIndex: Integer;
    FShowFavicons: Boolean;
    FShowSiteHint: Boolean;
    FTextAfterNav: TTextAfterNav;
    FTextOnLoad: TTextOnLoad;
    FTextOnShow: WideString;
    FThemes: TThemes;
    FTypedUrlsMethod: TTypedUrlsMethod;
    FUAfterNav: Boolean;
    FUpdateRegistry: Boolean;
    FUseAppIcon: Boolean;
    FUseSecureSiteBGColor: Boolean;
    FAutoComplete2Source: TAutoComplete2Source;
    FAutoComplete: IAutoComplete;
    FAutoCompleteOptions: TAutoCompleteOptions;
    FAutoComplete2Options: TAutoComplete2Options;
    FAComp: TAComp;
    function AddFaviconToImageList: integer;
    function GetImageIndex(aUrl: string; IntoLV: Boolean): Integer;
    function GrabFavicon(URL: string; dest: string): Boolean;
    function RemovePrefix(UrlIn, Prefix: WideString): WideString;
    function FixUrl(Url: string): string;
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    procedure GetTypedURLs;
    procedure InsertTextToList;
    procedure RegistryUpdate;
    procedure RepaintIEAddress(MouseActive: Boolean);
    procedure SetAbout(Value: string);
    procedure SetDropDown(const Value: Boolean);
    procedure SetTextPosition;
    procedure SetFlat(const Value: Boolean);
    procedure SetHasBorder(const Value: Boolean);
    procedure SetSiteHint;
    procedure SetTextOnLd;
    procedure SetTheme;
    procedure SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
    procedure SetAutoComplete2Source(const Value: TAutoComplete2Source);
    procedure SetACOptions(const Value: TAutoComplete2Options);
    procedure TextUpdate;
    procedure UpdateIAutoComplete2;
    procedure UpdateAutoComplete;
  protected
    procedure CalculateRGN;
    procedure Change; override;
    procedure CheckButtonState(X, Y: Integer);
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyWnd; override;
    procedure DrawIEAddress(MouseInControl, DroppedDown: boolean);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsValidURL(const URL: WideString): Boolean;
    function GetDllVersion(const ADll: string): Extended;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure CheckSecureSite;
    procedure ClearList;
    procedure AddToList;
{$IFDEF DELPHI6_UP}
    procedure DeleteSelected; override;
{$ENDIF}
    procedure LoadFromFile(FileName: WideString);
    procedure SaveToFile(FileName: WideString);
    procedure SetBounds(Left, Top, Width, Height: Integer); override;
   {published}
    property AutoCompleteVersion: TAComp read FAComp write FAComp default AutoComplete2;
    property AutoComplete2Source: TAutoComplete2Source read FAutoComplete2Source
      write SetAutoComplete2Source default [acsList, acsHistory, acsMRU, acsShell];
    property AutoComplete2Options: TAutoComplete2Options read FAutoComplete2Options
      write FAutoComplete2Options default [acoAutoSuggest, acoAutoAppend, acoSearch];
    property AutoCompleteOptions: TAutoCompleteOptions read FAutoCompleteOptions
      write SetAutoCompleteOptions default [acAutoAppend, acUrlHistory];
    property About: string read FAbout write SetAbout;
    property ArrowColor: Tcolor read FArrowColor write FArrowColor default clblack;
    property AutoNavigateOnDblClk: Boolean read FNavOnDblClk write FNavOnDblClk default True;
    property AutoNavigateOnEnterKey: Boolean read FNavOnEnterKey write FNavOnEnterKey default True;
    property AutoNavigateOnLoad: Boolean read FNavOnLoad write FNavOnLoad default True;
    property AutoNavigateOnSelected: Boolean read FNavOnSelected write FNavOnSelected default True;
    property BorderColor: Tcolor read FBorderColor write FBorderColor default clblack;
    property ButtonColor: Tcolor read FButtonColor write FButtonColor default clBtnFace;
    property ButtonPressedColor: Tcolor read FButtonPressedColor write FButtonPressedColor default clBtnFace;
    property ButtonWidth: integer read FButtonWidth;
    property DropDownCount;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GUI: TGUI read FGUI write FGUI default gsThemes;
    property HasBorder: Boolean read FHasBorder write SetHasBorder default True;
    property HasDropDown: Boolean read FHasDropDown write SetDropDown default True;
    property HintColor: Tcolor read FHintColor write FHintColor default clInfoBK;
    property IconLeft: Integer read FIconLeft write FIconLeft;
    property IconTop: Integer read FIconTop write FIconTop;
    property Modified: Boolean read GetModified write SetModified;
    property OnGetAppIcon: TOnGetAppIconEvent read FOnGetAppIcon write FOnGetAppIcon;
    property OnGetFavicon: TOnGetFaviconEvent read FOnGetFavicon write FOnGetFavicon;
    property OnGetIcon: TOnGetIconEvent read FOnGetIcon write FOnGetIcon;
    property OnUpdateRegistry: TOnUpdateRegistryEvent read FOnUpdateRegistry write FOnUpdateRegistry;
    property OnUpdateText: TOnUpdateTextEvent read FOnUpdateText write FOnUpdateText;
    property OnUrlSelected: TOnUrlSelectedEvent read FOnUrlSelected write FOnUrlSelected;
    property SecureSiteBG: TColor read FSecureSiteBG write FSecureSiteBG default clInfobk;
    property ShowFavicon: Boolean read FShowFavicons write FShowFavicons default False;
    property ShowSiteToolTip: Boolean read FShowSiteHint write FShowSiteHint default True;
    property TextAfterNav: TTextAfterNav read FTextAfterNav write FTextAfterNav default anLocationUrl;
    property TextOnLoad: TTextOnLoad read FTextOnLoad write FTextOnLoad default tlIEHomepage;
    property TextOnShow: WideString read FTextOnShow write FTextOnShow;
    property Themes: TThemes read FThemes write FThemes default tmNone;
    property UpdateItemsToRegistry: Boolean read FUpdateRegistry write FUpdateRegistry default True;
    property UpdateTextAfterNav: Boolean read FUAfterNav write FUAfterNav default True;
    property UseAppIcon: Boolean read FUseAppIcon write FUseAppIcon default False;
    property UseSecureSiteBGColor: Boolean read FUseSecureSiteBGColor write FUseSecureSiteBGColor default True;
  end;

  TIEAddress = class(TCustomIEAddress)
  published
    property Style; //Apparently this must be published first (see VCL);
    //New stuff-----------------------------------------------------------------
    property AutoComplete2Source default [acsList, acsHistory, acsMRU, acsShell];
    property AutoComplete2Options default [acoAutoSuggest, acoAutoAppend, acoSearch];
    property AutoCompleteOptions default [acAutoAppend, acUrlHistory];
    property AutoNavigateOnDblClk default True;
    property AutoNavigateOnEnterKey default True;
    property AutoNavigateOnLoad default True;
    property AutoNavigateOnSelected default True;
    property AutoCompleteVersion default AutoComplete2;
    property About;
    property Align;
    property Anchors;
    property ArrowColor;
    property BiDiMode;
    property BorderColor;
    property ButtonColor;
    property ButtonPressedColor;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property EmbeddedWB;
    property Enabled;
    property Flat;
    property Font;
    property GUI;
    property HasBorder;
    property HasDropDown;
    property HelpContext;
    property Hint;
    property HintColor;
    property IconLeft;
    property IconTop;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetAppIcon;
    property OnGetFavicon;
    property OnGetIcon;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property OnUpdateRegistry;
    property OnUpdateText;
    property OnUrlSelected;
    property ParentBiDiMode default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint default False;
    property PopupMenu;
    property SecureSiteBG;
    property ShowFavicon;
    property ShowHint default True;
    property ShowSiteToolTip;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextAfterNav;
    property TextOnLoad;
    property TextOnShow;
    property Themes;
    property UpdateTextAfterNav default True;
    property UseAppIcon;
    property UseSecureSiteBGColor;
    property Visible;
    property Items; //And this must be published last
  end;

implementation

uses
  ComObj, UrlMon, ImgList, ShellAPI, Forms, SysUtils, Registry, EwbIEConst, EwbCoreTools;

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  pointer(enm) := nil;
end;

constructor TEnumString.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FCurrIndex := 0;
end;

destructor TEnumString.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TEnumString.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
  wStr: WideString;
begin
  I := 0;
  while (I < celt) and (FCurrIndex < FStrings.Count) do
  begin
    wStr := FStrings[FCurrIndex];
    TPointerList(elt)[I] := PWideChar(WideString(wStr));
        /// TPointerList(elt)[I] := CoTaskMemAlloc(2 * (Length(wStr) + 1));
    StringToWideChar(wStr, TPointerList(elt)[I], 2 * (Length(wStr) + 1));
    Inc(I);
    Inc(FCurrIndex);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := I;
  if I = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumString.Reset: HResult;
begin
  FCurrIndex := 0;
  Result := S_OK;
end;

function TEnumString.Skip(celt: Integer): HResult;
begin
  if (FCurrIndex + celt) <= FStrings.Count then
  begin
    Inc(FCurrIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FCurrIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

//Accesories--------------------------------------------------------------------

function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HRESULT; stdcall; external 'shlwapi.dll';
{$R-}

function GetExtension(Url: WideString): string;
var
  st: string;
begin
  st := LowerCase(Trim(ExtractFileExt(Url)));
  if (AnsiPos('.xml', st) <> 0) or (AnsiPos('.txt', st) <> 0) or (AnsiPos('.doc', st) <> 0)
    or (AnsiPos('.bmp', st) <> 0) or (AnsiPos('.zip', st) <> 0) or (AnsiPos('.rar', st) <> 0)
    or (AnsiPos('.jpg', st) <> 0) or (AnsiPos('.gif', st) <> 0) or (AnsiPos('.jpeg', st) <> 0) then
    Result := st
  else
    Result := '*.htm';
end;

function GetCacheFolder: WideString;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  with Reg do
  try
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False) then
        Result := ReadString('Cache') + '\';
    end;
  finally
    CloseKey;
    Free;
  end;
end;

function StrToCase(StringOf: string; CasesList: array of string): Integer;
var
  Idx: integer;
begin
  Result := -1;
  for Idx := 0 to Length(CasesList) - 1 do
  begin
    if CompareText(StringOf, CasesList[Idx]) = 0 then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

function GetSpecialFolderNo(bUrl: WideString): Cardinal;
var
  Url: string;
begin
  Result := 3000;
  Url := AnsiUpperCase(Trim(bUrl));
  case StrToCase(Url, ['DESKTOP', 'INTERNET', 'PROGRAMS', 'CONTROL PANEL', 'PRINTERS',
    'MY DOCUMENTS', 'FAVORITES', 'STARTUP', 'RECENT', 'SENDTO',
      'RECYCLE BIN', 'STARTMENU', 'DESKTOP DIRECTORY', 'MY COMPUTER',
      'NETWORK NEIGHBORHOOD', 'NETHOOD', 'FONTS', 'TEMPLATES', 'START MENU',
      'COMMON PROGRAMS', 'COMMON STARTUP', 'COMMON DESKTOP', 'APPDATA',
      'PRINTHOOD', 'APPLICATION DATA', 'TEMPORARY INTERNET FILES',
      'COOKIES', 'HISTORY', 'USERPROFILE', 'DIAL UP CONNECTIONS',
      'MY MUSIC', 'MY PICTURES', 'MY VIDEO', 'CD BURNING',
      'COMPUTERS NEAR ME', 'PROFILES']) of
    0: Result := CSIDL_DESKTOP;
    1: Result := CSIDL_INTERNET;
    2: Result := CSIDL_PROGRAMS;
    3: Result := CSIDL_CONTROLS;
    4: Result := CSIDL_PRINTERS;
    5: Result := CSIDL_PERSONAL;
    6: Result := CSIDL_FAVORITES;
    7: Result := CSIDL_STARTUP;
    8: Result := CSIDL_RECENT;
    9: Result := CSIDL_SENDTO;
    10: Result := CSIDL_BITBUCKET;
    11: Result := CSIDL_STARTMENU;
    12: Result := CSIDL_DESKTOPDIRECTORY;
    13: Result := CSIDL_DRIVES;
    14: Result := CSIDL_NETWORK;
    15: Result := CSIDL_NETHOOD;
    16: Result := CSIDL_FONTS;
    17: Result := CSIDL_TEMPLATES;
    18: Result := CSIDL_COMMON_STARTMENU;
    19: Result := CSIDL_COMMON_PROGRAMS;
    20: Result := CSIDL_COMMON_STARTUP;
    21: Result := CSIDL_COMMON_DESKTOPDIRECTORY;
    22: Result := CSIDL_APPDATA;
    23: Result := CSIDL_PRINTHOOD;
{$IFDEF DELPHI9_UP}
    24: Result := CSIDL_LOCAL_APPDATA;
    25: Result := CSIDL_INTERNET_CACHE;
    26: Result := CSIDL_COOKIES;
    27: Result := CSIDL_HISTORY;
    28: Result := CSIDL_PROFILE;
    29: Result := CSIDL_CONNECTIONS;
    30: Result := CSIDL_COMMON_MUSIC;
    31: Result := CSIDL_COMMON_PICTURES;
    32: Result := CSIDL_COMMON_VIDEO;
    33: Result := CSIDL_CDBURN_AREA;
    34: Result := CSIDL_COMPUTERSNEARME;
{$IFNDEF DELPHI16_UP}
    35: Result := CSIDL_PROFILES
{$ENDIF}
{$ENDIF}
  end;
end;

function TCustomIEAddress.GetDllVersion(const ADll: string): Extended;
type //by Fabio Lucarelli
  DLLVERSIONINFO = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformID: DWORD;
  end;
  DLLGETVERSIONPROC = function(var pdvi: DLLVERSIONINFO): HRESULT; stdcall;
var
  hinstDll: THANDLE;
  pBuffer: POINTER;
  dvi: DLLVERSIONINFO;
  pDllGetVersion: DLLGETVERSIONPROC;
begin
  hinstDll := LoadLibrary(PChar(ADll));
  if hinstDll = 0 then
    Result := 0
  else
  try
    pBuffer := GetProcAddress(hinstDll, 'DllGetVersion');
    if Assigned(pBuffer) then
    begin
      ZeroMemory(@dvi, SizeOf(dvi));
      dvi.cbSize := SizeOf(dvi);
      pDllGetVersion := DLLGETVERSIONPROC(pBuffer);
      if pDllGetVersion(dvi) = NOERROR then
      begin
        Result := (dvi.dwMajorVersion + dvi.dwMinorVersion);
      end
      else
        Result := 0;
    end
    else
      Result := 0;
  finally
    FreeLibrary(hinstDll);
  end;
end;

function TCustomIEAddress.GetModified: Boolean;
begin
  Result := FModified;
  if HandleAllocated then
    Result := SendMessage(EditHandle, EM_GETMODIFY, 0, 0) <> 0;
end;

function TCustomIEAddress.IsValidURL(const URL: WideString): Boolean;
begin
  if (URL <> EmptyStr) and (UrlMon.IsValidURL(nil, PWideChar(URL), 0) = S_OK) then
    Result := True
  else
    Result := False;
end;

function TCustomIEAddress.RemovePrefix(UrlIn, Prefix: WideString): WideString;
var
  i, j: integer;
begin
  if (UrlIn = EmptyStr) then Exit;
  i := Length(Prefix);
  j := AnsiPos(Prefix, UPPERCASE(URLIn));
  if j <> 0 then
    System.delete(UrlIn, j, i);
  Result := UrlIn;
end;

procedure TCustomIEAddress.ClearList;
begin
  if Items.Count > 0 then
    Items.Clear;
end;

{$IFDEF DELPHI6_UP}

procedure TCustomIEAddress.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;
{$ENDIF}

procedure TCustomIEAddress.AddToList;
begin
  if not (csDesigning in ComponentState) then
  begin
    if (Text = EmptyStr) then Exit;
    Text := FixUrl(Text);
    RegistryUpdate;
    CheckSecureSite;
    TextUpdate;
    InsertTextToList;
    SetSiteHint;
    AddFaviconToImageList;
  end;
end;

procedure TCustomIEAddress.CheckButtonState(X, Y: Integer);
var
  ARect: TRect;
begin
  SetRect(ARect, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  if (csButtonPressed in FEditState) and not PtInRect(ARect, Point(X, Y)) then
  begin
    Exclude(FEditState, csButtonPressed);
    RepaintIEAddress(True);
  end;
end;

function TCustomIEAddress.FixUrl(Url: string): string;
  function AnsiEndsStr(const ASubText, AText: string): Boolean;
  var
    SubTextLocation: Integer;
  begin
    SubTextLocation := Length(AText) - Length(ASubText) + 1;
    if (SubTextLocation > 0) and (ASubText <> '') and
      (ByteType(AText, SubTextLocation) <> mbTrailByte) then
      Result := AnsiStrComp((PChar(ASubText)), Pointer(@AText[SubTextLocation])) = 0
    else
      Result := False;
  end;

var
  DotPos, ipos: Integer;
begin
  Result := Url;
  if (Text <> EmptyStr) and (not AnsiEndsStr('/', Url)) then
  begin
    ipos := LastDelimiter('/', Url);
    DotPos := LastDelimiter('.', Url);
    if DotPos < ipos then
      Result := Url + '/';
  end;
end;

procedure TCustomIEAddress.InsertTextToList;
var
  i: integer;
  Found: boolean;
begin
  Found := False;
  if (Text = EmptyStr) then Exit;
  for i := 0 to Items.Count do
  begin
    if Text = Items.Strings[i] then
      Found := True;
  end;
  if Items.Count = 0 then
    Found := False;
  if not Found then
    Items.Insert(0, Text);
end;

procedure TCustomIEAddress.CheckSecureSite;

  function AnsiStartsStr(const ASubText, AText: WideString): Boolean;
  begin
    Result := AnsiSameStr(ASubText, Copy(AText, 1, Length(ASubText)));
  end;
begin
  if FUseSecureSiteBGColor and (Text = EmptyStr) then
  begin
    if AnsiStartsStr('https', Text) then
      Color := FSecureSiteBG
    else
      Color := FOldBGColor;
  end;
end;

procedure TCustomIEAddress.LoadFromFile(FileName: WideString);
begin
  Clear;
  if (FileName = EmptyStr) then Exit;
  try
    Items.LoadFromFile(FileName);
  finally
  end;
end;

procedure TCustomIEAddress.SaveToFile(FileName: WideString);
begin
  if (FileName = EmptyStr) then Exit;
  try
    Items.SaveToFile(FileName);
  finally
  end;
end;

procedure TCustomIEAddress.TextUpdate;
var
  st: string;
  bCancel: Boolean;
begin
  bCancel := False;
  if Assigned(FEmbeddedWB) and FUAfterNav and (Text <> EmptyStr)then
  begin
    if Assigned(FOnUpdateText) then
      FOnUpdateText(Self, Text, st, bCancel);
    if bCancel then
      Text := st;
    FEmbeddedWB.WaitWhileBusy;
    case FTextAfterNav of
      anLocationUrl: Text := FEmbeddedWB.LocationURL;
      anLocationName: Text := FEmbeddedWB.LocationName;
    end;
  end;
end;

//End of Accesories-------------------------------------------------------------

//Graphical interface----------------------------------------------------------

procedure TCustomIEAddress.DrawIEAddress(MouseInControl, DroppedDown: boolean);
var
  CanvasCtrl: TControlCanvas;
  Rect: TRect;
  Position, RectT: integer;
begin
  case FGUI of
    gsThemes:
      begin
        CanvasCtrl := TControlCanvas.Create;
        try
          CanvasCtrl.Control := Self;
          Rect := ClientRect;
          CanvasCtrl.Brush.Style := bsClear; //bsSolid;

          with CanvasCtrl do
          begin
            if BorderColor <> clNone then
            begin
              Brush.Color := BorderColor;
              if FHasBorder then
              begin
                FrameRect(Rect);
              end;
            end
            else
            begin
              Brush.Color := clwhite; //Color;
              if FHasBorder then
              begin
                FrameRect(Rect);
              end;
            end;
          end;
          Rect.Left := Rect.Right - GetSystemMetrics(SM_CXHTHUMB) - 3;
          Dec(Rect.Right);
          InflateRect(Rect, 0, -1);
          if DroppedDown then
            with CanvasCtrl do
            begin //the button backround
              Brush.Color := FButtonPressedColor;
              FillRect(Rect);
              Rect.Right := Rect.Left + 6;
              Brush.Color := Color;
              FillRect(Rect);
            end
          else
            with CanvasCtrl do
            begin
              Brush.Color := ButtonColor;
              FillRect(Rect);
              Rect.Right := Rect.Left + 6;
              Brush.Color := Color;
              FillRect(Rect);
            end;
          if BorderColor <> clNone then
          begin
            Dec(Rect.Right);
            with CanvasCtrl do
            begin
              Pen.Color := BorderColor;
              MoveTo(Rect.Right, Rect.Top);
              LineTo(Rect.Right, Rect.Bottom);
            end;
          end;
          CanvasCtrl.Pen.Color := ArrowColor;
          Rect := ClientRect;
          Position := Rect.Right - 10;
          RectT := Rect.Top;
          if not DroppedDown then
            with CanvasCtrl do
            begin //Draw the arrow Head Down
              Moveto(Position + 0, RectT + 10);
              LineTo(Position + 5, RectT + 10);
              MoveTo(Position + 1, RectT + 11);
              LineTo(Position + 4, RectT + 11);
              MoveTo(Position + 2, RectT + 12);
              LineTo(Position + 3, RectT + 12);
            end
          else
            with CanvasCtrl do
            begin //Draw the arrow Head Up
              MoveTo(Position + 3, RectT + 10);
              LineTo(Position + 2, RectT + 10);
              MoveTo(Position + 4, RectT + 11);
              LineTo(Position + 1, RectT + 11);
              MoveTo(Position + 5, RectT + 12);
              LineTo(Position + 0, RectT + 12);
            end;
        finally
          CanvasCtrl.Free;
        end;
      end;
  end;
end;

procedure TCustomIEAddress.RepaintIEAddress(MouseActive: Boolean);
var
  bCont: boolean;
begin
  bCont := True;
  if Assigned(FonPaint) then
    FOnPaint(Self, bCont);
  try
    if not bCont then
    begin
      ValidateRect(EditHandle, nil);
      Refresh;
      Exit;
    end;
    if not (csDesigning in ComponentState) then
      DrawIEAddress(FMouseActive, DroppedDown);
  finally
  end;
end;

procedure TCustomIEAddress.CalculateRGN;
var
  BorderRGN, DropDownRGN: HRGN;
  BorderWidth, W: Integer;
begin
  if Parent = nil then
    Exit;
  BorderRGN := CreateRectRGN(0, 0, Width, Height);
  BorderWidth := GetSystemMetrics(SM_CXDLGFRAME);

  if not FHasDropDown and not (Style in [csSimple]) then
  begin
    W := GetSystemMetrics(SM_CXVSCROLL);
    Invalidate;
    DropDownRGN := CreateRectRGN(Width - W - BorderWidth, 0, Width, Height);
    CombineRgn(BorderRGN, BorderRGN, DropDownRGN, RGN_XOR);
    DeleteObject(DropDownRGN);
  end;
  SetWindowRGN(Handle, BorderRGN, True);
end;

procedure TCustomIEAddress.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
  Offset: Integer;
begin
  if (Text = EmptyStr) then Exit;
  Offset := 16;
  if not (csDesigning in ComponentState) and DroppedDown then
  begin
    ImageIndex := GetImageIndex(Items[Index], True); //ListV
    if (odSelected in State) then
      FImageIndex := ImageIndex;
    Bitmap := TBitMap.Create();
    with Canvas do
    begin
      FillRect(Rect);
      if Index < Items.Count then
      begin
        FImageList.GetBitmap(ImageIndex, Bitmap);
        if Assigned(Bitmap) then
        begin
          Bitmap.Canvas.Brush.Style := bsClear;
          BrushCopy(Bounds(Rect.Left + 4, (Rect.Top + Rect.Bottom - Bitmap.Height) div 2,
            Bitmap.Width, Bitmap.Height), Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height),
            Bitmap.Canvas.Pixels[0, Bitmap.Height - 1]);
          Offset := Bitmap.Width + 6;
        end;
        TextOut(Rect.Left + OffSet, Rect.Top, Items[Index]);
      end;
    end;
    BitMap.free;
  end;
end;
//End of Graphical interface----------------------------------------------------

//Icons Section-----------------------------------------------------------------

function TCustomIEAddress.GetImageIndex(aUrl: string; IntoLV: Boolean): Integer;
var
  Malloc: Imalloc;
  SpecialFolder: Cardinal;
  sfi: TShFileInfo;
  pidl: PItemIDList;
  ImgIdx: integer;
  Ext: string;
  bCancel: Boolean;
  Icon: TIcon;
begin
  Result := -1;
  if (aUrl = EmptyStr) then Exit;
  try
    ShGetMalloc(Malloc);
    //If its a MS special folder
    SpecialFolder := GetSpecialFolderNo(aUrl);
    if (SUCCEEDED(SHGetSpecialFolderLocation(Handle, SpecialFolder, Pidl))) then
    begin
      ShGetFileInfo(PChar(pidl), 0, sfi, sizeof(sfi), SHGFI_ICON or SHGFI_PIDL);
      Result := sfi.iIcon;
    end
    else
    begin //If its a local file
      if FileExists(aUrl) or (AnsiCompareText(Copy(aURL, 1, 7), 'file://') = 0) then
      begin
        ShGetFileInfo(PChar(aUrl), FILE_ATTRIBUTE_NORMAL, sfi, sizeOf(sfi),
          SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
        Result := sfi.iIcon;
      end
        // If its a folder
      else
        if DirectoryExists(aUrl) then
        begin
          ShGetFileInfo(PChar(aUrl), FILE_ATTRIBUTE_DIRECTORY, sfi, SHGFI_EXETYPE,
            SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
          Result := sfi.iIcon;
        end
        // if its a a url (File/internet Address or Shortcut). so, lets start ;)
        else
          if aURL <> '' then
          begin
            Ext := GetExtension(aUrl);
            if Ext = '*.htm' then
            begin
              if (not (DroppedDown)) and FShowFavicons then
              begin //Get Favicon
                ImgIdx := AddFaviconToImageList;
                if ImgIdx > -1 then
                begin
                  sfi.iIcon := AddFaviconToImageList;
                  Result := sfi.iIcon;
                  DestroyIcon(sfi.iIcon);
                  Malloc.Free(pidl);
                  Exit;
                end;
              end;
              if FUseAppIcon then
              begin //Get app icon
                icon := Forms.Application.Icon;
                if Assigned(FOnGetAppIcon) then
                  FOnGetAppIcon(Self, bCancel, Icon);
                if not bCancel then
                begin
                  ImgIdx := FImageList.AddIcon(Icon);
                  sfi.iIcon := ImgIdx;
                  Result := sfi.iIcon;
                end;
              end
              else
              begin //Get icon for internet shortcuts and addresses
                ShGetFileInfo('*.htm', FILE_ATTRIBUTE_NORMAL, sfi, sizeOf(sfi),
                  SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
                if Assigned(FOnGetIcon) then
                  FOnGetIcon(Self, 'htm', sfi.hIcon);
                Result := sfi.iIcon;
              end
            end
            else
            begin //Get all the rest system icons
              ShGetFileInfo(Pchar(Ext), FILE_ATTRIBUTE_NORMAL, sfi, sizeOf(sfi),
                SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
              Result := sfi.iIcon;
            end;
          end;
    end;
  finally
    try
      if Result <> -1 then
        DestroyIcon(sfi.iIcon)
      else
        Result := 0;
    finally
    end;
    Malloc.Free(pidl);
  end;
end;

function TCustomIEAddress.GrabFavicon(URL: string; dest: string): Boolean;
var
  i: Integer;
  St: string;
  bCancel: Boolean;
begin
  Result := False;
  if (URL = EmptyStr) then Exit;
  try
    bCancel := False;
    St := RemovePrefix(URL, 'HTTP://');
    I := AnsiPos('/', RemovePrefix(St, 'HTTP://'));
    if I > 0 then
      St := 'http://' + System.Copy(St, 1, I);
    if Assigned(FOnGetFavicon) then
      FOnGetFavicon(Self, (GetCacheFolder + 'favicon.ico'), Text, bCancel, nil);
    if not bCancel then
    try
      Result := UrlDownloadToFile(nil, PChar(St + 'favicon.ico'), PChar(dest), 0, nil) = 0;
    except
      Result := False;
    end;
  finally
  end;
end;

function TCustomIEAddress.AddFaviconToImageList: integer;
var
  ImgIdx: integer;
  Icon: TIcon;
  bCancel: Boolean;
  st: WideString;
begin
  Result := -1;
  bCancel := False;
  Application.ProcessMessages;
  Sleep(10);
  if (Text <> EmptyStr) and FShowFavicons and (not DroppedDown) and IsValidURL(Text) and
    (not (csDesigning in ComponentState)) then
  begin
    st := GetCacheFolder + 'favicon.ico';
    if GrabFavIcon(Text, st) then
    begin
      ImgIdx := -1;
      Icon := TIcon.Create();
      if Icon <> nil then
      try
        Icon.LoadFromFile(st);
      except
        Icon.Free;
        Exit;
      end;
{$IFDEF DELPHI10_UP}
      Icon.SetSize(16, 16);
{$ELSE}
      Icon.Height := 16;
      Icon.Width := 16;
{$ENDIF}
      if Assigned(FOnGetFavicon) then
        FOnGetFavicon(Self, st, Text, bCancel, Icon);
      if not bCancel then
      try
        ImgIdx := FImageList.AddIcon(Icon)
      except
      end;
      if ImgIdx > 0 then
      begin
        Result := ImgIdx;
        if not bCancel then
          FImageList.Draw(FCanvas, IconLeft, IConTop, ImgIdx, True);
      end;
      Icon.Free;
    end;
  end;
end;

//End of Icons section----------------------------------------------------------

// Registry Section ------------------------------------------------------------

procedure TCustomIEAddress.GetTypedURLs;
var
  Counter: Integer;
  S: WideString;
  GetTextTmp: PChar;
begin
  Items.Clear;
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if FTypedUrlsMethod = tuCommon then
    begin
      if OpenKey('Software\Microsoft\Internet Explorer\TypedURLs', False) then
      begin
        for Counter := 1 to 25 do
        begin
          if ValueExists('Url' + IntToStr(Counter)) then
          begin
            S := ReadString('Url' + IntToStr(Counter));
            GetTextTmp := Items.GetText;
            if (AnsiPos(S, GetTextTmp) = 0) and (Trim(S) <> '') then
              Items.Add(Trim(S));
            StrDispose(GetTextTmp);
          end;
        end;
        CloseKey;
      end;
    end;
    if FTypedUrlsMethod = tuExtended then
    begin
      if OpenKey('Software\Microsoft\Internet Explorer\TypedAddress', False) then
      begin
        for Counter := 1 to 25 do
        begin
          if ValueExists('Url' + IntToStr(Counter)) then
          begin
            S := ReadString('Url' + IntToStr(Counter));
            GetTextTmp := Items.GetText;
            if (Ansipos(S, GetTextTmp) = 0) and (Trim(S) <> '') then
              Items.Add(S);
            StrDispose(GetTextTmp);
          end;
        end;
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
  if {not Assigned(FEmbeddedWB) and }(TextOnLoad <> tlUserDefine) then
    if Items.Count <> 0 then
      Text := Trim(Items[0])
    else
      Text := '';
end;

procedure TCustomIEAddress.RegistryUpdate;
var
  MaxItems, Counter: Integer;
  Name, SName: WideString;
  Reg: TRegistry;
  bCancel: Boolean;
begin
  if (Trim(Text) <> '') and FUpdateRegistry then
  begin
    Reg := TRegistry.Create;
    bCancel := False;
    try
      with Reg do
      begin
        if Assigned(FOnUpdateRegistry) then
          FOnUpdateRegistry(Self, Text, bCancel);
        if not bCancel then
        begin
          RootKey := HKEY_CURRENT_USER;
          if FTypedUrlsMethod = tuCommon then
          begin
            if OpenKey('Software\Microsoft\Internet Explorer\TypedURLs', True) then
            begin
              if Items.Count < 24 then
                MaxItems := Items.Count + 1
              else
                MaxItems := 25;
              for Counter := 0 to MaxItems - 1 do
              begin
                Name := 'Url' + IntToStr(Counter);
                if items[Counter - 1] <> '' then
                  WriteString(Name, items[Counter - 1]);
              end;
            end;
            CloseKey;
          end;
          if FTypedUrlsMethod = tuExtended then
          begin
            if OpenKey('Software\Microsoft\Internet Explorer\TypedAddress', True) then
            begin
              if Items.Count < 24 then
                MaxItems := Items.Count + 1
              else
                MaxItems := 25;
              for Counter := 0 to MaxItems - 1 do
              begin
                Name := 'Url' + IntToStr(Counter);
                if items[Counter - 1] <> '' then
                begin
                  if Assigned(FEmbeddedWB) then
                  begin
                    FEmbeddedWB.Wait;
                    SName := items[Counter - 1] + '      SiteName:' +
                      FEmbeddedWB.LocationName;
                    WriteString(Name, SName);
                  end;
                end;
              end;
            end;
          end;
          CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;
// End of Registry Section -----------------------------------------------------

// Set--------------------------------------------------------------------------

procedure TCustomIEAddress.SetAbout(Value: string);
begin
  Exit;
end;

procedure TCustomIEAddress.SetBounds(Left, Top, Width, Height: Integer);
begin
  inherited SetBounds(Left, Top, Width, Height);
  SetTextPosition;
  CalculateRGN;
end;

procedure TCustomIEAddress.SetDropDown(const Value: Boolean);
begin
  FHasDropDown := Value;
  CalculateRGN;
end;

procedure TCustomIEAddress.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Ctl3D := not Value;
    FHasBorder := False;
    Invalidate;
  end;
end;

procedure TCustomIEAddress.SetTextPosition;
begin
  SetWindowPos(EditHandle, 0, FImageSize + 7, 5, Width - 46, Height - 7, 0);
end;

procedure TCustomIEAddress.SetHasBorder(const Value: Boolean);
begin
  FHasBorder := Value;
  Invalidate;
  CalculateRGN;
  DrawIEAddress(FMouseActive, DroppedDown);
end;

procedure TCustomIEAddress.SetSiteHint;
begin
  if Assigned(FEmbeddedWB) and FShowSiteHint then
  begin
    FEmbeddedWB.Wait;
    ShowHint := True;
    Application.HintColor := FHintColor;
    if FEmbeddedWB.LocationUrl = Text then
      Hint := FEmbeddedWB.LocationName;
  end;
end;

procedure TCustomIEAddress.SetTheme;
begin
  case FGUI of
    gsThemes:
      begin
        case FThemes of
          tmNone:
            begin
              FBorderColor := clblack;
              FArrowColor := clblack;
              FButtonPressedColor := clBtnShadow;
              FButtonColor := clBtnFace;
            end;
          tmXP:
            begin
              FBorderColor := $00B99D7F;
              FArrowColor := $0085614D;
              FButtonColor := $00FCD5C2;
              FButtonPressedColor := $00F1A680;
            end;
          tmSilver:
            begin
              FBorderColor := clGray;
              FArrowColor := cl3DDKShadow;
              FButtonColor := clSilver;
              FButtonPressedColor := clActiveBorder;
            end;
          tmSoil:
            begin
              FBorderColor := clMaroon;
              FArrowColor := clMaroon;
              FButtonColor := clInfoBk;
              FButtonPressedColor := cl3DLight;
            end;
        end;
        RepaintIEAddress(True);
      end;
  end;
end;

function GetIEHomePage: string;
var
  IEHomePage: string;
begin
  IEHomePage := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey('\Software\Microsoft\Internet Explorer\Main', False);
    IEHomePage := ReadString('Start Page');
    CloseKey;
  finally
    Free;
  end;
  Result := IEHomePage;
end;

procedure TCustomIEAddress.SetTextOnLd;
begin
  case FTextOnLoad of
    tlIELastVisited: ;
    tlIEHomePage: Text := GetIEHomePage;
    tlBlank:
      begin
        if Assigned(FEmbeddedWB) then
          FEmbeddedWB.AssignEmptyDocument
        else
          Text := EmptyStr;
      end;
    tlUserDefine: Text := FTextOnShow;
  end;
  if Assigned(FEmbeddedWB) and FNavOnLoad then
    FEmbeddedWB.Go(Text);
end;

procedure TCustomIEAddress.SetModified(Value: Boolean);
begin
  if HandleAllocated then
    PostMessage(EditHandle, EM_SETMODIFY, Byte(Value), 0)
  else
    FModified := Value;
end;

procedure TCustomIEAddress.SetAutoCompleteOptions(const Value: TAutoCompleteOptions);
begin
  if FAutoCompleteOptions <> Value then
  begin
    FAutoCompleteOptions := Value;
    case FAComp of
      AutoComplete1: UpdateAutoComplete;
      AutoComplete2: Exit;
    end;

  end;
end;

procedure TCustomIEAddress.SetACOptions(const Value: TAutoComplete2Options);
const
  IID_IAutoComplete2: TGUID = '{EAC04BC0-3791-11d2-BB95-0060977B464C}';
  Options: array[TAutoComplete2Option] of integer =
  ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040, $0080, $0100);
var
  Option: TAutoComplete2Option;
  Opt: DWORD;
  AC2: IAutoComplete2;
begin
  if (FAutoComplete <> nil) then
  begin
    if S_OK = FAutoComplete.QueryInterface(IID_IAutoComplete2, AC2) then
    begin
      Opt := ACO_NONE;
      for Option := Low(Options) to High(Options) do
      begin
        if (Option in FAutoComplete2Options) then
          Opt := Opt or DWORD(Options[Option]);
      end;
      AC2.SetOptions(Opt);
    end;
  end;
  FAutoComplete2Options := Value;
end;

procedure TCustomIEAddress.SetAutoComplete2Source(const Value: TAutoComplete2Source);
begin
  if FAutoComplete2Source <> Value then
  begin
    FAutoComplete2Source := Value;
    RecreateWnd;
  end;
end;

// End of Set-------------------------------------------------------------------

//events------------------------------------------------------------------

constructor TCustomIEAddress.Create(AOwner: TComponent);
var
  sfi: TShFileInfo;
  aHandle: Cardinal;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
  DoubleBuffered := True;
  FAbout := 'TIEAddress. ' + WEB_SITE;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FButtonColor := clBtnFace;
  FButtonPressedColor := clBtnFace;
  AutoComplete2Options := [acoAutoSuggest, acoAutoAppend, acoSearch];
  AutoComplete2Source := [acsList, acsHistory, acsMRU, acsShell];
  AutoCompleteOptions := [acAutoAppend, acUrlHistory];
  AutoCompleteVersion := AutoComplete2;
  AutoNavigateOnDblClk := True;
  AutoNavigateOnEnterKey := True;
  AutoNavigateOnLoad := True;
  AutoNavigateOnSelected := True;
  FCustomProperty := '';
  FGUI := gsThemes;
  FHasBorder := True;
  FHasDropDown := True;
  FHintColor := clInfoBK;
  FImageIndex := -1;
  FImageSize := 16;
  FMouseActive := False;
  ShowHint := True;
  FSecureSiteBG := clInfobk;
  FSelImageIndex := -1;
  FShowFavicons := False;
  FShowSiteHint := True;
  FTextOnLoad := tlIEHomePage;
  FTypedUrlsMethod := tuCommon;
  FUAfterNav := True;
  FUpdateRegistry := True;
  FUseAppIcon := False;
  FUseSecureSiteBGColor := True;
  Height := 22;
  IconLeft := 4;
  IconTop := 3;
  ItemHeight := 16;
  Sorted := False;
  Style := csDropDown;
  TabOrder := 0;
  Width := 145;
  FImageList := TImageList.Create(self);
  FImageList.DrawingStyle := dsNormal;
  FImageList.ShareImages := True;
  FImageList.Height := 16;
  FImageList.Width := 16;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FCanvas.Brush.Style := bsClear;
  FCanvas.Handle := EditHandle;
  dllVer := GetDllVersion('Shlwapi.dll');
  aHandle := ShGetFileInfo('', 0, sfi, sizeOf(sfi), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if (aHandle <> 0) then
    FImageList.Handle := aHandle;
end;

destructor TCustomIEAddress.Destroy;
begin
  Application.HintColor := FOldHintColor;
  Color := FOldBGColor;
  FImageList.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TCustomIEAddress.Loaded;
begin
  inherited;
  FOldBGColor := Self.Color;
  FOldHintColor := Application.HintColor;
  DoubleBuffered := True;
  Modified := False;
  if not (csDesigning in ComponentState) then
  begin
    case FGUI of
      gsThemes:
        begin
          BevelEdges := [];
          BevelInner := bvnone;
          BevelKind := bkFlat;
          BevelOuter := BVNone;
          BiDiMode := bdLeftToRight;
          Ctl3D := False;
          ParentBiDiMode := False;
          ParentCtl3D := False;
          ImeMode := imDontCare;
          ImeName := '';
          SetTheme;
        end;
    end;
    CalculateRGN;
    GetTypedURLs;
    SetTextOnLd;
    Text := FixUrl(Text);
    CheckSecureSite;
    TextUpdate;
    SetSiteHint;
    AddFaviconToImageList;
  end;
end;

procedure TCustomIEAddress.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or cbs_OwnerDrawFixed or ES_MULTILINE;
end;

procedure TCustomIEAddress.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  SetTextPosition;
end;

procedure TCustomIEAddress.CreateWnd;
begin
  inherited CreateWnd;
  case FAComp of
    AutoComplete1: UpdateAutoComplete;
    AutoComplete2:
      if (GetDllVersion('Shlwapi.dll') > 4.72) then
        UpdateIAutoComplete2
      else
        UpdateAutoComplete;
  end;
end;

procedure TCustomIEAddress.UpdateAutoComplete;
var
  SelOptions: DWORD;
begin
  SelOptions := 0;
  if acAutoSuggest in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_AUTOSUGGEST_FORCE_ON
  else
    SelOptions := SelOptions or SHACF_AUTOSUGGEST_FORCE_OFF;
  if acAutoAppend in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_AUTOAPPEND_FORCE_ON
  else
    SelOptions := SelOptions or SHACF_AUTOAPPEND_FORCE_OFF;
  if acFileSystem in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_FILESYSTEM
  else
    SelOptions := SelOptions or SHACF_FILESYSTEM;
  if acUrlHistory in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_URLHISTORY
  else
    SelOptions := SelOptions or SHACF_URLHISTORY;
  if acUrlMRU in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_URLMRU
  else
    SelOptions := SelOptions or SHACF_URLMRU;
  if acUrlAll in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_URLALL
  else
    SelOptions := SelOptions or SHACF_URLALL;
  if acUseTab in FAutoCompleteOptions then
    SelOptions := SelOptions or SHACF_USETAB
  else
    SelOptions := SelOptions or SHACF_USETAB;
  SHAutoComplete(EditHandle, SelOptions);
end;

procedure TCustomIEAddress.UpdateIAutoComplete2;
const
  IID_IAutoComplete: TGUID = '{00bb2762-6a77-11d0-a535-00c04fd7d062}';
  CLSID_IAutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';
  CLSID_ACLHistory: TGUID = (D1: $00BB2764; D2: $6A77; D3: $11D0; D4: ($A5, $35, $00, $C0, $4F, $D7, $D0, $62));
  CLSID_ACListISF: TGUID = (D1: $03C036F1; D2: $A186; D3: $11D0; D4: ($82, $4A, $00, $AA, $00, $5B, $43, $83));
  CLSID_ACLMRU: TGUID = (D1: $6756A641; D2: $DE71; D3: $11D0; D4: ($83, $1B, $00, $AA, $00, $5B, $43, $83));
var
  Uk: IUnknown;
  ACInterface: IEnumString;
begin
  try
    Uk := CreateComObject(CLSID_IAutoComplete);
    if (Uk <> nil) and (Uk.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then
    begin
      if acsHistory in FAutoComplete2Source then
        ACInterface := CreateComObject(CLSID_ACLHistory) as IEnumString
      else
        if acsMRU in FAutoComplete2Source then
          ACInterface := CreateComObject(CLSID_ACLMRU) as IEnumString
        else
          if acsShell in FAutoComplete2Source then
            ACInterface := CreateComObject(CLSID_ACListISF) as IEnumString;
      ACInterface := CreateComObject(CLSID_ACLHistory) as IEnumString;
      SetACOptions(FAutoComplete2Options);
      FAutoComplete.Init(Edithandle, ACInterface, nil, nil)
    end;
  except
  end;
end;

procedure TCustomIEAddress.DestroyWnd;
begin
  FModified := Modified;
  inherited DestroyWnd;
end;

procedure TCustomIEAddress.WndProc(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  case Message.Msg of
    CBN_DropDown or CB_ShowDropDown: SetTextPosition;
    CM_MouseEnter: RepaintIEAddress(True);

    WM_MOUSEACTIVATE:
      begin
        if not DroppedDown then
          Exit;
        Message.Result := MA_NOACTIVATE;
        SetWindowPos(Parent.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
        if (GetActiveWindow <> Parent.Handle) then
          SetActiveWindow(Parent.Handle);
      end;

    WM_PAINT:
      begin
        if not (csReading in ComponentState) then
        begin
          ControlStyle := ControlStyle + [csOpaque];
          RepaintIEAddress(False);
          SetTextPosition;
          i := GetImageIndex(Text, False);
          if i > 0 then
            FImageList.Draw(FCanvas, IconLeft, IconTop,
              GetImageIndex(Text, False), True);
        end;
      end;
  end;
end;

procedure TCustomIEAddress.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  CheckButtonState(-1, -1);
  inherited;
end;

procedure TCustomIEAddress.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DroppedDown then
  begin
    Include(FEditState, csButtonPressed);
    Include(FEditState, csMouseCaptured);
  end;
  inherited;
end;

procedure TCustomIEAddress.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CheckButtonState(-1, -1);
  inherited;
end;

procedure TCustomIEAddress.Change;
// var
 //Key: Word;
begin
  PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
  FImageList.Draw(FCanvas, IconLeft, IconTop, GetImageIndex(Text, False), True);
//  key := VK_RETURN;
//  KeyDown(key, []);
end;

procedure TCustomIEAddress.Click;
var
  Rec: TRect;
  pt: TPoint;
  bCancel: Boolean;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    bCancel := False;
    GetCursorPos(pt);
    PostMessage(Handle, CB_GETDROPPEDCONTROLRECT, 0, longint(@rec));
    if ((pt.x >= Rec.Left) and (pt.x <= Rec.Right)
      and (pt.y >= Rec.Top) and (pt.y <= Rec.Bottom)) then
    begin
      if Assigned(FOnUrlSelected) then
        FOnUrlSelected(Self, Text, bCancel);
      if not bCancel then
      begin
        FSelImageIndex := FImageIndex;
        FImageList.Draw(FCanvas, 4, 3, FSelImageIndex, True);
        if FNavOnSelected and Assigned(FEmbeddedWB) then
          FEmbeddedWB.Go(Text);
      end;
      AddToList;
    end;
       // PostMessage(EditHandle, EM_SETREADONLY, 0, 0);
       // PostMessage(EditHandle, EM_SETSEL, 0, -1);
  end;
end;

procedure TCustomIEAddress.DblClick;
var
  Rect: TRect;
  pt: TPoint;
begin
  inherited;
  GetCursorPos(pt);
  PostMessage(Handle, CB_GETDROPPEDCONTROLRECT, 0, longint(@Rect));
  {if ((pt.x >= Rect.Left) and (pt.x <= Rect.Right)
    and (pt.y >= Rect.Top) and (pt.y <= Rect.Bottom)) then }
  begin
    fSelImageIndex := FImageIndex;
    fImageList.Draw(FCanvas, 4, 3, fSelImageIndex, True);
    if FNavOnDblClk and Assigned(FEmbeddedWB) then
      FEmbeddedWB.Go(Text);
    AddToList;
  end;
  PostMessage(EditHandle, EM_SETREADONLY, 0, 0);
  PostMessage(EditHandle, EM_SETSEL, 0, -1);
end;

procedure TCustomIEAddress.KeyDown(var Key: Word; Shift: TShiftState);
var
  FListIndex: integer;
  bCancel: Boolean;
  newText: string;
begin
  newText := Text;
  inherited;
  bCancel := False;
  if (DroppedDown) then
  begin
    if (Key = VK_RETURN) then
    begin
      if Assigned(FOnUrlSelected) then
        FOnUrlSelected(Self, Text, bCancel);
      if not bCancel then
      begin
        Key := VK_CLEAR;
        FListIndex := SendMessage(Handle, CB_GETCURSEL, 0, 0);
        if FListIndex > 0 then
          Items.Move(FListIndex, 0);
        Text := Items[0];
        PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
        PostMessage(handle, CB_SETCURSEL, 0, 0);
        PostMessage(EditHandle, EM_SETREADONLY, 0, 0);
        PostMessage(EditHandle, EM_SETSEL, 0, -1);
        AddToList;
        if Assigned(FEmbeddedWB) then
        begin
          if FNavOnEnterKey then
            FEmbeddedWB.Go(Text);
          if FNavOnSelected then
            FEmbeddedWB.Go(Text);
        end;
        FSelImageIndex := FImageIndex;
        if Modified then
          FImageList.Draw(FCanvas, 4, 3, FSelImageIndex, True);
      end;
    end
    else
      if NewText <> Text then
      begin
        if ((ssAlt in Shift) and ((Key = VK_DOWN) or (Key = VK_UP))) or
          (Key = VK_ESCAPE) then
        begin
          Key := VK_CLEAR;
          PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
          PostMessage(EditHandle, EM_SETREADONLY, 0, 0);
          PostMessage(EditHandle, EM_SETSEL, 0, -1);
        end
        else
          if (not (ssAlt in Shift)) and (Key = VK_DOWN) then
          begin
            Key := VK_CLEAR;
            FListIndex := SendMessage(Handle, CB_GETCURSEL, 0, 0);
            if FListIndex >= 24 then
              Exit;
            PostMessage(Handle, CB_SETCURSEL, (FListIndex + 1), 0);
            PostMessage(EditHandle, EM_SETSEL, WPARAM(-1), 0);
            FSelImageIndex := FImageIndex;
            FImageList.Draw(FCanvas, 4, 3, FSelImageIndex, True);
            if FNavOnSelected and Assigned(FEmbeddedWB) then
              FEmbeddedWB.Go(Text);
          end;
      end
      else
        if (not (ssAlt in Shift)) and (Key = VK_UP) then
        begin
          Key := VK_CLEAR;
          FListIndex := SendMessage(Handle, CB_GETCURSEL, 0, 0);
          if FListIndex <= 0 then
            Exit;
          PostMessage(Handle, CB_SETCURSEL, (FListIndex - 1), 0);
          PostMessage(EditHandle, EM_SETSEL, WPARAM(-1), 0);
          FSelImageIndex := FImageIndex;
          FImageList.Draw(FCanvas, 4, 3, FSelImageIndex, True);
          if FNavOnSelected and Assigned(FEmbeddedWB) then
            FEmbeddedWB.Go(Text);
        end;
  end
  else //Not the Droped Down. Its the text in the ComboBox
  begin
    if (Key = VK_RETURN) and (NewText <> Text) then
    begin
      if Text <> '' then
      begin
        if Assigned(FOnUrlSelected) then
          FOnUrlSelected(Self, Text, bCancel);
        if not bCancel then
        begin
          if FNavOnEnterKey and Assigned(FEmbeddedWB) then
            FEmbeddedWB.Go(Text);
        end;
        AddToList;
      end;
    end
    else
      if (NewText <> Text) and (Key = VK_DOWN) or ((ssAlt in Shift) and (Key = VK_DOWN) and (NewText <> Text)) then
      begin
        Key := VK_CLEAR;
        PostMessage(Handle, CB_GETCURSEL, 0, 0);
      // PostMessage(EditHandle, EM_SETREADONLY, 1, 0);
        PostMessage(EditHandle, EM_SETSEL, WPARAM(-1), 0);
        PostMessage(Handle, CB_SHOWDROPDOWN, 1, 0);
        if FNavOnSelected and Assigned(FEmbeddedWB) then
          FEmbeddedWB.Go(Text);
      end
      else
        if (Key = VK_UP) then
        begin
          Key := VK_CLEAR;
          Exit;
        end;
  end;
end;

procedure TCustomIEAddress.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
end;

procedure TCustomIEAddress.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
end;

initialization //Must have for ShGetFileInfo
  OleInitialize(nil);

finalization
  OleUninitialize;
end.

