(*
  Name:             EwbTools
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      1 May 2014

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 May 2014 - mcdurdin - I4214 - V9.0 - Keyman spams message 0x0401 and frequent timer messages too
                    
*)
//***********************************************************
//                        EwbTools                          *
//                                                          *
//                       For Delphi                         *
//                     Freeware unit                        *
//                            by                            *
//                      bsalsa, Smot,                       *
//                  per lindso larsen                       *
//                                                          *
//  Documentation and updated versions:                     *
//               http://www.bsalsa.com                      *
//***********************************************************

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
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please,  consider donation in our web site!
{*******************************************************************************}

unit EwbTools;

interface

{$I EWB.inc}

uses
  System.Types, System.UITypes,
  EwbAcc, Windows, Classes, ExtCtrls, ShlObj, Graphics, Dialogs, ActiveX,
{$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  MSHTML_EWB, SHDocVw_EWB, EmbeddedWB, URLMon;

var
  PrintingWithOptions: Boolean;

//Document and Frame
function DocumentLoaded(Document: IDispatch): Boolean;
procedure AssignEmptyDocument(WebBrowser: TEmbeddedWB);

//Html
function AddHtmlToAboutBlank(WebBrowser: TEmbeddedWB; StringToHtml: string): Boolean;
function DocumentSourceText(OleObject: Variant; Document: IDispatch): string;
function DocumentSource(OleObject: Variant): string;
function GetWordAtCursor(const X, Y: Integer; WebBrowser: TEmbeddedWB): string;

//frames
function GetFrame(Document: IDispatch; FrameNo: Integer): IWebBrowser2;
function GetFrameFromDocument(SourceDoc: IHTMLDocument2; FrameNo: Integer): IWebBrowser2; //By Aladin
function FrameCount(Document: IDispatch): Longint;
function FrameCountFromDocument(SourceDoc: IHTMLDocument2): Integer; //By Aladin

//Document Operations
procedure SetFocusToDoc(WebBrowser: TEmbeddedWB; Dispatch, Document: IDispatch);
function CMD_Copy(Document: IDispatch): Boolean;
function Cmd_Paste(Document: IDispatch): Boolean;
function Cmd_Cut(Document: IDispatch): Boolean;
function SelectAll(Document: IDispatch): Boolean;
function UnSelectAll(Document: IDispatch): Boolean;

//scroll
procedure ScrollToTop(OleObject: Variant);
procedure ScrollToPosition(OleObject: Variant; X, Y: Integer);
procedure ScrollToBottom(Document: IDispatch);
procedure ScrollToID(ID: Integer; WebBrowser: TEmbeddedWB);
procedure ScrollToIDEx(ID: string; WebBrowser: TEmbeddedWB);
procedure GetScrollBarVisibility(WebBrowser: TEmbeddedWB; var HScroll, VScroll: Boolean);
function GetScrollBarPosition(WebBrowser: TEmbeddedWB; var ScrollPos: TPoint): Boolean;

// zoom
function Zoom(Document: IDispatch; ZoomValue: Integer): Boolean;
function ZoomValue(Document: IDispatch): Integer;
function ZoomRangeHigh(Document: IDispatch): Integer;
function ZoomRangeLow(Document: IDispatch): Integer;

function SetCharartersSet(WebBrowser: TEmbeddedWB; Document: IDispatch; const ACharactersSet: string; Refresh: Boolean = True): Boolean;
procedure GetThumbnail(Dispatch: IDispatch; var Image: TImage);
function GetBmpFromBrowser(Document: IDispatch; Handle: THandle; Width, Height: Integer; FileName: string): Boolean;
function GetJPEGfromBrowser(Document: IDispatch; ControlInterface: IWebBrowser2; FileName: string; SourceHeight, SourceWidth, TargetHeight, TargetWidth: Integer): Boolean;

//View Document Fields/Properties/Images
procedure ViewPageLinksToStrings(OleObject: Variant; LinksList: TStrings);
procedure ViewPageSourceHTMLToStrings(OleObject: Variant; Document: IDispatch; HtmlList: TStrings);
procedure ViewPageSourceTextToStrings(OleObject: Variant; Document: IDispatch; TextList: TStrings);
procedure ViewPageSourceText(OleObject: Variant; Document: IDispatch);

//Save
function SaveDocToStrings(Document: IDispatch; var AStrings: TStrings): HRESULT;
function SaveDocToStream(Document: IDispatch; var AStream: TStream): HRESULT;
function SaveDocToFile(Document: IDispatch; const Fname: string): HRESULT;

//Printing
procedure Print(ControlInterface: IWebBrowser2; bHideSetup: Boolean = False; bCustomHeaderFooter: Boolean = False; Header: string = ''; Footer: string = '');
procedure PrintWithOptions(ControlInterface: IWebBrowser2; Document: IDispatch; UsePrintOptions, PrintOptionsEnabled, HideSetup: Boolean; var InvokingPageSetup: Boolean);
procedure PrintPreview(Webbrowser: IWebBrowser2);
procedure PrintPreviewExtended(ControlInterface: IWebBrowser2; nCMDShow: Integer; HideSetup: Boolean);
procedure PrintPreviewFromTemplate(const TemplateFileName: string; Document: IDispatch);
function PageSetup(Document: IDispatch; UsePrintOptions, PrintOptionsEnabled: Boolean; var InvokingPageSetup: Boolean): Boolean;
procedure PrintSetup(ControlInterface: IWebBrowser2; HideSetup: Boolean);
procedure GetPrintValues(WebBrowser: TEmbeddedWB; PrintOptions: TPrintOptions; Measure: TMeasure);
function PrintMarginStr(Measure, RuntimeMeasure: TMeasure; M: Real): string;
procedure RestorePrintValues;

//Dialogs
function OpenDialog(WebBrowser: TEmbeddedWB; AOwner: TComponent): Boolean;
function SaveDialog(Document: IDispatch): Boolean; overload;
function SaveDialog(WebBrowser: TEmbeddedWB; AOwner: TComponent; ATitle: string = ''; AFilter: string = ''): string; overload;

function ShowInternetOptions(Document: IDispatch): Boolean;
function ShowPageProperties(Document: IDispatch): Boolean;
function ShowOrganizeFavorites(Handle: THandle): Boolean;
procedure ShowImportExportFavoritesAndCookies(Handle: THandle);
function ShowFindDialog(Document: IDispatch): Boolean;
procedure SaveImagesDialog(OleObject: Variant; Document: IDispatch);
function ViewPageSourceHtml(Document: IDispatch): Boolean;
procedure SavePageTextDialog(AOwner: TComponent; OleObject: Variant; Document: IDispatch);

//Open external programs
procedure OpenAddressBook;
procedure OpenEudoraMail;
procedure OpenOutlookExpressMail;
procedure OpenOutlookMail;
procedure OpenRegistryEditor;
function OpenCalendar: Boolean;
function OpenClient(Client: string): Boolean;
function OpenNetMeeting: Boolean;
function OpenNewsClient: Boolean;
procedure DoExploreFolder(Handle: THandle; Path: string);
procedure OpenIEBrowserWithAddress(Handle: THandle);

//Open specific webpages
function OpenHotmailMail(WebBrowser: TEmbeddedWB): Boolean;
function OpenYahooMail(WebBrowser: TEmbeddedWB): Boolean;
function OpenGoogleMail(WebBrowser: TEmbeddedWB): Boolean;
procedure GoSearchInGoogle(WebBrowser: TEmbeddedWB; SearchTerm: string);
procedure GoSearchInMSN(WebBrowser: TEmbeddedWB; SearchTerm: string);
procedure GoSearchInYahoo(WebBrowser: TEmbeddedWB; SearchTerm: string);

//Navigate & Download
procedure Go(WebBrowser: TEmbeddedWB; Url: string);
procedure GoWithQueryDetails(WebBrowser: TEmbeddedWB; Url, Query: string);
procedure GoNoHistory(WebBrowser: TEmbeddedWB; const URL: string);
procedure NavigatePidl(WebBrowser: TEmbeddedWB; pidl: PItemIdList);
procedure GoAboutBlank(WebBrowser: TEmbeddedWB);
procedure GoDownloadFile(WebBrowser: TEmbeddedWB; URL: string);
function DownloadFile(SourceFile, TargetFile: string): Boolean;
procedure GoDownloadMaskedFile(SourceFile, TargetFile: string; Notify: Boolean);

//Get Special Folders/URL paths etc.
function GetSpecialFolderPath(CallerHandle: THandle; CSIDL: Integer): PChar;
function GetShellFolderPath(FolderName: Widestring): string;
function GetIEHomePage: string;
function GetCachePath: string;
function GetCachedFileFromURL(ItemUrl: string): string;
function GetDefaultBrowserFromRegistry: string;
function GetIPAndHostName(var HostName, IPaddr, WSAErr: string): Boolean;


//E-Mail functions
procedure SendPageInMailAsAttachment(WebBrowser: TEmbeddedWB; AOwner: TComponent; Document: IDispatch; mFileName, mSubject, mBody: string);
function CreateNewMail: Boolean;
procedure SendUrlInMail(LocationURL, LocationName: WideString);

//Search in Document & Fill Forms
function SearchString(Webbrowser: TEmbeddedWB; const strText: string): Boolean;
//function SearchText(WebBrowser: TEmbeddedWB; Document: IDispatch; const Value: string; const iPos: Integer = 1): IHTMLTxtRange;
function SearchText(WebBrowser: TEmbeddedWB; Document: IDispatch; const Value: string; aTypeSearch: Integer; const iPos: Integer = 1): IHTMLTxtRange;

procedure SearchAndHighlight(Document: IDispatch;
  AText: string; const ACaption, APrompt: string; Flags: TSearchFlags = [];
  cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll; ShowInputQuery: Boolean = True); overload;

procedure SearchAndHighlight(Document: IDispatch; aText: string; Flags: TSearchFlags = [];
 cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll); overload;

procedure SetTextAreaValue(Document: IDispatch; sName, sValue: string; Options: TFindOptions);
function FillForm(WebBrowser: TEmbeddedWB; FieldName, FieldValue: string; ElementNr: Integer = -1): Boolean; overload;
function FillForm(Document: IDispatch; FieldName: string; FieldValue: string; ElementNr: Integer = -1): Boolean; overload;

function FillForm(WebBrowser: TEmbeddedWB; FieldName: string; FieldValue: string; Value: Boolean): Boolean; overload;


function GetFieldValue(OleObject: Variant; FieldName: string): string;
procedure ClickInputImage(WebBrowser: TEmbeddedWB; ImageURL: string);

procedure FillIEFormAndExcecute;

//Clearing
procedure ClearCache;
procedure ClearTypedUrls;

//Online Status
function CheckOnlineStatus: Boolean;
function IsGlobalOffline: Boolean;
procedure WorkOffline();
procedure WorkOnline();

//Restricted & Trusted Lists
function CheckIfInRestricredList(const Host: string; SecureSite: Boolean): Boolean;
function CheckIfInTrustedList(const Host: string; SecureSite: Boolean): Boolean;
procedure AddToTrustedSiteList(WebBrowser: TEmbeddedWB; const URL: string);
procedure AddToRestrictedSiteList(WebBrowser: TEmbeddedWB; const URL: string);

//Zone Icon, Security Zone, SSL Status
procedure GetZoneIcon(IconPath: string; var Icon: TIcon);
function GetZoneIconToForm(LocationURL: string; Caption, Hint: string): Boolean;
function GetZoneAttributes(const URL: string): TZoneAttributes;
function GetSSLStatus(OleObject: Variant; LocationURL: string; var SSLName, SSLDescription: string): Boolean;
function GetUrlSecurityZone(LocationURL: string; var ZoneName, ZoneDescription: string; var Icon: TIcon): Boolean;

//Proxy & User agent
function SetProxy(UserAgent, Address, Bypass: string): Boolean; overload;
function SetProxy(UserAgent, Address, UserName, Password: string; Port: Integer): Boolean; overload;
function SetProxyFromPAC(UserAgent, PACFile: string): Boolean;

function RemoveProxy(): Boolean;
procedure RemoveUserAgent(UserAgent: string);

//MIME Filter & NameSpace
function RegisterMIMEFilter(clsid: TGUID; MIME: PWideChar): HRESULT;
function UnregisterMIMEFilter(MIME: PWideChar): HRESULT;
function RegisterNameSpace(clsid: TGUID): HRESULT;
function UnregisterNameSpace: HRESULT;

//Cookies
function GetCookiesPath: string;
procedure ClearSessionCookies;

//Favorites
function OrganizeFavorite(h: THandle; Path: PAnsiChar): Boolean; stdcall; overload;
{$IFDEF UNICODE}
function OrganizeFavorite(h: THandle; Path: PWideChar): Boolean; overload;
{$ENDIF UNICODE}

function URLFromFavorites(const dotURL: string): string;
function GetFavoritesPath: string;
procedure AddToFavorites(URL, Title: string);

//History
function GetHistoryPath: string;
function UrlFromHistory(ShellFolder: IShellFolder; pidl: PItemIDList): string;
procedure ClearHistory;

//Pages
procedure SetNewHomePage(HomePage: string);
function GetLastVisitedPage(var LastVisitedPage: string): Boolean;
function SaveLastVisitedPage(WebBrowser: TEmbeddedWB; LocationURL: string): Boolean;

//Code accessories
procedure Wait(WebBrowser: TEmbeddedWB);
function InvokeCMD(Document: IDispatch; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): HRESULT;
function GetIEHandle(WebBrowser: TEmbeddedWB; ClassName: string): HWND;

//Execute Script
procedure ExecScript(WebBrowser: TEmbeddedWB; sExpression, sLanguage: string);
function ExecScriptEx(WebBrowser: TEmbeddedWB; MethodName: string; ParamValues: array of const): OleVariant;
function WBExecScript(TargetObj: IDispatch; MethodName: string; ParamValues: array of const): OleVariant;

//Miscellaneous
procedure RestoreApplicationFormSize(WebBrowser: TEmbeddedWB);
procedure SaveApplicationFormSize(WebBrowser: TEmbeddedWB);
procedure ShowIEVersionInfo(Handle: THandle);
procedure CreateDesktopShortcut(Handle: THandle);
procedure DisableNavSound(bDisable: Boolean);

//----- add to ewb-------------------------------------------------------
function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
function CopyITEMID(Malloc: IMalloc; ID: PItemIDList): PItemIDList;
function CopyPIDL(IDList: PItemIDList): PItemIDList;
function CreatePIDL(Size: Integer): PItemIDList;
function DeleteUrl(Url: PWideChar): HResult;
function Encode(const S: string): string;
function ExtractUrl(ShellFolder: IShellFolder; pidl: PItemIDList): string;
function GetDisplayName(Folder: IShellFolder; pidl: PItemIDList): string;
function GetFileName(Folder: IShellFolder; pidl: PItemIDList): string;
function GetIEVersion: string;
function GetIEVersionMajor: Integer;
function GetImageIndex(pidl: PItemIDList): Integer;
function GetMailClients: TStrings;
function GetPIDLSize(IDList: PItemIDList): Integer;
function IE5_Installed: Boolean;
function IsChannel(ChannelShortcut: string; ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
function IsFolder(ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
function IsFolderEx(ChannelShortcut: string; ShellFolder: IShellFolder; ID: PItemIDList): Boolean;




function NextPIDL(IDList: PItemIDList): PItemIDList;
function ResolveChannel(pFolder: IShellFolder; pidl: PItemIDList; var lpszURL: string): HRESULT;
function ResolveLink(const Path: string): string;
function ResolveUrlIni(FileName: string): string;
function ResolveUrlIntShCut(FileName: string): string;
function StringToVarArray(const S: string): Variant;
function URLFromShortcut(const dotURL: string): string;
function VarArrayToString(const V: Variant): string;
procedure DisposePIDL(ID: PItemIDList);
procedure StripLastID(IDList: PItemIDList);
function IsWinXPSP2OrLater(): Boolean;
function EncodeUrl(const InputStr: string; const bQueryStr: Boolean): string;
function DecodeURL(const InputStr: string): string;
function IsValidProtocol(URL: string): Boolean;
function ImportCertFile(AFileName, AStoreType: string): Boolean;
//--end of add to ewb---------------------------------

implementation

uses
  Registry, ShellAPI, Controls, Messages, Forms, SysUtils,
  OleCtrls, WinInet, SendMail_For_EWB, ComObj, EwbIEConst, IniFiles, JPEG, WinSock,
  Wcrypt2, Browse4Folder, EWBCoreTools;

type
  OSVERSIONINFOEX = packed record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of Char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  TOSVersionInfoEx = OSVERSIONINFOEX;
  POSVersionInfoEx = ^TOSVersionInfoEx;


type
  fn_VerifyVersionInfo = function(var VersionInformation: OSVERSIONINFOEX;
    dwTypeMask: DWORD; dwlConditionMask: LONGLONG): BOOL; stdcall;
  fn_VerSetConditionMask = function(ConditionMask: LONGLONG; TypeMask: DWORD;
    Condition: Byte): LONGLONG; stdcall;

function ImportCertFile(AFileName, AStoreType: string): Boolean;
var
  f: file; //by Ray
  encCert: PByte;
  encCertLen: DWORD;
  store: HCERTSTORE;
  context: PCCERT_CONTEXT;
  n: PCCERT_CONTEXT;
  encType: DWORD;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    AssignFile(f, AFileName);
    Reset(f, 1);
    encCertLen := FileSize(f);
    GetMem(encCert, encCertLen);
    BlockRead(f, encCert^, encCertLen);
    CloseFile(f);
    try
      encType := PKCS_7_ASN_ENCODING or X509_ASN_ENCODING;
      context := CertCreateCertificateContext(encType, encCert, encCertLen);
      if context <> nil then
      begin
        store := CertOpenSystemStore(0, PChar(AStoreType));
        if store <> nil then
        begin
          n := nil;
          Result := CertAddCertificateContextToStore(store, context,
            CERT_STORE_ADD_REPLACE_EXISTING, n);
          CertCloseStore(store, 0);
          CertFreeCertificateContext(context);
        end;
      end;
    finally
      FreeMem(encCert, encCertLen);
    end;
  end;
end;

function IsWinXPSP2OrLater(): Boolean;
var
  osvi: TOSVersionInfoEx;
  dwlConditionMask: LONGLONG;
  op: Integer;
  hlib: THandle;
  VerifyVersionInfo: fn_VerifyVersionInfo;
  VerSetConditionMask: fn_VerSetConditionMask;
begin
  Result := False;
  hLib := LoadLibrary('kernel32.dll');
  if (hLib <> 0) then
  begin
    @VerifyVersionInfo := GetProcAddress(hLib, 'VerifyVersionInfoA');
    @VerSetConditionMask := GetProcAddress(hLib, 'VerSetConditionMask');
    if ((@VerifyVersionInfo = nil) or (@VerSetConditionMask = nil)) then
      Exit;

    dwlConditionMask := 0;
    op := VER_GREATER_EQUAL;

    // Initialize the OSVERSIONINFOEX structure.
    ZeroMemory(@osvi, SizeOf(OSVERSIONINFOEX));
    osvi.dwOSVersionInfoSize := SizeOf(OSVERSIONINFOEX);
    osvi.dwMajorVersion := 5;
    osvi.dwMinorVersion := 1;
    osvi.wServicePackMajor := 2;
    osvi.wServicePackMinor := 0;

    // Initialize the condition mask.
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_MAJORVERSION, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_MINORVERSION, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMAJOR, op);
    dwlConditionMask := VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMINOR, op);

    // Perform the test.
    Result := VerifyVersionInfo(osvi, VER_MAJORVERSION or VER_MINORVERSION or
      VER_SERVICEPACKMAJOR or VER_SERVICEPACKMINOR, dwlConditionMask);
  end;
end;

function EncodeURL(const InputStr: string; const bQueryStr: Boolean): string;
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 1 to Length(InputStr) do
  begin
    case InputStr[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + InputStr[Idx];
      ' ':
        if bQueryStr then
          Result := Result + '+'
        else
          Result := Result + '%20';
    else
      Result := Result + '%' + SysUtils.IntToHex(Ord(InputStr[Idx]), 2);
    end;
  end;
end;

function DecodeURL(const InputStr: string): string;
var
  Idx: Integer;
  Hex: string;
  Code: Integer;
begin
  Result := '';
  Idx := 1;
  while Idx <= Length(InputStr) do
  begin
    case InputStr[Idx] of
      '%':
        begin
          if Idx <= Length(InputStr) - 2 then
          begin
            Hex := InputStr[Idx + 1] + InputStr[Idx + 2];
            Code := SysUtils.StrToIntDef('$' + Hex, -1);
            Inc(Idx, 2);
          end
          else
            Code := -1;
          if Code = -1 then
            raise SysUtils.EConvertError.Create('Invalid HEX digit in URL');
          Result := Result + Chr(Code);
        end;
      '+':
        Result := Result + ' '
    else
      Result := Result + InputStr[Idx];
    end;
    Inc(Idx);
  end;
end;

function IsValidProtocol(URL: string): Boolean;
const
  Protocols: array[1..11] of string = ('ftp://', 'http://', 'https://',
    'gopher://', 'mailto:', 'news:', 'nntp://', 'telnet://', 'wais://',
    'file://', 'prospero://');
var
  I: Integer;
begin
  Result := False;
  URL := SysUtils.LowerCase(URL);
  for I := Low(Protocols) to High(Protocols) do
    if Pos(Protocols[I], URL) <> 0 then
    begin
      Result := True;
      Break;
    end;
end;

function DocumentLoaded(Document: IDispatch): Boolean;
var
  iDoc: IHTMLDocument2;
begin
  Result := False;
  if Assigned(Document) then
  begin
    Document.QueryInterface(IHTMLDocument2, iDoc);
    Result := Assigned(iDoc);
  end;
end;

procedure AssignEmptyDocument(WebBrowser: TEmbeddedWB);
begin
  WebBrowser.Go('about:blank');
end;

function AddHtmlToAboutBlank(WebBrowser: TEmbeddedWB; StringToHtml: string): Boolean;
var
  Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  WebBrowser.Navigate('about:' + StringToHtml, Flags, TargetFrameName, PostData, Headers);
  Result := True;
end;

function GetWordAtCursor(const X, Y: Integer; WebBrowser: TEmbeddedWB): string;
var
  Doc: IHTMLDocument2;
  Selection: IHTMLSelectionObject;
  Range: IHTMLTxtRange;
begin
  Result := '';
  if WebBrowser.DocumentLoaded(Doc) then
  begin
    Selection := (Doc as IHTMLDocument2).selection;
    if Assigned(Selection) then
    begin
      Range := Selection.createRange as IHTMLTxtRange;
      Range.moveToPoint(X, Y);
      Range.moveStart('word', -1);
      Range.moveEnd('word', 1);
      Result := Trim(Range.text);
    end;
  end;
end;

procedure PrintPreviewFromTemplate(const TemplateFileName: string; Document: IDispatch);
var
  OleCommandTarget: IOleCommandTarget;
  ParamIn, EmptyParam: OleVariant;
begin
  if Assigned(Document) then
  begin
    EmptyParam := EmptyStr;
    Document.QueryInterface(IID_IoleCommandTarget, OLECOMMANDTARGET);
    ParamIn := TemplateFileName;
    OleCommandTarget.Exec(
      nil,
      OLECMDID_PRINTPREVIEW,
      OLECMDEXECOPT_PROMPTUSER,
      ParamIn, EmptyParam);
  end;
end;

procedure ScrollToIDEx(ID: string; WebBrowser: TEmbeddedWB);
var
  Doc3: IHTMLDocument3;
  Elem: IHTMLElement;
  RV: IHTMLRect;
begin
  Doc3 := WebBrowser.Doc3;
  if Assigned(Doc3) then
  begin
    Elem := Doc3.getElementById(ID);
    if Assigned(Elem) then
    begin
      RV := (Elem as IHTMLElement2).getBoundingClientRect;
      Webbrowser.Doc2.parentWindow.scrollBy(RV.left, RV.top);
    end;
  end;
end;

procedure ScrollToID(ID: Integer; WebBrowser: TEmbeddedWB);
var
  Doc: IHTMLDocument2;
  ACollection: IHTMLElementCollection;
  Elem: IHTMLElement;
  Match: IHTMLElement2;
  I: Integer;
  S: string;
  RV: IHTMLRect;
begin
  if WebBrowser.DocumentLoaded(Doc) then
  begin
    ACollection := Doc.all;
    if Assigned(ACollection) then
    begin
      Match := nil;
      S := IntToStr(ID);
      for I := 0 to ACollection.length - 1 do
      begin
        Elem := ACollection.item(I, '') as IHTMLElement;
        if Assigned(Elem) and (Elem.id = S) then
        begin
          Match := Elem as IHTMLElement2;
          Break;
        end;
      end;
      if Assigned(Match) then
      begin
        RV := Match.getBoundingClientRect;
        WebBrowser.Doc2.parentWindow.scrollBy(RV.left, RV.top);
      end;
    end;
  end;
end;

// Get SysListView32 Child from the Webbrowser Control

function GetWBLV(WBHandle: HWND): HWND;
var
  WND: HWND;
begin
  Result := 0;
  Wnd := GetNextWindow(WBHandle, GW_CHILD);
  while (Result = 0) and (WND <> 0) do
  begin
    Result := FindWindowEx(Wnd, 0, 'SysListView32', nil);
    Wnd := GetNextWindow(Wnd, GW_CHILD)
  end;
end;

// Check if the horizontal / vertical Scrollbars are visible

procedure GetScrollBarVisibility(WebBrowser: TEmbeddedWB; var HScroll, VScroll: Boolean);
var
  WndLV: HWND;
  IDoc: IHTMLDocument2;
begin
  VScroll := False;
  HScroll := False;
  WndLV := GetWBLV(WebBrowser.Handle);
  if WndLV = 0 then
  begin
    if Assigned(WebBrowser.Document) and (Succeeded(WebBrowser.Document.QueryInterface(IHTMLDocument2, IDoc))) then
    begin
      IDoc := WebBrowser.Document as IHTMLDocument2;
      if Assigned(IDoc) and Assigned((IHTMLDocument2(IDoc).Body)) then
      begin
        VScroll := WebBrowser.OleObject.Document.body.ScrollHeight > WebBrowser.OleObject.Document.Body.ClientHeight;
        HScroll := (WebBrowser.OleObject.Document.body.ScrollWidth > WebBrowser.OleObject.Document.Body.ClientWidth);
      end;
    end;
  end else
  begin
    // if the WB is in "ListView" mode:
    VScroll := ((GetWindowLong(WndLV, GWL_STYLE) and WS_VSCROLL) <> 0);
    HScroll := ((GetWindowLong(WndLV, GWL_STYLE) and WS_HSCROLL) <> 0)
  end;
end;

// Get TEmbeddedWB Scrollbar X,Y Position

function GetScrollBarPosition(WebBrowser: TEmbeddedWB; var ScrollPos: TPoint): Boolean;

  // Get Scrollbar X,Y Position of the ListView
  function WB_GetLVScrollPosition(WebBrowser: TEmbeddedWB; var ScrollPos: TPoint): Boolean;
  var
    lpsi: TScrollInfo;
    WndLV: HWND;
  begin
    Result := False;
    // Retrieve SysListView32 Child of TEmbeddedWB
    WndLV := GetWBLV(WebBrowser.Handle);
    if WndLV <> 0 then // SysListView32 found
    begin
    // initialize TScrollInfo
      FillChar(lpsi, SizeOf(lpsi), 0);
      with lpsi do
      begin
        cbSize := SizeOf(lpsi);
        fMask := SIF_POS;
      end;
      // Get ScrollInfos from the vertical Scrollbar
      if GetScrollInfo(WndLV, SB_VERT, lpsi) then
      begin
        ScrollPos.Y := lpsi.nPos;
        // Get ScrollInfos from the horizontal Scrollbar
        if GetScrollInfo(WndLV, SB_HORZ, lpsi) then
        begin
          ScrollPos.X := lpsi.nPos;
          Result := True;
        end;
      end;
    end;
  end;

  // Get Scrollbar X,Y Position of the HTML Document
  function WB_GetDOCScrollPosition(WebBrowser: TEmbeddedWB; var ScrollPos: TPoint): Boolean;
  var
    IDoc: IHTMLDocument2;
    IDoc3: IHTMLDocument3;
    IElement: IHTMLElement;
  begin
    ScrollPos := Point(-1, -1);
    Result := False;
    if Assigned(WebBrowser.Document) and (Succeeded(WebBrowser.Document.QueryInterface(IHTMLDocument2, IDoc))) then
    begin
      IDoc := WebBrowser.Document as IHTMLDocument2;
      if Assigned(IDoc) and Assigned((IHTMLDocument2(IDoc).Body)) then
      begin
        if (IDoc.QueryInterface(IHTMLDocument3, IDoc3) = S_OK) then
          if Assigned(IDoc3) then
            IElement := IDoc3.get_documentElement;
        if (Assigned(IElement)) and (Variant(IDoc).DocumentElement.scrollTop = 0) then
          ScrollPos.Y := IHTMLDocument2(IDoc).Body.getAttribute('ScrollTop', 0)
        else
          ScrollPos.Y := Variant(IDoc).DocumentElement.scrollTop;
        if Assigned(IElement) and (Variant(IDoc).DocumentElement.scrollLeft = 0) then
          ScrollPos.X := IHTMLDocument2(IDoc).Body.getAttribute('ScrollLeft', 0)
        else
          ScrollPos.X := Variant(IDoc).DocumentElement.scrollLeft
      end;
      Result := (ScrollPos.X <> -1) and (ScrollPos.Y <> -1)
    end;
  end;

begin
  Result := WB_GetDOCScrollPosition(WebBrowser, ScrollPos);
  if not Result then
    Result := WB_GetLVScrollPosition(WebBrowser, ScrollPos);
end;

function DocumentSource(OleObject: Variant): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    ViewPageSourceHTMLToStrings(OleObject, OleObject.Document, Strings);
    Result := Strings.Text;
  finally
    FreeAndNil(Strings);
  end;
end;

function DocumentSourceText(OleObject: Variant; Document: IDispatch): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    EwbTools.ViewPageSourceTextToStrings(OleObject, Document, Strings);
    Result := Strings.Text;
  finally
    FreeAndNil(Strings);
  end;
end;

function GetFrame(Document: IDispatch; FrameNo: Integer): IWebBrowser2;
var
  OleContainer: IOleContainer;
  enum: ActiveX.IEnumUnknown;
  unk: IUnknown;
  Fetched: PLongint;
begin
  if Assigned(Document) then
  begin
    Fetched := nil;
    OleContainer := Document as IOleContainer;
    if OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum) = S_OK then
    begin
      Enum.Skip(FrameNo);
      Enum.Next(1, Unk, Fetched);
      Result := Unk as IWebBrowser2;
    end else
      Result := nil;
  end
  else
    Result := nil;
end;

function FrameCount(Document: IDispatch): LongInt;
var //fix by Aladin
  OleContainer: IOleContainer;
  enum: ActiveX.IEnumUnknown;
  FetchedContrs: LongInt;
  Unknown: IUnknown;
  IWeb: IWebBrowser2;
begin
  Result := 0; //bsalsa
  if not DocumentLoaded(Document) then Exit;
  OleContainer := Document as IOleContainer;
  if OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum) = S_OK then
  begin
    while Enum.Next(1, Unknown, @FetchedContrs) = S_OK do
    begin
      if Unknown.QueryInterface(IID_IWebBrowser2, IWeb) = S_OK then //check if it is frame
        Inc(Result);
    end;
  end;
end;

function FrameCountFromDocument(SourceDoc: IHTMLDocument2): Integer;
var //by Aladin
  OleContainer: IOleContainer;
  enum: ActiveX.IEnumUnknown;
  unk: array[0..99] of IUnknown; // CHANGED from "unk: IUnknown;"
  EnumResult: HRESULT;
begin
  Result := 0;
  if not DocumentLoaded(SourceDoc) then Exit;
  OleContainer := SourceDoc as IOleContainer;
  EnumResult := OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum);
  if EnumResult = S_OK then
       // Added per OLE help
    Enum.Next(100, Unk, @Result)
  else // Added per OLE help
    Enum := nil;
end;

procedure SetFocusToDoc(WebBrowser: TEmbeddedWB; Dispatch, Document: IDispatch);
begin
  if DocumentLoaded(Document) then
    with (Dispatch as IOleObject) do
      DoVerb(OLEIVERB_UIACTIVATE, nil, WebBrowser, 0, WebBrowser.Handle, WebBrowser.ClientRect);
end;

function CMD_Copy(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function CMD_Paste(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_PASTE, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function CMD_Cut(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_CUT, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function SelectAll(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function UnSelectAll(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_CLEARSELECTION, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

procedure ScrollToTop(OleObject: Variant);
begin
  try
    if DocumentLoaded(OleObject.Document) then
      OleObject.Document.ParentWindow.ScrollTo(0, 0);
  except
  end;
end;

procedure ScrollToPosition(OleObject: Variant; X, Y: Integer);
begin
  try
    if DocumentLoaded(OleObject.Document) then
      OleObject.Document.ParentWindow.ScrollTo(X, Y);
  except
  end;
end;

procedure ScrollToBottom(Document: IDispatch);
var
  HTMLParentWin: IHTMLWindow2;
  Doc2: IHTMLDocument2;
begin
  try
    if Supports(Document, IHTMLDocument2, Doc2) then
    begin
      // OleObject.Document.ParentWindow.ScrollTo(0, MaxInt); doesn't work in IE8
      HTMLParentWin := IHTMLWindow2((Doc2 as IHTMLDocument2).parentWindow);
      HTMLParentWin.scrollBy(0, (Doc2.body as IHTMLElement2).scrollHeight);
    end;
  except
  end;
end;

function Zoom(Document: IDispatch; ZoomValue: Integer): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  if ZoomValue < ZoomRangeLow(Document) then
    vaIn := ZoomRangeLow(Document)
  else
    if ZoomValue > ZoomRangeHigh(Document) then
      vaIn := ZoomRangeHigh(Document)
    else
      vaIn := ZoomValue;
  Result := InvokeCmd(Document, False, OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut) = S_OK;
end;

function ZoomValue(Document: IDispatch): Integer;
var
  vaIn, vaOut: OleVariant;
begin
  vaIn := null;
  InvokeCmd(Document, False, OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := vaOut;
end;

function ZoomRangeHigh(Document: IDispatch): Integer;
var
  vaIn, vaOut: OleVariant;
begin
  InvokeCmd(Document, False, OLECMDID_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := HiWord(DWORD(vaOut));
end;

function ZoomRangeLow(Document: IDispatch): Integer;
var
  vaIn, vaOut: OleVariant;
begin
  InvokeCmd(Document, False, OLECMDID_GETZOOMRANGE, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  Result := LoWord(DWORD(vaOut));
end;

function SetCharartersSet(WebBrowser: TEmbeddedWB; Document: IDispatch; const ACharactersSet: string; Refresh: Boolean = True): Boolean;
var
  RefreshLevel: OleVariant;
begin
  Wait(WebBrowser);
  Result := False;
  if DocumentLoaded(Document) then
  begin
    try
      WebBrowser.Doc2.Set_CharSet(ACharactersSet);
      Result := True;
      if Refresh then
      begin
        RefreshLevel := 7;
        WebBrowser.Refresh2(RefreshLevel);
      end;
    except
    end;
  end;
end;

{
function GetCookie(OleObject: Variant): string;
begin
  Result := '';
  if DocumentLoaded(OleObject.Document) then
  try
    Result := OleObject.Document.Cookie;
  except
  end;
end;    }

procedure ClearSessionCookies;
begin
  InternetSetOption(nil, INTERNET_OPTION_END_BROWSER_SESSION, nil, 0);
end;

procedure GetThumbnail(Dispatch: IDispatch; var Image: TImage);
var
  DrawRect: TRect;
begin
  if Image = nil then
    Exit;
  DrawRect := Rect(0, 0, Image.Height, Image.Width);
  Image.Picture.Bitmap.Height := Image.Height;
  Image.Picture.Bitmap.Width := Image.Width;
  (Dispatch as IViewObject).Draw(DVASPECT_DOCPRINT, 0, nil, nil, 0,
    Image.Canvas.Handle, @DrawRect, nil, nil, 0);
  Image.Refresh;
end;

function GetBmpFromBrowser(Document: IDispatch; Handle: THandle; Width, Height: Integer; FileName: string): Boolean;
var
  ViewObject: IViewObject;
  sourceDrawRect: TRect;
  ScreenImg: Graphics.TBitmap;
begin
  Result := False;
  if DocumentLoaded(Document) then
  try
    Document.QueryInterface(IViewObject, ViewObject);
    if Assigned(ViewObject) then
    try
      ScreenImg := TBitmap.Create;
      ScreenImg.Height := Height;
      ScreenImg.Width := Width;
      sourceDrawRect := Rect(0, 0, ScreenImg.Width, ScreenImg.Height);
      ViewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, Handle,
        ScreenImg.Canvas.Handle, @sourceDrawRect, nil, nil, 0);
      ScreenImg.SaveToFile(FileName);
      Result := True;
    finally
      //ViewObject._Release;  I4214   // I4214
    end;
  except
    Result := False;
  end;
end;

function GetJPEGfromBrowser(Document: IDispatch; ControlInterface: IWebBrowser2; FileName: string; SourceHeight, SourceWidth,
  TargetHeight, TargetWidth: Integer): Boolean;
var
  sourceDrawRect: TRect;
  targetDrawRect: TRect;
  sourceBitmap: Graphics.TBitmap;
  targetBitmap: Graphics.TBitmap;
  aJPG: TJPEGImage;
  aViewObject: IViewObject;
  IWeb: IWebBrowser2;
begin
  Result := False;
  sourceBitmap := Graphics.TBitmap.Create;
  targetBitmap := Graphics.TBitmap.Create;
  aJPG := TJPEGImage.Create;
  IWeb := ControlInterface;
  try
    try
      sourceDrawRect := Rect(0, 0, SourceWidth, SourceHeight);
      sourceBitmap.Width := SourceWidth;
      sourceBitmap.Height := SourceHeight;
      aViewObject := IWeb as IViewObject;
      if aViewObject = nil then
        Exit;
      OleCheck(aViewObject.Draw(DVASPECT_CONTENT, 1, nil, nil,
        Forms.Application.Handle,
        sourceBitmap.Canvas.Handle,
        @sourceDrawRect, nil, nil, 0));
      targetDrawRect := Rect(0, 0, TargetWidth, TargetHeight);
      targetBitmap.Height := TargetHeight;
      targetBitmap.Width := TargetWidth;
      targetBitmap.Canvas.StretchDraw(targetDrawRect, sourceBitmap);
      aJPG.Assign(targetBitmap);
      aJPG.SaveToFile(FileName);
      Result := True;
    finally
      aJPG.Free;
      sourceBitmap.Free;
      targetBitmap.Free;
    end;
  except
    Result := False;
  end;
end;

procedure ViewPageLinksToStrings(OleObject: Variant; LinksList: TStrings);
var //by smot
  UNum: Variant;
  s: string;

  procedure RecurseLinks(htmlDoc: Variant);
  var
    BodyElement, ElementCo, HTMLFrames, HTMLWnd, doc: OleVariant;
    j, i: Integer;
  begin
    if VarIsEmpty(htmlDoc) then Exit;
    BodyElement := htmlDoc.body;
    if BodyElement.tagName = 'BODY' then
    begin
      ElementCo := htmlDoc.links;
      j := ElementCo.Length - 1;
      for i := 0 to j do
      begin
        UNum := ElementCo.item(i);
        s := UNum.href;
        if j = 0 then
          s := 'No Links found in the page body';
        LinksList.Add(s);
      end;
    end;
    HTMLFrames := htmlDoc.Frames;
    j := HTMLFrames.Length - 1;
    for i := 0 to j do
    begin
      HTMLWnd := HTMLFrames.Item(i);
      try
        doc := HTMLWnd.Document;
        RecurseLinks(doc);
      except
        Continue;
      end;
    end;
  end;
begin
  LinksList.Clear;
  if not DocumentLoaded(OleObject.Document) then
    Exit;
  RecurseLinks(OleObject.Document);
end;

procedure ViewPageSourceHTMLToStrings(OleObject: Variant; Document: IDispatch; HtmlList: TStrings);
begin
  HtmlList.Clear;
  if DocumentLoaded(Document) then
  begin
    try
      HtmlList.Add(VarToStr(OleObject.Document.documentElement.innerHTML));
    except
    end;
  end;
end;

procedure ViewPageSourceTextToStrings(OleObject: Variant; Document: IDispatch; TextList: TStrings);
begin
  TextList.Clear;
  if DocumentLoaded(Document) then
  begin
    try
      TextList.Add(VarToStr(OleObject.Document.documentElement.innerText));
    except
    end;
  end;
end;

procedure ViewPageSourceText(OleObject: Variant; Document: IDispatch);
var
  TextLst: TStringList;
begin
  TextLst := TStringList.Create;
  try
    if DocumentLoaded(Document) then
    begin
      TextLst.Add(VarToStr(OleObject.Document.documentElement.innerText));
      MessageDlg(TextLst.Text, mtCustom, [mbOK], 0);
    end;
  finally
    TextLst.Free;
  end;
end;

function SaveDocToStrings(Document: IDispatch; var AStrings: TStrings): HResult;
var
  IpStream: IPersistStreamInit;
  AStream: TMemoryStream;
begin
  Result := S_FALSE;
  if not DocumentLoaded(Document) then
    Exit;
  AStream := TMemoryStream.Create;
  try
    IpStream := Document as IPersistStreamInit;
    if not Assigned(IpStream) then
      Result := S_FALSE
    else
      if Succeeded(IpStream.save(TStreamadapter.Create(AStream), True))
        then
      begin
        AStream.Seek(0, 0);
        AStrings.LoadFromStream(AStream);
        Result := S_OK;
      end;
  except
  end;
  AStream.Free;
end;

function SaveDocToStream(Document: IDispatch; var AStream: TStream): HResult;
var
  IpStream: IPersistStreamInit;
begin
  if DocumentLoaded(Document) then
  begin
    IpStream := Document as IPersistStreamInit;
    Result := IpStream.Save(TStreamAdapter.Create(AStream), True);
  end
  else
    Result := S_FALSE;
end;

function SaveDocToFile(Document: IDispatch; const Fname: string): HResult;
var
  PFile: IPersistFile;
begin
  Result := S_FALSE;
  if DocumentLoaded(Document) then
  begin
    PFile := Document as IPersistFile;
    Result := PFile.Save(StringToOleStr(FName), False);
  end;
end;

procedure PrintWithHeaderFooter(ControlInterface: IWebBrowser2; Header, Footer: PWideChar; Options: OLECMDEXECOPT);
var
  saBound: TSafeArrayBound;
  psaHeadFoot: PSafeArray;
  vaIn, vaOut: TVariantArg;
  vHeadStr, vFootStr: TVariantArg;
  rgIndex: LongInt;
begin
  try
    saBound.lLbound := 0;
    saBound.cElements := 2;
    psaHeadFoot := SafeArrayCreate(VT_VARIANT, 1, @saBound);
    vHeadStr.vt := VT_BSTR;
    vHeadStr.bstrVal := SysAllocString(Header);
    vFootStr.vt := VT_BSTR;
    vFootStr.bstrVal := SysAllocString(Footer);
    rgIndex := 0;
    OleCheck(SafeArrayPutElement(psaHeadFoot, rgIndex, vHeadStr));
    rgIndex := 1;
    OleCheck(SafeArrayPutElement(psaHeadFoot, rgIndex, vFootStr));
    vaIn.vt := VT_ARRAY or VT_BYREF;
    vaIn.parray := psaHeadFoot;
    ControlInterFace.ExecWB(OLECMDID_PRINT, Options,
      OleVariant(vaIn), OleVariant(vaOut));
    if vHeadStr.bstrVal <> nil then
      SysFreeString(vHeadStr.bstrVal);
    if vFootStr.bstrVal <> nil then
      SysFreeString(vFootStr.bstrVal);
  except
  end;
end;

procedure Print(ControlInterface: IWebBrowser2; bHideSetup: Boolean = False; bCustomHeaderFooter: Boolean = False; Header: string = ''; Footer: string = '');
var
  vaIn, vaOut: OleVariant;
begin
  if DocumentLoaded(ControlInterface.Document) then
  begin
    if bCustomHeaderFooter then
    begin
      if bHideSetup then
        PrintWithHeaderFooter(ControlInterface, TaskAllocWideString(Header), TaskAllocWideString(Footer), OLECMDEXECOPT_DONTPROMPTUSER)
      else
        PrintWithHeaderFooter(ControlInterface, TaskAllocWideString(Header), TaskAllocWideString(Footer), OLECMDEXECOPT_PROMPTUSER);
    end
    else
      if bHideSetup then
        ControlInterface.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut)
      else
        ControlInterface.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_PROMPTUSER, vaIn, vaOut)
  end;
end;

procedure PrintWithOptions(ControlInterface: IWebBrowser2; Document: IDispatch; UsePrintOptions, PrintOptionsEnabled, HideSetup: Boolean; var InvokingPageSetup: Boolean);
begin
  PrintingWithOptions := True;
  PageSetup(Document, UsePrintOptions, PrintOptionsEnabled, InvokingPagesetup);
  Print(ControlInterface, HideSetup);
end;

procedure PrintPreview(Webbrowser: IWebBrowser2);
// IE 5.5 only
var
  vaIn, vaOut: Olevariant;
begin
  if DocumentLoaded(Webbrowser.Document) then
    Webbrowser.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
end;

function OpenClient(Client: string): Boolean;
var
  s, params, Exec: string;
begin
  Result := False;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('Software\Clients\' + Client, False);
    S := ReadString('');
    CloseKey;
    OpenKey('Software\Clients\' + Client + '\' + S + '\shell\open\command', False);
    S := ReadString('');
    CloseKey;
    if S <> '' then
    begin
      if Pos('/', S) > 0 then
      begin
        Exec := system.Copy(S, 1, Pos('/', S) - 2);
        Params := system.Copy(s, Length(exec) + 1, length(S));
      end
      else
      begin
        Exec := S;
        Params := '';
      end;
      Result := True;
      ShellExecute(Application.handle, 'open', PChar(Exec), PChar(Params), '', SW_SHOW);
    end;
  finally
    Free;
  end;
end;

procedure PrintPreviewExtended(ControlInterface: IWebBrowser2; nCMDShow: Integer; HideSetup: Boolean);
var
  Preview_HWND, App_HWND: THandle;
  ClassName: array[0..255] of Char;
  StartTime, EndTime: DWORD; //Smot
  vaIn, vaOut: OleVariant;
begin
  if DocumentLoaded(ControlInterface.Document) then
  begin
    if HideSetup then
      ControlInterface.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut) //jerzy
    else
      ControlInterface.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_PROMPTUSER, vaIn, vaOut);
    Preview_HWND := 0;
    StartTime := GetTickCount;
    repeat
      App_HWND := GetForegroundWindow();
      GetClassName(App_HWND, ClassName, SizeOf(ClassName));
      if lstrcmp(@ClassName[0], @IE_PPREVIEWCLASS[1]) = 0 then
        Preview_HWND := App_HWND;
      Forms.Application.ProcessMessages;
      EndTime := GetTickCount;
    until (Preview_HWND <> 0) or (EndTime - StartTime > 7000);
    if Preview_HWND <> 0 then
      ShowWindow(Preview_HWND, nCmdShow);
  end;
end;

function PageSetup(Document: IDispatch; UsePrintOptions, PrintOptionsEnabled: Boolean; var InvokingPageSetup: Boolean): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := False;
  if DocumentLoaded(Document) then
  begin
    if PrintOptionsEnabled and UsePrintOptions then
      InvokingPageSetup := True;
    Result := InvokeCmd(Document, False, OLECMDID_PAGESETUP, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK
  end;
end;

procedure PrintSetup(ControlInterface: IWebBrowser2; HideSetup: Boolean);
var
  vaIn, vaOut: OleVariant;
begin
  if DocumentLoaded(ControlInterface.Document) then
  begin
    if HideSetup then
      ControlInterface.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut)
    else
      ControlInterface.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_PROMPTUSER, vaIn, vaOut)
  end;
end;

procedure GetPrintValues(WebBrowser: TEmbeddedWB; PrintOptions: TPrintOptions; Measure: TMeasure);
var
  S: string;
  Reg: TRegistry;
  {$IFDEF DELPHI7_UP}
    FS: TFormatSettings;
  {$ENDIF}


  function ReadMargin(key: string): Real;
  begin
    S := Reg.ReadString(key);
    if S = '' then
      S := '0.750000'; // <-- default margin value  by takeru_tk_81
    S := StringReplace(S, ' ', '', [rfReplaceAll]);

    if {$IFDEF DELPHI7_UP}FS.{$ENDIF}DecimalSeparator <> '.' then
      S := StringReplace(S, '.',{$IFDEF DELPHI7_UP}FS.{$ENDIF}DecimalSeparator ,[]);

    if Measure = mMetric then
      Result := StrToFloat(S) * InchToMetric
    else
      Result := StrToFloat(S);
  end;

begin
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\Microsoft\Internet Explorer\PageSetup', False) then
      begin
        with PrintOptions do
        begin
          Header := ReadString('header');
          Footer := ReadString('footer');
          Margins.Left := ReadMargin('margin_left');
          Margins.Right := ReadMargin('margin_right');
          Margins.Top := ReadMargin('margin_top');
          Margins.Bottom := ReadMargin('margin_bottom');
        end;
      end;
      Reg.Free;
    end;
  except

  end;
end;

function PrintMarginStr(Measure, RuntimeMeasure: TMeasure; M: Real): string;
begin
  if Measure <> RuntimeMeasure then
  begin
    if RuntimeMeasure = mMetric then
      Result := FloatToStr(M * InchToMetric)
    else
      Result := FloatToStr(M / InchToMetric);
  end
  else
    Result := FloatToStr(M);
end;

procedure RestorePrintValues;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\Microsoft\Internet Explorer\PageSetup', True) then
      begin
        WriteString('header', '&w&bPage &p of &P');
        WriteString('footer', '&u&b&d');
        WriteString('margin_left', '0.750000');
        WriteString('margin_right', '0.750000');
        WriteString('margin_top', '0.750000');
        WriteString('margin_bottom', '0.750000');
      end;
      Reg.Free;
    end;
  except
    MessageDlg('Error while writing page print values to the registry!', mtError, [mbOK], 0);
  end;
end;

function OpenDialog(WebBrowser: TEmbeddedWB; AOwner: TComponent): Boolean;
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(AOwner);
  try
    with OD do
    begin
      Filter := 'Internet Files|*.htm; *.html; *.url; *.mht; *.mhtml; *.php *.asp'
        + #10 + #13 + '|Image Files| *.gif;*.bmp;*.ico;*.jpg;*.png;*.wmf; *.emf; '
        + #10 + #13 + '|Text & Documents Files| *.txt;*.doc;*.xls;*.dot;'
        + #10 + #13 + '|Compressed Files| *.zip;'
        + #10 + #13 + '|XML Files| *.xml;'
        + #10 + #13 + '|Any Files|*.*';
      Options := Options + [ofShowHelp, ofEnableSizing];
      Title := 'Browser - Open Dialog';
      HelpContext := 0;
      Result := Execute;
      if Result then
        WebBrowser.Go(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

function SaveDialog(WebBrowser: TEmbeddedWB; AOwner: TComponent; ATitle: string = ''; AFilter: string = ''): string;
var
  SD: TSaveDialog;
begin
  SD := TSaveDialog.Create(AOwner);
  try
    with SD do
    begin
      if AFilter = '' then
        Filter := 'Internet Files|*.htm; *.html;*.mht; *.mhtml; *.php *.asp'
          + #10 + #13 + '|Text & Documents Files| *.txt;*.doc;*.xls;*.dot;'
          + #10 + #13 + '|XML Files| *.xml;'
          + #10 + #13 + '|Any Files|*.*'
      else
        Filter := AFilter;
      Options := Options + [ofShowHelp, ofEnableSizing];
      if ATitle = '' then
        Title := 'Browser - Save Dialog';
      HelpContext := 0;
      if Execute then
        Result := SD.FileName;
      if SD.FileName <> '' then
        WebBrowser.SaveToFile(SD.FileName);
    end;
  finally
    SD.Free;
  end;
end;

function SaveDialog(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, False, OLECMDID_SAVEAS, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function ShowInternetOptions(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, True, HTMLID_OPTIONS, 0, vaIn, vaOut) = S_OK;
end;

function ShowPageProperties(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin // OLECMDID_SHOWPAGEACTIONMENU
  Result := InvokeCmd(Document, False, OLECMDID_PROPERTIES, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut) = S_OK;
end;

function ShowOrganizeFavorites(Handle: THandle): Boolean;
begin
  Result := OrganizeFavorite(Handle, GetSpecialFolderPath(Handle, CSIDL_FAVORITES));
end;

procedure ShowImportExportFavoritesAndCookies(Handle: THandle);
begin
  SendMessage(Handle, WM_COMMAND, ID_IE_FILE_IMPORTEXPORT, 0);
end;

function ShowFindDialog(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, True, HTMLID_FIND, 0, vaIn, vaOut) = S_OK;
end;

procedure SaveImagesDialog(OleObject: Variant; Document: IDispatch);
var
  k, p: Integer;
  path, Source, dest, ext: string;
begin
  if DocumentLoaded(Document) then
  begin
 //        path := TBrowse4Folder.('Web Browser - Please select a destination folder' + #10 + #13
  //          + 'for the images', 'Desktop');
    MessageDlg(Path, mtCustom, [mbYes, mbAll, mbCancel], 0);
    begin
      for k := 0 to OleObject.Document.Images.Length - 1 do
      begin
        Source := OleObject.Document.Images.Item(k).Src;
        p := LastDelimiter('.', Source);
        ext := UpperCase(System.Copy(Source, p + 1, Length(Source)));
        if (ext = 'GIF') or (ext = 'JPG') or (ext = 'BMP') or (ext = 'PNG') then
        begin
          p := LastDelimiter('/', Source);
          dest := path + '/Images' + System.Copy(Source, p + 1, Length(Source));
          DownloadFile(Source, dest);
        end;
      end;
    end;
  end;
end;

function ViewPageSourceHtml(Document: IDispatch): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCmd(Document, True, HTMLID_VIEWSOURCE, 0, vaIn, vaOut) = S_OK;
end;

procedure SavePageTextDialog(AOwner: TComponent; OleObject: Variant; Document: IDispatch);
var
  sd: TSaveDialog;
  textStr: TStringList;
begin
  if not DocumentLoaded(Document) then Exit;
  textstr := TStringList.Create;
  try
    textStr.Add(VarToStr(OleObject.Document.documentElement.innerText));
    begin
      sd := TSaveDialog.Create(AOwner);
      try
        sd.Filter := 'Text file|*.txt|Word file|*.doc';
        sd.DefaultExt := 'txt';
        sd.FilterIndex := 1;
        sd.FileName := 'WebSiteText.txt';
        sd.Title := 'Web Site Text';
        if sd.Execute then
        begin
          textStr.SaveToFile(sd.FileName);
        end;
      finally
        sd.Free;
      end;
    end;
  finally
    textStr.Free;
  end;
end;

procedure ShellExecuteOpen(const sApplication: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(sApplication), nil, nil, SW_SHOW);
end;

procedure OpenOutlookMail;
begin
  ShellExecuteOpen('outlook.exe');
end;

procedure OpenOutlookExpressMail;
begin
  ShellExecuteOpen('msimn.exe');
end;

procedure OpenEudoraMail;
begin
  ShellExecuteOpen('eudora.exe');
end;

procedure OpenRegistryEditor;
begin
  ShellExecuteOpen('regedit.exe');
end;

function OpenNewsClient: Boolean;
begin
  Result := OpenClient('News');
end;

procedure OpenAddressBook;
begin
  ShellExecuteOpen('wab.exe');
end;

function OpenCalendar: Boolean;
begin
  Result := OpenClient('Calendar');
end;

function OpenNetMeeting: Boolean;
begin
  Result := OpenClient('Internet Call');
end;

procedure DoExploreFolder(Handle: THandle; Path: string);
begin
  ShellExecute(handle, 'explore', PChar(Path), nil, nil, SW_SHOWNORMAL);
end;

procedure OpenIEBrowserWithAddress(Handle: THandle);
begin
  SendMessage(Handle, WM_COMMAND, ID_IE_FILE_NEWWINDOW, 0);
end;

function OpenHotmailMail(WebBrowser: TEmbeddedWB): Boolean;
begin
  Result := True;
  Go(WebBrowser, 'http://lc1.law5.hotmail.passport.com/cgi-bin/login');
end;

function OpenGoogleMail(WebBrowser: TEmbeddedWB): Boolean;
begin
  Result := True;
  Go(WebBrowser, 'http://mail.google.com/mail/');
end;

function OpenYahooMail(WebBrowser: TEmbeddedWB): Boolean;
begin
  Result := True;
  Go(WebBrowser, 'http://mail.yahoo.com/');
end;

procedure GoSearchInGoogle(WebBrowser: TEmbeddedWB; SearchTerm: string);
const
  GOOGLE_QUERY = 'http://www.google.com/search?ie=ISO-8859-1&q=';
var
  sQuery: string;
begin
  sQuery := GOOGLE_QUERY + SearchTerm;
  Go(WebBrowser, sQuery);
end;

procedure GoSearchInMSN(WebBrowser: TEmbeddedWB; SearchTerm: string);
const
  MSN_QUERY = 'http://search.live.com/results.aspx?q=';
  MSN_Const = '&FORM=CBPW&first=1&noredir=1';
var
  sQuery: string;
begin
  sQuery := MSN_QUERY + SearchTerm + MSN_Const;
  Go(WebBrowser, sQuery);
end;

procedure GoSearchInYahoo(WebBrowser: TEmbeddedWB; SearchTerm: string);
const
  YAHOO_QUERY = 'http://search.yahoo.com/bin/search?p=';
var
  sQuery: string;
begin
  sQuery := YAHOO_QUERY + SearchTerm;
  WebBrowser.Go(sQuery);
end;

procedure Go(WebBrowser: TEmbeddedWB; Url: string);
var
  _URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  _URL := Url;
  Flags := 0;
  TargetFrameName := 0;
  Postdata := 0;
  Headers := 0;
  if (Trim(_URL) <> '') then
    WebBrowser.Navigate2(_URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure GoWithQueryDetails(WebBrowser: TEmbeddedWB; Url, Query: string);
var
  _URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  _URL := Url + Query;
  TargetFrameName := 0;
  headers := StringtoVarArray('Content-Type:application/x-www-form-urlencoded'#13#10);
  Postdata := StringToVarArray('version=current&name=myname' + #13#10);
  Flags := 0;
  WebBrowser.Navigate2(_URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure GoNoHistory(WebBrowser: TEmbeddedWB; const URL: string);

  function StrToChr(Str: string; Pos: Integer): Char;
  begin
    Result := Str[Pos];
  end;

var
  Flags: OleVariant;
  HistoryStg: IUrlHistoryStg;
begin
  Flags := navNoHistory;
  WebBrowser.Navigate(WideString(URL), Flags);
  Wait(WebBrowser);
  HistoryStg := CreateComObject(CLSID_CUrlHistory) as IUrlHistoryStg;
  HistoryStg.DeleteUrl(PWideChar(StrToChr(URL, 0)), 0);
end;

procedure NavigatePidl(WebBrowser: TEmbeddedWB; pidl: PItemIdList);
var
  VaEmpty, vaPidl: OleVariant;
  psa: PSafeArray;
  cbData: UINT;
begin
  cbdata := GetPIDLSize(pidl);
  psa := SafeArrayCreateVector(VT_UI1, 0, cbData);
  if (psa <> nil) then
  begin
    CopyMemory(psa.pvData, pidl, cbData);
    VariantInit(vaPidl);
    TVariantArg(vaPidl).vt := VT_ARRAY or VT_UI1;
    TVariantArg(vaPidl).parray := psa;
    WebBrowser.Navigate2(vaPidl, vaEmpty, vaEmpty, vaEmpty, vaEmpty);
    VariantClear(vaPidl);
  end;
end;

function GetFrameFromDocument(SourceDoc: IHTMLDocument2; FrameNo: Integer): IWebBrowser2;
var //by Aladin
  OleContainer: IOleContainer;
  enum: ActiveX.IEnumUnknown;
  unk: IUnknown;
  Fetched: PLongint;
begin
  Result := nil;
  Fetched := nil;
  if DocumentLoaded(SourceDoc) then
  begin
    OleContainer := SourceDoc as IOleContainer;
    OleContainer.EnumObjects(OLECONTF_EMBEDDINGS or OLECONTF_OTHERS, Enum);
    Enum.Skip(FrameNo);
    Enum.Next(1, Unk, Fetched);
    if Supports(Unk, IWebBrowser2, Result) then //perva 2008/12/10
      Result := Unk as IWebBrowser2;
  end;
end;

procedure GoAboutBlank(WebBrowser: TEmbeddedWB);
begin
  WebBrowser.Go('about:blank');
  Wait(WebBrowser);
end;

procedure SendPageInMailAsAttachment(WebBrowser: TEmbeddedWB; AOwner: TComponent; Document: IDispatch; mFileName, mSubject, mBody: string);
begin
  WebBrowser.SaveToFile(mFileName);
  Sleep(800);
  with TEwbMapiMail.Create(AOwner) do
  begin
    try
      Subject := mSubject;
      Body := mBody;
      Attachments.Add(mFileName);
      EditDialog := True;
      Send;
    finally
      // Free;
    end;
  end;
end;

procedure GoDownloadFile(WebBrowser: TEmbeddedWB; URL: string);
var
  Flags: OleVariant;
begin
  Flags := navNoHistory or navNoReadFromCache or navNoWriteToCache
    or navAllowAutosearch or navBrowserBar;
  WebBrowser.Navigate(URL, Flags);
end;

function DownloadFile(SourceFile, TargetFile: string): Boolean;
begin
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(TargetFile), 0, nil) = 0;
  except
    Result := False;
  end;
end;

procedure GoDownloadMaskedFile(SourceFile, TargetFile: string; Notify: Boolean);
begin
  if Notify then
  begin
    if DownloadFile(SourceFile, TargetFile) then
      MessageBox(0, PChar('Downloading: ' + SourceFile + #10 + #13 +
        'To: ' + TargetFile + #10 + #13 + 'was successfully finished.'),
        PChar('Download successful.'), MB_OK)
    else
      MessageBox(0, PChar(
        'An error ocurred while downloading the file.' + SourceFile),
        PChar('Downloading Error!!'), MB_ICONERROR or MB_OK);
  end
  else
    DownloadFile(SourceFile, TargetFile);
end;

procedure AddToFavorites(URL, Title: string);
// The URL parameter must specify a valid URL using HTTP, Secure Hypertext Transfer Protocol (HTTPS),
// or File Transfer Protocol (FTP) protocols only. Calling the IShellUIHelper::AddFavorite method with a
// file:// or javascript: URL returns E_ACCESSDENIED.
const
  CLSID_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
var
  ShellUIHelper: ISHellUIHelper;
  Url1, Title1: OleVariant;
   Res : HRESULT;
begin
  if (Trim(URL) <> '') and (Trim(Title) <> '') then
  begin
    Title1 := Title;
    Url1 := Url;
    Res := CoCreateInstance(CLSID_SHELLUIHELPER, nil, CLSCTX_INPROC_SERVER, IID_IShellUIHelper, ShellUIHelper);
    if SUCCEEDED(Res) then
    try
      ShellUIHelper.AddFavorite(URL1, Title1);
    except
    end;
  end;
end;

function GetFavoritesPath: string;
begin
  Result := GetShellFolderPath('Favorites');
end;

function GetCookiesPath: string;
begin
  Result := GetShellFolderPath('Cookies');
end;

function GetHistoryPath: string;
begin
  Result := GetShellFolderPath('History');
end;

function GetCachePath: string;
begin
  Result := GetShellFolderPath('Cache');
end;

function GetShellFolderPath(FolderName: Widestring): string;
const
  REG_PATH = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  with Reg do
  try
    Rootkey := HKEY_CURRENT_USER;
    OpenKey(REG_PATH, False);
    if (ValueExists(FolderName)) and not (length(trim(ReadString(FolderName))) = 0) then
      Result := ReadString(FolderName);
  finally
    CloseKey;
    Free;
  end;
end;

function GetSpecialFolderPath(CallerHandle: THandle; CSIDL: Integer): PChar;
var
  exInfo: TShellExecuteInfo;
  Buf: PChar;
begin
  FillChar(exInfo, SizeOf(exInfo), 0);
  with exInfo do
  begin
    cbSize := SizeOf(exInfo);
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_IDLIST;
    Wnd := CallerHandle;
    nShow := SW_SHOWNORMAL;
    Buf := StrAlloc(MAX_PATH);
    try
{$IFDEF UNICODE}
      FillChar(Buf^, MAX_PATH * SizeOf(Char), 0);
{$ELSE}
      FillChar(Buf^, MAX_PATH, 0);
{$ENDIF UNICODE}
      if SHGetSpecialFolderPath(wnd, Buf, CSIDL, True) then
        Result := Buf
      else
        Result := '';
    finally
      // StrDispose(Buf);
    end;
  end;
end;

function GetIEHomePage: string;
var
  HomePage: string;
begin
  HomePage := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey('\Software\Microsoft\Internet Explorer\Main', False);
    HomePage := ReadString('Start Page');
    CloseKey;
  finally
    Free;
  end;
  Result := HomePage;
end;

function GetCachedFileFromURL(ItemUrl: string): string;
var
  IntCacheInfo: PInternetCacheEntryInfo;
  CacheEntry, dwEntrySize, dwLastError: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(IntCacheInfo, dwEntrySize);
  CacheEntry := FindFirstUrlCacheEntry(nil, IntCacheInfo^, dwEntrySize);
  if (CacheEntry <> 0) and (ItemUrl = IntCacheInfo^.lpszSourceUrlName) then
    Result := IntCacheInfo^.lpszLocalFileName;
  FreeMem(IntCacheInfo);
  if Result = '' then
    repeat
      dwEntrySize := 0;
      FindNextUrlCacheEntry(CacheEntry, TInternetCacheEntryInfo(nil^), dwEntrySize);
      dwLastError := GetLastError();
      if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        GetMem(IntCacheInfo, dwEntrySize);
        if (FindNextUrlCacheEntry(CacheEntry, IntCacheInfo^, dwEntrySize)) then
        begin
          if ItemUrl = IntCacheInfo^.lpszSourceUrlName then
          begin
            Result := IntCacheInfo^.lpszLocalFileName;
            Break;
          end;
        end;
        FreeMem(IntCacheInfo);
      end;
    until (dwLastError = ERROR_NO_MORE_ITEMS);
end;

function OrganizeFavorite(h: THandle; Path: PAnsiChar): Boolean; stdcall;
  external 'shdocvw.dll' name 'DoOrganizeFavDlg'; overload;

{$IFDEF UNICODE}

function OrganizeFavorite(h: THandle; Path: PWideChar): Boolean;
begin
  Result := OrganizeFavorite(h, PAnsiChar(AnsiString(UnicodeString(Path))));
end;

{$ENDIF UNICODE}

function URLFromFavorites(const dotURL: string): string;
begin
  Result := '';
  with TIniFile.Create(dotURL) do
  try
    try
      Result := ReadString('InternetShortcut', 'URL', '');
    except;
    end;
  finally
    Free;
  end;
end;

function UrlFromHistory(ShellFolder: IShellFolder; pidl: PItemIDList): string;
var
  Handle: THandle;
  Info: IQueryInfo;
  W: PWideChar;
begin
  Result := '';
  Handle := 0;
  Info := nil;
  ShellFolder.GetUIObjectOf(Handle, 1, pidl, IID_IQUERYINFO, nil, Pointer(Info));
  if Assigned(Info) then
  begin
    Info.GetInfoTip(0, w);
    Result := W;
  end;
  Result := Trim(System.Copy(Result, Pos(#10, Result) + 1, length(Result)));
end;

function GetDefaultBrowserFromRegistry: string;
var
  Reg: TRegistry;
  KeyName: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    KeyName := 'htmlfile\shell\open\command';
    if Reg.OpenKey(KeyName, False) then
    begin
      Result := Reg.ReadString('');
      Reg.CloseKey;
    end
    else
      Result := 'No default browser found.';
  finally
    Reg.Free;
  end;
end;

function GetIPAndHostName(var HostName, IPaddr, WSAErr: string): Boolean;
var
  WSAResult: Integer;
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  Host: AnsiString;
  SockAddr: TSockAddrIn;
begin
  Result := False;
  WSAResult := WSAStartup(MakeWord(1, 1), WSAData);
  if WSAResult <> 0 then
  begin
    WSAErr := 'Winsock is not responding."';
  end else
  try
    if Host = '' then
    begin
      SetLength(Host, MAX_PATH);
      GetHostName(PAnsiChar(Host), MAX_PATH);
    end;
    HostEnt := GetHostByName(PAnsiChar(Host));
    if HostEnt <> nil then
    begin
      HostName := string(AnsiString(Host));
      SetLength(HostName, StrLen(PChar(HostName)));
      SockAddr.sin_addr.S_addr := Longint(PLongint(HostEnt^.h_addr_list^)^);
      IPaddr := string(AnsiString(inet_ntoa(SockAddr.sin_addr)));
      Result := True;
    end else
    begin
      begin
        case WSAGetLastError of
          WSANOTINITIALISED: WSAErr := 'WSANotInitialised';
          WSAENETDOWN: WSAErr := 'WSAENetDown';
          WSAEINPROGRESS: WSAErr := 'WSAEInProgress';
        end;
      end;
    end;
  finally
    WSACleanup;
  end;
end;

function CreateNewMail: Boolean;
var
  em_subject, em_body, em_mail: string;
begin
  em_subject := '';
  em_body := '';
  em_mail := 'mailto:?subject=' + em_subject + '&body=' + em_body;
  Result := ShellExecute(0, 'open', PChar(em_mail), nil, nil, SW_SHOWNORMAL) > 32;
end;

procedure SendUrlInMail(LocationURL, LocationName: WideString);
begin
  with TEwbMapiMail.Create(nil) do
  begin
    try
      Subject := LocationName;
      Body := LocationURL;
      EditDialog := True;
      Send;
    finally
    end;
  end;
end;


function SearchText(WebBrowser: TEmbeddedWB; Document: IDispatch; const Value: string; aTypeSearch: Integer; const iPos: Integer = 1): IHTMLTxtRange;
//by JJM
{ aTypeSearch can have the following values
(*
0 Default. Match partial words.
1 Match backwards.
2 Match whole words only.
4 Match case.
*)
}
var
  B: Boolean;
begin
  Wait(WebBrowser);
  Result := nil;
  try
    if DocumentLoaded(Document) then
      if Assigned((Document as IHTMLDocument2).body) then
      begin
        Result := ((Document as IHTMLDocument2).body as IHTMLBodyElement).CreateTextRange;
        if Result.moveStart('character', ipos) = S_OK then
          B := Result.findText(Value, 1, aTypeSearch)
        else
          B := Result.findText(Value, iPos, aTypeSearch);
        if B then
          Result.ScrollIntoView(True)
        else
          Result := nil;
      end;
  except
    on e: Exception do ;
  end;
end;

function SearchString(Webbrowser: TEmbeddedWB; const strText: string): Boolean;
var
  tr: IHTMLTxtRange;
begin
  Wait(WebBrowser);
  Result := False;
  try
    if Assigned(Webbrowser.Document) then
    begin
      tr := ((Webbrowser.Document as IHTMLDocument2).body as IHTMLBodyElement).createTextRange;
      Result := tr.findText(strText, 1, 0);
    end;
  except
    on e: Exception do
      ;
  end;
end;

function DoSearchAndHighlight(Document: IDispatch; sFind: string;
  Flags: TSearchFlags = []; cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll): Integer;
var
  Doc2: IHTMLDocument2;
  pElem: IHTMLElement;
  pBodyelem: IHTMLBodyElement;
  pTxtRange: IHTMLTxtRange;
  searchdir, searchcase, iMatches: Integer;
begin
  iMatches := 0;
  if (Length(sFind) <> 0) and
    Supports(Document, IHTMLDocument2, Doc2) then
  begin
    searchdir := 1;
    searchcase := 0;
    //Set up search case
    if (sfMatchWholeWord in Flags) and (sfMatchCase in Flags) then
      searchcase := 6
    else if sfMatchWholeWord in Flags then
      searchcase := 2
    else if sfMatchCase in Flags then
      searchcase := 4;

    pElem := Doc2.body;
    if (pElem <> nil) then
    begin
      pBodyelem := pElem as IHTMLBodyElement;
      if (pBodyelem <> nil) then
      begin
        pTxtRange := pBodyelem.createTextRange();
        if (pTxtRange <> nil) then
        begin
          while (pTxtRange.findText(sFind, searchdir, searchcase)) do
          begin
            if (cbackColor <> '') then
              pTxtRange.execCommand('BackColor', False, cbackColor);
            if (cForeColor <> '') then
              pTxtRange.execCommand('ForeColor', False, cForeColor);
            pTxtRange.moveStart('Character', 1);
            pTxtRange.moveEnd('Textedit', 1);
            iMatches := iMatches + 1;
            if (iMatches = 1) and (ScrollIntoView = sivFirstMatch) then
             pTxtRange.scrollIntoView(True);
          end;
          if (iMatches > 1) and (ScrollIntoView = sivLastMatch) then
             pTxtRange.scrollIntoView(True);
        end;
      end;
    end;
  end;
  Result := iMatches;
end;

procedure SearchAndHighlight(Document: IDispatch;
  AText: string; const ACaption, APrompt: string; Flags: TSearchFlags = [];
   cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll; ShowInputQuery: Boolean = True); overload;
var
// tr: IHTMLTxtRange;
  FrameCount, i: Integer;
  Wb2: IWebBrowser2;
begin
  if DocumentLoaded(Document) then
  begin
    if ShowInputQuery then
      if not InputQuery(ACaption, APrompt, AText) then Exit;

    if Length(aText) = 0 then Exit;
    try
      FrameCount := FrameCountFromDocument(Document as IHTMLDocument2);
      if FrameCount > 0 then
      begin
        for i := 0 to Pred(FrameCount) do
        begin
          Wb2 := GetFrameFromDocument(Document as IHTMLDocument2, i);
          if Assigned(Wb2) then
            SearchAndHighlight(Wb2.Document, AText, ACaption, APrompt, Flags,
             cbackColor, cForeColor, ScrollIntoView, False);
        end;
      end
      else
      begin
        DoSearchAndHighlight(Document, AText, Flags,
          cbackColor, cForeColor, ScrollIntoView);
      {  tr := ((Document as IHTMLDocument2).body as IHTMLBodyElement).createTextRange;
        while tr.findText(aText, 1, 0) do
        begin
          tr.pasteHTML('<span style="background-color: ' + aColor + '; font-weight: bolder;">' +
            tr.htmlText + '</span>');
          tr.scrollIntoView(True);
        end; }
      end;
    except
    end;
  end;
end;

procedure SearchAndHighlight(Document: IDispatch; aText: string; Flags: TSearchFlags = [];
  cbackColor: string = 'yellow'; cForeColor: string = '';
  ScrollIntoView: TScrollIntoView = sivNoScroll); overload;
begin
  SearchAndHighlight(Document, '', '', aText, Flags, cbackColor, cForeColor, ScrollIntoView, False);
end;

{function FillForm(OleObject: Variant; FieldName: string; Value: string): Boolean;
var
  I, j: Integer;
  FormItem: Variant;
begin
  Result := False;
  if not DocumentLoaded(OleObject.Document) or OleObject.Document.all.tags('FORM').Length = 0 then
    Exit;
  for I := 0 to OleObject.Document.forms.Length - 1 do
  begin
    FormItem := OleObject.Document.forms.Item(I);
    for j := 0 to FormItem.Length - 1 do
    begin
      try
        if (FormItem.Item(j).Name = FieldName) and
          (FormItem.Item(j).Name <> 'length') then
        begin
          FormItem.Item(j).Value := Value;
          Result := True;
        end;
      except
        Exit;
      end;
    end;
  end;
end;  }

procedure SetTextAreaValue(Document: IDispatch; sName, sValue: string; Options: TFindOptions);
var
  Doc2: IHTMLDocument2;
  i: Integer;
  field: IHTMLElement;
  textarea: IHTMLTextAreaElement;
begin
  if Supports(Document, IHTMLDocument2, Doc2) then
    for i := 0 to Doc2.all.length - 1 do
    begin
      field := Doc2.all.item(i, '') as IHTMLElement;
      if Assigned(field) then
      begin
        if SameText(field.tagName, 'TEXTAREA') then
        begin
          textarea := field as IHTMLTextAreaElement;
          if Assigned(textarea) then
          begin
            if ((frWholeWord in Options) and (sName = textarea.Name))
              or ((Options = []) and (AnsiPos(sName, textarea.Name) <> 0)) then
              textarea.Value := sValue;
          end;
        end;
      end;
    end;
end;

function FillForm(Document: IDispatch; FieldName: string; FieldValue: string; ElementNr: Integer = -1): Boolean; overload;
var
  Inputs: IHTMLElementCollection;
  HTMLElement: IHTMLElement;
  TagName: string;
  k, iItemNr, iInputCount: Integer;
begin
  Result := False;
  Inputs := IHTMLDocument3(Document).getElementsByName(FieldName);
  if Assigned(Inputs) then
  begin
    try
      if ElementNr = -1 then
        iInputCount := Inputs.Length
      else
        iInputCount := ElementNr;

      if iInputCount = -1 then iInputCount := 0;

      for k := 0 to iInputCount - 1 do
      begin
        if ElementNr = -1 then
          iItemNr := k
        else
          iItemNr := ElementNr;

        HTMLElement := Inputs.item(iItemNr, '') as IHTMLElement;
        if Assigned(HTMLElement) then
        begin
          TagName := AnsiUpperCase(HTMLElement.tagName);
          if TagName = 'INPUT' then
          begin
            (HTMLElement as IHTMLInputElement).Value := FieldValue;
            Result := True;
            Exit;
          end
          else if TagName = 'SELECT' then
          begin
            (HTMLElement as IHTMLSelectElement).Value := FieldValue;
            Result := True;
            Exit;
          end
          else if TagName = 'TEXTAREA' then
          begin
            (HTMLElement as IHTMLTextAreaElement).Value := FieldValue;
            Result := True;
            Exit;
          end;
        end;
        if ElementNr <> -1 then Exit;
      end;
    except
    end;
  end;
end;

function FillForm(WebBrowser: TEmbeddedWB; FieldName: string; FieldValue: string; ElementNr: Integer = -1): Boolean; overload;
var
  Doc3: IHTMLDocument3;
begin
  Result := False;
  if Assigned(WebBrowser.Document) and
    (Succeeded(WebBrowser.Document.QueryInterface(IHTMLDocument3, Doc3))) then
  begin
    Result := FillForm(Doc3, FieldName, FieldValue, ElementNr)
  end;
end;


function FillForm(WebBrowser: TEmbeddedWB; FieldName: string; FieldValue: string; Value: Boolean): Boolean;
var
  I, j: Integer;
  FormItem: Variant;
begin
  Result := False;
  if not DocumentLoaded(WebBrowser.Document) then
    if WebBrowser.OleObject.Document.all.tags('FORM').Length = 0 then
      if (FieldName = '') and (FieldValue = '') then
        for I := 0 to WebBrowser.OleObject.Document.forms.Length - 1 do
        begin
          FormItem := WebBrowser.OleObject.Document.forms.Item(I);
          for j := 0 to FormItem.Length - 1 do
          begin
            try
              if (FormItem.Item(j).Name = FieldName) or (Fieldname = '') then
                if (FormItem.Item(j).Value = FieldValue) or (Fieldvalue = '') then
                begin
                  FormItem.Item(j).checked := Value;
                  Result := True;
                end;
            except
              Continue;
            end;
          end;
        end;
end;

procedure ClickInputImage(WebBrowser: TEmbeddedWB; ImageURL: string);
var
  iDoc: IHTMLDocument2;
  iDisp: IDispatch;
  iColl: IHTMLElementCollection;
  InputImage: htmlInputImage;
  i: Integer;
begin
  if WebBrowser.DocumentLoaded then
  begin
    if Supports(WebBrowser.Document, IHTMLDocument2, iDoc) then
    begin
      iDisp := iDoc.all.tags('INPUT');
      if Assigned(iDisp) then
      begin
        if Supports(iDisp, IHTMLElementCollection, iColl) then
        begin
          ImageURL := AnsiUpperCase(ImageURL);
          for i := 1 to iColl.Get_length do
          begin
            iDisp := iColl.item(Pred(i), 0);
            if Supports(iDisp, HTMLInputImage, ImageURL) then
            begin
              if Pos(ImageURL, AnsiUpperCase(InputImage.src)) <> 0 then
              begin
                InputImage.Click;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GetFieldValue(OleObject: Variant; FieldName: string): string;
var
  I, j: Integer;
  FormItem: Variant;
begin
  Result := '';
  if DocumentLoaded(OleObject.Document) then
    if OleObject.Document.all.tags('FORM').Length = 0 then
      for I := 0 to OleObject.Document.forms.Length - 1 do
      begin
        FormItem := OleObject.Document.forms.Item(I);
        for j := 0 to FormItem.Length - 1 do
        begin
          try
            if FormItem.Item(j).Name = FieldName then
              Result := FormItem.Item(j).Value;
          except
            Continue;
          end;
        end;
      end;
end;

procedure FillIEFormAndExcecute;
var
  ShellWindow: IShellWindows;
  IWeb: IWebBrowser2;
  spDisp: IDispatch;
  IDoc1: IHTMLDocument2;
  Document: Variant;
  k, m: Integer;
  ovElements: OleVariant;
  i: Integer;
begin
  ShellWindow := CoShellWindows.Create;
  // get the running instance of Internet Explorer
  for k := 0 to ShellWindow.Count do
  begin
    spDisp := ShellWindow.Item(k);
    if spDisp = nil then
      Continue;
    // QueryInterface determines if an interface can be used with an object
    spDisp.QueryInterface(IWebBrowser2, IWeb);

    if IWeb <> nil then
    begin
      IWeb.Document.QueryInterface(IHTMLDocument2, iDoc1);
      if iDoc1 <> nil then
      begin
        IWeb := ShellWindow.Item(k) as IWebBrowser2;
        begin
          Document := IWeb.Document;
            // count forms on document and iterate through its forms
          for m := 0 to Document.Forms.Length - 1 do
          begin
            ovElements := Document.Forms.Item(m).Elements;
            // iterate through elements
            for i := 0 to ovElements.Length - 1 do
            begin
              // when input fieldname is found, try to fill out
              try
                if (CompareText(ovElements.Item(i).tagName, 'INPUT') = 0) and
                  (CompareText(ovElements.Item(i).type, 'text') = 0) then
                begin
                  ovElements.Item(i).Value := 'FindWindow';
                end;
              except
              end;
              // when Submit button is found, try to click
              try
                if (CompareText(ovElements.Item(i).tagName, 'INPUT') = 0) and
                  (CompareText(ovElements.Item(i).type, 'SUBMIT') = 0) and
                  (ovElements.Item(i).Value = 'Search') then // Suchen for German
                begin
                  ovElements.Item(i).Click;
                end;
              except
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure ClearHistory;
var
  HistoryStg: IUrlHistoryStg2;
begin
  HistoryStg := CreateComObject(CLSID_CUrlHistory) as IUrlHistoryStg2;
  HistoryStg.ClearHistory;
end;

function DeleteFirstCacheEntry(var H: THandle): DWORD;
var
  T: PInternetCacheEntryInfo;
  D: DWord;
begin
  Result := S_OK;
  H := 0;
  D := 0;
  FindFirstUrlCacheEntryEx(nil, 0, URLCACHE_FIND_DEFAULT_FILTER, 0, nil, @D, nil, nil, nil);
  GetMem(T, D);
  try
    H := FindFirstUrlCacheEntryEx(nil, 0, URLCACHE_FIND_DEFAULT_FILTER, 0, T, @D, nil, nil, nil);
    if (H = 0) then
      Result := GetLastError
    else
      DeleteUrlCacheEntry(T^.lpszSourceUrlname);
  finally
    FreeMem(T, D)
  end;
end;

function DeleteNextCacheEntry(H: THandle): DWORD;
var
  T: PInternetCacheEntryInfo;
  D: DWORD;
begin
  Result := S_OK;
  D := 0;
  FindnextUrlCacheEntryEx(H, nil, @D, nil, nil, nil);
  GetMem(T, D);
  try
    if not FindNextUrlCacheEntryEx(H, T, @D, nil, nil, nil) then
      Result := GetLastError
    else
      DeleteUrlCacheEntry(T^.lpszSourceUrlname);
  finally
    FreeMem(T, D)
  end;
end;

procedure ClearCache;
var
  H: THandle;
begin
  if DeleteFirstCacheEntry(H) = S_OK then
    repeat
    until DeleteNextCacheEntry(H) = ERROR_NO_MORE_ITEMS;
  FindCloseUrlCache(H);
end;

procedure ClearTypedUrls;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    DeleteKey('Software\Microsoft\Internet Explorer\TypedURLs');
  finally
    Free;
  end;
end;

function CheckOnlineStatus: Boolean;
var
  dwConnectionTypes: Integer;
begin
  Result := False;
  try
    dwConnectionTypes := INTERNET_CONNECTION_MODEM + INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
    Result := InternetGetConnectedState(@dwConnectionTypes, 0);
  except
  end;
end;

procedure SetGlobalOffline(Value: Boolean);
const
  INTERNET_STATE_DISCONNECTED_BY_USER = $10;
  ISO_FORCE_DISCONNECTED = $1;
  INTERNET_STATE_CONNECTED = $1;
var
  ci: TInternetConnectedInfo;
  dwSize: DWORD;
begin
  dwSize := SizeOf(ci);
  if Value then
  begin
    ci.dwConnectedState := INTERNET_STATE_DISCONNECTED_BY_USER;
    ci.dwFlags := ISO_FORCE_DISCONNECTED;
  end else
  begin
    ci.dwFlags := 0;
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
  end;
  InternetSetOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ci, dwSize);
end;

procedure WorkOffline();
begin
  SetGlobalOffline(False);
end;

procedure WorkOnline();
begin
  SetGlobalOffline(True);
end;

function IsGlobalOffline: Boolean;
var
  dwState: DWORD;
  dwSize: DWORD;
begin
  dwState := 0;
  dwSize := SizeOf(dwState);
  Result := False;
  if (InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @dwState, dwSize)) then
    Result := ((dwState and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0);
end;

function GetTLDFromHost(Host: string): string;
var
  i, Dots: Integer;
begin
  Dots := 0;
  for i := Length(Host) downto 1 do
  begin
    if Copy(Host, i, 1) = '.' then
      Inc(Dots);
    if Dots = 2 then
      break;
    Result := Copy(Host, i, 1) + Result;
  end;
end;

function CheckIfInRestricredList(const Host: string; SecureSite: Boolean): Boolean;
const
  Path = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\';
var
  TLD: string;
begin // todo: check for IPs IN RANGES
  Result := False;
  TLD := GetTLDFromHost(Host);
  with TRegistry.Create(KEY_READ) do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      if not OpenKey(Path + 'Domains' + '\' + TLD + '\' +
        Copy(Host, 1, Length(Host) - Length(TLD) - 1), False) then
      begin
        CloseKey;
        if not OpenKey(Path + 'EscDomains' + '\' + TLD + '\' +
          Copy(Host, 1, Length(Host) - Length(TLD) - 1), False) then // found on IE6, W2003
        begin
          CloseKey;
          Exit;
        end;
      end;
      if SecureSite then
        Result := ReadInteger('https') = 4
      else
        Result := ReadInteger('http') = 4
    finally
      CloseKey;
      Free;
    end;
  end;
end;

function CheckIfInTrustedList(const Host: string; SecureSite: Boolean): Boolean;
const
  Path = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\';
var
  TLD: string;
begin // todo: check for IPs in RANGES
  Result := False;
  TLD := GetTLDFromHost(Host);
  with TRegistry.Create(KEY_READ) do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      if not OpenKey(Path + 'Domains' + '\' + TLD + '\' +
        Copy(Host, 1, Length(Host) - Length(TLD) - 1), False) then
      begin
        CloseKey;
        if not OpenKey(Path + 'EscDomains' + '\' + TLD + '\' +
          Copy(Host, 1, Length(Host) - Length(TLD) - 1), False) then // found on IE6, W2003
        begin
          CloseKey;
          Exit;
        end;
      end;
      if SecureSite then
        Result := ReadInteger('https') = 2
      else
        Result := ReadInteger('http') = 2
    finally
      CloseKey;
      Free;
    end;
  end;
end;

procedure AddToTrustedSiteList(WebBrowser: TEmbeddedWB; const URL: string);
const
  REG_PATH = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\Domains';
var
  Reg: TRegistryIniFile;
begin
  if AnsiPos('HTTPS', AnsiUpperCase(URL)) = 0 then
    MessageDlg('Only sites with https:// prefix (secured sites) can be added to the trusted sites list zone!', mtError, [mbOK], 0)
  else
  begin
    try
      Reg := TRegistryIniFile.Create(REG_PATH);
      try
        Reg.WriteInteger(URL, 'https', (2));
      finally
        Reg.Free;
      end;
    except
    end;
  end;
end;

procedure AddToRestrictedSiteList(WebBrowser: TEmbeddedWB; const URL: string);
const
  REG_PATH = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\Domains';
var
  st: string;
  I: Integer;
  Reg: TRegistryIniFile;
begin
  I := LastDelimiter(':', Url) + 2;
  st := Copy(Url, I + 1, MaxInt);
  if AnsiPos('www', st) > 0 then
  begin
    I := 4;
    st := Copy(st, I + 1, MaxInt);
  end;
  try
    Reg := TRegistryIniFile.Create(REG_PATH);
    try
      Reg.WriteInteger(st, '*', (4));
    finally
      Reg.Free;
    end;
  except
  end;
end;


function GetZoneAttributes(const URL: string): TZoneAttributes;
var
  dwZone: Cardinal;
  ZoneAttr: TZoneAttributes;
var
  ZoneManager: IInternetZoneManager;
  SecManager: IInternetSecurityManager;
begin
  ZeroMemory(@ZoneAttr, SizeOf(TZoneAttributes));
  if CoInternetCreateSecuritymanager(nil, SecManager, 0) = S_OK then
    if CoInternetCreateZoneManager(nil, ZoneManager, 0) = S_OK then
    begin
      SecManager.MapUrlToZone(PWideChar(WideString(URL)), dwZone, 0);
      ZoneManager.GetZoneAttributes(dwZone, Result);
    end;
end;

function GetZoneIconToForm(LocationURL: string; Caption, Hint: string): Boolean;
var
  ZoneAttr: TZoneAttributes;
  ZoneIcon: TIcon;
begin
  ZoneAttr := GetZoneAttributes(LocationURL);
  ZoneIcon := TIcon.Create;
  try
    GetZoneIcon(ZoneAttr.szIconPath, ZoneIcon);
    Caption := ZoneAttr.szDisplayName;
    Hint := ZoneAttr.szDisplayName;
    Forms.Application.Icon := ZoneIcon;
  finally
    ZoneIcon.Free;
  end;
  Result := True;
end;

procedure GetZoneIcon(IconPath: string; var Icon: TIcon);
var
  FName, ImageName: string;
  h: hInst;
begin
  FName := Copy(IconPath, 1, Pos('#', IconPath) - 1);
  ImageName := Copy(IconPath, Pos('#', IconPath), Length(IconPath));
  h := LoadLibrary(PChar(FName));
  try
    if h <> 0 then
      Icon.Handle := LoadImage(h, PChar(ImageName), IMAGE_ICON, 16, 16, 0);
  finally
    FreeLibrary(h);
  end;
end;

function GetUrlSecurityZone(LocationURL: string; var ZoneName, ZoneDescription: string; var Icon: TIcon): Boolean;
var
  ZoneAttr: TZoneAttributes;
begin
  Assert(Icon <> nil);
  ZoneAttr := GetZoneAttributes(LocationURL);
  try
    try
      GetZoneIcon(ZoneAttr.szIconPath, Icon);
      ZoneName := ZoneAttr.szDisplayName;
      ZoneDescription := ZoneAttr.szDescription;
      Result := True;
    except
      Result := False;
    end;
  finally
  end;
end;

function GetSSLStatus(OleObject: Variant; LocationURL: string; var SSLName, SSLDescription: string): Boolean;
begin
  Result := False;
  if (Pos('https://', LocationURL) > 0) then
  begin
    if OleObject.Document.Location.Protocol = 'https:' then
    begin
      SSLName := 'SSL';
      SSLDescription := 'It is a secure web page.';
      Result := True;
    end;
  end
  else
  begin
    SSLName := 'None';
    SSLDescription := 'The page is not secured.';
    Result := False;
  end
end;

function SetProxy(UserAgent, Address, Bypass: string): Boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  hInternet: Pointer;
  Options: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  Result := False;
  dwBufSize := SizeOf(list);
  list.dwSize := SizeOf(list);
  list.pszConnection := nil;
  list.dwOptionCount := High(Options);

  Options[1].dwOption := INTERNET_PER_CONN_FLAGS;
//  Options[1].Value.dwValue := PROXY_TYPE_DIRECT or PROXY_TYPE_PROXY; //Original code
  Options[1].dwValue := PROXY_TYPE_PROXY;

  Options[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
  Options[2].pszValue := PChar(Address);

  Options[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
  Options[3].pszValue := PChar(Bypass);
  ShowMessage(Bypass);
  list.pOptions := @Options;
  hInternet := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if hInternet <> nil then
  try
    Result := InternetSetOption(nil, INTERNET_OPTION_PER_CONNECTION_OPTION, @list, dwBufSize);
    Result := Result and InternetSetOption(nil, INTERNET_OPTION_REFRESH, nil, 0);
  finally
    InternetCloseHandle(hInternet)
  end;
end;

function SetProxy(UserAgent, Address, UserName, Password: string; Port: Integer): Boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  hInternet, hInternetConnect: Pointer;
  Options: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  Result := False;
  dwBufSize := SizeOf(list);
  list.dwSize := SizeOf(list);
  list.pszConnection := nil;
  list.dwOptionCount := High(Options);
  Options[1].dwOption := INTERNET_PER_CONN_FLAGS;
  Options[1].dwValue := PROXY_TYPE_DIRECT or PROXY_TYPE_PROXY;
  Options[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
  Options[2].pszValue := PChar(Address);
  Options[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
  Options[3].pszValue := '<local>';
  list.pOptions := @Options;
  hInternet := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if hInternet <> nil then
  try
    hInternetConnect := InternetConnect(hInternet, PChar(Address), Port, PChar(UserName), PChar(Password),
      INTERNET_SERVICE_HTTP, 0, 0);
    if hInternetConnect <> nil then
    begin
      Result := InternetSetOption(hInternet, INTERNET_OPTION_PER_CONNECTION_OPTION, @list, dwBufSize);
      Result := Result and InternetSetOption(hInternet, INTERNET_OPTION_REFRESH, nil, 0);
    end;
  finally
    InternetCloseHandle(hInternet)
  end;
end;


function SetProxyFromPAC(UserAgent, PACFile: string): Boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  hInternet: Pointer;
  Options: array[1..2] of INTERNET_PER_CONN_OPTION;
begin
  Result := False;
  dwBufSize := SizeOf(list);
  list.dwSize := SizeOf(list);
  list.pszConnection := nil;
  list.dwOptionCount := High(Options);
  Options[1].dwOption := INTERNET_PER_CONN_AUTOCONFIG_URL;
  Options[1].pszValue := PChar(PacFile);
  Options[2].dwOption := INTERNET_PER_CONN_FLAGS;
  Options[2].dwValue := PROXY_TYPE_AUTO_PROXY_URL;
  list.dwOptionCount := 2;
  list.dwOptionError := 0;
  list.pOptions := @Options;
  hInternet := InternetOpen(PChar(UserAgent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if hInternet <> nil then
  try
    Result := InternetSetOption(hInternet, INTERNET_OPTION_PER_CONNECTION_OPTION, @list, dwBufSize);
    Result := Result and InternetSetOption(hInternet, INTERNET_OPTION_REFRESH, nil, 0);
  finally
    InternetCloseHandle(hInternet)
  end;
end;

function RemoveProxy(): Boolean;
var
  list: INTERNET_PER_CONN_OPTION_LIST;
  dwBufSize: DWORD;
  hInternet: Pointer;
  Options: array[1..3] of INTERNET_PER_CONN_OPTION;
begin
  Result := False;
  dwBufSize := SizeOf(list);
  list.dwSize := SizeOf(list);
  list.pszConnection := nil;
  list.dwOptionCount := High(Options);
  Options[1].dwOption := INTERNET_PER_CONN_FLAGS;
  Options[1].dwValue := PROXY_TYPE_DIRECT or PROXY_TYPE_PROXY;
  Options[2].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
  Options[2].pszValue := PChar('');
  Options[3].dwOption := INTERNET_PER_CONN_PROXY_BYPASS;
  Options[3].pszValue := '<local>';
  list.pOptions := @Options;
  hInternet := InternetOpen(PChar(''), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if hInternet <> nil then
  try
    InternetSetOption(hInternet, INTERNET_OPTION_PER_CONNECTION_OPTION, @list, dwBufSize);
    InternetSetOption(hInternet, INTERNET_OPTION_REFRESH, nil, 0);
    Result := True;
  finally
    InternetCloseHandle(hInternet)
  end;
end;

procedure RemoveUserAgent(UserAgent: string);
var
  reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_CURRENT_USER;
    try
      if OpenKey(USER_AGENT_PATH, False)
        then
        DeleteValue(UserAgent);
    finally
      CloseKey;
      Free;
    end;
  end;
end;

var
  MimeFactory, NSFactory: IClassFactory;
  MimeInternetSession, NSInternetSession: IInternetSession;

function RegisterMIMEFilter(clsid: TGUID; MIME: PWideChar): HRESULT;
begin
  CoGetClassObject(Clsid, CLSCTX_SERVER, nil, IClassFactory, MimeFactory);
  CoInternetGetSession(0, MimeInternetSession, 0);
  Result := MIMEInternetSession.RegisterMimeFilter(MimeFactory, Clsid, MIME);
end;

function UnregisterMIMEFilter(MIME: PWideChar): HRESULT;
begin
  Result := MIMEInternetSession.UnregisterMimeFilter(MIMEFactory, MIME);
end;

function RegisterNameSpace(clsid: TGUID): HRESULT;
begin
  CoGetClassObject(Clsid, CLSCTX_SERVER, nil, IClassFactory, NSFactory);
  CoInternetGetSession(0, NSInternetSession, 0);
  Result := NSInternetSession.RegisterNameSpace(NSFactory, Clsid, 'http', 0, nil, 0);
end;

function UnregisterNameSpace: HRESULT;
begin
  Result := NSInternetSession.UnregisterNameSpace(NSFactory, 'http');
end;

procedure RestoreApplicationFormSize(WebBrowser: TEmbeddedWB);
var
  ws: Integer;
  RegPath: string;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title + '\FormSize';
    if OpenKey(RegPath, False) then
    try
      with Forms.Application.MainForm do
      begin
        Left := ReadInteger('Left');
        Top := ReadInteger('Top');
        Width := ReadInteger('Width');
        Height := ReadInteger('Height');
        ws := ReadInteger('WindowState');
        case ws of
          0: WindowState := wsNormal;
          1: WindowState := wsMinimized;
          2: WindowState := wsMaximized;
        end;
      end;
    except
    end;
    CloseKey;
    Free;
  end;
end;

procedure SaveApplicationFormSize(WebBrowser: TEmbeddedWB);
var
  RegPath: string;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title + '\FormSize';
    if OpenKey(RegPath, True) then
    try
      with Forms.Application.MainForm do
      begin
        WriteInteger('Top', Top);
        WriteInteger('Left', Left);
        WriteInteger('Width', Width);
        WriteInteger('Height', Height);
        with Forms.Application.MainForm do
          case WindowState of
            wsNormal: WriteInteger('WindowState', 0);
            wsMinimized: WriteInteger('WindowState', 0);
            wsMaximized: WriteInteger('WindowState', 0);
          end;
      end;
      CloseKey;
      Free;
    except
    end;
  end;
end;

procedure Wait(WebBrowser: TEmbeddedWB);
begin
  WebBrowser.Wait;
end;

function InvokeCMD(Document: IDispatch; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): HRESULT;
var
  CmdTarget: IOleCommandTarget;
  PtrGUID: PGUID;
begin
 //  New(PtrGUID);
  Result := S_FALSE;
  if InvokeIE then
  begin
    New(PtrGUID);
    PtrGUID^ := CLSID_WebBrowser;
  end
  else
    PtrGuid := PGUID(nil);
  if DocumentLoaded(Document) then
  try
    Document.QueryInterface(IOleCommandTarget, CmdTarget);
    if CmdTarget <> nil then
    try
      Result := CmdTarget.Exec(PtrGuid, Value1, Value2, vaIn, vaOut);
    finally
      //CmdTarget._Release;  I4214   // I4214
    end;
  except
  end;
  Dispose(PtrGUID);
end;

function GetIEHandle(WebBrowser: TEmbeddedWB; ClassName: string): HWND;
begin
  Result := WebBrowser.GetIEHandle(WebBrowser, ClassName);
end;

procedure ShowIEVersionInfo(Handle: THandle);
begin
  SendMessage(Handle, WM_COMMAND, ID_IE_HELP_VERSIONINFO, 0);
end;

procedure SetNewHomePage(HomePage: string);
begin
  with TRegistry.Create do
  begin
    try
      OpenKey('\Software\Microsoft\Internet Explorer\Main', True);
      WriteString('Start Page', HomePage);
      CloseKey;
    finally
      Free;
    end;
  end;
end;

function GetLastVisitedPage(var LastVisitedPage: string): Boolean;
begin
  Result := False;
  with TRegistry.Create do
  begin
    LastVisitedPage := '';
    RootKey := HKEY_LOCAL_MACHINE;
    try
      if OpenKey('SOFTWARE\' + Forms.Application.Title + '\WebPages', False) then
      begin
        LastVisitedPage := ReadString('LastVisitedPage');
        CloseKey;
        Result := (LastVisitedPage <> '') and (AnsiPos('.', LastVisitedPage) > 0);
      end;
    finally
      Free;
    end;
  end;
end;

function SaveLastVisitedPage(WebBrowser: TEmbeddedWB; LocationURL: string): Boolean;
var
  RegPath: string;
begin
  Result := False;
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title + '\WebPages';
    if OpenKey(RegPath, False) then
    try
      DeleteKey('LastVisitedPage');
    except
    end;
    Free;
  end;
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    RegPath := 'SOFTWARE\' + Forms.Application.Title;
    if OpenKey(RegPath, True) then
    begin
      try
        WriteString('WebPages', 'LastVisitedPage', LocationURL);
        Result := True;
      except
      end;
      CloseKey;
    end;
    Free;
  end;
end;

procedure CreateDesktopShortcut(Handle: THandle);
begin
  SendMessage(Handle, WM_COMMAND, ID_IE_FILE_SENDDESKTOPSHORTCUT, 0);
end;

procedure DisableNavSound(bDisable: Boolean);
const
  REG_PATH = 'AppEvents\Schemes\Apps\Explorer\Navigating\';
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create;
  with Reg do
  begin
    RootKey := HKEY_CURRENT_USER;
    try
      if bDisable then
      begin
        if KeyExists(REG_PATH + '.Current') then
          if OpenKey(REG_PATH, True) then
            MoveKey('.Current', 'Old_Current', True);
      end
      else
      begin
        if KeyExists(REG_PATH + 'Old_Current') then
          if OpenKey(REG_PATH, False) then
            MoveKey('Old_Current', '.Current', True);
      end;
    finally
      CloseKey;
      Free;
    end;
  end;
end;

function WBExecScript(
  TargetObj: IDispatch;
  MethodName: string;
  ParamValues: array of const): OleVariant;
var
  wide: WideString;
  disps: TDispIDList;
  panswer: ^OleVariant;
  answer: OleVariant;
  dispParams: TDispParams;
  aexception: TExcepInfo;
  pVarArg: PVariantArgList;
  res: HRESULT;
  ParamCount, i: Integer;
begin
  Result := False;

  // prepare for function call
  ParamCount := High(ParamValues) + 1;
  wide := MethodName;
  pVarArg := nil;
  if ParamCount > 0 then
    GetMem(pVarArg, ParamCount * sizeof(TVariantArg));

  try
    // get dispid of requested method
    if not Succeeded(TargetObj.GetIDsOfNames(GUID_NULL, @wide, 1, 0, @disps)) then
      raise Exception.Create('This object does not support this method');
    pAnswer := @answer;

    // prepare parameters
    for i := 0 to Pred(ParamCount) do
    begin
      case ParamValues[ParamCount - 1 - i].VType of
        vtBoolean: begin
            pVarArg^[i].vt := VT_BOOL;
            pVarArg^[i].vbool := ParamValues[ParamCount - 1 - i].VBoolean;
          end;
        vtCurrency: begin
            pVarArg^[i].vt := VT_CY;
            pVarArg^[i].cyVal := ParamValues[ParamCount - 1 - i].VCurrency^;
          end;
        vtInt64: begin
            pVarArg^[i].vt := VT_I8;
            PInt64(@pVarArg^[i].cyVal)^ := ParamValues[ParamCount - 1 - i].VInt64^;
          end;
        vtInteger: begin
            pVarArg^[i].vt := VT_I4;
            pVarArg^[i].lVal := ParamValues[ParamCount - 1 - i].VInteger;
          end;
        vtExtended: begin
            pVarArg^[i].vt := VT_R8;
            pVarArg^[i].dblVal := ParamValues[ParamCount - 1 - i].VExtended^;
          end;
        vtVariant: begin
            pVarArg^[i].vt := VT_BYREF or VT_VARIANT;
            pVarArg^[i].pvarVal := ParamValues[ParamCount - 1 - i].VVariant;
          end;
        vtChar: begin
            {pVarArg^[i].vt := VT_I1;
            pVarArg^[i].cVal := ParamValues[ParamCount - 1 - i].VChar;}
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(ParamValues[ParamCount - 1 - i].VChar));
          end;
        vtWideChar: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(ParamValues[ParamCount - 1 - i].VWideChar));
          end;
        vtPChar: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(ParamValues[ParamCount - 1 - i].VPChar));
          end;
        vtPWideChar: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := ParamValues[ParamCount - 1 - i].VPWideChar;
          end;
        vtAnsiString: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(PAnsiChar(ParamValues[ParamCount - 1 - i].VAnsiString)));
          end;
        vtWideString: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(ParamValues[ParamCount - 1 - i].VWideString));
          end;
        vtString: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(WideString(PAnsiChar(ParamValues[ParamCount - 1 - i].VString)));
          end;
{$IFDEF UNICODE}
        vtUnicodeString: begin
            pVarArg^[i].vt := VT_BSTR;
            pVarArg^[i].bstrVal := PWideChar(UnicodeString(ParamValues[ParamCount - 1 - i].VUnicodeString));
          end;
{$ENDIF UNICODE}
      else
        raise Exception.CreateFmt('Unsupported type for Parameter with Index %d', [i]);
      end;
    end;

    // prepare dispatch parameters
    dispparams.rgvarg := pVarArg;
    dispparams.rgdispidNamedArgs := nil;
    dispparams.cArgs := ParamCount;
    dispparams.cNamedArgs := 0;

    // make IDispatch call
    res := TargetObj.Invoke(disps[0],
      GUID_NULL, 0, DISPATCH_METHOD or DISPATCH_PROPERTYGET,
      dispParams, pAnswer, @aexception, nil);

    // check the Result
    if res <> 0 then
      raise Exception.CreateFmt(
        'Method call unsuccessful. %s (%s).',
        [string(aexception.bstrDescription), string(aexception.bstrSource)]);

    // return the Result
    Result := answer;
  finally
    if ParamCount > 0 then
      FreeMem(pVarArg, ParamCount * sizeof(TVariantArg));
  end;
end;

function ExecScriptEx(WebBrowser: TEmbeddedWB; MethodName: string; ParamValues: array of const): OleVariant;
var
  doc: IHTMLDocument2;
  dScript: IDispatch;
begin
  if WebBrowser.DocumentLoaded(Doc) then
  begin
    dScript := doc.Script;
    if Assigned(dScript) then
      Result := WBExecScript(DScript, MethodName, ParamValues);
  end;
end;

procedure ExecScript(WebBrowser: TEmbeddedWB; sExpression, sLanguage: string);
// e.g. sLanguage = 'JavaScript';
var
  Doc: IHTMLDocument2; // current HTML document
  HTMLWin: IHTMLWindow2; // parent window of current HTML document
begin
  if WebBrowser.DocumentLoaded(Doc) then
  begin
    HTMLWin := Doc.parentWindow;
    if Assigned(HTMLWin) then
    begin
      try
        HTMLWin.execScript(sExpression, sLanguage);
      except
      end;
    end;
  end;
end;

//To Add--------------------------------------------------

function URLFromShortcut(const dotURL: string): string;
begin
  Result := '';
  with TIniFile.Create(dotURL) do
  try
    Result := ReadString('InternetShortcut', 'URL', '');
  finally
    Free;
  end;
end;

function ExtractUrl(ShellFolder: IShellFolder; pidl: PItemIDList): string;
var
  Handle: THandle;
  Info: IQueryInfo;
  W: PWideChar;
begin
  Handle := 0;
  Info := nil;
  Result := '';
  ShellFolder.GetUIObjectOf(Handle, 1, pidl, IID_IQUERYINFO, nil, Pointer(Info));
  if Assigned(Info) then
  begin
    Info.GetInfoTip(0, w);
    Result := W;
  end;
  Result := Trim(Copy(Result, Pos(#10, Result) + 1, length(Result)));
end;

function StringToVarArray(const S: string): Variant;
begin
  Result := Unassigned;
  if S <> '' then
  begin
    Result := VarArrayCreate([0, Length(S) - 1], varByte);
    Move(Pointer(S)^, VarArrayLock(Result)^, Length(S));
    VarArrayUnlock(Result);
  end;
end;

function VarArrayToString(const V: Variant): string;
var
  i, j: Integer;
begin
  if VarIsArray(V) then
    for i := 0 to VarArrayHighBound(V, 1) do
    begin
      j := V[i];
      Result := Result + chr(j);
    end;
end;

function Encode(const S: string): string;
var
  I: Integer;
  Hex: string;
begin
  for I := 1 to Length(S) do
    case S[i] of
      ' ': Result := Result + '+';
      'A'..'Z', 'a'..'z', '*', '@', '.', '_', '-',
        '0'..'9', '$', '!', '''', '(', ')':
        Result := Result + s[i];
    else
      begin
        Hex := IntToHex(ord(S[i]), 2);
        if Length(Hex) = 2 then
          Result := Result + '%' + Hex
        else
          Result := Result + '%0' + hex;
      end;
    end;
end;

function IE5_Installed: Boolean;
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('Software\Microsoft\Internet Explorer', False);
    if ValueExists('Version') then
      S := ReadString('Version')
    else
      S := '0';
    CloseKey;
    Free;
  end;
  Result := (StrToInt(S[1]) > 4);
end;

function GetIEVersionMajor: Integer;
var
  i: Integer;
  s: string;
begin
  s := GetIEVersion;
  i := Pos('.', s);
  Result := -1;
  if i <> 0 then
  begin
    try
      Result := StrToInt(Copy(s, 1, Pos('.', s) - 1));
    except
      Result := -1;
    end;
  end;
end;

function GetIEVersion: string;
var
  SysDir: PChar;
  Info: Pointer;
  InfoData: Pointer;
  InfoSize: LongInt;
  Len: DWORD;
  FName: Pchar;
  SystemDir, Infotype: string;
  LangPtr: Pointer;
begin
  Len := MAX_PATH + 1;
  GetMem(SysDir, Len);
  try
    if Windows.GetSystemDirectory(SysDir, Len) <> 0 then
      SystemDir := SysDir;
  finally
    FreeMem(SysDir);
  end;
  Result := '';
  InfoType := 'FileVersion';
  if FileExists(SystemDir + '\ieframe.dll') then
    FName := PChar(SystemDir + '\ieframe.dll')
  else
    FName := PChar(SystemDir + '\shdocvw.dll');
  InfoSize := GetFileVersionInfoSize(Fname, Len);
  if (InfoSize > 0) then
  begin
    GetMem(Info, InfoSize);
    try
      if GetFileVersionInfo(FName, Len, InfoSize, Info) then
      begin
        Len := 255;
        if VerQueryValue(Info, '\VarFileInfo\Translation', LangPtr, Len) then
          InfoType := Format('\StringFileInfo\%0.4x%0.4x\%s'#0, [LoWord(LongInt(LangPtr^)),
            HiWord(LongInt(LangPtr^)), InfoType]);
        if VerQueryValue(Info, Pchar(InfoType), InfoData, len) then
{$IFDEF UNICODE}
          Result := Trim(PWideChar(InfoData));
{$ELSE}
          Result := StrPas(PAnsiChar(InfoData));
{$ENDIF UNICODE}
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
  end;
end;

function ResolveUrlIni(Filename: string): string;
var
  ini: TiniFile;
begin
  Result := '';
  ini := TIniFile.Create(Filename);
  try
    Result := ini.ReadString('InternetShortcut', 'URL', '');
  finally
    ini.Free;
  end;
end;

function ResolveUrlIntShCut(Filename: string): string;
var
  IURL: IUniformResourceLocator;
  PersistFile: IPersistfile;
  FName: array[0..MAX_PATH] of WideChar;
  p: PChar;
begin
  if Succeeded(CoCreateInstance(CLSID_InternetShortcut, nil, CLSCTX_INPROC_SERVER,
    IID_IUniformResourceLocator, IURL)) then
  begin
    Persistfile := IUrl as IPersistFile;
    StringToWideChar(FileName, FName, MAX_PATH);
    PersistFile.Load(FName, STGM_READ);
    IUrl.GetUrl(@P);
    Result := P;
  end;
end;

function ResolveChannel(pFolder: IShellFolder; pidl: PItemIDList; var lpszURL: string): HRESULT;
var
  pidlChannel: PItemIDList;
  psfDesktop: IShellFolder;
  pShellLink: IShellLink;
begin
  Result := S_FALSE;
  if Succeeded(pFolder.GetUIObjectOf(0, 1, pidl, IShellLink, nil, Pointer(pShellLink)))
    then
    if Succeeded(pShellLink.GetIDList(pidlChannel)) then
      if Succeeded(SHGetDesktopFolder(psfDesktop)) then
      begin
        lpszURL := getDisplayName(psfDesktop, PidlChannel);
        Result := S_OK;
      end;
  DisposePidl(PidlChannel);
end;

function ResolveLink(const Path: string): string;
var
  link: IShellLink;
  storage: IPersistFile;
  filedata: TWin32FindData;
  buf: array[0..MAX_PATH] of Char;
  widepath: WideString;
begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, link));
  OleCheck(link.QueryInterface(IPersistFile, storage));
  widepath := path;
  Result := '';
  if Succeeded(storage.Load(@widepath[1], STGM_READ)) then
    if Succeeded(link.Resolve(GetActiveWindow, SLR_NOUPDATE)) then
      if Succeeded(link.GetPath(buf, SizeOf(buf), filedata, SLGP_UNCPRIORITY)) then
        Result := buf;
  storage := nil;
  link := nil;
end;

function IsFolder(ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
var
  Flags: UINT;
begin
  Flags := SFGAO_FOLDER;
  ShellFolder.GetAttributesOf(1, ID, Flags);
  Result := SFGAO_FOLDER and Flags <> 0;
end;

function IsChannel(ChannelShortcut: string; ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
var
  FileInfo: TShFileInfo;
begin
  SHGetFileInfo(Pchar(ID), 0, FileInfo, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_TYPENAME);
  Result := BOOL(fileinfo.szTypeName = ChannelShortcut);
end;

function IsFolderEx(ChannelShortcut: string; ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
var
  Flags: UINT;
begin
  Flags := SFGAO_FOLDER;
  ShellFolder.GetAttributesOf(1, ID, Flags);
  if SFGAO_FOLDER and Flags <> 0 then
    Result := not isChannel(ChannelShortcut, Shellfolder, id)
  else
    Result := False;
end;

function GetImageIndex(pidl: PItemIDList): Integer;
var
  Flags: UINT;
  FileInfo: TSHFileInfo;
begin
  Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or SHGFI_SMALLICON;
  if SHGetFileInfo(PChar(pidl), 0, FileInfo, SizeOf(TSHFileInfo), Flags) = 0 then
    Result := -1
  else
    Result := FileInfo.iIcon;
end;

{function GetDisplayName(Folder: IShellFolder; pidl: PItemIDList): string;
var
  StrRet: TStrRet;
begin
  Result := '';
  Folder.GetDisplayNameOf(pidl, SHGDN_NORMAL, StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
    STRRET_OFFSET:
      Result := Pchar(@pidl.mkid.abID[StrRet.uOffset - SizeOf(pidl.mkid.cb)]);
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end; }

function GetDisplayName(Folder: IShellFolder; PIDL: PItemIDList): string;
var
  StrRet: TStrRet;
  P: PChar;
  Flags: Integer;
begin
  Result := '';
  Flags := SHGDN_NORMAL;
  Folder.GetDisplayNameOf(PIDL, Flags, StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLenA(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;

{function GetFileName(Folder: IShellFolder; pidl: PItemIDList): string;
var
  StrRet: TStrRet;
begin
  Folder.GetDisplayNameOf(pidl, SHGDN_FORPARSING, StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
    STRRET_OFFSET:
      Result := Pchar(@pidl.mkid.abID[StrRet.uOffset - SizeOf(pidl.mkid.cb)]);
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;   }

function GetFileName(Folder: IShellFolder; pidl: PItemIDList): string;
var
  StrRet: TStrRet;
  P: PChar;
begin
  Result := '';
  Folder.GetDisplayNameOf(PIDL, SHGDN_FORPARSING, StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLenA(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;

procedure DisposePIDL(ID: PItemIDList);
var
  Malloc: IMalloc;
begin
  if ID <> nil then
  begin
    OLECheck(SHGetMalloc(Malloc));
    Malloc.Free(ID);
  end;
end;

function CopyITEMID(Malloc: IMalloc; ID: PItemIDList): PItemIDList;
begin
  Result := Malloc.Alloc(ID^.mkid.cb + SizeOf(ID^.mkid.cb));
  CopyMemory(Result, ID, ID^.mkid.cb + SizeOf(ID^.mkid.cb));
end;

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PAnsiChar(Result), IDList^.mkid.cb);
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

procedure StripLastID(IDList: PItemIDList);
var
  MarkerID: PItemIDList;
begin
  MarkerID := IDList;
  if Assigned(IDList) then
  begin
    while IDList.mkid.cb <> 0 do
    begin
      MarkerID := IDList;
      IDList := NextPIDL(IDList);
    end;
    MarkerID.mkid.cb := 0;
  end;
end;

function CreatePIDL(Size: Integer): PItemIDList;
var
  Malloc: IMalloc;
  HR: HResult;
begin
  Result := nil;
  HR := SHGetMalloc(Malloc);
  if Failed(HR) then
    Exit;
  try
    Result := Malloc.Alloc(Size);
    if Assigned(Result) then
      FillChar(Result^, Size, 0);
  finally
  end;
end;

function CopyPIDL(IDList: PItemIDList): PItemIDList;
var
  Size: Integer;
begin
  Size := GetPIDLSize(IDList);
  Result := CreatePIDL(Size);
  if Assigned(Result) then
    CopyMemory(Result, IDList, Size);
end;

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
var
  cb1, cb2: Integer;
begin
  if Assigned(IDList1) then
    cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb)
  else
    cb1 := 0;
  cb2 := GetPIDLSize(IDList2);
  Result := CreatePIDL(cb1 + cb2);
  if Assigned(Result) then
  begin
    if Assigned(IDList1) then
      CopyMemory(Result, IDList1, cb1);
    CopyMemory(PAnsiChar(Result) + cb1, IDList2, cb2);
  end;
end;

function DeleteUrl(Url: PWideChar): HResult;
begin
  Result := DeleteUrl(Url);
end;

function GetMailClients: TStrings;
var
  Reg: TRegistry;
  ts: TStrings;
  i: Integer;
begin
  ts := TStringList.Create;
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_CURRENT_USER;
    try
      OpenKey(RegMail, False);
      if HasSubKeys then
      begin
        GetKeyNames(ts);
        CloseKey;
        for i := 0 to ts.Count - 1 do
          OpenKey(RegMail + ts.Strings[i], False);
      end;
      Result := ts;
    finally
      CloseKey;
      Free;
    end;
  end;
end;



end.

