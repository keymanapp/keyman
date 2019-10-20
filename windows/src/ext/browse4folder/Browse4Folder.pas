//***********************************************************
//                        TBrowse4Folder                    *
//                                                          *
//                       For Delphi 5 to 2010               *
//                     Freeware Component                   *
//                            by                            *
//                     Eran Bodankin (bsalsa)               *                                                            //                   per.lindsoe@larsen.dk                  *
//                    bsalsa@gmail.com                      *
//                 Based on idea by brian@cryer.co.uk       *
//                                                          *
//  Documentation and updated versions:                     *
//                                                          *
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
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: Browse4Folder.pas,v 1.2 2006/11/15 21:01:38 sergev Exp $

unit Browse4Folder;

interface

uses
  Classes, ShellApi, shlobj, sysutils, Windows, Dialogs, ActiveX;

type
  TOption = (OnlySelectFileSysDir, NoNetDirBelowDomain, ShowStatusText,
    OnlySelectFileSysAncestors, ShowEditBox, ValidateEditBox, UseNewDialogStyle,
    HideNewUserInterface, ShowAlsoURLs, ShowUsageHint,
    HideNewFolderButton, DontTranslateShortcuts, OnlySelectComputers,
    OnlySelectPrinters, ShowAlsoFiles, ShowShareableRes);
  TOptions = set of TOption;

type
  TWinPaths = (Desktop, StartMenu,
    StartMenuPrograms, Startup, MyDocuments, ApplicationData,
    Fonts, SendTo, MyRecentDocuments, Favorites, Cache,
    Cookies, History, NetHood, PrintHood,
    Templates, LocalAppData, WindowsRoot, WindowsSystem,
    TempFolder, PCRootDrive, ProgramFiles, CommonFiles,
    Media, DownloadFolder);

  TBrowse4Folder = class(TComponent)

  private
    FRoot: TWinPaths;
    FTitle: string;
    FInitialDir: WideString;
    FOptions: TOptions;
    FShowTitle: Boolean;
    FAbout: string;
    function Browse4Folder: WideString;
    procedure SetAbout(const Value: string);
  protected
    function UpdateOptionsValue: LongInt;
  public
    FileName: WideString;
    function Execute: Boolean;
    function Execute2: WideString;
    constructor Create(Owner: Tcomponent); override;
    procedure Loaded; override;
  published
    property About: string read FAbout write SetAbout;
    property InitialDir: WideString read FInitialDir write FInitialDir;
    property Options: TOptions read FOptions write FOptions default
      [UseNewDialogStyle, ShowEditBox];
    property Root: TWinPaths read FRoot write FRoot;
    property Title: string read FTitle write FTitle;
    property ShowTitle: Boolean read FShowTitle write FShowTitle default True;
  end;

implementation

uses
  TypInfo, Registry, Forms, ComObj;

var
  lg_StartFolder: string;

function GetPath(Path: TWinPaths): string;
var
  ph: PChar;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey('\Software\Microsoft\Windows\CurrentVersion\' +
      'Explorer\Shell Folders', True);
    case Path of
      Desktop: Result := ReadString('Desktop');
      StartMenu: Result := ReadString('Start Menu');
      StartMenuPrograms: Result := ReadString('Programs');
      Startup: Result := ReadString('Startup');
      MyDocuments: Result := ReadString('Personal');
      ApplicationData: Result := ReadString('AppData');
      Fonts: Result := ReadString('Fonts');
      SendTo: Result := ReadString('SendTo');
      MyRecentDocuments: Result := ReadString('Recent');
      Favorites: Result := ReadString('Favorites');
      Cache: Result := ReadString('Cache');
      Cookies: Result := ReadString('Cookies');
      History: Result := ReadString('History');
      NetHood: Result := ReadString('NetHood');
      PrintHood: Result := ReadString('PrintHood');
      Templates: Result := ReadString('Templates');
      LocalAppData: Result := ReadString('Local AppData');
      WindowsRoot:
        begin
          GetMem(ph, (MAX_PATH + 1) * SizeOf(Char));
          GetWindowsDirectory(ph, MAX_PATH);
          Result := StrPas(ph);
          FreeMem(ph);
        end;
      WindowsSystem:
        begin
          GetMem(ph, (MAX_PATH + 1) * SizeOf(Char));
          GetWindowsDirectory(ph, MAX_PATH);
          Result := StrPas(ph);
          FreeMem(ph);
        end;
      TempFolder:
        begin
          GetMem(ph, (MAX_PATH + 1) * SizeOf(Char));
          GetWindowsDirectory(ph, MAX_PATH);
          Result := StrPas(ph);
          FreeMem(ph);
        end;
      PCRootDrive:
        begin
          GetMem(ph, (MAX_PATH + 1) * SizeOf(Char));
          GetWindowsDirectory(ph, MAX_PATH);
          Result := (Copy(StrPas(ph), 1, 2));
          FreeMem(ph);
        end;
      DownloadFolder:
        begin
          if OpenKey('Software\Microsoft\Internet Explorer', True) then
            Result := Readstring('Download Directory');
        end;
    end;
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion', True);
    case Path of
      ProgramFiles: Result := ReadString('ProgramFilesDir');
      CommonFiles: Result := ReadString('CommonFilesDir');
      Media: Result := ReadString('MediaPath');
    end;
  finally
    CloseKey;
    Free;
  end;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
  lParam, lpData: LPARAM): Integer stdcall;
{$IFDEF DELPHI7_UP}
var
  wa, rect: TRect;
  dialogPT: TPoint;
{$ENDIF}
begin
  if uMsg = BFFM_INITIALIZED then
  begin
    SendMessage(Wnd, BFFM_SETSELECTION, 1, Integer(@lg_StartFolder[1]));
{$IFDEF DELPHI7_UP}
    wa := Screen.WorkAreaRect;
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right - wa.Left) div 2) -
      ((rect.Right - rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom - wa.Top) div 2) -
      ((rect.Bottom - rect.Top) div 2);
    MoveWindow(Wnd, dialogPT.X, dialogPT.Y, Rect.Right - Rect.Left,
      Rect.Bottom - Rect.Top, True);
{$ENDIF}
  end;
  Result := 0;
end;

procedure TBrowse4Folder.SetAbout(const Value: string);
begin
  Exit;
end;

function TBrowse4Folder.Browse4Folder: Widestring;
var
  Browse_info: TBrowseInfo;
  Folder: array[0..MAX_PATH] of char;
  find_context: PItemIDList;
begin
  FillChar(Browse_info, SizeOf(Browse_info), #0);
  lg_StartFolder := FInitialDir;
  Browse_info.pszDisplayName := @Folder[0];
  if FShowTitle then
    Browse_info.lpszTitle := PChar(FTitle);
  Browse_info.ulFlags := UpdateOptionsValue;
  Browse_info.hwndOwner := Application.Handle;
  Browse_info.lpfn := BrowseForFolderCallBack;
  find_context := SHBrowseForFolder(Browse_info);
  if Assigned(find_context) then
  begin
    if SHGetPathFromIDList(find_context, Folder) then
      Result := Folder
    else
      Result := '';
    GlobalFreePtr(find_context);
  end
  else
    Result := '';
end;

function TBrowse4Folder.UpdateOptionsValue: LongInt;
const
  AcardOptionsControlValues: array[TOption] of Cardinal =
  ($0001, $0002, $0004,
    $0008, $00010, $0020,
    $0040, $0040 or $0010,
    $0080, $100, $200, $400,
    $1000, $2000, $4000, $8000);
var
  i: TOption;
  j: Longint;
begin
  j := 0;
  if (FOptions <> []) then
    for i := Low(TOption) to High(TOption) do
      if (i in FOptions) then
        Inc(j, AcardOptionsControlValues[i]);
  Result := j;
end;

function TBrowse4Folder.Execute2: WideString;
begin
  try
    Result := Browse4Folder;
  except
    Result := '';
  end;
end;

function TBrowse4Folder.Execute: Boolean;
begin
  try
    FileName := Browse4Folder;
    Result := True;
  except
    Result := False;
  end;
end;

constructor TBrowse4Folder.Create(Owner: Tcomponent);
begin
  inherited Create(Owner);
  FOptions := [UseNewDialogStyle, ShowEditBox];
  FShowTitle := True;
end;

procedure TBrowse4Folder.Loaded;
begin
  inherited Loaded;
  if FInitialDir = '' then
    FInitialDir := GetPath(FRoot);
end;

initialization
  OleInitialize(nil);

finalization
  try
    OleUninitialize;
  except
  end;

end.
