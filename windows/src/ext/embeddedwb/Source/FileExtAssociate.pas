//***********************************************************
//                       TFileExtAssociate                  *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                     Eran Bodankin (bsalsa)               *
//                     bsalsa@gmail.com                     *
//                                                          *
//           Based on idea by:  Zarko Gajic                 *
//                                                          *
//     Documentation and updated versions:                  *
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
3. Mail me (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: FileExtAssociate.pas,v 1.2 2006/11/15 21:01:41 sergev Exp $

unit FileExtAssociate;

interface

{$I EWB.inc}

uses
  Dialogs, Windows, Classes, Graphics, Forms;

type
  TFileInfo = record
    Icon: hIcon;
    Image: Integer;
    DisplayName: string;
    TypeName: string;
    Size: Integer;
    SizeAsString: string;
    DateTime: TDateTime;
  end;

type
  TOnErrorEvent = procedure(Text: string) of object;
  TOnSuccessEvent = procedure(Text: string) of object;
  TOnCompleteEvent = procedure(Extension, Status: string; HResult: HResult) of object;
  TOnShortcutEvent = procedure(ShortcutItem, Status: string; HResult: HResult) of object;
  TShortcutItems = (SendTo, StartMenu, StartUp, Desktop, OpenWith, Programs,
    QuickLaunch, ProgramsSubDir);
  TShortcutItem = set of TShortcutItems;

  TAppShortcuts = class(TPersistent)
  private
    FCreateShortcuts: Boolean;
    FOptions: TShortcutItem;
    FMenuSubDir: WideString;
    FOnShortcut: TOnShortcutEvent;
  public
  published
    property CreateShortcuts: Boolean read FCreateShortcuts write FCreateShortcuts default True;
    property Options: TShortcutItem read FOptions write FOptions default [OpenWith];
    property MenuSubDir: WideString read FMenuSubDir write FMenuSubDir;
    property OnShortcutEvent: TOnShortcutEvent read FOnShortcut write FOnShortcut;
  end;

  TExtensionAssociate = class(TPersistent)
  private
    FCreateAssociation: Boolean;
    FExtensions: TStrings;
  protected
    procedure SetExtensions(Value: TStrings);
  public
  published
    property CreateAssociation: boolean read FCreateAssociation write FCreateAssociation default True;
    property Extensions: TStrings read FExtensions write SetExtensions;
  end;

  TAppDetails = class(TPersistent)
  private
    FAppTitle: string;
    FAppExeName: string;
    FIcon: TIcon;
  published
    property Title: string read FAppTitle write FAppTitle;
    property ExeName: string read FAppExeName write FAppExeName;
    property Icon: TIcon read FIcon write FIcon;
  end;

type
  TFileExtAssociate = class(TComponent)
  private
    FAbout: string;
    FBusy: Boolean;
    FOnBusy: TNotifyEvent;
    FEnabled: Boolean;
    FApplicationDetails: TAppDetails;
    FExtensionAssociate: TExtensionAssociate;
    FOnError: TOnErrorEvent;
    FOnSuccess: TOnSuccessEvent;
    FOnComplete: TOnCompleteEvent;
    FOnShortcut: TOnShortcutEvent;
    FAppShortcuts: TAppShortcuts;
    function RegisterFileType(rExt: string; rTitle: string; rIcon: TIcon): Boolean;
    function UnRegisterFileType(uExt: string; uTitle: string): Boolean;
    function UpdateAppExeName(sExeName: string): string;
    function UpdateIcon(sIcon: TIcon): TIcon;
    function UpdateAppTitle(sTitle: string): string;
    function UpdateExtension(sExt: string): string;
    function CreateShortcut(SourceFileName: string; ShellFolder: TShortcutItems;
      SubFolder, WorkingDir, Parameters, Description: string): string;
    function DoExtension(sExt, sTitle, sExeName: string; sIcon: TIcon): Boolean;
    function DoShortcuts(dExeName, dTitle: string): Boolean;
    procedure BusyChange; dynamic;
  protected
    procedure SetAbout(Value: string);
    function UpdateShortcutItemValue: LongInt;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload;
    function Execute(Extension, AppTitle, ExeName: string; Icon: TIcon): Boolean; overload;
    function Execute(Extensions: TStrings; AppTitle, ExeName: string; Icon: TIcon): Boolean; overload;
    function Remove: Boolean; overload;
    function Remove(Ext: string; ExeName: string): Boolean; overload;
    function GetExeByExtension(sExt: string): string;
    function GetIconByExtension(Extension: string; Small: Boolean): HIcon;
    function GetFileDisplayName(const Path: string): string;
    function GetFileSize(const Path: string): string;
    function GetFileTypeName(const Path: string): string;
    function GetFileIcon(const Path: string): HIcon;
    function GetFileImage(const Path: string): integer;
    function GetFileDateTime(const Path: string): TDateTime;
    function CreateShortcutSendTo(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutStartUp(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutStartMenu(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutDesktop(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutPrograms(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutQuickLaunch(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function CreateShortcutOtherFolder(cExeName, cSubDir, cWorkDir, cParam,
      cTitle: string; CShellDir: TShortcutItems): HResult;
    function RemoveShortcuts(rmExeName: string): HResult;
    property Busy: Boolean read FBusy write FBusy default False;
  published
    property About: string read FAbout write SetAbout;
    property Enabled: boolean read FEnabled write FEnabled default True;
    property ExtensionAssociate: TExtensionAssociate read FExtensionAssociate write FExtensionAssociate;
    property ApplicationDetails: TAppDetails read FApplicationDetails write FApplicationDetails;
    property ApplicationShortcuts: TAppShortcuts read FAppShortcuts write FAppShortcuts;
    property OnBusyStateChange: TNotifyEvent read FOnBusy write FOnBusy;
    property OnErrorText: TOnErrorEvent read FOnError write FOnError;
    property OnSuccessText: TOnSuccessEvent read FOnSuccess write FOnSuccess;
    property OnComplete: TOnCompleteEvent read FOnComplete write FOnComplete;
    property OnShortcutEvent: TOnShortcutEvent read FOnShortcut write FOnShortcut;
  end;

implementation

uses
  ShellAPI, SysUtils, ActiveX, Registry, ShlObj, ComObj, EwbIEConst;

// ExtensionAssociate===========================================================

procedure TExtensionAssociate.SetExtensions(Value: TStrings);
begin
  FExtensions.Assign(Value)
end;
// FileExtAssociate=============================================================

constructor TFileExtAssociate.Create;
begin
  FAbout := 'File Extension Associate component by bsalsa. ' + WEB_SITE;
  FApplicationDetails := TAppDetails.Create;
  FApplicationDetails.FIcon := Application.Icon;
  FEnabled := True;
  FAppShortcuts := TAppShortcuts.Create;
  FAppShortcuts.FOptions := [OpenWith];
  FAppShortcuts.FCreateShortcuts := True;
  FExtensionAssociate := TExtensionAssociate.Create;
  FExtensionAssociate.FExtensions := TStringList.Create;
  FExtensionAssociate.FCreateAssociation := True;
  inherited;
end;

destructor TFileExtAssociate.Destroy;
begin
  FApplicationDetails.Free;
  FAppShortcuts.Free;
  FExtensionAssociate.FExtensions.Free;
  FExtensionAssociate.Free;
  inherited Destroy;
end;

procedure TFileExtAssociate.SetAbout(Value: string);
begin
  Exit;
end;

procedure TFileExtAssociate.BusyChange;
begin
  if Assigned(FOnBusy) then
    FOnBusy(Self);
end;

function TFileExtAssociate.UpdateAppExeName(sExeName: string): string;
begin
  if sExeName = '' then
    sExeName := Application.ExeName;
  FApplicationDetails.FAppExeName := sExeName;
  Result := sExeName;
  if not FileExists(sExeName) then
  begin
    if Assigned(FOnError) then
      FOnError('Can not locate your Exe name and path');
    Result := '';
  end;
end;

function TFileExtAssociate.UpdateAppTitle(sTitle: string): string;
begin
  if sTitle = '' then
    sTitle := Application.Title;
  FApplicationDetails.FAppTitle := sTitle;
  if sTitle = '' then
  begin
    if Assigned(FOnError) then
      FOnError('Please enter a valid application title.');
    Result := '';
  end
  else
    Result := sTitle;
end;

function TFileExtAssociate.UpdateIcon(sIcon: TIcon): TIcon;
begin
  if (sIcon = nil) then
    sIcon := Application.Icon;
  FApplicationDetails.FIcon := sIcon;
  Result := sIcon;
end;

function TFileExtAssociate.UpdateExtension(sExt: string): string;
begin
  if sExt = '' then
  begin
    if Assigned(FOnError) then
      FOnError('Please Enter a valid extension.');
  end;
  Result := sExt;
end;

function TFileExtAssociate.Execute: Boolean;
var
  i: integer;
  stExt: string;
  bExt, bSc: Boolean;
begin
  FBusy := True;
  BusyChange;
  bExt := False;
  Result := bExt;
  for i := 0 to FExtensionAssociate.FExtensions.Count - 1 do
  begin
    stExt := FExtensionAssociate.FExtensions[i];
    bExt := DoExtension(stExt, FApplicationDetails.FAppTitle,
      FApplicationDetails.FAppExeName, FApplicationDetails.FIcon);
  end;
  bSc := DoShortcuts(FApplicationDetails.FAppExeName, FApplicationDetails.FAppTitle);
  if ((FAppShortcuts.FCreateShortcuts and bSc) or
    (FExtensionAssociate.FCreateAssociation and bExt)) then
    Result := True;
  FBusy := False;
  BusyChange;
end;

function TFileExtAssociate.Execute(Extension, AppTitle, ExeName: string; Icon:
  TIcon): Boolean;
begin
  FBusy := True;
  BusyChange;
  Result := False;
  if DoExtension(Extension, AppTitle, ExeName, Icon) and
    DoShortcuts(ExeName, AppTitle) then
    Result := True;
  FBusy := False;
  BusyChange;
end;

function TFileExtAssociate.Execute(Extensions: TStrings; AppTitle, ExeName: string;
  Icon: TIcon): Boolean;
var
  i: integer;
  stExt: string;
  bExt, bSc: Boolean;
begin
  FBusy := True;
  BusyChange;
  bExt := False;
  Result := bExt;
  for i := 0 to Extensions.Count - 1 do
  begin
    stExt := Extensions[i];
    bExt := DoExtension(stExt, AppTitle, ExeName, Icon);
  end;
  bSc := DoShortcuts(ExeName, AppTitle);
  if ((FAppShortcuts.FCreateShortcuts and bSc) or
    (FExtensionAssociate.FCreateAssociation and bExt)) then
    Result := True;
  FBusy := False;
  BusyChange;
end;

function TFileExtAssociate.DoExtension(sExt, sTitle, sExeName: string; sIcon: TIcon): Boolean;
begin
  Result := False;
  if FExtensionAssociate.FCreateAssociation then
  begin
    if Assigned(FOnError) then
      FOnError('');
    if Assigned(FOnSuccess) then
      FOnSuccess('');
    sTitle := UpdateAppTitle(sTitle);
    if sTitle = '' then
      Exit;
    sExeName := UpdateAppExeName(sExeName);
    if sExeName = '' then
      Exit;
    sIcon := UpdateIcon(sIcon);
    if sIcon = nil then
      Exit;
    sExt := UpdateExtension(sExt);
    if sExt = '' then
      Exit;
    if RegisterFileType(sExt, sExeName, sIcon) then
    begin
      if Assigned(FOnComplete) then
        FOnComplete(sExt, 'Registration successful.', S_OK);
      if Assigned(FOnSuccess) then
        FOnSuccess('Done.');
    end
    else
      if Assigned(FOnError) then
        FOnError('Registration failure.');
  end;
end;

function TFileExtAssociate.DoShortcuts(dExeName, dTitle: string): Boolean;
begin
  Result := False;
  if FAppShortcuts.FCreateShortcuts then
  begin
    if Assigned(FOnError) then
      FOnError('');
    if Assigned(FOnSuccess) then
      FOnSuccess('');
    dExeName := UpdateAppExeName(dExeName);
    if dExeName = '' then
      Exit;
    UpdateShortcutItemValue;
    if SendTo in FAppShortcuts.FOptions then
      CreateShortcutSendTo(dExeName, '', '', '', dTitle, SendTo);
    if StartUp in FAppShortcuts.FOptions then
      CreateShortcutStartUp(dExeName, '', '', '', dTitle, StartUp);
    if StartMenu in FAppShortcuts.FOptions then
      CreateShortcutStartMenu(dExeName, '', '', '', dTitle, StartMenu);
    if Desktop in FAppShortcuts.FOptions then
      CreateShortcutDesktop(dExeName, '', '', '', dTitle, Desktop);
    if Programs in FAppShortcuts.FOptions then
      CreateShortcutPrograms(dExeName, '', '', '', dTitle, Programs);
    if QuickLaunch in FAppShortcuts.FOptions then
      CreateShortcutQuickLaunch(dExeName, '', '', '', dTitle, QuickLaunch);
    if ProgramsSubDir in FAppShortcuts.FOptions then
      CreateShortcutOtherFolder(dExeName, FAppShortcuts.FMenuSubDir, '', '',
        dTitle, ProgramsSubDir);
    Result := True;
    if Assigned(FOnSuccess) then
      FOnSuccess('Done.');
  end;
  if (not FAppShortcuts.FCreateShortcuts) and
    (not FExtensionAssociate.FCreateAssociation) then
    if Assigned(FOnComplete) then
      FOnComplete('', 'No action selected.', S_FALSE);

end;

function TFileExtAssociate.Remove: Boolean;
var
  i: integer;
  stExt: string;
begin
  FBusy := True;
  BusyChange;
  Result := False;
  if Assigned(FOnError) then
    FOnError('');
  if Assigned(FOnSuccess) then
    FOnSuccess('');
  RemoveShortcuts(FApplicationDetails.FAppExeName);
  for i := 0 to FExtensionAssociate.FExtensions.Count - 1 do
  begin
    stExt := FExtensionAssociate.FExtensions[i];
    if UnRegisterFileType(stExt, FApplicationDetails.FAppExeName) then
    begin
      if Assigned(FOnComplete) then
        FOnComplete(stExt, 'Unregister', S_OK);
      if Assigned(FOnSuccess) then
        FOnSuccess('Successfuly removed ' + stExt);
      Result := True;
    end
    else
    begin
      if Assigned(FOnComplete) then
        FOnComplete(stExt, 'Unregister', S_FALSE);
      Result := False;
    end;
  end;
  FBusy := False;
  BusyChange;
end;

function TFileExtAssociate.Remove(Ext: string; ExeName: string): Boolean;
begin
  FBusy := True;
  BusyChange;
  if Assigned(FOnError) then
    FOnError('');
  if Assigned(FOnSuccess) then
    FOnSuccess('');
  RemoveShortcuts(ExeName);
  if UnRegisterFileType(Ext, ExeName) then
  begin
    if Assigned(FOnComplete) then
      FOnComplete(Ext, 'Unregister', S_OK);
    if Assigned(FOnSuccess) then
      FOnSuccess('Successfuly removed ' + Ext);
    Result := True;
  end
  else
  begin
    if Assigned(FOnComplete) then
      FOnComplete(Ext, 'Unregister', S_FALSE);
    Result := False;
  end;
  FBusy := False;
  BusyChange;
end;

function TFileExtAssociate.RegisterFileType(rExt: string; rTitle: string; rIcon: TIcon): Boolean;
begin
  if rExt[1] = '.' then
    rExt := Copy(rExt, 2, MaxInt);
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_CLASSES_ROOT;
      OpenKey('.' + rExt, True);
      WriteString('', rExt + 'file');
      CloseKey;
      CreateKey(rExt + 'file');
      OpenKey(rExt + 'file\DefaultIcon', True);
      WriteString('', rTitle + ',0');
      CloseKey;
      OpenKey(rExt + 'file\shell\open\command', True);
      WriteString('', rTitle + ' "%1"');
      CloseKey;
      Result := True;
    finally
      Free;
    end;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function TFileExtAssociate.UnRegisterFileType(uExt: string; uTitle: string): Boolean;
var
  st: string;
begin
  if uExt = '' then
  begin
    if Assigned(FOnError) then
      FOnError('Can not remove a null extension.');
    Result := False;
    exit;
  end;
  with TRegistry.Create do
  begin
    try
      if AnsiPos('.', uExt) = 0 then
        st := '.' + uExt;
      RootKey := HKEY_CLASSES_ROOT;
      if KeyExists(st) then
        DeleteKey(uExt)
      else
      begin
        if Assigned(FOnError) then
          FOnError('Error occured while removing ' + st);
        //Result := False;
      end;
      if uExt[1] = '.' then
        uExt := Copy(uExt, 2, MaxInt);
      if KeyExists(uExt + 'file') then
        DeleteKey(uExt + 'file')
      else
      begin
        if Assigned(FOnError) then
          FOnError('Error occured while removing ' + uExt + 'file');
        //Result := False;
      end;
      Result := True;
    finally
      Free;
    end;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

// Shortcuts====================================================================

function TFileExtAssociate.CreateShortcut(SourceFileName: string; ShellFolder: TShortcutItems;
  SubFolder, WorkingDir, Parameters, Description: string): string; //Idea by Smot
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  FolderPath, LinkName: string;
  ShortcutFile: WideString;
  Reg: TRegIniFile;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;
  MySLink.SetPath(PChar(SourceFileName));
  MySLink.SetArguments(PChar(Parameters));
  MySLink.SetDescription(PChar(Description));
  LinkName := ChangeFileExt(SourceFileName, '.lnk');
  LinkName := ExtractFileName(LinkName);
  if LinkName = '' then
  begin
    if Assigned(FOnError) then
      FOnError('Error occured creating link.');
    Result := '';
    Exit;
  end
  else
  begin
    Reg := TRegIniFile.Create(SHELL_FOLDERS_ROOT);
    try
      case ShellFolder of
        ProgramsSubDir: FolderPath := Reg.ReadString('Shell Folders',
            'Programs', '') + '\' + SubFolder;
        Desktop: FolderPath := Reg.ReadString('Shell Folders', 'Desktop', '');
        StartUp: FolderPath := Reg.ReadString('Shell Folders', 'Startup', '');
        StartMenu: FolderPath := Reg.ReadString('Shell Folders', 'Start Menu', '');
        SendTo: FolderPath := Reg.ReadString('Shell Folders', 'SendTo', '');
        Programs: FolderPath := Reg.ReadString('Shell Folders', 'Programs', '');
        QuickLaunch: FolderPath := Reg.ReadString('Shell Folders', 'AppData', '') +
          '\Microsoft\Internet Explorer\Quick Launch';
      end;
    finally
      Reg.Free;
    end;
  end;
  if FolderPath <> '' then
  begin
    if (SubFolder <> '') and (ShellFolder = ProgramsSubDir) then
    begin
      if CreateDir(FolderPath) then
        ShortcutFile := FolderPath + '\' + LinkName;
    end
    else
      if (SubFolder <> '') and (ShellFolder <> ProgramsSubDir) then
        ShortcutFile := FolderPath + '\' + SubFolder + '\' + LinkName
      else
        ShortcutFile := FolderPath + '\' + LinkName;
    if WorkingDir = '' then
      MySLink.SetWorkingDirectory(PChar(ExtractFilePath(SourceFileName)))
    else
      MySLink.SetWorkingDirectory(PChar(WorkingDir));
    MyPFile.Save(PWChar(ShortcutFile), False);
    Result := ShortcutFile;
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError('Error occured while locating path to the shortcut.');
    Result := '';
    Exit;
  end;
end;

function TFileExtAssociate.CreateShortcutSendTo(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('SendTo', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutStartUp(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('StartUp', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutStartMenu(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('StartMenu', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutPrograms(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('Programs', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutDesktop(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('Desktop', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutOtherFolder(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, cSubDir, '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('ProgramsSubDir', 'Add', Result);
end;

function TFileExtAssociate.CreateShortcutQuickLaunch(cExeName, cSubDir, cWorkDir, cParam, cTitle: string; CShellDir: TShortcutItems): HResult;
begin
  Result := S_OK;
  CreateShortcut(cExeName, CShellDir, '', '', '', cTitle);
  if Assigned(FOnShortcut) then
    FOnShortcut('QuickLaunch', 'Add', Result);
end;

function TFileExtAssociate.RemoveShortcuts(rmExeName: string): HResult;
var
  FolderPath: WideString;
  Reg: TRegIniFile;
  FName, IName: string;
begin
  if rmExeName = '' then
  begin
    rmExeName := Application.ExeName;
    FApplicationDetails.FAppExeName := rmExeName;
    if not FileExists(rmExeName) then
      if Assigned(FOnError) then
        FOnError('Error occured while removing shortcuts. (nil Exe file)');
    Result := S_FALSE;
    Exit;
  end
  else
  begin
    Reg := TRegIniFile.Create(SHELL_FOLDERS_ROOT);
    IName := ExtractFileName(rmExeName);
    FName := ChangeFileExt(IName, '.lnk');
    try
      FolderPath := Reg.ReadString('Shell Folders', 'Programs', '') + '\' + FAppShortcuts.FMenuSubDir;
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        RemoveDir(FolderPath);
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;

      FolderPath := Reg.ReadString('Shell Folders', 'Desktop', '');
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('Desktop', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
      FolderPath := Reg.ReadString('Shell Folders', 'Startup', '');
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('Startup', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
      FolderPath := Reg.ReadString('Shell Folders', 'Start Menu', '');
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('Start Menu', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
      FolderPath := Reg.ReadString('Shell Folders', 'SendTo', '');
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('SendTo', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
      FolderPath := Reg.ReadString('Shell Folders', 'Programs', '');
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('Programs', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
      FolderPath := Reg.ReadString('Shell Folders', 'AppData', '') +
        '\Microsoft\Internet Explorer\Quick Launch';
      if FileExists(FolderPath + '\' + FName) and
        DeleteFile(FolderPath + '\' + FName) then
      begin
        Result := S_OK;
        if Assigned(FOnShortcut) then
          FOnShortcut('Quick Launch', 'Remove', Result);
      end
      else
      begin
        Result := S_FALSE;
        if Assigned(FOnShortcut) then
          FOnShortcut('ProgramsSubDir', 'Remove', Result);
      end;
    finally
      Reg.Free;
    end;
  end;
end;

function TFileExtAssociate.UpdateShortcutItemValue: LongInt;
const
  AcardShortcutItemValues: array[TShortcutItems] of Cardinal =
  ($01, $02, $03, $04, $05, $06, $07, $08);
var
  i: TShortcutItems;
  j: Longint;
begin
  j := 0;
  if (FAppShortcuts.FOptions <> []) then
    for i := Low(TShortcutItems) to High(TShortcutItems)
      do
      if (i in FAppShortcuts.FOptions) then
        Inc(j, AcardShortcutItemValues[i]);
  Result := j;
end;

//==============================================================================

function FormatSize(Byte: Double): string;
begin
  if (Byte < 1024) then
    Result := Format('%.2n b', [Byte])
  else
  begin
    Byte := (Byte / 1024);
    if (Byte < 1024) then
      Result := Format('%.2n Kb', [Byte])
    else
    begin
      Byte := (Byte / 1024);
      Result := Format('%.2n Mb', [Byte]);
    end;
  end;
end;

procedure GetFileInfo(Path: string; var Info: TFileInfo);
var
  SHFileInfo: TSHFileInfo;
  SearchRec: TSearchRec;
  intFileAge: Integer;
begin
  if Trim(Path) <> '' then
  begin
  ShGetFileInfo(PChar(Path), 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_ICON);
  ZeroMemory(@Info, 0);
  with Info do
  begin
    Icon := SHFileInfo.hIcon;
    Image := SHFileInfo.iIcon;
    DisplayName := SHFileInfo.szDisplayName;
    TypeName := SHFileInfo.szTypeName;
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_DEPRECATED OFF}{$ENDIF}
    intFileAge := FileAge(Path);
    if intFileAge > -1 then
      DateTime := FileDateToDateTime(intFileAge);
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_DEPRECATED ON}{$ENDIF}
    if FindFirst(Path, 0, SearchRec) = 0 then
    begin
      Size := SearchRec.Size;
      SizeAsString := FormatSize(Size);
      FindClose(searchRec);
    end;
  end;
 end;
end;

function TFileExtAssociate.GetExeByExtension(sExt: string): string;
var
  sExtDesc: string;
begin
  with TRegistry.Create do
  begin
    try
      if sExt = '' then
      begin
        if Assigned(FOnError) then
          FOnError('Can not locate a null extension');
        Result := '';
        Exit;
      end;
      if AnsiPos('.', sExt) = 0 then
        sExt := '.' + sExt;
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly(sExt) then
      begin
        sExtDesc := ReadString('');
        CloseKey;
      end;
      if sExtDesc <> '' then
      begin
        if OpenKeyReadOnly(sExtDesc + '\Shell\Open\Command') then
          Result := ReadString('');
      end;
    finally
      Free;
    end;
  end;

  if Result <> '' then
  begin
    if Result[1] = '"' then
    begin
      Result := Copy(Result, 2, -1 + Pos('"', Copy(Result, 2, MaxINt)));
    end
  end;
end;

function TFileExtAssociate.GetIconByExtension(Extension: string; Small: Boolean): HIcon;
var
  Info: TSHFileInfo;
  Flags: Cardinal;
begin
  if Small then
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES;
  if AnsiPos('.', Extension) = 0 then
    Extension := '.' + Extension;
  SHGetFileInfo(PChar(Extension), FILE_ATTRIBUTE_NORMAL, Info, SizeOf(TSHFileInfo), Flags);
  Result := Info.hIcon;
end;

function TFileExtAssociate.GetFileDisplayName(const Path: string): string;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.DisplayName;
end;

function TFileExtAssociate.GetFileSize(const Path: string): string;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.SizeAsString;
end;

function TFileExtAssociate.GetFileTypeName(const Path: string): string;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.TypeName;
end;

function TFileExtAssociate.GetFileImage(const Path: string): Integer;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.Image;
end;

function TFileExtAssociate.GetFileIcon(const Path: string): HIcon;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.Icon;
end;

function TFileExtAssociate.GetFileDateTime(const Path: string): TDateTime;
var
  Info: TFileInfo;
begin
  GetFileInfo(Path, Info);
  Result := Info.DateTime;
end;

end.
