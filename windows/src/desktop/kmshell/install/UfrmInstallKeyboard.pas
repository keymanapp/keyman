(*
  Name:             UfrmInstallKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Check if keyboard is already installed and uninstall if so
                    06 Oct 2006 - mcdurdin - Display welcome after package install
                    04 Dec 2006 - mcdurdin - Change to a xml/xslt/html page
                    05 Dec 2006 - mcdurdin - Refactor using XML-Renderer
                    12 Dec 2006 - mcdurdin - Refresh after uninstalling keyboard
                    12 Dec 2006 - mcdurdin - Fix package and keyboard names in messages
                    12 Dec 2006 - mcdurdin - Capitalize form name
                    04 Jan 2007 - mcdurdin - Fix crash when install keyboard fails to initialise
                    30 May 2007 - mcdurdin - InstallKeyboardFromFile - refactored from UfrmMain
                    23 Aug 2007 - mcdurdin - Refactor to DoShowPackageWelcome
                    23 Aug 2007 - mcdurdin - Display errors with corrupt packages instead of crashing
                    14 Sep 2007 - mcdurdin - I956 - Ensure the select language dialog appears after installing a locale pack
                    17 Sep 2007 - mcdurdin - I1070 - Add silent flag (only used for locale downloads at present)
                    05 Nov 2007 - mcdurdin - I869 - Add elevated package/keyboard install
                    05 Nov 2007 - mcdurdin - I1109 - Support selecting the keyboards to install for desktop light
                    07 Nov 2007 - mcdurdin - I1109 - Fix bug where package has 2 keyboards, 1 keyboard already installed
                    27 Mar 2008 - mcdurdin - I1220 - Add language environment checks
                    27 Mar 2008 - mcdurdin - I1168 - Fix kbd.OwnerPackage causing crash (Assigned check)
                    14 Jun 2008 - mcdurdin - I1220 - Refactor language environment into kmcomapi
                    14 Jun 2008 - mcdurdin - Install multiple files together
                    20 Jul 2008 - mcdurdin - I1524 - Fix crashes when installing keyboards
                    16 Jan 2009 - mcdurdin - I1793 - Add shield icon to "install for all users"
                    25 May 2009 - mcdurdin - I1995 - Don't ask about uninstalling if FSilent
                    12 Mar 2010 - mcdurdin - I2215 - Fix crash upgrading keyboards and packages
                    12 Mar 2010 - mcdurdin - I2169 - Fix installed keyboard count when reinstalling an existing package in Light
                    29 Mar 2010 - mcdurdin - I2259 - Install for all users should not show install dialog twice
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 Feb 2011 - mcdurdin - I2719 - Lang Config Tasks not showing locale strings when installing keyboard
                    28 Feb 2011 - mcdurdin - I2701 - Installing a package for all users does not appear to prompt for lang config tasks
                    25 Mar 2011 - mcdurdin - I2678 - Uninitialized variant can crash Keyman Configuration when reading package data
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Dec 2012 - mcdurdin - I3612 - V9.0 - Keyboard install should run as Admin only
                    26 Dec 2013 - mcdurdin - I4012 - V9.0 - Merge I4007 - Keyman Desktop check language environment blocks install if "remind me later" ticked
                    24 Apr 2014 - mcdurdin - I4172 - Pressing Enter in install keyboard dialog gives error about admin req
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit UfrmInstallKeyboard;  // I3306   // I3612

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.WideStrings,

  Winapi.Messages,
  Winapi.Windows,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.StdCtrls,

  keymanapi_TLB,
  UfrmKeymanBase,
  UfrmWebContainer;


type
  TfrmInstallKeyboard = class(TfrmWebContainer)
    procedure TntFormDestroy(Sender: TObject);
  private
    FInstallFile: string;
    FKeyboard: IKeymanKeyboardFile;
    FPackage: IKeymanPackageFile;

    FFiles: TStringDynArray;
    FTempPath: string;
    FSilent: Boolean;
    PageTag: Integer;
    //FLanguageEnvironmentManager: TLanguageEnvironmentManager; // I1220

    procedure SetInstallFile(const Value: string);
    procedure DeleteFileReferences;
    procedure InstallKeyboard(const ALogFile: string);
    procedure CheckLogFileForWarnings(const Filename: string; Silent: Boolean);
    function CleanupPaths(var xml: string): string;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    property InstallFile: string read FInstallFile write SetInstallFile;
    property Silent: Boolean read FSilent write FSilent;
  end;

function InstallKeyboardFromFile(Owner: TComponent): Boolean;
function InstallFile(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean; const LogFile: string): Boolean;
function InstallFiles(Owner: TComponent; const FileNames: TStrings; ASilent: Boolean): Boolean;

implementation

{$R *.DFM}

uses
  System.StrUtils,
  System.Variants,
  System.Win.ComObj,
  Winapi.ShellApi,

  custinterfaces,
  GetOSVersion,
  MessageIdentifierConsts,
  MessageIdentifiers,
  Keyman.Configuration.UI.MitigationForWin10_1803,
  Keyman.Configuration.System.HttpServer.App.InstallKeyboard,
  Keyman.Configuration.System.UmodWebHttpServer,
  kmcomapi_errors,
  kmint,
  OnlineConstants,
  TempFileManager,
  UfrmHTML,
  //UfrmSelectLanguage,
  utildir,
  utilkmshell,
  utilsystem,
  utiluac,
  UtilWaitForTSF,
  Xml.XmlDoc,
  Xml.XmlIntf;

{procedure FixupShadowKeyboards;
begin

end;}

procedure InstallKeyboardPackageLanguage(FPackage: IKeymanPackageInstalled; const BCP47: string);
var
  DefaultBCP47Language: string;
  i: Integer;
begin
  if FPackage.Keyboards.Count > 1 then
  begin
    // We don't attempt to propagate the language association preference
    // when there is more than one keyboard in the package. This means a
    // little bit of nasty hoop jumping in order to get the default
    // language code for each keyboard.
    for i := 0 to FPackage.Keyboards.Count - 1 do
    begin
      DefaultBCP47Language := FPackage.Keyboards[i].DefaultBCP47Languages;
      if DefaultBCP47Language.Contains(' ') then
        DefaultBCP47Language := DefaultBCP47Language.Split([' '])[0];
      (FPackage.Keyboards[i] as IKeymanKeyboardInstalled).Languages.Install(DefaultBCP47Language);
    end;
  end
  else if FPackage.Keyboards.Count > 0 then
    (FPackage.Keyboards[0] as IKeymanKeyboardInstalled).Languages.Install(BCP47);
end;

function InstallFiles(Owner: TComponent; const FileNames: TStrings; ASilent: Boolean): Boolean;
var
  I, J: Integer;
  FPackage: IKeymanPackageInstalled;
  FKeyboard: IKeymanKeyboardInstalled;
  Filename: string;
  FilenameBCP47: TArray<string>;
  IsPackage: Boolean;
begin
  Result := False;
  FPackage := nil;
  FKeyboard := nil;

  for I := 0 to FileNames.Count - 1 do
  begin
    try
      kmcom.Keyboards.Refresh;

      FilenameBCP47 := Filenames[I].Split(['=']);
      Filename := FilenameBCP47[0];
      IsPackage := AnsiSameText(ExtractFileExt(FileNames[i]), '.kmp');

      if (Length(FilenameBCP47) > 1) and (FilenameBCP47[1] <> '') then
      begin
        if IsPackage then
        begin
          FPackage := (kmcom.Packages as IKeymanPackagesInstalled2).Install2(FileNames[i], True, False);
          InstallKeyboardPackageLanguage(FPackage, FilenameBCP47[2]);
        end
        else
        begin
          FKeyboard := (kmcom.Keyboards as IKeymanKeyboardsInstalled2).Install2(FileNames[i], True, False);
          FKeyboard.Languages.Install(FilenameBCP47[2]);
        end;
      end
      else
      begin
        if AnsiSameText(ExtractFileExt(FileNames[i]), '.kmp')
          then kmcom.Packages.Install(FileNames[i], True)
          else kmcom.Keyboards.Install(FileNames[i], True);
      end;
      CheckForMitigationWarningFor_Win10_1803(ASilent, '');
    except
      on E:EOleException do
      begin
        if kmcom.Errors.Count = 0 then Raise;
        if not ASilent then
          for j := 0 to kmcom.Errors.Count - 1 do
            ShowMessage(kmcom.Errors[j].Description);
        Exit;
      end;
    end;

    FPackage := nil;
    FKeyboard := nil;
  end;

  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  Result := True;
end;

procedure AddDefaultLanguageHotkey(Keyboard: IKeymanKeyboardInstalled);
var
  i: Integer;
begin
  if not Keyboard.DefaultHotkey.IsEmpty and (Keyboard.Languages.Count > 0) then
  begin
    for i := 0 to kmcom.Languages.Count - 1 do
    begin
      if kmcom.Languages[i].KeymanKeyboardLanguage = Keyboard.Languages[0] then
      begin
        kmcom.Languages[i].Hotkey.RawValue := Keyboard.DefaultHotkey.RawValue;
        Break;
      end;
    end;
  end;
end;

procedure AddDefaultLanguageHotkeys(InstalledKeyboards: array of IKeymanKeyboardInstalled);
var
  i: Integer;
begin
  for i := 0 to High(InstalledKeyboards) do
    AddDefaultLanguageHotkey(InstalledKeyboards[i]);
end;

function InstallFile(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean; const LogFile: string): Boolean;
var
  n: Integer;
  InstalledKeyboards: array of IKeymanKeyboardInstalled;
  InstalledPackage: IKeymanPackageInstalled;
  i: Integer;
begin
  with TfrmInstallKeyboard.Create(Owner) do
  try
    Silent := ASilent;
    InstallFile := FileName;
    if ModalResult = mrCancel then
      // failed to start install
      Result := False
    else
    begin
      if ASilent then
      begin
        InstallKeyboard(LogFile);
        Result := True;
      end
      else
        Result := ShowModal = mrOk;
    end;
  finally
    Free;
  end;

  if not Result then
    Exit;

  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  kmcom.Packages.Refresh;

  InstalledPackage := nil;
  SetLength(InstalledKeyboards, 0);
  if LowerCase(ExtractFileExt(FileName)) = '.kmp' then
  begin
    n := kmcom.Packages.IndexOf(FileName);
    if (n >= 0) then
    begin
      InstalledPackage := kmcom.Packages[n];
      SetLength(InstalledKeyboards, InstalledPackage.Keyboards.Count);
      for i := 0 to InstalledPackage.Keyboards.Count - 1 do
        InstalledKeyboards[i] := InstalledPackage.Keyboards[i] as IKeymanKeyboardInstalled;
    end;
  end
  else
  begin
    n := kmcom.Keyboards.IndexOf(FileName);
    if n >= 0 then
    begin
      SetLength(InstalledKeyboards, 1);
      InstalledKeyboards[0] := kmcom.Keyboards[n];
    end;
  end;

  kmcom.Languages.Apply;
  TWaitForTSF.WaitForLanguageProfilesToBeApplied(InstalledKeyboards);
  AddDefaultLanguageHotkeys(InstalledKeyboards);

  if InstalledPackage <> nil then
  begin
    //if not ASilent then SelectLanguage(False);
    if not ASilent and not ANoWelcome then
      DoShowPackageWelcome(InstalledPackage, False);
  end;

  {$MESSAGE HINT 'How do we correlate this with a Cancel in configuration? Do we change that to Close?' }
  kmcom.Apply;
end;

{ TfrmInstall }

{-------------------------------------------------------------------------------
 - Property function handlers                                                  -
 ------------------------------------------------------------------------------}

procedure TfrmInstallKeyboard.SetInstallFile(const Value: string);
var
  FXML: string;
  FFileReferences: OleVariant;
  i: Integer;
  Data: IInstallKeyboardSharedData;
  FPackagePath: string;
begin
  screen.Cursor := crHourglass;
  try
    FInstallFile := Value;
    FTempPath := KGetTempPath;
    if AnsiSameText(ExtractFileExt(Value), '.kmp') then
    begin
      try
        FPackage := kmcom.Packages.GetPackageFromFile(Value);
      except
        on E:EOleException do
        begin
          if kmcom.Errors.Count = 0 then Raise;
          for i := 0 to kmcom.Errors.Count - 1 do
            ShowMessage(kmcom.Errors[i].Description);
          ModalResult := mrCancel;
          Exit;
        end;
      end;
      FFileReferences := Null; // I2678
      FXML := FPackage.SerializeXML(keymanapi_TLB.ksfExportImages, FTempPath, FFileReferences);
    end
    else
    begin
      try
        FKeyboard := kmcom.Keyboards.GetKeyboardFromFile(Value);
      except
        on E:EOleException do
        begin
          if kmcom.Errors.Count = 0 then Raise;
          for i := 0 to kmcom.Errors.Count - 1 do
            ShowMessage(kmcom.Errors[i].Description);
          ModalResult := mrCancel;
          Exit;
        end;
      end;
      FFileReferences := Null; // I2678
      FXML := FKeyboard.SerializeXML(keymanapi_TLB.ksfExportImages, FTempPath, FFileReferences);
    end;

    if VarIsArray(FFileReferences) then
    begin
      SetLength(FFiles, VarArrayHighBound(FFileReferences, 1) - VarArrayLowBound(FFileReferences, 1) + 1);
      for I := VarArrayLowBound(FFileReferences, 1) to VarArrayHighBound(FFileReferences, 1) do
        FFiles[I-VarArrayLowBound(FFileReferences, 1)] := FFileReferences[I];
    end;

    FPackagePath := CleanupPaths(FXML);

    Data := TInstallKeyboardSharedData.Create(FXML, FTempPath, FPackagePath, FFiles);
    PageTag := modWebHttpServer.SharedData.Add(Data);
    FRenderPage := 'installkeyboard';
    Content_Render(False, 'tag='+IntToStr(PageTag));
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TfrmInstallKeyboard.CleanupPaths(var xml: string): string;
var
  doc: IXMLDocument;
  node: IXMLNode;
begin
  // TODO: add ksfRelativePaths to SerializeXML and add ChildValues['packagepath']
  // so this is not necessary; this requires update to Keyman API though.

  Result := '';
  doc := LoadXMLData(xml);

  node := doc.DocumentElement.ChildNodes.FindNode('readme');
  if Assigned(node) then
  begin
    Result := node.NodeValue;
    node.NodeValue := ExtractFileName(Result);
    Result := ExtractFilePath(Result);
  end;

  node := doc.DocumentElement.ChildNodes.FindNode('graphic');
  if Assigned(node) then
  begin
    Result := node.NodeValue;
    node.NodeValue := ExtractFileName(Result);
    Result := ExtractFilePath(Result);
  end;

  doc.SaveToXML(xml);
end;

procedure TfrmInstallKeyboard.TntFormDestroy(Sender: TObject);
begin
  inherited;
  if PageTag > 0 then
    modWebHttpServer.SharedData.Remove(PageTag);
  DeleteFileReferences;
end;

procedure TfrmInstallKeyboard.DeleteFileReferences;
var
  I: Integer;
begin
  for I := Low(FFiles) to High(FFiles) do
    System.SysUtils.DeleteFile(FTempPath + FFiles[i]);   // I4181
  SetLength(FFiles, 0);
end;

procedure TfrmInstallKeyboard.FireCommand(const command: WideString; params: TStringList);
var
  t: TTempFile;
begin
  if (command = 'keyboard_install') and kmcom.SystemInfo.IsAdministrator then   // I4172
  begin
    InstallKeyboard('');
  end
  else if command = 'keyboard_cancel' then
    ModalResult := mrCancel
  else if (command = 'keyboard_installallusers') or (command = 'keyboard_install') then   // I4172
  begin
    t := TTempFileManager.Get('.log');
    try
      if WaitForElevatedConfiguration(Handle, '-log "'+t.Name+'" -s -i "'+FInstallFile+'" -nowelcome') = 0 then
        ModalResult := mrOk
      else
        ModalResult := mrCancel;

      CheckLogFileForWarnings(t.Name, False);
    finally
      t.Free;
    end;
  end
  else
    inherited;
end;

procedure TfrmInstallKeyboard.CheckLogFileForWarnings(const Filename: string; Silent: Boolean);
var
  str: TStringList;
begin
  if FileExists(Filename) then
  begin
    str := TStringList.Create;
    try
      str.LoadFromFile(Filename);
      ShowMitigationWarningFormFor_Win10_1803(str.Text);
    finally
      str.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
 - Control events                                                              -
 ------------------------------------------------------------------------------}

procedure TfrmInstallKeyboard.InstallKeyboard(const ALogFile: string);
var
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
  pkg: IKeymanPackageInstalled;
  //FKeyboardsList: WideString;
  j: Integer;
begin
  kmcom.Errors.Clear;
  try
    try
      if Assigned(FKeyboard) then
      begin
        for i := 0 to kmcom.Keyboards.Count - 1 do
        begin
          kbd := kmcom.Keyboards[i];
          if WideSameText(kbd.ID, FKeyboard.ID) then
          begin
            if not FSilent and (MessageDlg(MsgFromIdFormat(SKKeyboardAlreadyInstalled, [FKeyboard.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
              Exit;
            if kbd.OwnerPackage <> nil then // I1168
            begin
              if not FSilent and (MessageDlg(MsgFromIdFormat(SKKeyboardPartOfPackage, [kbd.Name, kbd.OwnerPackage.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
                Exit;
              if not kmcom.SystemInfo.IsAdministrator
                then WaitForElevatedConfiguration(Handle, '-s -up "'+kbd.OwnerPackage.ID+'"', True) // I2215
                else kbd.OwnerPackage.Uninstall(True);
            end
            else
            begin
              if not kmcom.SystemInfo.IsAdministrator
                then WaitForElevatedConfiguration(Handle, '-s -uk "'+kbd.ID+'"', True) // I2215
                else kbd.Uninstall;
            end;
            Break;
          end;
        end;
        kbd := nil;
        kmcom.Keyboards.Apply;
        kmcom.Keyboards.Refresh;
        FKeyboard.Install(True);
        CheckForMitigationWarningFor_Win10_1803(FSilent, ALogFile);
      end
      else
      begin
        for i := 0 to kmcom.Packages.Count - 1 do
        begin
          pkg := kmcom.Packages[i];
          if WideSameText(pkg.ID, FPackage.ID) then
          begin
            if not FSilent and (MessageDlg(MsgFromIdFormat(SKPackageAlreadyInstalled, [FPackage.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
              Exit;
            if not kmcom.SystemInfo.IsAdministrator
              then WaitForElevatedConfiguration(Handle, '-s -up "'+pkg.ID+'"', True) // I2215
              else pkg.Uninstall(True);
            Break;
          end;
        end;

        kbd := nil;
        pkg := nil;
        kmcom.Keyboards.Apply;
        kmcom.Keyboards.Refresh;  // I2169

        for i := 0 to kmcom.Keyboards.Count - 1 do // I2215
        begin
          kbd := kmcom.Keyboards[i];
          for j := 0 to FPackage.Keyboards.Count - 1 do
          begin
            if WideSameText(kbd.ID, FPackage.Keyboards[j].ID) then
            begin
              if not FSilent and (MessageDlg(MsgFromIdFormat(SKKeyboardAlreadyInstalled, [kbd.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
                Exit;
              if kbd.OwnerPackage <> nil then // I1168
              begin
                if not FSilent and (MessageDlg(MsgFromIdFormat(SKKeyboardPartOfPackage, [kbd.Name, kbd.OwnerPackage.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
                  Exit;
                if not kmcom.SystemInfo.IsAdministrator
                  then WaitForElevatedConfiguration(Handle, '-s -up "'+kbd.OwnerPackage.ID+'"', True)
                  else kbd.OwnerPackage.Uninstall(True);
              end
              else
              begin
                if not kmcom.SystemInfo.IsAdministrator
                  then WaitForElevatedConfiguration(Handle, '-s -uk "'+kbd.ID+'"', True)
                  else kbd.Uninstall;
              end;
              Break;
            end;
          end;
        end;
        kbd := nil;
        pkg := nil;
        kmcom.Keyboards.Apply;
        kmcom.Keyboards.Refresh;  // I2169

        FPackage.Install(True);
        CheckForMitigationWarningFor_Win10_1803(FSilent, ALogFile);
      end;
    except
      on E:EOleException do
      begin
        if kmcom.Errors.Count > 0 then
        begin
          MessageDlg(kmcom.Errors[0].Description, mtError, [mbOk], 0);
          Exit;
        end
          //if kmcom.Errors[0].ErrorCode = E_ then
        else Raise;
      end;
    end;
  finally
    kbd := nil;
    pkg := nil;
    kmcom.Packages.Refresh;
    kmcom.Keyboards.Refresh;
    kmcom.Keyboards.Apply;
  end;
  ModalResult := mrOk;
end;

function InstallKeyboardFromFile(Owner: TComponent): Boolean;
var
  dlgOpen: TOpenDialog;
begin
  dlgOpen := TOpenDialog.Create(nil);
  try
    dlgOpen.Filter :=
      'Keyman files (*.kmx, *.kxx, *.kmp)|*.kmx;*.kxx;*.kmp|Keyman keyboards (*.kmx,*.kxx)' +
      '|*.kmx;*.kxx|Keyman packages (*.kmp)|*.kmp|All files (*.*)|*.*';
    dlgOpen.Title := 'Install Keyman Keyboard';

    if dlgOpen.Execute then
      Result := InstallFile(Owner, dlgOpen.FileName, False, False, '')
    else
      Result := False;
  finally
    dlgOpen.Free;
  end;
end;

end.
