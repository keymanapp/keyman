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
                    26 Dec 2013 - mcdurdin - I4012 - V9.0 - Merge I4007 - Keyman check language environment blocks install if "remind me later" ticked
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
    FDefaultBCP47Tag: string;
    //FLanguageEnvironmentManager: TLanguageEnvironmentManager; // I1220

    procedure SetInstallFile(const Value: string);
    procedure DeleteFileReferences;
    procedure CheckLogFileForWarnings(const Filename: string; Silent: Boolean);
    function CleanupPaths(var xml: string): string;
    function InstallTipForKeyboard(const BCP47Tag: string): Boolean;
    procedure SetDefaultBCP47Tag(const Value: string);
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    procedure InstallKeyboard(const ALogFile, BCP47Tag: string);
    property DefaultBCP47Tag: string read FDefaultBCP47Tag write SetDefaultBCP47Tag;
    property InstallFile: string read FInstallFile write SetInstallFile;
    property Silent: Boolean read FSilent write FSilent;
  end;

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
  Keyman.UI.UfrmProgress,
  Keyman.Configuration.UI.MitigationForWin10_1803,
  Keyman.Configuration.System.HttpServer.App.InstallKeyboard,
  Keyman.Configuration.System.UmodWebHttpServer,
  Keyman.Configuration.System.TIPMaintenance,
  kmcomapi_errors,
  kmint,
  OnlineConstants,
  TempFileManager,
  utildir,
  utilkmshell,
  utilsystem,
  utiluac,
  utilxml,
  Xml.XmlDoc,
  Xml.XmlIntf;

{ TfrmInstall }

{-------------------------------------------------------------------------------
 - Property function handlers                                                  -
 ------------------------------------------------------------------------------}

procedure TfrmInstallKeyboard.SetDefaultBCP47Tag(const Value: string);
begin
  FDefaultBCP47Tag := (kmcom as IKeymanBCP47Canonicalization).GetCanonicalTag(Value);
end;

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

    FXML := FXML + '<DefaultBCP47Tag>' + XMLEncode(FDefaultBCP47Tag) + '</DefaultBCP47Tag>';

    Data := TInstallKeyboardSharedData.Create(FXML, FTempPath, FPackagePath, FFiles);
    PageTag := modWebHttpServer.SharedData.Add(Data);
    FRenderPage := 'installkeyboard';
    Content_Render('tag='+IntToStr(PageTag));
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

  if Assigned(doc.DocumentElement) then
  begin
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
  BCP47Tag: string;
begin
  BCP47Tag := '';
  if (command = 'keyboard_install') and kmcom.SystemInfo.IsAdministrator then   // I4172
  begin
    if params.Count = 1 then
      BCP47Tag := params.ValueFromIndex[0];
    TfrmProgress.Execute(Self,
      function(Manager: IProgressManager): Boolean
      begin
        Manager.Title := 'Installing Keyboard';
        Manager.CanCancel := False;
        Manager.UpdateProgress('Installing Keyboard', 0, 0);
        InstallKeyboard('', BCP47Tag);
        Result := True;
      end
    );
  end
  else if command = 'keyboard_cancel' then
    ModalResult := mrCancel
  else if (command = 'keyboard_installallusers') or (command = 'keyboard_install') then   // I4172
  begin
    if params.Count = 1 then
      BCP47Tag := params.ValueFromIndex[0];
    TfrmProgress.Execute(Self,
      function(Manager: IProgressManager): Boolean
      begin
        Manager.Title := 'Installing Keyboard';
        Manager.CanCancel := False;
        Manager.UpdateProgress('Installing Keyboard', 0, 0);
        t := TTempFileManager.Get('.log');
        try
          if WaitForElevatedConfiguration(Handle, '-log "'+t.Name+'" -s -i "'+FInstallFile+'='+BCP47Tag+'" -nowelcome') = 0 then
          begin
            // install the keyboard tip
            if not InstallTipForKeyboard(BCP47Tag) then
              Exit(False);

            CheckForMitigationWarningFor_Win10_1803(False, '');
            ModalResult := mrOk;
          end
          else
            ModalResult := mrCancel;

          CheckLogFileForWarnings(t.Name, False);
        finally
          t.Free;
        end;
        Result := True;
      end
    );
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

// TODO: move this to TInstallFile
procedure TfrmInstallKeyboard.InstallKeyboard(const ALogFile, BCP47Tag: string);
var
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
  pkg: IKeymanPackageInstalled;
  //FKeyboardsList: WideString;
  j: Integer;
  FInstalledPackage: IKeymanPackageInstalled;
  FInstalledKeyboard: IKeymanKeyboardInstalled;
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
        FInstalledKeyboard := (FKeyboard as IKeymanKeyboardFile2).Install2(True);
        if not InstallTipForKeyboard(BCP47Tag) then
        begin
          // TODO can we return a failure code?
          Exit;
        end;
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

        (FPackage as IKeymanPackageFile2).Install2(True);

        kmcom.Refresh;

        if not InstallTipForKeyboard(BCP47Tag) then
        begin
          Exit;
        end;

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

function TfrmInstallKeyboard.InstallTipForKeyboard(const BCP47Tag: string): Boolean;
  function DoInstallTipForKeyboard(const BCP47Tag: string): Boolean;
  var
    i: Integer;
  begin
    // Install the TIP for the current user
    if Assigned(FKeyboard) then
    begin
      if BCP47Tag <> ''
        then Result := TTIPMaintenance.DoInstall(FKeyboard.ID, BCP47Tag)
        else Result := TTIPMaintenance.DoInstall(FKeyboard.ID, TTIPMaintenance.GetFirstLanguage(FKeyboard));
    end
    else
    begin
      if (FPackage.Keyboards.Count = 1) and (BCP47Tag <> '') then
        Result := TTIPMaintenance.DoInstall(FPackage.Keyboards[0].ID, BCP47Tag)
      else
      begin
        Result := True;
        for i := 0 to FPackage.Keyboards.Count - 1 do
          // For a multi-keyboard package, it's hard to know what to do!
          if not TTIPMaintenance.DoInstall(FPackage.Keyboards[i].ID,
              TTIPMaintenance.GetFirstLanguage(FPackage.Keyboards[i] as IKeymanKeyboardFile)) then
            Result := False;
      end;
    end;
  end;
begin
  // Ensure keyboard is recorded in CU registry before we install the TIP, otherwise it will be marked as disabled by default,
  // as installing the TIP adds CU registry settings, potentially confusing the keyboard settings

  // Only do this for the keyboards, so we don't cause a global refresh
  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;

  Result := DoInstallTipForKeyboard(BCP47Tag);
  if not Result then
  begin
    // We'll silently fall back to installing under the default language;
    // they may have hit the limit of custom languages but an error message
    // is likely to be very confusing.
    Result := DoInstallTipForKeyboard(TTIPMaintenance.GetUserDefaultLanguage);
    if not Result then
    begin
      if not FSilent then
        ShowMessage(MsgFromIdFormat(SKInstallLanguageTransientLimit, [BCP47Tag]));
    end;
  end;
end;

end.
