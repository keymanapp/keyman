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
                    05 Dec 2006 - mcdurdin - Refactor using XMLRenderer
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
  System.Contnrs,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, keymanapi_TLB, ExtCtrls, Menus, UfrmKeymanBase, UfrmWebContainer,
  OleCtrls, SHDocVw, EmbeddedWB,
  WideStrings, SHDocVw_EWB, EwbCore, KeymanEmbeddedWB;


type
  TfrmInstallKeyboard = class(TfrmWebContainer)
    procedure TntFormDestroy(Sender: TObject);
  private
    FInstallFile: string;
    FKeyboard: IKeymanKeyboardFile;
    FPackage: IKeymanPackageFile;

    FFileReferences: OleVariant;
    FTempPath: string;
    FSilent: Boolean;
    //FLanguageEnvironmentManager: TLanguageEnvironmentManager; // I1220

    procedure SetInstallFile(const Value: string);
    procedure DeleteFileReferences;
    procedure InstallKeyboard;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    property InstallFile: string read FInstallFile write SetInstallFile;
    property Silent: Boolean read FSilent write FSilent;
  end;

function InstallKeyboardFromFile(Owner: TComponent): Boolean;
function InstallFile(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean): Boolean;
function InstallFiles(Owner: TComponent; const FileNames: TWideStrings; ASilent: Boolean): Boolean;

implementation

{$R *.DFM}

uses
  ComObj,
  custinterfaces,
  GenericXMLRenderer,
  GetOSVersion,
  MessageIdentifierConsts,
  MessageIdentifiers,
  kmint,
  OnlineConstants,
  ShellApi,
  StrUtils,
  UfrmHTML,
  //UfrmSelectLanguage,
  utildir,
  utilkmshell,
  utilsystem,
  utiluac,
  UtilWaitForTSF,
  Variants;

{procedure FixupShadowKeyboards;
begin

end;}

function InstallFiles(Owner: TComponent; const FileNames: TWideStrings; ASilent: Boolean): Boolean;
var
  I: Integer;
  //FLanguageEnvironmentManager: TLanguageEnvironmentManager;
  FPackage: IKeymanPackageFile;
  FKeyboard: IKeymanKeyboardFile;
  j: Integer;
begin
  Result := False;
  FPackage := nil;
  FKeyboard := nil;

  for I := 0 to FileNames.Count - 1 do
  begin
    try
      kmcom.Keyboards.Refresh;

      if AnsiSameText(ExtractFileExt(FileNames[i]), '.kmp')
        then kmcom.Packages.Install(FileNames[i], True)
        else kmcom.Keyboards.Install(FileNames[i], True);
    except
      on E:EOleException do
      begin
        if kmcom.Errors.Count = 0 then Raise;
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

function InstallFile(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean): Boolean;
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
        InstallKeyboard;
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
  FXML: WideString;
  i: Integer;
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

    XMLRenderers.RenderTemplate := 'InstallKeyboard.xsl';
    XMLRenderers.Add(TGenericXMLRenderer.Create(FXML));
    Content_Render;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmInstallKeyboard.TntFormDestroy(Sender: TObject);
begin
  inherited;
  DeleteFileReferences;
end;

procedure TfrmInstallKeyboard.DeleteFileReferences;
var
  I: Integer;
begin
  if VarIsArray(FFileReferences) then
    for I := VarArrayLowBound(FFileReferences, 1) to VarArrayHighBound(FFileReferences, 1) do
      DeleteFile(FTempPath + FFileReferences[i]);   // I4181
end;

procedure TfrmInstallKeyboard.FireCommand(const command: WideString; params: TStringList);
begin
  if (command = 'keyboard_install') and kmcom.SystemInfo.IsAdministrator then   // I4172
  begin
    InstallKeyboard;
  end
  else if command = 'keyboard_cancel' then
    ModalResult := mrCancel
  else if (command = 'keyboard_installallusers') or (command = 'keyboard_install') then   // I4172
  begin
    if WaitForElevatedConfiguration(Handle, '-s -i "'+FInstallFile+'" -nowelcome') = 0 then
      ModalResult := mrOk
    else
      ModalResult := mrCancel;
  end
  else
    inherited;
end;

{-------------------------------------------------------------------------------
 - Control events                                                              -
 ------------------------------------------------------------------------------}

procedure TfrmInstallKeyboard.InstallKeyboard;
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
      Result := InstallFile(Owner, dlgOpen.FileName, False, False)
    else
      Result := False;
  finally
    dlgOpen.Free;
  end;
end;

end.
