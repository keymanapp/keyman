(*
  Name:             uninstall
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    1 Jan 2013
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    28 Aug 2008 - mcdurdin - I1609 - Uninstall requires administrator if installed by admin
                    12 Mar 2010 - mcdurdin - I2216 - Show currently installed fonts in package details
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Jan 2013 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
*)
unit uninstall;  // I3306

interface

uses
  System.UITypes,
  Controls;

function UninstallPackage(Owner: TWinControl; const PackageName: WideString; Silent: Boolean): Boolean;
function UninstallKeyboard(Owner: TWinControl; const KeyboardName: WideString; Silent: Boolean): Boolean;
function UninstallKeyboardLanguage(const LanguageProfileGUID: WideString; Silent: Boolean): Boolean;   // I3624

implementation

uses
  keymanapi_TLB,
  kmint,
  utiluac,
  utilkmshell,
  Dialogs,
  SysUtils,
  MessageIdentifiers,
  MessageIdentifierConsts;

{ I1201, I1169, I1167, I1163, I1150, I1135 - Fix crash uninstalling admin-installed keyboards and packages }

procedure UninstallKeyboardTips(Keyboard: IKeymanKeyboardInstalled);
var
  i: Integer;
begin
  for i := 0 to Keyboard.Languages.Count - 1 do
    if Keyboard.Languages[i].IsInstalled then
      Keyboard.Languages[i].Uninstall;
end;

procedure UninstallPackageTips(Package: IKeymanPackageInstalled);
var
  i: Integer;
begin
  for i := 0 to Package.Keyboards.Count - 1 do
    UninstallKeyboardTips(Package.Keyboards[i] as IKeymanKeyboardInstalled);
end;

function UninstallPackage(Owner: TWinControl; const PackageName: WideString; Silent: Boolean): Boolean;
var
  i, n: Integer;
  pkg: IKeymanPackageInstalled;
  pkgID: WideString;
  Handle: THandle;
  FFonts: WideString;
begin
  Result := False;
  if Assigned(Owner)
    then Handle := Owner.Handle
    else Handle := 0;

  n := kmcom.Packages.IndexOf(PackageName);
  if n < 0 then Exit;
  pkg := kmcom.Packages[n];

  kmcom.Keyboards.Apply;

  FFonts := ''; // I2216
  for i := 0 to pkg.Fonts.Count - 1 do
  begin
    if i > 1 then FFonts := FFonts + ', ';
    FFonts := FFonts + pkg.Fonts[i].Name;
  end;
  if FFonts <> '' then FFonts := MsgFromIdFormat(SKUninstallPackageFonts, [FFonts]);

  if not Silent and (MessageDlg(Trim(MsgFromIdFormat(SKUninstallPackage, [pkg.Name, FFonts])), mtConfirmation, mbOkCancel, 0) = mrCancel) then
    Exit;

  if CanElevate then
  begin
    UninstallPackageTips(pkg);
    pkgID := pkg.ID;
    pkg := nil;
    if WaitForElevatedConfiguration(Handle, '-up "'+pkgID+'" -s') = 0 then
    begin
      kmcom.Keyboards.Refresh;
      kmcom.Keyboards.Apply;
      Result := True;
    end;
    Exit;
  end;

  if not kmcom.SystemInfo.IsAdministrator then
  begin
    { I1609 - Uninstall requires administrator if package was installed by administrator }
    if not Silent then
      ShowMessage(MsgFromIdFormat(SKUninstallRequireAdmin, [pkg.Name]));
    Exit;
  end;

  kmcom.Keyboards.Apply;

  UninstallPackageTips(pkg);
  pkg.Uninstall(True);
  pkg := nil;
  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  Result := True;
end;

{ I1201, I1169, I1167, I1163, I1150, I1135 - Fix crash uninstalling admin-installed keyboards and packages }

function UninstallKeyboard(Owner: TWinControl; const KeyboardName: WideString; Silent: Boolean): Boolean;
var
  n: Integer;
  kbd: IKeymanKeyboardInstalled;
  kbdID: WideString;
  Handle: THandle;
begin
  Result := False;
  if Assigned(Owner)
    then Handle := Owner.Handle
    else Handle := 0;

  n := kmcom.Keyboards.IndexOf(KeyboardName);
  if n < 0 then Exit;
  kbd := kmcom.Keyboards[n];

  if not Silent and (MessageDlg(MsgFromIdFormat(SKUninstallKeyboard, [kbd.Name]), mtConfirmation, mbOkCancel, 0) = mrCancel) then
    Exit;

  // First, uninstall TIPs in user context
  UninstallKeyboardTips(kbd);

  if CanElevate then
  begin
    kbdID := kbd.ID;
    kbd := nil;
    if WaitForElevatedConfiguration(Handle, '-uk "'+kbdID+'" -s') = 0 then
    begin
      kmcom.Keyboards.Refresh;
      kmcom.Keyboards.Apply;
      Result := True;
    end;
    Exit;
  end;

  kmcom.Keyboards.Apply;
  kbd.Uninstall;
  kbd := nil;
  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  Result := True;
end;

function UninstallKeyboardLanguage(const LanguageProfileGUID: WideString; Silent: Boolean): Boolean;   // I3624
var
  I: Integer;
  kbd: IKeymanKeyboardInstalled;
  J: Integer;
  kbdlang: IKeymanKeyboardLanguageInstalled;
  FProfileGUID: TGUID;
begin
  Result := False;

  FProfileGUID := StringToGUID(LanguageProfileGUID);
  kbdlang := nil;
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    kbd := kmcom.Keyboards[I];
    for J := 0 to kbd.Languages.Count - 1 do
    begin
      if IsEqualGUID(kbd.Languages[J].ProfileGUID, FProfileGUID) then
      begin
        kbdlang := kbd.Languages[J];
        Break;
      end;
    end;
  end;

  if not Assigned(kbdlang) then Exit;

  kbdlang.Uninstall;
  kbdlang := nil;
  kmcom.Keyboards.Refresh;
  Result := True;
end;

end.
