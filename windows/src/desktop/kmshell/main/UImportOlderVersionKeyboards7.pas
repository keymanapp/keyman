(*
  Name:             UImportOlderVersionKeyboards7
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2010

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2010 - mcdurdin - I2548 - Support for upgrading Desktop 7 to Desktop 8
                    10 Dec 2010 - mcdurdin - I2361 - Support admin and non-admin modes (relates to setup elevation)
                    11 Jan 2011 - mcdurdin - I2642 - Installer uninstalls KM7 keyboards before upgrade can happen
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit UImportOlderVersionKeyboards7;

interface

procedure ImportOlderVersionKeyboards7(DoAdmin: Boolean);   // I2361

implementation

uses
  UImportOlderVersionKeyboards,
  GetOsVersion,
  KeymanVersion,
  kmint,
  Windows,
  Classes,
  ErrorControlledRegistry, 
  RegistryKeys,
  StrUtils,
  SysUtils,
  utildir,
  utilsystem,
  UImportOlderKeyboardUtils;

type
  TImportKeyman7Keyboard = class(TImportKeymanKeyboardBase)
  public
    procedure Import;
  end;

procedure ImportOlderVersionKeyboards7(DoAdmin: Boolean);  // I2361
begin
  with TImportKeyman7Keyboard.Create(DoAdmin) do  // I2361
  try
    Import;
  finally
    Free;
  end;
end;

procedure TImportKeyman7Keyboard.Import;
var
  keysWrite, keys: TStringList;
  i: Integer;
  n: Integer;
  s: string;
begin
  values := TStringList.Create;
  keys := TStringList.Create;
  keysWrite := TStringList.Create;
  regRead := TRegistryErrorControlled.Create;  // I2890
  regWrite := TRegistryErrorControlled.Create;  // I2890
  try
    if FAdmin then  // I2361
    begin
      regRead.RootKey := HKEY_LOCAL_MACHINE;
      regWrite.RootKey := HKEY_LOCAL_MACHINE;
    end
    else
    begin
      regRead.RootKey := HKEY_CURRENT_USER;
      regWrite.RootKey := HKEY_CURRENT_USER;
    end;

    { Copy Installed Keyboards }

    rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine70_InstalledPackages;  // I2642
    rootWrite := '\'+SRegKey_InstalledPackages; // '\Software\...\Keyman Engine\x.x\Installed Packages';

    if regRead.OpenKeyReadOnly(rootRead) then
    begin
      keys.Clear;
      regRead.GetKeyNames(keys);
      for i := 0 to keys.Count - 1 do
      begin
        if FAdmin
          then Log('Copying package '+keys[i]+' (LM)')
          else Log('Copying package '+keys[i]+' (CU)');
        CopyPackage(keys[i]);
      end;
    end;

    rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine70_InstalledKeyboards;  // I2642
    rootWrite := '\'+SRegKey_InstalledKeyboards;  // '\Software\...\Keyman Engine\x.x\Installed Keyboards';

    if regRead.OpenKeyReadOnly(rootRead) then
    begin
      keys.Clear;
      regRead.GetKeyNames(keys);
      for i := 0 to keys.Count - 1 do
        CopyKeyboard(keys[i]);
    end;

    { Copy Active Keyboards }
    if not FAdmin then  // I2361
    begin
      rootRead := SRegKey_UpgradeBackupPath + SRegKey_KeymanEngine70_ActiveKeyboards;  // I2642
      rootWrite := '\'+SRegKey_ActiveKeyboards; //'\Software\...\Keyman Engine\x.x\Active Keyboards';

      if regRead.OpenKeyReadOnly(rootRead) and regWrite.OpenKey(rootWrite, True) then
      begin
        values.Clear;
        keys.Clear;
        keysWrite.Clear;
        regWrite.GetValueNames(keysWrite);
        regRead.GetValueNames(keys);
        n := 0;
        for i := 0 to keysWrite.Count - 1 do
          values.Add(regWrite.ReadString(keysWrite[i]));

        { Write active keyboards }
        for i := 0 to keys.Count - 1 do
        begin
          s := regRead.ReadString(keys[i]);
          if values.IndexOf(s) < 0 then
          begin
            while keysWrite.IndexOf(IntToStr(n)) >= 0 do Inc(n);  // Find next available KeymanID
            regWrite.WriteString(IntToStr(n), s);
            values.Add(s + '=' + IntToStr(n));
            Inc(n);
          end;
        end;

        keys.Clear;
        regRead.GetKeyNames(keys);
        for i := 0 to Keys.Count - 1 do
          CopyKeyboardActive(keys[i]);
      end;
    end;
  finally
    regRead.Free;
    regWrite.Free;
    keys.Free;
    keysWrite.Free;
    values.Free;
  end;
end;

end.
