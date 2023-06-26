(*
  Name:             KPInstallVisualKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      3 May 2011

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit KPInstallVisualKeyboard;

interface

uses
  kpbase;

type
  TKPInstallVisualKeyboard = class(TKPBase)
  public
    procedure Execute(FileName, KeyboardName: string);
  end;

implementation

uses
  Windows,
  Classes,
  SysUtils,
  ErrorControlledRegistry,
  RegistryKeys,
  KeymanErrorCodes,
  utilkeyman,
  utildir,
  VisualKeyboard,
  Variants;

procedure TKPInstallVisualKeyboard.Execute(FileName, KeyboardName: string);
var
  SDest: string;
  FIsAdmin: Boolean;
  Path: string;
begin
  with TVisualKeyboard.Create do
  try
    LoadFromFile(FileName);

    // TODO: Enforce passing in a KeyboardName so that we never use
    // AssociatedKeyboard any more
    if KeyboardName = '' then
      KeyboardName := Header.AssociatedKeyboard;

    if KeyboardInstalled(KeyboardName, FIsAdmin) then
    begin
      with TRegistryErrorControlled.Create do  // I2890
      try
        if FIsAdmin then
        begin
          RootKey := HKEY_LOCAL_MACHINE;
          Path := GetRegistryKeyboardInstallKey_LM(KeyboardName);
        end
        else
        begin
          RootKey := HKEY_CURRENT_USER;
          Path := GetRegistryKeyboardInstallKey_CU(KeyboardName);
        end;

        if OpenKey(Path, True) then
        begin
          if not ValueExists(SRegValue_KeymanFile) then Exit;
          SDest := ExtractFilePath(ReadString(SRegValue_KeymanFile));
          if ValueExists(SRegValue_VisualKeyboard) then
            DeleteFileCleanAttr(SDest + ExtractFileName(ReadString(SRegValue_VisualKeyboard)));   // I4574

          WriteString(SRegValue_VisualKeyboard, SDest + ExtractFileName(FileName));

          if not CopyFileCleanAttr(PChar(FileName), PChar(SDest + ExtractFileName(FileName)), False) then   // I4574
          begin
            DeleteValue(SRegValue_VisualKeyboard);
            ErrorFmt(KMN_E_VisualKeyboard_Install_CouldNotInstall, VarArrayOf([FileName]));
          end;
        end
        else
          ErrorFmt(KMN_E_VisualKeyboard_Install_CouldNotInstall, VarArrayOf([FileName]));
      finally
        Free;
      end;
    end
    else
      ErrorFmt(KMN_E_VisualKeyboard_Install_KeyboardNotInstalled, VarArrayOf([FileName, KeyboardName]));
  finally
    Free;
  end;

  Context.Control.AutoApplyKeyman;
end;

end.
