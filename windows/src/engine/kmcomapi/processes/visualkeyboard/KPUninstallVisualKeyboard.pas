(*
  Name:             KPUninstallVisualKeyboard
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
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit KPUninstallVisualKeyboard;

interface

uses kpbase;

type
  TKPUninstallVisualKeyboard = class(TKPBase)
  public
    procedure Execute(KeyboardName: string);
  end;

implementation

uses windows, classes, sysutils, ErrorControlledRegistry, isadmin, registrykeys,
  utilkeyman, utildir, keymanerrorcodes, Variants;

procedure TKPUninstallVisualKeyboard.Execute(KeyboardName: string);
var
  FInstByAdmin: Boolean;
  KeyboardFullName, Filename: string;
  Path: string;
begin
  Filename := '';

  KeyboardName := GetShortKeyboardName(KeyboardName);

  if not KeyboardInstalled(KeyboardName, FInstByAdmin) then
    ErrorFmt(KMN_E_Uninstall_InvalidKeyboard, VarArrayOf([KeyboardName]));

  if FInstByAdmin
    then KeyboardFullName := GetKeyboardFullName_LM(KeyboardName)
    else KeyboardFullName := GetKeyboardFullName_CU(KeyboardName);

  { Check if keyboard was installed by administrator, and if user has permission to delete it }

  if FInstByAdmin and not IsAdministrator then
    ErrorFmt(KMN_E_Uninstall_AdminKeyboardInstalled, VarArrayOf([KeyboardName]));

  { Find install path }

  with TRegistryErrorControlled.Create do  // I2890
  try
    if FInstByAdmin then // as determined by where the keyboard was installed
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      Path := '\'+SRegKey_InstalledKeyboards_LM+'\'+KeyboardName;
    end
    else
    begin
      RootKey := HKEY_CURRENT_USER;
      Path := '\'+SRegKey_InstalledKeyboards_CU+'\'+KeyboardName;
    end;

    if OpenKey(Path, False) and
        ValueExists(SRegValue_VisualKeyboard) then
      FileName := ReadString(SRegValue_VisualKeyboard)
    else
      ErrorFmt(KMN_E_Uninstall_InvalidKeyboard, VarArrayOf([KeyboardFullName]));

    DeleteValue(SRegValue_VisualKeyboard);
  finally
    Free;
  end;

  { Delete .kvk file }

  if not DeleteFileCleanAttr(FileName) then   // I4181   // I4574
  begin
    WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([FileName]));
  end;

  Context.Control.AutoApplyKeyman;
end;

end.
