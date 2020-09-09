(*
  Name:             kpuninstallkeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add AutoApplyKeyman call
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    28 Nov 2012 - mcdurdin - I3599 - V9.0 - Refactor GetKeyboardIconFileName
                    28 Nov 2012 - mcdurdin - I3600 - V9.0 - Delete keyboard icon when uninstalling keyboard
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    13 Dec 2012 - mcdurdin - I3663 - V9.0 - Uninstall of keyboard does not remove KMTIP entries
                    24 Apr 2014 - mcdurdin - I4173 - V9 Uninstalling a keyboard leaves the mnemonic recompiled layouts behind
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
*)
unit kpuninstallkeyboard;

interface

uses kpbase;

type
  TKPUninstallOptions = set of (upPartOfPackage);

  TKPUninstallKeyboard = class(TKPBase)
    procedure Execute(KeyboardName: string; FOptions: TKPUninstallOptions);
  end;

implementation

uses
  Windows,
  SysUtils,
  keymancontext,
  keymanerrorcodes,
  Keyman.System.Process.KPUninstallKeyboardLanguage,
  utildir,
  utilkeyman,
  utilsystem,
  isadmin,
  ErrorControlledRegistry,
  RegistryKeys,
  Variants;

procedure TKPUninstallKeyboard.Execute(KeyboardName: string; FOptions: TKPUninstallOptions);
var
  FileName, VisualKeyboardFileName, KeyboardFullName, PackageName: string;
  FInstByAdmin: Boolean;
  Path, FIconFileName: string;
begin
  KeyboardName := GetShortKeyboardName(KeyboardName);

  if not KeyboardInstalled(KeyboardName, FInstByAdmin) then
    ErrorFmt(KMN_E_Uninstall_InvalidKeyboard, VarArrayOf([KeyboardName]));

  if FInstByAdmin
    then KeyboardFullName := GetKeyboardFullName_LM(KeyboardName)
    else KeyboardFullName := GetKeyboardFullName_CU(KeyboardName);

  if not (upPartOfPackage in FOptions) and KeyboardIsPartOfPackage(KeyboardName, PackageName) then
    ErrorFmt(KMN_E_Uninstall_KeyboardPartOfPackage, VarArrayOf([KeyboardFullName, PackageName]));

  { Check if keyboard was installed by administrator, and if user has permission to delete it }

  if FInstByAdmin and not IsAdministrator then
    ErrorFmt(KMN_E_Uninstall_AdminKeyboardInstalled, VarArrayOf([KeyboardFullName]));

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
        ValueExists(SRegValue_KeymanFile) then
      FileName := ReadString(SRegValue_KeymanFile)
    else
      ErrorFmt(KMN_E_Uninstall_InvalidKeyboard, VarArrayOf([KeyboardFullName]));

    if ValueExists(SRegValue_VisualKeyboard)
      then VisualKeyboardFileName := ReadString(SRegValue_VisualKeyboard)
      else VisualKeyboardFileName := '';
  finally
    Free;
  end;

  { Unregister language profiles }

  with TKPUninstallKeyboardLanguage.Create(Context) do   // I3663
  try
    UnregisterTip(KeyboardName);
  finally
    Free;
  end;

  { Delete .kmx file }

  if not FileExists(FileName) then
    WarnFmt(KMN_W_UninstallFileNotFound, VarArrayOf([FileName]))
  else if not DeleteFileCleanAttr(FileName) then   // I4181   // I4574
  begin
    WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([FileName]));
  end;

  { Delete .kvk file, if it exists }

  if (VisualKeyboardFileName <> '') and FileExists(VisualKeyboardFileName) then   // I4173
    if not DeleteFileCleanAttr(VisualKeyboardFileName) then   // I4181   // I4574
    begin
      WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([FileName]));
    end;

  FIconFileName := GetKeyboardIconFileName(FileName);   // I3599
  if FileExists(FIconFileName) then   // I3600
    if not DeleteFileCleanAttr(FIconFileName) then   // I4181   // I4574
    begin
      WarnFmt(KMN_W_UninstallFileInUse, VarArrayOf([FIconFileName]));
    end;

  { Delete kmx id from registry }

  with TRegistryErrorControlled.Create do  // I2890
  try
    { Delete keyboards from active keyboard list for all users }

    //RemoveActiveKeyboardInfo(KeyboardName); // runs through all users...

    { Delete kmx details from registry }

    if FInstByAdmin then // as determined by where the keyboard was installed
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      Path := GetRegistryKeyboardInstallKey_LM(KeyboardName);
    end
    else
    begin
      RootKey := HKEY_CURRENT_USER;
      Path := GetRegistryKeyboardInstallKey_CU(KeyboardName);
    end;

   // I3613
    if not DeleteKey(Path) then
      WarnFmt(KMN_W_UninstallError_UnableToDeleteKeyboardRegistrySetting,
        VarArrayOf([KeyboardFullName, Path]));
  finally
    Free;
  end;

  { Remove keyboard directory (if empty) -- it may be part of a package }

  if not (upPartOfPackage in FOptions) then
  begin
    if not RecursiveDelete(ExtractFileDir(FileName)) then   // I4173   // I4181
      WarnFmt(KMN_W_UninstallError_UnableToRemoveDirectory,
        VarArrayOf([KeyboardFullName, SysErrorMessage(GetLastError)]));
  end;

  { No start menu entries to remove; No other files to remove; No packages to remove }

  Context.Control.AutoApplyKeyman;
end;

end.
