(*
  Name:             kpinstalladdin
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 May 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit kpinstalladdin;

interface

uses kpbase;

type
  TKPInstallAddin = class(TKPBase)
    procedure Execute(const FileName, PackageName: string);
  end;

implementation

uses Windows, SysUtils, ErrorControlledRegistry, RegistryKeys, KeymanErrorCodes, Variants;

type
  TKeymanAddinFunc = function: BOOL; stdcall;

procedure TKPInstallAddin.Execute(const FileName, PackageName: string);
var
  hMod: HMODULE;
  pAddinReg: TKeymanAddinFunc;
  FResult: Boolean;
  s: string;
begin
  if not FileExists(FileName) then
    ErrorFmt(KMN_E_AddinInstall_FileNotFound, VarArrayOf([FileName]));

  hMod := LoadLibrary(PChar(FileName));
  if hMod = 0 then
    ErrorFmt(KMN_E_AddinInstall_AddinCouldNotBeLoaded,
      VarArrayOf([FileName, Integer(GetLastError), SysErrorMessage(GetLastError)]));

  @pAddinReg := GetProcAddress(hMod, 'KeymanAddinRegister');
  if @pAddinReg = nil then
  begin
    FreeLibrary(hMod);
    ErrorFmt(KMN_E_AddinInstall_NotValidKeymanAddin, VarArrayOf([FileName, Integer(GetLastError), SysErrorMessage(GetLastError)]));
  end;

  FResult := pAddinReg;

  FreeLibrary(hMod);

  if not FResult then
    ErrorFmt(KMN_E_AddinInstall_AddinFailedToRegister, VarArrayOf([FileName]));

  if PackageName <> '' then
  begin
    s := ChangeFileExt(ExtractFileName(FileName), '');
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(SRegKey_KeymanAddins + '\' + s, False) then
        WriteString(SRegValue_AddinPackage, PackageName)
      else
      begin
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(SRegKey_KeymanAddins + '\' + s, False)
          then WriteString(SRegValue_AddinPackage, PackageName)
          else WarnFmt(KMN_W_AddinInstall_AddinPackageFailedToRegister, VarArrayOf([FileName, PackageName]));
      end;
    finally
      Free;
    end;
  end;
end;

end.
