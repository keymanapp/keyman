(*
  Name:             kpuninstalladdin
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
unit kpuninstalladdin;

interface

uses kpbase;

type
  TKPUninstallAddin = class(TKPBase)
  public
    procedure Execute(const FName: string; FAllUsers: Boolean);
  private
    procedure Cleanup(const FName: string; FAllUsers: Boolean);
  end;

implementation

{ Add-in uninstallation }

uses Windows, SysUtils, ErrorControlledRegistry, RegistryKeys, KeymanErrorCodes, Classes, Variants;

type
  TKeymanAddinFunc = function: BOOL; stdcall;

procedure TKPUninstallAddin.Execute(const FName: string; FAllUsers: Boolean);
var
  hMod: HMODULE;
  pAddinUnreg: TKeymanAddinFunc;

begin
  if not FileExists(FName) then
  begin
    Cleanup(FName, FAllUsers);
    ErrorFmt(KMN_E_AddinUninstall_FileNotFound, VarArrayOf([FName]));
  end;

  { Ask the add-in to uninstall itself }

  hMod := LoadLibrary(PChar(FName));
  if hMod <> 0 then
  begin
    @pAddinUnreg := GetProcAddress(hMod, 'KeymanAddinUnregister');
    if @pAddinUnreg <> nil then pAddinUnreg;
    FreeLibrary(hMod);
  end;

  Cleanup(FName, FAllUsers);
end;

procedure TKPUninstallAddin.Cleanup(const FName: string; FAllUsers: Boolean);
var
  str: TStringList;
  i: Integer;
  s: string;
begin
  { Cleanup after a damaged add-in }
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if FAllUsers
      then RootKey := HKEY_LOCAL_MACHINE
      else RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_KeymanAddins, True) then
    begin
      GetValueNames(str);
      for i := 0 to str.Count - 1 do
      begin
        s := ReadString(str[i]);
        if LowerCase(s) = LowerCase(FName) then DeleteValue(str[i]);
      end;
      DeleteKey(ChangeFileExt(ExtractFileName(FName), ''));
    end;
  finally
    str.Free;
    Free;
  end;
end;

end.
