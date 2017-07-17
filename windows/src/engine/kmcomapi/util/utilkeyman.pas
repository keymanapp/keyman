(*
  Name:             utilkeyman
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    13 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Jun 2009 - mcdurdin - I2001 - use current user not local machine when testing root keyboard path
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    28 Nov 2012 - mcdurdin - I3599 - V9.0 - Refactor GetKeyboardIconFileName
                    01 Jan 2013 - mcdurdin - I3619 - V9.0 - Substitute KBDUS or default HKL for kmtip per mnemonic status
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    13 Mar 2015 - mcdurdin - I4615 - CrashID:kmshell.exe_9.0.481.0_2C6795CE_EOleException
*)
unit utilkeyman;

interface

uses
  Winapi.Windows,
  System.SysUtils;

function GetShortKeyboardName(const FileName: string): string;
function GetShortPackageName(const FileName: string): string;

function GetPackageInstallPath(const FileName: string): string;
function GetKeyboardInstallPath(const FileName: string; FPackage: Boolean = False): string;

function GetRegistryKeyboardInstallKey(const FileName: string): string;
function GetRegistryPackageInstallKey(const FileName: string): string;
function GetRegistryKeyboardActiveKey(const FileName: string): string;

function GetKeyboardFullName(InstByAdmin: Boolean; const KeyboardName: string): string;

function KeyboardInstalled(const KeyboardName: string; var FIsAdmin: Boolean): Boolean;
function KeyboardIsPartOfPackage(KeyboardName: string; out PackageName: string): Boolean;

function PackageInstalled(const PackageName: string; var FIsAdmin: Boolean): Boolean;

function GetKeyboardIconFileName(const KeyboardFileName: string): string;   // I3599

function GetKeymanInstallPath: string;

function GetDefaultHKL: HKL;   // I3581   // I3619   // I3619
function GetDefaultKeyboardID: Cardinal;   // I4169

var
  FInstallingKeyman: Boolean = False;

type
  EKeymanNotInstalled = class(Exception);

implementation

uses
  Winapi.ShlObj,

  Glossary,
  isadmin,
  ErrorControlledRegistry,
  RegistryKeys,
  KeymanPaths,
  kmxfile,
  utilsystem;

function GetShortKeyboardName(const FileName: string): string;
begin
  if (LowerCase(ExtractFileExt(FileName)) = '.kmx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kxx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kmp')
    then Result := ChangeFileExt(ExtractFileName(FileName), '')
    else Result := FileName;
end;

function GetShortPackageName(const FileName: string): string;
begin
  Result := GetShortKeyboardName(FileName);
end;

function GetPackageInstallPath(const FileName: string): string;
begin
  Result := GetKeyboardInstallPath(FileName, True);
end;

function GetKeyboardInstallPath(const FileName: string; FPackage: Boolean): string;
var
  s: string;
begin
  s := GetShortKeyboardName(FileName);
  if FPackage then s := TKeymanPaths.S__Package+s;
  Result := TKeymanPaths.KeyboardsInstallPath(s);
end;

function GetRegistryKeyboardInstallKey(const FileName: string): string;
begin
  Result := SRegKey_InstalledKeyboards+'\'+GetShortKeyboardName(FileName);
end;

function GetRegistryPackageInstallKey(const FileName: string): string;
begin
  Result := SRegKey_InstalledPackages+'\'+GetShortKeyboardName(FileName);
end;

function GetRegistryKeyboardActiveKey(const FileName: string): string;
begin
  Result := SRegKey_ActiveKeyboards+'\'+GetShortKeyboardName(FileName);
end;

function GetKeyboardFullName(InstByAdmin: Boolean; const KeyboardName: string): string;
var
  s: string;
  ki: TKeyboardInfo;
begin
  Result := KeyboardName;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if InstByAdmin // as determined by where the keyboard was installed
      then RootKey := HKEY_LOCAL_MACHINE
      else RootKey := HKEY_CURRENT_USER;

    if OpenKeyReadOnly(SRegKey_InstalledKeyboards+'\'+KeyboardName) and ValueExists(SRegValue_KeymanFile) then
    begin
      s := ReadString(SRegValue_KeymanFile);
      try
        GetKeyboardInfo(s, False, ki);
        Result := ki.KeyboardName;
      except
        ;
      end;
    end;
  finally
    Free;
  end;
end;


function KeyboardInstalled(const KeyboardName: string; var FIsAdmin: Boolean): Boolean;
var
  s: string;
begin
  if KeyboardName = '' then   // I4615
    Exit(False);

  s := GetShortKeyboardName(KeyboardName);
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(GetRegistryKeyboardInstallKey(s)) then
    begin
      FIsAdmin := True;
      Result := True;
      Exit;
    end;
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(GetRegistryKeyboardInstallKey(s)) then
    begin
      FIsAdmin := False;
      Result := True;
      Exit;
    end;
    Result := False;
    FIsAdmin := False;
  finally
    Free;
  end;
end;


function KeyboardIsPartOfPackage(KeyboardName: string; out PackageName: string): Boolean;
var
  FIsAdmin: Boolean;
begin
  Result := False;

  KeyboardName := GetShortKeyboardName(KeyboardName);
  if not KeyboardInstalled(KeyboardName, FIsAdmin) then Exit;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if FIsAdmin
      then RootKey := HKEY_LOCAL_MACHINE
      else RootKey := HKEY_CURRENT_USER;

    if not OpenKeyReadOnly(GetRegistryKeyboardInstallKey(KeyboardName)) or not ValueExists(SRegValue_PackageName) then
      Exit;

    PackageName := ReadString(SRegValue_PackageName);
    Result := True;
  finally
    Free;
  end;
end;

function PackageInstalled(const PackageName: string; var FIsAdmin: Boolean): Boolean;
var
  s: string;
begin
  s := GetShortPackageName(PackageName);
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(GetRegistryPackageInstallKey(s)) then
    begin
      FIsAdmin := True;
      Result := True;
      Exit;
    end;
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(GetRegistryPackageInstallKey(s)) then
    begin
      FIsAdmin := False;
      Result := True;
      Exit;
    end;
    Result := False;
    FIsAdmin := False;
  finally
    Free;
  end;
end;

function GetKeymanInstallPath: string;
var
  RootPath: string;
begin
  RootPath := ExtractFilePath(ParamStr(0));
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine) then
      if ValueExists(SRegValue_RootPath) then
        RootPath := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;
  Result := IncludeTrailingPathDelimiter(RootPath);

  if not FileExists(Result + 'keyman32.dll') then
    raise EKeymanNotInstalled.Create( 'The executable keyman32.dll could not '+
      'be found.  You should reinstall.');
end;

function GetKeyboardIconFileName(const KeyboardFileName: string): string;   // I3599
begin
  Result := ChangeFileExt(KeyboardFileName, '.kmx.ico');   // I3581
end;

function GetDefaultHKL: HKL;   // I3581   // I3619   // I3619
begin
  if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
    Result := 0;
end;

function GetDefaultKeyboardID: Cardinal;   // I4169
const
  BaseKeyboardID_USEnglish: Integer = $00000409;
begin
  if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
    Result := 0;

  if Result <> 0 then
    Result := HKLToKeyboardID(Result);

  if Result = 0 then
    Result := BaseKeyboardID_USEnglish;
end;

end.
