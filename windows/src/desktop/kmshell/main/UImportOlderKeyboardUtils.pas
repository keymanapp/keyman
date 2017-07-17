(*
  Name:             UImportOlderKeyboardUtils
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      28 Aug 2008

  Modified Date:    16 Jun 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Aug 2008 - mcdurdin - I1616 - Upgrade keyboards from 6.x
                    01 Jun 2009 - mcdurdin - I2001 - use current user not local machine when testing root keyboard path
                    11 Jan 2011 - mcdurdin - I2642 - Installer uninstalls KM7 keyboards before upgrade can happen
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    16 Jun 2014 - mcdurdin - I4185 - V9.0 - Upgrade V8.0 keyboards to 9.0
*)
unit UImportOlderKeyboardUtils;

interface

uses
  RegistryKeys;

type
  TImportOlderKeyboardUtils = class
  public
    class function GetShortKeyboardName(const FileName: string): string;
    class function GetShortPackageName(const FileName: string): string;
    class function GetPackageInstallPath: string;
    class function GetKeyboardInstallPath: string;
    class function GetRegistryKeyboardInstallKey(const FileName: string): string; static;   // I4185
    class function GetKeyboardIconFileName(const KeyboardFileName: string): string; static;   // I4185
  end;

implementation

uses
  Classes,
  ErrorControlledRegistry,
  ShlObj,
  SysUtils,
  KeymanPaths,
  utildir,
  utilsystem,
  Windows;

class function TImportOlderKeyboardUtils.GetShortKeyboardName(const FileName: string): string;
begin
  if (LowerCase(ExtractFileExt(FileName)) = '.kmx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kxx') or
      (LowerCase(ExtractFileExt(FileName)) = '.kmp')
    then Result := ChangeFileExt(ExtractFileName(FileName), '')
    else Result := FileName;
end;

class function TImportOlderKeyboardUtils.GetShortPackageName(const FileName: string): string;
begin
  Result := GetShortKeyboardName(FileName);
end;

class function TImportOlderKeyboardUtils.GetPackageInstallPath: string;
begin
  Result := TKeymanPaths.KeyboardsInstallPath(TKeymanPaths.S__Package);
end;

class function TImportOlderKeyboardUtils.GetKeyboardInstallPath: string;
begin
  Result := TKeymanPaths.KeyboardsInstallPath;
end;

class function TImportOlderKeyboardUtils.GetRegistryKeyboardInstallKey(const FileName: string): string;   // I4185
begin
  Result := SRegKey_InstalledKeyboards+'\'+GetShortKeyboardName(FileName);
end;

class function TImportOlderKeyboardUtils.GetKeyboardIconFileName(const KeyboardFileName: string): string;   // I4185
begin
  Result := ChangeFileExt(KeyboardFileName, '.kmx.ico');
end;

end.
