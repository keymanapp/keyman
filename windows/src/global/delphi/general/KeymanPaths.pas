unit KeymanPaths;

interface

type
  TKeymanPaths = class
  public
    const S_KMShell = 'kmshell.exe';
    const S_Xml_LocaleDef = 'xml\localedef.dtd';
    const S_TSysInfoExe = 'tsysinfo.exe';
    const S_KeymanExe = 'keyman.exe';
    const S_CfgIcon = 'cfgicon.ico';
    const S_AppIcon = 'appicon.ico';
    const S_FallbackKeyboardPath = 'Keyboards\';
    const S__Package = '_Package\';
    const S_MCompileExe = 'mcompile.exe';
    class function KeymanDesktopInstallPath(const filename: string = ''): string; static;
    class function KeymanEngineInstallPath(const filename: string = ''): string; static;
    class function KeymanDesktopInstallDir: string; static;
    class function KeymanEngineInstallDir: string; static;
    class function KeyboardsInstallPath(const filename: string = ''): string; static;
    class function KeyboardsInstallDir: string; static;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShlObj,
  System.SysUtils,
  System.Win.Registry,

  DebugPaths,
  RegistryKeys;


// Duplicated from utilsystem to avoid importing other units
function GetFolderPath(csidl: Integer): string;
var
  buf: array[0..260] of Char;
  idl: PItemIDList;
  mm: IMalloc;
begin
  Result := '';
  if SHGetMalloc(mm) = NOERROR then
  begin
    if SHGetSpecialFolderLocation(0, csidl, idl) = NOERROR then
    begin
      if SHGetPathFromIDList(idl, buf) then
      begin
        Result := Buf;
      end;
      mm.Free(idl);
    end;
    mm._Release;
  end;

  if (Result = '') and (csidl = CSIDL_PROGRAM_FILES) then
    with TRegistry.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then  // I2890
        RaiseLastOSError(LastError);
      Result := ReadString('ProgramFilesDir');
    finally
      Free;
    end;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then Result := Result + '\';
end;


class function TKeymanPaths.KeymanDesktopInstallDir: string;
begin
  Result := ExtractFileDir(KeymanDesktopInstallPath);
end;

class function TKeymanPaths.KeymanDesktopInstallPath(const filename: string = ''): string;
begin
  with TRegistry.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanDesktop) and ValueExists(SRegValue_RootPath) then
      Result := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;

  Result := GetDebugPath('KeymanDesktop', Result);

  if Result = '' then
    raise Exception.Create('Unable to find the Keyman Desktop directory.  You should reinstall the product.');

  Result := IncludeTrailingPathDelimiter(Result) + filename;
end;

class function TKeymanPaths.KeymanEngineInstallDir: string;
begin
  Result := ExtractFileDir(KeymanEngineInstallPath);
end;

class function TKeymanPaths.KeymanEngineInstallPath(const filename: string = ''): string;
begin
  with TRegistry.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine) and ValueExists(SRegValue_RootPath) then
      Result := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;

  Result := GetDebugPath('KeymanEngine', Result);

  if Result = '' then
    raise Exception.Create('Unable to find the Keyman Engine directory.  You should reinstall the product.');

  Result := IncludeTrailingPathDelimiter(Result) + filename;
end;

class function TKeymanPaths.KeyboardsInstallDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(KeyboardsInstallPath);
end;

class function TKeymanPaths.KeyboardsInstallPath(const filename: string): string;
begin
  with TRegistry.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine) and ValueExists(SRegValue_RootKeyboardAdminPath) then
    begin
      Result := ReadString(SRegValue_RootKeyboardAdminPath)
    end
    else
    begin
      Result := GetFolderPath(CSIDL_COMMON_APPDATA);
      if Result = '' then
        Result := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_FallbackKeyboardPath)
      else
        Result := Result + SFolderKeymanKeyboard;
    end;
  finally
    Free;
  end;

  Result := GetDebugPath('Keyboards', Result, True);
  if Result = '' then
    raise Exception.Create('Unable to find the Keyboards directory.  You should reinstall the product.');

  Result := Result + Filename;
end;

end.
