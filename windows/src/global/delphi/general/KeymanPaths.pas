unit KeymanPaths;

interface

uses
  System.SysUtils;

type
  EKeymanPath = class(Exception);

  TKeymanPaths = class
  private
    const S_CEF_DebugPath = 'Debug_CEFPath';
    const S_CEF_EnvVar = 'KEYMAN_CEF4DELPHI_ROOT';
    const S_CEF_SubFolder = 'cef\';
    const S_CEF_SubProcess = 'kmbrowserhost.exe';
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
    class function CEFPath: string; static; // Chromium Embedded Framework
    class function CEFDataPath(const mode: string): string; static;
    class function CEFSubprocessPath: string; static;
  end;

function GetFolderPath(csidl: Integer): string;

implementation

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShlObj,
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
    if OpenKeyReadOnly(SRegKey_KeymanDesktop_LM) and ValueExists(SRegValue_RootPath) then
      Result := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;

  Result := GetDebugPath('KeymanDesktop', Result);

  if Result = '' then
    raise EKeymanPath.Create('Unable to find the Keyman Desktop directory.  You should reinstall the product.');

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
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_RootPath) then
      Result := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;

  Result := GetDebugPath('KeymanEngine', Result);

  if Result = '' then
    raise EKeymanPath.Create('Unable to find the Keyman Engine directory.  You should reinstall the product.');

  Result := IncludeTrailingPathDelimiter(Result) + filename;
end;

class function TKeymanPaths.CEFPath: string;
begin
  Result := GetDebugPath(S_CEF_DebugPath, '');
  if Result = '' then
  begin
    Result := GetEnvironmentVariable(S_CEF_EnvVar);
    if Result = ''
      then Result := KeymanEngineInstallPath+S_CEF_SubFolder
      else Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

class function TKeymanPaths.CEFSubprocessPath: string;
begin
  // Normal install location - in Keyman Engine install folder
  Result := KeymanEngineInstallPath(S_CEF_SubProcess);
  if FileExists(Result) then Exit;

  // Same folder as executable
  Result := ExtractFilePath(ParamStr(0)) + S_CEF_SubProcess;
  if FileExists(Result) then Exit;

  // Source repo, bin folder
  Result := ExtractFilePath(ParamStr(0)) + '..\engine\' + S_CEF_SubProcess;
  if FileExists(Result) then Exit;

  // Source repo, source folder
  Result := ExtractFilePath(ParamStr(0)) + '..\..\engine\kmbrowserhost\win32\debug\' + S_CEF_SubProcess;
  if FileExists(Result) then Exit;

  Result := ExtractFilePath(ParamStr(0)) + '..\..\engine\kmbrowserhost\win32\release\' + S_CEF_SubProcess;
end;

class function TKeymanPaths.CEFDataPath(const mode: string): string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderCEFBrowserData + '\' + mode;
end;

class function TKeymanPaths.KeyboardsInstallDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(KeyboardsInstallPath);
end;

class function TKeymanPaths.KeyboardsInstallPath(const filename: string): string;
begin
  with TRegistry.Create do  // I2890
  try
    // TODO: We only want to use %CommonAppData%\Keyman now for all keyboards
    // So we should eliminate use of RootKeyboardAdminPath and S_FallbackKeyboardPath
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_RootKeyboardAdminPath) then
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
    raise EKeymanPath.Create('Unable to find the Keyboards directory.  You should reinstall the product.');

  Result := Result + Filename;
end;

end.
