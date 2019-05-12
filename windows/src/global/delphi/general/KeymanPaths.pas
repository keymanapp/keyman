unit KeymanPaths;

interface

uses
  System.SysUtils;

type
  EKeymanPath = class(Exception);

  TKeymanPaths = class
  public
    const S_KMShell = 'kmshell.exe';
    const S_Xml_LocaleDef = 'xml\localedef.dtd';
    const S_TSysInfoExe = 'tsysinfo.exe';
    const S_KeymanExe = 'keyman.exe';
    const S_CfgIcon = 'cfgicon.ico';
    const S_AppIcon = 'appicon.ico';
    const S_KeymanMenuTitleImage = 'menutitle.png';
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
  System.Classes,
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

type
  TRegSzHelper = class helper for TRegistry
    function ReadMultiSz(const name: string; strings: TStrings): Boolean;
  end;

function TRegSzHelper.ReadMultiSz(const name: string; Strings: TStrings): boolean;
var
  iSizeInWChar, iSizeInByte: integer;
  Buffer: array of WChar;
  p, EndBuffer: PWChar;
begin
  iSizeInByte := GetDataSize(name);
  iSizeInWChar := iSizeInByte div SizeOf(WChar);
  if iSizeInByte > 0 then
  begin
    SetLength(Buffer, iSizeInWChar);
    iSizeInWChar := ReadBinaryData(name, Buffer[0], iSizeInByte) div sizeof(WChar);
    p := @Buffer[0];
    EndBuffer := p;
    Inc(EndBuffer, iSizeInWChar);
    while p < EndBuffer do
    begin
      Strings.Append(p);
      p := StrScan(p, #0);
      Inc(p);
    end;
    Result := True;
  end
  else
    Result := False;
end;

class function TKeymanPaths.KeymanDesktopInstallPath(const filename: string = ''): string;
var
  s: TStrings;
begin
  //
  // If the current executable is kmshell.exe, then return the folder it is in (unless
  // a debug path overrides it).
  //
  // Otherwise, Keyman Desktop takes precedence if installed. If it is not installed,
  // check the installed OEM product registry and the first installed OEM product is
  // used. Currently, I assess that the likelihood of two OEM products being installed
  // without Keyman being installed is low, and we can live with the first one taking
  // precedence.
  //
  // Finally, if no OEM product can be found, then abort.
  //

  Result := '';

  if SameText(ExtractFileName(ParamStr(0)), TKeymanPaths.S_KMShell) then
  begin
    // Just for kmshell.exe, use the current folder
    Result := ExtractFilePath(ParamStr(0));
  end;

  if Result = '' then
  begin
    // If Keyman Desktop is installed, then use its install path.
    with TRegistry.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_LM) and ValueExists(SRegValue_RootPath) then
        Result := ReadString(SRegValue_RootPath);
    finally
      Free;
    end;
  end;

  if Result = '' then
  begin
    // Look in the registry for any installed OEM product; this is a multistring,
    // but we only want the first value
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_Engine_OEMProductPath) then
      begin
        s := TStringList.Create;
        try
          ReadMultiSz(SRegValue_Engine_OEMProductPath, s);
          if s.Count > 0 then
            Result := s[0];
        finally
          s.Free;
        end;
      end;
    finally
      Free;
    end;
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
