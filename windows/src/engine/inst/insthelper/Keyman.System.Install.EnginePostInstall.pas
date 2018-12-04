unit Keyman.System.Install.EnginePostInstall;

interface

uses JwaWinType, JwaMsi, JwaMsiQuery, JwaWinError, JwaWinReg, JwaWinUser, JwaWinBase;

function EnginePostInstall(hInstall: MSIHANDLE): UINT; stdcall;   // I4302

implementation

uses
  Winapi.AccCtrl,
  Winapi.ShlObj,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Keyman.System.Security,
  RegistryKeys,
  utilsystem;

function ReportFailure(hInstall: MSIHANDLE; const func: string; code: UINT): UINT;
begin
  MsiSetProperty(hInstall, 'EnginePostInstall_Error',
    PChar('Keyman Engine failed to set permissions on shared data in '+func+': '+SysErrorMessage(code)));
  Result := code;
end;

{
  Add permission for ALL APPLICATION PACKAGES to read %ProgramData%\Keyman folder
}
function EnginePostInstall(hInstall: MSIHANDLE): UINT;
var
  hFile: THandle;
  Path: string;
begin
  try
    Path := GetFolderPath(CSIDL_COMMON_APPDATA) + SFolderKeymanRoot;
    if not DirectoryExists(Path) then
      if not CreateDir(Path) then
        Exit(ReportFailure(hInstall, 'CreateDir', GetLastError));

    hFile := CreateFile(
      PChar(Path),
      READ_CONTROL or WRITE_DAC,
      0,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS,
      0);
    if hFile = INVALID_HANDLE_VALUE then
      Exit(ReportFailure(hInstall, 'CreateFile', GetLastError));

    try
      try
        GrantPermissionToAllApplicationPackages(hFile,
          GENERIC_READ or GENERIC_EXECUTE,
          SE_FILE_OBJECT);
      except
        on E:EOSError do
          Exit(ReportFailure(hInstall, 'GrantPermission', E.ErrorCode));
      end;

      Result := ERROR_SUCCESS;

    finally
      if not CloseHandle(hFile) then
        Result := ReportFailure(hInstall, 'CloseHandle', GetLastError);
    end;
  except
    on E:Exception do
    begin
      MsiSetProperty(hInstall, 'EnginePostInstall_Error', PChar('Keyman Engine failed to set permissions on shared data, '+
        E.ClassName+': '+E.Message));
      // We don't have an error value so we'll just return !success
      Exit(ERROR_INVALID_FUNCTION);
    end;
  end;
end;

end.
