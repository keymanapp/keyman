unit Keyman.Configuration.System.AllApplicationPackagePermissions;

interface

uses
  System.SysUtils;

type
  EAppPackagePermissions = class(Exception)

  end;

function GrantPermissionToAllApplicationPackagesToKeymanProgramData: Boolean;

implementation

uses
  Winapi.AccCtrl,
  Winapi.ShlObj,
  Winapi.Windows,
  System.Classes,
  Keyman.System.Security,
  RegistryKeys,
  utilsystem;

procedure ReportFailure(const func: string; code: UINT);
begin
  raise EAppPackagePermissions.Create('Keyman for Windows failed to set permissions on ProgramData\Keymana in '+func+': '+SysErrorMessage(code));
end;

function GrantPermissionToAllApplicationPackagesToKeymanProgramData: Boolean;
var
  Path: string;
  hFile: THandle;
begin
  Result := False;

  Path := GetFolderPath(CSIDL_COMMON_APPDATA) + SFolderKeymanRoot;
  if not DirectoryExists(Path) then
    if not CreateDir(Path) then
      ReportFailure('CreateDir', GetLastError);

  hFile := CreateFile(
    PChar(Path),
    READ_CONTROL or WRITE_DAC,
    0,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS,
    0);
  if hFile = INVALID_HANDLE_VALUE then
    ReportFailure('CreateFile', GetLastError);

  try
    try
      GrantPermissionToAllApplicationPackages(hFile,
        GENERIC_READ or GENERIC_EXECUTE,
        SE_FILE_OBJECT);
    except
      on E:EOSError do
        ReportFailure('GrantPermission', E.ErrorCode);
    end;

    Result := True;

  finally
    if not CloseHandle(hFile) then
      ReportFailure('CloseHandle', GetLastError);
  end;
end;

end.
