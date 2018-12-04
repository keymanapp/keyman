unit Keyman.System.Test.MetroPermissions;

interface

function EnginePostInstall: Boolean;

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

{
  Add permission for ALL APPLICATION PACKAGES to read %ProgramData%\Keyman folder
}
function EnginePostInstall: Boolean;
var
  hFile: THandle;
  Path: string;
begin
  Path := GetFolderPath(CSIDL_COMMON_APPDATA) + SFolderKeymanRoot;
  if not DirectoryExists(Path) then
    if not CreateDir(Path) then
      RaiseLastOSError;

  hFile := CreateFile(
    PChar(Path),
    READ_CONTROL or WRITE_DAC,
    0,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS,
    0);
  if hFile = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  try
    GrantPermissionToAllApplicationPackages(hFile,
      GENERIC_READ or GENERIC_EXECUTE,
      SE_FILE_OBJECT);

    Result := True;

  finally
    if not CloseHandle(hFile) then
      RaiseLastOSError;
  end;
end;

end.
