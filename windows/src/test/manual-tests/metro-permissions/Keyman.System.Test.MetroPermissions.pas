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

// This function is essentially a duplicate of EnginePostInstall in engine/inst/insthelper.
// Copied here because there's a number of boilerplate changes required and it's not worth
// refactoring as this is really a manual test app just to make sure that the permission
// change applies. It may be useful as a fixup app in a situation where a user finds that
// programdata permissions are broken?
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
