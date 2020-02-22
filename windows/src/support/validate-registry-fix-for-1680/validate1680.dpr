program validate1680;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  Keyman.System.Security in '..\..\global\delphi\general\Keyman.System.Security.pas',
  Keyman.Winapi.VersionHelpers in '..\..\global\delphi\winapi\Keyman.Winapi.VersionHelpers.pas';

const
  SRegKey_KeymanRoot_CU = 'Software\Keyman';

var
  Disposition: Integer;
  res: Integer;
  key: HKEY;

begin
  writeln(ExtractFileName(ParamStr(0))+' attempts to resolve the issue described in');
  writeln('https://github.com/keymanapp/keyman/issues/1680');
  writeln;
  try
    if not IsWindows8OrGreater then
    begin
      writeln('FAIL: IsWindows8OrGreater returned false');
      Exit;
    end;

    res := RegCreateKeyEx(HKEY_CURRENT_USER, PChar(SRegKey_KeymanRoot_CU), 0, nil,
      REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, key, @Disposition);
    if res <> ERROR_SUCCESS then
    begin
      writeln('FAIL: Failed to open key '+SRegKey_KeymanRoot_CU+' with error '+IntToStr(res));
      Exit;
    end;
    try
      writeln('INFO: Attempting to grant permission');
      GrantPermissionToAllApplicationPackages(key, KEY_READ);
      writeln('SUCCESS: Permission apparently granted successfully');
    finally
      res := RegCloseKey(key);
      if res <> ERROR_SUCCESS then
        writeln('FAIL: Failed to close key '+SRegKey_KeymanRoot_CU+' with error '+IntToStr(res));
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
