program Kontextmenuhandler;

uses
  SysUtils,
  JwaWindows;

var Pipe: THandle; str: String; Dummy: Cardinal;
begin
  if Paramcount<1 then
    exit;
  if not WaitNamedPipe('\\.\pipe\XPElevationPipe', 5000) then
  begin
    MessageBox(0, 'The XP Elevation service is currently not available.', 'XP Elevation', MB_OK);
    exit;
  end;
  Pipe:=CreateFile('\\.\pipe\XPElevationPipe', GENERIC_WRITE, 0, nil, OPEN_EXISTING, SECURITY_IMPERSONATION, 0);
  if Pipe=INVALID_HANDLE_VALUE then
  begin
    MessageBox(0, 'The communication pipe could not be created.', 'XP Elevation', MB_OK);
    Exit;
  end;
  try
    str:=Paramstr(1);
    WriteFile(Pipe, @str[1], Length(str), @Dummy, nil);
  finally
    CloseHandle(Pipe);
  end;
end.
