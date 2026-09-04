unit Keyman.Configuration.System.BaseKeyboard;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  keymanapi_TLB;

function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
function MCompileBaseKeyboard(BaseKeyboardID: Integer): Boolean;

implementation

uses
  kmint,
  utilkmshell;

function BaseKeyboardNeedsMCompile(BaseKeyboardID: Integer): Boolean;
var
  I: Integer;
  Keyboard: IKeymanKeyboardInstalled;
  BaseFileName: string;
  BaseKeyboardIDHex: string;
begin
  BaseKeyboardIDHex := IntToHex(BaseKeyboardID, 8);
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    Keyboard := kmcom.Keyboards.Items[I];
    BaseFileName := Keyboard.Filename;
    if FileExists(BaseFileName) and
      (not FileExists(ChangeFileExt(BaseFileName, '') + '-' + BaseKeyboardIDHex + '.kmx') or
       not FileExists(ChangeFileExt(BaseFileName, '') + '-' + BaseKeyboardIDHex + '-d.kmx')) then
      Exit(True);
  end;
  Result := False;
end;

function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
begin
  Result := False;
  if BaseKeyboardNeedsMCompile(BaseKeyboardID) then
  begin
    if not kmcom.SystemInfo.IsAdministrator then
    begin
      WaitForElevatedConfiguration(WindowHandle, '-mcompilekbds ' + IntToHex(BaseKeyboardID, 8));
    end
    else
      MCompileBaseKeyboard(BaseKeyboardID);
  end;

  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  kmcom.Options.Apply;
  Result := True;
end;

function MCompileBaseKeyboard(BaseKeyboardID: Integer): Boolean;
var
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
begin
  Result := False;
  // can be called from command line so test for admin
  if not kmcom.SystemInfo.IsAdministrator then
    Exit;
  for i := 0 to kmcom.Keyboards.Count - 1 do
  begin
    kbd := kmcom.Keyboards[i];
    (kbd as IKeymanKeyboardInstalled2).MCompileForBaseKeyboard(BaseKeyboardID);
  end;
  Result := True;
end;

end.
