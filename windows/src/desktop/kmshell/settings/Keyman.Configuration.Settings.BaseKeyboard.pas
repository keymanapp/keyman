unit Keyman.Configuration.Settings.BaseKeyboard;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  keymanapi_TLB;

function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
function MCompileBaseKeyboard(const BaseKeyboardIDText: string): Boolean;
procedure CompileForBaseKeyboard(BaseKeyboardID: Integer);

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
      CompileForBaseKeyboard(BaseKeyboardID);
  end;

  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  kmcom.Options.Apply;
  Result := True;
end;

function MCompileBaseKeyboard(const BaseKeyboardIDText: string): Boolean;
var
  BaseKeyboardID: Integer;
begin
  Result := False;
  if not TryStrToInt('$' + BaseKeyboardIDText, BaseKeyboardID) or
    not kmcom.SystemInfo.IsAdministrator then
    Exit;
  CompileForBaseKeyboard(BaseKeyboardID);
  // TODO: sort out whether we need todo return a result
  Result := True;
end;

procedure CompileForBaseKeyboard(BaseKeyboardID: Integer);
var
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
begin
  for i := 0 to kmcom.Keyboards.Count - 1 do
  begin
    kbd := kmcom.Keyboards[i];
    (kbd as IKeymanKeyboardInstalled2).MCompileForBaseKeyboard(BaseKeyboardID);
  end;
end;

end.
