(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Ross Cruickshank on 2026-09-12
 *
 *
 * This unit assists in setting the base keyboard configuration,
 * including compiling the installed keyboard layouts against
 * the selected base keyboard.
 *)
unit Keyman.Configuration.System.BaseKeyboard;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  keymanapi_TLB;

(**
 * Returns true if the keyboard files need to be compiled for the specified KLID.
 * @param  BaseKeyboardID  KLID of the base keyboard to compile.
 * @returns  True  If the keyboard files need to be compiled.
 *)
function BaseKeyboardNeedsMCompile(BaseKeyboardID: Integer): Boolean;

(**
 * Sets the base keyboard KLID for the current user and compiles the keyboard
 * layout files if necessary. In the case the compiled keyboard files are
 *not present, it will require elevation.
 * @param  WindowHandle  Window handle to own the elevation prompt.
 * @param  BaseKeyboardID  KLID of the base keyboard KLID to set.
 * @returns  True  when the base keyboard setting has been applied.
 *)
function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;

(**
 * Compiles the installed keyboard layouts for the specified KLID.
 * Must run elevated.
 *
 * @param  BaseKeyboardID  KLID of the base keyboard to compile.
 * @returns  True  when the compilation is successful.
 *)
function MCompileBaseKeyboard(BaseKeyboardID: Integer): Boolean;

implementation

uses
  kmint,
  utilkmshell,
  utilfiletypes;

function BaseKeyboardNeedsMCompile(BaseKeyboardID: Integer): Boolean;
var
  I: Integer;
  Keyboard: IKeymanKeyboardInstalled;
  KeyboardFileName: string;
  BaseKeyboardIDHex: string;
begin
  BaseKeyboardIDHex := IntToHex(BaseKeyboardID, 8);
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    Keyboard := kmcom.Keyboards.Items[I];
    KeyboardFileName := Keyboard.Filename;
    if FileExists(KeyboardFileName) and
      (not FileExists(BuildKeyboardFilenameWithBaseKeyboardID(KeyboardFileName, BaseKeyboardIDHex)) or
       not FileExists(BuildKeyboardFilenameWithBaseKeyboardIDAndDeadkey(KeyboardFileName, BaseKeyboardIDHex))) then
      Exit(True);
  end;
  Result := False;
end;

function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
begin
  Result := True;
  if BaseKeyboardNeedsMCompile(BaseKeyboardID) then
  begin
    if not kmcom.SystemInfo.IsAdministrator then
    begin
      Result := WaitForElevatedConfiguration(WindowHandle, '-mcompilekbds ' + IntToHex(BaseKeyboardID, 8)) = 0;
    end
    else
      Result := MCompileBaseKeyboard(BaseKeyboardID);
  end;
  if not Result then
      Exit;
  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  kmcom.Options.Apply;
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
