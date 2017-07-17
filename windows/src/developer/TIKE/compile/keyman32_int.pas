unit keyman32_int;

interface

uses Windows, SysUtils, Forms;

function Keyman_Initialise(Handle: HWND; FSingleApp: Boolean): Boolean;
function Keyman_Exit: Boolean;
function Keyman_ForceKeyboard(const s: string): Boolean;
function Keyman_StopForcingKeyboard: Boolean;

implementation

uses TikeUtils;

type TKeyman_ForceKeyboard = function (s: PChar): Boolean; stdcall;
type TKeyman_StopForcingKeyboard = function: Boolean; stdcall;
type TKeyman_Initialise = function(h: THandle; FSingleApp: LongBool): Boolean; stdcall;
type TKeyman_Exit = function: Boolean; stdcall;

var
  FInitKeyman: Boolean = False;
  FKeyman32Path: string = '';

function Keyman_Initialise(Handle: HWND; FSingleApp: Boolean): Boolean;
var
  hkeyman: THandle;
  FLoad: Boolean;
  ki: TKeyman_Initialise;
begin
  Result := False;
  FLoad := False;
  hkeyman := GetModuleHandle('keyman32.dll');
  if hkeyman = 0 then
  begin
    hkeyman := LoadLibrary(PChar(GetKeymanInstallPath+'keyman32.dll'));
    if hkeyman = 0 then Exit;
    FLoad := True;
  end;
  ki := TKeyman_Initialise(GetProcAddress(hkeyman, 'Keyman_Initialise'));
  if not Assigned(@ki) then Exit;
  if not ki(Handle, FSingleApp) then
  begin
    if FLoad then FreeLibrary(hkeyman);
    Exit;
  end;

  FInitKeyman := True;
  Result := True;
end;

function Keyman_Exit: Boolean;
var
  hkeyman: THandle;
  ke: TKeyman_Exit;
begin
  if not FInitKeyman then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  hkeyman := GetModuleHandle('keyman32.dll');
  if hkeyman = 0 then Exit;
  ke := TKeyman_Exit(GetProcAddress(hkeyman, 'Keyman_Exit'));
  if not Assigned(@ke) then Exit;
  if not ke then Exit;
  if FInitKeyman then FreeLibrary(hkeyman);
  FInitKeyman := False;
  Result := True;
end;

function Keyman_ForceKeyboard(const s: string): Boolean;
var
  hkeyman: THandle;
  fk: TKeyman_ForceKeyboard;
begin
  Result := False;
  hkeyman := GetModuleHandle('keyman32.dll');
  if hkeyman = 0 then
  begin
    if not Keyman_Initialise(Application.MainForm.Handle, True) then Exit;
    hkeyman := GetModuleHandle('keyman32.dll');
    if hkeyman = 0 then Exit;
  end;
  fk := TKeyman_ForceKeyboard(GetProcAddress(hkeyman, 'Keyman_ForceKeyboard'));
  if(Assigned(@fk)) then
    Result := fk(PChar(s));
end;

function Keyman_StopForcingKeyboard: Boolean;
var
  hkeyman: THandle;
  sfk: TKeyman_StopForcingKeyboard;
begin
  Result := False;
  hkeyman := GetModuleHandle('keyman32.dll');
  if hkeyman = 0 then Exit;
  sfk := TKeyman_StopForcingKeyboard(GetProcAddress(hkeyman, 'Keyman_StopForcingKeyboard'));
  if(Assigned(@sfk)) then
    Result := sfk;
  Keyman_Exit;
end;

initialization
finalization
  Keyman_Exit;
end.
