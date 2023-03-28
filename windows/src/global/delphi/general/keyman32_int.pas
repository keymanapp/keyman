(*
  Name:             keyman32_int
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    17 Aug 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add GetKeymanInstallPath function
                    14 Sep 2006 - mcdurdin - Retrieve Debug_Keyman32 path if assigned
                    18 Mar 2011 - mcdurdin - I2825 - Debug_Keyman32 override not working correctly
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit keyman32_int;

interface

uses Windows, SysUtils, Forms, ErrorControlledRegistry, RegistryKeys; //, util;

function Keyman_Initialise(Handle: HWND; FSingleApp: Boolean): Boolean;
function Keyman_Exit: Boolean;
function Keyman_ForceKeyboard(const s: string): Boolean;
function Keyman_StopForcingKeyboard: Boolean;

implementation

uses DebugPaths, Dialogs, KLog;

type TKeyman_ForceKeyboard = function (s: PAnsiChar): Boolean; stdcall;  // I3310
type TKeyman_StopForcingKeyboard = function: Boolean; stdcall;
type TKeyman_Initialise = function(h: THandle; FSingleApp: LongBool): Boolean; stdcall;
type TKeyman_Exit = function: Boolean; stdcall;

var
  FInitKeyman: Boolean = False;
  FKeyman32Path: string = '';

function GetKeymanInstallPath: string;
var
  RootPath: string;
begin
  RootPath := '';
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_RootPath) then
        RootPath := ReadString(SRegValue_RootPath);
  finally
    Free;
  end;

  RootPath := GetDebugPath('Debug_Keyman32Path', RootPath);  // I2825

  if RootPath = '' then
  begin
    RootPath := ExtractFilePath(ParamStr(0));
  end;

  Result := IncludeTrailingPathDelimiter(RootPath);

  if not FileExists(Result + 'keyman32-ver17.0.48-alpha-local.dll') then
    raise Exception.Create( 'The executable keyman32-ver17.0.48-alpha-local.dll could not '+
      'be found.  You should reinstall.');
end;

function Keyman_Initialise(Handle: HWND; FSingleApp: Boolean): Boolean;
var
  hkeyman: THandle;
  FLoad: Boolean;
  ki: TKeyman_Initialise;
  s: string;
begin
  Result := False;
  FLoad := False;
  hkeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local');
  if hkeyman = 0 then
  begin
    s := GetKeymanInstallPath;
    hkeyman := LoadLibrary(PChar(s+'keyman32-ver17.0.48-alpha-local'));
    if hkeyman = 0 then
    begin
      KL.LogError('Keyman_Initialise: Unable to load '+s+'keyman32-ver17.0.48-alpha-local: '+SysErrorMessage(GetLastError));
      Exit;
    end;
    KL.Log('Keyman_Initialise: Loaded keyman32-ver17.0.48-alpha-local');
    FLoad := True;
  end
  else
    KL.Log('Keyman_Initialise: Found keyman32.dll already loaded');
  ki := TKeyman_Initialise(GetProcAddress(hkeyman, 'Keyman_Initialise'));
  if not Assigned(@ki) then
  begin
    KL.LogError('Keyman_Initialise: Unable to find Keyman_Initialise in '+s+'keyman32.dll: '+SysErrorMessage(GetLastError));
    if FLoad then FreeLibrary(hkeyman);
    Exit;
  end;
  if not ki(Handle, FSingleApp) then
  begin
    KL.LogError('Keyman_Initialise: Call to Keyman_Initialise in '+s+'keyman32.dll failed: '+SysErrorMessage(GetLastError));
    if FLoad then FreeLibrary(hkeyman);
    Exit;
  end;

  KL.Log('Keyman_Initialise: Loaded successfully');
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
  hkeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local');
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
  hkeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local');
  if hkeyman = 0 then
  begin
    KL.Log('Keyman_ForceKeyboard: Attempting to load keyman32.dll');
    if not Keyman_Initialise(Application.MainForm.Handle, True) then
    begin
      KL.LogError('Keyman_ForceKeyboard: Unable to load keyman32.dll');
      Exit;
    end;
    hkeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local.dll');
    if hkeyman = 0 then Exit;
  end;
  fk := TKeyman_ForceKeyboard(GetProcAddress(hkeyman, 'Keyman_ForceKeyboard'));
  if(Assigned(@fk)) then
  begin
    Result := fk(PAnsiChar(AnsiString(s)));   // todo: k9: unicode  // I3310
    if not Result then KL.LogError('Keyman_ForceKeyboard in keyman32.dll failed: '+s)
    else KL.Log('Keyman_ForceKeyboard: success');
  end
  else
    KL.LogError('Keyman_ForceKeyboard: failed to find Keyman_ForceKeyboard in keyman32.dll');
end;

function Keyman_StopForcingKeyboard: Boolean;
var
  hkeyman: THandle;
  sfk: TKeyman_StopForcingKeyboard;
begin
  Result := False;
  hkeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local.dll');
  if hkeyman = 0 then Exit;
  sfk := TKeyman_StopForcingKeyboard(GetProcAddress(hkeyman, 'Keyman_StopForcingKeyboard'));
  if(Assigned(@sfk)) then
    Result := sfk;
end;

initialization
finalization
  Keyman_Exit;
end.

