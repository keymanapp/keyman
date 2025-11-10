unit utilwow64;

interface

uses
  Winapi.Windows;

{$IFNDEF IMAGE_FILE_MACHINE_ARM64}
const
  IMAGE_FILE_MACHINE_ARM64 = $AA64;
{$ENDIF}

function IsWow64: Boolean;
function IsNativeMachineArm64: Boolean;

implementation

uses
  System.SysUtils;

type
  TIsWow64Process2 = function(hProcess: THandle; out pProcessMachine: USHORT; out pNativeMachine: USHORT): BOOL; stdcall;

function IsWow64: Boolean;
var
  FIsWow64: BOOL;
begin
  if not IsWow64Process(GetCurrentProcess, FIsWow64) then
    RaiseLastOSError;
  Result := FIsWow64; // not on Wow64
end;

(**
  Determines whether the current system's native architecture is ARM64.
  @returns:  True  if the native machine architecture is ARM64, False otherwise
*)
function IsNativeMachineArm64: Boolean;
var
  processMachine: USHORT;
  nativeMachine: USHORT;
  IsWow64Process2 : TIsWow64Process2;
begin
  Result := False;
  IsWow64Process2 := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process2');
  if Assigned(IsWow64Process2) then
  begin
   if IsWow64Process2(GetCurrentProcess, processMachine, nativeMachine) then
    begin
      Result := (nativeMachine = IMAGE_FILE_MACHINE_ARM64);
    end;
  end;
end;


end.
