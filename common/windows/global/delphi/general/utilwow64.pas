unit utilwow64;

interface

uses
  Winapi.Windows;

function IsWow64: Boolean;

implementation

uses
  System.SysUtils;

function IsWow64: Boolean;
var
  FIsWow64: BOOL;
begin
  if not IsWow64Process(GetCurrentProcess, FIsWow64) then
    RaiseLastOSError;
  Result := FIsWow64; // not on Wow64
end;

end.
