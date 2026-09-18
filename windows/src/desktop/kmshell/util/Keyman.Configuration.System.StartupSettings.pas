(**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Ross Cruickshank on 2026-09-18
 *
 *
 * This unit assists in determining whether Keyman is enabled to start with Windows in the Windows startup settings.
 *)
unit Keyman.Configuration.System.StartupSettings;

interface

type
  TWindowsStartupSettings = class
    class function IsWindowsStartupDisabled: Boolean; static;
  end;

implementation

uses
  RegistryKeys,
  System.Win.Registry,
  Windows,
  SysUtils;

const
  StartupEnabled: Byte = $02;
  StartupDisabled: Byte = $03;



class function TWindowsStartupSettings.IsWindowsStartupDisabled: Boolean;
var
  reg: TRegistry;
  data: array[0..11] of Byte;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if not reg.OpenKeyReadOnly('\' + SRegKey_StartupApproved_Run) then
        Exit;
      if not reg.ValueExists(SRegValue_WindowsRun_Keyman) then
        Exit;
      if reg.GetDataSize(SRegValue_WindowsRun_Keyman) <> SizeOf(data) then
        Exit;
      reg.ReadBinaryData(SRegValue_WindowsRun_Keyman, data, SizeOf(data));
      Result := data[0] = StartUpDisabled;
    except
      on E: ERegistryException do
        Result := False;
    end;
  finally
    reg.Free;
  end;
end;

end.
