(**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Ross Cruickshank on 2026-09-18
 *
 * This unit assists in determining whether Keyman is enabled to start
 * with Windows in the Windows startup settings.
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

(* This registry key is not documented by Microsoft, others have determined:
The value associated with it is 12 bytes in length the first being the
enabled/disabled state. The rest of the byte is the timestamp
of when it was disabled. Drawing from our investigation of the registry
and a broader search of stackoverflow, superuser and other sources on
the web, the following table can be derived for the first byte of the value:
See issue #15785 for more details and links.

| First byte | Reported behaviour             | Source quality                        |
| ---------- | ------------------------------ | --------------------------------------|
| `00`       | enabled, user can disable      | Our test on two Win 11 Home Machines  |
| `01`       | Disabled, user can enable      | Our test + Super User                 |
| `02`       | Enabled, user can disable      | Multiple sources                      |
| `03`       | Disabled, user can enable      | Multiple sources                      |
| `06`       | Enableduser can disable        | Stack Overflow,  not seen by us       |
| `07`       | Disabled, user can enable      | Stack Overflow, not seen by us        |
| `08`       | Enabled, user cannot disable   | Super User, not seen by us            |
| `09`       | Disabled, user cannot enable   | Super User, not seen by us            |

Maybe it is just bit 0 of the first byte that is enabled disabled? There in no
point checking for $09 as the user cannot change the setting. We would need a
different message.

*)

const
  StartupDisabled: set of Byte = [$01, $03, $07];

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
      if reg.ReadBinaryData(SRegValue_WindowsRun_Keyman, data, SizeOf(data)) <> sizeof(data) then
        Exit;
      Result := data[0] in StartupDisabled;
    except
      on E: ERegistryException do
        Result := False;
    end;
  finally
    reg.Free;
  end;
end;

end.
