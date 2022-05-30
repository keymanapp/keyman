unit Keyman.System.DebugLogCommon;

interface

const
  // GUID that identifies the provider that you want
  // to enable to your session.
  DebugLogProviderGuid: TGUID = '{DA621615-E08B-4283-918E-D2502D3757AE}';

function ShouldDebug: Boolean;

implementation

uses
  System.Win.Registry,

  RegistryKeys;

var
  FShouldDebug: Boolean;

function ShouldDebug: Boolean;
begin
  Result := FShouldDebug;
end;

initialization
  with TRegistry.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and ValueExists(SRegValue_KeymanDebug)
      then FShouldDebug := ReadBool(SRegValue_KeymanDebug)
      else FShouldDebug := False;
  finally
    Free;
  end;
end.
