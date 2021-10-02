unit GetOsVersion;

interface

// This is a stub version of GetOsVersion for unit tests
type
  TOS = (osLegacy, osVista, osWin7, osWin8, osWin10, osOther);

function GetOs: TOS;

implementation

function GetOs: TOS;
begin
  Result := osWin10;
end;

end.
