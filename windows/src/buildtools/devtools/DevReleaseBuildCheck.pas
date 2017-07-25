unit DevReleaseBuildCheck;

interface

type
  TReleaseBuildCheck = class
    class function Run: Boolean;
  end;

implementation

uses
  Registry,
  RegistryKeys;

{ TReleaseBuildCheck }

class function TReleaseBuildCheck.Run: Boolean;
begin
  with TRegistry.Create do
  try
    if KeyExists(SRegKey_KeymanDebug) then
    begin
      writeln('HKCU\'+SRegKey_KeymanDebug+' key exists.  Rename or delete before build.');
      Result := False;
    end
    else
      Result := True;
  finally
    Free;
  end;
end;

end.
