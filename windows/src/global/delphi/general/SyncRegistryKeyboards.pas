unit SyncRegistryKeyboards;

interface

//procedure SyncRegistry;

implementation

uses SysUtils, Classes, Windows, ErrorControlledRegistry, RegistryKeys, kmint, util;

{procedure SyncRegistry;
begin
  with TRegKeyboardList.Create do
  try
    Load;
    Save;
  finally
    Free;
  end;
end;}

end.

