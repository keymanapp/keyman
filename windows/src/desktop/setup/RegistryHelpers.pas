(*
  Name:             RegistryHelpers
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Feb 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Feb 2011 - mcdurdin - I2749 - Audit all uses of TRegistry, TTntRegistry and replace with TShellUserRegistry where appropriate
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit RegistryHelpers;

interface

uses
  ShellUserRegistry,
  ErrorControlledRegistry;

function CreateHKCURegistry: TRegistryErrorControlled;  // I2890
function CreateHKLMRegistry: TRegistryErrorControlled;  // I2890

implementation

uses
  Windows,
  SysUtils,
  RunTools;

var
  ShellUserRegistryFailed: Boolean = False;

function CreateHKCURegistry: TRegistryErrorControlled;  // I2890
begin
  if ShellUserRegistryFailed then
    Result := TRegistryErrorControlled.Create  // I2890
  else
  begin
    try
      Result := TShellUserRegistry.Create;
    except
      on E:EOSError do
      begin
        ShellUserRegistryFailed := True;
        GetRunTools.LogError('Could not access the shell user registry; will be accessing Administrator user registry for remainder of setup', False);
        Result := TRegistryErrorControlled.Create;  // I2890
      end;
    end;
  end;
end;

function CreateHKLMRegistry: TRegistryErrorControlled;  // I2890
begin
  Result := TRegistryErrorControlled.Create;  // I2890
  Result.RootKey := HKEY_LOCAL_MACHINE;
end;

end.
