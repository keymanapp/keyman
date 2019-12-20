(*
  Name:             DebugPaths
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    4 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - Unicode strings
                    20 Jan 2009 - mcdurdin - Fix bug loading files with a blank debug path
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit DebugPaths;  // I3306

interface

function GetDebugPath(const RegValue, Default: WideString; TerminateInSlash: Boolean = True): WideString;
function Reg_GetDebugFlag(const RegValue: string; Default: Boolean = False): Boolean;

implementation

uses
  ErrorControlledRegistry, RegistryKeys, SysUtils;

function GetDebugPath(const RegValue, Default: WideString; TerminateInSlash: Boolean): WideString;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanDebug_CU) and ValueExists(RegValue)
      then Result := ReadString(RegValue)
      else Result := Default;
  finally
    Free;
  end;
  if TerminateInSlash and (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function Reg_GetDebugFlag(const RegValue: string; Default: Boolean): Boolean;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngineDebug_CU) and ValueExists(RegValue)
      then Result := ReadInteger(RegValue) <> 0
      else Result := Default;
  finally
    Free;
  end;
end;

end.
