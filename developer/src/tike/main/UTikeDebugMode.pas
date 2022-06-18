(*
  Name:             UTikeDebugMode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Nov 2007

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Nov 2007 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit UTikeDebugMode;

interface

function TikeDebugMode: Boolean;

implementation

uses
  ErrorControlledRegistry,
  RegistryKeys;

var
  FTikeDebugMode: Boolean;
  FTikeDebugModeInit: Boolean = False;

function TikeDebugMode: Boolean;
begin
  if not FTikeDebugModeInit then
    with TRegistryErrorControlled.Create do  // I2890
    try
      FTikeDebugMode := OpenKeyReadOnly(SRegKey_KeymanDebug_CU) and ValueExists(SRegValue_Debug_TikeDebugMode) and ReadBool(SRegValue_Debug_TikeDebugMode);
      FTikeDebugModeInit := True;
    finally
      Free;
    end;
  Result := FTikeDebugMode;
end;

end.
