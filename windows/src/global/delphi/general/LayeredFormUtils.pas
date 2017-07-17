(*
  Name:             LayeredFormUtils
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      10 Dec 2010

  Modified Date:    28 Jun 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 Dec 2010 - mcdurdin - I2555 - Initial version, transparency fails
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    28 Jun 2011 - mcdurdin - I2953 - Some computers fail to display translucent windows correctly
*)
unit LayeredFormUtils;

interface

type
  TLayeredFormUtils = class
    class function ShouldUseAlpha: Boolean;
  end;

implementation

uses
  Windows,
  ErrorControlledRegistry, 
  RegistryKeys;

{ TLayeredFormUtils }

(* I2953
const
  SM_REMOTESESSION = $1000;

type
  TWTSGetActiveConsoleSessionId = function: DWord; stdcall;

var
  WTSGetActiveConsoleSessionId: TWTSGetActiveConsoleSessionId = nil;

function ProcessIdToSessionId(dwProcessId: DWord; var pSessionId: DWord): BOOL; stdcall; external kernel32;
*)

var
  FTransparency: Boolean = False;
  FTransparencyInit: Boolean = False;

class function TLayeredFormUtils.ShouldUseAlpha: Boolean;
var
  FValue: Integer;
  (* I2953
  FConsoleSessionID, FProcessSessionID: DWord;
  *)
begin
  if FTransparencyInit then
    Result := FTransparency
  else
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanEngine) and ValueExists(SRegValue_Transparency) then
      begin
        FValue := ReadInteger(SRegValue_Transparency);
        if (FValue < 0) or (FValue > 2) then FValue := 0;
      end
      else
        FValue := 0;
    finally
      Free;
    end;

    case FValue of
      0: Result := False; // I2953 - turn off translucency by default
  (* I2953
        begin
          // Disable if we are in a remote session, console remote or otherwise
          if GetSystemMetrics(SM_REMOTESESSION) <> 0 then Result := False
          else
          begin
            Result := True;
            if Assigned(WTSGetActiveConsoleSessionId) then
            begin
              FConsoleSessionID := WTSGetActiveConsoleSessionId;
              if ProcessIdToSessionId(GetCurrentProcessId, FProcessSessionID) then
              begin
                Result := FConsoleSessionID = FProcessSessionID;
              end;
            end;
          end;
        end;
  *)
      1: Result := True;
      2: Result := False;
    else
      Result := False;
    end;
    FTransparency := Result;
    FTransparencyInit := True;
  end;
end;

(* I2953
initialization
  WTSGetActiveConsoleSessionId := GetProcAddress(GetModuleHandle(kernel32), 'WTSGetActiveConsoleSessionId');
*)
end.
