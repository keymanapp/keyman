(*
  Name:             utiluac
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jan 2009

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jan 2009 - mcdurdin - I1793 - Initial version - CanElevate function refactored
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    28 May 2014 - mcdurdin - I4222 - V9.0 - Deprecate osWin2000, osWinXP, osWin2003Server
*)
unit utiluac;  // I3306

interface

function CanElevate: Boolean;

implementation

uses
  Windows,
  GetOsVersion;

function CanElevate: Boolean;
type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
const
  TokenElevation: TTokenInformationClass = TTokenInformationClass(20);
var
//	hResult:  = E_FAIL; // assume an error occured
	hToken: THandle;
	te: TOKEN_ELEVATION;
	dwReturnLength: DWORD;
begin
  Result := False;   // I4222

	if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken) then Exit;
  try
    FillChar(te, sizeof(te), 0);
  	if not GetTokenInformation(hToken, TokenElevation, @te, sizeof(te), dwReturnLength) then Exit;
    Assert(dwReturnLength = sizeof(te));
    Result := te.TokenIsElevated = 0;
  finally
  	CloseHandle(hToken);
  end;
end;

end.
