(*
  Name:             utilrun
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      25 Jan 2011

  Modified Date:    25 Jan 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 Jan 2011 - mcdurdin - I2569 - Keyboard welcome from kmshell - run util
*)
unit utilrun;

interface

function Run(CmdLine: WideString): Boolean;

implementation

uses
  Windows;

function Run(CmdLine: WideString): Boolean;
var
  si: TStartupInfoW;
  pi: TProcessInformation;
begin
  Result := False;
  FillChar(si, sizeof(TStartupInfoW), 0);
  FillChar(pi, sizeof(TProcessInformation), 0);
  si.cb := sizeof(TStartupInfo);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOWNORMAL;
  // TODO: parse CmdLine and pass correct first parameter - I2934 test app crashes when NULL passed as first parameter, find out why?
  // TODO: Set UNICODE_ENVIRONMENT flag and pass the environment variables? - I2934
  if CreateProcessW(nil, PWideChar(CmdLine), nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
  begin
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    Result := True;
  end;
end;

end.
