(*
This demonstration tries to shutdown your computer by enabling the
needed privilege and calling the WinAPI-function afterwards.
The shutdown will be cancelled after ten seconds.
*)

program SimpleShutdown;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclExceptions,
  JwsclToken;

var
  Token: TJwSecurityToken;

begin
  try
    Token:=TJwSecurityToken.CreateTokenByProcess(GetCurrentProcess,
      TOKEN_ADJUST_PRIVILEGES or TOKEN_READ or TOKEN_QUERY);
  except
    on E: EJwsclOpenProcessTokenException do
    begin
      Writeln(E.Message);
      Readln;
      Exit;
    end;
  end;
  try
    if Token.PrivilegeAvailable[SE_SHUTDOWN_NAME] then
    begin
      Writeln('SE_SHUTDOWN_NAME is available');
      Token.PrivilegeEnabled[SE_SHUTDOWN_NAME]:=True;
    end
    else
      Writeln('SE_SHUTDOWN_NAME is not available');
  finally
    FreeAndNil(Token);
  end;

  if not InitiateSystemShutdown(nil, 'The computer will be shut down in 60 seconds.', 60, False, False) then
    Writeln(SysErrorMessage(GetLastError))
  else
  begin
    Sleep(10000);
    AbortSystemShutdown(nil); //stops the shutdown
  end;

  Readln;
end.
