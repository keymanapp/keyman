(*
This demonstration shows how to create a new logon user by asking the
user for credentials and calling TJwSecurityToken.CreateLogonUser.
This program will then impersonate the logon user and try to shutdown
the computer the same way as the SimpleShutdown-example.

remarks:
  The demonstration will fail if you try to logon as an administrator
  with empty password.
*)

program LogonAndShutdown;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclExceptions,
  JwsclCredentials,
  JwsclToken,
  JwsclStrings;

var
  Token: TJwSecurityToken;
  UserName, DomainName, Password: TJwString;
  CredentialsPrompt: TJwCredentialsPrompt;

begin
  CredentialsPrompt:=TJwCredentialsPrompt.Create;
  try
    if CredentialsPrompt.ShowModal(False) then
    begin
      TJwCredentialsTools.ParseUserName(CredentialsPrompt.UserName,
        UserName, DomainName);
      Password := CredentialsPrompt.Password;
    end
    else
      Abort;
  finally
    FreeAndNil(CredentialsPrompt);
  end;

  try
    Token := TJwSecurityToken.CreateLogonUser(UserName, DomainName, Password, LOGON32_LOGON_INTERACTIVE, 0);
  except
    on E: EJwsclSecurityException do
    begin
      Writeln('Logon failed');
      writeln(SysErrorMessage(E.LastError));
      Readln;
      Exit;
    end;
  end;
  try
    if Token.PrivilegeAvailable[SE_SHUTDOWN_NAME] then
      Token.PrivilegeEnabled[SE_SHUTDOWN_NAME]:=true
    else
      writeln('SE_SHUTDOWN_NAME is not available');
    Token.ImpersonateLoggedOnUser;  //current thread now acts as that user
    if not InitiateSystemShutdown(nil, 'The computer will be shut down in 60 seconds.', 60, False, False) then
      Writeln(SysErrorMessage(GetLastError))
    else
    begin
      Sleep(10000);
      AbortSystemShutdown(nil);  //stops the shutdown
    end;
    TJwSecurityToken.RevertToSelf; //old user again
  finally
    FreeAndNil(Token);
  end;

  Readln;
end.
