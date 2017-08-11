(*
This program shows how to start a program with a restricted token.
It will create a restricted token without the SE_SHUTDOWN_NAME-Privilege
an then start the SimpleShutdown-example with this token.
The SimpleShutdown-program will fail since it does not have the needed
privilege.

remarks:
  SimpleShutdown.exe must be in the working directory.
*)
program Forbid;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclToken;

const
  PathToShutdown='SimpleShutdown.exe';

var
  Token, RestrictedToken: TJwSecurityToken;
  PrivilegeSet: TJwPrivilegeSet;
  ProcInfo: _PROCESS_INFORMATION;
  {$IFDEF UNICODE}
  StartInfo: _STARTUPINFOW;
  {$ELSE}
  StartInfo: _STARTUPINFOA;
  {$ENDIF}
begin
  Token := TJwSecurityToken.CreateTokenByProcess(getCurrentProcess, TOKEN_ALL_ACCESS);
  try
    PrivilegeSet := TJwPrivilegeSet.Create;
    try
      PrivilegeSet.AddPrivilege(SE_SHUTDOWN_NAME); //this privilege should be removed
      RestrictedToken := Token.CreateRestrictedToken(
        TOKEN_QUERY or TOKEN_DUPLICATE or TOKEN_ASSIGN_PRIMARY, 0, nil, PrivilegeSet, nil);
      try
        ZeroMemory(@StartInfo, SizeOf(Startinfo)); //dummy
        Startinfo.cb := sizeof(Startinfo);

        //start shutdown.exe with the restricted token; it will use the current console!
        if not {$IFDEF UNICODE}CreateProcessAsUserW{$ELSE}CreateProcessAsUserA{$ENDIF}(
                 RestrictedToken.TokenHandle, PathToShutdown, nil, nil,
                 nil, False, 0, nil, nil, Startinfo, ProcInfo) then
        begin
          Writeln(SysErrorMessage(GetLastError));
        end
        else   //this cleanup is only necessary if the process could be created
        begin
          CloseHandle(ProcInfo.hProcess);
          CloseHandle(ProcInfo.hThread);
        end;
      finally
        FreeAndNil(RestrictedToken);
      end;
    finally
      FreeAndNil(PrivilegeSet);
    end;
  finally
    FreeAndNil(Token);
  end;

  Readln;
end.
