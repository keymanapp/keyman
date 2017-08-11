{This example shows how to change the process DACL
to prevent the process to be terminated forcefully. However
this is not a complete and secure way to prevent the application to be terminated.
The user may restore the DACL and allow herself to close the application.
Furthermore an administrator can always take ownership and restore the DACL.

Blog:
http://blog.delphi-jedi.net/2008/11/08/restrict-access-to-process

}

program ProcessDACLDemo;

{$APPTYPE CONSOLE}
uses
  JwaWindows,
  JwsclSecureObjects,
  JwsclDescriptor,
  JwsclToken,
  JwsclTypes,
  JwsclAcl,
  JwsclKnownSid;

procedure DenyCurrentUserToAccessCurrentProcess;
var
  Token : TJwSecurityToken;
  SD : TJwSecurityDescriptor;
  i : Integer;
  hProcess : TJwProcessHandle;
begin
  JwInitWellKnownSIDs;

  hProcess := OpenProcess(READ_CONTROL or WRITE_DAC, false, GetCurrentProcessId());
  if hProcess <> 0 then
  try
    SD := TJwSecureGeneralObject.GetSecurityInfo(hProcess,SE_KERNEL_OBJECT, [siDaclSecurityInformation]);
    try
      SD.DACL.Clear;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwLocalSystemSID));
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_READ, JwSecurityProcessUserSID));

      TJwSecureGeneralObject.SetSecurityInfo(hProcess, SE_KERNEL_OBJECT, [siDaclSecurityInformation], SD);
    finally
      SD.Free;
    end;
  finally
    CloseHandle(hProcess);
  end;
end;

begin
  DenyCurrentUserToAccessCurrentProcess;
  Writeln('Process DACL was adapted. Try to kill this window using a none-adminstrative Taskmanager or call TerminateProcess on process ID: ',GetCurrentProcessID);
  Writeln('Hit any key to finish....');
  Readln;
end.
 