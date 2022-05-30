program Server;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Dialogs,
  jwaWindows,
  jwsclDescriptor,
  jwsclACL,
  JwsclSID,
  jwsclKnownSid,
  jwsclMapping,
  jwsclToken,
  jwsclStrings;

type TMessage = record
       Data : array[0..1023] of Char; 
     end;

var PipeHandle : THandle;
    pSecAttr : TSecurityAttributes;
    SD : TJwSecurityDescriptor;
    i : Integer;
    lpData : TMessage;
    iWritten,
    iRead : DWORD;

    PipeToken : TJwSecurityToken;
    c : Char;
begin
  Writeln('Server Pipe');
  JwInitWellKnownSIDs;

  SD := TJwSecurityDescriptor.Create;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID, false));
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID, false));

  Writeln('Do you want to allow the currently logged on user to connect to the pipe? (y/n)');
  Writeln('If you say yes, your user will succed to connect to the pipe. However if you are using Vista, the user will get'+
   'the string ''denied'' from the server because he is deny only for admins. Is the process started elevated or on XP it should work fine.');
  Writeln('If you say no, the user will only allowed to be connected to the pipe, if he is an administrator or system ');
  Write('[Y/N]:');

  Read(c);
  if UpCase(c) = 'Y' then
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalGroupSID, false));


  //ShowMessage(SD.GetTextMap(TJwSecurityPipeMapping));
                                  // Mapping prop für SD schreiben
  //remove Everyone group if available
  i := SD.DACL.FindSID(JwWorldSID);
  if i > -1 then
  begin
    {If lpSecurityAttributes is NULL, the named pipe gets a default security descriptor
    and the handle cannot be inherited. The ACLs in the default security descriptor for
    a named pipe grant full control to the LocalSystem account, administrators, and the creator owner.
    They also grant read access to members of the Everyone group and the anonymous account.
    }
    SD.DACL.Delete(i); //remove Everyone group
  end;


  pSecAttr := SD.Create_SAEx(true);

  try
    PipeHandle := CreateNamedPipe(
      '\\.\pipe\JwsclPipeTest',//__in      LPCTSTR lpName,
      PIPE_ACCESS_DUPLEX or FILE_FLAG_FIRST_PIPE_INSTANCE,//__in      DWORD dwOpenMode,
      PIPE_TYPE_MESSAGE or       // message type pipe
        PIPE_WAIT,//__in      DWORD dwPipeMode,
      1,//__in      DWORD nMaxInstances,
      4096,//__in      DWORD nOutBufferSize,
      4096,//__in      DWORD nInBufferSize,
      0,//__in      DWORD nDefaultTimeOut,
      LPSECURITY_ATTRIBUTES(@pSecAttr)//__in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes
    );
    if PipeHandle = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
  finally
    SD.Free_SAEx(pSecAttr);
    FreeAndNil(SD);
  end;

  try
    if not ConnectNamedPipe(PipeHandle,nil) then
      RaiseLastOSError;

    if not ReadFile(PipeHandle,@lpData,sizeof(lpData),@iRead, nil) then
      RaiseLastOSError;

    writeln('Data from other side: ',lpData.Data);

    try
      //throws exception if fails
      TJwSecurityToken.ImpersonateNamedPipeClient(PipeHandle);
      //we are in context of user now
      
      try
        if JwCheckAdministratorAccess then
        begin
          lpData.Data := 'Success'#0#0;
          Writeln('Client is approved.');
        end
        else
        begin
          lpData.Data := 'Denied.'#0#0;
          Writeln('Client is denied.');
        end;

        WriteFile(PipeHandle, @lpData,sizeof(lpData), @iWritten, nil);
        
      finally
        TJwSecurityToken.RevertToSelf;
      end;
    finally
    end;
  finally
    Sleep(2000); //or wait for more connections as shown above
    CloseHandle(PipeHandle);
  end;
  writeln('Hit enter...');
  readln;
end.
 