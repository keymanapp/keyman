program Client;

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
    lpData : TMessage;

    iWritten,
    iRead : DWORD;
begin
  JwInitWellKnownSIDs;

  if not WaitNamedPipe('\\.\pipe\JwsclPipeTest', 20000) then
    RaiseLastOSError;

  PipeHandle := CreateFile(
      '\\.\pipe\JwsclPipeTest',//__in      LPCTSTR lpFileName,
      GENERIC_ALL,//__in      DWORD dwDesiredAccess,
      0,//__in      DWORD dwShareMode,
      nil,//__in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
      OPEN_EXISTING,//__in      DWORD dwCreationDisposition,
      0,//__in      DWORD dwFlagsAndAttributes,
      0//__in_opt  HANDLE hTemplateFile
    );
  if PipeHandle = INVALID_HANDLE_VALUE then
      RaiseLastOSError;

  try
    lpData.Data := 'This is data from client.'#0#0;
    if not WriteFile(PipeHandle, @lpData,sizeof(lpData), @iWritten, nil) then
      RaiseLastOSError;

    if not ReadFile(PipeHandle,@lpData,sizeof(lpData),@iRead, nil) then
      RaiseLastOSError;

     writeln('Data from other side: ',lpData.Data);
  finally
    CloseHandle(PipeHandle);
  end;
  writeln('Hit Enter');
  Readln;
end.
