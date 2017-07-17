unit ThreadUnit;

interface

uses
  Classes, JwsclToken, JwaWindows, SysUtils;

type
  TUnloadProfThread = class(TThread)
  private
    fList:             TThreadList;
    fIOCompletionPort: THandle;
    fCurrentId:        Integer;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    procedure Add(Job, Profile: Cardinal; Token: TJwSecurityToken);
    procedure RequestTerminate; //Terminate is not virtual, so use a new method instead
  end;

var UnloadProfThread: TUnloadProfThread;

implementation
uses MainUnit, JwsclUtils;

type
  PJobInformation = ^TJobInformation;
  TJobInformation = record
    Job:     Cardinal;
    Profile: Cardinal;
    Token:   TJwSecurityToken;
    Id:      Integer;
  end;

procedure TUnloadProfThread.Execute;
var Val, Key: Cardinal; Ov: POVERLAPPED; i: integer;
begin
  JwSetThreadName('UnloadProfThread');
  while True do
  begin
    repeat
      GetQueuedCompletionStatus(fIOCompletionPort, Val, Key, Ov, INFINITE);
    until (Val=JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO);

    if Ov <> nil then //Is this a stop message?
    begin
      with fList.LockList do
      begin
        for i:=0 to Count-1 do
        begin
          with PJobInformation(Items[i])^ do
          begin
            TerminateJobObject(Job, 0);
            CloseHandle(Job);
            UnloadUserProfile(Token.TokenHandle, Profile);
            Token.Free;
          end;
          Dispose(PJobInformation(Items[i]));
        end;
        break;
      end;
    end;

    with fList.LockList do
    try
      for i:=0 to Count-1 do
        if Cardinal(PJobInformation(Items[i])^.Id) = Key then
        begin
          with PJobInformation(Items[i])^ do
          begin
            CloseHandle(Job);
            UnloadUserProfile(Token.TokenHandle, Profile);
            Token.Free;
          end;
          Dispose(PJobInformation(Items[i]));
          Delete(i);
          break;
        end;
    finally
      fList.UnlockList;
    end;
  end;
end;

procedure TUnloadProfThread.Add(Job: Cardinal; Profile: Cardinal; Token: TJwSecurityToken);
var JobInformation: PJobInformation;
    AssocPort: JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
begin
  New(JobInformation);
  JobInformation.Job := Job;
  JobInformation.Profile := Profile;
  JobInformation.Token := Token;

  {The following can be dangerous. It assumes that there are
  no more than 2^32 processes started by the service. If this
  is not the case, an id might be doubled. We could use the list
  lock to lock other threads while checking if the current id
  does already exist, but this would be time expensive.}
  JobInformation.Id := InterlockedExchangeAdd(fCurrentId, 1);
  AssocPort.CompletionPort := fIOCompletionPort;
  AssocPort.CompletionKey := Pointer(JobInformation.Id);
  if not SetInformationJobObject(Job, JobObjectAssociateCompletionPortInformation,
           @AssocPort, SizeOf(AssocPort)) then
    XPService.LogEvent('SetInformationJobObject failed with '+SysErrorMessage(GetLastError), ltError)
  else
    fList.Add(JobInformation);
end;

procedure TUnloadProfThread.DoTerminate;
begin
  CloseHandle(fIOCompletionPort);
  fList.Free;
  XPService.LogEvent('UnloadProfThread terminates');
end;

procedure TUnloadProfThread.RequestTerminate;
begin
  Terminate;
  PostQueuedCompletionStatus(fIOCompletionPort, JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO, 0, Pointer(1));
end;

constructor TUnloadProfThread.Create;
begin
  fList := TThreadList.Create;
  fIOCompletionPort := CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 1);
  fCurrentId := 0;
  inherited Create(false);
end;

end.
