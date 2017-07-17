unit HandleRequestThread;

interface

uses
  Classes, JwaWindows, ElevationHandler, JwsclToken, JwsclSid,
  JwsclUtils, SessionPipe, SysUtils, JwsclLogging, uLogging, JwsclProcess,
  ThreadedPasswords;

type
  THandleRequestThread = class(TJwThread)
  private
    { Private declarations }
    fPipeHandle: THandle;
    fAllowedSIDs:  TJwSecurityIdList;
    fJobs : TJwJobObjectSessionList;
    fPasswords   : TPasswordList;
    fThreadsStoppedEvent,
    fServiceStopEvent  : THandle;
    fOnServiceProcessRequest : TOnServiceProcessRequest;
    fStopState : PBoolean;

    procedure IncreaseRequestThreadCount;
    procedure DecreaseRequestThreadCount;
  public
    constructor Create(
        CreateSuspended: Boolean;
        const Jobs : TJwJobObjectSessionList;
        const AllowedSIDs:  TJwSecurityIdList;
        const Passwords   : TPasswordList;
        const ServiceStopEvent : THandle;
        const ThreadsStoppedEvent : THandle;

        const OnServiceProcessRequest : TOnServiceProcessRequest;
        const StopState : PBoolean);
    property PipeHandle: THandle read fPipeHandle write fPipeHandle;

    procedure Execute; override;
  end;

implementation
uses MainUnit, ComObj;

{ THandleRequestThread }

constructor THandleRequestThread.Create(
  CreateSuspended: Boolean;
  const Jobs : TJwJobObjectSessionList;
  const AllowedSIDs: TJwSecurityIdList;
  const Passwords: TPasswordList;
  const ServiceStopEvent: THandle;
  const ThreadsStoppedEvent : THandle;
  const OnServiceProcessRequest: TOnServiceProcessRequest;
  const StopState: PBoolean);
begin

  fJobs := Jobs;
  fAllowedSIDs := AllowedSIDs;
  fPasswords := Passwords;
  fServiceStopEvent := ServiceStopEvent;
  fThreadsStoppedEvent := ThreadsStoppedEvent;
  fOnServiceProcessRequest := OnServiceProcessRequest;
  fStopState := StopState;

  inherited Create(CreateSuspended,'HandleRequest');
end;

procedure THandleRequestThread.IncreaseRequestThreadCount;
begin
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, 1) = 0 then
    ResetEvent(fThreadsStoppedEvent);
end;

procedure THandleRequestThread.Execute;
var AppName: string; PipeSize: Cardinal;
    OvLapped: OVERLAPPED;
    ElevationObj : TElevationHandler;
    Log : IJwLogClient;
begin
  Self.Name := 'HandleRequest: '+IntToStr(fPipeHandle);

  Log := uLogging.LogServer.Connect(etThread,ClassName,'Execute','HandleRequestThread.pas','Init Requestthread: '+Name);

  //count number of threads
  IncreaseRequestThreadCount;

  try
    try

      if (fStopState <> nil) and (fStopState^) then
      begin
        Log.Log(lsStop,'XPService.Stopped = true ');
        exit;
      end;

      OvLapped.hEvent := CreateEvent(nil, false, false, nil);
      try
        If not ReadFile(fPipeHandle, nil, 0, nil, @OvLapped) then
          LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','');

        case JwWaitForMultipleObjects([OvLapped.hEvent, XPService.ServiceStopEvent], false, 10 *1000) of
          WAIT_TIMEOUT:
          begin
            XPService.LogEvent('HandleRequestThread was timed out');
            exit;
          end;
          WAIT_OBJECT_0+1:
          begin
            Log.Log('Server shutdown registered.');
            exit;
          end;
        end;

        if not PeekNamedPipe(fPipeHandle, nil, 0, nil, @PipeSize, nil) then
          LogAndRaiseLastOsError(Log,ClassName,'PeekNamedPipe@Execute','');

        SetLength(AppName, PipeSize);

        if not ReadFile(fPipeHandle, @AppName[1], PipeSize, nil, @OvLapped) then
          LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','');

        Log.Log('Waiting for path name to be received...');
        //Wait for incoming application name to elevate
        WaitForSingleObject(OvLapped.hEvent, INFINITE);

        TJwSecurityToken.ImpersonateNamedPipeClient(fPipeHandle);
        //impersonated thread is used in elevation handler

        ElevationObj := TElevationHandler.Create(
           fAllowedSIDs,
           fJobs,
           fPasswords,
           fServiceStopEvent,
           fStopState);
        try
          ElevationObj.StartApplication(AppName);
        finally
          ElevationObj.Free;
        end;

      finally
        CloseHandle(OvLapped.hEvent);
        DisconnectNamedPipe(fPipeHandle);
        CloseHandle(fPipeHandle);
      end;

    finally
      DecreaseRequestThreadCount;
    end;
  except
    on E : Exception do
     Log.Exception(E);
  end;

end;

procedure THandleRequestThread.DecreaseRequestThreadCount;
begin
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, -1) = 1 then
    SetEvent(fThreadsStoppedEvent);
end;

end.
