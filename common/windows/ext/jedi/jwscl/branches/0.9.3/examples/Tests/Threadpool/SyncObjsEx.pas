unit SyncObjsEx;

interface
uses Windows, SyncObjs, JwsclUtils;

type
  {TMutexEx extends the standard VCL class TMutex with additional functions
   * Aquire mutex with timeout
   * Try 	acquirement of mutex}
  TMutexEx = class
  protected
    fEvent : SyncObjs.TEvent;
    fHandle: THandle;
  public
    constructor Create(MutexAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean = False); overload;
    constructor Create(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: string; UseCOMWait: Boolean = False); overload;

    {Standard call with infinite timeout. Blocks until the mutex
    is freed.
    raises
      EAbort This exception will be raised if the StopEven is fired. The exception
       prevents code to be executed after the return of the method.
    }
    procedure Acquire; overload;

    {Releases the mutex}
    procedure Release;

    {Like the method Acquire it waits until the mutex is free.
     If the specified timeout is reached the method also returns.
     @return Returns true if the mutex is free. If the timeout has been
     reached the return value is false.
    }
    function Acquire(const TimeOut : DWORD) : Boolean; overload; virtual;

    {TryAcquire checks the mutex signal state and returns immediately.
     @return Returns true if the mutex is free otherwise false.
    }
    function TryAcquire : boolean; virtual;

    {WaitFor is an extended version that supports a stop event that can let
     return any Acquire method early.
    }
    function WaitFor(Timeout: LongWord): TWaitResult;

    {The stop event makes it possible to return from a waiting state early.
    All Acquire methods blocks until the mutex is free, a timeout has occured
    or an error. However if you create a TEvent instance and assigns it to
    this property before any Acquire methods, you can return immediately
    any acquirement call.
    Setting the event after a call to Acquire has no effect.
    Closing an event while it is used can have unpredictable results.
    }
    property StopEvent : SyncObjs.TEvent read fEvent write fEvent;

    property Handle: THandle read FHandle;
  end;


implementation
uses SysUtils;

constructor TMutexEx.Create(MutexAttributes: PSecurityAttributes;
  InitialOwner: Boolean; const Name: string; UseCOMWait: Boolean);
var
  lpName: PChar;
begin
  inherited Create;
  if Name <> '' then
    lpName := PChar(Name)
  else
    lpName := nil;
  FHandle := CreateMutex(MutexAttributes, InitialOwner, lpName);
  if FHandle = 0 then
    RaiseLastOSError;
end;

constructor TMutexEx.Create(DesiredAccess: LongWord; InheritHandle: Boolean;
  const Name: string; UseCOMWait: Boolean);
var
  lpName: PChar;
begin
  inherited Create;

  if Name <> '' then
    lpName := PChar(Name)
  else
    lpName := nil;
  FHandle := OpenMutex(DesiredAccess, InheritHandle, lpName);
  if FHandle = 0 then
    RaiseLastOSError;
end;

procedure TMutexEx.Acquire;
var WR : TWaitResult;
begin
  WR := WaitFor(INFINITE);
  case WR of
    wrError : RaiseLastOSError;
    wrTimeout : raise EAbort.Create('StopEvent fired');
  end;
end;

function TMutexEx.Acquire(const TimeOut : DWORD) : Boolean;
var WR : TWaitResult;
begin
  WR := WaitFor(TimeOut);
  case WR of
    wrError : RaiseLastOSError;
    wrAbandoned,
    wrSignaled : result := true;
  else
    //wrTimeout
    result := false;
  end;
end;

function TMutexEx.WaitFor(Timeout: LongWord): TWaitResult;
var
  Index: DWORD;
  Handles: array of THandle;
begin
  SetLength(Handles, 1);
  Handles[0] := FHandle;

  if Assigned(fEvent) then
  begin
    SetLength(Handles, high(Handles)+1);
    Handles[high(Handles)] := fEvent.Handle;
  end;

    case JwWaitForMultipleObjects(Handles, false, Timeout) of
      WAIT_ABANDONED: Result := wrAbandoned;
      WAIT_OBJECT_0: Result := wrSignaled;
      WAIT_OBJECT_0+1,
      WAIT_TIMEOUT: Result := wrTimeout;
      WAIT_FAILED:
        begin
          Result := wrError;
        end;
    else
      Result := wrError;
    end;
end;

procedure TMutexEx.Release;
begin
  if not ReleaseMutex(fHandle) then
    RaiseLastOSError;
end;

function TMutexEx.TryAcquire: boolean;
begin
  result := Acquire(0);
end;

end.
