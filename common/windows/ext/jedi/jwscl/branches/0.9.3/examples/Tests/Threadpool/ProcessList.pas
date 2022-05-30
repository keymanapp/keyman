unit ProcessList;

interface
uses JwaWindows, Classes, SyncObjs, SyncObjsEx, SysUtils, JwsclTypes;

type
  PProcessEntry = ^TProcessEntry;
  TProcessEntry = record
    Handle : THandle;
    ID,
    Session,
    Error : DWORD;
    Duplicated,
    Checked : Boolean;
  end;

  TProcessEntries = array of TProcessEntry;
  TProcessEntriesArray = array of TProcessEntries;
  TProcessList = class;

  TOnCloseAppsPrePrep = procedure(Sender : TProcessList; const SessionID : DWORD; out ProcessHandle : THandle) of object;
  TOnCloseAppsPostPrep = procedure(Sender : TProcessList; const SessionID : DWORD; const Processes : TProcessEntries) of object;
  TOnCloseApps = procedure(Sender : TProcessList; const Processes : TProcessEntriesArray) of object;

  TListChangeType = (lctAdd,lctRemove);

  TOnListChange = procedure(Sender : TProcessList; Index : Integer; ProcessEntry : TProcessEntry; ChangeType : TListChangeType) of object;

  {TProcessList maintains a list of processes across all sessions.}
  TProcessList = class
  protected
    fCloseAll : Boolean;
    fOnCloseAppsPrePrep  : TOnCloseAppsPrePrep;
    fOnCloseAppsPostPrep : TOnCloseAppsPostPrep;
    fOnCloseApps : TOnCloseApps;

    fWaitHandles,
    fContext,
    fList : TList;
    fCritSec : TMultiReadExclusiveWriteSynchronizer;

    fOnListChange : TOnListChange;

    function GetProcessHandle(Index : Integer) : THandle;
    function GetProcessID(Index : Integer) : DWORD;
  public
    constructor Create;

    {}
    destructor Destroy;

    {Adds a process handlet to the list. This process can be closed.
     @param Handle defines the process handle to be used. This handle is automatically
       closed. If you don't want it this way set Duplicate to true.
     @param Duplicate defines whether the handle is to be duplicated.
    }
    function Add(Handle : THandle; const Duplicate : Boolean = false) : Integer;

    {Remove removes the given process from the list. It does not close
     the process though.

     @param Index defines the index of the process to be removed from list.
     @param Close Not used.
    }
    procedure Remove(const Index : Integer; const Close : Boolean = true);

    {GetProcesses returns a copy of the internal process list.}
    function GetProcesses : TProcessEntries;

    {Prepares the closing of all processes. It creates one process list per
     session and calls OnCloseAppsPrePrep, OnCloseAppsPostPrep and OnCloseApps
     in this order.
     The method does not actually close the processes. This task is done in a
     separate process in the appropriate session.

    }
    procedure CloseAll;

    {SendEndSessionToAllWindows sends end message to all given processes.}
    class procedure SendEndSessionToAllWindows(const Processes : TProcessEntries);
    class procedure SendEndSessionMessage(const Wnd : HWND);

  public
    {If CloseAllOnDestroy is true the CloseAll method will be called when the
     instance is freed.
     Not used yet.
     }
    property CloseAllOnDestroy : Boolean read fCloseAll write fCloseAll;

    {Process returns the stored process handle at index.}
    property Process[Index : Integer] : THandle read GetProcessHandle; default;
    {ProcessID returns the stored process ID at index}
    property ProcessID[Index : Integer] : DWORD read GetProcessID;

    {OnCloseAppsPrePrep is called before a process has to be started into the
    target session. The method parameter receives the created process handle
    for duplicating the process handles list.
    }
    property OnCloseAppsPrePrep  : TOnCloseAppsPrePrep read fOnCloseAppsPrePrep write fOnCloseAppsPrePrep;

    {OnCloseAppsPostPrep is called after the whole process list handles were
     duplicated into the target session.
    }
    property OnCloseAppsPostPrep : TOnCloseAppsPostPrep read fOnCloseAppsPostPrep write fOnCloseAppsPostPrep;


    {OnCloseApps is called after the last OnCloseAppsPostPrep event was fired.
     There will be no more processes to be closed.
    }
    property OnCloseApps : TOnCloseApps read fOnCloseApps write fOnCloseApps;

    property OnListChange : TOnListChange read fOnListChange write fOnListChange;
  end;

  {TProcessListMemory maintains a shared memory section for a process list.}
  TProcessListMemory = class
  protected
    fMutex : TMutexEx;

    fHandle : THandle;

  public
    function LoadFromStream(const Stream : TStream) : TProcessEntries;
    procedure SaveToStream(const Stream : TStream; const Processes : TProcessEntries);
  public
    constructor Create(const Name : WideString);
    constructor CreateOpen(const Name : WideString);
    destructor Destroy;


    procedure Write(const Processes : TProcessEntries);
    procedure Read(const TimeOut : DWORD; out Processes : TProcessEntries);

    procedure FireWriteEvent;
    property Mutex : TMutexEx read fMutex;
  end;

implementation
uses math, JwsclToken, JwsclExceptions, JwsclUtils, JwsclComUtils,
   JwsclVersion,
   JwsclCryptProvider;

{ TProcessList }

const MAP_SIZE = 1024*1024;



function CreateProcessEntry(const Handle : THandle; Session : DWORD) : PProcessEntry;
begin
  New(result);
  result.Handle := Handle;
  result.ID     := GetProcessId(Handle);
  result.Session := Session;
  result.Error   := 0;
  result.Checked := false;
end;

type
  PCallbackContext = ^TCallbackContext;
  TCallbackContext = record
    Self : TProcessList;
    ID : DWORD;
  end;

procedure WaitOrTimerCallback(lpParameter : Pointer; TimerOrWaitFired : Boolean) stdcall;
var
  Data : PCallbackContext;
  i : Integer;
begin
  Data := PCallbackContext(lpParameter);


  if Assigned(Data.Self.fList) then
  begin
    Data.Self.fCritSec.BeginWrite;

    try
      for I := 0 to Data.Self.fList.Count - 1 do
      begin
        if Data.Self.ProcessID[i] = Data.ID then
        begin
          Data.Self.Remove(i, false);
          break;
        end;
      end;
    finally
      Data.Self.fCritSec.EndWrite;
    end;
  end;
end;

function TProcessList.Add(Handle: THandle;
  const Duplicate: Boolean): Integer;
var
  T : TJwSecurityToken;
  WaitHandle : THandle;

  CallbackContext : PCallbackContext;
begin
  if Duplicate then
  begin
    if not DuplicateHandle(
      GetCurrentProcess,//hSourceProcessHandle: HANDLE;
      Handle,//hSourceHandle: HANDLE;
      GetCurrentProcess,//hTargetProcessHandle: HANDLE;
      @Handle,//lpTargetHandle: LPHANDLE;
      0,//dwDesiredAccess: DWORD;
      false,//bInheritHandle: BOOL;
      DUPLICATE_SAME_ACCESS//dwOptions: DWORD): BOOL;
    ) then
      RaiseLastOSError;
  end;


  T := TJwSecurityToken.CreateTokenByProcess(Handle, TOKEN_READ or TOKEN_QUERY);
  TJwAutoPointer.Wrap(T);

  New(CallbackContext);
  CallbackContext.Self := Self;
  CallbackContext.ID := JwaWindows.GetProcessId(Handle);

  fCritSec.BeginWrite;
  try
    WaitHandle := INVALID_HANDLE_VALUE;
    if not RegisterWaitForSingleObject(
       WaitHandle,//_out     PHANDLE phNewWaitObject,
       Handle,//__in      HANDLE hObject,
       @WaitOrTimerCallback,//__in      WAITORTIMERCALLBACK Callback,
       CallbackContext,//_in_opt  PVOID Context,
       INFINITE,//__in      ULONG dwMilliseconds,
       WT_EXECUTEDEFAULT or WT_EXECUTEONLYONCE//__in      ULONG dwFlags
      ) then
    begin
      Dispose(CallbackContext);
      RaiseLastOSError;
    end;

    result := fList.Add(CreateProcessEntry(Handle,T.TokenSessionId));
    fContext.Add(CallbackContext);

    if fWaitHandles.IndexOf(Pointer(WaitHandle)) < 0 then
      fWaitHandles.Add(Pointer(WaitHandle));

    if Assigned(OnListChange) then
      OnListChange(Self, result, PProcessEntry(fList[result])^, lctAdd);

  finally
    fCritSec.EndWrite;
  end;


end;

procedure TProcessList.CloseAll;
var
  i,i2 : Integer;
  P : PProcessEntry;
  Sessions : TProcessEntriesArray;
  ProcessHandle : THandle;
begin
  fCritSec.BeginWrite;

  try
    //create a list of sessions that each contains
    //a list of indexes of the fList
    SetLength(Sessions, 1);
    for i := 0 to fList.Count - 1 do
    begin
      P := PProcessEntry(fList[i]);

      if P.Session > high(Sessions) then
        SetLength(Sessions, P.Session);

      SetLength(Sessions[P.Session], high(Sessions[P.Session])+1);
    
      //create copy of this entry
      Sessions[P.Session, high(Sessions[P.Session])+1] := PProcessEntry(fList[i])^;

      P.Checked := not P.Checked;
    end;



    for i := 0 to Length(Sessions) - 1 do
    begin
      if Length(Sessions[i]) > 0 then
      begin
        //let process handles be created into the target session
        OnCloseAppsPrePrep(Self,i, ProcessHandle);

        for i2 := 0 to Length(Sessions[i]) - 1 do
        begin
          if not DuplicateHandle(
            GetCurrentProcess,//hSourceProcessHandle: HANDLE;
            Sessions[i,i2].Handle,//hSourceHandle: HANDLE;
            ProcessHandle,//hTargetProcessHandle: HANDLE;
            @Sessions[i,i2].Handle,//lpTargetHandle: LPHANDLE;
            0,//dwDesiredAccess: DWORD;
            false,//bInheritHandle: BOOL;
            DUPLICATE_SAME_ACCESS//dwOptions: DWORD): BOOL;
          ) then
            Sessions[i,i2].Error := GetLastError;
        end;

        //send new process handles 
        OnCloseAppsPostPrep(Self,i, Sessions[i]);
      end;
    end;

    OnCloseApps(Self,Sessions);
  finally
    fCritSec.EndWrite;
  end;
end;

constructor TProcessList.Create;
begin
  fList := TList.Create;
  fWaitHandles := TList.Create;
  fContext := TList.Create;
  fCritSec := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TProcessList.Destroy;
 procedure ClearContext;
 var i : Integer;
 begin
   for i := 0 to fContext.Count - 1 do
     Dispose(fContext[i]);
 end;

 procedure ClearList;
 var i : Integer;
 begin
   for i := 0 to fList.Count - 1 do
   begin
     if PProcessEntry(fList[i])^.Duplicated then
       CloseHandle(PProcessEntry(fList[i])^.Handle);
     Dispose(fList[i]);
   end;
 end;

var i : Integer;
begin
  for I := 0 to fWaitHandles.Count - 1 do
    UnregisterWait(DWORD(fWaitHandles[i]));

  fCritSec.BeginWrite;
  try
    ClearContext;
    ClearList;

    FreeAndNil(fContext);
    FreeAndNil(fList);
    FreeAndNil(fWaitHandles);
  finally
    fCritSec.EndWrite;
  end;

  FreeAndNil(fCritSec);
end;

function TProcessList.GetProcesses: TProcessEntries;
var i : Integer;
begin
  SetLength(result, fList.Count);
  for I := 0 to fList.Count - 1 do
  begin
    result[i] := PProcessEntry(fList)^;
  end;
end;

function TProcessList.GetProcessHandle(Index: Integer): THandle;
begin
  fCritSec.BeginRead;
  try
    result := PProcessEntry(fList[Index])^.Handle;
  finally
    fCritSec.EndRead;
  end;
end;

function TProcessList.GetProcessID(Index: Integer): DWORD;
begin
  fCritSec.BeginRead;
  try
    result := PProcessEntry(fList[Index])^.ID;
  finally
    fCritSec.EndRead;
  end;
end;

procedure TProcessList.Remove(const Index: Integer; const Close: Boolean);
begin
  fCritSec.BeginWrite;
  try
    if fList[Index] <> nil then
    begin
      if Assigned(OnListChange) then
        OnListChange(Self, Index, PProcessEntry(fList[Index])^ , lctRemove);

      //if PProcessEntry(fList[Index]).Duplicated then
        CloseHandle(PProcessEntry(fList[Index]).Handle);
      Dispose(PProcessEntry(fList[Index]));
      fList.Delete(Index);
    end;
  finally
    fCritSec.EndWrite;
  end;
end;

class procedure TProcessList.SendEndSessionMessage(const Wnd: HWND);
var
  Res,
  MsgRes : DWORD;
  TimedOut : Boolean;
begin
  Res := SendMessageTimeoutW(Wnd, WM_QUERYENDSESSION, 0, ENDSESSION_LOGOFF,
    SMTO_ABORTIFHUNG or SMTO_ABORTIFHUNG or SMTO_BLOCK,
     30 * 1000, MsgRes);

  if Res = 0 then
  begin
    if TJwWindowsVersion.IsWindows2000(false) then
    begin
      TimedOut := GetLastError = 0;
    end
    else
    begin
      TimedOut := GetLastError = ERROR_TIMEOUT;
    end;
  end;

  if not TimedOut then
  begin
    Res := SendMessageTimeoutW(Wnd, WM_ENDSESSION, 1, ENDSESSION_LOGOFF,
       SMTO_ABORTIFHUNG or SMTO_ABORTIFHUNG or SMTO_BLOCK,
         30 * 1000, MsgRes);
  end;
end;


function EndSessionEnumWindowsProc(wnd : HWND; _lParam : LPARAM) : Boolean; stdcall;
var ID : DWORD;
begin
  GetWindowThreadProcessId(wnd, @ID);
  if (_lParam <> 0) and (TProcessEntry(Pointer(_lParam)^).ID = ID) then
  begin
    TProcessList.SendEndSessionMessage(wnd);
  end;
  result := true;
end;


class procedure TProcessList.SendEndSessionToAllWindows(const Processes : TProcessEntries);
var i : Integer;
begin
  for I := 0 to Length(Processes) - 1 do
  begin
    EnumWindows(@EndSessionEnumWindowsProc, LPARAM(@Processes[i]));
  end;
end;

type
  TPointMemoryStream = class(TMemoryStream)
  public
    procedure SetPointer(Ptr: Pointer; Size: Longint);
  end;

procedure TPointMemoryStream.SetPointer(Ptr: Pointer; Size: Longint);
begin
  inherited;
end;

{ TProcessListMemory }

constructor TProcessListMemory.Create(const Name: WideString);
begin
  fHandle := CreateFileMappingW(
      INVALID_HANDLE_VALUE,//__in      HANDLE hFile,
      nil,//__in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      PAGE_READWRITE,//__in      DWORD flProtect,
      0,//__in      DWORD dwMaximumSizeHigh,
      MAP_SIZE,//__in      DWORD dwMaximumSizeLow,
      PWideChar(Name) //__in_opt  LPCTSTR lpName
      );
  if fHandle = 0 then
    RaiseLastOSError;

  //true means, the mutex is set to unsignled  
  fMutex := TMutexEx.Create(nil, true, PWideChar(Name+'_WriteEvent'));
end;

constructor TProcessListMemory.CreateOpen(const Name: WideString);
begin
  fHandle := OpenFileMappingW(
      FILE_MAP_READ,//__in  DWORD dwDesiredAccess,
      false, //__in  BOOL bInheritHandle,
      PWideChar(Name)//__in  LPCTSTR lpName
    );
(*  fHandle := CreateFileMappingW(
      INVALID_HANDLE_VALUE,//__in      HANDLE hFile,
      nil,//__in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      PAGE_READONLY, //PAGE_READONLY,//__in      DWORD flProtect,
      0,//__in      DWORD dwMaximumSizeHigh,
      MAP_SIZE,//in      DWORD dwMaximumSizeLow,
      PWideChar(Name) //__in_opt  LPCTSTR lpName
      );*)
  if fHandle = 0 then
    RaiseLastOSError;


  fMutex := TMutexEx.Create(MUTEX_MODIFY_STATE or SYNCHRONIZE, false, PWideChar(Name+'_WriteEvent'));
end;

destructor TProcessListMemory.Destroy;
begin
  CloseHandle(fHandle);

  fMutex.Free;
  inherited;
end;

procedure TProcessListMemory.FireWriteEvent;
begin
end;

function TProcessListMemory.LoadFromStream(const Stream : TStream) : TProcessEntries;
var
  i,
  ReadHashLen,
  HashLen,
  Len,
  Size : DWORD;
  Hash : TJwHash;

  Pos : Int64;

  HashData,
  ReadHash : Pointer;

  HH : Array[0..16] of byte;
  RH : Array[0..100] of byte;

begin
  TMemoryStream(Stream).SaveToFile('E:\Temp\_DataLoad.dat');

  //get actual size of data
  Stream.Read(Size, sizeof(ReadHashLen));

  //jump to hash data
  Stream.Seek(Size, soFromBeginning);

  //get len of hash written to stream
  Stream.Read(ReadHashLen, sizeof(ReadHashLen));

  //get the actual hash

  GetMem(ReadHash, ReadHashLen);
  Stream.Read(ReadHash^, ReadHashLen);
  //CopyMemory(@RH, @ReadHash, ReadHashLen);

  //save stream position of the end
  Pos := Stream.Position;

  //hash data
  Hash := TJwHash.Create(haMD5);
  try
    Stream.Seek(0, soFromBeginning);
    Hash.HashStream(Stream, Size);

    HashData := Hash.RetrieveHash(HashLen);
    //CopyMemory(@HH[0], HashData, HashLen);
    try
      if not CompareMem(ReadHash, HashData, min(HashLen,ReadHashLen)) then
        raise EJwsclHashMismatch.Create('');
    finally
      Hash.FreeBuffer(HashData);
    end;
  finally
    Hash.Free;

  end;
  FreeMem(ReadHash);

  Stream.Seek(0, soFromBeginning);

  //get actual size of data
  Stream.Read(Size, sizeof(ReadHashLen));

  //get count of process entries
  Stream.Read(Len, sizeof(Len));

  SetLength(result, Len);
  for i := 0 to Len - 1 do
  begin
    Stream.Read(result[i], sizeof(result[i]));
  end;

  Stream.Position := Pos;
end;

procedure TProcessListMemory.Read(const TimeOut : DWORD; out Processes: TProcessEntries);
var
  Data : Pointer;
  Stream : TPointMemoryStream;

begin
  if not Mutex.Acquire(TimeOut) then
    raise EAbort.Create('Read of process list from shared memory aborted due to timeout');
  try
    Data := MapViewOfFile(fHandle,   // handle to map object
                          FILE_MAP_READ, // read/write permission
                          0,
                          0,
                          MAP_SIZE);


    if Data = nil then
      RaiseLastOSError;


    Stream := TPointMemoryStream.Create;
    try
      //we use a custom stream implementation to act on the memory
      Stream.SetPointer(Data, MAP_SIZE);

      Processes := LoadFromStream(Stream);
    finally
      Stream.SetPointer(nil, 0);
      Stream.Free;
      UnmapViewOfFile(Data);
    end;
  finally
    Mutex.Release;
  end;
end;

procedure TProcessListMemory.SaveToStream(const Stream : TStream; const Processes : TProcessEntries);
var
  HashData : Pointer;
  i,
  Len,
  HashLen,
  ProcDataSize,
  Size : DWORD;
  Hash : TJwHash;

  HH : Array[0..16] of byte;

begin

  Size := Length(Processes) * sizeof(TProcessEntry);

  ProcDataSize := Size+sizeof(Size)+sizeof(Len);

  Stream.Write(ProcDataSize, sizeof(ProcDataSize));

  Len := Length(Processes);
  Stream.Write(Len, sizeof(Len));

  for i := 0 to Length(Processes) - 1 do
  begin
    Stream.Write(Processes[i], sizeof(Processes[i]));
  end;

  Hash := TJwHash.Create(haMD5);

  Stream.Seek(0, soFromBeginning);
  Hash.HashStream(Stream, ProcDataSize);
  HashData := Hash.RetrieveHash(HashLen);
  //CopyMemory(@HH, HashData, HashLen);

  Stream.Seek(ProcDataSize, soFromBeginning);
  try
    Stream.Write(HashLen,sizeof(HashLen));
    Stream.Write(HashData^, HashLen);
  finally
    Hash.FreeBuffer(HashData);
    Hash.Free;
  end;

  Stream.Seek(0, soFromBeginning);
  TMemoryStream(Stream).SaveToFile('E:\Temp\_DataSave.dat');

end;

procedure TProcessListMemory.Write(const Processes: TProcessEntries);
var
  Data: Pointer;
  Stream : TPointMemoryStream;
begin
  Mutex.Acquire;
  try
    Data := MapViewOfFile(fHandle,   // handle to map object
                          FILE_MAP_ALL_ACCESS, // read/write permission
                          0,
                          0,
                          MAP_SIZE);      

    //we use a custom stream implementation to act on the memory
    Stream := TPointMemoryStream.Create;
    try
      Stream.SetPointer(Data, MAP_SIZE);

      SaveToStream(Stream, Processes);
    finally
      Stream.SetPointer(nil, 0);
      Stream.Free;
      UnmapViewOfFile(Data);
    end;
  finally
    Mutex.Release;
  end;
end;



end.
