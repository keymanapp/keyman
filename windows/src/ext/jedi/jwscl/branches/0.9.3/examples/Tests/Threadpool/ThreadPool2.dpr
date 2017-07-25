program ThreadPool2;

{$APPTYPE CONSOLE}

uses
  ExceptionLog,
  JwaWindows,
  Forms,
  classes,
  syncobjs,
  sysutils,
  ThreadPoolForm in 'ThreadPoolForm.pas' {Form1},
  ProcessList in 'ProcessList.pas';

var
  M1, M2 : TProcessListMemory;
  Processes,
  Processes2: TProcessEntries;
  MEm : TFileStream;
begin
  try
  M2 := TProcessListMemory.CreateOpen('test');
  //WaitForSingleObject(M2.Mutex, INFINITE);


  if M2.Mutex.Acquire(20000) then
  begin
  M2.Read(1000, Processes2);
  M2.Mutex.Release;
  end;
  except
    on e : Exception do
      MessageBox(0,PChar(e.MessagE),'',MB_OK);

  end;
end.
