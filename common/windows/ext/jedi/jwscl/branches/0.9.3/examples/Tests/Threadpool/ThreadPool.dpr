program ThreadPool;

uses
  ExceptionLog,
  Forms,
  JwaWindows,
  classes,
  sysutils,
  ThreadPoolForm in 'ThreadPoolForm.pas' {Form1},
  ProcessList in 'ProcessList.pas',
  SyncObjsEx in 'SyncObjsEx.pas';

{$R *.res}

var
  M1, M2 : TProcessListMemory;
  Processes,
  Processes2: TProcessEntries;
  MEm : TFileStream;
begin
  M1 := TProcessListMemory.Create('test');

  SetLength(Processes,3);
  Processes[0].Handle := 1;
  Processes[1].Handle := 2;
  Processes[2].Handle := 3;

{  Mem := TFileStream.Create('E:\Temp\_Data.dat',fmCreate);
  M1.SaveToStream(Mem,Processes);
  Mem.Free;

  Mem := TFileStream.Create('E:\Temp\_Data.dat',fmOpenReadWrite);
  Processes := M1.LoadFromStream(Mem);
  Mem.Free;

  exit;
 }
  M1.Mutex.Acquire;
  M1.Write(Processes);
 // M1.Mutex.Release;
  M1.Mutex.Release;
 // ReleaseMutex(M1.Mutex);

  readln;
 {M2 := TProcessListMemory.CreateOpen('test');
  M2.Read(Processes2);

  WaitForSingleObject(M1.Mutex, INFINITE);
  }

  exit;
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
