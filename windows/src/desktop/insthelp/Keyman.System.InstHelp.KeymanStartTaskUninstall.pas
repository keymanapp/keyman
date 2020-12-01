unit Keyman.System.InstHelp.KeymanStartTaskUninstall;

interface

type
  TKeymanStartTaskUninstall = class
    class procedure DeleteAllTasks;
  private
    class procedure ReportEvent(const Message: string); static;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  Winapi.ActiveX,
  Winapi.Windows,

  klog,
  TaskScheduler_TLB;

const
  CTaskFolderName = 'Keyman';

class procedure TKeymanStartTaskUninstall.ReportEvent(const Message: string);
var
  hEventLog: THandle;
begin
  kl.LogError(Message);
  hEventLog := RegisterEventSource(nil, 'Keyman-Uninstall');
  Winapi.Windows.ReportEvent(
    hEventLog,
    EVENTLOG_ERROR_TYPE,
    1,
    1,
    nil,
    0,
    Length(Message) * sizeof(Char),
    nil,
    PChar(Message)
  );
  CloseHandle(hEventLog);
end;

class procedure TKeymanStartTaskUninstall.DeleteAllTasks;
var
  pService: ITaskService;
  pTaskFolder: ITaskFolder;
  pTasks: IRegisteredTaskCollection;
  i: Integer;
begin
  try
    pService := CoTaskScheduler_.Create;
    pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
    try
      pTaskFolder := pService.GetFolder('\'+CTaskFolderName);
    except
      on E:Exception do
      begin
        // We can't find the folder, for some unknown reason, so give up
        ReportEvent('Failed to find '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
        Exit;
      end;
    end;

    pTasks := pTaskFolder.GetTasks(0);

    for i := 0 to pTasks.Count - 1 do
      pTaskFolder.DeleteTask(pTasks.Item[i].Name, 0);

    pTaskFolder := nil;
    pService.GetFolder('\').DeleteFolder(CTaskFolderName, 0);
  except
    on E:Exception do
    begin
      // Silently ignore errors; this is cleanup that won't really hurt but
      // just be a little messy if it fails -- the scheduled task will never
      // be triggered as the relevant event will never be added!
      ReportEvent('Failed to delete '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
    end;
  end;
end;

end.
