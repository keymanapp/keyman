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
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.Windows,

  klog,
  TaskScheduler_TLB;

const
  CTaskFolderName = 'Keyman';

class procedure TKeymanStartTaskUninstall.ReportEvent(const Message: string);
var
  hEventLog: THandle;
  msgs: array[0..0] of PChar;
begin
  kl.LogError(Message);
  hEventLog := RegisterEventSource(nil, 'Keyman-Uninstall');
  msgs[0] := PChar(Message);
  Winapi.Windows.ReportEvent(
    hEventLog,
    EVENTLOG_ERROR_TYPE,
    0,
    99,
    nil,
    1,
    0,
    @msgs,
    nil
  );
  DeregisterEventSource(hEventLog);
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
      on E:EOleException do
      begin
        // We can't find the folder, for some unknown reason, so give up
        if E.ErrorCode <> HResultFromWin32(ERROR_FILE_NOT_FOUND) then
          ReportEvent('Failed to find '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
        Exit;
      end;

      on E:Exception do
      begin
        ReportEvent('Failed to find '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
        Exit;
      end;
    end;

    pTasks := pTaskFolder.GetTasks(0);

    for i := 1 to pTasks.Count do // 1-based collection
    begin
      pTaskFolder.DeleteTask(pTasks.Item[i].Name, 0);
    end;

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
