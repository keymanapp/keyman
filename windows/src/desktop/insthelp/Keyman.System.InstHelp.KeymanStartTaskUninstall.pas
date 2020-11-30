unit Keyman.System.InstHelp.KeymanStartTaskUninstall;

interface

type
  TKeymanStartTaskUninstall = class
    class procedure DeleteAllTasks;
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
        kl.LogError('Failed to find '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
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
      kl.LogError('Failed to delete '+CTaskFolderName+' task folder: '+E.ClassName+', '+E.Message);
    end;
  end;
end;

end.
