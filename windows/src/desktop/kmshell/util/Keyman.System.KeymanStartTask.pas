unit Keyman.System.KeymanStartTask;

interface

type
  TKeymanStartTask = class
  private
  public
    class procedure Recreate;
    class procedure DeleteTask;
    class procedure CreateTask;
  end;

implementation

uses
  System.Variants,
  Winapi.ActiveX,
  Winapi.Windows,

  DebugPaths,
  KeymanPaths,
  KeymanVersion,
  RegistryKeys,
  TaskScheduler_TLB;

{ TKeymanStartTask }

const
  CTaskName = 'Start On Demand';
  CTaskTrigger =
    '<QueryList>'+
      '<Query Id="0" Path="Application">'+
        '<Select Path="Application">*[System[Provider[@Name=''Keyman''] and EventID=256]]</Select>'+
      '</Query>'+
    '</QueryList>';

class procedure TKeymanStartTask.CreateTask;
var
  pService: ITaskService;
  pTaskFolder: ITaskFolder;
  pTask: ITaskDefinition;
  pRegInfo: IRegistrationInfo;
  pSettings: ITaskSettings;
  pEventTrigger: IEventTrigger;
  pAction: IExecAction;
begin
  pService := CoTaskScheduler_.Create;
  pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  try
    pTaskFolder := pService.GetFolder('\Keyman');
  except
    pTaskFolder := pService.GetFolder('\').CreateFolder('Keyman', EmptyParam);
  end;

  pTask := pService.NewTask(0);
  pService := nil;

  // TODO: create a Keyman folder
  pRegInfo := pTask.RegistrationInfo;
  pRegInfo.Author := 'SIL International';
  pRegInfo.Description := 'Starts Keyman on demand when a Keyman input method is selected';
  pRegInfo.Version := CKeymanVersionInfo.VersionWithTag;
  pRegInfo := nil;

  pSettings := pTask.Settings;
  pSettings.DisallowStartIfOnBatteries := False;
  pSettings.StopIfGoingOnBatteries := False;
  pSettings.ExecutionTimeLimit := 'PT0S';
  pSettings.Priority := 4; // https://github.com/microsoft/WinDev/issues/55

  pEventTrigger := pTask.Triggers.Create(TASK_TRIGGER_EVENT) as IEventTrigger;
  pEventTrigger.Id := 'Keyman_StartOnDemand';
  pEventTrigger.Subscription := CTaskTrigger;

  pAction := pTask.Actions.Create(TASK_ACTION_EXEC) as IExecAction;
  pAction.Path := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
  pAction.Arguments := '-kmc start';

  pTaskFolder.RegisterTaskDefinition(CTaskName, pTask, TASK_CREATE_OR_UPDATE or TASK_IGNORE_REGISTRATION_TRIGGERS,
    EmptyParam, EmptyParam, TASK_LOGON_NONE, EmptyParam);
end;

class procedure TKeymanStartTask.DeleteTask;
var
  pService: ITaskService;
  pTaskFolder: ITaskFolder;
begin
  pService := CoTaskScheduler_.Create;
  pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  try
    pTaskFolder := pService.GetFolder('\Keyman');
  except
    // Assume that the folder doesn't exist
    Exit;
  end;

  try
    pTaskFolder.DeleteTask(CTaskName, 0);
  except
    // We'll assume that the task doesn't exist, and continue
  end;
  if pTaskFolder.GetTasks(0).Count = 0 then
  begin
    pTaskFolder := nil;
    pService.GetFolder('\').DeleteFolder('Keyman', 0);
  end;
end;

class procedure TKeymanStartTask.Recreate;
begin
  // Create or recreate the automatic task to start Keyman if a TIP is selected
  // based on an event in the Event Log
  if not Reg_GetDebugFlag(SRegValue_Flag_UseAutoStartTask, True) then
  begin
    DeleteTask;
    Exit;
  end;

  CreateTask;
end;


end.
