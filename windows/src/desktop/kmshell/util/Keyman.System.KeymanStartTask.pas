unit Keyman.System.KeymanStartTask;

interface

type
  /// <summary>Create or recreate the automatic task to start Keyman if a TIP
  /// is selected based on an event in the Event Log</summary>
  TKeymanStartTask = class
  private
  public
    class procedure RecreateTask;
    class procedure DeleteTask;
    class procedure CreateTask;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.Windows,

  DebugPaths,
  KeymanPaths,
  KeymanVersion,
  RegistryKeys,
  Keyman.System.KeymanSentryClient,
  Sentry.Client,
  TaskScheduler_TLB;

{ TKeymanStartTask }

const
  CTaskName = 'Start On Demand';
  CTaskDescription = 'Starts Keyman on demand when a Keyman input method is selected';
  CTaskAuthor = 'SIL International';
  CTaskFolderName = 'Keyman';
  CTaskTrigger =
    '<QueryList>'+
      '<Query Id="0" Path="Application">'+
        '<Select Path="Application">*[System[Provider[@Name=''Keyman''] and EventID=256]]</Select>'+
      '</Query>'+
    '</QueryList>';
  CTaskEventTriggerId = 'Keyman_StartOnDemand';

  CKeymanExeStartArguments = '-kmc start';

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
  try
    pService := CoTaskScheduler_.Create;
    pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);

    // Create or open the Keyman task folder
    try
      pTaskFolder := pService.GetFolder('\' + CTaskFolderName);
    except
      on E:EOleException do
      begin
        // Don't report if the folder doesn't exist, because we need to create it
        if E.ErrorCode <> HResultFromWin32(ERROR_FILE_NOT_FOUND) then
          TKeymanSentryClient.ReportHandledException(E, 'Failed to get task folder');

        try
          pTaskFolder := pService.GetFolder('\').CreateFolder(CTaskFolderName, EmptyParam);
        except
          on E:EOleException do
          begin
            // Don't report if we might have a race (someone else created it at
            // the same time we did)
            if E.ErrorCode <> HResultFromWin32(ERROR_ALREADY_EXISTS) then
              TKeymanSentryClient.ReportHandledException(E, 'Failed to create task folder');

            // well let's try one last time to get the folder
            try
              pTaskFolder := pService.GetFolder('\' + CTaskFolderName);
            except
              on E:EOleException do
              begin
                TKeymanSentryClient.ReportHandledException(E, 'Failed to get task folder, second try');
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;

    // Create a new task
    pTask := pService.NewTask(0);

    pRegInfo := pTask.RegistrationInfo;
    pRegInfo.Author := CTaskAuthor;
    pRegInfo.Description := CTaskDescription;
    pRegInfo.Version := CKeymanVersionInfo.VersionWithTag;

    pSettings := pTask.Settings;
    pSettings.DisallowStartIfOnBatteries := False;
    pSettings.StopIfGoingOnBatteries := False;
    pSettings.ExecutionTimeLimit := 'PT0S'; // no time limit
    pSettings.Priority := 4; // https://github.com/microsoft/WinDev/issues/55

    // Start the task when Keyman event 256 is logged
    pEventTrigger := pTask.Triggers.Create(TASK_TRIGGER_EVENT) as IEventTrigger;
    pEventTrigger.Id := CTaskEventTriggerId;
    pEventTrigger.Subscription := CTaskTrigger;

    // Action is to run keyman.exe as login user
    pAction := pTask.Actions.Create(TASK_ACTION_EXEC) as IExecAction;
    pAction.Path := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);
    pAction.Arguments := CKeymanExeStartArguments;

    pTaskFolder.RegisterTaskDefinition(CTaskName, pTask, TASK_CREATE_OR_UPDATE or TASK_IGNORE_REGISTRATION_TRIGGERS,
      EmptyParam, EmptyParam, TASK_LOGON_NONE, EmptyParam);
  except
    on E:Exception do
      TKeymanSentryClient.ReportHandledException(E, 'Failed to create task');
  end;
end;

class procedure TKeymanStartTask.DeleteTask;
var
  pService: ITaskService;
  pTaskFolder: ITaskFolder;
begin
  try
    pService := CoTaskScheduler_.Create;
    pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
    try
      pTaskFolder := pService.GetFolder('\'+CTaskFolderName);
    except
      on E:EOleException do
      begin
        if E.ErrorCode = HResultFromWin32(ERROR_FILE_NOT_FOUND) then
          // Assume that the folder doesn't exist, nothing to do
          Exit;
      end;
    end;

    try
      pTaskFolder.DeleteTask(CTaskName, 0);
    except
      on E:EOleException do
      begin
        if E.ErrorCode = HResultFromWin32(ERROR_FILE_NOT_FOUND) then
          // Assume that the task doesn't exist, nothing to do
          Exit;
      end;
    end;

    if pTaskFolder.GetTasks(0).Count = 0 then
    begin
      pTaskFolder := nil;
      pService.GetFolder('\').DeleteFolder(CTaskFolderName, 0);
    end;
  except
    on E:Exception do
      TKeymanSentryClient.ReportHandledException(E, 'Failed to delete task');
  end;
end;

class procedure TKeymanStartTask.RecreateTask;
begin
  if not Reg_GetDebugFlag(SRegValue_Flag_UseAutoStartTask, True) then
  begin
    DeleteTask;
    Exit;
  end;

  CreateTask;
end;


end.
