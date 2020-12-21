unit Keyman.System.KeymanStartTask;

interface

uses
  TaskScheduler_TLB;

type
  /// <summary>Create or recreate the automatic task to start Keyman if a TIP
  /// is selected based on an event in the Event Log</summary>
  TKeymanStartTask = class
  private
    class function GetTaskName: string;
    class procedure CleanupAlphaTasks(pTaskFolder: ITaskFolder); static;
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
  Sentry.Client,
  Keyman.System.KeymanSentryClient;


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

  CKmshellExeStartArguments = '-s';

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

    CleanupAlphaTasks(pTaskFolder);

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
    pAction.Path := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
    pAction.Arguments := CKMShellExeStartArguments;

    pTaskFolder.RegisterTaskDefinition(GetTaskName, pTask, TASK_CREATE_OR_UPDATE or TASK_IGNORE_REGISTRATION_TRIGGERS,
      EmptyParam, EmptyParam, TASK_LOGON_INTERACTIVE_TOKEN, EmptyParam);
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'Failed to create task');
    end;
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
        raise;
      end;
    end;

    try
      pTaskFolder.DeleteTask(GetTaskName, 0);
    except
      on E:EOleException do
      begin
        if E.ErrorCode <> HResultFromWin32(ERROR_FILE_NOT_FOUND) then
          // We ignore when the task doesn't exist, but report other errors
          TKeymanSentryClient.ReportHandledException(E, 'Failed to delete task '+GetTaskName);
      end;
    end;

    CleanupAlphaTasks(pTaskFolder);

    if pTaskFolder.GetTasks(0).Count = 0 then
    begin
      pTaskFolder := nil;
      try
        pService.GetFolder('\').DeleteFolder(CTaskFolderName, 0);
      except
        // We'll ignore errors here because other users may have created the
        // task on their accounts
      end;
    end;
  except
    on E:Exception do
      TKeymanSentryClient.ReportHandledException(E, 'Failed to delete task');
  end;
end;

class function TKeymanStartTask.GetTaskName: string;
begin
  Result := CTaskName + ' - ' + GetEnvironmentVariable('USERNAME');
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

class procedure TKeymanStartTask.CleanupAlphaTasks(pTaskFolder: ITaskFolder);
var
  tasks: IRegisteredTaskCollection;
  i: Integer;
begin
  // Cleanup earlier alpha-version tasks: we renamed the task to include the
  // user's login name in build 14.0.194 of Keyman, so that multiple users could
  // create the task on the same machine. It's not obvious, but the Tasks
  // namespace is shared between all users -- non-admin users will not be able
  // to see or overwrite tasks created by other users; they'd just get a
  // (silent) access denied result.
  try
    tasks := pTaskFolder.GetTasks(0);
    for i := 1 to tasks.Count do
      if SameText(tasks.Item[i].Name, CTaskName) then
      begin
        tasks := nil;
        pTaskFolder.DeleteTask(CTaskName, 0);
        Exit;
      end;
  except
    on E:EOleException do
    begin
      if (E.ErrorCode <> HResultFromWin32(ERROR_FILE_NOT_FOUND)) and
        (E.ErrorCode <> HResultFromWin32(ERROR_ACCESS_DENIED)) then
        // We ignore when the task doesn't exist, but report other errors
        TKeymanSentryClient.ReportHandledException(E, 'Failed to delete task '+CTaskName);
    end;
  end;
end;


end.
