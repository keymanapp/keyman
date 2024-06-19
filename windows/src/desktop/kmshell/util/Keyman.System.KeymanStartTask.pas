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
  {$IF FALSE}
    //Disabled to avoid hint during build; re-enable if wanting to
    //re-establish the task model
    class procedure DeleteTask;
  {$ENDIF}
    class procedure CreateTask;
  public
    class procedure RecreateTask;
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
  UserName: OleVariant;
  UserNameBuf: array[0..64] of Char;
  UserNameBufLen: DWORD;
begin
  try
    pService := CoTaskScheduler_.Create;
    pService.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'Failed to connect to task scheduler');
      Exit;
    end;
  end;

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

  try
    // Create a new task
    pTask := pService.NewTask(0);
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'Failed to generate a new task');
      Exit;
    end;
  end;

  try
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

    UserNameBufLen := SizeOf(UserNameBuf) div SizeOf(UserNameBuf[0]);
    if GetUserName(UserNameBuf, UserNameBufLen)
      then UserName := string(userNameBuf)
      else UserName := EmptyParam;

  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'Failed to prepare task parameters');
      Exit;
    end;
  end;

  try
    pTaskFolder.RegisterTaskDefinition(GetTaskName, pTask, TASK_CREATE_OR_UPDATE or TASK_IGNORE_REGISTRATION_TRIGGERS,
      UserName, EmptyParam, TASK_LOGON_INTERACTIVE_TOKEN, EmptyParam);
  except
    on E:EOleException do
    begin
      if E.ErrorCode <> HResultFromWin32(ERROR_ACCESS_DENIED) then
      begin
        // We don't report on ERROR_ACCESS_DENIED. This can arise if the task is
        // originally created by an elevated instance of kmshell.exe, and then
        // later an instance with standard privileges attempts to re-create it.
        // That is not a problem, as the task will still work in either case.
        // Note that in either case, the uninstall will clean up the tasks
        // successfully.
        TKeymanSentryClient.ReportHandledException(E, 'Failed to create task');
      end;
    end;
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E, 'Failed to create task');
    end;
  end;
end;

{$IF FALSE}
// Disabled to avoid hint during build; re-enable if wanting to re-establish
// the task model
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
{$ENDIF}

class function TKeymanStartTask.GetTaskName: string;
begin
  Result := CTaskName + ' - ' + GetEnvironmentVariable('USERNAME');
end;

class procedure TKeymanStartTask.RecreateTask;
begin
  if not Reg_GetDebugFlag(SRegValue_Flag_UseAutoStartTask, False) then
    Exit;

  CreateTask;
end;

end.
