(*
  * Keyman is copyright (C) SIL Global. MIT License.
  *
  * Notes: For the state diagram in mermaid ../BackgroundUpdateStateDiagram.md
*)
unit Keyman.System.UpdateStateMachine;

interface

uses
  System.SysUtils,
  System.Types,
  System.TypInfo,
  Sentry.Client,

  KeymanPaths,
  Keyman.System.ExecutionHistory,
  Keyman.System.UpdateCheckResponse,
  utilkmshell;

type
  EUpdateStateMachine = class(Exception);

  TUpdateState = (usIdle, usUpdateAvailable, usDownloading, usWaitingRestart,
    usInstalling);

  // Forward declaration
  TUpdateStateMachine = class;

  { State Classes Update }

  TStateClass = class of TState;

  TState = class abstract
  private
    bucStateContext: TUpdateStateMachine;
    procedure ChangeState(newState: TStateClass);

  public
    constructor Create(Context: TUpdateStateMachine);
    procedure Enter; virtual; abstract;
    procedure Exit; virtual; abstract;
    procedure HandleCheck; virtual; abstract;
    function  HandleKmShell: Integer; virtual; abstract;
    procedure HandleDownload; virtual; abstract;
    procedure HandleAbort; virtual; abstract;
    procedure HandleInstallNow; virtual; abstract;
    procedure HandleInstallPackages; virtual;
    procedure HandleFirstRun; virtual;
  end;

  { This class also controls the state flow see
    ../BackgroundUpdateStateDiagram.md }
  TUpdateStateMachine = class
  private
    FForce: Boolean;
    FAutomaticUpdate: Boolean;
    FErrorMessage: string;
    FShowErrors: Boolean;

    CurrentState: TState;
    // State object for performance (could lazy create?)

    FStateInstance: array [TUpdateState] of TState;

    function GetState: TStateClass;
    procedure SetState(const Value: TStateClass);
    procedure SetStateOnly(const enumState: TUpdateState);
    function ConvertStateToEnum(const StateClass: TStateClass): TUpdateState;
    function IsCurrentStateAssigned: Boolean;
    procedure RemoveCachedFiles;

    function SetRegistryState(Update: TUpdateState): Boolean;
    function GetAutomaticUpdates: Boolean;
    function SetApplyNow(Value: Boolean): Boolean;
    function GetApplyNow: Boolean;

  protected
    property State: TStateClass read GetState write SetState;

  public
    constructor Create(AForce: Boolean);
    destructor Destroy; override;

    procedure HandleCheck;
    function HandleKmShell: Integer;
    procedure HandleDownload;
    procedure HandleAbort;
    procedure HandleInstallNow;
    procedure HandleInstallPackages;
    procedure HandleFirstRun;
    function CurrentStateName: string;
    (**
     * Checks if Keyman is the WaitingRestartState and that
     * Keyman has not run in this Windows session.
     * The sole purpose is for the calling code then produce
     * a UI to confirm the user wants to continue install.
     *
     * @returns True  if the Keyman is ready to install.
     *)
    function ReadyToInstall: Boolean;
    function IsInstallingState: Boolean;

    property ShowErrors: Boolean read FShowErrors write FShowErrors;
    function CheckRegistryState: TUpdateState;

  end;

implementation

uses

  System.Win.Registry,
  Winapi.Windows,
  Winapi.WinINet,
  ErrorControlledRegistry,

  GlobalProxySettings,
  kmint,
  keymanapi_TLB,
  KeymanMutex,
  Keyman.System.KeymanSentryClient,
  Keyman.System.DownloadUpdate,
  Keyman.System.RemoteUpdateCheck,
  Keyman.System.UpdateCheckStorage,
  KLog,
  RegistryKeys,
  utilexecute,
  utiluac;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';
  kmShellContinue = 0;
  kmShellExit = 1;

  { State Class Memebers }

constructor TState.Create(Context: TUpdateStateMachine);
begin
  inherited Create;
  bucStateContext := Context;
end;

procedure TState.ChangeState(newState: TStateClass);
begin
  bucStateContext.State := newState;
end;

type

  // Derived classes for each state
  IdleState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  UpdateAvailableState = class(TState)
  private
    procedure StartDownloadProcess;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  DownloadingState = class(TState)
  private
    function DownloadUpdatesBackground: Boolean;
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  WaitingRestartState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  InstallingState = class(TState)
  private

  (**
   * Installs the Keyman setup file using separate shell.
   *
   * @params  SavePath  The path to the downloaded files.
   *
   * @returns True  if the installation is successful, False otherwise.
   *)

    function DoInstallKeyman: Boolean; overload;

      (**
     * Installs the Keyman Keyboard files using separate shell.
     *
     * @params  SavePath  The path to the downloaded files.
     *
     * @returns True  if the installation is successful, False otherwise.
     *)

    function DoInstallPackages(Params: TUpdateCheckResponse): Boolean;
    function DoInstallPackage(PackageFileName: String): Boolean;
    procedure LaunchInstallPackageProcess;

  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    procedure HandleInstallPackages; override;
    procedure HandleFirstRun; override;
  end;

  { TUpdateStateMachine }

constructor TUpdateStateMachine.Create(AForce: Boolean);
begin
  inherited Create;
  FShowErrors := True;

  FForce := AForce;
  FAutomaticUpdate := GetAutomaticUpdates;

  FStateInstance[usIdle] := IdleState.Create(Self);
  FStateInstance[usUpdateAvailable] := UpdateAvailableState.Create(Self);
  FStateInstance[usDownloading] := DownloadingState.Create(Self);
  FStateInstance[usWaitingRestart] := WaitingRestartState.Create(Self);
  FStateInstance[usInstalling] := InstallingState.Create(Self);

  // Check the Registry setting.
  SetStateOnly(CheckRegistryState);
end;

destructor TUpdateStateMachine.Destroy;
var
  lpState: TUpdateState;
begin
  if (FErrorMessage <> '') and FShowErrors then
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      '"+FErrorMessage+"');

  for lpState := Low(TUpdateState) to High(TUpdateState) do
  begin
    FreeAndNil(FStateInstance[lpState]);
  end;

  // TODO: #10210 TODO: epic-windows-update remove debugging comments throughout this Unit.

  inherited Destroy;
end;

function TUpdateStateMachine.SetRegistryState(Update: TUpdateState): Boolean;
var
  UpdateStr: string;
  Registry: TRegistryErrorControlled;
begin
  Result := False;
  Registry := TRegistryErrorControlled.Create;

  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if not Registry.OpenKey(SRegKey_KeymanEngine_CU, True) then
    begin
      TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
        'Failed to open registry key: "' + SRegKey_KeymanEngine_CU + '"');
      Exit;
    end;

    try
      UpdateStr := GetEnumName(TypeInfo(TUpdateState), Ord(Update));
      Registry.WriteString(SRegValue_Update_State, UpdateStr);
      Result := True;
    except
      on E: ERegistryException do
      begin
        TKeymanSentryClient.ReportHandledException(E,
          'Failed to write install state machine state');
      end;
    end;

  finally
    Registry.Free;
  end;

end;

function TUpdateStateMachine.CheckRegistryState: TUpdateState;
var
  UpdateState: TUpdateState;
  Registry: TRegistryErrorControlled;
  StateValue: string;
  EnumValue: Integer;
begin
  // Default to Idle state if any issues occur
  UpdateState := usIdle;
  Registry := TRegistryErrorControlled.Create;

  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and
      Registry.ValueExists(SRegValue_Update_State) then
    begin
      try
        StateValue := Registry.ReadString(SRegValue_Update_State);
        EnumValue := GetEnumValue(TypeInfo(TUpdateState), StateValue);

        // Bounds Check EnumValue against TUpdateState
        if (EnumValue >= Ord(Low(TUpdateState))) and
          (EnumValue <= Ord(High(TUpdateState))) then
          UpdateState := TUpdateState(EnumValue)
        else
          UpdateState := usIdle; // Default if out of bounds
      except
        on E: ERegistryException do
        begin
          TKeymanSentryClient.ReportHandledException(E,
            'Failed to read install state machine state');
          UpdateState := usIdle;
        end;
      end;
    end;
  finally
    Registry.Free;
  end;

  Result := UpdateState;
end;

function TUpdateStateMachine.GetAutomaticUpdates: Boolean; // I2329
var
  Registry: TRegistryErrorControlled;

begin
  // check the registry value
  Registry := TRegistryErrorControlled.Create; // I2890
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    try
      Result := not Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) or
        not Registry.ValueExists(SRegValue_CheckForUpdates) or
        Registry.ReadBool(SRegValue_CheckForUpdates);
    except
      on E: ERegistryException do
      begin
        TKeymanSentryClient.ReportHandledException(E,
          'Failed to read automatic updates');
        Result := False;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TUpdateStateMachine.SetApplyNow(Value: Boolean): Boolean;
var
  Registry: TRegistryErrorControlled;
begin
  Result := False;
  Registry := TRegistryErrorControlled.Create;

  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if not Registry.OpenKey(SRegKey_KeymanEngine_CU, True) then
    begin
      Exit;
    end;
    try
      Registry.WriteBool(SRegValue_ApplyNow, Value);
      Result := True;
    except
      on E: ERegistryException do
      begin
        TKeymanSentryClient.ReportHandledException(E,
          'Failed to write "apply now"');
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TUpdateStateMachine.GetApplyNow: Boolean;
var
  Registry: TRegistryErrorControlled;
begin
  // check the registry value
  Registry := TRegistryErrorControlled.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    try
      Result := Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and
        Registry.ValueExists(SRegValue_ApplyNow) and
        Registry.ReadBool(SRegValue_ApplyNow);
    except
      on E: ERegistryException do
      begin
        TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
          'Failed to read registry: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TUpdateStateMachine.GetState: TStateClass;
begin
  if Assigned(CurrentState) then
    Result := TStateClass(CurrentState.ClassType)
  else
  begin
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Error CurrentState was uninitiallised');
    Result := nil;
  end;
end;

procedure TUpdateStateMachine.SetState(const Value: TStateClass);
begin
  if Assigned(CurrentState) then
  begin
    CurrentState.Exit;
  end;

  SetStateOnly(ConvertStateToEnum(Value));

  if Assigned(CurrentState) then
  begin
    CurrentState.Enter;
  end
  else
  begin
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Set CurrentState was failed');
  end;

end;

procedure TUpdateStateMachine.SetStateOnly(const enumState: TUpdateState);
begin
  CurrentState := FStateInstance[enumState];
end;

function TUpdateStateMachine.ConvertStateToEnum(const StateClass: TStateClass)
  : TUpdateState;
begin
  if StateClass = IdleState then
    Result := usIdle
  else if StateClass = UpdateAvailableState then
    Result := usUpdateAvailable
  else if StateClass = DownloadingState then
    Result := usDownloading
  else if StateClass = WaitingRestartState then
    Result := usWaitingRestart
  else if StateClass = InstallingState then
    Result := usInstalling
  else
  begin
    Result := usIdle;
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Unknown State Machine class');
  end;
end;

function TUpdateStateMachine.IsCurrentStateAssigned: Boolean;
begin
  if Assigned(CurrentState) then
    Result := True
  else
  begin
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Error CurrentState was uninitiallised');
    Result := False;
  end;
end;

procedure TUpdateStateMachine.RemoveCachedFiles;
var
  SavePath: string;
  FileName: String;
  FileNames: TStringDynArray;
begin
  SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  GetFileNamesInDirectory(SavePath, FileNames);
  for FileName in FileNames do
  begin
    System.SysUtils.DeleteFile(FileName);
  end;
end;

procedure TUpdateStateMachine.HandleCheck;
begin
  if not IsCurrentStateAssigned then
    Exit;
  CurrentState.HandleCheck;
end;

function TUpdateStateMachine.HandleKmShell: Integer;
begin
  if not IsCurrentStateAssigned then
    Exit(kmShellContinue);
  Result := CurrentState.HandleKmShell;
end;

procedure TUpdateStateMachine.HandleDownload;
begin
  if not IsCurrentStateAssigned then
    Exit;
  CurrentState.HandleDownload;
end;

procedure TUpdateStateMachine.HandleAbort;
begin
  if not IsCurrentStateAssigned then
    Exit;
  CurrentState.HandleAbort;
end;

procedure TUpdateStateMachine.HandleInstallNow;
begin
  if not IsCurrentStateAssigned then
    Exit;
  CurrentState.HandleInstallNow;
end;

procedure TUpdateStateMachine.HandleInstallPackages;
begin
  CurrentState.HandleInstallPackages;
end;

procedure TUpdateStateMachine.HandleFirstRun;
begin
  CurrentState.HandleFirstRun;
end;

function TUpdateStateMachine.CurrentStateName: string;
begin
  if not IsCurrentStateAssigned then
    Exit('Undefined');
  Result := CurrentState.ClassName;
end;

function TUpdateStateMachine.ReadyToInstall: Boolean;
begin
  if not IsCurrentStateAssigned then
    Exit(False);
  if (CurrentState is WaitingRestartState) and not HasKeymanRun then
    Result := True
  else
    Result := False;
end;

function TUpdateStateMachine.IsInstallingState: Boolean;
begin
  Result := (CurrentState is InstallingState);
end;

// base implmentation to be overiden

procedure TState.HandleInstallPackages;
begin
  // Do Nothing
end;

procedure TState.HandleFirstRun;
begin
  // If Handle First run hits base implementation
  // something is wrong.
  TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
    'Handle first run called in state:"' + Self.ClassName + '"');
  bucStateContext.RemoveCachedFiles;
  ChangeState(IdleState);
end;

{ IdleState }

procedure IdleState.Enter;
begin
  // Enter UpdateAvailableState
  bucStateContext.SetRegistryState(usIdle);
end;

procedure IdleState.Exit;
begin

end;

procedure IdleState.HandleCheck;
var
  CheckForUpdates: TRemoteUpdateCheck;
  Result: TRemoteUpdateCheckResult;
begin

  CheckForUpdates := TRemoteUpdateCheck.Create(True);
  try
    Result := CheckForUpdates.Run;
  finally
    CheckForUpdates.Free;
  end;

  { Response OK and Update is available }
  if Result = wucSuccess then
  begin
    ChangeState(UpdateAvailableState);
  end;
  // else staty in idle state
end;

function IdleState.HandleKmShell;
var
  CheckForUpdates: TRemoteUpdateCheck;
  UpdateCheckResult: TRemoteUpdateCheckResult;
begin
  // Remote manages the last check time therefore
  // we will allow it to return early if it hasn't reached
  // the configured time between checks.
  CheckForUpdates := TRemoteUpdateCheck.Create(False);
  try
    UpdateCheckResult := CheckForUpdates.Run;
  finally
    CheckForUpdates.Free;
  end;
  { Response OK and Update is available }
  if UpdateCheckResult = wucSuccess then
  begin
    ChangeState(UpdateAvailableState);
  end;
  Result := kmShellContinue;
end;

procedure IdleState.HandleDownload;
begin
  // Do Nothing
end;

procedure IdleState.HandleAbort;
begin
  // Do Nothing
end;

procedure IdleState.HandleInstallNow;
begin
  // Do Nothing
end;

{ UpdateAvailableState }

procedure UpdateAvailableState.StartDownloadProcess;
var
  FResult: Boolean;
  RootPath: string;
begin
  // call separate process
  RootPath := ExtractFilePath(ParamStr(0));
  FResult := TUtilExecute.ShellCurrentUser(0, ParamStr(0),
    IncludeTrailingPathDelimiter(RootPath), '-bd');
  if not FResult then
  begin
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Executing kmshell process to download updated Failed');
    ChangeState(IdleState);
  end;
end;

procedure UpdateAvailableState.Enter;
begin
  // Enter UpdateAvailableState
  bucStateContext.SetRegistryState(usUpdateAvailable);
  if bucStateContext.FAutomaticUpdate then
  begin
    StartDownloadProcess;
  end;
end;

procedure UpdateAvailableState.Exit;
begin
  // Exit UpdateAvailableState
end;

procedure UpdateAvailableState.HandleCheck;
var
  CheckForUpdates: TRemoteUpdateCheck;
  Result: TRemoteUpdateCheckResult;
begin
  // Check if new updates while in this state
  CheckForUpdates := TRemoteUpdateCheck.Create(True);
  try
    Result := CheckForUpdates.Run;
  finally
    CheckForUpdates.Free;
  end;
  if Result <> wucSuccess then
    begin
      KL.Log('UpdateAvailableState.HandleCheck CheckForUpdates not successful: '+
        GetEnumName(TypeInfo(TUpdateState), Ord(Result)));
    end;
end;

function UpdateAvailableState.HandleKmShell;
begin
  if bucStateContext.FAutomaticUpdate then
  begin
    // we will use a new kmshell process to enable
    // the download as background process.
    StartDownloadProcess;
  end;
  Result := kmShellContinue;
end;

procedure UpdateAvailableState.HandleDownload;
begin
  ChangeState(DownloadingState);
end;

procedure UpdateAvailableState.HandleAbort;
begin

end;

procedure UpdateAvailableState.HandleInstallNow;
begin
  bucStateContext.SetApplyNow(True);
  ChangeState(DownloadingState);
end;

{ DownloadingState }

procedure DownloadingState.Enter;
var
  DownloadResult: Boolean;
  RetryCount: Integer;
  FMutex: TKeymanMutex;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usDownloading);

  RetryCount := 0;
  DownloadResult := False;
  FMutex := TKeymanMutex.Create('KeymanDownloading');
  // Should be impossible but just exit anyway and let the process current
  // downloading process finish.
  if not FMutex.MutexOwned then
  begin
    FreeAndNil(FMutex);
    Exit;
  end;

  while (not DownloadResult) and (RetryCount < 3) do
  begin
    DownloadResult := DownloadUpdatesBackground;
    if not DownloadResult then
      Inc(RetryCount);
  end;

  FreeAndNil(FMutex);

  if (not DownloadResult) then
  begin
    // Failed three times in this process; return to the
    // IdleState to wait 'CheckPeriod' before trying again
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
    'Error Updates not downloaded after 3 attempts');
    bucStateContext.RemoveCachedFiles;
    ChangeState(IdleState);
  end
  else
  begin
    if HasKeymanRun then
    begin
      if bucStateContext.GetApplyNow then
      begin
        bucStateContext.SetApplyNow(False);
        ChangeState(InstallingState);
      end
      else
        ChangeState(WaitingRestartState);
    end
    else
    begin
      ChangeState(InstallingState);
    end;
  end

end;

procedure DownloadingState.Exit;
begin
  // Exit DownloadingState
end;

procedure DownloadingState.HandleCheck;
begin

end;

function DownloadingState.HandleKmShell;
var
  FMutex: TKeymanMutex;
begin
  // Check to ensure a download process is running if not
  // clean up return to the the idle state and check for updates
  FMutex := TKeymanMutex.Create('KeymanDownloading');
  if FMutex.MutexOwned then
  begin
    bucStateContext.RemoveCachedFiles;
    FreeAndNil(FMutex);
    ChangeState(IdleState);
    bucStateContext.CurrentState.HandleCheck;
  end;
  FreeAndNil(FMutex);
  // Downloading state, in another process, so continue to execute kmshell
  Result := kmShellContinue;
end;

procedure DownloadingState.HandleDownload;
var
  FMutex: TKeymanMutex;
begin
  // If downloading process is not running clean files and return to idle
  FMutex := TKeymanMutex.Create('KeymanDownloading');
  if FMutex.MutexOwned then
  begin
    bucStateContext.RemoveCachedFiles;
    FreeAndNil(FMutex);
    ChangeState(IdleState);
    bucStateContext.CurrentState.HandleCheck;
  end;
  FreeAndNil(FMutex);
end;

procedure DownloadingState.HandleAbort;
begin
  // To abort during the downloading
end;

procedure DownloadingState.HandleInstallNow;
begin
  // Already downloading set the registry apply now
  bucStateContext.SetApplyNow(True);
end;

function DownloadingState.DownloadUpdatesBackground: Boolean;
var
  DownloadResult: Boolean;
  DownloadUpdate: TDownloadUpdate;
begin
  DownloadUpdate := TDownloadUpdate.Create;
  try
    DownloadResult := DownloadUpdate.DownloadUpdates;
    Result := DownloadResult;
  finally
    DownloadUpdate.Free;
  end;
end;

{ WaitingRestartState }

procedure WaitingRestartState.Enter;
begin
  // Enter WaitingRestartState
  bucStateContext.SetRegistryState(usWaitingRestart);
end;

procedure WaitingRestartState.Exit;
begin
  // Exit DownloadingState
end;

procedure WaitingRestartState.HandleCheck;
var
  CheckForUpdates: TRemoteUpdateCheck;
  Result: TRemoteUpdateCheckResult;
begin
  // Check if new updates while in this state
  CheckForUpdates := TRemoteUpdateCheck.Create(True);
  try
    Result := CheckForUpdates.Run;
  finally
    CheckForUpdates.Free;
  end;
  { Response OK and go back to update available so files can be downloaded }
  if Result = wucSuccess then
  begin
    ChangeState(UpdateAvailableState);
  end;
end;

function WaitingRestartState.HandleKmShell;
var
  ucr: TUpdateCheckResponse;
  hasPackages, hasKeymanInstall: Boolean;
begin
  // Still can't go if keyman has run
  if HasKeymanRun then
  begin
    Result := kmShellContinue;
  end
  else
  begin
    hasPackages := False;
    hasKeymanInstall := False;
    if (TUpdateCheckStorage.LoadUpdateCacheData(ucr)) then
    begin
      hasPackages := TUpdateCheckStorage.HasKeyboardPackages(ucr);
      hasKeymanInstall := TUpdateCheckStorage.HasKeymanInstallFile(ucr);
    end;
    if not (hasPackages Or hasKeymanInstall) then
    begin
      // Return to Idle state and check for Updates state
      ChangeState(IdleState);
      bucStateContext.CurrentState.HandleCheck;
      Result := kmShellExit;
    end
    else
    begin
      ChangeState(InstallingState);
      Result := kmShellExit;
    end;
  end;
end;

procedure WaitingRestartState.HandleDownload;
begin

end;

procedure WaitingRestartState.HandleAbort;
begin
   ChangeState(UpdateAvailableState);
end;

procedure WaitingRestartState.HandleInstallNow;
begin
  bucStateContext.SetApplyNow(True);
  ChangeState(InstallingState);
end;

// Installing packages needs to be elevated
procedure InstallingState.LaunchInstallPackageProcess;
var
  executeResult: Cardinal;
begin
  if not kmcom.SystemInfo.IsAdministrator then
  begin
    if CanElevate then
    begin
      executeResult := WaitForElevatedConfiguration(0, '-ikp');
      if (executeResult <> 0) then
      begin
        TKeymanSentryClient.Client.MessageEvent
          (Sentry.Client.SENTRY_LEVEL_ERROR,
          'Executing kmshell process to install keyboard packages failed:"' +
          IntToStr(Ord(executeResult)) + '"');
        ChangeState(IdleState);
      end;
    end
    else
    begin
      // TODO: epic-windows-updates How do we alert the user that package requires a user with admin rights
      // ShowMessage('Some of these updates require an Administrator to complete installation.  Please login as an Administrator and re-run the update.');
    end;
  end
  else
  begin
    HandleInstallPackages; // can install packages straight away
  end;
end;

function InstallingState.DoInstallKeyman: Boolean;
var
  FResult: Boolean;
  SavePath: String;
  fileExt: String;
  FileName: String;
  FileNames: TStringDynArray;
  found: Boolean;
begin

  SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  GetFileNamesInDirectory(SavePath, FileNames);
  found := False;
  for FileName in FileNames do
  begin
    fileExt := LowerCase(ExtractFileExt(FileName));
    if fileExt = '.exe' then
    begin
      found := True;
      break;
    end;
  end;

  // switch -au for auto update in silent mode.
  // We will need to add the pop up that says install update now yes/no
  // This will run the setup executable which will ask for  elevated permissions
  if found then
    FResult := TUtilExecute.Shell(0, SavePath + ExtractFileName(FileName),
      '', '-au')
  else
    FResult := False;

  if not FResult then
  begin
    bucStateContext.RemoveCachedFiles;
    TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Executing kmshell process to install failed:"' +
      IntToStr(Ord(FResult)) + '"');
    ChangeState(IdleState);
  end;

  Result := FResult;
end;

function InstallingState.DoInstallPackage(PackageFileName: String): Boolean;
var
  FPackage: IKeymanPackageFile2;
begin
  Result := False;
  try
    FPackage := kmcom.Packages.GetPackageFromFile(PackageFileName)
      as IKeymanPackageFile2;
      // Force overwrites existing package and leaves most settings for it intact
    FPackage.Install2(True);
    Result := True;
  except
    on E:Exception do
    begin
      TKeymanSentryClient.ReportHandledException(E,
        'Failed to install keyboard package');
    end;
  end;

  FPackage := nil;
  if Result then
  begin
    kmcom.Refresh;
    kmcom.Apply;
    System.SysUtils.DeleteFile(PackageFileName);
  end;

end;

function InstallingState.DoInstallPackages
  (Params: TUpdateCheckResponse): Boolean;
var
  i: Integer;
  SavePath: String;
  PackageFullPath: String;
begin
  SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  for i := 0 to High(Params.Packages) do
  begin
    PackageFullPath := SavePath + Params.Packages[i].FileName;
    if not FileExists(PackageFullPath) then
    begin
      Continue;
    end;

    if not DoInstallPackage(PackageFullPath) then // I2742
    begin
      TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
      'Installing Package failed"' + PackageFullPath + '"');
    end;
  end;
  Result := True;
end;

procedure InstallingState.Enter;
var
  ucr: TUpdateCheckResponse;
  hasPackages, hasKeymanInstall: Boolean;
begin

  hasPackages := False;
  hasKeymanInstall := False;
  bucStateContext.SetRegistryState(usInstalling);

  if (TUpdateCheckStorage.LoadUpdateCacheData(ucr)) then
  begin
    hasPackages := TUpdateCheckStorage.HasKeyboardPackages(ucr);
    hasKeymanInstall := TUpdateCheckStorage.HasKeymanInstallFile(ucr);
  end;
  { Notes: The reason packages (keyboards) is installed first is
  because we are trying to reduce the number of times the user has
  to be asked to elevate to admin or restart. Keyboard installation always
  needs elevation, when we do that and execute kmshell as an elevated process
  we can then launch the Keyman installer and it will not need
  to ask for elevation. }
  if hasPackages then
  begin
    LaunchInstallPackageProcess;
    Exit;
  end;
  // If no packages then install Keyman now
  if hasKeymanInstall then
  begin
    DoInstallKeyman;
    Exit;
  end;
  // unexpected: should have had either packages or a keyman file
  bucStateContext.RemoveCachedFiles;
  ChangeState(IdleState);
end;

procedure InstallingState.Exit;
begin

end;

procedure InstallingState.HandleCheck;
begin

end;

function InstallingState.HandleKmShell;
begin
  // Should not be possible while called in InstallingState. The MSI installer may have
  // failed. Clean Up and return to Idle
  bucStateContext.RemoveCachedFiles;
  ChangeState(IdleState);
  Result := kmShellContinue;
end;

procedure InstallingState.HandleDownload;
begin

end;

procedure InstallingState.HandleAbort;
begin
   // To late as MSI is installing
end;

procedure InstallingState.HandleInstallNow;
begin
  // Do Nothing. Need the UI to let user know installation in progress OR
end;

procedure InstallingState.HandleInstallPackages;
var
  ucr: TUpdateCheckResponse;
  hasKeymanInstall : Boolean;
begin
  TUpdateCheckStorage.LoadUpdateCacheData(ucr);
  hasKeymanInstall := TUpdateCheckStorage.HasKeymanInstallFile(ucr);
  // This event should only be reached in elevated process if not then
  // move on to just installing Keyman
  if not kmcom.SystemInfo.IsAdministrator then
  begin
    if hasKeymanInstall then
      DoInstallKeyman;
    Exit;
  end;

  if (TUpdateCheckStorage.LoadUpdateCacheData(ucr)) then
  begin
    DoInstallPackages(ucr);
  end;

  if hasKeymanInstall then
    DoInstallKeyman;
end;

procedure InstallingState.HandleFirstRun;
begin
  bucStateContext.RemoveCachedFiles;
  ChangeState(IdleState);
end;

end.
