(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Notes: For the state diagram in mermaid ../BackgroundUpdateStateDiagram.md
 *)
unit Keyman.System.UpdateStateMachine;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.IOUtils,
  System.Types,
  System.TypInfo,

  httpuploader,
  KeymanPaths,
  Keyman.Configuration.UI.UfrmStartInstall,
  Keyman.Configuration.UI.UfrmStartInstallNow,
  Keyman.System.ExecutionHistory,
  Keyman.System.UpdateCheckResponse,
  utilkmshell;

type
  EUpdateStateMachine = class(Exception);

  TUpdateState = (usIdle, usUpdateAvailable, usDownloading, usWaitingRestart,
    usInstalling, usRetry, usPostInstall);

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
    function CurrentStateName: string;

    property ShowErrors: Boolean read FShowErrors write FShowErrors;
    function CheckRegistryState: TUpdateState;

  end;

implementation

uses

  Winapi.Windows,
  Winapi.WinINet,
  ErrorControlledRegistry,

  GlobalProxySettings,
  Keyman.System.DownloadUpdate,
  Keyman.System.RemoteUpdateCheck,
  KLog,
  RegistryKeys,
  utilexecute;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';
  kmShellContinue = 0;
  kmShellExit = 1;

  { State Class Memebers }

constructor TState.Create(Context: TUpdateStateMachine);
begin
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
    function  HandleKmShell: Integer; override;
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

  function DoInstallKeyman(SavePath: string): Boolean; overload;

  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  RetryState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
  end;

  PostInstallState = class(TState)
  private
    procedure HandleMSIInstallComplete;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell: Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
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
  FStateInstance[usRetry] := RetryState.Create(Self);
  FStateInstance[usPostInstall] := PostInstallState.Create(Self);

  // Check the Registry setting.
  SetStateOnly(CheckRegistryState);
end;

destructor TUpdateStateMachine.Destroy;
var
  lpState: TUpdateState;
begin
  if (FErrorMessage <> '') and FShowErrors then
    KL.Log(FErrorMessage); // TODO: #10210 Log to Sentry

  for lpState := Low(TUpdateState) to High(TUpdateState) do
  begin
    FStateInstance[lpState].Free;
  end;

  // TODO: #10210 remove debugging comments
  // KL.Log('TUpdateStateMachine.Destroy: FErrorMessage = '+FErrorMessage);
  // KL.Log('TUpdateStateMachine.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

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
      // TODO: #10210 Log to Sentry
      KL.Log('Failed to open registry key: ' + SRegKey_KeymanEngine_CU);
      Exit;
    end;

    try
      UpdateStr := GetEnumName(TypeInfo(TUpdateState), Ord(Update));
      Registry.WriteString(SRegValue_Update_State, UpdateStr);
      Result := True;
    except
      on E: Exception do
      begin
        // TODO: #10210 Log to Sentry
        KL.Log('Failed to write to registry: ' + E.Message);
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
        if (EnumValue >= Ord(Low(TUpdateState))) and (EnumValue <= Ord(High(TUpdateState))) then
          UpdateState := TUpdateState(EnumValue)
        else
          UpdateState := usIdle; // Default if out of bounds
      except
        on E: Exception do
        begin
          // TODO: #10210 Log to Sentry
          KL.Log('Failed to write to registry: ' + E.Message);
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
        not Registry.ValueExists(SRegValue_AutomaticUpdates) or
        Registry.ReadBool(SRegValue_AutomaticUpdates);
      except
        on E: Exception do
        begin
          // TODO: #10210 Log to Sentry
          KL.Log('Failed to read registery: ' + E.Message);
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
      on E: Exception do
      begin
        // TODO: #10210 Log to Sentry 'Failed to write '+SRegValue_ApplyNow+' to registry: ' + E.Message
        KL.Log('Failed to write to registry: ' + E.Message);
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
    on E: Exception do
      begin
        KL.Log('Failed to read registry: ' + E.Message);
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
    // TODO: #10210 Log to Sentry
    KL.Log('Error CurrentState was uninitiallised: ' );
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
    // TODO: #10210 Error log for Unable to set state for Value
  end;

end;

procedure TUpdateStateMachine.SetStateOnly(const enumState: TUpdateState);
begin
  CurrentState := FStateInstance[enumState];
end;

function TUpdateStateMachine.ConvertStateToEnum(const StateClass: TStateClass) : TUpdateState;
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
  else if StateClass = RetryState then
    Result := usRetry
  else if StateClass = PostInstallState then
    Result := usPostInstall
  else
  begin
    // TODO: #10210 Log to Sentry
    Result := usIdle;
    KL.Log('Unknown StateClass'); // TODO-WINDOWS-UPDATES
  end;
end;

function TUpdateStateMachine.IsCurrentStateAssigned: Boolean;
begin
  if Assigned(CurrentState) then
    Result := True
  else
  begin
    // TODO: #10210 Log to Sentry
    KL.Log('Unexpected Error: Current state is not assigned.');
    Result := False;
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

function TUpdateStateMachine.CurrentStateName: string;
begin
  if not IsCurrentStateAssigned then
    Exit('Undefined');
  Result := CurrentState.ClassName;
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
  Result : TRemoteUpdateCheckResult;
begin

  { ##### For Testing only just advancing to downloading #### }
  ChangeState(UpdateAvailableState);
  { #### End of Testing ### };

  { Make a HTTP request out and see if updates are available for now do
    this all in the Idle HandleCheck message. But could be broken into an
    seperate state of WaitngCheck RESP }
  { if Response not OK stay in the idle state and return }


  // If handle check event force check
  //  CheckForUpdates := TRemoteUpdateCheck.Create(True);
  //  try
  //    Result:= CheckForUpdates.Run;
  //  finally
  //   CheckForUpdates.Free;
  //  end;

  { Response OK and Update is available }
  // if Result = wucSuccess then
  // begin
  // ChangeState(UpdateAvailableState);
  // end;

  // else staty in idle state
end;

function IdleState.HandleKmShell;
var
  CheckForUpdates: TRemoteUpdateCheck;
  UpdateCheckResult: TRemoteUpdateCheckResult;
begin
  // Remote manages the last check time therfore
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

end;

procedure IdleState.HandleInstallNow;
begin
  bucStateContext.CurrentState.HandleCheck;
  // TODO: How do we notify the command line no update available
end;

{ UpdateAvailableState }

procedure UpdateAvailableState.StartDownloadProcess;
var
  FResult: Boolean;
  RootPath: string;
begin
  // call seperate process
  RootPath := ExtractFilePath(ParamStr(0));
  FResult := TUtilExecute.ShellCurrentUser(0, ParamStr(0), IncludeTrailingPathDelimiter(RootPath), '-bd');
  if not FResult then
    // TODO: #10210 Log to Sentry
    KL.Log('TrmfMain: Executing KMshell for download updated Failed');
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
begin

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
var
  frmStartInstallNow: TfrmStartInstallNow;
  InstallNow: Boolean;
begin

  InstallNow := True;
  if HasKeymanRun then
  begin
    // TODO: UI and non-UI units should be split
    // if the unit launches UI then it should be a .UI. unit
    // https://github.com/keymanapp/keyman/pull/12375/files#r1751041747
    frmStartInstallNow := TfrmStartInstallNow.Create(nil);
    try
      if frmStartInstallNow.ShowModal = mrOk then
        InstallNow := True
      else
        InstallNow := False;
    finally
      frmStartInstallNow.Free;
    end;
  end;
  // If user decides NOT to install now stay in UpdateAvailable State
  if InstallNow = True then
  begin
    bucStateContext.SetApplyNow(True);
    ChangeState(InstallingState)
  end;

end;

{ DownloadingState }

procedure DownloadingState.Enter;
var
  DownloadResult: Boolean;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usDownloading);
  { ##  for testing log that we would download }
  KL.Log('DownloadingState.HandleKmshell test code continue');
  DownloadResult := True;
  { End testing }
  DownloadResult := DownloadUpdatesBackground;
  // TODO check if keyman is running then send to Waiting Restart
  if DownloadResult then
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
      bucStateContext.SetApplyNow(False);
      ChangeState(InstallingState);
    end;
  end
  else
  begin
    ChangeState(RetryState);
  end;

end;

procedure DownloadingState.Exit;
begin
  // Exit DownloadingState
end;

procedure DownloadingState.HandleCheck;
begin

end;

function DownloadingState.HandleKmShell;
begin
  // Downloading state, in other process, so continue
  Result := kmShellContinue;
end;

procedure DownloadingState.HandleDownload;
begin
  // Enter Already Downloading
end;

procedure DownloadingState.HandleAbort;
begin
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
    // #TODO: #10210 workout when we need to refresh kmcom keyboards
    // if Result in [ wucSuccess] then
    // begin
    // kmcom.Keyboards.Refresh;
    // kmcom.Keyboards.Apply;
    // kmcom.Packages.Refresh;
    // end;
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
begin

end;

function WaitingRestartState.HandleKmShell;
var
  SavedPath: String;
  Filenames: TStringDynArray;
  frmStartInstall: TfrmStartInstall;
begin
  // Still can't go if keyman has run
  if HasKeymanRun then
  begin
    Result := kmShellContinue;
    // Exit; // Exit is not wokring for some reason.
    // this else is only here because the exit is not working.
  end
  else
  begin
    // Check downloaded cache if available then
    SavedPath := IncludeTrailingPathDelimiter
      (TKeymanPaths.KeymanUpdateCachePath);
    GetFileNamesInDirectory(SavedPath, Filenames);
    if Length(Filenames) = 0 then
    begin
      // Return to Idle state and check for Updates state
      ChangeState(IdleState);
      bucStateContext.CurrentState.HandleCheck; // TODO no event here
      Result := kmShellExit;
      // Exit; // again exit was not working
    end
    else
    begin
      // TODO Pop up toast here to ask user if we want to continue
      frmStartInstall := TfrmStartInstall.Create(nil);
      try
        if frmStartInstall.ShowModal = mrOk then
        begin
          ChangeState(InstallingState);
          Result := kmShellExit;
        end
        else
          Result := kmShellContinue;
      finally
        frmStartInstall.Free;
      end;
    end;
  end;
end;

procedure WaitingRestartState.HandleDownload;
begin

end;

procedure WaitingRestartState.HandleAbort;
begin

end;

procedure WaitingRestartState.HandleInstallNow;
// If user decides not to install now stay in WaitingRestart State
var
  frmStartInstallNow: TfrmStartInstallNow;
  InstallNow: Boolean;
begin
  InstallNow := True;
  if HasKeymanRun then
  begin
    frmStartInstallNow := TfrmStartInstallNow.Create(nil);
    try
      if frmStartInstallNow.ShowModal = mrOk then
        InstallNow := True
      else
        InstallNow := False;
    finally
      frmStartInstallNow.Free;
    end;
  end;
  if InstallNow = True then
  begin
    bucStateContext.SetApplyNow(True);
    ChangeState(InstallingState);
  end;
end;

function InstallingState.DoInstallKeyman(SavePath: string): Boolean;
var
  s: string;
  FResult: Boolean;
begin
  s := LowerCase(ExtractFileExt(SavePath));
  if s = '.msi' then
    FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /i "' + SavePath +
      '" AUTOLAUNCHPRODUCT=1') // I3349
  else if s = '.exe' then
  begin
    // switch -au for auto update in silent mode.
    // We will need to add the pop up that says install update now yes/no
    // This will run the setup executable which will ask for  elevated permissions
    FResult := TUtilExecute.Shell(0, SavePath, '', '-au') // I3349
  end
  else
    FResult := False;

  if not FResult then
  begin
    // TODO: #10210 Log to Sentry
    KL.Log('TUpdateStateMachine.InstallingState.DoInstall: Result = ' +
      IntToStr(Ord(FResult)));
    // Log message ShowMessage(SysErrorMessage(GetLastError));
  end;

  Result := FResult;
end;

procedure InstallingState.Enter;
var
  SavePath: String;
  fileExt: String;
  fileName: String;
  Filenames: TStringDynArray;
begin

  bucStateContext.SetRegistryState(usInstalling);
  SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);

  GetFileNamesInDirectory(SavePath, Filenames);
  // for now we only want the exe although excute install can
  // handle msi
  for fileName in Filenames do
  begin
    fileExt := LowerCase(ExtractFileExt(fileName));
    if fileExt = '.exe' then
      break;
  end;

  if DoInstallKeyman(SavePath + ExtractFileName(fileName)) then
  begin
    KL.Log('TUpdateStateMachine.InstallingState.Enter: DoInstall OK');
  end
  else
  begin
    // TODO: #10210 clean failed download
    // TODO: #10210 Do we do a retry on install? probably not
    KL.Log('TUpdateStateMachine.InstallingState.Enter: DoInstall fail');
    ChangeState(IdleState);
  end
end;

procedure InstallingState.Exit;
begin

end;

procedure InstallingState.HandleCheck;
begin

end;

function InstallingState.HandleKmShell;
begin
  // Result = exit straight away as we are installing (MSI installer)
  // need to just do a no-op keyman will it maybe using kmshell to install
  // packages.
  Result := kmShellContinue;
end;

procedure InstallingState.HandleDownload;
begin

end;

procedure InstallingState.HandleAbort;
begin
  ChangeState(IdleState);
end;

procedure InstallingState.HandleInstallNow;
begin
  // Do Nothing. Need the UI to let user know installation in progress OR
end;

{ RetryState }

procedure RetryState.Enter;
begin
  bucStateContext.SetRegistryState(usRetry);
end;

procedure RetryState.Exit;
begin

end;

procedure RetryState.HandleCheck;
begin

end;

function RetryState.HandleKmShell;
begin
  // #TODO: #10210 Implement retry
  Result := kmShellContinue
end;

procedure RetryState.HandleDownload;
begin

end;

procedure RetryState.HandleAbort;
begin

end;

procedure RetryState.HandleInstallNow;
begin
  // TODO: #10038 handle retry counts
  ChangeState(InstallingState);
end;

{ PostInstallState }

procedure PostInstallState.Enter;
begin
  // Enter downloading state
  bucStateContext.SetRegistryState(usPostInstall);
end;

procedure PostInstallState.Exit;
begin

end;

procedure PostInstallState.HandleCheck;
begin
  // Handle Check
end;

function PostInstallState.HandleKmShell;
begin
  HandleMSIInstallComplete;
  Result := kmShellContinue;
end;

procedure PostInstallState.HandleDownload;
begin
  // Do Nothing
end;

procedure PostInstallState.HandleMSIInstallComplete;
var
  SavePath: string;
  fileName: String;
  Filenames: TStringDynArray;
begin
  SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);

  GetFileNamesInDirectory(SavePath, Filenames);
  for fileName in Filenames do
  begin
    System.SysUtils.DeleteFile(fileName);
  end;
  ChangeState(IdleState);
end;

procedure PostInstallState.HandleAbort;
begin
  // Handle Abort
end;

procedure PostInstallState.HandleInstallNow;
begin
  // Do nothing as files will be cleaned via HandleKmShell
end;

end.
