(*
  Name:             UpdateStateMachine
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      2 Nov 2023

  Modified Date:    2 Nov 2023
  Authors:          rcruickshank
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes: For the state diagram in mermaid ../BackgroundUpdateStateDiagram.md
  History:
*)
unit Keyman.System.UpdateStateMachine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.IOUtils,
  System.Types,
  Vcl.Forms,
  TypInfo,
  KeymanPaths,
  utilkmshell,

  httpuploader,
  Keyman.System.UpdateCheckResponse,
  UfrmStartInstall,
  UfrmStartInstallNow,
  Keyman.System.ExecutionHistory,
  UfrmDownloadProgress;

const
  CheckPeriod: Integer = 7; // Days between checking for updates

type
  EUpdateStateMachine = class(Exception);

  TUpdateStateMachineResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucUpdatesAvailable, oucFailure, oucOffline);

  TUpdateState = (usIdle, usUpdateAvailable, usDownloading, usWaitingRestart, usInstalling, usRetry, usPostInstall);

  { Keyboard Package Params }
  TUpdateStateMachineParamsPackage = record
    ID: string;
    NewID: string;
    Description: string;
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;
  { Main Keyman Program }
  TUpdateStateMachineParamsKeyman = record
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TUpdateStateMachineParams = record
    Keyman: TUpdateStateMachineParamsKeyman;
    Packages: array of TUpdateStateMachineParamsPackage;
    Result: TUpdateStateMachineResult;
  end;

  TUpdateStateMachineDownloadParams = record
    Owner: TfrmDownloadProgress;
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

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
    function  HandleKmShell : Integer; virtual; abstract;
    procedure HandleDownload; virtual; abstract;
    procedure HandleAbort; virtual; abstract;
    procedure HandleInstallNow; virtual; abstract;

    // For convenience
    function StateName: string; virtual; abstract;

  end;

  // Derived classes for each state
  IdleState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  UpdateAvailableState = class(TState)
  private
    procedure StartDownloadProcess;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  DownloadingState = class(TState)
  private

    function DownloadUpdatesBackground: Boolean;
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  WaitingRestartState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  InstallingState = class(TState)
  private
    procedure DoInstallKeyman; overload;
    function DoInstallKeyman(SavePath: string) : Boolean; overload;
    {
      Installs the Keyman file using either msiexec.exe or the setup launched in
      a separate shell.

      @params  Package  The package to be installed.

      @returns True  if the installation is successful, False otherwise.
    }
    function DoInstallPackage(Package: TUpdateStateMachineParamsPackage): Boolean;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  RetryState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;

  PostInstallState = class(TState)
  private
      procedure HandleMSIInstallComplete;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    function  HandleKmShell : Integer; override;
    procedure HandleDownload; override;
    procedure HandleAbort; override;
    procedure HandleInstallNow; override;
    function StateName: string; override;
  end;


  { This class also controls the state flow see  }
  TUpdateStateMachine = class
  private
    FForce: Boolean;
    FAutomaticUpdate: Boolean;
    FParams: TUpdateStateMachineParams;
    FErrorMessage: string;
    DownloadTempPath: string;
    FShowErrors: Boolean;
    FDownload: TUpdateStateMachineDownloadParams;

    CurrentState: TState;
    // State object for performance (could lazy create?)
    FIdle: IdleState;
    FUpdateAvailable: UpdateAvailableState;
    FDownloading: DownloadingState;
    FWaitingRestart: WaitingRestartState;
    FInstalling: InstallingState;
    FRetry: RetryState;
    FPostInstall: PostInstallState;
    function GetState: TStateClass;
    procedure SetState(const Value: TStateClass);
    procedure SetStateOnly(const Value: TStateClass);
    function ConvertEnumState(const TEnumState: TUpdateState): TStateClass;

    procedure ShutDown;
    {
      SavePackageUpgradesToDownloadTempPath saves any new package IDs to a
      single file in the download tempPath. This procedure saves the IDs of any
      new packages to a file named "upgrade_packages.inf" in the download
      tempPath.
    }
    procedure SavePackageUpgradesToDownloadTempPath;
    function checkUpdateSchedule : Boolean;

    function SetRegistryState (Update : TUpdateState): Boolean;
    function GetAutomaticUpdate: Boolean;
    function SetApplyNow(Value : Boolean): Boolean;
    function GetApplyNow: Boolean;

  protected
  property State: TStateClass read GetState write SetState;

  public
    constructor Create(AForce: Boolean);
    destructor Destroy; override;

    procedure   HandleCheck;
    function    HandleKmShell : Integer;
    procedure   HandleDownload;
    procedure   HandleAbort;
    procedure   HandleInstallNow;
    function    CurrentStateName: string;

    property ShowErrors: Boolean read FShowErrors write FShowErrors;
    function CheckRegistryState : TUpdateState;

  end;

  IOnlineUpdateSharedData = interface
    ['{7442A323-C1E3-404B-BEEA-5B24A52BBB0E}']
    function Params: TUpdateStateMachineParams;
  end;

  TOnlineUpdateSharedData = class(TInterfacedObject, IOnlineUpdateSharedData)
  private
    FParams: TUpdateStateMachineParams;
  public
    constructor Create(AParams: TUpdateStateMachineParams);
    function Params: TUpdateStateMachineParams;
  end;
  // Private Utility functions
  function ConfigCheckContinue: Boolean;
implementation

uses
  Winapi.Shlobj,
  System.WideStrUtils,
  Vcl.Dialogs,
  Winapi.ShellApi,
  Winapi.Windows,
  Winapi.WinINet,

  GlobalProxySettings,
  KLog,
  keymanapi_TLB,
  KeymanVersion,
  kmint,
  ErrorControlledRegistry,
  RegistryKeys,
  Upload_Settings,
  utildir,
  utilexecute,
  OnlineUpdateCheckMessages,    // todo create own messages
  UfrmOnlineUpdateIcon,
  UfrmOnlineUpdateNewVersion,
  utilsystem,
  utiluac,
  versioninfo,
  Keyman.System.RemoteUpdateCheck,
  Keyman.System.DownloadUpdate;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';
  kmShellContinue = 0;
  kmShellExit = 1;

{ TUpdateStateMachine }

constructor TUpdateStateMachine.Create(AForce : Boolean);
begin
  inherited Create;
  FShowErrors := True;
  FParams.Result := oucUnknown;

  FForce := AForce;
  FAutomaticUpdate := GetAutomaticUpdate;
  FIdle := IdleState.Create(Self);
  FUpdateAvailable := UpdateAvailableState.Create(Self);
  FDownloading := DownloadingState.Create(Self);
  FWaitingRestart := WaitingRestartState.Create(Self);
  FInstalling := InstallingState.Create(Self);
  FRetry := RetryState.Create(Self);
  FPostInstall := PostInstallState.Create(Self);
  // Check the Registry setting.
  SetStateOnly(ConvertEnumState(CheckRegistryState));
  KL.Log('TUpdateStateMachine.Create');
end;

destructor TUpdateStateMachine.Destroy;
begin
  if (FErrorMessage <> '') and FShowErrors then
    KL.Log(FErrorMessage);

  if FParams.Result = oucShutDown then
    ShutDown;

  FIdle.Free;
  FUpdateAvailable.Free;
  FDownloading.Free;
  FWaitingRestart.Free;
  FInstalling.Free;
  FRetry.Free;
  FPostInstall.Free;

  KL.Log('TUpdateStateMachine.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TUpdateStateMachine.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  inherited Destroy;
end;


procedure TUpdateStateMachine.SavePackageUpgradesToDownloadTempPath;
var
  i: Integer;
  StringList : TStringList;
begin
  StringList := TStringList.Create;
  try
    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].NewID <> '' then
        StringList.Add(FParams.Packages[i].NewID+'='+FParams.Packages[i].ID);
    if StringList.Count > 0 then
      StringList.SaveToFile(DownloadTempPath + SPackageUpgradeFileName);
  finally
    StringList.Free;
  end;
end;

procedure TUpdateStateMachine.ShutDown;
begin
  if Assigned(Application) then
    Application.Terminate;
end;

{ TOnlineUpdateSharedData }

constructor TOnlineUpdateSharedData.Create(AParams: TUpdateStateMachineParams);
begin
  inherited Create;
  FParams := AParams;
end;

function TOnlineUpdateSharedData.Params: TUpdateStateMachineParams;
begin
  Result := FParams;
end;

function TUpdateStateMachine.SetRegistryState(Update : TUpdateState): Boolean;
var
  UpdateStr : string;
  Registry: TRegistryErrorControlled;
begin
  Result := False;
  Registry := TRegistryErrorControlled.Create;

  try
    Registry.RootKey := HKEY_CURRENT_USER;
    KL.Log('SetRegistryState State Entry');
    if not Registry.OpenKey(SRegKey_KeymanEngine_CU, True) then
    begin
      KL.Log('Failed to open registry key: ' + SRegKey_KeymanEngine_CU);
      Exit;
    end;

    try
      UpdateStr := GetEnumName(TypeInfo(TUpdateState), Ord(Update));
      Registry.WriteString(SRegValue_Update_State, UpdateStr);
      KL.Log('SetRegistryState State is: [' + UpdateStr + ']');
      Result := True;
    except
      on E: Exception do
      begin
        KL.Log('Failed to write to registry: ' + E.Message);
      end;
    end;

  finally
    Registry.Free;
  end;

end;

function TUpdateStateMachine.CheckRegistryState: TUpdateState;  // I2329
var
  UpdateState: TUpdateState;
  Registry: TRegistryErrorControlled;

begin
  // We will use a registry flag to maintain the state of the background update

  // check the registry value
  Registry := TRegistryErrorControlled.Create;  // I2890
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and Registry.ValueExists(SRegValue_Update_State) then
    begin
      UpdateState := TUpdateState(GetEnumValue(TypeInfo(TUpdateState), Registry.ReadString(SRegValue_Update_State)));
      KL.Log('CheckRegistryState State is:[' + Registry.ReadString(SRegValue_Update_State) + ']');
    end
    else
    begin
      UpdateState := usIdle; // do we need a unknown state ?
      KL.Log('CheckRegistryState State reg value not found default:[' + Registry.ReadString(SRegValue_Update_State) + ']');
    end;
  finally
    Registry.Free;
  end;

  Result := UpdateState;
end;

function TUpdateStateMachine.GetAutomaticUpdate: Boolean;  // I2329
var
  Registry: TRegistryErrorControlled;

begin
  // check the registry value
  Registry := TRegistryErrorControlled.Create;  // I2890
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and Registry.ValueExists(SRegValue_AutomaticUpdates) then
    begin
      Result := Registry.ReadBool(SRegValue_AutomaticUpdates);
    end
    else
    begin
      Result := True; // Default
    end;
  finally
    Registry.Free;
  end;
end;

function TUpdateStateMachine.SetApplyNow(Value : Boolean): Boolean;
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
    if Registry.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and Registry.ValueExists(SRegValue_ApplyNow) then
    begin
      Result := Registry.ReadBool(SRegValue_ApplyNow);
    end
    else
    begin
      Result := False; // Default
    end;
  finally
    Registry.Free;
  end;
end;


function TUpdateStateMachine.CheckUpdateSchedule: Boolean;
var
  RegistryErrorControlled :TRegistryErrorControlled;
begin
  try
    Result := False;
    RegistryErrorControlled := TRegistryErrorControlled.Create;

    try
      if RegistryErrorControlled.OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if RegistryErrorControlled.ValueExists(SRegValue_CheckForUpdates) and not RegistryErrorControlled.ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := False;
          Exit;
        end;
        if RegistryErrorControlled.ValueExists(SRegValue_LastUpdateCheckTime) and (Now - RegistryErrorControlled.ReadDateTime(SRegValue_LastUpdateCheckTime) < 1) and not FForce then
        begin
          Result := False;
          Exit;
        end;
        // Else Time to check for updates
        Result := True;
      end;
    finally
      RegistryErrorControlled.Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := False;
      FErrorMessage := E.Message;
      Exit;
    end;
  end;
end;

function TUpdateStateMachine.GetState: TStateClass;
begin
  Result := TStateClass(CurrentState.ClassType);
end;

procedure TUpdateStateMachine.SetState(const Value: TStateClass);
begin
  if Assigned(CurrentState) then
  begin
    CurrentState.Exit;
  end;

  SetStateOnly(Value);

  if Assigned(CurrentState) then
  begin
    CurrentState.Enter;
  end
  else
  begin
   // TODO: #10210 Error log for Unable to set state for Value
  end;

end;

procedure TUpdateStateMachine.SetStateOnly(const Value: TStateClass);
begin
  if Value = IdleState then
  begin
    CurrentState := FIdle;
  end
  else if Value = UpdateAvailableState then
  begin
    CurrentState := FUpdateAvailable;
  end
  else if Value = DownloadingState then
  begin
    CurrentState := FDownloading;
  end
  else if Value = WaitingRestartState then
  begin
    CurrentState := FWaitingRestart;
  end
  else if Value = InstallingState then
  begin
    CurrentState := FInstalling;
  end
  else if Value = RetryState then
  begin
    CurrentState := FRetry;
  end
  else if Value = PostInstallState then
  begin
    CurrentState := FPostInstall;
  end;
end;

function TUpdateStateMachine.ConvertEnumState(const TEnumState: TUpdateState) : TStateClass;
begin
  case  TEnumState of
    usIdle: Result := IdleState;
    usUpdateAvailable: Result := UpdateAvailableState;
    usDownloading: Result := DownloadingState;
    usWaitingRestart: Result := WaitingRestartState;
    usInstalling: Result := InstallingState;
    usRetry: Result := RetryState;
    usPostInstall: Result := PostInstallState;
  else
    // TODO: #10210 Log error unknown state setting to idle
    Result := IdleState;
  end;
end;

procedure TUpdateStateMachine.HandleCheck;
begin
  CurrentState.HandleCheck;
end;

function TUpdateStateMachine.HandleKmShell;
begin
  Result := CurrentState.HandleKmShell;
end;

procedure TUpdateStateMachine.HandleDownload;
begin
  CurrentState.HandleDownload;
end;

procedure TUpdateStateMachine.HandleAbort;
begin
  CurrentState.HandleAbort;
end;

procedure TUpdateStateMachine.HandleInstallNow;
begin
  CurrentState.HandleInstallNow;
end;

function TUpdateStateMachine.CurrentStateName: string;
begin
  Result := CurrentState.StateName;
end;

{ State Class Memebers }
constructor TState.Create(Context: TUpdateStateMachine);
begin
  bucStateContext := Context;
end;

procedure TState.ChangeState(NewState: TStateClass);
begin
  KL.Log('TUpdateStateMachine.ChangeState old' + bucStateContext.CurrentStateName  );
  bucStateContext.State := NewState;
  KL.Log('TUpdateStateMachine.ChangeState new' + bucStateContext.CurrentStateName  );
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

  {##### For Testing only just advancing to downloading ####}
  ChangeState(UpdateAvailableState);
  {#### End of Testing ### };



  { Make a HTTP request out and see if updates are available for now do
    this all in the Idle HandleCheck message. But could be broken into an
    seperate state of WaitngCheck RESP }
  { if Response not OK stay in the idle state and return }


  // If handle check event force check
  //CheckForUpdates := TRemoteUpdateCheck.Create(True);
  //try
 //    Result:= CheckForUpdates.Run;
 // finally
 //   CheckForUpdates.Free;
 // end;

  { Response OK and Update is available }
 // if Result = wucSuccess then
 // begin
 //   ChangeState(UpdateAvailableState);
 // end;

  // else staty in idle state
end;

function IdleState.HandleKmShell;
var
 CheckForUpdates: TRemoteUpdateCheck;
 UpdateCheckResult : TRemoteUpdateCheckResult;
//const CheckPeriod: Integer = 7; // Days between checking for updates
begin
  // Check if auto updates enable and if scheduled time has expired
  KL.Log('IdleState.HandleKmShell');
  if ConfigCheckContinue then
  begin
    CheckForUpdates := TRemoteUpdateCheck.Create(True);
    try
       UpdateCheckResult:= CheckForUpdates.Run;
    finally
      CheckForUpdates.Free;
    end;
    { Response OK and Update is available }
    if UpdateCheckResult = wucSuccess then
    begin
      ChangeState(UpdateAvailableState);
    end;
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
  //  TODO: How do we notify the command line no update available
end;

function IdleState.StateName;
begin

  Result := 'IdleState';
end;

{ UpdateAvailableState }


procedure UpdateAvailableState.StartDownloadProcess;
var DownloadResult, FResult : Boolean;
RootPath: string;
begin
  // call seperate process
  RootPath := ExtractFilePath(ParamStr(0));
  FResult := TUtilExecute.ShellCurrentUser(0, ParamStr(0), IncludeTrailingPathDelimiter(RootPath), '-bd');
  if not FResult then
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
  frmStartInstallNow : TfrmStartInstallNow;
  InstallNow : Boolean;
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
  // If user decides NOT to install now stay in UpdateAvailable State
  if InstallNow = True then
  begin
    bucStateContext.SetApplyNow(True);
    ChangeState(InstallingState)
  end;

end;

function UpdateAvailableState.StateName;
begin
  Result := 'UpdateAvailableState';
end;

{ DownloadingState }

procedure DownloadingState.Enter;
var DownloadResult, FResult : Boolean;
RootPath: string;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usDownloading);
   {##  for testing log that we would download }
  KL.Log('DownloadingState.HandleKmshell test code continue');
  DownloadResult := True;
  { End testing}
  //DownloadResult := DownloadUpdatesBackground;
  // TODO check if keyman is running then send to Waiting Restart
  if DownloadResult then
  begin
    if HasKeymanRun then
    begin
      if bucStateContext.GetApplyNow  then
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
var DownloadResult, FResult : Boolean;
RootPath: string;
begin
  // Enter Already Downloading
  KL.Log('DownloadingState.HandleDownload already downloading');
end;

procedure DownloadingState.HandleAbort;
begin
end;

procedure DownloadingState.HandleInstallNow;
begin
  // Already downloading set the registry apply now
  bucStateContext.SetApplyNow(True);
end;

function DownloadingState.StateName;
begin
  Result := 'DownloadingState';
end;

function DownloadingState.DownloadUpdatesBackground: Boolean;
var
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
  DownloadUpdate: TDownloadUpdate;
begin
  DownloadUpdate := TDownloadUpdate.Create;
  try
    DownloadResult := DownloadUpdate.DownloadUpdates;
    KL.Log('TUpdateStateMachine.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
    Result := DownloadResult;
// #TODO: #10210 workout when we need to refresh kmcom keyboards

//      if Result in [ wucSuccess] then
//  begin
//    kmcom.Keyboards.Refresh;
//    kmcom.Keyboards.Apply;
//    kmcom.Packages.Refresh;
//  end;
  finally
    DownloadUpdate.Free;
  end;
end;

{ WaitingRestartState }

procedure WaitingRestartState.Enter;
begin
  // Enter WaitingRestartState
  KL.Log('WaitingRestartState.HandleKmShell Enter');
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
  SavedPath : String;
  Filenames : TStringDynArray;
  frmStartInstall : TfrmStartInstall;
begin
  KL.Log('WaitingRestartState.HandleKmShell Enter');
  // Still can't go if keyman has run
  if HasKeymanRun then
  begin
    KL.Log('WaitingRestartState.HandleKmShell Keyman Has Run');
    Result := kmShellContinue;
    // Exit; // Exit is not wokring for some reason.
    // this else is only here because the exit is not working.
  end
  else
  begin
    // Check downloaded cache if available then
    SavedPath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
    GetFileNamesInDirectory(SavedPath, FileNames);
    if Length(FileNames) = 0 then
    begin
        KL.Log('WaitingRestartState.HandleKmShell No Files in Download Cache');
        // Return to Idle state and check for Updates state
        ChangeState(IdleState);
        bucStateContext.CurrentState.HandleCheck;  // TODO no event here
        Result := kmShellExit;
        // Exit; // again exit was not working
    end
    else
    begin
      KL.Log('WaitingRestartState.HandleKmShell is good to install');
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
  frmStartInstallNow : TfrmStartInstallNow;
  InstallNow : Boolean;
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
    ChangeState(InstallingState)
  end;
end;

function WaitingRestartState.StateName;
begin

  Result := 'WaitingRestartState';
end;

{ InstallingState }
function InstallingState.DoInstallPackage(Package: TUpdateStateMachineParamsPackage): Boolean;
var
  FPackage: IKeymanPackageFile2;
begin
  Result := True;

  FPackage := kmcom.Packages.GetPackageFromFile(Package.SavePath) as IKeymanPackageFile2;
  FPackage.Install2(True);  // Force overwrites existing package and leaves most settings for it intact
  FPackage := nil;

  kmcom.Refresh;
  kmcom.Apply;
  System.SysUtils.DeleteFile(Package.SavePath);
end;

procedure InstallingState.DoInstallKeyman;
var
  s: string;
  FResult: Boolean;
begin
  FResult := False;
  s := LowerCase(ExtractFileExt(bucStateContext.FParams.Keyman.SavePath));
  if s = '.msi' then
    FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /i "'+bucStateContext.FParams.Keyman.SavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
  else if s = '.exe' then
    FResult := TUtilExecute.Shell(0, bucStateContext.FParams.Keyman.SavePath, '', '-au')  // I3349
  else
    Exit;
  if not FResult then
    ShowMessage(SysErrorMessage(GetLastError));
end;

function InstallingState.DoInstallKeyman(SavePath: string) : Boolean;
var
  s: string;
  FResult: Boolean;
begin
  s := LowerCase(ExtractFileExt(SavePath));
  if s = '.msi' then
    FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /i "'+SavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
  else if s = '.exe' then
  begin
    KL.Log('TUpdateStateMachine.InstallingState.DoInstallKeyman SavePath:"'+ SavePath+'"');
    // switch -au for auto update in silent mode.
    // We will need to add the pop up that says install update now yes/no
    // This will run the setup executable which will ask for elevated permissions
    FResult := TUtilExecute.Shell(0, SavePath, '', '-au')  // I3349
  end
  else
    FResult := False;

  if not FResult then
  begin
     KL.Log('TUpdateStateMachine.InstallingState.DoInstall: Result = '+IntToStr(Ord(FResult)));
     // Log messageShowMessage(SysErrorMessage(GetLastError));
  end;

  Result := FResult;
end;

procedure InstallingState.Enter;
var
  SavePath: String;
  fileExt : String;
  fileName: String;
  fileNames: TStringDynArray;
begin
    bucStateContext.SetRegistryState(usInstalling);
    SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);

    GetFileNamesInDirectory(SavePath, fileNames);
    // for now we only want the exe although excute install can
    // handle msi
    for fileName in fileNames do
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

function InstallingState.StateName;
begin
  Result := 'InstallingState';
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

function RetryState.StateName;
begin

  Result := 'RetryState';
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
var SavePath: string;
    FileName: String;
    FileNames: TStringDynArray;
begin
      KL.Log('PostInstallState.HandleMSIInstallComplete');
      SavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
      KL.Log('PostInstallState.HandleMSIInstallComplete remove SavePath:'+ SavePath);

      GetFileNamesInDirectory(SavePath, FileNames);
      for FileName in FileNames do
      begin
        System.SysUtils.DeleteFile(FileName);
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

function PostInstallState.StateName;
begin

  Result := 'PostInstallState';
end;

// Private Functions:
function ConfigCheckContinue: Boolean;
var
  registry: TRegistryErrorControlled;
begin
{ Verify that it has been at least CheckPeriod days since last update check }
  Result := False;
  try
    registry := TRegistryErrorControlled.Create;   // I2890
    try
      if registry.OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if registry.ValueExists(SRegValue_CheckForUpdates) and not registry.ReadBool(SRegValue_CheckForUpdates) then
        begin
          Result := False;
          Exit;
        end;
        if registry.ValueExists(SRegValue_LastUpdateCheckTime) and (Now - registry.ReadDateTime(SRegValue_LastUpdateCheckTime) > CheckPeriod) then
        begin
          Result := True;
        end
        else
        begin
          Result := False;
        end;
        Exit;
      end;
    finally
      registry.Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := False;
      LogMessage(E.Message);
      Exit;
    end;
  end;
end;

end.
