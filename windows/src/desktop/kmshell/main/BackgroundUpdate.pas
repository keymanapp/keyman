(*
  Name:             BackgroundUpdate
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
unit BackgroundUpdate;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.IOUtils,
  System.Types,
  Vcl.Forms,
  TypInfo,

  httpuploader,
  Keyman.System.UpdateCheckResponse,
  UfrmDownloadProgress;

type
  EBackgroundUpdate = class(Exception);

  TBackgroundUpdateResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucUpdatesAvailable, oucFailure, oucOffline);

  TUpdateState = (usIdle, usUpdateAvailable, usDownloading, usWaitingRestart, usInstalling, usRetry, usWaitingPostInstall);

  { Keyboard Package Params }
  TBackgroundUpdateParamsPackage = record
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
  TBackgroundUpdateParamsKeyman = record
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TBackgroundUpdateParams = record
    Keyman: TBackgroundUpdateParamsKeyman;
    Packages: array of TBackgroundUpdateParamsPackage;
    Result: TBackgroundUpdateResult;
  end;

  TBackgroundUpdateDownloadParams = record
    Owner: TfrmDownloadProgress;
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

   // Forward declaration
   TBackgroundUpdate = class;
    { State Classes Update }

   TStateClass = class of TState;

  TState = class abstract
  private
    bucStateContext: TBackgroundUpdate;
    procedure ChangeState(newState: TStateClass);

  public
    constructor Create(Context: TBackgroundUpdate);
    procedure Enter; virtual; abstract;
    procedure Exit; virtual; abstract;
    procedure HandleCheck; virtual; abstract;
    procedure HandleDownload; virtual; abstract;
    function HandleKmShell : Integer; virtual; abstract;
    procedure HandleInstall; virtual; abstract;
    procedure HandleMSIInstallComplete; virtual; abstract;
    procedure HandleAbort; virtual; abstract;

    // For convenience
    function StateName: string; virtual; abstract;

  end;

  // Derived classes for each state
  IdleState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  UpdateAvailableState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  DownloadingState = class(TState)
  private
  { These function could become members of the state objects
       or at the very least controlled by the state objects }

    { TODO: make a comment clear when we are in a elevate process}

    {
      Performs updates download in the background, without displaying a GUI
      progress bar. This function is similar to DownloadUpdates, but it runs in
      the background.

      @returns  True  if all updates were successfully downloaded, False if any
      download failed.
    }

    function DownloadUpdatesBackground: Boolean;
    {
      Performs updates download in the background, without displaying a GUI
      progress bar. This procedure is similar to DownloadUpdates, but it runs in
      the background.

      @params  SavePath  The path where the downloaded files will be saved.
               Result    A Boolean value indicating the overall result of the
               download process.
    }
    procedure DoDownloadUpdatesBackground(SavePath: string; var Result: Boolean);
    {
      Performs an online update check, including package retrieval and version
      query.

      This function checks if a week has passed since the last update check. It
      utilizes the kmcom API to retrieve the current packages. The function then
      performs an HTTP request to query the remote versions of these packages.
      The resulting information is stored in the FParams variable. Additionally,
      the function handles the main Keyman install package.

      @returns  A TBackgroundUpdateResult indicating the result of the update
      check.
    }
    // This is just for testing only.
    procedure DoDownloadUpdatesBackgroundTest(SavePath: string; var Result: Boolean);
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function  HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  WaitingRestartState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function  HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
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
    function DoInstallPackage(Package: TBackgroundUpdateParamsPackage): Boolean;
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function  HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  RetryState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function  HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  WaitingPostInstallState = class(TState)
  public
    procedure Enter; override;
    procedure Exit; override;
    procedure HandleCheck; override;
    procedure HandleDownload; override;
    function  HandleKmShell : Integer; override;
    procedure HandleInstall; override;
    procedure HandleMSIInstallComplete; override;
    procedure HandleAbort; override;
    function StateName: string; override;
  end;

  { This class also controls the state flow see  }
  TBackgroundUpdate = class
  private
    FOwner: TCustomForm;
    FSilent: Boolean;
    FForce: Boolean;
    FAuto: Boolean;
    FParams: TBackgroundUpdateParams;

    FErrorMessage: string;

    DownloadTempPath: string;

    FShowErrors: Boolean;



    FDownload: TBackgroundUpdateDownloadParams;

    CurrentState: TState;
    // State object for performance (could lazy create?)
    FIdle: IdleState;
    FUpdateAvailable: UpdateAvailableState;
    FDownloading: DownloadingState;
    FWaitingRestart: WaitingRestartState;
    FInstalling: InstallingState;
    FRetry: RetryState;
    FWaitingPostInstall: WaitingPostInstallState;
    function GetState: TStateClass;
    procedure SetState(const Value: TStateClass);
    function ConvertEnumState(const TEnumState: TUpdateState): TStateClass;

    procedure ShutDown;

    {
      SavePackageUpgradesToDownloadTempPath saves any new package IDs to a
      single file in the download tempPath. This procedure saves the IDs of any
      new packages to a file named "upgrade_packages.inf" in the download
      tempPath.
    }
    procedure SavePackageUpgradesToDownloadTempPath;
    function IsKeymanRunning: Boolean;
    function checkUpdateSchedule : Boolean;

    function SetRegistryState (Update : TUpdateState): Boolean;

  protected
  property State: TStateClass read GetState write SetState;

  public
    constructor Create(AOwner: TCustomForm; AForce, ASilent: Boolean);
    destructor Destroy; override;

    procedure   HandleCheck;
    function    HandleKmShell : Integer;
    procedure   HandleDownload;
    procedure   HandleInstall;
    procedure   HandleMSIInstallComplete;
    procedure   HandleAbort;
    function    CurrentStateName: string;

    property ShowErrors: Boolean read FShowErrors write FShowErrors;
    function CheckRegistryState : TUpdateState;

  end;

  IOnlineUpdateSharedData = interface
    ['{7442A323-C1E3-404B-BEEA-5B24A52BBB0E}']
    function Params: TBackgroundUpdateParams;
  end;

  TOnlineUpdateSharedData = class(TInterfacedObject, IOnlineUpdateSharedData)
  private
    FParams: TBackgroundUpdateParams;
  public
    constructor Create(AParams: TBackgroundUpdateParams);
    function Params: TBackgroundUpdateParams;
  end;

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
  utilkmshell,
  utilsystem,
  utiluac,
  versioninfo;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';
  kmShellContinue = 0;
  kmShellExit = 1;

{ TBackgroundUpdate }

constructor TBackgroundUpdate.Create(AOwner: TCustomForm; AForce, ASilent: Boolean);
var TSerailsedState : TUpdateState;
begin
  inherited Create;

  FOwner := AOwner;

  FShowErrors := True;
  FParams.Result := oucUnknown;

  FSilent := ASilent;
  FForce := AForce;
  FAuto := True; // Default to automatically check, download, and install
  FIdle := IdleState.Create(Self);
  FUpdateAvailable := UpdateAvailableState.Create(Self);
  FDownloading := DownloadingState.Create(Self);
  FWaitingRestart := WaitingRestartState.Create(Self);
  FInstalling := InstallingState.Create(Self);
  FRetry := RetryState.Create(Self);
  FWaitingPostInstall := WaitingPostInstallState.Create(Self);
  // Check the Registry setting.
  state := ConvertEnumState(CheckRegistryState);
  KL.Log('TBackgroundUpdate.Create');
end;

destructor TBackgroundUpdate.Destroy;
begin
  if (FErrorMessage <> '') and not FSilent and FShowErrors then
    ShowMessage(FErrorMessage);

  if FParams.Result = oucShutDown then
    ShutDown;

  FIdle.Free;
  FUpdateAvailable.Free;
  FDownloading.Free;
  FWaitingRestart.Free;
  FInstalling.Free;
  FRetry.Free;
  FWaitingPostInstall.Free;

  KL.Log('TBackgroundUpdate.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TBackgroundUpdate.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  inherited Destroy;
end;


procedure TBackgroundUpdate.SavePackageUpgradesToDownloadTempPath;
var
  i: Integer;
begin
  with TStringList.Create do
  try
    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].NewID <> '' then
        Add(FParams.Packages[i].NewID+'='+FParams.Packages[i].ID);
    if Count > 0 then
      SaveToFile(DownloadTempPath + SPackageUpgradeFileName);
  finally
    Free;
  end;
end;

procedure TBackgroundUpdate.ShutDown;
begin
  if Assigned(Application) then
    Application.Terminate;
end;


{ TOnlineUpdateSharedData }

constructor TOnlineUpdateSharedData.Create(AParams: TBackgroundUpdateParams);
begin
  inherited Create;
  FParams := AParams;
end;

function TOnlineUpdateSharedData.Params: TBackgroundUpdateParams;
begin
  Result := FParams;
end;


function TBackgroundUpdate.SetRegistryState(Update : TUpdateState): Boolean;
var
  UpdateStr : string;
begin

  Result := False;
  with TRegistryErrorControlled.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    KL.Log('SetRegistryState State Entry');
    if OpenKey(SRegKey_KeymanEngine_LM, True) then
    begin
        UpdateStr := GetEnumName(TypeInfo(TUpdateState), Ord(Update));
        WriteString(SRegValue_Update_State, UpdateStr);
        KL.Log('SetRegistryState State is:[' + UpdateStr + ']');
    end;
    Result := True;
  finally
      Free;
  end;

end;


function TBackgroundUpdate.CheckRegistryState : TUpdateState;  // I2329
var
  UpdateState : TUpdateState;

begin
  // We will use a registry flag to maintain the state of the background update

  UpdateState := usIdle; // do we need a unknown state ?
  // check the registry value
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_Update_State) then
      begin
        UpdateState := TUpdateState(GetEnumValue(TypeInfo(TUpdateState), ReadString(SRegValue_Update_State)));
        KL.Log('CheckRegistryState State is:[' + ReadString(SRegValue_Update_State) + ']');
      end
    else
      begin
          UpdateState := usIdle; // do we need a unknown state ?
          KL.Log('CheckRegistryState State reg value not found default:[' + ReadString(SRegValue_Update_State) + ']');
      end
    finally
      Free;
  end;
  Result := UpdateState;
end;



function TBackgroundUpdate.IsKeymanRunning: Boolean;  // I2329
begin
  try
      Result :=  kmcom.Control.IsKeymanRunning;
  except
    on E:Exception do
    begin
      KL.Log(E.Message);
      Exit(False);
    end;
  end;
end;

function TBackgroundUpdate.CheckUpdateSchedule: Boolean;
begin
  try
    Result := False;
    with TRegistryErrorControlled.Create do
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := False;
          Exit;
        end;
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 1) and not FForce then
        begin
          Result := False;
          Exit;
        end;
        // Else Time to check for updates
        Result := True;
      end;
    finally
      Free;
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

function TBackgroundUpdate.GetState: TStateClass;
begin
  Result := TStateClass(CurrentState.ClassType);
end;

procedure TBackgroundUpdate.SetState(const Value: TStateClass);
begin
  if Assigned(CurrentState) then
  begin
    CurrentState.Exit;
  end;

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
  else if Value = WaitingPostInstallState then
  begin
    CurrentState := FWaitingPostInstall;
  end;

  if Assigned(CurrentState) then
  begin
    CurrentState.Enter;
  end
  else
  begin
   // TODO: Unable to set state for Value []
  end;

end;

function TBackgroundUpdate.ConvertEnumState(const TEnumState: TUpdateState) : TStateClass;
begin
  case  TEnumState of
    usIdle: Result := IdleState;
    usUpdateAvailable: Result := UpdateAvailableState;
    usDownloading: Result := DownloadingState;
    usWaitingRestart: Result := WaitingRestartState;
    usInstalling: Result := InstallingState;
    usRetry: Result := RetryState;
    usWaitingPostInstall: Result := WaitingPostInstallState;
  else
    // Log error unknown state setting to idle
    Result := IdleState;
  end;
end;

procedure TBackgroundUpdate.HandleCheck;
begin
  CurrentState.HandleCheck;
end;

function TBackgroundUpdate.HandleKmShell;
begin
  Result := CurrentState.HandleKmShell;
end;

procedure TBackgroundUpdate.HandleDownload;
begin
  CurrentState.HandleDownload;
end;

procedure TBackgroundUpdate.HandleInstall;
begin
  CurrentState.HandleInstall;
end;

procedure TBackgroundUpdate.HandleMSIInstallComplete;
begin
  CurrentState.HandleMSIInstallComplete;
end;

procedure TBackgroundUpdate.HandleAbort;
begin
  CurrentState.HandleAbort;
end;

function TBackgroundUpdate.CurrentStateName: string;
begin
  // Implement your logic here
  Result := CurrentState.StateName;
end;



{ State Class Memebers }
constructor TState.Create(Context: TBackgroundUpdate);
begin
  bucStateContext := Context;
end;

procedure TState.ChangeState(NewState: TStateClass);
begin
  bucStateContext.State := NewState;
end;


{ IdleState }

procedure IdleState.Enter;
begin
  // Enter UpdateAvailableState
  // register name
  bucStateContext.SetRegistryState(usIdle);
end;

procedure IdleState.Exit;
begin
  // Exit UpdateAvailableState
end;

procedure IdleState.HandleCheck;
begin
  { TODO: Verify that it has been at least 7 days since last update check -
    only if FSilent = TRUE }

  { Make a HTTP request out and see if updates are available for now do
    this all in the Idle HandleCheck message. But could be broken into an
    seperate state of WaitngCheck RESP }
  { if Response not OK stay in the idle state and return }

  { Response OK and Update is available }
  ChangeState(UpdateAvailableState);

end;

procedure IdleState.HandleDownload;
begin
  // Implement your logic here
end;

function IdleState.HandleKmShell;
begin
  // Implement your logic here
  Result := kmShellContinue;
end;

procedure IdleState.HandleInstall;
begin
  // Implement your logic here
end;

procedure IdleState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure IdleState.HandleAbort;
begin
  // Implement your logic here
end;

function IdleState.StateName;
begin
  // Implement your logic here
  Result := 'IdleState';
end;

{ UpdateAvailableState }

procedure UpdateAvailableState.Enter;
begin
  // Enter UpdateAvailableState
  bucStateContext.SetRegistryState(usUpdateAvailable);
  if bucStateContext.FAuto then
  begin
    bucStateContext.CurrentState.HandleDownload ;
  end;
end;

procedure UpdateAvailableState.Exit;
begin
  // Exit UpdateAvailableState
end;

procedure UpdateAvailableState.HandleCheck;
begin
  // Implement your logic here
end;

procedure UpdateAvailableState.HandleDownload;
begin
  ChangeState(DownloadingState);
end;

function UpdateAvailableState.HandleKmShell;
begin
  if bucStateContext.FAuto then
  begin
    bucStateContext.CurrentState.HandleDownload ;
  end;
  Result := kmShellContinue;
end;

procedure UpdateAvailableState.HandleInstall;
begin
  // Implement your logic here
end;

procedure UpdateAvailableState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure UpdateAvailableState.HandleAbort;
begin
  // Implement your logic here
end;

function UpdateAvailableState.StateName;
begin
  // Implement your logic here
  Result := 'UpdateAvailableState';
end;

{ DownloadingState }

procedure DownloadingState.Enter;
var DownloadResult : Boolean;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usDownloading);
  DownloadResult := DownloadUpdatesBackground;
  if DownloadResult then
  begin
    if bucStateContext.IsKeymanRunning then
      ChangeState(WaitingRestartState)
    else
      ChangeState(InstallingState);
  end
  else
  begin
    ChangeState(RetryState);
  end
end;

procedure DownloadingState.Exit;
begin
  // Exit DownloadingState
end;

procedure DownloadingState.HandleCheck;
begin
  // For now just pretend updated found
end;

procedure DownloadingState.HandleDownload;
var DownloadResult : Boolean;
begin
  // We are already downloading do nothing
end;

function DownloadingState.HandleKmShell;
var DownloadResult : Boolean;
begin
  DownloadResult := DownloadUpdatesBackground;
  // TODO check if keyman is running then send to Waiting Restart
  if DownloadResult then
  begin
    ChangeState(InstallingState);
  end
  else
  begin
    ChangeState(RetryState);
  end;
  Result := kmShellContinue;
end;

procedure DownloadingState.HandleInstall;
begin
  // Implement your logic here
  ChangeState(InstallingState);
end;

procedure DownloadingState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure DownloadingState.HandleAbort;
begin
  // Implement your logic here
end;

function DownloadingState.StateName;
begin
  // Implement your logic here
  Result := 'DownloadingState';
end;

procedure DownloadingState.DoDownloadUpdatesBackground(SavePath: string; var Result: Boolean);
begin
end;

// Test installing only
procedure DownloadingState.DoDownloadUpdatesBackgroundTest(SavePath: string; var Result: Boolean);
var
  i, downloadCount: Integer;
  UpdateDir : string;

begin
   try
    Result := False;

    UpdateDir := 'C:\Projects\rcswag\testCache';
    KL.Log('DoDownloadUpdatesBackgroundTest SavePath:'+ SavePath);
    // Check if the update source directory exists
    if DirectoryExists(UpdateDir) then
    begin
      // Create the update cached directory if it doesn't exist
      if not DirectoryExists(SavePath) then
        ForceDirectories(SavePath);

      // Copy all files from the updatedir to savepath
      TDirectory.Copy(UpdateDir, SavePath);
      Result:= True;
      KL.Log('All files copied successfully.');
    end
    else
      KL.Log('Source directory does not exist.');
  except
    on E: Exception do
      KL.Log('Error: ' + E.Message);
  end;

end;


function DownloadingState.DownloadUpdatesBackground: Boolean;
var
  i: Integer;
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
begin
  //DownloadTempPath := IncludeTrailingPathDelimiter(CreateTempPath);
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(GetFolderPath(CSIDL_COMMON_APPDATA) + SFolder_CachedUpdateFiles);
  //DownloadBackGroundSavePath :=  DownloadBackGroundSavePath + 'RCTest.txt';
  { For now lets download all the updates. We need to take these from the user via check box form }

  if bucStateContext.FParams.Keyman.DownloadURL <> '' then
    bucStateContext.FParams.Keyman.Install := True;

  for i := 0 to High(bucStateContext.FParams.Packages) do
    bucStateContext.FParams.Packages[i].Install := True;

  // Download files
  // DoDownloadUpdatesBackground(DownloadBackGroundSavePath, DownloadResult);
  // For development by passing the DoDownloadUpdatesBackground and just add
  // the cached file on my local disk as a simulation
   DoDownloadUpdatesBackgroundTest(DownloadBackGroundSavePath, DownloadResult);

  KL.Log('TBackgroundUpdate.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  //ShowMessage('TBackgroundUpdate.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  Result := DownloadResult;
end;

{ WaitingRestartState }

procedure WaitingRestartState.Enter;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usWaitingRestart);
end;

procedure WaitingRestartState.Exit;
begin
  // Exit DownloadingState
end;

procedure WaitingRestartState.HandleCheck;
begin
  // Implement your logic here
end;

procedure WaitingRestartState.HandleDownload;
begin
  // Implement your logic here
end;

function WaitingRestartState.HandleKmShell;
begin
  // Check downloaded cache if available then
  // change state intalling
  // else then change to idle and handle checkupdates state
 // if bucStateContext.IsKeymanRunning then
      // stay as waiting
      // log this is unexpected
      //ChangeState(WaitingRestartState)
   // else
  //

  ChangeState(UpdateAvailableState);
  Result := kmShellExit;
end;

procedure WaitingRestartState.HandleInstall;
begin
  // Implement your logic here
end;

procedure WaitingRestartState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure WaitingRestartState.HandleAbort;
begin
  // Implement your logic here
end;

function WaitingRestartState.StateName;
begin
  // Implement your logic here
  Result := 'WaitingRestartState';
end;

{ InstallingState }

function InstallingState.DoInstallPackage(Package: TBackgroundUpdateParamsPackage): Boolean;
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
    FResult := TUtilExecute.Shell(0, SavePath, '', '-au')  // I3349
  else
    Exit;
    //Exit(False);

  if not FResult then
  begin
     KL.Log('TBackgroundUpdate.InstallingState.DoInstall: Result = '+IntToStr(Ord(FResult)));
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
    SavePath := IncludeTrailingPathDelimiter(GetFolderPath(CSIDL_COMMON_APPDATA) + SFolder_CachedUpdateFiles);
    // For testing
    SavePath := 'C:\Projects\rcswag\testCache\';

    GetFileNamesInDirectory(SavePath, fileNames);
    // for now we only want the exe although excute install can
    // handle msi
    for fileName in fileNames do
    begin
      fileExt := LowerCase(ExtractFileExt(fileName));
      if fileExt = '.exe' then
        break;
    end;
    // ExecuteInstall(SavePath + ExtractFileName(fileName));
    // TODO DoInstallPackages ( this may need to be as state in the enum seperate
    // to installing the main keyman executable.
    if DoInstallKeyman(SavePath + ExtractFileName(fileName)) then
    begin
       KL.Log('TBackgroundUpdate.InstallingState.Enter: DoInstall OK');
    end
    else
    begin
        // TODO: clean failed download
        // TODO: Do we do a retry on install? probably not
        // install error log the error.
        KL.Log('TBackgroundUpdate.InstallingState.Enter: DoInstall fail');
        ChangeState(IdleState);
    end
end;

procedure InstallingState.Exit;
begin
  // Exit DownloadingState
end;

procedure InstallingState.HandleCheck;
begin
  // Implement your logic here
end;

procedure InstallingState.HandleDownload;
begin
  // Implement your logic here
end;

function InstallingState.HandleKmShell;
begin
  // Result = exit straight away as we are installing (MSI installer)
  Result := kmShellExit;
end;

procedure InstallingState.HandleInstall;
begin
  // Implement your logic here
end;

procedure InstallingState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure InstallingState.HandleAbort;
begin
  ChangeState(IdleState);
end;

function InstallingState.StateName;
begin
  // Implement your logic here
  Result := 'InstallingState';
end;

{ RetryState }

procedure RetryState.Enter;
begin
  // Enter DownloadingState
  bucStateContext.SetRegistryState(usRetry);
end;

procedure RetryState.Exit;
begin
  // Exit DownloadingState
end;

procedure RetryState.HandleCheck;
begin
  // Implement your logic here
end;

procedure RetryState.HandleDownload;
begin
  // Implement your logic here
end;

function RetryState.HandleKmShell;
begin
  // TODO Implement retry
  Result := kmShellContinue
end;

procedure RetryState.HandleInstall;
begin
  // Implement your logic here
end;

procedure RetryState.HandleMSIInstallComplete;
begin
  // Implement your logic here
end;

procedure RetryState.HandleAbort;
begin
  // Implement your logic here
end;

function RetryState.StateName;
begin
  // Implement your logic here
  Result := 'RetryState';
end;

{ WaitingPostInstallState }

procedure WaitingPostInstallState.Enter;
begin
  // Enter downloading state
  bucStateContext.SetRegistryState(usWaitingPostInstall);
end;

procedure WaitingPostInstallState.Exit;
begin
  // Exit downloading state
end;

procedure WaitingPostInstallState.HandleCheck;
begin
  // Handle Check
end;

procedure WaitingPostInstallState.HandleDownload;
begin
  // Handle Download
end;

function WaitingPostInstallState.HandleKmShell;
begin
  // TODO maybe have a counter if we get called in this state
  // to many time we need
  HandleMSIInstallComplete;
  Result := kmShellContinue;
end;

procedure WaitingPostInstallState.HandleInstall;
begin
  // Handle Install
end;

procedure WaitingPostInstallState.HandleMSIInstallComplete;
var SavePath: string;
    FileName: String;
    FileNames: TStringDynArray;
begin
      KL.Log('WaitingPostInstallState.HandleMSIInstallComplete');
      // TODO Remove cached files. Do any loging updating of files etc and then set back to idle
      SavePath := IncludeTrailingPathDelimiter(GetFolderPath(CSIDL_COMMON_APPDATA) + SFolder_CachedUpdateFiles);
      /// For testing using local user area cache
      SavePath := 'C:\Projects\rcswag\testCache';
      KL.Log('WaitingPostInstallState.HandleMSIInstallComplete remove SavePath:'+ SavePath);

      GetFileNamesInDirectory(SavePath, FileNames);
      for FileName in FileNames do
      begin
        System.SysUtils.DeleteFile(FileName);
      end;
      ChangeState(IdleState);
end;

procedure WaitingPostInstallState.HandleAbort;
begin
  // Handle Abort
end;

function WaitingPostInstallState.StateName;
begin
  // Implement your logic here
  Result := 'WaitingPostInstallState';
end;



end.
