(*
  Name:             OnlineUpdateCheck
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Support download progress
                    12 Dec 2006 - mcdurdin - Don't shutdown if update is cancelled
                    14 Dec 2006 - mcdurdin - Only test for patches, not downloads
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Show nice error message for online update check when offline
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    30 May 2007 - mcdurdin - I817 - Pass version information from external application
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    31 Mar 2011 - mcdurdin - I2849 - Developer online update check fails because it looks for a patch update
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit Keyman.Developer.UI.TikeOnlineUpdateCheck;  // I3306

interface

uses
  Classes,
  SysUtils,
  UfrmDownloadProgress;

type
  EOnlineUpdateCheck = class(Exception);

  TOnlineUpdateCheckResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucFailure, oucAsync, oucOffline);

  TOnlineUpdateCheckParams = record
    NewVersion, InstallURL: string;  // I2849
    InstallSize: Integer;  // I2849
    Result: TOnlineUpdateCheckResult;
  end;

  TOnlineUpdateCheck = class(TThread)
  private
    FOnlineProductID: Integer;
    FThread: Boolean;
    FSilent: Boolean;
    FForce: Boolean;
    FCurrentVersion: string;
    FRootKey: string;
    FParams: TOnlineUpdateCheckParams;

    FErrorMessage: string;
    DownloadUpdate_Path: string;
    DownloadUpdate_URL: string;
    FProxyServer: string;
    FProxyPort: Integer;
    FProxyUsername, FProxyPassword: string;
    FShowErrors: Boolean;

    function DownloadUpdate(URL: string; var DownloadPath: string): Boolean;
    procedure DoDownloadUpdate(AOwner: TfrmDownloadProgress; var Result: Boolean);
    function DoRun: TOnlineUpdateCheckResult;
    procedure SyncShowUpdateForm;
    procedure SyncShutDown;
  private
    class var FRunning: Boolean;
  public

  protected
    procedure Execute; override;

  public
    constructor Create(ARootKey: string; AOnlineProductID: Integer; AForce, ASilent, AThread: Boolean; AProxyServer: string; AProxyPort: Integer; AProxyUsername, AProxyPassword: string);
    destructor Destroy; override;
    function Run: TOnlineUpdateCheckResult;
    class function Running: Boolean;
    property CurrentVersion: string read FCurrentVersion write FCurrentVersion;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

implementation

uses
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  httpuploader,
  KLog,
  ErrorControlledRegistry,
  Keyman.System.UpdateCheckResponse,
  KeymanVersion,
  RegistryKeys,
  ShellApi,
  Upload_Settings,
  Keyman.Developer.System.TikeOnlineUpdateCheckMessages,
  Keyman.Developer.UI.UfrmTikeOnlineUpdateNewVersion,
  utilexecute,
  utilsystem,
  versioninfo,
  Windows,
  WinINet;

{ TOnlineUpdateCheck }

constructor TOnlineUpdateCheck.Create(ARootKey: string; AOnlineProductID: Integer; AForce, ASilent, AThread: Boolean; AProxyServer: string; AProxyPort: Integer; AProxyUsername, AProxyPassword: string);
begin
  Assert(not FRunning);
  FRunning := True;

  FShowErrors := True;
  FCurrentVersion := GetVersionString;
  FParams.Result := oucUnknown;
  FRootKey := ARootKey;
  FOnlineProductID := AOnlineProductID;
  FSilent := ASilent;
  FForce := AForce;
  FThread := AThread;
  FProxyServer := AProxyServer;
  FProxyPort := AProxyPort;
  FProxyUsername := AProxyUsername;
  FProxyPassword := AProxyPassword;


  KL.Log('TOnlineUpdateCheck.Create: FRootKey = '+FRootKey);
  KL.Log('TOnlineUpdateCheck.Destroy: OnlineProductID = '+IntToStr(FOnlineProductID));
  inherited Create(True);
end;

destructor TOnlineUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and not FSilent and FShowErrors then
    ShowMessage(FErrorMessage);

  if FParams.Result = oucShutDown then
    SyncShutDown;

  KL.Log('TOnlineUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TOnlineUpdateCheck.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  FRunning := False;

  inherited Destroy;
end;

function TOnlineUpdateCheck.Run: TOnlineUpdateCheckResult;
begin
  if FThread then
  begin
    Start;  // I3309
    Result := oucAsync;
  end
  else
  begin
    Result := DoRun;
    FParams.Result := Result;
  end;
end;

class function TOnlineUpdateCheck.Running: Boolean;
begin
  Result := FRunning;
end;

procedure TOnlineUpdateCheck.DoDownloadUpdate(AOwner: TfrmDownloadProgress; var Result: Boolean);
begin
  Result := False;
  try
    with THTTPUploader.Create(AOwner) do
    try
      OnCheckCancel := AOwner.HTTPCheckCancel;
      OnStatus := AOwner.HTTPStatus;
      Proxy.Server := FProxyServer;
      Proxy.Port := FProxyPort;
      Proxy.Username := FProxyUsername;
      Proxy.Password := FProxyPassword;
      Request.Agent := API_UserAgent;
      Request.SetURL(DownloadUpdate_URL);
      Upload;
      if Response.StatusCode = 200 then
      begin
        with TFileStream.Create(DownloadUpdate_Path, fmCreate) do
        try
          Write(Response.PMessageBody^, Response.MessageBodyLength);
        finally
          Free;
        end;
        Result := True;
      end;
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then ShowMessage(S_OnlineUpdate_UnableToContact)
        else ShowMessage(WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]));
      Result := False;
    end;
  end;
end;

function TOnlineUpdateCheck.DownloadUpdate(URL: string; var DownloadPath: string): Boolean;
var
  buf: array[0..260] of char;
  n: Integer;
begin
  Result := False;

  GetTempPath(260, buf);

  n := LastDelimiter('/', URL);
  if n = 0 then
  begin
    ShowMessage('Unable to download file - not a recognised update file: '+URL);
    Exit;
  end;

  DownloadUpdate_Path := IncludeTrailingPathDelimiter(buf) + Copy(URL, n+1, Length(URL));
  DownloadUpdate_URL := URL;

  DownloadPath := DownloadUpdate_Path;

  with TfrmDownloadProgress.Create(nil) do
  try
    Callback := DoDownloadUpdate;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;

  if FileExists(DownloadPath) then
    if Result
      then DeleteFileOnReboot(DownloadPath)
      else SysUtils.DeleteFile(DownloadPath);
end;

procedure TOnlineUpdateCheck.SyncShowUpdateForm;
var
  s, FSavePath: string;
  FResult: Boolean;
begin
  { We have an update available }
  with OnlineUpdateNewVersion(nil) do
  try
    CurrentVersion := GetVersionString;
    NewVersion := FParams.NewVersion;
    InstallURL := FParams.InstallURL;  // I2849
    InstallSize := FParams.InstallSize;  // I2849
    if ShowModal = mrYes then
    begin
      if DownloadUpdate(InstallURL, FSavePath) then  // I2849
      begin
        { The user asked to install a patch for the update }
        s := LowerCase(ExtractFileExt(FSavePath));  // I2849
        if s = '.msp' then
          FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /p "'+FSavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
        else if s = '.msi' then
          FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /i "'+FSavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
        else if s = '.exe' then
          FResult := TUtilExecute.Shell(0, FSavePath, '', '-au')  // I3349
        else
          FResult := False;

        if FResult
          then FParams.Result := oucShutDown
          else FParams.Result := oucFailure;

      end
      else
        FParams.Result := oucSuccess;
    end
    else
      FParams.Result := oucSuccess;
  finally
    Free;
  end;
end;

procedure TOnlineUpdateCheck.SyncShutDown;
begin
  if Assigned(Application) then
    Application.Terminate;
end;

function TOnlineUpdateCheck.DoRun: TOnlineUpdateCheckResult;
var
  ucr: TUpdateCheckResponse;
  flags: DWord;
begin
  { Check if user is currently online and don't force dialup dialog to appear if not }
  if FSilent then
    if not InternetGetConnectedState(@flags, 0) then
    begin
      Result := oucOffline;
      Exit;
    end;

  { Verify that it has been at least 7 days since last update check - only if FSilent = TRUE }
  try
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(FRootKey) then
      begin
        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := oucNoUpdates;
          Exit;
        end;
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) and FSilent then
        begin
          Result := oucNoUpdates;
          Exit;
        end;
      end;
    finally
      Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := oucFailure;
      FErrorMessage := E.Message;
      Exit;
    end;
  end;

  Result := oucNoUpdates;

  try
    with THTTPUploader.Create(nil) do
    try
      Fields.Add('version', ansistring(CKeymanVersionInfo.Version));
      Fields.Add('tier', ansistring(CKeymanVersionInfo.Tier));
      if FForce
        then Fields.Add('manual', '1')
        else Fields.Add('manual', '0');

      Proxy.Server := FProxyServer;
      Proxy.Port := FProxyPort;
      Proxy.Username := FProxyUsername;
      Proxy.Password := FProxyPassword;

      Request.HostName := API_Server;
      Request.Protocol := API_Protocol;
      Request.UrlPath := API_Path_UpdateCheck_Developer;
      //OnStatus :=
      Upload;
      if Response.StatusCode = 200 then
      begin
        // TODO: Refactor with setup and desktop updatechecks

        if ucr.Parse(Response.MessageBodyAsString, 'developer', FCurrentVersion) then
        begin
          case ucr.Status of
            ucrsNoUpdate:
              begin
                FErrorMessage := ucr.ErrorMessage;
              end;
            ucrsUpdateReady:
              begin
                FParams.NewVersion := ucr.NewVersion;
                FParams.InstallURL := ucr.InstallURL;
                FParams.InstallSize := ucr.InstallSize;
                Synchronize(SyncShowUpdateForm);
                Result := FParams.Result;
              end;
          end;
        end
        else
        begin
          FErrorMessage := ucr.ErrorMessage;
          Result := oucFailure;
        end;
      end
      else
        raise EOnlineUpdateCheck.Create('Error '+IntToStr(Response.StatusCode));
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then FErrorMessage := S_OnlineUpdate_UnableToContact
        else FErrorMessage := WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]);
      Result := oucFailure;
    end;
    on E:Exception do
    begin
      FErrorMessage := E.Message;
      Result := oucFailure;
    end;
  end;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(FRootKey, True) then
      WriteDateTime(SRegValue_LastUpdateCheckTime, Now);
  finally
    Free;
  end;
end;

procedure TOnlineUpdateCheck.Execute;
begin
  FreeOnTerminate := True;
  FParams.Result := DoRun;
end;

end.
