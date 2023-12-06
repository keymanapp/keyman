(*
  Name:             WebUpdateCheck
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      5 Dec 2023

  Modified Date:
  Authors:          rcruickshank
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:
*)
unit RemoteUpdateCheck;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.IOUtils,
  Vcl.Forms,
  KeymanPaths,

  httpuploader,
  Keyman.System.UpdateCheckResponse,
  UfrmDownloadProgress,
  OnlineUpdateCheck;

type
  ERemoteUpdateCheck = class(Exception);

  TRemoteUpdateCheckResult = (wucUnknown, wucSuccess, wucNoUpdates, wucFailure, wucOffline);

  TRemoteUpdateCheckParams = record
    Keyman: TOnlineUpdateCheckParamsKeyman;
    Packages: array of TOnlineUpdateCheckParamsPackage;
    Result: TRemoteUpdateCheckResult;
  end;

  TRemoteUpdateCheckDownloadParams = record
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TRemoteUpdateCheck = class
  private
    FForce: Boolean;
    FParams: TRemoteUpdateCheckParams;

    FErrorMessage: string;

    FShowErrors: Boolean;
    FDownload: TRemoteUpdateCheckDownloadParams;
    FCheckOnly: Boolean;

    function DownloadUpdates: Boolean;
    procedure DoDownloadUpdates(SavePath: string; var Result: Boolean);
    function DoRun: TRemoteUpdateCheckResult;
  public
    function ResponseToParams(const ucr: TUpdateCheckResponse): TRemoteUpdateCheckParams;

    constructor Create(AForce : Boolean; ACheckOnly: Boolean = False);
    destructor Destroy; override;
    function Run: TRemoteUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

procedure LogMessage(LogMessage: string);

implementation

uses
  System.WideStrUtils,
  Vcl.Dialogs,
  Winapi.ShellApi,
  Winapi.Windows,
  Winapi.WinINet,

  GlobalProxySettings,
  KLog,
  keymanapi_TLB,
  KeymanVersion,
  Keyman.System.UpdateCheckStorage,
  kmint,
  ErrorControlledRegistry,
  RegistryKeys,
  Upload_Settings,
  utildir,
  utilexecute,
  OnlineUpdateCheckMessages,
  UfrmOnlineUpdateIcon,
  UfrmOnlineUpdateNewVersion,
  utilkmshell,
  utilsystem,
  utiluac,
  versioninfo;

{ TRemoteUpdateCheck }

constructor TRemoteUpdateCheck.Create(AForce, ACheckOnly: Boolean);
begin
  inherited Create;

  FShowErrors := True;
  FParams.Result := wucUnknown;

  FForce := AForce;
  FCheckOnly := ACheckOnly;

  KL.Log('TRemoteUpdateCheck.Create');
end;

destructor TRemoteUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and FShowErrors then
    LogMessage(FErrorMessage);

  KL.Log('TRemoteUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TRemoteUpdateCheck.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  inherited Destroy;
end;

function TRemoteUpdateCheck.Run: TRemoteUpdateCheckResult;
begin
  Result := DoRun;

  if Result in [ wucSuccess] then
  begin
    kmcom.Keyboards.Refresh;
    kmcom.Keyboards.Apply;
    kmcom.Packages.Refresh;
  end;

  FParams.Result := Result;
end;


procedure TRemoteUpdateCheck.DoDownloadUpdates(SavePath: string; var Result: Boolean);
var
  i, downloadCount: Integer;

    function DownloadFile(const url, savepath: string): Boolean;
    begin
      Result := False;
      with THttpUploader.Create(nil) do
      try
        Proxy.Server := GetProxySettings.Server;
        Proxy.Port := GetProxySettings.Port;
        Proxy.Username := GetProxySettings.Username;
        Proxy.Password := GetProxySettings.Password;
        Request.Agent := API_UserAgent;

        Request.SetURL(url);
        Upload;
        if Response.StatusCode = 200 then
        begin
          with TFileStream.Create(savepath, fmCreate) do
          try
            Write(Response.PMessageBody^, Response.MessageBodyLength);
          finally
            Free;
          end;
          Result := True;
        end
        else // I2742
          // If it fails we set to false but will try the other files
          Result := False;
          Exit;
      finally
        Free;
      end;
    end;


begin
  Result := False;
  try
    FDownload.TotalSize := 0;
    FDownload.TotalDownloads := 0;
    downloadCount := 0;

    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].Install then
      begin
        Inc(FDownload.TotalDownloads);
        Inc(FDownload.TotalSize, FParams.Packages[i].DownloadSize);

        FParams.Packages[i].SavePath := SavePath + FParams.Packages[i].FileName;
      end;

    if FParams.Keyman.Install then
    begin
      Inc(FDownload.TotalDownloads);
      Inc(FDownload.TotalSize, FParams.Keyman.DownloadSize);
      FParams.Keyman.SavePath := SavePath + FParams.Keyman.FileName;
    end;

    FDownload.StartPosition := 0;
    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].Install then
      begin
        if not DownloadFile(FParams.Packages[i].DownloadURL, FParams.Packages[i].SavePath) then // I2742
        begin
          FParams.Packages[i].Install := False; // Download failed but install other files
        end
        else
          Inc(downloadCount);
        FDownload.StartPosition := FDownload.StartPosition + FParams.Packages[i].DownloadSize;
      end;

    if FParams.Keyman.Install then
      if not DownloadFile(FParams.Keyman.DownloadURL, FParams.Keyman.SavePath) then  // I2742
      begin
        FParams.Keyman.Install := False;  // Download failed but user wants to install other files
      end;

    // There needs to be at least one file successfully downloaded to return
    // TRUE that files where downloaded
    if downloadCount > 0  then
      Result := True;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then LogMessage(S_OnlineUpdate_UnableToContact)
        else LogMessage(WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]));
      Result := False;
    end;
  end;
end;

function TRemoteUpdateCheck.DownloadUpdates: Boolean;
var
  i: Integer;
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
begin
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(TKeymanPaths.KeymanUpdateCachePath);
  { For now lets download all the updates. Set all as true }

  if FParams.Keyman.DownloadURL <> '' then
    FParams.Keyman.Install := True;

  for i := 0 to High(FParams.Packages) do
    FParams.Packages[i].Install := True;

  DoDownloadUpdates(DownloadBackGroundSavePath, DownloadResult);
  KL.Log('TRemoteUpdateCheck.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  Result := DownloadResult;

end;

function TRemoteUpdateCheck.DoRun: TRemoteUpdateCheckResult;
var
  flags: DWord;
  i: Integer;
  ucr: TUpdateCheckResponse;
  pkg: IKeymanPackage;
  downloadResult: boolean;
begin
  {FProxyHost := '';
  FProxyPort := 0;}

  { Check if user is currently online }
  if not InternetGetConnectedState(@flags, 0) then
  begin
    Result := wucOffline;
    Exit;
  end;

  { Verify that it has been at least 7 days since last update check }
  try
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := wucNoUpdates;
          Exit;
        end;
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) and not FForce then
        begin
          Result := wucNoUpdates;
          // TODO: This exit is just to remove the time check for testing.
          //Exit;
        end;

        {if ValueExists(SRegValue_UpdateCheck_UseProxy) and ReadBool(SRegValue_UpdateCheck_UseProxy) then
        begin
          FProxyHost := ReadString(SRegValue_UpdateCheck_ProxyHost);
          FProxyPort := StrToIntDef(ReadString(SRegValue_UpdateCheck_ProxyPort), 80);
        end;}
      end;
    finally
      Free;
    end;
  except
    { we will not run the check if an error occurs reading the settings }
    on E:Exception do
    begin
      Result := wucFailure;
      FErrorMessage := E.Message;
      Exit;
    end;
  end;

  Result := wucNoUpdates;

  try
    with THTTPUploader.Create(nil) do
    try
      Fields.Add('version', ansistring(CKeymanVersionInfo.Version));
      Fields.Add('tier', ansistring(CKeymanVersionInfo.Tier));
      if FForce
        then Fields.Add('manual', '1')
        else Fields.Add('manual', '0');

      for i := 0 to kmcom.Packages.Count - 1 do
      begin
        pkg := kmcom.Packages[i];

        // Due to limitations in PHP parsing of query string parameters names with
        // space or period, we need to split the parameters up. The legacy pattern
        // is still supported on the server side. Relates to #4886.
        Fields.Add(AnsiString('packageid_'+IntToStr(i)), AnsiString(pkg.ID));
        Fields.Add(AnsiString('packageversion_'+IntToStr(i)), AnsiString(pkg.Version));
        pkg := nil;
      end;

      Proxy.Server := GetProxySettings.Server;
      Proxy.Port := GetProxySettings.Port;
      Proxy.Username := GetProxySettings.Username;
      Proxy.Password := GetProxySettings.Password;

      Request.HostName := API_Server;
      Request.Protocol := API_Protocol;
      Request.UrlPath := API_Path_UpdateCheck_Windows;
      //OnStatus :=
      Upload;
      if Response.StatusCode = 200 then
      begin
        if ucr.Parse(Response.MessageBodyAsString, 'bundle', CKeymanVersionInfo.Version) then
        begin
          ResponseToParams(ucr);

          if FCheckOnly then
          begin
            // TODO: Refactor this
            TUpdateCheckStorage.SaveUpdateCacheData(ucr);
            Result := FParams.Result;
          end
          else if (Length(FParams.Packages) > 0) or (FParams.Keyman.DownloadURL <> '') then
          begin
            // TODO: Integrate in to the Background update state machine.
            // for now just go straight to DownloadUpdates
            downloadResult := DownloadUpdates;
            if DownloadResult then
            begin
              Result := wucSuccess;
            end;
          end;
        end
        else
        begin
          FErrorMessage := ucr.ErrorMessage;
          Result := wucFailure;
        end;
      end
      else
        raise ERemoteUpdateCheck.Create('Error '+IntToStr(Response.StatusCode));
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then FErrorMessage := S_OnlineUpdate_UnableToContact
        else FErrorMessage := WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]);
      Result := wucFailure;
    end;
    on E:Exception do
    begin
      FErrorMessage := E.Message;
      Result := wucFailure;
    end;
  end;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop_CU, True) then
      WriteDateTime(SRegValue_LastUpdateCheckTime, Now);
  finally
    Free;
  end;
end;

function TRemoteUpdateCheck.ResponseToParams(const ucr: TUpdateCheckResponse): TRemoteUpdateCheckParams;
var
  i, j, n: Integer;
  pkg: IKeymanPackage;
begin
  SetLength(FParams.Packages,0);
  for i := Low(ucr.Packages) to High(ucr.Packages) do
  begin
    n := kmcom.Packages.IndexOf(ucr.Packages[i].ID);
    if n >= 0 then
    begin
      pkg := kmcom.Packages[n];
      j := Length(FParams.Packages);
      SetLength(FParams.Packages, j+1);
      FParams.Packages[j].NewID := ucr.Packages[i].NewID;
      FParams.Packages[j].ID := ucr.Packages[i].ID;
      FParams.Packages[j].Description := ucr.Packages[i].Name;
      FParams.Packages[j].OldVersion := pkg.Version;
      FParams.Packages[j].NewVersion := ucr.Packages[i].NewVersion;
      FParams.Packages[j].DownloadSize := ucr.Packages[i].DownloadSize;
      FParams.Packages[j].DownloadURL := ucr.Packages[i].DownloadURL;
      FParams.Packages[j].FileName := ucr.Packages[i].FileName;
      pkg := nil;
    end
    else
      FErrorMessage := 'Unable to find package '+ucr.Packages[i].ID;
  end;

  case ucr.Status of
    ucrsNoUpdate:
      begin
        FErrorMessage := ucr.ErrorMessage;
      end;
    ucrsUpdateReady:
      begin
        FParams.Keyman.OldVersion := ucr.CurrentVersion;
        FParams.Keyman.NewVersion := ucr.NewVersion;
        FParams.Keyman.DownloadURL := ucr.InstallURL;
        FParams.Keyman.DownloadSize := ucr.InstallSize;
        FParams.Keyman.FileName := ucr.FileName;
      end;
  end;

  Result := FParams;
end;

 // temp wrapper for converting showmessage to logs don't know where
 // if nt using klog
 procedure LogMessage(LogMessage: string);
 begin
   KL.Log(LogMessage);
 end;

end.
