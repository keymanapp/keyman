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
unit Keyman.System.RemoteUpdateCheck;  // I3306

interface
uses
  System.Classes,
  System.SysUtils,
  KeymanPaths,
  httpuploader,
  Keyman.System.UpdateCheckResponse,
  OnlineUpdateCheck;

const
  CheckPeriod: Integer = 7; // Days between checking for updates

type
  ERemoteUpdateCheck = class(Exception);

  TRemoteUpdateCheckResult = (wucUnknown, wucSuccess, wucNoUpdates, wucFailure, wucOffline);

  TRemoteUpdateCheckDownloadParams = record
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TRemoteUpdateCheck = class
  private
    FForce: Boolean;
    FRemoteResult: TRemoteUpdateCheckResult;

    FErrorMessage: string;

    FShowErrors: Boolean;
    FDownload: TRemoteUpdateCheckDownloadParams;
    FCheckOnly: Boolean;

    function DownloadUpdates(Params: TUpdateCheckResponse) : Boolean;
    procedure DoDownloadUpdates(SavePath: string; Params: TUpdateCheckResponse;  var Result: Boolean);
    function DoRun: TRemoteUpdateCheckResult;
  public

    constructor Create(AForce : Boolean; ACheckOnly: Boolean = False);
    destructor Destroy; override;
    function Run: TRemoteUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

procedure LogMessage(LogMessage: string);
function CheckForUpdates: Boolean;

implementation

uses
  System.WideStrUtils,
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

  OnlineUpdateCheckMessages;

{ TRemoteUpdateCheck }

constructor TRemoteUpdateCheck.Create(AForce, ACheckOnly: Boolean);
begin
  inherited Create;

  FShowErrors := True;
  FRemoteResult := wucUnknown;

  FForce := AForce;
  FCheckOnly := ACheckOnly;

  KL.Log('TRemoteUpdateCheck.Create');
end;

destructor TRemoteUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and FShowErrors then
    LogMessage(FErrorMessage);

  KL.Log('TRemoteUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TRemoteUpdateCheck.Destroy: FRemoteResult = '+IntToStr(Ord(FRemoteResult)));

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

  FRemoteResult := Result;
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

  { Verify that it has been at least CheckPeriod days since last update check }
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
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < CheckPeriod) and not FForce then
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
          //ResponseToParams(ucr);

          if FCheckOnly then
          begin
            // TODO: Refactor this
            TUpdateCheckStorage.SaveUpdateCacheData(ucr);
            Result := FRemoteResult;
          end
          // TODO: #10038
          // Integerate into state machine. in the download state
          // the process can call LoadUpdateCacheData if needed to get the
          // response result.
          else if (Length(ucr.Packages) > 0) or (ucr.InstallURL <> '') then
          begin
            downloadResult := DownloadUpdates(ucr);
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

 // temp wrapper for converting showmessage to logs don't know where
 // if nt using klog
 procedure LogMessage(LogMessage: string);
 begin
   KL.Log(LogMessage);
 end;

function CheckForUpdates: Boolean;
begin
{ Verify that it has been at least CheckPeriod days since last update check }
  try
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) then
        begin
          Result := False;
          Exit;
        end;
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) > CheckPeriod) then
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
      Free;
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
