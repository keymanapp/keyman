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
                    16 Jan 2009 - mcdurdin - I1730 - Check update of keyboards (refactor from global)
                    14 Jun 2009 - mcdurdin - I1704 - Activation through a firewall
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 Oct 2010 - mcdurdin - I2513 - Online update for package fails to install when package installed by admin
                    21 Feb 2011 - mcdurdin - I2738 - Online update should be silent
                    21 Feb 2011 - mcdurdin - I2742 - No error message is given if downloading a file fails in online update
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit OnlineUpdateCheck;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  Vcl.Forms,

  httpuploader,
  Keyman.System.UpdateCheckResponse,
  UfrmDownloadProgress;

type
  EOnlineUpdateCheck = class(Exception);

  TOnlineUpdateCheckResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucFailure, oucOffline);

  TOnlineUpdateCheckParamsPackage = record
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

  TOnlineUpdateCheckParamsKeyman = record
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TOnlineUpdateCheckParams = record
    Keyman: TOnlineUpdateCheckParamsKeyman;
    Packages: array of TOnlineUpdateCheckParamsPackage;
    Result: TOnlineUpdateCheckResult;
  end;

  TOnlineUpdateCheckDownloadParams = record
    Owner: TfrmDownloadProgress;
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TOnlineUpdateCheck = class
  private
    FOwner: TCustomForm;
    FSilent: Boolean;
    FForce: Boolean;
    FParams: TOnlineUpdateCheckParams;

    FErrorMessage: string;

    DownloadTempPath: string;

    FShowErrors: Boolean;
    FDownload: TOnlineUpdateCheckDownloadParams;
    {
      Sets the download path and then calls the DoDownloadUpdates function. If
      the user confirms the download (by clicking OK), the downloaded files are
      marked to be deleted on the next system reboot. If the user cancels the
      download, the downloaded files are deleted immediately.

      @return  True  if the download was successful and the user confirmed,
                False if the user canceled the download.
    }
    function DownloadUpdates: Boolean;
    {
      Downloads updates by performing file downloads one by one, while keeping
      track of the download progress. This function is called by passing an
      instance of TfrmDownloadProgress as the AOwner parameter.

      @params  AOwner  The owner form that initiates the download process.
               Result  A Boolean value indicating the overall result of the
               download process.

      Notes:
      - The function handles file downloads sequentially, one after another.
      - In case of a download failure, a dialog is displayed to prompt the user
        about continuing with other updates.
      - The function assumes that the necessary download URLs and file paths
        have been properly set before invoking this function.
    }
    procedure DoDownloadUpdates(AOwner: TfrmDownloadProgress; var Result: Boolean);
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

      @returns  A TOnlineUpdateCheckResult indicating the result of the update
      check.
    }
    function DoRun: TOnlineUpdateCheckResult;
    {
      Displays the Update form, which allows selection of updates to be manual
      downloaded.

      This procedure shows the Update form, which contains controls for
      downloading and performing updates. If elevation of permissions is
      required for certain operations, it will be handled internally. If the
      user does not have administrative permissions, a dialog will be raised to
      inform them to use an account with administration privileges.
    }
    procedure ShowUpdateForm;
    procedure ShutDown;
    { Works out the status of the downloaded }
    procedure DownloadUpdatesHTTPStatus(Sender: THTTPUploader;
      const Message: string; Position, Total: Int64); // I2855
    {
      Installs the packages using the kmcom API and deletes the package from the
      specified SavePath. This procedure utilizes the kmcom API to install the
      packages.
    }
    procedure DoInstallKeyman;
    {
      Installs the Keyman file using either msiexec.exe or the setup launched in
      a separate shell.

      @params  Package  The package to be installed.

      @returns True  if the installation is successful, False otherwise.
    }
    function DoInstallPackage(Package: TOnlineUpdateCheckParamsPackage): Boolean;
    {
      SavePackageUpgradesToDownloadTempPath saves any new package IDs to a
      single file in the download tempPath. This procedure saves the IDs of any
      new packages to a file named "upgrade_packages.inf" in the download
      tempPath.
    }
    procedure SavePackageUpgradesToDownloadTempPath;
  public

  public
    constructor Create(AOwner: TCustomForm; AForce, ASilent: Boolean);
    destructor Destroy; override;
    function Run: TOnlineUpdateCheckResult;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

  IOnlineUpdateSharedData = interface
    ['{7442A323-C1E3-404B-BEEA-5B24A52BBB0E}']
    function Params: TOnlineUpdateCheckParams;
  end;

  TOnlineUpdateSharedData = class(TInterfacedObject, IOnlineUpdateSharedData)
  private
    FParams: TOnlineUpdateCheckParams;
  public
    constructor Create(AParams: TOnlineUpdateCheckParams);
    function Params: TOnlineUpdateCheckParams;
  end;

procedure OnlineUpdateAdmin(OwnerForm: TCustomForm; Path: string);

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
  OnlineUpdateCheckMessages,
  UfrmOnlineUpdateIcon,
  UfrmOnlineUpdateNewVersion,
  utilkmshell,
  utilsystem,
  utiluac,
  versioninfo;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';

{ TOnlineUpdateCheck }

constructor TOnlineUpdateCheck.Create(AOwner: TCustomForm; AForce, ASilent: Boolean);
begin
  inherited Create;

  FOwner := AOwner;

  FShowErrors := True;
  FParams.Result := oucUnknown;

  FSilent := ASilent;
  FForce := AForce;

  KL.Log('TOnlineUpdateCheck.Create');
end;

destructor TOnlineUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and not FSilent and FShowErrors then
    ShowMessage(FErrorMessage);

  if FParams.Result = oucShutDown then
    ShutDown;

  KL.Log('TOnlineUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TOnlineUpdateCheck.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  inherited Destroy;
end;

function TOnlineUpdateCheck.Run: TOnlineUpdateCheckResult;
begin
  Result := DoRun;

  DownloadUpdatesBackground;
  if Result in [oucShutDown, oucSuccess] then
  begin
    kmcom.Keyboards.Refresh;
    kmcom.Keyboards.Apply;
    kmcom.Packages.Refresh;
  end;

  FParams.Result := Result;
end;

procedure TOnlineUpdateCheck.DownloadUpdatesHTTPStatus(Sender: THTTPUploader; const Message: string; Position, Total: Int64); // I2855
begin
  FDownload.Owner.HTTPStatus(Sender, Message, Position+FDownload.StartPosition, FDownload.TotalSize);
end;

procedure TOnlineUpdateCheck.DoDownloadUpdates(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  i: Integer;

    function DownloadFile(const url, savepath: string): TModalResult;
    begin
      Result := mrCancel;
      with THttpUploader.Create(AOwner) do
      try
        OnCheckCancel := AOwner.HTTPCheckCancel;
        OnStatus := DownloadUpdatesHTTPStatus;
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
          Result := mrOk;
        end
        else // I2742
          case MessageDlg('Unable to download file '+ExtractFileName(savepath)+' from the Keyman Update Server: the error code '+
              IntToStr(Response.StatusCode)+' was received.  Do you want to continue to try and install any other selected updates?', mtError, mbYesNoCancel, 0) of
            mrYes: Result := mrYes;
            mrNo, mrCancel: Exit;
            else Exit;
          end;
      finally
        Free;
      end;
    end;


begin
  Result := False;
  try
    FDownload.TotalSize := 0;
    FDownload.Owner := AOwner;
    FDownload.TotalDownloads := 0;

    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].Install then
      begin
        Inc(FDownload.TotalDownloads);
        Inc(FDownload.TotalSize, FParams.Packages[i].DownloadSize);

        FParams.Packages[i].SavePath := DownloadTempPath + FParams.Packages[i].FileName;
      end;

    if FParams.Keyman.Install then
    begin
      Inc(FDownload.TotalDownloads);
      Inc(FDownload.TotalSize, FParams.Keyman.DownloadSize);
      FParams.Keyman.SavePath := DownloadTempPath + FParams.Keyman.FileName;
    end;

    FDownload.StartPosition := 0;
    for i := 0 to High(FParams.Packages) do
      if FParams.Packages[i].Install then
      begin
        case DownloadFile(FParams.Packages[i].DownloadURL, FParams.Packages[i].SavePath) of  // I2742
          mrCancel: Exit;
          mrOk: ;
          mrYes: FParams.Packages[i].Install := False; // Download failed but user wants to install other files
        end;
        FDownload.StartPosition := FDownload.StartPosition + FParams.Packages[i].DownloadSize;
      end;

    if FParams.Keyman.Install then
      case DownloadFile(FParams.Keyman.DownloadURL, FParams.Keyman.SavePath) of  // I2742
        mrCancel: Exit;
        mrOk: ;
        mrYes: FParams.Keyman.Install := False;  // Download failed but user wants to install other files
      end;

    Result := True;
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

function TOnlineUpdateCheck.DownloadUpdates: Boolean;
var
  i: Integer;
begin
  DownloadTempPath := IncludeTrailingPathDelimiter(CreateTempPath);

  with TfrmDownloadProgress.Create(nil) do
  try
    Callback := DoDownloadUpdates;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;

  for i := 0 to High(FParams.Packages) do
    if FParams.Packages[i].Install and FileExists(FParams.Packages[i].SavePath) then
      if Result
        then DeleteFileOnReboot(FParams.Packages[i].SavePath)
        else System.SysUtils.DeleteFile(FParams.Packages[i].SavePath);

  if FParams.Keyman.Install and FileExists(FParams.Keyman.SavePath) then
    if Result
      then DeleteFileOnReboot(FParams.Keyman.SavePath)
      else System.SysUtils.DeleteFile(FParams.Keyman.SavePath);

  if not Result
    then RemoveDir(ExcludeTrailingPathDelimiter(DownloadTempPath))
    else DeleteFileOnReboot(ExcludeTrailingPathDelimiter(DownloadTempPath));
end;


procedure TOnlineUpdateCheck.DoDownloadUpdatesBackground(SavePath: string; var Result: Boolean);
var
  i: Integer;

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
        end;
        FDownload.StartPosition := FDownload.StartPosition + FParams.Packages[i].DownloadSize;
      end;

    if FParams.Keyman.Install then
      if not DownloadFile(FParams.Keyman.DownloadURL, FParams.Keyman.SavePath) then  // I2742
      begin
        FParams.Keyman.Install := False;  // Download failed but user wants to install other files
      end;
    Result := True;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then KL.Log(S_OnlineUpdate_UnableToContact)
        else KL.Log(WideFormat(S_OnlineUpdate_UnableToContact_Error, [E.Message]));
      Result := False;
    end;
  end;
end;

function TOnlineUpdateCheck.DownloadUpdatesBackground: Boolean;
var
  i: Integer;
  DownloadBackGroundSavePath : String;
  DownloadResult : Boolean;
begin
  //DownloadTempPath := IncludeTrailingPathDelimiter(CreateTempPath);
  DownloadBackGroundSavePath := IncludeTrailingPathDelimiter(GetFolderPath(CSIDL_COMMON_APPDATA) + SFolder_CachedUpdateFiles);
  //DownloadBackGroundSavePath :=  DownloadBackGroundSavePath + 'RCTest.txt';
  { For now lets download all the updates. We need to take these from the user via check box form }

  if FParams.Keyman.DownloadURL <> '' then
    FParams.Keyman.Install := True;

  for i := 0 to High(FParams.Packages) do
    FParams.Packages[i].Install := True;

  // Download files
  DoDownloadUpdatesBackground(DownloadBackGroundSavePath, DownloadResult);
  KL.Log('TOnlineUpdateCheck.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  ShowMessage('TOnlineUpdateCheck.DownloadUpdatesBackground: DownloadResult = '+IntToStr(Ord(DownloadResult)));
  // Set registry keys
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanEngine_LM, True) then
      WriteString(SRegValue_Install_Update, BoolToStr(True));
  finally
    Free;
  end;
  Result := DownloadResult;
end;




function TOnlineUpdateCheck.DoInstallPackage(Package: TOnlineUpdateCheckParamsPackage): Boolean;
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

procedure TOnlineUpdateCheck.DoInstallKeyman;
var
  s: string;
  FResult: Boolean;
begin
  s := LowerCase(ExtractFileExt(FParams.Keyman.SavePath));
  if s = '.msp' then
    FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /p "'+FParams.Keyman.SavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
  else if s = '.msi' then
    FResult := TUtilExecute.Shell(0, 'msiexec.exe', '', '/qb /i "'+FParams.Keyman.SavePath+'" AUTOLAUNCHPRODUCT=1')  // I3349
  else if s = '.exe' then
    FResult := TUtilExecute.Shell(0, FParams.Keyman.SavePath, '', '-au')  // I3349
  else
    Exit;
  if not FResult then
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TOnlineUpdateCheck.SavePackageUpgradesToDownloadTempPath;
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

procedure TOnlineUpdateCheck.ShowUpdateForm;
var
  i: Integer;
  FRequiresAdmin: Boolean;
  FOwnerHandle: THandle;
begin
  if Assigned(FOwner)
    then FOwnerHandle := FOwner.Handle
    else FOwnerHandle := Application.Handle;

  { We have an update available }
  with OnlineUpdateNewVersion(FOwner) do
  try
    Params := Self.FParams;
    if ShowModal <> mrYes then
    begin
      Self.FParams.Result := oucUnknown;
      Self.FErrorMessage := '';
      Exit;
    end;

    Self.FParams := Params;
  finally
    Free;
  end;

  if not DownloadUpdates then
  begin
    Self.FParams.Result := oucUnknown;  // I2742
    Exit;
  end
  else
  begin
    if not kmcom.SystemInfo.IsAdministrator then
    begin
      FRequiresAdmin := FParams.Keyman.Install;
      for i := 0 to High(FParams.Packages) do
        if FParams.Packages[i].Install then
        begin
          FRequiresAdmin := True;
          Break;
        end;
    end
    else
      FRequiresAdmin := False;

    if FRequiresAdmin then
    begin
      if CanElevate then
      begin
        SavePackageUpgradesToDownloadTempPath;
        if WaitForElevatedConfiguration(FOwnerHandle, '-ou "'+DownloadTempPath+'"', not FParams.Keyman.Install) <> 0 then  // I2513
          FParams.Result := oucFailure
        else if FParams.Keyman.Install then
          FParams.Result := oucShutDown
        else
          FParams.Result := oucSuccess;
      end
      else
      begin
        ShowMessage('Some of these updates require an Administrator to complete installation.  Please login as an Administrator and re-run the update.');
        FParams.Result := oucFailure;
      end;
    end
    else
    begin
      FParams.Result := oucSuccess;
      for i := 0 to High(FParams.Packages) do
        if FParams.Packages[i].Install then
          if not DoInstallPackage(FParams.Packages[i]) then FParams.Result := oucFailure;

      if FParams.Keyman.Install then
      begin
        DoInstallKeyman;
        FParams.Result := oucShutDown;
      end;
    end;
  end;
end;

procedure TOnlineUpdateCheck.ShutDown;
begin
  if Assigned(Application) then
    Application.Terminate;
end;

function TOnlineUpdateCheck.DoRun: TOnlineUpdateCheckResult;
var
  flags: DWord;
  i, n: Integer;
  pkg: IKeymanPackage;
  j: Integer;
  ucr: TUpdateCheckResponse;
begin
  {FProxyHost := '';
  FProxyPort := 0;}

  { Check if user is currently online and don't force dialup dialog to appear if not }
  if FSilent then
    if not InternetGetConnectedState(@flags, 0) then
    begin
      Result := oucOffline;
      Exit;
    end;

  { Verify that it has been at least 7 days since last update check - only if FSilent = TRUE }
 // TODO: OU This is just commented out to by pass the 7 day check for testing.

//  try
//    with TRegistryErrorControlled.Create do  // I2890
//    try
//      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
//      begin
//        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) and not FForce then
//        begin
//          Result := oucNoUpdates;
//          Exit;
//        end;
//        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) and FSilent and not FForce then
//        begin
//          Result := oucNoUpdates;
//          Exit;
//        end;
//
//        {if ValueExists(SRegValue_UpdateCheck_UseProxy) and ReadBool(SRegValue_UpdateCheck_UseProxy) then
//        begin
//          FProxyHost := ReadString(SRegValue_UpdateCheck_ProxyHost);
//          FProxyPort := StrToIntDef(ReadString(SRegValue_UpdateCheck_ProxyPort), 80);
//        end;}
//      end;
//    finally
//      Free;
//    end;
//  except
//    { we will not run the check if an error occurs reading the settings }
//    on E:Exception do
//    begin
//      Result := oucFailure;
//      FErrorMessage := E.Message;
//      Exit;
//    end;
//  end;

  Result := oucNoUpdates;

  try
    with THTTPUploader.Create(nil) do
    try
      ShowUI := not FSilent;
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
          { TODO: OU instead of showing a form to update it just has to go and download }

          if (Length(FParams.Packages) > 0) or (FParams.Keyman.DownloadURL <> '') then
          begin
            if not FSilent then
              ShowUpdateForm
            else
            begin
              ShowUpdateIcon;
            end;
            Result := FParams.Result;
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
    if OpenKey(SRegKey_KeymanDesktop_CU, True) then
      WriteDateTime(SRegValue_LastUpdateCheckTime, Now);
  finally
    Free;
  end;
end;

procedure OnlineUpdateAdmin(OwnerForm: TCustomForm; Path: string);
var
  Package: TOnlineUpdateCheckParamsPackage;
  f: TSearchRec;
  ext: string;
  FPackageUpgradeList: TStringList;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  FPackageUpgradeList := TStringList.Create;
  with TOnlineUpdateCheck.Create(OwnerForm,False,True) do
  try
    if FileExists(Path + SPackageUpgradeFileName) then
      FPackageUpgradeList.LoadFromFile(Path + SPackageUpgradeFilename);

    if FindFirst(Path + '*.k??', 0, f) = 0 then
    begin
      repeat
        ext := LowerCase(ExtractFileExt(f.Name));
        if (ext = '.kmx') or (ext = '.kmp') then
        begin
          Package.SavePath := Path + f.Name;
          Package.NewID := ChangeFileExt(f.Name, '');
          Package.ID := FPackageUpgradeList.Values[Package.NewID];
          DoInstallPackage(Package);
        end;
      until FindNext(f) <> 0;

      System.SysUtils.FindClose(f);
    end;

    if FindFirst(Path + '*.exe', 0, f) = 0 then
    begin
      FParams.Keyman.SavePath := Path + f.Name;
      DoInstallKeyman;
      System.SysUtils.FindClose(f);
    end;
  finally
    Free;
    FPackageUpgradeList.Free;
  end;
end;

{ TOnlineUpdateSharedData }

constructor TOnlineUpdateSharedData.Create(AParams: TOnlineUpdateCheckParams);
begin
  inherited Create;
  FParams := AParams;
end;

function TOnlineUpdateSharedData.Params: TOnlineUpdateCheckParams;
begin
  Result := FParams;
end;

end.
