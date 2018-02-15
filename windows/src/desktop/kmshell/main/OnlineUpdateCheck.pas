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
  System.UITypes,
  Classes,
  httpuploader,
  SysUtils,
  UfrmDownloadProgress;

type
  EOnlineUpdateCheck = class(Exception);

  TOnlineUpdateCheckResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucFailure, oucSuccessReboot, oucOffline);

  TOnlineUpdateCheckParamsPackage = record
    ID: string;
    NewID: string;
    Description: string;
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TOnlineUpdateCheckParamsKeyman = record
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
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
    FSilent: Boolean;
    FForce: Boolean;
    FCurrentVersion: string;
    FParams: TOnlineUpdateCheckParams;

    FErrorMessage: string;

    DownloadTempPath: string;

    FShowErrors: Boolean;
    FDownload: TOnlineUpdateCheckDownloadParams;

    function DownloadUpdates: Boolean;
    procedure DoDownloadUpdates(AOwner: TfrmDownloadProgress; var Result: Boolean);
    function DoRun: TOnlineUpdateCheckResult;
    procedure ShowUpdateForm;
    procedure ShutDown;
    procedure DownloadUpdatesHTTPStatus(Sender: THTTPUploader;
      const Message: string; Position, Total: Int64); // I2855
    procedure DoInstallKeyman;
    function DoInstallPackage(Package: TOnlineUpdateCheckParamsPackage): Boolean;
    procedure SavePackageUpgradesToDownloadTempPath;
  public

  public
    constructor Create(AForce, ASilent: Boolean);
    destructor Destroy; override;
    function Run: TOnlineUpdateCheckResult;
    property CurrentVersion: string read FCurrentVersion write FCurrentVersion;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

procedure OnlineUpdateAdmin(Path: string);

implementation

uses
  Vcl.Dialogs,
  Vcl.Forms,
  xmlintf,
  GlobalProxySettings,
  JsonUtil,
  KLog,
  keymanapi_TLB,
  kmint,
  OnlineConstants,
  ErrorControlledRegistry,
  RegistryKeys,
  ShellApi,
  System.JSON,
  Upload_Settings,
  utildir,
  utilexecute,
  OnlineUpdateCheckMessages,
  UfrmOnlineUpdateIcon,
  UfrmOnlineUpdateNewVersion,
  utilkmshell,
  utilsystem,
  utiluac,
  versioninfo,
  WideStrUtils,
  Windows,
  WinINet;

const
  SPackageUpgradeFilename = 'upgrade_packages.inf';

{ TOnlineUpdateCheck }

constructor TOnlineUpdateCheck.Create(AForce, ASilent: Boolean);
begin
  inherited Create;

  FShowErrors := True;
  FCurrentVersion := GetVersionString;
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

  if Result in [oucShutDown, oucSuccess, oucSuccessReboot] then
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

    function GetSavePath(const url: string): string;
    var
      n: Integer;
    begin
      n := LastDelimiter('/', url);
      if n = 0 then
      begin
        Result := '';
        ShowMessage('Unable to download file - not a recognised URL: '+url);
        Exit;
      end;
      Result := DownloadTempPath + Copy(url,n+1,Length(url));
    end;

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

        FParams.Packages[i].SavePath := GetSavePath(FParams.Packages[i].DownloadURL);
      end;

    if FParams.Keyman.Install then
    begin
      Inc(FDownload.TotalDownloads);
      Inc(FDownload.TotalSize, FParams.Keyman.DownloadSize);
      FParams.Keyman.SavePath := GetSavePath(FParams.Keyman.DownloadURL);
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
        else SysUtils.DeleteFile(FParams.Packages[i].SavePath);

  if FParams.Keyman.Install and FileExists(FParams.Keyman.SavePath) then
    if Result
      then DeleteFileOnReboot(FParams.Keyman.SavePath)
      else SysUtils.DeleteFile(FParams.Keyman.SavePath);

  if not Result
    then RemoveDir(ExcludeTrailingPathDelimiter(DownloadTempPath))
    else DeleteFileOnReboot(ExcludeTrailingPathDelimiter(DownloadTempPath));
end;

function TOnlineUpdateCheck.DoInstallPackage(Package: TOnlineUpdateCheckParamsPackage): Boolean;
var
  i: Integer;
  pkg: IKeymanPackageInstalled;
  FPackage: IKeymanPackageFile;
begin
  Result := True;
  
  for i := 0 to kmcom.Packages.Count - 1 do
  begin
    pkg := kmcom.Packages[i];
    if WideSameText(pkg.ID, Package.ID) then
    begin
      pkg.Uninstall(True);
      Break;
    end;
  end;
  pkg := nil;
  kmcom.Keyboards.Apply;

  FPackage := kmcom.Packages.GetPackageFromFile(Package.SavePath);
  FPackage.Install(True);
  FPackage := nil;

  kmcom.Keyboards.Refresh;
  kmcom.Packages.Refresh;
  //kmcom.Packages.Install(Package.SavePath, kmcom.SystemInfo.IsAdministrator, True,'');
  SysUtils.DeleteFile(Package.SavePath);
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
begin
  { We have an update available }
  with OnlineUpdateNewVersion(nil) do
  try
    CurrentVersion := GetVersionString;
    Params := Self.FParams;
    if ShowModal <> mrYes then
    begin
      Self.FParams.Result := oucUnknown;
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
        if WaitForElevatedConfiguration(Application.Handle, '-ou "'+DownloadTempPath+'"', not FParams.Keyman.Install) <> 0 then  // I2513
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

  if (FParams.Result = oucSuccess) and kmcom.SystemInfo.RebootRequired then
    FParams.Result := oucSuccessReboot;
end;

procedure TOnlineUpdateCheck.ShutDown;
begin
  if Assigned(Application) then
    Application.Terminate;
end;

function TOnlineUpdateCheck.DoRun: TOnlineUpdateCheckResult;
var
  node, doc: TJSONObject;
  {FProxyHost: string;
  FProxyPort: Integer;}
  flags: DWord;
  i, n: Integer;
  kbd0: IKeymanKeyboard;
  pkg0: IKeymanPackageInstalled;
  kbd: IKeymanKeyboardInstalled;
  pkg: IKeymanPackage;
  j: Integer;
  nodes: TJSONObject;
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
  try
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        if ValueExists(SRegValue_CheckForUpdates) and not ReadBool(SRegValue_CheckForUpdates) and not FForce then
        begin
          Result := oucNoUpdates;
          Exit;
        end;
        if ValueExists(SRegValue_LastUpdateCheckTime) and (Now - ReadDateTime(SRegValue_LastUpdateCheckTime) < 7) and FSilent and not FForce then
        begin
          Result := oucNoUpdates;
          Exit;
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
      Result := oucFailure;
      FErrorMessage := E.Message;
      Exit;
    end;
  end;

  try
    with THTTPUploader.Create(nil) do
    try
      ShowUI := not FSilent;
      Fields.Add('Version', ansistring(FCurrentVersion));
      for i := 0 to kmcom.Packages.Count - 1 do
      begin
        pkg := kmcom.Packages[i];
        Fields.Add(ansistring('Package_'+pkg.ID), ansistring(pkg.Version));
        pkg := nil;
      end;
        
      Proxy.Server := GetProxySettings.Server;
      Proxy.Port := GetProxySettings.Port;
      Proxy.Username := GetProxySettings.Username;
      Proxy.Password := GetProxySettings.Password;

      Request.HostName := API_Server;
      Request.Protocol := API_Protocol;
      Request.UrlPath := API_Path_UpdateCheck_Desktop;
      //OnStatus :=
      Upload;
      if Response.StatusCode = 200 then
      begin
        doc := TJSONObject.ParseJSONValue(UTF8String(Response.MessageBodyAsString)) as TJSONObject;
        if doc = nil then
          raise EOnlineUpdateCheck.Create('Invalid response:'#13#10+string(Response.MessageBodyAsString));

        SetLength(FParams.Packages,0);
        if doc.Values['keyboards'] is TJSONObject
          then nodes := doc.Values['keyboards'] as TJSONObject
          else nodes := nil;

        if Assigned(nodes) then
        begin
          for i := 0 to nodes.Count - 1 do
          begin
            node := nodes.Pairs[i].JsonValue as TJSONObject;
            n := kmcom.Packages.IndexOf(nodes.Pairs[i].JsonString.Value);
            if n >= 0 then
            begin
              pkg := kmcom.Packages[n];
              j := Length(FParams.Packages);
              SetLength(FParams.Packages, j+1);
              FParams.Packages[j].NewID := node.Values['id'].Value;
              FParams.Packages[j].ID := nodes.Pairs[i].JsonString.Value;
              FParams.Packages[j].Description := node.Values['name'].Value;
              FParams.Packages[j].OldVersion := pkg.Version;
              FParams.Packages[j].NewVersion := node.Values['version'].Value;
              FParams.Packages[j].DownloadSize := (node.Values['packageFileSize'] as TJSONNumber).AsInt64;
              FParams.Packages[j].DownloadURL := node.Values['url'].Value;
              pkg := nil;
            end
            else
              FErrorMessage := 'Unable to find package '+node.Pairs[i].JsonString.Value;
          end;
        end;

        if doc.Values['windows'] is TJSONObject then
        begin
          node := doc.Values['windows'] as TJSONObject;
          if CompareVersions(node.Values['version'].Value, FCurrentVersion) < 0 then
          begin
            FParams.Keyman.OldVersion := FCurrentVersion;
            FParams.Keyman.NewVersion := node.Values['version'].Value;
            FParams.Keyman.DownloadURL := node.Values['url'].Value;
            FParams.Keyman.DownloadSize := (node.Values['size'] as TJSONNumber).AsInt64;
          end;
        end;

        if (Length(FParams.Packages) > 0) or (FParams.Keyman.DownloadURL <> '') then
        begin
          if not FSilent then
            ShowUpdateForm
          else
          begin
            ShowUpdateIcon;
          end;
          Result := FParams.Result;
        end
        else if doc.Values['message'] <> nil then
        begin
          Result := oucFailure;
          FErrorMessage := doc.Values['message'].Value;
        end
        else
        begin
          FErrorMessage := 'No updates are currently available.';
          Result := oucNoUpdates;
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

procedure OnlineUpdateAdmin(Path: string);
var
  Package: TOnlineUpdateCheckParamsPackage;
  f: TSearchRec;
  ext: string;
  FPackageUpgradeList: TStringList;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  FPackageUpgradeList := TStringList.Create;
  with TOnlineUpdateCheck.Create(False,True) do
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

      SysUtils.FindClose(f);
    end;

    if FindFirst(Path + '*.exe', 0, f) = 0 then
    begin
      FParams.Keyman.SavePath := Path + f.Name;
      DoInstallKeyman;
      SysUtils.FindClose(f);
    end;
  finally
    Free;
    FPackageUpgradeList.Free;
  end;
end;

end.
