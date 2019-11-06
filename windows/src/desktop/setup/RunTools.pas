(*
  Name:             RunTools
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Dec 2010

  Modified Date:    24 Jun 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Dec 2010 - mcdurdin - I2562 - Have a common install utils unit after splitting Keyman Desktop and OEM setup dialogs
                    31 Dec 2010 - mcdurdin - I2605 - Fixup crash starting Desktop when downloaded locale is selected post-upgrade
                    14 Jan 2011 - mcdurdin - I2645 - Refactor install process steps - move SetupMSI to individual forms
                    17 Jan 2011 - mcdurdin - I1917 - Fix references to patchurl, patchsize; show correct file size and save to correct filename
                    21 Feb 2011 - mcdurdin - I2740 - Auto Update should close down Keyman before starting the update
                    22 Feb 2011 - mcdurdin - I2747 - After upgrading from KM7, the backup registry key is not deleted
                    22 Feb 2011 - mcdurdin - I2314 - setup.log saved to incorrect location
                    22 Feb 2011 - mcdurdin - I2651 - Install does not set desired default options
                    22 Feb 2011 - mcdurdin - I2738 - Auto Update does not start automatically
                    22 Feb 2011 - mcdurdin - I2741 - After install tutorial starts automatically
                    22 Feb 2011 - mcdurdin - I2748 - Delete backup path from shell user registry, not admin user registry
                    22 Feb 2011 - mcdurdin - I2749 - Audit all uses of TRegistry, TTntRegistry and replace with TShellUserRegistry where appropriate
                    22 Feb 2011 - mcdurdin - I2755 - Setup.log should be stored in Diag folder
                    22 Feb 2011 - mcdurdin - I2754 - Auto update is too silent and reboots unexpectedly for user
                    22 Feb 2011 - mcdurdin - I2757 - Installer does not wait for kmshell actions to complete before continuing
                    22 Feb 2011 - mcdurdin - I2756 - Keyman Desktop is not starting after update
                    28 Feb 2011 - mcdurdin - I2768 - Keyman Desktop setup fails on a new computer (regression)
                    18 Mar 2011 - mcdurdin - I2792 - Fix crash logging import of older keyboards
                    03 May 2011 - mcdurdin - I2896 - Check root certificate availability
                    04 Jul 2011 - mcdurdin - I2970 - Keyman installer should not set defaults for OEM products
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3326 - V9.0 - Add missing MsiEnumRelatedProducts from jwamsi
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    19 Oct 2012 - mcdurdin - I3476 - V9.0 - Fixup additional hints and warnings around string conversion
                    15 Jun 2012 - mcdurdin - I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    03 Nov 2012 - mcdurdin - I3500 - V9.0 - Merge of I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    24 Jun 2014 - mcdurdin - I4293 - V9.0 - Setup bootstrapper does not check for V8 upgrade
*)
unit RunTools;  // I3306

interface

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  Controls,
  UfrmDownloadProgress;

type
  TMSIInfo = record
    Version, ProductCode: WideString;
    InstallURL, Filename: WideString;
    InstallSize: Integer;  // I1917
  end;

  TStatusEvent = procedure(const Status: WideString) of object;

  TRunTools = class
  private
    FSilent: Boolean;
    FPromptForReboot: Boolean;  // I3355   // I3500
    FDownloadURL: WideString;
    FDownloadFilename: WideString;
    FErrorLog: TFileStream;
    FNewVersion, FInstalledVersion, FInstallerVersion: TMSIInfo;
    StatusMax: Integer;
    FOnStatus: TStatusEvent;
    FOnline: Boolean;
    FRunUpgrade6: Boolean;
    FRunUpgrade7: Boolean;
    FRunUpgrade8: Boolean;   // I4293
    FRunUpgrade9: Boolean;
    FRunUpgrade10: Boolean;
    constructor Create;
    procedure DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
    function InstallNewVersion(var HasAcceptedUpdate: Boolean): Boolean;
    function IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
    procedure CheckNewVersion;
    procedure Status(Text: WideString);
    function CacheMSIFile(const SrcMSIFileName: WideString): WideString;
    procedure FinishCacheMSIFile(const SrcMSIFileName: WideString;
      InstallSuccess: Boolean);
    procedure CheckInstalledVersion;
    function InstallMSI: Boolean;
    procedure InstallPackages(StartKeyman,StartWithWindows,CheckForUpdates,StartDisabled,StartWithConfiguration: Boolean);
    procedure PrepareForReboot(res: Cardinal);
    function RestartWindows: Boolean;
    procedure RunVersion6Upgrade(const kmshellpath: WideString);
    procedure RunVersion7Upgrade(const KMShellPath: WideString);
    procedure RunVersion8Upgrade(const KMShellPath: WideString);   // I4293
    procedure RunVersion9Upgrade(const KMShellPath: WideString);
    procedure RunVersion10Upgrade(const KMShellPath: WideString);
    procedure CloseKeymanApplications;  // I2740
    procedure DeleteBackupPath; // I2747
    procedure WaitFor(hProcess: THandle; var Waiting, Cancelled: Boolean);  // I3349
  public
    destructor Destroy; override;
    procedure CheckInternetConnectedState;
    function DownloadFile(ADownloadURL, ADownloadFilename: WideString): Boolean;
    function DoInstall(Handle: THandle; PackagesOnly, CheckForUpdatesInstall, StartAfterInstall, StartWithWindows, CheckForUpdates, StartDisabled, StartWithConfiguration: Boolean): Boolean;
    procedure LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);
    property Silent: Boolean read FSilent write FSilent;
    property PromptForReboot: Boolean read FPromptForReboot write FPromptForReboot;  // I3355   // I3500
    property Online: Boolean read FOnline;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;
    property RunUpgrade6: Boolean read FRunUpgrade6 write FRunUpgrade6;
    property RunUpgrade7: Boolean read FRunUpgrade7 write FRunUpgrade7;
    property RunUpgrade8: Boolean read FRunUpgrade8 write FRunUpgrade8;   // I4293
    property RunUpgrade9: Boolean read FRunUpgrade9 write FRunUpgrade9;
    property RunUpgrade10: Boolean read FRunUpgrade10 write FRunUpgrade10;
  end;

function GetRunTools: TRunTools;
procedure CheckMSIResult(res: UINT);

implementation

uses
  System.Variants,
  System.Win.ComObj,
  Vcl.Forms,
  Winapi.psapi,
  Winapi.ShlObj,
  Winapi.Tlhelp32,

  jwamsi,
  jwawintype,

  bootstrapmain,
  errlogpath,
  GetOsVersion,
  HTTPUploader,
  Keyman.System.UpgradeRegistryKeys,
  Keyman.System.UpdateCheckResponse,
  KeymanPaths,
  OnlineConstants,
  RegistryHelpers,
  RegistryKeys,
  SetupStrings,
  SFX,
  TntDialogHelp,
  ErrorControlledRegistry,
  UCreateProcessAsShellUser,
  Upload_Settings,
  utilsystem,
  utilexecute,
  VersionInfo;

var
  FRunTools: TRunTools = nil;


function GetRunTools: TRunTools;
begin
  if not Assigned(FRunTools) then
    FRunTools := TRunTools.Create;
  Result := FRunTools;
end;

procedure CheckMSIResult(res: UINT);
begin
  if res <> ERROR_SUCCESS then
    raise Exception.Create('Failed to install '+FInstallInfo.MSIFileName+': ['+IntToStr(res)+'] '+SysErrorMessage(res));
end;

{ TRunTools }

constructor TRunTools.Create;
begin
end;

destructor TRunTools.Destroy;
begin
  FreeAndNil(FErrorLog);
  inherited;
end;

function TRunTools.DoInstall(Handle: THandle; PackagesOnly, CheckForUpdatesInstall, StartAfterInstall, StartWithWindows, CheckForUpdates, StartDisabled, StartWithConfiguration: Boolean): Boolean;
begin
  Result := False;

  if PackagesOnly
    then StatusMax := FInstallInfo.Packages.Count
    else StatusMax := 6 + FInstallInfo.Packages.Count;

  if not PackagesOnly then
  begin
    CheckInstalledVersion;

    if CheckForUpdatesInstall then
    begin
      Status(FInstallInfo.Text(ssStatusCheckingForUpdates));
      try
        CheckNewVersion;
      except
        on E:Exception do // I1440 - avoid update check failure stopping install
          LogError('Error checking for updates: '+E.Message);
      end;
    end;
    Status(FInstallInfo.Text(ssStatusInstalling));

    CloseKeymanApplications;  // I2740

    if InstallMSI then
    begin
      InstallPackages(StartAfterInstall,StartWithWindows,CheckForUpdates,StartDisabled,StartWithConfiguration);
      Result := True;
    end
  end
  else
  begin
    CheckInstalledVersion;
    InstallPackages(StartAfterInstall,StartWithWindows,CheckForUpdates,FInstallInfo.StartDisabled, FInstallInfo.StartWithConfiguration);
    Result := True;
  end;
end;

function TRunTools.DownloadFile(ADownloadURL, ADownloadFilename: WideString): Boolean;
begin
  FDownloadURL := ADownloadURL;
  FDownloadFilename := ADownloadFilename;

  { Download the redistributable }
  with TfrmDownloadProgress.Create(nil) do
  try
    Callback := DownloadFileCallback;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TRunTools.DownloadFileCallback(AOwner: TfrmDownloadProgress; var Result: Boolean);
begin
  Result := False;
  try
    with THTTPUploader.Create(AOwner) do
    try
      OnCheckCancel := AOwner.HTTPCheckCancel;
      OnStatus := AOwner.HTTPStatus;
      Request.Agent := API_UserAgent;
      //Request.Protocol := Upload_Protocol;
      //Request.HostName := Upload_Server;
      Request.SetURL(FDownloadURL);// UrlPath := URL;
      Upload;
      if Response.StatusCode = 200 then
      begin
        with TFileStream.Create(ExtPath + FDownloadFilename, fmCreate) do  // I3476
        try
          Write(Response.PMessageBody^, Response.MessageBodyLength);
        finally
          Free;
        end;
        Result := True;
      end
      else
        LogError(FInstallInfo.Text(ssErrorDownloadingUpdate, [Response.StatusCode]));
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then LogError(FInstallInfo.Text(ssErrorUnableToContactServer))
        else LogError(FInstallInfo.Text(ssErrorUnableToContactServerDetailed, [E.Message]));
      Result := False;
    end;
  end;
end;

procedure TRunTools.LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);
const
  nl: WideString = #13#10;
var
  path: WideString;
begin
  if not FSilent and ShowDialogIfNotSilent then
    ShowMessageW(msg);
  if not Assigned(FErrorLog) then
  begin
    path := GetErrLogPath + 'setup.log'; // I2314

    if SysUtils.FileExists(path) then
    begin
      FErrorLog := TFileStream.Create(path, fmOpenReadWrite);
      FErrorLog.Seek(0, soFromEnd);
    end
    else
      FErrorLog := TFileStream.Create(path, fmCreate);
  end;

  FErrorLog.Write(PWideChar(msg+nl)^, Length(msg+nl)*2);
end;

function TRunTools.IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
begin
  Result := (FInstalledVersion.Version <> '') and
    (CompareVersions(FInstalledVersion.Version, NewVersion) <= 0);
end;

function TRunTools.InstallNewVersion(var HasAcceptedUpdate: Boolean): Boolean;
begin
  Result := True;
  HasAcceptedUpdate := False;

  if GetRunTools.Silent then
    Exit;

  case MessageDlgW(FInstallInfo.Text(ssQueryUpdateVersion, [FNewVersion.InstallSize div 1024, FNewVersion.Version]),  // I1917
      mtConfirmation, mbYesNoCancel, 0) of
    mrYes:    HasAcceptedUpdate := GetRunTools.DownloadFile(FNewVersion.InstallURL, FNewVersion.Filename);
    mrNo:     ;
    mrCancel: Result := False;
  end;
end;

procedure TRunTools.CheckNewVersion;
var
  ucr: TUpdateCheckResponse;
begin
  with THTTPUploader.Create(nil) do
  try
    if FInstalledVersion.Version = ''
      then Fields.Add('Version', ansistring(FInstallInfo.Version))
      else Fields.Add('Version', ansistring(FInstalledVersion.Version));

    Request.HostName := API_Server;
    Request.Protocol := API_Protocol;
    Request.UrlPath := API_Path_UpdateCheck_Desktop;

    Upload;
    if Response.StatusCode = 200 then
    begin
      if ucr.Parse(Response.MessageBodyAsString, 'windows', FInstallInfo.Version) then
      begin
        if ucr.Status = ucrsUpdateReady then
        begin
          FNewVersion.Version := ucr.NewVersion;
          FNewVersion.InstallURL := ucr.InstallURL;
          FNewVersion.InstallSize := ucr.InstallSize;
          FNewVersion.Filename := ExtractFileName(StringReplace(FNewVersion.InstallURL, '/', '\', [rfReplaceAll]));  // I1917
        end;
      end
      else
        raise Exception.Create(ucr.ErrorMessage);
    end
    else
      raise Exception.Create('Error '+IntToStr(Response.StatusCode));
  finally
    Free;
  end;
end;

function CloseThreadWindowProc(hwnd: THandle; lParam: LPARAM): BOOL; stdcall;  // I2740
begin
  PostMessage(hwnd, WM_CLOSE, 0, 0);
  Result := True;
end;

procedure TRunTools.CloseKeymanApplications;  // I2740
const
  KeymanApplicationNames: array[0..2] of string = ('kmshell.exe', 'keyman.exe', 'tsysinfo.exe');
var
  i: Integer;
  cbNeeded, cb: DWORD;
  processes: PDWORD;
  nProcesses: Cardinal;
  BaseName: array[0..260] of char;
  hModule, hProcess: THandle;
  n: Integer;
  FAppName: string;
  te: tagTHREADENTRY32;
  FProcessIDs: array of Cardinal;
  pid: PDWORD;
var
  hSnapshot: THandle;
begin
  cb := 1000 * sizeof(DWORD);
  processes := AllocMem(cb);
  repeat
    if not EnumProcesses(processes, cb, cbNeeded) then Exit;
    if cb = cbNeeded then
    begin
      cb := cb * 2;
      ReallocMem(processes, cb);
      cbNeeded := cb;
    end;
  until cb > cbNeeded;
  try
    SetLength(FProcessIDs, 0);
    pid := processes;
    nProcesses := cbNeeded div sizeof(DWORD);
    for i := 0 to nProcesses - 1 do
    begin
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, pid^);
      if hProcess <> 0 then
      try
        if EnumProcessModules(hProcess, @hModule, sizeof(hModule), cbNeeded) then
        begin
          if GetModuleBaseName(hProcess, hModule, baseName, Sizeof(BaseName) div SizeOf(BaseName[0])) > 0 then
          begin
            FAppName := ExtractFileName(BaseName);
            for n := 0 to High(KeymanApplicationNames) do
              if SameText(FAppName, KeymanApplicationNames[n]) then
              begin
                SetLength(FProcessIDs, Length(FProcessIDs)+1);
                FProcessIDs[High(FProcessIDs)] := pid^;
              end;
          end;
        end;
      finally
        CloseHandle(hProcess);
      end;
      Inc(pid);
    end;
  finally
    FreeMem(processes);
  end;

  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
  if hSnapshot <> INVALID_HANDLE_VALUE then
  try
    FillChar(te,sizeof(te),0);
    te.dwSize := SizeOf(te);
    if Thread32First(hSnapshot, te) then
    begin
      repeat
        for i := 0 to High(FProcessIDs) do
          if te.th32OwnerProcessID = FProcessIDs[i] then
          begin
            EnumThreadWindows(te.th32ThreadID, @CloseThreadWindowProc, 0);
          end;
      until not Thread32Next(hSnapshot, te);
    end;
  finally
    CloseHandle(hSnapshot);
  end;
end;

procedure TRunTools.Status(Text: WideString);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Text);
end;

procedure TRunTools.CheckInternetConnectedState;
type
  TInternetGetConnectedState = function(lpdwFlags: LPDWORD; dwReserved: DWORD): BOOL; stdcall;
var
  hModule: THandle;
  FInternetGetConnectedState: TInternetGetConnectedState;
  flags: DWord;
begin
  FOnline := False;
  hModule := LoadLibrary('wininet.dll');
  if hModule <> 0 then
  begin
    FInternetGetConnectedState := GetProcAddress(hModule, 'InternetGetConnectedState');
    if Assigned(FInternetGetConnectedState) then
    begin
      FOnline := FInternetGetConnectedState(@flags, 0);
    end;
    FreeLibrary(hModule);
  end;
end;

function TRunTools.CacheMSIFile(const SrcMSIFileName: WideString): WideString;
var
  path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolder_CachedInstallerFiles+'\'+FInstallerVersion.ProductCode;  // I2561

  if not SysUtils.ForceDirectories(path) then
    raise Exception.Create('Failed to cache installer MSI file (code 1,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));

  Result := path+'\'+ExtractFileName(SrcMSIFileName);

  if FileExists(PWideChar(Result)) then
  begin
    // Backup the existing cached installer until after the upgrade
    RenameFile(Result, Result + '.1');
  end;

  if not CopyFile(PWideChar(SrcMSIFileName), PWideChar(Result), False) then
    raise Exception.Create('Failed to cache installer MSI file (code 2,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));
end;

procedure TRunTools.FinishCacheMSIFile(const SrcMSIFileName: WideString; InstallSuccess: Boolean);
var
  newmsi, path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolder_CachedInstallerFiles+'\'+FInstallerVersion.ProductCode;  // I2561
  newmsi := path+'\'+ExtractFileName(SrcMSIFileName);

  if SysUtils.FileExists(newmsi+'.1') then
  begin
    if InstallSuccess then
      DeleteFile(newmsi+'.1')
    else
    begin
      DeleteFile(newmsi);
      RenameFile(newmsi+'.1', newmsi);
    end;
  end
  else if not InstallSuccess then
  begin
    DeleteFile(newmsi);
    RemoveDir(path);
    RemoveDir(ExtractFileDir(path)); // may fail if other products cached there, we don't care
    RemoveDir(ExtractFileDir(ExtractFileDir(path))); // may fail if other products installed there, we don't care
  end;
end;

{$EXTERNALSYM MsiEnumRelatedProducts}

const
  msilib = 'msi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}

var
  _MsiEnumRelatedProducts: Pointer;

{$WARNINGS OFF}

function MsiEnumRelatedProducts(lpUpgradeCode: LPCTSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPTSTR): UINT; stdcall;  // I3326
begin
  GetProcedureAddress(_MsiEnumRelatedProducts, msilib, 'MsiEnumRelatedProducts' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnumRelatedProducts]
  end;
end;
{$WARNINGS ON}

procedure TRunTools.CheckInstalledVersion;
var
  hProduct: MSIHANDLE;
  buf: array[0..64] of WideChar;
  sz: DWord;
  UpgradeCode: WideString;
  i: DWord;
begin
  FInstalledVersion.Version := '';
  CheckMSIResult(MsiOpenPackageExW(PWideChar(ExtPath + FInstallInfo.MSIFileName), MSIOPENPACKAGEFLAGS_IGNOREMACHINESTATE, hProduct));  // I3476
  try
    sz := 64;
    CheckMSIResult(MsiGetProductPropertyW(hProduct, 'UpgradeCode', buf, @sz));
    UpgradeCode := buf;
    sz := 64;
    CheckMSIResult(MsiGetProductPropertyW(hProduct, 'ProductCode', buf, @sz));
    FInstallerVersion.ProductCode := buf;
  finally
    MsiCloseHandle(hProduct);
  end;

  i := 0;
  if MsiEnumRelatedProducts(PWideChar(UpgradeCode), 0, i, buf) = ERROR_SUCCESS then
  begin
    sz := 64;
    FInstalledVersion.ProductCode := buf;
    CheckMSIResult(MsiGetProductInfoW(PWideChar(FInstalledVersion.ProductCode), INSTALLPROPERTY_VERSIONSTRING, buf, @sz));
    FInstalledVersion.Version := buf;
  end;
end;

function TRunTools.InstallMSI: Boolean;
var
  pcode: array[0..39] of Char;
  res: Cardinal;
  ReinstallMode: WideString;
  FCacheFileName: WideString;
  FLogFileName: WideString;
  FHasAcceptedUpdate: Boolean;
begin
  Result := True;

  { We need to check if the included installer or the installer available is a newer version than the currently installed version }
  if FNewVersion.Version <> '' then
  begin
    if IsNewerVersionInstalled(FNewVersion.Version) then
      { The downloadable installer is older than the installed version }
      Exit;

    if not InstallNewVersion(FHasAcceptedUpdate) then
      Exit(False);

    if not FHasAcceptedUpdate then
    begin
      { We need to check if the included installer is a newer version than the currently installed version }
      FNewVersion.Filename := '';
      FNewVersion.Version := '';
    end;
  end;

  if not IsNewerVersionInstalled(FInstallInfo.Version) then // I2560
  begin
    ReinstallMode := 'REBOOTPROMPT=S REBOOT=ReallySuppress'; // I2754 - Auto update is too silent
    if (FInstalledVersion.Version <> '') and (FInstalledVersion.ProductCode = FInstallerVersion.ProductCode) then
    begin
      ReinstallMode := ReinstallMode + ' REINSTALLMODE=vomus REINSTALL=ALL';
    end
    else
    begin
      Status('Removing older versions');
      // Remove older versions of Keyman now. We'll still get the upgrade desired
      // because we've backed up the relevant keys for reapplication post-install
      // Version 11 and later do not need this treatment as they are upgraded in-place
      // with the file and registry locations remaining static
      if (MsiGetProductCode('{35E06B45-17C0-406C-B94F-70EFF1EC9278}', pcode) = ERROR_SUCCESS) or // Keyman 7.1 Light
         (MsiGetProductCode('{04C8710E-3D29-4A25-80A2-A56853A4267D}', pcode) = ERROR_SUCCESS) or // Keyman 7.1 Pro
         (MsiGetProductCode('{18E9B728-8E4E-48DF-9E9F-6F3086A1FE04}', pcode) = ERROR_SUCCESS) or // Keyman 8.0
         (MsiGetProductCode('{E6806190-7B09-4A8C-8C95-25982589D919}', pcode) = ERROR_SUCCESS) or // Keyman 9.0
         (MsiGetProductCode('{A20AFB02-7581-4019-9229-5312308FBA1E}', pcode) = ERROR_SUCCESS) then // Keyman 10.0
      begin
        MsiConfigureProduct(pcode, INSTALLLEVEL_DEFAULT, INSTALLSTATE_ABSENT);
        // We'll ignore errors ...
      end;
    end;


    { Log the install to the diag folder }

    FLogFileName := GetErrLogFileName(ChangeFileExt(ExtractFileName(FInstallInfo.MSIFileName), ''));  // I1610 // I2755 // I2792
    //ForceDirectories(GetErrLogPath);  // I2768

    MsiEnableLogW(INSTALLLOGMODE_VERBOSE, PWideChar(FLogFileName), 0);

    { Cache the msi file to avoid source dependencies for repair, patch, uninstall, bleagh }

    FCacheFileName := CacheMSIFile(ExtPath + FInstallInfo.MSIFileName);  // I3476

    res := MsiInstallProductW(PWideChar(FCacheFileName), PWideChar(ReinstallMode));

    FinishCacheMSIFile(ExtPath + FInstallInfo.MSIFileName,  // I3476
      (res = ERROR_SUCCESS_REBOOT_REQUIRED) or (res = ERROR_SUCCESS_REBOOT_INITIATED) or (res = ERROR_SUCCESS));

    case res of
      ERROR_SUCCESS: ;
      ERROR_SUCCESS_REBOOT_REQUIRED, ERROR_SUCCESS_REBOOT_INITIATED:
        begin
          PrepareForReboot(res); // We need to continue this install after reboot
          Result := False;
          Exit;
        end;
      ERROR_INSTALL_USEREXIT: begin Result := False; Exit; end;
    else
      CheckMSIResult(res);
    end;
  end;

  if not FSilent then
    MsiSetInternalUI(INSTALLUILEVEL_FULL, nil);

  if FNewVersion.Filename <> '' then
  begin
    //Run the setup app silently
    TUtilExecute.WaitForProcess('"'+ExtPath + FNewVersion.FileName+'" -s -o', ExtPath, SW_SHOW, WaitFor);  // I3349  // I3476
    //if not FSilent then
    //  MsiSetInternalUI(INSTALLUILEVEL_BASIC, nil);
    //CheckMSIResult(MsiApplyPatchW(PWideChar(ExtPath + FNewVersion.Filename), nil, INSTALLTYPE_DEFAULT, ''));
  end;
end;

procedure TRunTools.WaitFor(hProcess: THandle; var Waiting, Cancelled: Boolean);  // I3349
begin
  case MsgWaitForMultipleObjects(1, hProcess, FALSE, INFINITE, QS_ALLINPUT) of
    WAIT_OBJECT_0 + 1:
      Application.ProcessMessages;
    WAIT_OBJECT_0, WAIT_ABANDONED_0:
      Waiting := False;
  end;
end;

procedure TRunTools.InstallPackages(StartKeyman,StartWithWindows,CheckForUpdates,StartDisabled,StartWithConfiguration: Boolean);
var
  i: Integer;
  s: WideString;
  FKMShellPath: WideString;
  FExitCode: Cardinal;
begin
  //if FInstallInfo.Packages.Count = 0 then Exit;  I879

  FKMShellPath := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
  if SysUtils.FileExists(FKMShellPath) then
  begin
    RunVersion6Upgrade(FKMShellPath);
    RunVersion7Upgrade(FKMShellPath);  // I2548
    RunVersion8Upgrade(FKMShellPath);  // I4293
    RunVersion9Upgrade(FKMShellPath);
    RunVersion10Upgrade(FKMShellPath);

    { Install packages for all users }
    s := '-nowelcome -s -i '; //"'+ExtPath+'" ';
    for i := 0 to FInstallInfo.Packages.Count - 1 do
      s := s + '"'+ExtPath+FInstallInfo.Packages.Names[i]+'" ';  // I3476
    TUtilExecute.WaitForProcess('"'+FKMShellPath+'" '+s, ExtractFilePath(FKMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349

    { Configure Keyman Desktop for initial install, both local-machine, and current-user }
    s := '-firstrun=';

    if StartWithWindows then s := s + 'StartWithWindows,';
    if CheckForUpdates then s := s + 'CheckForUpdates,';

    if (FInstalledVersion.Version = '') or (FInstalledVersion.ProductCode <> FInstallerVersion.ProductCode) then s := s + 'InstallDefaults,';  // I2651

    if StartDisabled then
    begin
      s := s + ' -disablePackages=';
      for i := 0 to FInstallInfo.Packages.Count - 1 do
      begin
        if i > 0 then s := s + ',';
        s := s + '"'+ExtractFileName(ChangeFileExt(FInstallInfo.Packages.Names[i], ''))+'"';
      end;
    end;

    TUtilExecute.WaitForProcess('"'+FKMShellPath+'" '+s, ExtractFilePath(FKMShellPath), SW_SHOWNORMAL, WaitFor); // I2605 - for admin-installed packages  // I3349

    FExitCode := 0;
    if not CreateProcessAsShellUser(FKMShellPath, '"'+FKMShellPath+'" '+s, True, FExitCode) then // I2757
      LogError('Failed to setup default options for Keyman Desktop: '+SysErrorMessage(GetLastError))
    else if FExitCode = 2 then
    begin
      LogError('Failed to setup Keyman Desktop to start with Windows; this action may have been blocked by security software.');
    end;
  end;

  DeleteBackupPath;  // I2747

  if StartKeyman then  // I2738
  begin
    if SysUtils.FileExists(FKMShellPath) then
    begin
      s := '"'+FKMShellPath+'"';
      if StartWithConfiguration then
        s := s + ' -startWithConfiguration';
      if not CreateProcessAsShellUser(FKMShellPath, s, False) then  // I2741
        LogError('Failed to start Keyman Desktop: '+SysErrorMessage(GetLastError), False); // I2756
    end;
    //if not VarIsNull(ole_product) then
    //  ole_product.OpenProduct;
    //ModalResult := mrOk;
  end;

  Status(FInstallInfo.Text(ssStatusComplete));
end;

procedure TRunTools.PrepareForReboot(res: Cardinal);
begin
  with CreateHKCURegistry do  // I2749
  try
    if OpenKey(SRegKey_WindowsRunOnce_CU, True) then
      WriteString(SRegValue_WindowsRunOnce_Setup, '"'+ParamStr(0)+'" -c');
  finally
    Free;
  end;

  if res = ERROR_SUCCESS_REBOOT_REQUIRED then
  begin
    if GetRunTools.PromptForReboot and
      (MessageDlgW(FInstallInfo.Text(ssQueryRestart), mtConfirmation, [mbYes,mbNo], 0) = mrYes) then
    begin
      if not RestartWindows then
        GetRunTools.LogError(FInstallInfo.Text(ssErrorUnableToAutomaticallyRestart));
    end
    else
      GetRunTools.LogError(FInstallInfo.Text(ssMustRestart));
  end;
end;

type
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;
  {$EXTERNALSYM PTOKEN_PRIVILEGES}
  _TOKEN_PRIVILEGES = record
    PrivilegeCount: DWORD;
    Privileges: array [0..0] of LUID_AND_ATTRIBUTES;
  end;

  TAdjustTokenPrivileges = function (TokenHandle: THandle; DisableAllPrivileges: BOOL;
    const NewState: TTokenPrivileges; BufferLength: DWORD;
    PreviousState: PTokenPrivileges; var ReturnLength: DWORD): BOOL; stdcall;

  TOpenProcessToken = function (ProcessHandle: THandle; DesiredAccess: DWORD;
    var TokenHandle: THandle): BOOL; stdcall;

  TLookupPrivilegeValue = function (lpSystemName, lpName: PWideChar;
    var lpLuid: TLargeInteger): BOOL; stdcall;

function TRunTools.RestartWindows: Boolean;
var
  hToken: THandle;
  tkp: TOKEN_PRIVILEGES;
  FAdjustTokenPrivileges: TAdjustTokenPrivileges;
  FOpenProcessToken: TOpenProcessToken;
  FLookupPrivilegeValue: TLookupPrivilegeValue;
  hAdvApi32: Cardinal;
  retlen: Cardinal;
const
  SE_SHUTDOWN_NAME: WideString = 'SeShutdownPrivilege';
  SHTDN_REASON_MAJOR_APPLICATION     = $00040000;
  SHTDN_REASON_MINOR_INSTALLATION    = $00000002;
  SHTDN_REASON_FLAG_PLANNED      = DWORD($80000000);
begin
  Result := False;

  hAdvApi32 := LoadLibrary('advapi32.dll');
  if hAdvApi32 = 0 then Exit;

  FOpenProcessToken := GetProcAddress(hAdvApi32, 'OpenProcessToken');
  if not Assigned(FOpenProcessToken) then Exit;

  FLookupPrivilegeValue := GetProcAddress(hAdvApi32, 'LookupPrivilegeValueW');
  if not Assigned(FLookupPrivilegeValue) then Exit;

  FAdjustTokenPrivileges := GetProcAddress(hAdvApi32, 'AdjustTokenPrivileges');
  if not Assigned(FAdjustTokenPrivileges) then Exit;

  // Get a token for this process.
  if not FOpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then Exit;

  // Get the LUID for the shutdown privilege.
  if not FLookupPrivilegeValue(nil, PWideChar(SE_SHUTDOWN_NAME), tkp.Privileges[0].Luid) then Exit;

  tkp.PrivilegeCount := 1;  // one privilege to set
  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

  // Get the shutdown privilege for this process.

  FAdjustTokenPrivileges(hToken, False, tkp, 0, nil, retlen);
  if GetLastError() <> ERROR_SUCCESS then Exit;

  Result := ExitWindowsEx(EWX_REBOOT, SHTDN_REASON_MAJOR_APPLICATION or SHTDN_REASON_MINOR_INSTALLATION or SHTDN_REASON_FLAG_PLANNED);
end;

procedure TRunTools.RunVersion6Upgrade(const kmshellpath: WideString);
var
  s: WideString;
begin
  if FRunUpgrade6 then
  begin
    s := '"'+KMShellPath+'" -upgradekeyboards=';  // I2548
    TUtilExecute.WaitForProcess(s+'6,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
    TUtilExecute.WaitForProcess(s+'6,user', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349   // I4293
  end;
end;

procedure TRunTools.RunVersion7Upgrade(const KMShellPath: WideString);
var
  s: WideString;
begin
  if FRunUpgrade7 then
  begin
    s := '"'+KMShellPath+'" -upgradekeyboards='; // I2548
    TUtilExecute.WaitForProcess(s+'7,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
    TUtilExecute.WaitForProcess(s+'7,user', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349   // I4293
  end;
end;

procedure TRunTools.RunVersion8Upgrade(const KMShellPath: WideString);   // I4293
var
  s: WideString;
begin
  if FRunUpgrade8 then
  begin
    s := '"'+KMShellPath+'" -upgradekeyboards='; // I2548
    TUtilExecute.WaitForProcess(s+'8,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
    TUtilExecute.WaitForProcess(s+'8,user', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
  end;
end;

procedure TRunTools.RunVersion9Upgrade(const KMShellPath: WideString);
var
  s: WideString;
begin
  if FRunUpgrade9 then
  begin
    s := '"'+KMShellPath+'" -upgradekeyboards='; // I2548
    TUtilExecute.WaitForProcess(s+'9,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
  end;
end;

procedure TRunTools.RunVersion10Upgrade(const KMShellPath: WideString);
var
  s: WideString;
begin
  if FRunUpgrade10 then
  begin
    s := '"'+KMShellPath+'" -upgradekeyboards='; // I2548
    TUtilExecute.WaitForProcess(s+'10,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
  end;
end;

procedure TRunTools.DeleteBackupPath;  // I2747
begin
  with CreateHKLMRegistry do  // I2749
  try
    if KeyExists(SRegKey_UpgradeBackupPath_LM) then DeleteKey(SRegKey_UpgradeBackupPath_LM);
  finally
    Free;
  end;

  with CreateHKCURegistry do  // I2748, I2749
  try
    if KeyExists(SRegKey_UpgradeBackupPath_CU) then DeleteKey(SRegKey_UpgradeBackupPath_CU);
  finally
    Free;
  end;
end;

initialization
finalization
  FreeAndNil(FRunTools);
end.
