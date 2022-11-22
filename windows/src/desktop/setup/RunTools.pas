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
  History:          30 Dec 2010 - mcdurdin - I2562 - Have a common install utils unit after splitting Keyman D_esktop and OEM setup dialogs
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
                    22 Feb 2011 - mcdurdin - I2756 - Keyman D_esktop is not starting after update
                    28 Feb 2011 - mcdurdin - I2768 - Keyman D_esktop setup fails on a new computer (regression)
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
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.Setup.System.InstallInfo,
  UfrmDownloadProgress;

type
  TStatusEvent = procedure(const Status: WideString) of object;

  TRunTools = class
  private
    FSilent: Boolean;
    FMustReboot: Boolean;
    FPromptForReboot: Boolean;  // I3355   // I3500
    FErrorLog: TFileStream;
    StatusMax: Integer;
    FOnStatus: TStatusEvent;
    FOnline: Boolean;
    FRunUpgrade6: Boolean;
    FRunUpgrade7: Boolean;
    FRunUpgrade8: Boolean;   // I4293
    FRunUpgrade9: Boolean;
    FRunUpgrade10: Boolean;
    constructor Create;
    function IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
    procedure Status(Text: WideString);
    function CacheMSIFile(msiLocation: TInstallInfoFileLocation): WideString;
    procedure FinishCacheMSIFile(msiLocation: TInstallInfoFileLocation;
      InstallSuccess: Boolean);
    function InstallMSI(msiLocation: TInstallInfoFileLocation; var InstallDefaults: Boolean; ContinueSetup: Boolean): Boolean;
    procedure ConfigFirstRun(StartKeyman,StartWithWindows,
      CheckForUpdates,StartDisabled,StartWithConfiguration,InstallDefaults,
      AutomaticallyReportUsage: Boolean);
    procedure PrepareForReboot(res: Cardinal; InstallDefaults: Boolean);
    function RestartWindows: Boolean;
    procedure RunVersion6Upgrade(const kmshellpath: WideString);
    procedure RunVersion7Upgrade(const KMShellPath: WideString);
    procedure RunVersion8Upgrade(const KMShellPath: WideString);   // I4293
    procedure RunVersion9Upgrade(const KMShellPath: WideString);
    procedure RunVersion10Upgrade(const KMShellPath: WideString);
    procedure CloseKeymanApplications;  // I2740
    procedure DeleteBackupPath; // I2747
    procedure WaitFor(hProcess: THandle; var Waiting, Cancelled: Boolean);  // I3349
    procedure WriteToLog(const msg: string);
    procedure RunVersion11To13Upgrade(const KMShellPath: WideString);
    procedure ApplyWow64SystemProfilePatch;
  public
    destructor Destroy; override;
    procedure CheckInternetConnectedState;
    function DoInstall(Handle: THandle;
      StartAfterInstall, StartWithWindows, CheckForUpdates, StartDisabled,
      StartWithConfiguration, InstallDefaults, AutomaticallyReportUsage, ContinueSetup: Boolean): Boolean;
    procedure LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);
    procedure LogInfo(const msg: string; ShowDialogIfNotSilent: Boolean = False);

    class procedure CheckInstalledVersion(msiLocation: TInstallInfoFileLocation);

    property MustReboot: Boolean read FMustReboot;
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
procedure CheckMSIResult(msiLocation: TInstallInfoFileLocation; res: UINT);

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
  GetOsVersion,
  Keyman.Setup.System.ResourceDownloader,
  Keyman.Setup.System.SetupUILanguageManager,
  Keyman.System.UpgradeRegistryKeys,
  KeymanPaths,
  KeymanVersion,
  RegistryHelpers,
  RegistryKeys,
  SetupStrings,
  SFX,
  TntDialogHelp,
  ErrorControlledRegistry,
  utildir,
  utilsystem,
  utilexecute,
  VersionInfo;

var
  FRunTools: TRunTools = nil;

const
 UpgradeCode_Keyman11Plus =   '{c70af17c-8b9e-47a1-a099-b65aee3dc8b4}'; // Keyman 11+
 ProductCode_Keyman7_1Light = '{35E06B45-17C0-406C-B94F-70EFF1EC9278}'; // Keyman 7.1 Light
 ProductCode_Keyman7_1Pro =   '{04C8710E-3D29-4A25-80A2-A56853A4267D}'; // Keyman 7.1 Pro
 ProductCode_Keyman8 =        '{18E9B728-8E4E-48DF-9E9F-6F3086A1FE04}'; // Keyman 8.0
 ProductCode_Keyman9 =        '{E6806190-7B09-4A8C-8C95-25982589D919}'; // Keyman 9.0
 ProductCode_Keyman10 =       '{A20AFB02-7581-4019-9229-5312308FBA1E}'; // Keyman 10.0

function GetRunTools: TRunTools;
begin
  if not Assigned(FRunTools) then
    FRunTools := TRunTools.Create;
  Result := FRunTools;
end;

procedure CheckMSIResult(msiLocation: TInstallInfoFileLocation; res: UINT);
begin
  if res <> ERROR_SUCCESS then
    if Assigned(msiLocation) then
      raise Exception.Create('Failed to install '+msiLocation.Path+': ['+IntToStr(res)+'] '+SysErrorMessage(res))
    else
      raise Exception.Create('Failed to install msi: ['+IntToStr(res)+'] '+SysErrorMessage(res));
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

function TRunTools.DoInstall(Handle: THandle;
  StartAfterInstall, StartWithWindows, CheckForUpdates, StartDisabled,
  StartWithConfiguration, InstallDefaults, AutomaticallyReportUsage, ContinueSetup: Boolean): Boolean;
var
  msiLocation: TInstallInfoFileLocation;
begin
  if FInstallInfo.ShouldInstallKeyman
    then StatusMax := 6 + FInstallInfo.Packages.Count
    else StatusMax := FInstallInfo.Packages.Count;

  msiLocation := FInstallInfo.MsiInstallLocation;
  if Assigned(msiLocation) and FInstallInfo.ShouldInstallKeyman then
  begin
    if msiLocation.LocationType = iilOnline then
    begin
      LogInfo('Downloading '+msiLocation.Url);
      if not TResourceDownloader.Execute(FInstallInfo, msiLocation, FSilent) then
      begin
        LogInfo('Failed to download '+msiLocation.Url);
        Exit(False);
      end;
    end;

    Status(FInstallInfo.Text(ssStatusInstalling));

    CloseKeymanApplications;  // I2740

    if not InstallMSI(msiLocation, InstallDefaults, ContinueSetup) then
      Exit(False);
  end;

  ConfigFirstRun(StartAfterInstall,StartWithWindows,CheckForUpdates,
    StartDisabled,StartWithConfiguration,InstallDefaults,AutomaticallyReportUsage);

  Result := True;
end;

procedure TRunTools.LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);
begin
  WriteToLog('ERROR: '+msg);
  if not FSilent and ShowDialogIfNotSilent then
    ShowMessageW(msg);
end;

procedure TRunTools.WriteToLog(const msg: string);
const
  nl: WideString = #13#10;
var
  path: WideString;
begin
  if not Assigned(FErrorLog) then
  begin
    path := TKeymanPaths.ErrorLogPath + 'setup.log'; // I2314

    if System.SysUtils.FileExists(path) then
    begin
      FErrorLog := TFileStream.Create(path, fmOpenReadWrite);
      FErrorLog.Seek(0, soFromEnd);
    end
    else
      FErrorLog := TFileStream.Create(path, fmCreate);
  end;

  FErrorLog.Write(PWideChar(msg+nl)^, Length(msg+nl)*2);
end;

procedure TRunTools.LogInfo(const msg: string; ShowDialogIfNotSilent: Boolean = False);
begin
  WriteToLog('INFO: '+msg);
  if not FSilent and ShowDialogIfNotSilent then
    ShowMessageW(msg);
end;

function TRunTools.IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
begin
  Result := (FInstallInfo.InstalledVersion.Version <> '') and
    (CompareVersions(FInstallInfo.InstalledVersion.Version, NewVersion) <= 0);
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

function TRunTools.CacheMSIFile(msiLocation: TInstallInfoFileLocation): WideString;
var
  path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolder_CachedInstallerFiles+'\'+msiLocation.ProductCode;  // I2561

  if not System.SysUtils.ForceDirectories(path) then
    raise Exception.Create('Failed to cache installer MSI file (code 1,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));

  Result := path+'\'+ExtractFileName(msiLocation.Path);

  if FileExists(PWideChar(Result)) then
  begin
    // Backup the existing cached installer until after the upgrade
    RenameFile(Result, Result + '.1');
  end;

  if not CopyFile(PWideChar(msiLocation.Path), PWideChar(Result), False) then
    raise Exception.Create('Failed to cache installer MSI file (code 2,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));
end;

procedure TRunTools.FinishCacheMSIFile(msiLocation: TInstallInfoFileLocation; InstallSuccess: Boolean);
var
  newmsi, path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolder_CachedInstallerFiles+'\'+msiLocation.ProductCode;  // I2561
  newmsi := path+'\'+ExtractFileName(msiLocation.Path);

  if System.SysUtils.FileExists(newmsi+'.1') then
  begin
    if InstallSuccess then
      System.SysUtils.DeleteFile(newmsi+'.1')
    else
    begin
      System.SysUtils.DeleteFile(newmsi);
      RenameFile(newmsi+'.1', newmsi);
    end;
  end
  else if not InstallSuccess then
  begin
    System.SysUtils.DeleteFile(newmsi);
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

// TODO: move to another unit so we can mock?
class procedure TRunTools.CheckInstalledVersion(msiLocation: TInstallInfoFileLocation);
var
  hProduct: MSIHANDLE;
  buf: array[0..64] of WideChar;
  sz: DWord;
  UpgradeCode: WideString;
  i: DWord;
  ver: TMSIInfo;
begin
  if not Assigned(msiLocation) or (msiLocation.LocationType = iilOnline) then
  begin
    UpgradeCode := UpgradeCode_Keyman11Plus
  end
  else
  begin
    CheckMSIResult(msiLocation, MsiOpenPackageExW(PWideChar(msiLocation.Path), MSIOPENPACKAGEFLAGS_IGNOREMACHINESTATE, hProduct));  // I3476
    try
      sz := 64;
      CheckMSIResult(msiLocation, MsiGetProductPropertyW(hProduct, 'UpgradeCode', buf, @sz));
      UpgradeCode := buf;
      sz := 64;
      CheckMSIResult(msiLocation, MsiGetProductPropertyW(hProduct, 'ProductCode', buf, @sz));
      msiLocation.ProductCode := buf;
    finally
      MsiCloseHandle(hProduct);
    end;
  end;

  i := 0;
  if MsiEnumRelatedProducts(PWideChar(UpgradeCode), 0, i, buf) = ERROR_SUCCESS then
  begin
    ver.ProductCode := buf;
    sz := 64;
    CheckMSIResult(msiLocation, MsiGetProductInfoW(PWideChar(ver.ProductCode), INSTALLPROPERTY_VERSIONSTRING, buf, @sz));
    ver.Version := buf;
    FInstallInfo.InstalledVersion := ver;
  end;
end;

function TRunTools.InstallMSI(msiLocation: TInstallInfoFileLocation;var InstallDefaults: Boolean; ContinueSetup: Boolean): Boolean;
var
  pcode: array[0..39] of Char;
  res: Cardinal;
  ReinstallMode: WideString;
  FCacheFileName: WideString;
  FLogFileName: WideString;
begin
  Result := True;

  if not IsNewerVersionInstalled(msiLocation.Version) then // I2560
  begin
    ReinstallMode := 'REBOOTPROMPT=S REBOOT=ReallySuppress'; // I2754 - Auto update is too silent
    if (FInstallInfo.InstalledVersion.Version <> '') and (FInstallInfo.InstalledVersion.ProductCode = msiLocation.ProductCode) then
    begin
      ReinstallMode := ReinstallMode + ' REINSTALLMODE=vomus REINSTALL=ALL';
    end
    else
    begin
      // Remove older versions of Keyman now. We'll still get the upgrade desired
      // because we've backed up the relevant keys for reapplication post-install.
      // Version 11 and later do not need this treatment as they are upgraded in-place
      // with the file and registry locations remaining static
      if (MsiGetProductCode(ProductCode_Keyman7_1Light, pcode) = ERROR_SUCCESS) or // Keyman 7.1 Light
         (MsiGetProductCode(ProductCode_Keyman7_1Pro, pcode) = ERROR_SUCCESS) or // Keyman 7.1 Pro
         (MsiGetProductCode(ProductCode_Keyman8, pcode) = ERROR_SUCCESS) or // Keyman 8.0
         (MsiGetProductCode(ProductCode_Keyman9, pcode) = ERROR_SUCCESS) or // Keyman 9.0
         (MsiGetProductCode(ProductCode_Keyman10, pcode) = ERROR_SUCCESS) then // Keyman 10.0
      begin
        Status(FInstallInfo.Text(ssStatusRemovingOlderVersions));
        MsiConfigureProduct(pcode, INSTALLLEVEL_DEFAULT, INSTALLSTATE_ABSENT);
        // We'll ignore errors ...
      end;
    end;

    //  InstallDefaults may have already been set True from the command line if not then
    //  update the InstallDefaults flag based on current installed version information.
    if not(InstallDefaults) and not (ContinueSetup) then
    begin
      if Assigned(FInstallInfo.MsiInstallLocation) and (FInstallInfo.InstalledVersion.Version = '') then
        InstallDefaults := True;
    end;

    { Log the install to the diag folder }

    FLogFileName := TKeymanPaths.ErrorLogPath(ChangeFileExt(ExtractFileName(msiLocation.Path), ''));  // I1610 // I2755 // I2792

    ApplyWow64SystemProfilePatch; // #6221

    MsiEnableLogW(INSTALLLOGMODE_VERBOSE, PWideChar(FLogFileName), 0);

    { Cache the msi file to avoid source dependencies for repair, patch, uninstall, bleagh }

    FCacheFileName := CacheMSIFile(msiLocation);  // I3476

    Status(FInstallInfo.Text(ssStatusInstalling));
    res := MsiInstallProductW(PWideChar(FCacheFileName), PWideChar(ReinstallMode));

    FinishCacheMSIFile(msiLocation,  // I3476
      (res = ERROR_SUCCESS_REBOOT_REQUIRED) or (res = ERROR_SUCCESS_REBOOT_INITIATED) or (res = ERROR_SUCCESS));

    case res of
      ERROR_SUCCESS: ;
      ERROR_SUCCESS_REBOOT_REQUIRED, ERROR_SUCCESS_REBOOT_INITIATED:
        begin
          PrepareForReboot(res, InstallDefaults); // We need to continue this install after reboot
          Result := False;
          Exit;
        end;
      ERROR_INSTALL_USEREXIT: begin Result := False; Exit; end;
    else
      CheckMSIResult(msiLocation, res);
    end;
  end;

  if not FSilent then
    MsiSetInternalUI(INSTALLUILEVEL_FULL, nil);
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

procedure TRunTools.ConfigFirstRun(StartKeyman,StartWithWindows,CheckForUpdates,
  StartDisabled,StartWithConfiguration,InstallDefaults,AutomaticallyReportUsage: Boolean);
var
  i: Integer;
  s: WideString;
  FKMShellPath: WideString;
  FExitCode: Cardinal;
  FKMShellVersion: string;

  procedure DoInstallPackages;
  var
    s: string;
    pack: TInstallInfoPackage;
    packLocation: TInstallInfoPackageFileLocation;
    FExitCode: Cardinal;
  begin
    s := '-nowelcome -s -i ';
    for pack in FInstallInfo.Packages do
    begin
      if pack.ShouldInstall then
      begin
        packLocation := pack.InstallLocation;
        if Assigned(packLocation) then
        begin
          if packLocation.LocationType = iilOnline then
          begin
            LogInfo('Downloading '+packLocation.Url);
            if not TResourceDownloader.Execute(FInstallInfo, packLocation, FSilent) then
            begin
              LogInfo('Failed to download '+packLocation.Url);
              pack.ShouldInstall := False;
              Continue;
            end;
          end;

          // Need to check kmshell version >= 14.0 for support for non-default
          // language registration into local machine context; will not install
          // current user context.
          //
          // If kmshell.exe is an older version, then the default language for
          // the keyboard will be registered *and* installed, and furthermore,
          // the user's language selection will be ignored (sadface).

          // Note, if the BCP47 code specified here is in the list of the package's
          // supported BCP47 codes, then it would have been registered anyway. This
          // allows for custom BCP47 codes to be registered while elevated.
          if CompareVersions(SKeymanVersion_Min_SpecifyLanguage, FKMShellVersion) >= 0
            then s := s + '"'+packLocation.Path+'='+pack.BCP47+'" '
            else s := s + '"'+packLocation.Path+'" ';  // I3476
        end;
      end;
    end;
    TUtilExecute.WaitForProcess('"'+FKMShellPath+'" '+s, ExtractFilePath(FKMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349

    if CompareVersions(SKeymanVersion_Min_SpecifyLanguage, FKMShellVersion) >= 0 then
    begin
      // For each package add the BCP47 install details for current user.
      // This relies on the TIP having been registered by the previous invocation
      // of kmshell.exe -i
      s := '-s -install-tips-for-packages ';
      for pack in FInstallInfo.Packages do
      begin
        if pack.ShouldInstall and Assigned(pack.InstallLocation) then
          s := s + '"'+pack.ID+'='+pack.BCP47+'" ';
      end;

      TUtilExecute.CreateProcessAsShellUser(FKMShellPath, '"'+FKMShellPath+'" '+s, True, FExitCode);
    end;
  end;

  function IsKeymanRunning: Boolean;
  begin
    Result := FindWindow('TfrmKeyman7Main', nil) <> 0;
  end;
begin
  FKMShellPath := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
  if System.SysUtils.FileExists(FKMShellPath) then
  begin
    RunVersion6Upgrade(FKMShellPath);
    RunVersion7Upgrade(FKMShellPath);  // I2548
    RunVersion8Upgrade(FKMShellPath);  // I4293
    RunVersion9Upgrade(FKMShellPath);
    RunVersion10Upgrade(FKMShellPath);
    RunVersion11To13Upgrade(FKMShellPath);

    FKMShellVersion := GetFileVersionString(FKMShellPath);

    // Install packages for all users; keyboard languages (TIPs) are only
    // installed for the current user
    if FInstallInfo.Packages.Count > 0 then
      DoInstallPackages;

    // Configure Keyman for initial install, both local-machine,
    // and current-user
    s := '-firstrun=';

    if StartWithWindows then s := s + 'StartWithWindows,';
    if CheckForUpdates then s := s + 'CheckForUpdates,';
    if AutomaticallyReportUsage then s := s + 'AutomaticallyReportUsage,';

    if InstallDefaults then
    begin
      s := s + 'InstallDefaults,';  // I2651
      s := s + ' -defaultuilanguage='+TSetupUILanguageManager.ActiveLocale;
    end;

    if StartDisabled then
    begin
      s := s + ' -disablePackages=';
      for i := 0 to FInstallInfo.Packages.Count - 1 do
      begin
        if i > 0 then s := s + ',';
        s := s + '"'+FInstallInfo.Packages[i].ID+'"';
      end;
    end;

    TUtilExecute.WaitForProcess('"'+FKMShellPath+'" '+s, ExtractFilePath(FKMShellPath), SW_SHOWNORMAL, WaitFor); // I2605 - for admin-installed packages  // I3349

    FExitCode := 0;
    if not TUtilExecute.CreateProcessAsShellUser(FKMShellPath, '"'+FKMShellPath+'" '+s, True, FExitCode) then // I2757
      LogError('Failed to setup default options for Keyman: '+SysErrorMessage(GetLastError))
    else if FExitCode = 2 then
    begin
      LogError('Failed to setup Keyman to start with Windows; this action may have been blocked by security software.');
    end;
  end;

  DeleteBackupPath;  // I2747

  // Delete the Keyman autostart backup
  with CreateHKCURegistry do
  try
    if OpenKey('\' + SRegKey_KeymanDesktop_CU, True) and ValueExists(SRegValue_UpgradeRunKeyman) then
      DeleteValue(SRegValue_UpgradeRunKeyman);
  finally
    Free;
  end;

  if StartKeyman then  // I2738
  begin
    if System.SysUtils.FileExists(FKMShellPath) then
    begin
      if StartWithConfiguration or not IsKeymanRunning then
      begin
        s := '"'+FKMShellPath+'"';
        if StartWithConfiguration then
          s := s + ' -startWithConfiguration';
        if not TUtilExecute.CreateProcessAsShellUser(FKMShellPath, s, False) then  // I2741
          LogError('Failed to start Keyman: '+SysErrorMessage(GetLastError), False); // I2756
      end;
    end;
  end;

  Status(FInstallInfo.Text(ssStatusComplete));
end;

procedure TRunTools.PrepareForReboot(res: Cardinal; InstallDefaults: Boolean);
var
  FTempFilename: string;
  s: string;
begin
  FMustReboot := True;

  with CreateHKCURegistry do  // I2749
  try
    if OpenKey(SRegKey_WindowsRunOnce_CU, True) then
    begin
      FTempFilename := KGetTempFilename;
      FInstallInfo.SaveToJSONFile(FTempFilename);
      s := '"'+ParamStr(0)+'" -c "'+FTempFilename+'"';
      if InstallDefaults then
        s := s + ' -d';
      WriteString(SRegValue_WindowsRunOnce_Setup, s);
    end;
  finally
    Free;
  end;

  // Delete the Keyman autostart; it will be reinstated when setup finishes
  // but this avoids conflict of Keyman starting while the setup is still
  // running
  with CreateHKCURegistry do
  try
    if OpenKey(SRegKey_WindowsRun_CU, False) and ValueExists(SRegValue_WindowsRun_Keyman) then
    begin
      DeleteValue(SRegValue_WindowsRun_Keyman);
      if OpenKey('\' + SRegKey_KeymanDesktop_CU, True) then
        WriteBool(SRegValue_UpgradeRunKeyman, True);
    end;
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

procedure TRunTools.RunVersion11To13Upgrade(const KMShellPath: WideString);
begin
  // We run this after installation; it is idempotent and will upgrade all keyboards
  // from earlier versions to version 14+
  TUtilExecute.WaitForProcess('"'+KMShellPath+'" -upgradekeyboards=13,admin', ExtractFilePath(KMShellPath), SW_SHOWNORMAL, WaitFor);  // I3349
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

//
// See #6221.
//
// If Keyman is being installed under the SYSTEM profile, e.g. when using tools
// such as SCCM or Intune, then the installer runs with a redirected folder for
// %LocalAppData%, under C:\Windows\SysWow64\config, but is given the name
// C:\Windows\System32\config....
//
// Keyman Setup then creates %LocalAppData%\Keyman\Diag, which it thinks is in
// C:\Windows\System32\config... But in reality it is in SysWow64... Keyman
// Setup passes a file under this folder to Windows Installer, running as 64
// bit, which immediately falls over because the path does not exist to it, in
// the real System32\config folder.
//
// This patch disables redirection temporarily just to create the folder under
// both 32 and 64 bit versions of the profile. It has no effect on normal user
// accounts (apart from verifying that the folder is present twice rather than
// once).
//
procedure TRunTools.ApplyWow64SystemProfilePatch;
type
  TWow64DisableWow64FsRedirection = function(out cookie: PVOID): BOOL; stdcall;
  TWow64RevertWow64FsRedirection = function(cookie: PVOID): BOOL; stdcall;
var
  cookie: PVOID;
  hKernel32: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection;
begin
  @Wow64DisableWow64FsRedirection := nil;
  @Wow64RevertWow64FsRedirection := nil;

  hKernel32 := GetModuleHandle('kernel32.dll');
  if hKernel32 <> 0 then
  begin
    @Wow64DisableWow64FsRedirection := GetProcAddress(hKernel32, 'Wow64DisableWow64FsRedirection');
    @Wow64RevertWow64FsRedirection := GetProcAddress(hKernel32, 'Wow64RevertWow64FsRedirection');
  end;

  if Assigned(Wow64DisableWow64FsRedirection) and Assigned(Wow64RevertWow64FsRedirection) then
  begin
    if Wow64DisableWow64FsRedirection(cookie) then
    begin
      try
        if not ForceDirectories(TKeymanPaths.ErrorLogPath) then
          LogError(Format('Could not create diag path: %d %s',
            [GetLastError, SysErrorMessage(GetLastError)]), False);
      finally
        if not Wow64RevertWow64FsRedirection(cookie) then
          LogError(Format('Wow64RevertWow64FsRedirection failed: %d %s',
            [GetLastError, SysErrorMessage(GetLastError)]), False);
      end;
    end
    else
      LogError(Format('Wow64DisableWow64FsRedirection failed: %d %s',
        [GetLastError, SysErrorMessage(GetLastError)]),False);
  end
  else
    LogError('Could not find Wow64 fs redirection functions', False);
end;

initialization
finalization
  FreeAndNil(FRunTools);
end.
