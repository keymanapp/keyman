(*
  Name:             bootstrapmain
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 Jun 2007

  Modified Date:    23 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Jun 2007 - mcdurdin - Initial version
                    05 Jun 2007 - mcdurdin - I817 - Fix -x option
                    19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    14 Sep 2007 - mcdurdin - I1065 - -s silent option in setup.exe stills asks about updated version
                    14 Sep 2007 - mcdurdin - I1066 - -s option does not do a fully silent install in setup.exe
                    14 Sep 2007 - mcdurdin - I1063 - setup.exe crashing when an update is found online rather than installing it
                    27 Mar 2008 - mcdurdin - I1306 - Fix -s option not doing fully silent extract
                    14 Jun 2008 - mcdurdin - Close handles after starting app
                    28 Jul 2008 - mcdurdin - I1158 - Report error codes
                    30 Dec 2010 - mcdurdin - I2562 - EULA in bootstrapper
                    31 Dec 2010 - mcdurdin - I2615 - Fix hidden controls when Alt pressed in Vista
                    31 Dec 2010 - mcdurdin - I2611 - Incorrect icon for setup form
                    21 Feb 2011 - mcdurdin - I2738 - Auto update does not start automatically when triggered
                    31 Mar 2011 - mcdurdin - I2847 - Version 8.0 silent upgrade runs keyboard import from version 7.0 and shouldn't
                    03 Oct 2011 - mcdurdin - I3081 - Create target folder when -x specified
                    04 Nov 2011 - mcdurdin - I3126 - Add flag to control msi UI level
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    19 Oct 2012 - mcdurdin - I3476 - V9.0 - Fixup additional hints and warnings around string conversion
                    02 Feb 2012 - mcdurdin - I2975 - VistaAltFixUnit can crash on shutdown
                    15 Jun 2012 - mcdurdin - I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    03 Nov 2012 - mcdurdin - I3500 - V9.0 - Merge of I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    28 Feb 2014 - mcdurdin - I4099 - V9.0 - Keyman Setup dialog is still 8.0 style
                    24 Jun 2014 - mcdurdin - I4293 - V9.0 - Setup bootstrapper does not check for V8 upgrade
                    12 Aug 2014 - mcdurdin - I4365 - V9.0 - Installer still doesn't enforce Win7 or later
                    16 Oct 2014 - mcdurdin - I4460 - V9.0 - Setup bootstrapper should handle upgrade scenarios with a prompt
                    23 Oct 2014 - mcdurdin - I4470 - Internet Explorer 9.0 of later required [CrashID:kmshell.exe_9.0.472.0_script_TfrmMain_0]
*)
unit bootstrapmain;  // I3306

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  Keyman.Setup.System.InstallInfo,
  SetupStrings;

var
  FInstallInfo: TInstallInfo = nil;

procedure Run;
function FormatFileSize(Size: Integer): string;

implementation

uses
  System.StrUtils,
  System.TypInfo,
  Vcl.Forms,
  Winapi.ActiveX,
  Winapi.Windows,

  CommonControls,
  ErrorControlledRegistry,
  GetOsVersion,
  Keyman.Setup.System.OnlineResourceCheck,
  Keyman.Setup.System.SetupUILanguageManager,
  Keyman.System.UpgradeRegistryKeys,
  KeymanPaths,
  KeymanVersion,
  RegistryHelpers,
  RegistryKeys,
  RunTools,
  SetupForm,
  SFX,
  TntDialogHelp,
  UfrmDownloadProgress,
  UfrmRunDesktop,
  Upload_Settings,
  utilexecute;

type
  TSetupBootstrap = class
  private
    ProgramPath: string;
    FExtractOnly: Boolean;
    FContinueSetupFilename: string;
    FStartAfterInstall: Boolean;  // I2738
    FDisableUpgradeFrom6Or7Or8: Boolean; // I2847   // I4293
    FPromptForReboot: Boolean;  // I3355   // I3500
    FSilent: Boolean;
    FForceOffline: Boolean;
    FInstallDefaults: Boolean;
    FTier, FPackages, FExtractOnly_Path: string;

    function CheckForOldVersionScenario: Boolean;
    procedure InstallKeyboardsInOldVersion(const ShellPath: string);
    procedure DoExtractOnly(FSilent: Boolean; const FExtractOnly_Path: string);
    function CreateTempDir: string;
    procedure RemoveTempDir(const path: string);
    procedure ProcessCommandLine(
      var FPromptForReboot, FSilent, FForceOffline, FExtractOnly: Boolean;
      var FContinueSetupFilename: string;
      var FStartAfterInstall, FDisableUpgradeFrom6Or7Or8, FInstallDefaults: Boolean;
      var FPackages, FExtractPath, FTier: string);
    procedure SetExitVal(c: Integer);
    function IsKeymanDesktop7Installed: string;
    function IsKeymanDesktop8Installed: string;
    function GetResourcesFromOnline(FSilent: Boolean;
      var FForceOffline: Boolean): Boolean;
    procedure LocateResourcesCallback(Owner: TfrmDownloadProgress;
      var Result: Boolean);
    procedure DeletePath(const path: WideString);
  public
    procedure Run;
  end;

var
  FNiceExitCodes: Boolean = True; // always, now

const
  ICC_PROGRESS_CLASS     = $00000020; // progress


procedure Run;
var
  SetupBootstrap: TSetupBootstrap;
begin
  SetupBootstrap := TSetupBootstrap.Create;
  try
    SetupBootstrap.Run;
  finally
    SetupBootstrap.Free;
  end;
end;

procedure TSetupBootstrap.Run;
var
  FTempPath: string;
  frmDownloadProgress: TfrmDownloadProgress;
  msiLocation: TInstallInfoFileLocation;
  FResult: Boolean;
BEGIN
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    try
      Vcl.Forms.Application.Icon.LoadFromResourceID(hInstance, 1);  // I2611
      InitCommonControl(ICC_PROGRESS_CLASS);

      TSetupUILanguageManager.SetDefault;

      FTempPath := CreateTempDir;
      try
        FInstallInfo := TInstallInfo.Create(FTempPath);
        try
          { Display the dialog }

          ProcessCommandLine(
            FPromptForReboot, FSilent, FForceOffline, FExtractOnly,
            FContinueSetupFilename, FStartAfterInstall,
            FDisableUpgradeFrom6Or7Or8, FInstallDefaults, FPackages, FExtractOnly_Path, FTier);  // I2738, I2847  // I3355   // I3500   // I4293

          GetRunTools.Silent := FSilent;

          if FContinueSetupFilename <> '' then
          begin
            if not FileExists(FContinueSetupFilename) then
            begin
              GetRunTools.LogError('Control file '+FContinueSetupFilename+' for continuing setup is missing. Cannot continue.');
              SetExitVal(ERROR_FILE_NOT_FOUND);
              Exit;
            end;

            FInstallInfo.LoadFromJSONFile(FContinueSetupFilename);

            // We're using an existing temp folder, so throw away the one we
            // just created and use the other one instead.
            RemoveDir(FTempPath);
            FTempPath := FInstallInfo.TempPath;
          end
          else if FExtractOnly then
          begin
            DoExtractOnly(FSilent, FExtractOnly_Path);
            Exit;
          end;

          // There are two possible paths for files: ProgramPath and TempPath,
          // and also files that can be downloaded during the installation
          // (into TempPath).

          ProgramPath := ExtractFilePath(ParamStr(0));

          if FContinueSetupFilename = '' then
          begin
            // We only need to collect install info if we are not
            // continuing a previous setup run.
            if not FSilent then
            begin
              frmDownloadProgress := TfrmDownloadProgress.Create(nil);
              try
                frmDownloadProgress.Callback := LocateResourcesCallback;
                if frmDownloadProgress.ShowModal = mrCancel then
                  Exit;
              finally
                frmDownloadProgress.Free;
              end;
            end
            else
            begin
              FResult := True;
              LocateResourcesCallback(nil, FResult);
              if not FResult then
                Exit;
            end;
          end;

          with TfrmRunDesktop.Create(nil) do    // I2562
          try
            ContinueSetup := FContinueSetupFilename <> '';
            StartAfterInstall := FStartAfterInstall; // I2738
            DisableUpgradeFrom6Or7Or8 := FDisableUpgradeFrom6Or7Or8;  // I2847   // I4293
            InstallDefaults := FInstallDefaults;
            if FSilent
              then DoInstall(True, FPromptForReboot)  // I3355   // I3500
              else ShowModal;
          finally
            Free;
          end;
        finally
          FreeAndNil(FInstallInfo);
        end;

        SetExitVal(ERROR_SUCCESS);

      finally
        if FContinueSetupFilename <> '' then
        begin
          System.SysUtils.DeleteFile(FContinueSetupFilename);
        end;
        if not GetRunTools.MustReboot then
        begin
          // If we are going to reboot, then we don't want to delete the
          // temporary files, because they will be used to continue setup
          // after rebooting.
          RemoveTempDir(FTempPath);
        end;
      end;
    except
      on e:Exception do
      begin
        // For the future, we may consider logging exceptions with Sentry.
        // However, Sentry adds a 400kb library to the bootstrap which we
        // would need to embed and extract at startup in order to do this
        // capture. That's a bit of a downer for this project, which we are
        // trying (somewhat unsuccessfully) to keep as lightweight as we can.
        GetRunTools.LogError('Exception '+e.ClassName+': '+e.Message);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TSetupBootstrap.LocateResourcesCallback(Owner: TfrmDownloadProgress; var Result: Boolean);
var
  FProcessFileName: string;
begin
  Result := False;

  if Assigned(Owner) then
  begin
    Owner.Caption := FInstallInfo.Text(ssApplicationTitle);
    if not Owner.Status(FInstallInfo.Text(ssBootstrapExtractingBundle), 0, 4) then Exit;
  end;
  // Extract SFX archive into temp path

  ProcessArchive(FInstallInfo.TempPath);

  // Get the list of anticipated packages from:
  // 1. the program filename
  // 2. parameters
  // 3. files extracted from the archive
  // 4. files in the same folder as this program

  // The filename of this executable can be changed to tell it which
  // packages to download, e.g. keyman-setup.khmer_angkor.km.exe tells
  // it to download khmer_angkor from the Keyman cloud and install it
  // for bcp47 tag km. See the setup documentation for more
  // examples.
{$IFDEF DEBUG}
  // In order to run this in the debugger, it's much easier to define
  // an environment variable than to rename the executable, but we don't
  // want this except for debug builds.
  FProcessFileName := GetEnvironmentVariable('KEYMAN_SETUP_TESTFILENAME');
  if FProcessFileName = '' then
{$ENDIF}
  FProcessFileName := ParamStr(0);

  FInstallInfo.LocatePackagesAndTierFromFilename(FProcessFileName);

  // Additionally, packages can be specified on the command line, with
  // the -p parameter, e.g. -p khmer_angkor=km,sil_euro_latin=fr
  FInstallInfo.LocatePackagesFromParameter(FPackages);

  // Lookup a tier from command line parameter
  if FTier <> '' then
    FInstallInfo.Tier := FTier;

  // This loads setup.inf, if present, for various additional strings and settings
  // The bundled installer usually contains a setup.inf.
  if FileExists(FInstallInfo.TempPath + 'setup.inf') then
    FInstallInfo.LoadSetupInf(FInstallInfo.TempPath)
  else if FileExists(ProgramPath + 'setup.inf') then
    FInstallInfo.LoadSetupInf(ProgramPath);

  if Assigned(Owner) then
    if not Owner.Status(FInstallInfo.Text(ssBootstrapCheckingPackages), 1) then Exit;

  // Finally, load details for any .kmp packages referenced in setup.inf
  FInstallInfo.LoadLocalPackagesMetadata;

  if Assigned(Owner) and not FForceOffline then
    if not Owner.Status(FInstallInfo.Text(ssBootstrapCheckingForUpdates), 2) then Exit;

  // Try and get information from online
  if not GetResourcesFromOnline(FSilent, FForceOffline) then
  begin
    SetExitVal(ERROR_FILE_NOT_FOUND);
    Exit;
  end;

  if Assigned(Owner) then
    if not Owner.Status(FInstallInfo.Text(ssBootstrapCheckingInstalledVersions), 3) then Exit;

  FInstallInfo.CheckPackageLocations;

  // If the user is trying to install on downlevel version of Windows (Vista or earlier),
  // we can simplify their life by installing the packages into an existing Keyman
  // install, or point them to the old version of Keyman otherwise.
  if CheckForOldVersionScenario then   // I4460
    Exit;

  TRunTools.CheckInstalledVersion(FInstallInfo.MsiInstallLocation);

  // Looks for installation state for Keyman, and determines best packages for installation.
  // If no .msi can be found, and Keyman is not installed, this will show/log an error and
  // abort installation.
  if not FInstallInfo.CheckMsiUpgradeScenarios then
  begin
    // TODO: this should be refactored together with the retry strategy for online check above
    // TODO: Delineate between log messages and dialogs.
    // TODO: localize
    GetRunTools.LogError('A valid Keyman install could not be found offline. Please connect to the Internet or allow this installer through your firewall in order to install Keyman.');
    SetExitVal(ERROR_NO_MORE_FILES);
    Exit;
  end;

  // Default scenario is that if any newer installer is available, then
  // Setup should install it.
  FInstallInfo.ShouldInstallKeyman := FInstallInfo.IsNewerAvailable;

  if Assigned(Owner) then
    if not Owner.Status(FInstallInfo.Text(ssBootstrapReady), 4) then Exit;

  Result := True;
end;

function TSetupBootstrap.GetResourcesFromOnline(FSilent: Boolean; var FForceOffline: Boolean): Boolean;
begin
  if FForceOffline then
    Exit(True);
  repeat
    try
      GetRunTools.CheckInternetConnectedState;

      if GetRunTools.Online then
        TOnlineResourceCheck.QueryServer(FSilent, FInstallInfo);

      // We've succeeded.
      Exit(True);
    except
      on E:Exception do
      begin
        GetRunTools.LogInfo('Could not connect to site: '+E.Message);
        if FSilent then
        begin
          // We log and attempt to continue
          FForceOffline := True;
        end
        else
        begin
          case MessageDlgW(
              Trim(FInstallInfo.Text(ssOffline)+#13#10#13#10+FInstallInfo.Text(ssOffline2)+#13#10#13#10+FInstallInfo.Text(ssOffline3)),
              mtError, mbAbortRetryIgnore, 0) of
            mrAbort: Exit(False);
            mrRetry: Continue;
            mrIgnore: FForceOffline := True;
          end;
        end;
      end;
    end;
  until FForceOffline;

  Result := True;
end;

function TSetupBootstrap.CheckForOldVersionScenario: Boolean;   // I4460
var
  OldKMShellPath: string;
begin
  if not (GetOS in [osLegacy, osVista]) then   // I4365
    Exit(False);

  if FInstallInfo.Packages.Count = 0 then
  begin
    // No keyboards installed, so we determine install based on Windows version
    if MessageDlgW(FInstallInfo.Text(ssOldOsVersionDownload), mtConfirmation, mbOkCancel, 0) = mrOk then
      TUtilExecute.URL(MakeKeymanURL(URLPath_ArchivedDownloads));
    SetExitVal(ERROR_OLD_WIN_VERSION);
    Exit(True);
  end;

  OldKMShellPath := IsKeymanDesktop8Installed;
  if OldKMShellPath = '' then OldKMShellPath := IsKeymanDesktop7Installed;

  if OldKMShellPath <> '' then
  begin
    if MessageDlgW(FInstallInfo.Text(ssOldOsVersionInstallKeyboards), mtConfirmation, mbYesNoCancel, 0) = mrYes then
      InstallKeyboardsInOldVersion(OldKMShellPath);
  end
  else
  begin
    if MessageDlgW(FInstallInfo.Text(ssOldOsVersionDownload), mtConfirmation, mbOkCancel, 0) = mrOk then
      TUtilExecute.URL(MakeKeymanURL(URLPath_ArchivedDownloads));
  end;
  SetExitVal(ERROR_OLD_WIN_VERSION);
  Exit(True);
end;

// TODO: move this into a separate unit: it deals with installing keyboards into
// an existing Keyman install on downlevel versions of Windows (e.g. Vista). So
// we are constrained in how we do this -- no ability to do language associations
// etc.
procedure TSetupBootstrap.InstallKeyboardsInOldVersion(const ShellPath: string);   // I4460
  procedure DoInstall(pack: TInstallInfoPackage; const silentFlag: string);
  var
    location: TInstallInfoPackageFileLocation;
  begin
    location := pack.InstallLocation;
    if Assigned(location) and pack.ShouldInstall then
    begin
      if location.LocationType = iilOnline then
        Assert(FALSE, 'TODO: implement download of this resource');
        //TODO:
      TUtilExecute.WaitForProcess('"' + ShellPath + '" '+silentFlag+' -i "'+location.Path+'"', FInstallInfo.TempPath);
    end;
  end;
var
  pack: TInstallInfoPackage;
begin
  if FInstallInfo.Packages.Count = 1 then
  begin
    DoInstall(FInstallInfo.Packages[0], '');
  end
  else
  begin
    for pack in FInstallInfo.Packages do
      DoInstall(pack, '-s');
    ShowMessageW('All packages have been installed');
  end;
end;

procedure TSetupBootstrap.DoExtractOnly(FSilent: Boolean; const FExtractOnly_Path: string);
var
  path: string;
begin
  path := FExtractOnly_Path;  // I3476

  if path = '' then
    path := '.';

  if (path <> '.') and (path <> '.\') and not DirectoryExists(path) then  // I3081  // I3476
  begin
    if not CreateDir(path) then  // I3476
    begin
      GetRunTools.LogError('Setup could not create the target folder '+path);  // I3476
      SetExitVal(Integer(GetLastError));
      Exit;
    end;
  end;

  if not ProcessArchive(path) then
  begin
    GetRunTools.LogError('This file was not a valid self-extracting archive.  The files should already be in the same folder as the archive.');
    SetExitVal(ERROR_BAD_FORMAT);
    Exit;
  end;

  GetRunTools.LogInfo('All files extracted from the archive to '+path+'\.', True);  // I3476
  SetExitVal(ERROR_SUCCESS);
end;

function TSetupBootstrap.CreateTempDir: string;
var
  buf: array[0..260] of WideChar;
  path: string;
begin
  GetTempPath(MAX_PATH-1, buf);
  path := ExcludeTrailingPathDelimiter(buf);  // I3476
  GetTempFileName(PWideChar(path), 'kmt', 0, buf);  // I3476
  path := buf;  // I3476
  if FileExists(buf) then DeleteFile(buf);  // I3476
  // NOTE: race condition here...
  CreateDirectory(buf, nil);  // I3476
  Result := IncludeTrailingPathDelimiter(path);
end;

procedure TSetupBootstrap.DeletePath(const path: WideString);
var
  fd: TWin32FindDataW;
  n: DWord;
begin
  n := FindFirstFile(PWideChar(path + '\*.*'), fd);
  if n = INVALID_HANDLE_VALUE then Exit;
  repeat
    DeleteFile(PWideChar(path + '\' + fd.cFileName));
  until not FindNextFile(n, fd);
  FindClose(n);
  RemoveDirectory(PWideChar(path));
end;

procedure TSetupBootstrap.RemoveTempDir(const path: string);
begin
  if path <> '' then
    DeletePath(ExcludeTrailingPathDelimiter(path));  // I3476
end;

procedure TSetupBootstrap.ProcessCommandLine(
  var FPromptForReboot, FSilent, FForceOffline, FExtractOnly: Boolean;
  var FContinueSetupFilename: string;
  var FStartAfterInstall, FDisableUpgradeFrom6Or7Or8, FInstallDefaults: Boolean;
  var FPackages, FExtractPath, FTier: string
);
var
  i: Integer;
begin
  FPromptForReboot := True;  // I3355   // I3500
  FSilent := False;
  FForceOffline := False;
  FExtractOnly := False;
  FContinueSetupFilename := '';
  FDisableUpgradeFrom6Or7Or8 := False; // I2847   // I4293
  FStartAfterInstall := True;  // I2738
  FInstallDefaults := False;
  i := 1;
  while i <= ParamCount do
  begin
    if WideSameText(ParamStr(i), '-c') then
    begin
      Inc(i);
      FContinueSetupFilename := ParamStr(i);
    end
    else if WideSameText(ParamStr(i), '-s') then
    begin
      FSilent := True;
      FStartAfterInstall := False;
      FPromptForReboot := False;  // I3355   // I3500
    end
    else if WideSameText(ParamStr(i), '-au') then  // I2738
    begin
      // auto update - options for more flexibility later...
      FSilent := True;
      FForceOffline := True;
      FStartAfterInstall := True;
      FDisableUpgradeFrom6Or7Or8 := True;  // I2847   // I4293
    end
    else if WideSameText(ParamStr(i), '-d') then
      FInstallDefaults := True
    else if WideSameText(ParamStr(i), '-o') then
      FForceOffline := True
    else if WideSameText(ParamStr(i), '-r') then
      // previously, 'nice exit codes'
    else if WideSameText(ParamStr(i), '-x') then
    begin
      Inc(i);
      FExtractOnly := True;
      FExtractPath := ParamStr(i);
      if FExtractPath = '' then
        FExtractPath := ExtractFilePath(ParamStr(0));
    end
    else if SameText(ParamStr(i), '-p') then
    begin
      // e.g. -p khmer_angkor=km,sil_euro_latin=fr
      Inc(i);
      FPackages := ParamStr(i);
    end
    else if SameText(ParamStr(i), '-t') then
    begin
      Inc(i);
      FTier := ParamStr(i).ToLower.Trim;
      if not FTier.Equals(TIER_ALPHA) and not FTier.Equals(TIER_BETA) and not FTier.Equals(TIER_STABLE) then
      begin
        FTier := '';
      end;
    end;
    Inc(i);
  end;
end;

procedure TSetupBootstrap.SetExitVal(c: Integer);
begin
  if c = ERROR_SUCCESS
    then GetRunTools.LogInfo('Install finished successfully')
    else GetRunTools.LogError('Install failed, exiting with error code '+IntToStr(c), False);
  ExitCode := c;
end;

function TSetupBootstrap.IsKeymanDesktop7Installed: string;   // I4460
begin
  with CreateHKLMRegistry do
  try
    if OpenKeyReadOnly(SRegKey_Keyman70_LM) and ValueExists('root path')  //TODO -- test this
      then Result := IncludeTrailingPathDelimiter(ReadString('root path')) + TKeymanPaths.S_KMShell
      else Exit('');

    if not FileExists(Result) then
      Result := '';
  finally
    Free;
  end;
end;

function TSetupBootstrap.IsKeymanDesktop8Installed: string;   // I4460
begin
  with CreateHKLMRegistry do
  try
    if OpenKeyReadOnly(SRegKey_Keyman80_LM) and ValueExists('root path')  //TODO -- test this
      then Result := IncludeTrailingPathDelimiter(ReadString('root path')) + TKeymanPaths.S_KMShell
      else Exit('');

    if not FileExists(Result) then
      Result := '';
  finally
    Free;
  end;
end;

function FormatFileSize(Size: Integer): string;
begin
  if Size > 1024*1024 then
    Result := Format('%.1fMB', [Size/1024/1024])
  else if Size > 1024 then
    Result := Format('%dKB',[Size div 1024])
  else if Size = 1 then
    Result := '1 byte'
  else
    Result := Format('%d bytes', [Size]);
end;

end.
