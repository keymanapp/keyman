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
                    28 Feb 2014 - mcdurdin - I4099 - V9.0 - Keyman Desktop Setup dialog is still 8.0 style
                    24 Jun 2014 - mcdurdin - I4293 - V9.0 - Setup bootstrapper does not check for V8 upgrade
                    12 Aug 2014 - mcdurdin - I4365 - V9.0 - Installer still doesn't enforce Win7 or later
                    16 Oct 2014 - mcdurdin - I4460 - V9.0 - Setup bootstrapper should handle upgrade scenarios with a prompt
                    23 Oct 2014 - mcdurdin - I4470 - Internet Explorer 9.0 of later required [CrashID:kmshell.exe_9.0.472.0_script_TfrmMain_0]
*)
unit bootstrapmain;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  SetupStrings;

type
  EInstallInfo = class(Exception);


  TInstallInfo = class
  private
    FAppName: WideString;
    FMSIFileName: WideString;
    FMSIOptions: string;  // I3126
    FVersion: WideString;
    FPackages: TStrings;
    FStrings: TStrings;
    FLicenseFileName: WideString;  // I2562
    FTitleImageFilename: string;
    FStartDisabled: Boolean;
    FStartWithConfiguration: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    function Text(const Name: TInstallInfoText): WideString; overload;
    function Text(const Name: TInstallInfoText; const Args: array of const): WideString; overload;
    property Strings: TStrings read FStrings;
    property EditionTitle: WideString read FAppName;
    property MSIFileName: WideString read FMSIFileName;
    property MSIOptions: string read FMSIOptions;  // I3126
    property Version: WideString read FVersion write FVersion;
    property Packages: TStrings read FPackages;
    property LicenseFileName: WideString read FLicenseFileName;  // I2562
    property TitleImageFilename: string read FTitleImageFilename;
    property StartDisabled: Boolean read FStartDisabled;
    property StartWithConfiguration: Boolean read FStartWithConfiguration;
  end;

var
  FInstallInfo: TInstallInfo = nil;

procedure Run;
//function ExecuteApp(const cmdline, curdir: WideString; sw: Integer): Boolean;

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
  Keyman.System.UpgradeRegistryKeys,
  KeymanPaths,
  KeymanVersion,
  OnlineConstants,
  RegistryHelpers,
  RegistryKeys,
  SetupForm,
  SFX,
  TntDialogHelp,
  UfrmRunDesktop,
  Upload_Settings,
  utilexecute;

function CheckDependencies(FSilent: Boolean): Boolean; forward;   // I4470

var
  FNiceExitCodes: Boolean = False;

const
  ICC_PROGRESS_CLASS     = $00000020; // progress


var
  TempPath: string = '';

procedure CreateTempDir;
var
  buf: array[0..260] of WideChar;
begin
  GetTempPath(MAX_PATH-1, buf);
  ExtPath := ExcludeTrailingPathDelimiter(buf);  // I3476
  GetTempFileName(PWideChar(ExtPath), 'kmt', 0, buf);  // I3476
  ExtPath := buf;  // I3476
  if FileExists(buf) then DeleteFile(buf);  // I3476
  // NOTE: race condition here...
  CreateDirectory(buf, nil);  // I3476
  TempPath := ExtPath;
end;

procedure DeletePath(const path: WideString);
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

procedure RemoveTempDir;
begin
  if TempPath <> '' then
    DeletePath(TempPath);  // I3476
end;

procedure ProcessCommandLine(var FPromptForReboot, FSilent, FOffline, FExtractOnly, FContinueSetup, FStartAfterInstall, FDisableUpgradeFrom6Or7Or8: Boolean; var FExtractPath: WideString);  // I2847  // I3355   // I3500   // I4293
var
  i: Integer;
begin
  FPromptForReboot := True;  // I3355   // I3500
  FSilent := False;
  FOffline := False;
  FExtractOnly := False;
  FContinueSetup := False;
  FDisableUpgradeFrom6Or7Or8 := False; // I2847   // I4293
  FStartAfterInstall := True;  // I2738
  i := 1;
  while i <= ParamCount do
  begin
    if WideSameText(ParamStr(i), '-c') then
      FContinueSetup := True
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
      FOffline := True;
      FStartAfterInstall := True;
      FDisableUpgradeFrom6Or7Or8 := True;  // I2847   // I4293
    end
    else if WideSameText(ParamStr(i), '-o') then
      FOffline := True
    else if WideSameText(ParamStr(i), '-r') then
      FNiceExitCodes := True
    else if WideSameText(ParamStr(i), '-x') then
    begin
      Inc(i);
      FExtractOnly := True;
      FExtractPath := ParamStr(i);
    end;
    Inc(i);
  end;
end;

procedure LogError(const s: WideString);
begin
  ShowMessageW(s);
end;

procedure SetExitVal(c: Integer);
begin
  if FNiceExitCodes then
    ExitCode := c
  else if c = 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

function IsKeymanDesktop7Installed: string;   // I4460
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

function IsKeymanDesktop8Installed: string;   // I4460
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

procedure InstallKeyboardsInOldVersion(const ShellPath: string);   // I4460
var
  I: Integer;
begin
  if FInstallInfo.Packages.Count = 1 then
  begin
    TUtilExecute.WaitForProcess('"' + ShellPath + '" -i "'+ExtPath+FInstallInfo.Packages.Names[0]+'"', ExtPath);
  end
  else
  begin
    for I := 0 to FInstallInfo.Packages.Count - 1 do
      TUtilExecute.WaitForProcess('"' + ShellPath + '" -s -i "'+ExtPath+FInstallInfo.Packages.Names[i]+'"', ExtPath);
    ShowMessageW('All packages have been installed');
  end;
end;

function CheckForOldVersionScenario: Boolean;   // I4460
var
  OldKMShellPath: string;
begin
  if FInstallInfo.Packages.Count = 0 then
  begin
    // No keyboards installed, so we determine install based on Windows version
    if GetOS in [osLegacy, osVista] then   // I4365
    begin
      if MessageDlgW(FInstallInfo.Text(ssOldOsVersionDownload), mtConfirmation, mbOkCancel, 0) = mrOk then
        TUtilExecute.URL(MakeKeymanURL(URLPath_ArchivedDownloads));
      SetExitVal(ERROR_OLD_WIN_VERSION);
      Exit(True);
    end;
    Exit(False);
  end;

  OldKMShellPath := IsKeymanDesktop8Installed;
  if OldKMShellPath = '' then OldKMShellPath := IsKeymanDesktop7Installed;

  if GetOS in [osLegacy, osVista] then
  begin
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

  if OldKMShellPath <> '' then
  begin
    case MessageDlgW(FInstallInfo.Text(ssOldKeymanVersionInstallKeyboards), mtConfirmation, mbYesNoCancel, 0) of
      mrYes: Exit(False);
      mrNo: begin InstallKeyboardsInOldVersion(OldKMShellPath); SetExitVal(ERROR_OLD_WIN_VERSION); Exit(True); end;
      mrCancel: begin SetExitVal(ERROR_OLD_WIN_VERSION); Exit(True); end;
    end;
  end;

  Result := False;
end;

procedure Run;
var
  FExtractOnly: Boolean;
  FContinueSetup: Boolean;
  FStartAfterInstall: Boolean;  // I2738
  FDisableUpgradeFrom6Or7Or8: Boolean; // I2847   // I4293
  FPromptForReboot: Boolean;  // I3355   // I3500
  FSilent: Boolean;
  FOffline: Boolean;
  FExtractOnly_Path: WideString;
BEGIN
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    try

    Vcl.Forms.Application.Icon.LoadFromResourceID(hInstance, 1);  // I2611

    FInstallInfo := TInstallInfo.Create;

    if ParamStr(1) = '--test-dialog' then   // I4099
    begin
      with TfrmRunDesktop.Create(nil) do    // I2562
      try
        ContinueSetup := False;
        Offline := False;
        StartAfterInstall := True;
        DisableUpgradeFrom6Or7Or8 := False;   // I4293
        ShowModal;
      finally
        Free;
      end;
      Exit;
    end;

    try
      CreateTempDir;
      try
        InitCommonControl(ICC_PROGRESS_CLASS);

        { Display the dialog }

        ProcessCommandLine(FPromptForReboot, FSilent, FOffline, FExtractOnly, FContinueSetup, FStartAfterInstall, FDisableUpgradeFrom6Or7Or8, FExtractOnly_Path);  // I2738, I2847  // I3355   // I3500   // I4293

        if FExtractOnly then
        begin
          ExtPath := FExtractOnly_Path;  // I3476
          if ExtPath = '' then
            ExtPath := '.';
          if (ExtPath <> '.') and (ExtPath <> '.\') and not DirectoryExists(ExtPath) then  // I3081  // I3476
          begin
            if not CreateDir(ExtPath) then  // I3476
            begin
              LogError('Setup could not create the target folder '+ExtPath);  // I3476
              SetExitVal(Integer(GetLastError));
              Exit;
            end;
          end;
        end;

        if not ProcessArchive then
        begin
          if FExtractOnly then
          begin
            LogError('This file was not a valid self-extracting archive.  The files should already be in the same folder as the archive.');
            SetExitVal(ERROR_BAD_FORMAT);
            Exit;
          end;
          { The files must be in the current directory.  Use them instead }
          ExtPath := ExtractFilePath(ParamStr(0));  // I3476
        end
        else if FExtractOnly then
        begin
          if not FSilent then
            LogError('All files extracted from the archive to '+ExtPath+'\.');  // I3476
          SetExitVal(ERROR_SUCCESS);
          Exit;
        end;

        ExtPath := IncludeTrailingPathDelimiter(ExtPath);  // I3476

        if not System.SysUtils.FileExists(ExtPath + 'setup.inf') then  // I3476
        begin
          LogError('The file "setup.inf" is missing.  Setup cannot continue.');
          SetExitVal(ERROR_FILE_NOT_FOUND);
          Exit;
        end;

        FInstallInfo.Load;
        {TESTING CODE: ExtPath := 'c:\keyman\5.1\devel\kmredist';
        SFXDetails.Flags := [sdfRegistered, sdfGraphicDialog];}

        if CheckForOldVersionScenario then   // I4460
          Exit;

        if not CheckDependencies(FSilent) then   // I4470
          Exit;

        with TfrmRunDesktop.Create(nil) do    // I2562
        try
          ContinueSetup := FContinueSetup;
          Offline := FOffline;
          StartAfterInstall := FStartAfterInstall; // I2738
          DisableUpgradeFrom6Or7Or8 := FDisableUpgradeFrom6Or7Or8;  // I2847   // I4293
          if FSilent
              then DoInstall(False, True, FPromptForReboot)  // I3355   // I3500
            else ShowModal;
        finally
          Free;
        end;
      finally
        RemoveTempDir;
      end;
    finally
      SetExitVal(ERROR_SUCCESS);
      FInstallInfo.Free;
    end;
    except
      on e:Exception do
      begin
        //TODO: handle exceptions with JCL
        ShowMessageW(e.message);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

function CheckDependencies(FSilent: Boolean): Boolean;   // I4470
begin
  Result := False;

  // Check Internet Explorer version
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly('Software\Microsoft\Internet Explorer') or not ValueExists('Version') or (ReadString('Version') < '9.0') then
    begin
      LogError(FInstallInfo.Text(ssQueryUpdateInternetExplorer));
      SetExitVal(ERROR_NOT_SUPPORTED);
      Exit;
    end;
  finally
    Free;
  end;

  Result := True;
end;


{ TInstallInfo }

constructor TInstallInfo.Create;
begin
  inherited Create;
  FAppName := SKeymanDesktopName;
  FPackages := TStringList.Create;
  FStrings := TStringList.Create;
end;

destructor TInstallInfo.Destroy;
begin
  FPackages.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TInstallInfo.Text(const Name: TInstallInfoText): WideString;
var
  s: WideString;
begin
  s := GetEnumName(TypeInfo(TInstallInfoText), Ord(Name));
  Result := FStrings.Values[s];
  if Result = '' then
    Result := FDefaultStrings[Name];

  Result := ReplaceText(Result, '$VERSION', SKeymanVersion);
  Result := ReplaceText(Result, '$APPNAME', FAppName);
end;

procedure TInstallInfo.Load;
var
  i: Integer;
  FInSetup: Boolean;
  FInPackages: Boolean;
  FInStrings: Boolean;
  val, nm: WideString;
begin
  FInSetup := False;
  FInPackages := False;
  FInStrings := False;

  with TStringList.Create do
  try
    LoadFromFile(ExtPath + 'setup.inf');  // We'll just use the preamble for encoding  // I3476

    for i := 0 to Count - 1 do
    begin
      if Trim(Strings[i]) = '' then Continue;

      if Copy(Strings[i], 1, 1) = '[' then
      begin
        FInSetup := WideSameText(Strings[i], '[Setup]');
        FInPackages := WideSameText(Strings[i], '[Packages]');
        FInStrings := WideSameText(Strings[i], '[Strings]');
      end
      else if FInSetup then
      begin
        nm := Names[i]; val := ValueFromIndex[i];
        if WideSameText(nm, 'Version') then FVersion := val
        else if WideSameText(nm, 'AppName') then FAppName := val
        else if WideSameText(nm, 'MSIFileName') then FMSIFileName := val
        else if WideSameText(nm, 'TitleImage') then FTitleImageFileName := val
        else if WideSameText(nm, 'License') then FLicenseFileName := val  // I2562
        else if WideSameText(nm, 'MSIOptions') then FMSIOptions := val   // I3126
        else if WideSameText(nm, 'StartWithConfiguration') then FStartWithConfiguration := StrToBoolDef(val, False)
        else if WideSameText(nm, 'StartDisabled') then FStartDisabled := StrToBoolDef(val, False);
      end
      else if FInPackages then
        FPackages.Add(Strings[i])
      else if FInStrings then
        FStrings.Add(Strings[i]);
    end;

    if (FVersion = '') then
      raise EInstallInfo.Create('setup.inf is corrupt (code 1).  Setup cannot continue.');

    if not System.SysUtils.FileExists(ExtPath + FMSIFileName) then  // I3476
      raise EInstallInfo.Create('Installer '+FMSIFileName+' does not exist (code 3).  Setup cannot continue.');

    if not System.SysUtils.FileExists(ExtPath + FTitleImageFileName) then  // I3476
      FTitleImageFileName := '';

//    ReadSectionValues('Packages', FPackages);
//    ReadSectionValues('Strings', FStrings);

    for i := FPackages.Count - 1 downto 0 do
      if not System.SysUtils.FileExists(ExtPath + FPackages.Names[i]) then  // I3476
      begin
        ShowMessageW(Self.Text(ssPackageMissing, [FPackages.ValueFromIndex[i], FPackages.Names[i]]));
        FPackages.Delete(i);
      end;
  finally
    Free;
  end;
end;

function TInstallInfo.Text(const Name: TInstallInfoText;
  const Args: array of const): WideString;
begin
  Result := WideFormat(Text(Name), Args);
end;

{
////ExecuteApp insertion
      case MsgWaitForMultipleObjects(1, pi.hProcess, FALSE, INFINITE, QS_ALLINPUT) of
        WAIT_OBJECT_0 + 1:
          while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
          begin
            if not IsDialogMessage(MainWin, Msg) then
            begin
              TranslateMessage(Msg);
              DispatchMessage(Msg);
            end;
          end;
        WAIT_OBJECT_0, WAIT_ABANDONED_0:
          Waiting := False;
          //if GetExitCodeProcess(pi.hProcess, ec)
          //  then Waiting := ec <> STILL_ACTIVE
          //  else Waiting := False;
      end;
}

end.
