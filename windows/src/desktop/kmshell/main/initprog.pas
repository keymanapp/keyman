(*
  Name:             initprog
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    23 Jun 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework for Keyman 7
                    02 Aug 2006 - mcdurdin - Timeout when Beta expires
                    04 Dec 2006 - mcdurdin - Add -activate, locale functionality, icons
                    04 Jan 2007 - mcdurdin - Add help support
                    04 Jan 2007 - mcdurdin - Workaround Vista button refresh issue in Delphi
                    30 May 2007 - mcdurdin - Add -w to show welcome at any time
                    20 Jun 2007 - mcdurdin - I838 - Fix shadow keyboards (usually from KM6)
                    23 Aug 2007 - mcdurdin - I933 - Use Unicode application title
                    05 Nov 2007 - mcdurdin - I869 - Add -nowelcome for elevated keyboard install
                    05 Nov 2007 - mcdurdin - I937, I1128 - Repair COM object if it fails at startup
                    06 Nov 2007 - mcdurdin - I1140 - Support -configure-addin for elevated addin configuration
                    27 Mar 2008 - mcdurdin - I1201 - Admin required uninstalling package
                    27 Mar 2008 - mcdurdin - I1248 - Redesign Welcome
                    14 Jun 2008 - mcdurdin - Support installing multiple files at once
                    28 Aug 2008 - mcdurdin - I1616 - Upgrade keyboards from 6.x
                    16 Jan 2009 - mcdurdin - I1730 - Check update of keyboards
                    27 Jan 2009 - mcdurdin - Improve startup and shutdown performance
                    30 Jan 2009 - mcdurdin - I1826 - Fix performance starting and closing help
                    07 Sep 2009 - mcdurdin - I2051 - Crash in kmshell when -h parameter used with no page parameter
                    12 Mar 2010 - mcdurdin - I2226 - Include link to Text Editor in Keyman Menu
                    29 Mar 2010 - mcdurdin - I2260 - "Start Text Editor" should not start in Tutorial mode
                    25 May 2010 - mcdurdin - I1694 - UI language option accessibility
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    30 Nov 2010 - mcdurdin - I2548 - Support for upgrading Desktop 7 to Desktop 8
                    10 Dec 2010 - mcdurdin - I2548 - Consolidate -upgradekeyboards switch
                    17 Dec 2010 - mcdurdin - I2548 - Fix bugs with upgrade
                    30 Dec 2010 - mcdurdin - I2562 - Start Keyman install properties
                    30 Dec 2010 - mcdurdin - I2604 - Upgrade to Pro from Character Map is incomplete
                    31 Dec 2010 - mcdurdin - I2605 - Crash starting Keyman after upgrade when using non-default locale
                    18 Feb 2011 - mcdurdin - I2702 - "Getting Started" link not working
                    21 Feb 2011 - mcdurdin - I2651 - Setup does not set desired default options
                    22 Feb 2011 - mcdurdin - I2753 - Firstrun crashes because start with windows and auto update check options are set in Engine instead of Desktop
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman splash from showing multiple copies
                    18 Mar 2011 - mcdurdin - I2807 - Enable/disable addins
                    18 Mar 2011 - mcdurdin - I2782 - Unminimize configuration when bringing it to foreground
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    02 Feb 2012 - mcdurdin - I2975 - VistaAltFixUnit can crash on shutdown
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    01 Jan 2013 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    16 Jun 2014 - mcdurdin - I4185 - V9.0 - Upgrade V8.0 keyboards to 9.0
                    03 Sep 2014 - mcdurdin - I4400 - V9.0 - Add HKCU FEATURE_BROWSER_EMULATION 9000 for kmshell.exe
                    10 Oct 2014 - mcdurdin - I4436 - V9.0 - browser emulation control for kmshell breaks downlevel versions of Keyman
                    31 Dec 2014 - mcdurdin - I4553 - V9.0 - Upgrade to 476 or later requires recompile of all mnemonic layouts
                    23 Jun 2015 - mcdurdin - I4773 - Keyman needs to rebuild its language profiles if they are inadvertently deleted
*)
unit initprog;  // I3306   // I4248

interface

uses
  System.UITypes,
  Windows, Controls, SysUtils, Classes, ErrorControlledRegistry, Forms, MessageIdentifiers, MessageIdentifierConsts, keymanapi_TLB;

procedure Run;
procedure Main(Owner: TComponent = nil);


type
    TKMShellMode = (fmUndefined, fmInstall, fmView, fmUninstall, fmAbout,
                    fmUninstallKeyboard,
                    fmInstallTipsForPackages, fmInstallKeyboardLanguage, fmRegisterTip, fmInstallTip, fmUninstallKeyboardLanguage,   // I3624
                    fmUninstallPackage, fmRegistryAdd, fmRegistryRemove,
                    fmMain, fmHelp, fmHelpKMShell,
                    fmMigrate, fmSplash, fmStart,
                    fmUpgradeKeyboards, // I2548
                    fmTextEditor,
                    fmInstallKeyboardPackageAdmin,
                    fmBackgroundUpdateCheck,
                    fmBackgroundDownload,
                    fmApplyInstallNow,
                    fmFirstRun, // I2562
                    fmKeyboardWelcome,  // I2569
                    fmKeyboardPrint,  // I2329
                    fmBaseKeyboard,   // I4169
                    fmUpgradeMnemonicLayout,    // I4553
                    fmRepair,
                    fmKeepInTouch,
                    fmSettings,

                    // Commands from Keyman Engine
                    fmShowHint,
                    fmShowHelp);   // I4773

var
  ApplicationRunning: Boolean = False;
  UnRegCRC: LongWord = 0;
  FMode: TKMShellMode;

implementation

uses
  custinterfaces,
  DebugPaths,
  Dialogs,
  GetOsVersion,
  help,
  HTMLHelpViewer,
  Keyman.Configuration.UI.InstallFile,
  Keyman.Configuration.System.TIPMaintenance,
  Keyman.Configuration.System.UImportOlderVersionKeyboards11To13,
  Keyman.Configuration.UI.UfrmSettingsManager,
  Keyman.System.KeymanStartTask,
  KeymanPaths,
  KLog,
  kmint,
  KMShellHints,
  KeymanMutex,
  Keyman.System.RemoteUpdateCheck,
  RegistryKeys,
  UfrmBaseKeyboard,
  UfrmKeymanBase,
  UfrmInstallKeyboardLanguage,
  UfrmSplash,
  UfrmHelp,
  UfrmHTML,
  UfrmKeepInTouch,
  UfrmMain,
  UfrmPrintOSK,
  UfrmTextEditor,
  UfrmWebContainer,
  UImportOlderVersionSettings,
  UImportOlderVersionKeyboards8,
  UImportOlderVersionKeyboards9,
  UImportOlderVersionKeyboards10,
  UILanguages,
  uninstall,
  UpgradeMnemonicLayout,
  utilfocusappwnd,
  utilkmshell,
  Keyman.System.UpdateStateMachine,

  KeyboardTIPCheck,

  ValidateKeymanInstalledSystemKeyboards,

  VisualKeyboard,

  WideStrings,

  extctrls, stdctrls, comctrls;

function FirstRun(FQuery, FDisablePackages, FDefaultUILanguage: string): Boolean; forward;  // I2562
procedure ShowKeyboardWelcome(PackageName: WideString); forward;  // I2569
procedure PrintKeyboard(KeyboardName: WideString); forward;  // I2329

procedure Main(Owner: TComponent = nil);
var
  frmMain: TfrmMain;
begin
  if not Assigned(Owner) then
  begin
    UfrmWebContainer.CreateForm(TfrmMain, frmMain);
    ApplicationRunning := True;
    Application.Run;
    ApplicationRunning := False;
    FreeAndNil(frmMain);
  end
  else if not FocusConfiguration then  // I2720
  begin
    with TfrmMain.Create(Owner) do
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

function Show_frmHTML(AParent: TComponent; const ACaption, AText, AFileName: string; AHelpContext: Integer): Integer;
begin
  with TfrmHTML.Create(AParent) do
  try
    Caption := ACaption;
    HelpContext := AHelpContext;
    if AText <> ''
      then Text := AText
      else ShowFile(AFileName);
    Result := ShowModal;
  finally
    Free;
  end;
end;

function Init(var FMode: TKMShellMode; KeyboardFileNames: TStrings; var FSilent, FForce, FNoWelcome: Boolean;
  var FLogFile, FQuery: string; var FDisablePackages, FDefaultUILanguage: string; var FStartWithConfiguration: Boolean;
  var FParentWindow: THandle; var FDefaultBCP47: string; var FDefaultLangID: Integer): Boolean;
var
  s: string;
  i: Integer;
begin
  Result := False;
  FSilent := False;
  FForce := False;
  FNoWelcome := False;
  FStartWithConfiguration := False;
  FDisablePackages := '';
  FDefaultUILanguage := '';
  FQuery := '';
  FMode := fmStart;
  KeyboardFileNames.Clear;

  i := 1;
  while i <= ParamCount do
  begin
    if (ParamStr(i) <> '') and (ParamStr(i)[1] = '-') then
    begin
      s := LowerCase(ParamStr(i));
      if s = '-s' then FSilent := True
      else if s.StartsWith('-unique-') then
        // Ignore this parameter. Used just to avoid Start Menu hiding
        // shortcut entries which are pointing at the same target for
        // package installation. See https://superuser.com/a/1395288/521575 and
        // https://github.com/keymanapp/keyman/issues/2467
      else if s = '-f' then FForce := True
      else if s = '-c' then   FMode := fmMain
      else if s = '-m' then   FMode := fmMigrate
      else if s = '-i' then   FMode := fmInstall
      else if s = '-ikl' then FMode := fmInstallKeyboardLanguage   // I3624

      else if s = '-register-tip' then FMode := fmRegisterTip
      else if s = '-install-tip' then FMode := fmInstallTip
      else if s = '-install-tips-for-packages' then FMode := fmInstallTipsForPackages

      //else if s = '-i+' then  begin FMode := fmInstall; FNoWelcome := True; end;
      else if s = '-v' then   FMode := fmView
      else if s = '-u' then   FMode := fmUninstall
      else if s = '-uk' then  FMode := fmUninstallKeyboard     { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
      else if s = '-ukl' then FMode := fmUninstallKeyboardLanguage   // I3624
      else if s = '-up' then  FMode := fmUninstallPackage          { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
      else if s = '-ikp' then  FMode := fmInstallKeyboardPackageAdmin
      else if s = '-a' then   FMode := fmAbout
      else if s = '-ra' then  FMode := fmRegistryAdd
      else if s = '-rr' then  FMode := fmRegistryRemove
      else if s = '-splash'   then FMode := fmSplash
      else if s = '-?'   then FMode := fmHelpKMShell
      else if s = '-h'   then FMode := fmHelp
      else if s = '-t'   then FMode := fmTextEditor
      else if s = '-buc' then FMode := fmBackgroundUpdateCheck
      else if s = '-bd' then FMode := fmBackgroundDownload
      else if s = '-an' then FMode := fmApplyInstallNow
      else if s = '-basekeyboard' then FMode := fmBaseKeyboard   // I4169
      else if s = '-nowelcome'   then FNoWelcome := True
      else if s = '-kw' then FMode := fmKeyboardWelcome  // I2569
      else if s = '-kp' then FMode := fmKeyboardPrint  // I2329
      else if s = '-log' then begin Inc(i); FLogFile := ParamStr(i); end
      else if Copy(s,1,Length('-firstrun')) = '-firstrun' then begin FMode := fmFirstRun; FQuery := Copy(s,Length('-firstrun')+2,MAXINT); end  // I2562
      else if Copy(s,1,Length('-upgradekeyboards')) = '-upgradekeyboards' then begin FMode := fmUpgradeKeyboards; FQuery := Copy(s,Length('-upgradekeyboards')+2,MAXINT); end // I2548
      else if s = '-upgrademnemoniclayout' then FMode := fmUpgradeMnemonicLayout   // I4553
      else if s = '-repair' then FMode := fmRepair   // I4773
      else if s = '-keepintouch' then FMode := fmKeepInTouch
      else if Copy(s,1,Length('-disablepackages')) = '-disablepackages' then begin FDisablePackages := Copy(s, Length('-disablepackages')+2, MaxInt); end // Used with -firstrun
      else if Copy(s,1,Length('-defaultuilanguage')) = '-defaultuilanguage' then begin FDefaultUILanguage := Copy(s, Length('-defaultuilanguage')+2, MaxInt); end // Used with -firstrun

      else if s = '-startwithconfiguration' then FStartWithConfiguration := True
      else if s = '-q'   then
      begin
        FQuery := ''; Inc(i);
        while i <= ParamCount do begin FQuery := FQuery + ParamStr(i) + ' '; Inc(i); end;
        FQuery := Trim(FQuery);
      end
      else if Copy(s, 1, 3) = '-ur' then UnRegCRC := StrToIntDef('$'+Copy(s, 4, 8), 0)
      else if s = '-default-lang' then
      begin
        Inc(i);
        FDefaultBCP47 := ParamStr(i);
        Inc(i);
        FDefaultLangID := StrToIntDef('$'+ParamStr(i), 0);
      end
      // Controls from Keyman Engine
      else if s = '-showhint' then FMode := fmShowHint
      else if s = '-parentwindow' then
      begin
        Inc(i);
        FParentWindow := StrToIntDef(ParamStr(i), 0);
      end
      else if s = '-showhelp' then FMode := fmShowHelp
      else if s = '-settings' then FMode := fmSettings

      else Exit;
    end
    else
    begin
      KeyboardFileNames.Add(ParamStr(i));
    end;
    Inc(i);
  end;

  if FMode in [fmInstall, fmInstallKeyboardLanguage, fmInstallTipsForPackages, fmInstallTip, fmRegisterTip, fmView, fmUninstallKeyboard, fmUninstallKeyboardLanguage, fmUninstallPackage] then  // I2807   // I3624
    if KeyboardFileNames.Count = 0 then Exit;

  Result := FMode <> fmUndefined;
end;

procedure RegisterControlClasses;
begin
  RegisterClasses([TImage, TCheckBox, TLabel, TButton, TPanel, TGroupBox, TPageControl, TTabSheet]);
end;

procedure RunKMCOM(FMode: TKMShellMode; KeyboardFileNames: TStrings; FSilent, FForce, FNoWelcome: Boolean;
  FLogFile, FQuery: string; FDisablePackages, FDefaultUILanguage: string; FStartWithConfiguration: Boolean; FParentWindow: THandle;
  const FDefaultBCP47: string; FDefaultLangID: Integer); forward;

procedure Run;
var
  KeyboardFileNames: TStrings;
  FQuery: string;
  FSilent: Boolean;
  FNoWelcome: Boolean;
  FForce: Boolean;
  FParentWindow: THandle;
  FLogFile: string;
  FDefaultLangID: Integer;
  FDefaultBCP47, FDisablePackages, FDefaultUILanguage: string;
  FStartWithConfiguration: Boolean;
begin
  RegisterControlClasses;

  KeyboardFileNames := TStringList.Create;
  try
    FParentWindow := 0;
    if not Init(FMode, KeyboardFileNames, FSilent, FForce, FNoWelcome, FLogFile, FQuery, FDisablePackages, FDefaultUILanguage, FStartWithConfiguration, FParentWindow, FDefaultBCP47, FDefaultLangID) then
    begin
  //TODO:   TUtilExecute.Shell(PChar('hh.exe mk:@MSITStore:'+ExtractFilePath(KMShellExe)+'keyman.chm::/context/keyman_usage.html'), SW_SHOWNORMAL);
      Exit;
    end;

    if not LoadKMCOM then Exit;
    try
      RunKMCOM(FMode, KeyboardFileNames, FSilent, FForce, FNoWelcome, FLogFile, FQuery, FDisablePackages, FDefaultUILanguage, FStartWithConfiguration, FParentWindow, FDefaultBCP47, FDefaultLangID);
    finally
      kmcom := nil;
    end;
  finally
    KeyboardFileNames.Free;
  end;
end;

function DoCheckTIPInstallStatus(FSilent: Boolean): Boolean;
begin
(*
  Note: this needs a significant refactor because enumerating input processor
        profiles under Administrator account may return different results to an
        end user account. It is also much less likely that a keyboard will not
        be available with Keyman 14's keyboard registration model. So we may be
        able to avoid using this altogether.

        For now, we will disable this check here and in the future may re-enable
        it and rewrite it if it proves necessary to improve profile stability.

        See #4893.
  if not TKeyboardTIPCheck.CheckKeyboardTIPInstallStatus then
  begin
    if FSilent then
      Exit(False);

    if MessageDlg('Some keyboard profiles have been damaged. Correct these now?', mtConfirmation, mbOkCancel, 0) = mrOk then
    begin
      WaitForElevatedConfiguration(0, '-repair', True);
      // TKeyboardTIPCheck.ReEnablePostRepair; //possible future mitigation idea, see #4893.
    end;
  end;
*)
  Result := True;
end;

procedure RunKMCOM(FMode: TKMShellMode; KeyboardFileNames: TStrings; FSilent, FForce, FNoWelcome: Boolean;
  FLogFile, FQuery: string; FDisablePackages, FDefaultUILanguage: string; FStartWithConfiguration: Boolean;
  FParentWindow: THandle; const FDefaultBCP47: string; FDefaultLangID: Integer);
var
  kdl: IKeymanDefaultLanguage;
  FIcon: string;
  FMutex: TKeymanMutex;  // I2720
  BUpdateSM : TUpdateStateMachine;
    function FirstKeyboardFileName: WideString;
    begin
      if KeyboardFileNames.Count = 0
        then Result := ''
        else Result := KeyboardFileNames[0];
    end;

    function SecondKeyboardFileName: WideString;   // I3624
    begin
      if KeyboardFileNames.Count < 2
        then Result := ''
        else Result := KeyboardFileNames[1];
    end;

    function ThirdKeyboardFileName: string;
    begin
      if KeyboardFileNames.Count < 3
        then Result := ''
        else Result := KeyboardFileNames[2];
    end;

    function FourthKeyboardFileName: string;
    begin
      if KeyboardFileNames.Count < 4
        then Result := ''
        else Result := KeyboardFileNames[3];
    end;
begin
  kmcom.AutoApply := False;

  FMutex := nil;  // I2720

  { Run app reconfiguration tasks }

  TKeymanStartTask.RecreateTask;

  UILanguages.CreateUILanguages;

  Application.HelpFile := GetCHMPath; // may change if language changes
  Application.Title := MsgFromId(SKApplicationTitle);

  if GetOS in [osOther] then
  begin
    ShowMessage(MsgFromId(SKOSNotSupported));
    Exit;
  end;

  BUpdateSM := TUpdateStateMachine.Create(False);
    try
      if (FMode = fmBackgroundUpdateCheck) then
      begin
        BUpdateSM.HandleCheck;
        Exit;
      end
      else if (FMode = fmBackgroundDownload) then
      begin
        BUpdateSM.HandleDownload;
        Exit;
      end
      else if (FMode = fmApplyInstallNow) then
      begin
        BUpdateSM.HandleInstallNow;
        Exit;
      end
      else if (FMode = fmInstallKeyboardPackageAdmin) then
      begin
        BUpdateSM.HandleInstallPackages;
        Exit;
      end
      else
      begin
        if BUpdateSM.HandleKmShell = 1 then
          Exit;
      end;
    finally
      BUpdateSM.Free;
    end;


  if not FSilent or (FMode = fmUpgradeMnemonicLayout) then   // I4553
  begin
    // Note: will elevate and re-run if required
    // Upgrades mnemonic layouts to fix bugs in existing layouts
    TUpgradeMnemonicLayout.Run;
    if FMode = fmUpgradeMnemonicLayout then
      Exit;
  end;

  if not FSilent and (FMode in [fmStart, fmSplash, fmMain, fmAbout]) then   // I4773
  begin
    DoCheckTIPInstallStatus(False);
  end;

  // I1818 - remove start mode change

  if FMode = fmStart then FIcon := 'appicon.ico'
  else FIcon := 'cfgicon.ico';

  FIcon := GetDebugPath(FIcon, ExtractFilePath(ParamStr(0)) + FIcon, False);
  if FileExists(FIcon) then
    Application.Icon.LoadFromFile(FIcon);

  if (FDefaultBCP47 <> '') or (FDefaultLangID <> 0) then
  begin
    if Supports(kmcom, IKeymanDefaultLanguage, kdl) then
      kdl.SetDefaultLanguage(FDefaultBCP47, FDefaultLangID);
  end;

  case FMode of
    fmKeyboardWelcome:  // I2569
      ShowKeyboardWelcome(FirstKeyboardFileName);

    fmKeyboardPrint:  // I2329
      PrintKeyboard(FirstKeyboardFileName);

    fmFirstRun:  // I2562
      if FirstRun(FQuery, FDisablePackages, FDefaultUILanguage)
        then ExitCode := 0
        else ExitCode := 2;

    fmUpgradeKeyboards:// I2548
      begin
        if FQuery='13,backup' then
          TImportOlderVersionKeyboards11To13.BackupCurrentUser
        else if FQuery='13,import' then
          TImportOlderVersionKeyboards11To13.ImportCurrentUser
        else if FQuery='13,admin' then
          TImportOlderVersionKeyboards11To13.Execute
        else
        begin
          ImportOlderVersionKeyboards8(Pos('admin', FQuery) > 0);   // I4185
          ImportOlderVersionKeyboards9(Pos('admin', FQuery) > 0);   // I4185
          ImportOlderVersionKeyboards10(Pos('admin', FQuery) > 0);   // I4185
          DeleteLegacyKeymanInstalledSystemKeyboards;   // I3613
        end;
      end;

    fmMigrate:
      ;
    fmHelp:
      if KeyboardFileNames.Count > 0 then OpenHelp(KeyboardFileNames[0])
      else OpenHelp('');
    fmHelpKMShell: ;

////TODO:          TUtilExecute.Shell(PChar('hh.exe mk:@MSITStore:'+ExtractFilePath(KMShellExe)+'keyman.chm::/context/keyman_usage.html'), SW_SHOWNORMAL);

    fmStart:
      begin  // I2720
        StartKeyman(False, FSilent, FStartWithConfiguration);
      end;
    fmSplash:
      ShowSplash;

    fmMain, fmAbout:
      begin  // I2720
        FMutex := TKeymanMutex.Create('KeymanConfiguration');
        if FMutex.MutexOwned then Main else FocusConfiguration;
      end;
    fmTextEditor:
      begin  // I2720
        FMutex := TKeymanMutex.Create('KeymanTextEditor');
        if FMutex.MutexOwned then OpenTextEditor else FocusTextEditor;
      end;

    fmBaseKeyboard:   // I4169
      if ConfigureBaseKeyboard
        then ExitCode := 0
        else ExitCode := 1;

    fmInstall:
      if TInstallFile.Execute(KeyboardFileNames, FirstKeyboardFileName, FSilent, FNoWelcome, FLogFile)
        then ExitCode := 0
        else ExitCode := 1;

    fmInstallTipsForPackages:
      if TTipMaintenance.InstallTipsForPackages(KeyboardFileNames) then
      begin
        // TTIPMaintenance never does a kmcom.Apply to notify
        kmcom.Apply;
        ExitCode := 0;
      end
      else
        ExitCode := 1;

    fmUninstallKeyboard:            { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
      if UninstallKeyboard(nil, FirstKeyboardFileName, FSilent)
        then ExitCode := 0
        else ExitCode := 1;

    fmInstallKeyboardLanguage:   // I3624
      if InstallKeyboardLanguage(nil, FirstKeyboardFileName, SecondKeyboardFileName, FSilent)
        then ExitCode := 0
        else ExitCode := 1;

    fmUninstallKeyboardLanguage:   // I3624
      if UninstallKeyboardLanguage(FirstKeyboardFileName, FSilent)
        then ExitCode := 0
        else ExitCode := 1;

    fmRegisterTip:
      if TTipMaintenance.RegisterTip(StrToIntDef('$'+FirstKeyboardFilename, 0), SecondKeyboardFileName, ThirdKeyboardFileName)
        then ExitCode := 0
        else ExitCode := 1;

    fmInstallTip:
      if TTipMaintenance.InstallTip(StrToIntDef('$'+FirstKeyboardFilename, 0), SecondKeyboardFileName, ThirdKeyboardFileName, FourthKeyboardFileName)
        then ExitCode := 0
        else ExitCode := 1;

    fmUninstallPackage:             { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
      if UninstallPackage(nil, FirstKeyboardFileName, FSilent)
        then ExitCode := 0
        else ExitCode := 1;

    fmRepair:   // I4773
      if DoCheckTIPInstallStatus(FSilent)
        then ExitCode := 0
        else ExitCode := 1;

    fmKeepInTouch:
      ShowKeepInTouchForm(True);   // I4658

    fmSettings:
      if TfrmSettingsManager.Execute
        then ExitCode := 0
        else ExitCode := 1;

    fmShowHint:
      if ShowKMShellHintQuery(FParentWindow, FirstKeyboardFileName) = mrOk
        then ExitCode := 0
        else ExitCode := 1;
    fmShowHelp:
      TfrmHelp.Execute(FirstKeyboardFileName, SecondKeyboardFileName);
  end;

  if FMode <> fmMain then
  begin
    ApplicationRunning := True;
    Application.Run;
    ApplicationRunning := False;
  end;

  FreeAndNil(FMutex);  // I2720
end;

function FirstRun(FQuery, FDisablePackages, FDefaultUILanguage: string): Boolean; // I2562
var
  DoAdmin: Boolean;
begin
  Result := True;
  DoAdmin := kmcom.SystemInfo.IsAdministrator;

  if not DoAdmin then
  begin
    // I2651 - options not matching, case sensitivity, 8.0.309.0
    Result := FirstRunInstallDefaults(
      Pos('installdefaults', FQuery) > 0,
      Pos('startwithwindows', FQuery) > 0,
      Pos('checkforupdates', FQuery) > 0,
      Pos('automaticupdates', FQuery) > 0,
      FDisablePackages,
      FDefaultUILanguage,
      Pos('automaticallyreportusage', FQuery) > 0);  // I2651, I2753
  end;
end;

procedure ShowKeyboardWelcome(PackageName: WideString);  // I2569
var
  n: Integer;
begin
  n := kmcom.Packages.IndexOf(PackageName);
  if n >= 0 then
    DoShowPackageWelcome(nil, kmcom.Packages[n], True);
end;

procedure PrintKeyboard(KeyboardName: WideString);  // I2329
var
  n: Integer;
begin
  n := kmcom.Keyboards.IndexOf(KeyboardName);
  if n >= 0 then
    with TfrmPrintOSK.Create(nil) do
    try
      PrintKeyboard(kmcom.Keyboards[n]);
    finally
      Free;
    end;
end;

end.

