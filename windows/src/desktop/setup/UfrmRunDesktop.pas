(*
  Name:             UfrmRunDesktop
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      30 Dec 2010

  Modified Date:    23 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          30 Dec 2010 - mcdurdin - I2562 - Split Keyman install dialog into separate unit
                    31 Dec 2010 - mcdurdin - I2610 - Show message if not silent and not starting Desktop after successful install
                    31 Dec 2010 - mcdurdin - I2617 - Fix title of Setup
                    31 Dec 2010 - mcdurdin - I2607 - Start with Windows not on by default
                    31 Dec 2010 - mcdurdin - I2609 - Check for Updates not on by default
                    31 Dec 2010 - mcdurdin - I2608 - Start after install not on by default
                    31 Dec 2010 - mcdurdin - I2606 - License button not working
                    14 Jan 2011 - mcdurdin - I2645 - Refactor install process
                    14 Jan 2011 - mcdurdin - I2644 - MSI installer should be silent
                    14 Jan 2011 - mcdurdin - I2642 - Installer uninstalls KM7 keyboards before upgrade can happen
                    14 Jan 2011 - mcdurdin - I2647 - Cannot access options or license with keyboard
                    17 Jan 2011 - mcdurdin - I2644 - Make installer fully silent, again
                    22 Feb 2011 - mcdurdin - I2651 - Install does not set desired default options
                    22 Feb 2011 - mcdurdin - I2738 - Auto update does not start automatically
                    22 Feb 2011 - mcdurdin - I2748 - Install backs up user registry for Admin user, not shell user
                    22 Feb 2011 - mcdurdin - Audit all uses of TRegistry, TTntRegistry and replace with TShellUserRegistry where appropriate
                    31 Mar 2011 - mcdurdin - I2847 - Version 8.0 silent upgrade runs keyboard import from version 7.0 and shouldn't
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    19 Oct 2012 - mcdurdin - I3476 - V9.0 - Fixup additional hints and warnings around string conversion
                    15 Jun 2012 - mcdurdin - I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    03 Nov 2012 - mcdurdin - I3500 - V9.0 - Merge of I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    28 Feb 2014 - mcdurdin - I4099 - V9.0 - Keyman Setup dialog is still 8.0 style
                    24 Jun 2014 - mcdurdin - I4293 - V9.0 - Setup bootstrapper does not check for V8 upgrade
*)
unit UfrmRunDesktop;  // I3306   // I4099

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Variants,
  Vcl.AppEvnts,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  Vcl.StdCtrls,

  Keyman.Setup.System.InstallInfo,
  RunTools,
  UfrmDownloadProgress;

type
  TfrmRunDesktop = class(TForm)
    imgTitle: TImage;
    panContent: TPanel;
    lblLicense: TLabel;
    lblFree: TLabel;
    panBottom: TPanel;
    lblOptions: TLabel;
    progress: TProgressBar;
    lblStatus: TLabel;
    panAction: TPanel;
    cmdInstall: TButton;
    cmdExit: TButton;
    panText: TPanel;
    lblActions: TLabel;
    cbLanguage: TComboBox;
    lblGlobe: TLabel;
    appevents: TApplicationEvents;
    sbActionText: TScrollBox;
    panActionText: TPanel;
    panInnerContent: TPanel;
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure lblOptionsClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmdInstallClick(Sender: TObject);
    procedure lblLicenseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbLanguageClick(Sender: TObject);
    procedure appeventsMessage(var Msg: tagMSG; var Handled: Boolean);
  private
    g_iCurPos, g_iProgress, g_iProgressTotal: Integer;
    g_bCancelInstall, g_bScriptInProgress, g_bForwardProgress, g_bFirstTime, g_bEnableActionData: Boolean;
    StatusMax: Integer;
    FContinueSetup: Boolean;
    FRunUpgrade6: Boolean;
    FRunUpgrade7: Boolean;
    FRunUpgrade8: Boolean;   // I4293
    FRunUpgrade9: Boolean;
    FRunUpgrade10: Boolean;
    FCanUpgrade6: Boolean;
    FCanUpgrade7: Boolean;
    FCanUpgrade8: Boolean;   // I4293
    FCanUpgrade9: Boolean;
    FCanUpgrade10: Boolean;
    FCheckForUpdates: Boolean;

    FStartAfterInstall: Boolean;
    FStartWithWindows: Boolean;
    FAutomaticallyReportUsage: Boolean;
    FDisableUpgradeFrom6Or7Or8: Boolean;  // I2847   // I4293
    FInstallDefaults: Boolean;
    procedure CheckVersion6Upgrade;
    procedure CheckVersion7Upgrade;
    procedure CheckVersion8Upgrade;   // I4293
    procedure CheckVersion9Upgrade;
    procedure CheckVersion10Upgrade;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER;
    procedure Status(const Text: WideString = '');
    procedure SetupMSI;  // I2644
    procedure BackupKey(Root: HKEY; Path: WideString);  // I2642
    procedure GetDefaultSettings;
    procedure FillActionText;
    procedure FillLanguageList;
    procedure FillStrings;
    { Private declarations }
  public
    procedure DoInstall(Silent, PromptForReboot: Boolean);  // I3355   // I3500
    property ContinueSetup: Boolean read FContinueSetup write FContinueSetup;
    property StartAfterInstall: Boolean read FStartAfterInstall write FStartAfterInstall;  // I2738
    property DisableUpgradeFrom6Or7Or8: Boolean read FDisableUpgradeFrom6Or7Or8 write FDisableUpgradeFrom6Or7Or8; // I2847   // I4293
    property InstallDefaults: Boolean read FInstallDefaults write FInstallDefaults;
  end;

implementation

uses
  System.Generics.Collections,
  System.Win.Registry,
  jwamsi,

  bootstrapmain,
  SFX,
  SetupStrings,
  Keyman.Setup.System.SetupUILanguageManager,
  Keyman.System.MITLicense,
  Keyman.System.UpgradeRegistryKeys,
  KeymanVersion,
  RegistryHelpers,
  ErrorControlledRegistry,
  UfrmHTML,
  UfrmInstallOptions,
  versioninfo,
  RegistryKeys;

{$R *.dfm}

procedure TfrmRunDesktop.URLLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := [];
end;

procedure TfrmRunDesktop.cmdExitClick(Sender: TObject);
begin
  // TODO: we should use a boolean flag here rather than comparing caption
  if cmdExit.Caption = FInstallInfo.Text(ssCancelButton) then // I2644
  begin
    if MessageDlg(FInstallInfo.Text(ssCancelQuery), mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
    g_bCancelInstall := True;
  end
  else
    ModalResult := mrCancel;
end;

procedure TfrmRunDesktop.cmdInstallClick(Sender: TObject);
begin
  DoInstall(False, True);  // I3355   // I3500
end;

function MsiUIHandler(pvContext: Pointer; iMessageType: UINT; szMessage: PWideChar): Integer; stdcall; // I2644

  function Token(var s: WideString; t: WideString): WideString;
  var
    n: Integer;
  begin
    n := Pos(t, s);
    if n = 0 then
    begin
      Result := s;
      s := '';
    end
    else
    begin
      Result := Copy(s, 1, n-1);
      Delete(s,1,n);
    end;
  end;

var
  pmField: array[0..3] of Integer;

  function ParseProgressString(sz: WideString): Boolean;
  var
    ch: WideChar;
    s: WideString;
  begin
    Result := False;
    if sz = '' then Exit;

    s := Token(sz, ' ');
    while s <> '' do
    begin
      ch := s[1];
      s := Token(sz, ' ');

      case ch of
        '1': // field 1
          // progress message type
          begin
            if not TryStrToInt(s, pmField[0]) then Exit;
          end;
        '2': // field 2
          begin
            if not TryStrToInt(s, pmField[1]) then Exit;
            if pmField[0] in [2,3] then
            begin
              Result := True; // done processing
              Exit;
            end;
          end;
        '3': // field 3
          begin
            if not TryStrToInt(s, pmField[2]) then Exit;
            if pmField[0] = 1 then
            begin
              Result := True;
              Exit;
            end;
          end;
        '4': // field 4
          begin
            if not TryStrToInt(s, pmField[3]) then Exit;
            Result := True;
            Exit;
          end;
        else // unknown field
          Exit;
      end;
      s := Token(sz, ' ');
    end;

    Result := True;
  end;

  function ParseCommonDataString(sz: WideString): Boolean;
  var
    ch: WideChar;
    s: WideString;
  begin
    Result := False;
    if sz = '' then Exit;

    s := Token(sz, ' ');
    while s <> '' do
    begin
      ch := s[1];
      s := Token(sz, ' ');

      case ch of
        '1': // field 1
        begin
          if not TryStrToInt(s, pmField[0]) then Exit;
          if pmField[0] = 1 then Exit;
        end;

        '2': // field 2
          // because we are ignoring caption msg, these are all ints
          if not TryStrToInt(s, pmField[1]) then Exit
          else
          begin
            Result := True;
            Exit;
          end;
        else Exit; // unknown field
      end;

      s := Token(sz, ' ');
    end;
  end;

var
  mt: INSTALLMESSAGE;
  uiFlags: UINT;
  wnd: TfrmRunDesktop;
begin
  Result := 0;

  wnd := TfrmRunDesktop(pvContext);

	if wnd.g_bFirstTime then
	begin
		MsiSetInternalUI(INSTALLUILEVEL_BASIC, nil);
		wnd.g_bFirstTime := False;
	end;

  if not Assigned(szMessage) then Exit;

  mt := INSTALLMESSAGE($FF000000 and iMessageType);
  uiFlags := $FFFFFF and iMessageType;

  case mt of
    //Premature termination
    INSTALLMESSAGE_FATALEXIT:
      // Get fatal error message here and display it
      Result := MessageBoxW(0, szMessage, 'Fatal Error', uiFlags);

    INSTALLMESSAGE_ERROR:
      // Get error message here and display it
      // language and caption can be obtained from common data msg
      begin
        MessageBeep(uiFlags and MB_ICONMASK);
        Result := MessageBoxW(0, szMessage, 'Error', uiFlags);
      end;
    INSTALLMESSAGE_WARNING:
      // Get warning message here and display it
      Result := MessageBoxW(0, szMessage, 'Warning', uiFlags);

    INSTALLMESSAGE_USER:
      // Get user message here
      // parse uiFlags to get Message Box Styles Flag and return appopriate value, IDOK, IDYES, etc.
      Result := IDOK;

    INSTALLMESSAGE_INFO:
      Result := IDOK;

    INSTALLMESSAGE_FILESINUSE:
      // Display FilesInUse dialog
      // parse the message text to provide the names of the
      // applications that the user can close so that the
      // files are no longer in use.
      Result := 0;

    INSTALLMESSAGE_RESOLVESOURCE:
      // ALWAYS return 0 for ResolveSource
      Result := 0;

    INSTALLMESSAGE_OUTOFDISKSPACE:
      // Get user message here
      Result := IDOK;

    INSTALLMESSAGE_ACTIONSTART:
      Result := IDOK;

    INSTALLMESSAGE_ACTIONDATA:
      // only act if progress total has been initialized
      if 0 = wnd.g_iProgressTotal then Result := IDOK
      else
      begin
        wnd.lblStatus.Caption := szMessage;
        wnd.lblStatus.Update;
        if wnd.g_bEnableActionData then
          wnd.progress.StepIt;
        Result := IDOK;
      end;

    INSTALLMESSAGE_PROGRESS:
      begin
        Application.ProcessMessages;
        if ParseProgressString(szMessage) then
        begin
          case pmField[0] of
            0: // Reset progress bar
            begin
              //field 1 = 0, field 2 = total number of ticks, field 3 = direction, field 4 = in progress

              // get total number of ticks in progress bar
              wnd.g_iProgressTotal := pmField[1];

              // determine direction
              wnd.g_bForwardProgress := pmField[2] = 0;

              // get current position of progress bar, depends on direction
              // if Forward direction, current position is 0
              // if Backward direction, current position is Total # ticks
              if wnd.g_bForwardProgress
                then wnd.g_iProgress := 0
                else wnd.g_iProgress := wnd.g_iProgressTotal;
              wnd.progress.Max := wnd.g_iProgressTotal;

        			// if g_bScriptInProgress, finish progress bar, else reset (and set up according to direction)
              if wnd.g_bScriptInProgress
                then wnd.progress.Position := wnd.g_iProgressTotal
                else wnd.progress.Position := wnd.g_iProgress;

			        wnd.g_iCurPos := 0;

              // determine new state
              // if new state = 1 (script in progress), could send a "Please wait..." msg
              wnd.g_bScriptInProgress := (pmField[3] = 1);
            end;

            1:  // ActionInfo
            begin
              //field 1 = 1, field 2 will contain the number of ticks to increment the bar
              //ignore if field 3 is zero
              if pmField[2] = 1 then
              begin
                if wnd.g_bForwardProgress
                  then wnd.progress.Step := pmField[1]
                  else wnd.progress.Step := -pmField[1];
                wnd.g_bEnableActionData := True;
              end
              else
                wnd.g_bEnableActionData := False;
            end;

            2: //ProgressReport
            begin
              // only act if progress total has been initialized
              if 0 <> wnd.g_iProgressTotal then
              begin
                Inc(wnd.g_iCurPos, pmField[1]);

                //field 1 = 2,field 2 will contain the number of ticks the bar has moved
                // movement direction determined by g_bForwardProgress set by reset progress msg
                if wnd.g_bForwardProgress
                  then wnd.progress.Position := wnd.g_iCurPos
                  else wnd.progress.Position := -wnd.g_iCurPos;
              end;
            end;

            3: ; // ProgressAddition - fall through (we don't care to handle it -- total tick count adjustment)
          end;
        end;

        if wnd.g_bCancelInstall then
          Result := IDCANCEL
        else
          Result := IDOK;
      end;

    INSTALLMESSAGE_COMMONDATA:
      if ParseCommonDataString(szMessage) then
      begin
          // all fields off by 1 due to c array notation
          case pmField[0] of
            0: ; // field 1 = 0, field 2 = LANGID, field 3 = CodePage
            1: ; // field 1 = 1, field 2 = CAPTION // you could use this as the caption for MessageBoxes
            2:
              if pmField[1] = 0 then wnd.cmdExit.Visible := False else wnd.cmdExit.Visible := True;
          end;
        Result := IDOK;
      end;

    // this message is received prior to internal UI initialization, no string data
    INSTALLMESSAGE_INITIALIZE:
      Result := IDOK;

    // Sent after UI termination, no string data
    INSTALLMESSAGE_TERMINATE:
      Result := IDOK;

    //Sent prior to display of authored dialog or wizard
    INSTALLMESSAGE_SHOWDIALOG:
      Result := IDOK;

    else
      Result := 0;
  end;
end;


procedure TfrmRunDesktop.SetupMSI; // I2644
var
  h: THandle;
begin
  h := Handle;
  g_bFirstTime := True;
  MsiSetInternalUI(INSTALLUILEVEL_NONE, @h);  // I2644
  //if GetRunTools.Silent
    //then
    //else MsiSetInternalUI(INSTALLUILEVEL_NONE or INSTALLUILEVEL_SOURCERESONLY, @h);
  MsiSetExternalUIW(MSIUIHandler, INSTALLLOGMODE_PROGRESS, Self);
end;

procedure TfrmRunDesktop.DoInstall(Silent, PromptForReboot: Boolean);  // I3355   // I3500
begin
  if FDisableUpgradeFrom6Or7Or8 then // I2847   // I4293
  begin
    FRunUpgrade6 := False;
    FRunUpgrade7 := False;
    FRunUpgrade8 := False;   // I4293
    FRunUpgrade9 := False;
    FRunUpgrade10 := False;
  end;

  GetRunTools.PromptForReboot := PromptForReboot;  // I3355   // I3500
  GetRunTools.OnStatus := Status;

  //InstallPackages; Exit;
  SetCursor(LoadCursor(0, IDC_WAIT));
  try
    cmdExit.Caption := FInstallInfo.Text(ssCancelButton); // I2644
    cmdInstall.Enabled := False;
    lblOptions.Enabled := False;
    lblLicense.Enabled := False;
    g_bCancelInstall := False; // I2644

    GetRunTools.RunUpgrade6 := FRunUpgrade6;
    GetRunTools.RunUpgrade7 := FRunUpgrade7;
    GetRunTools.RunUpgrade8 := FRunUpgrade8;   // I4293
    GetRunTools.RunUpgrade9 := FRunUpgrade9;
    GetRunTools.RunUpgrade10 := FRunUpgrade10;

    SetupMSI; // I2644

    if GetRunTools.DoInstall(Handle, FStartAfterInstall, FStartWithWindows, FCheckForUpdates,
      FInstallInfo.StartDisabled, FInstallInfo.StartWithConfiguration, FInstallDefaults,
      FAutomaticallyReportUsage, FContinueSetup) then
    begin
      if not Silent and not FStartAfterInstall then   // I2610
        ShowMessage(FInstallInfo.Text(ssInstallSuccess));
      ModalResult := mrOk;
    end;
  finally
    cmdExit.Caption := FInstallInfo.Text(ssExitButton); // I2644
    cmdExit.Visible := True;
    cmdInstall.Enabled := True;
    lblOptions.Enabled := True;
    lblLicense.Enabled := True;

    SetCursor(LoadCursor(0, IDC_ARROW));
    Status();
  end;
end;

procedure TfrmRunDesktop.FormCreate(Sender: TObject);
var
  oldHeight: Integer;
begin
  FAutomaticallyReportUsage := True;

  FillStrings;

  if FInstallInfo.TitleImageFilename <> '' then
  begin
    oldHeight := imgTitle.Height;
    imgTitle.Picture.LoadFromFile(FInstallInfo.TitleImageFilename);
    ClientHeight := ClientHeight - oldHeight + imgTitle.Height;
  end;

  CheckVersion6Upgrade;
  CheckVersion7Upgrade;
  CheckVersion8Upgrade;   // I4293
  CheckVersion9Upgrade;
  CheckVersion10Upgrade;

  GetDefaultSettings;  // I2651

  FillLanguageList;
  FillStrings;
end;

procedure TfrmRunDesktop.FillStrings;
begin
  Application.Title := FInstallInfo.Text(ssApplicationTitle);
  Caption := FInstallInfo.Text(ssTitle);
  cmdInstall.Caption := FInstallInfo.Text(ssInstallButton);
  cmdExit.Caption := FInstallInfo.Text(ssExitButton);
  lblLicense.Caption := FInstallInfo.Text(ssLicenseLink);
  lblOptions.Caption := FInstallInfo.Text(ssInstallOptionsLink);
  lblFree.Caption := FInstallInfo.Text(ssFreeCaption);
  FillActionText;
end;

procedure TfrmRunDesktop.FillLanguageList;
var
  item: TPair<string,string>;
  n: Integer;
begin
  for item in TSetupUILanguageManager.Locales do
  begin
    n := cbLanguage.Items.Add(item.Value);
    if TSetupUILanguageManager.ActiveLocale = item.Key then
      cbLanguage.ItemIndex := n;
  end;
  cbLanguage.Visible := cbLanguage.Items.Count > 1;
  lblGlobe.Visible := cbLanguage.Visible;
end;

procedure TfrmRunDesktop.FillActionText;
var
  s: string;
  pack: TInstallInfoPackage;
  Found: Boolean;
  langname: string;
  packLocation: TInstallInfoPackageFileLocation;
  downloadSize: string;
begin
  Found := False;
  s := '';
  if FInstallInfo.ShouldInstallKeyman then
  begin
    if FInstallInfo.MsiInstallLocation.LocationType = iilOnline
      then downloadSize := FInstallInfo.Text(ssActionDownload, [FormatFileSize(FInstallInfo.MsiInstallLocation.Size)])
      else downloadSize := '';

    s := s + FInstallInfo.Text(ssActionInstallKeyman, [FInstallInfo.MsiInstallLocation.VersionWithTag, downloadSize]) + #13#10;

    Found := True;
  end;

  for pack in FInstallInfo.Packages do
  begin
    if pack.ShouldInstall then
    begin
      packLocation := pack.InstallLocation;
      if Assigned(packLocation) then
      begin
        if pack.BCP47 <> ''
          then langname := packLocation.GetLanguageNameFromBCP47(pack.BCP47)
          else langname := '';

        if packLocation.LocationType = iilOnline
          then downloadSize := FInstallInfo.Text(ssActionDownload, [FormatFileSize(packLocation.Size)])
          else downloadSize := '';

        if langname <> ''
          then s := s + FInstallInfo.Text(ssActionInstallPackageLanguage, [packLocation.Name.Trim, packLocation.Version.Trim, langname, downloadSize])
          else s := s + FInstallInfo.Text(ssActionInstallPackage, [packLocation.Name.Trim, packLocation.Version.Trim, downloadSize]);

        s := s + #13#10;

        Found := True;
      end;
    end;
  end;
  cmdInstall.Enabled := Found;

  if not Found then
    s := FInstallInfo.Text(ssActionNothingToInstall)
  else
    s := FInstallInfo.Text(ssActionInstall)+#13#10+s;

  lblActions.Caption := s.Trim;
  lblActions.Invalidate;
  panContent.Invalidate;
end;

procedure TfrmRunDesktop.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState); // I2647
begin
  if Key = Ord('R') then lblLicenseClick(Self)
  else if Key = Ord('O') then lblOptionsClick(Self)
  else Exit;
  Key := 0;
end;

procedure TfrmRunDesktop.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TfrmRunDesktop.lblLicenseClick(Sender: TObject);
begin
  with TfrmHTML.Create(Self) do  // I2606
  try
    ShowText(SKeymanMITLicense);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmRunDesktop.lblOptionsClick(Sender: TObject);
begin
  with TfrmInstallOptions.Create(Self) do
  try
    AutomaticallyReportUsage := FAutomaticallyReportUsage;
    StartWithWindows := FStartWithWindows;
    StartAfterInstall := FStartAfterInstall;
    CheckForUpdates := FCheckForUpdates;
    UpgradeKeyman := FRunUpgrade6 or FRunUpgrade7 or FRunUpgrade8 or FRunUpgrade9 or FRunUpgrade10;   // I4293
    CanUpgradeKeyman := FCanUpgrade6 or FCanUpgrade7 or FCanUpgrade8 or FCanUpgrade9 or FCanUpgrade10;   // I4293
    if ShowModal = mrOk then
    begin
      FStartWithWindows := StartWithWindows;
      FStartAfterInstall := StartAfterInstall;
      FCheckForUpdates := CheckForUpdates;
      FRunUpgrade6 := FCanUpgrade6 and UpgradeKeyman;
      FRunUpgrade7 := FCanUpgrade7 and UpgradeKeyman;
      FRunUpgrade8 := FCanUpgrade8 and UpgradeKeyman;
      FRunUpgrade9 := FCanUpgrade9 and UpgradeKeyman;   // I4293
      FRunUpgrade10 := FCanUpgrade10 and UpgradeKeyman;
      FillActionText;
    end;
  finally
    Free;
  end;
end;

procedure TfrmRunDesktop.URLLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Style := [fsUnderline];
end;

procedure TfrmRunDesktop.appeventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if (Msg.message = WM_SYSCOMMAND) and (Msg.wParam = SC_RESTORE) then
  begin
    // Handle the case where Win+M pressed, window never restores
    // Only really happens with Splash.
    PostMessage(Handle, WM_SHOWWINDOW, 1, SW_PARENTOPENING);
  end;
end;

procedure TfrmRunDesktop.BackupKey(Root: HKEY; Path: WideString); // I2642
var
  regRead: TRegistryErrorControlled;  // I2890
  regWrite: TRegistryErrorControlled;  // I2890
  FBackupPath: string;

    procedure CopyKey(p: WideString);  // I2890
    var
      s: TStringList;
      i: Integer;
      sz: Integer;
      m: Pointer;
    begin
      s := TStringList.Create;
      try
        if regRead.OpenKeyReadOnly('\'+p) and regWrite.OpenKey(FBackupPath + p, True) then
        begin
          regRead.GetValueNames(s);
          for i := 0 to s.Count - 1 do
          begin
            case regRead.GetDataType(s[i]) of
              rdString: regWrite.WriteString(s[i], regRead.ReadString(s[i]));
              rdExpandString: regWrite.WriteExpandString(s[i], regRead.ReadString(s[i]));
              rdInteger: regWrite.WriteInteger(s[i], regRead.ReadInteger(s[i]));
              rdBinary:
                begin
                  sz := regRead.GetDataSize(s[i]);
                  m := AllocMem(sz);
                  regRead.ReadBinaryData(s[i], m^, sz);
                  regWrite.WriteBinaryData(s[i], m^, sz);
                  FreeMem(m);
                end;
            end;
          end;
          regRead.GetKeyNames(s);
          for i := 0 to s.Count - 1 do
            CopyKey(p + '\' + s[i]);
        end;
      finally
        s.Free;
      end;
    end;
begin
  if Root = HKEY_CURRENT_USER then  // I2749
  begin
    regRead := CreateHKCURegistry;
    regWrite := CreateHKCURegistry;
    FBackupPath := SRegKey_UpgradeBackupPath_CU;
  end
  else
  begin
    regRead := CreateHKLMRegistry;
    regWrite := CreateHKLMRegistry;
    FBackupPath := SRegKey_UpgradeBackupPath_LM;
  end;

  try
    CopyKey(Path);
  finally
    regRead.Free;
    regWrite.Free;
  end;
end;

procedure TfrmRunDesktop.CheckVersion6Upgrade;
var
  str: TStringList;
  n: Integer;
begin
  FCanUpgrade6 := False;
  FRunUpgrade6 := False;

  str := TStringList.Create;
  n := 0;
  with CreateHKCURegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_Keyman60_InstalledKeyboards_CU) then
    begin
      GetKeyNames(str);
      n := str.Count;
    end;
  finally
    Free;
  end;

  str.Clear;

  with CreateHKLMRegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_Keyman60_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      Inc(n, str.Count);
    end;
  finally
    Free;
    str.Free;
  end;

  if n > 0 then
  begin
    FRunUpgrade6 := True;
    FCanUpgrade6 := True;
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_Keyman60_InstalledKeyboards_LM); // I2642
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_Keyman60_InstalledPackages_LM);
    BackupKey(HKEY_CURRENT_USER, SRegKey_Keyman60_CU);  // I2749
  end;
end;

procedure TfrmRunDesktop.CheckVersion7Upgrade;
var
  str: TStringList;
  n: Integer;
begin
  FCanUpgrade7 := False;
  FRunUpgrade7 := False;

  str := TStringList.Create;
  n := 0;
  with CreateHKCURegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine70_InstalledKeyboards_CU) then
    begin
      GetKeyNames(str);
      n := str.Count;
    end;
  finally
    Free;
  end;

  str.Clear;

  with CreateHKLMRegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine70_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      Inc(n, str.Count);
    end;
  finally
    Free;
    str.Free;
  end;

  if n > 0 then
  begin
    FRunUpgrade7 := True;
    FCanUpgrade7 := True;
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine70_InstalledKeyboards_LM); // I2642
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine70_InstalledPackages_LM);
    BackupKey(HKEY_CURRENT_USER, SRegKey_KeymanEngine70_CU);
  end;
end;

procedure TfrmRunDesktop.CheckVersion8Upgrade;   // I4293
var
  str: TStringList;
  n: Integer;
begin
  FCanUpgrade8 := False;
  FRunUpgrade8 := False;

  str := TStringList.Create;
  n := 0;
  with CreateHKCURegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine80_InstalledKeyboards_CU) then
    begin
      GetKeyNames(str);
      n := str.Count;
    end;
  finally
    Free;
  end;

  str.Clear;

  with CreateHKLMRegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine80_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      Inc(n, str.Count);
    end;
  finally
    Free;
    str.Free;
  end;

  if n > 0 then
  begin
    FRunUpgrade8 := True;
    FCanUpgrade8 := True;
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine80_InstalledKeyboards_LM); // I2642
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine80_InstalledPackages_LM);
    BackupKey(HKEY_CURRENT_USER, SRegKey_KeymanEngine80_CU);
  end;
end;

procedure TfrmRunDesktop.CheckVersion9Upgrade;   // I4293
var
  str: TStringList;
  n: Integer;
begin
  FCanUpgrade9 := False;
  FRunUpgrade9 := False;

  str := TStringList.Create;
  n := 0;

  with CreateHKLMRegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine90_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      Inc(n, str.Count);
    end;
  finally
    Free;
    str.Free;
  end;

  if n > 0 then
  begin
    FRunUpgrade9 := True;
    FCanUpgrade9 := True;
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine90_InstalledKeyboards_LM); // I2642
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine90_InstalledPackages_LM);
    BackupKey(HKEY_CURRENT_USER, SRegKey_KeymanEngine90_CU);
  end;
end;

procedure TfrmRunDesktop.cbLanguageClick(Sender: TObject);
begin
  TSetupUILanguageManager.ActiveLocale := TSetupUILanguageManager.Locales.Keys.ToArray[cbLanguage.ItemIndex];
  FillStrings;
end;

procedure TfrmRunDesktop.CheckVersion10Upgrade;   // I4293
var
  str: TStringList;
  n: Integer;
begin
  FCanUpgrade10 := False;
  FRunUpgrade10 := False;

  str := TStringList.Create;
  n := 0;

  with CreateHKLMRegistry do  // I2749
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine100_InstalledKeyboards_LM) then
    begin
      GetKeyNames(str);
      Inc(n, str.Count);
    end;
  finally
    Free;
    str.Free;
  end;

  if n > 0 then
  begin
    FRunUpgrade10 := True;
    FCanUpgrade10 := True;
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine100_InstalledKeyboards_LM); // I2642
    BackupKey(HKEY_LOCAL_MACHINE, SRegKey_KeymanEngine100_InstalledPackages_LM);
    BackupKey(HKEY_CURRENT_USER, SRegKey_KeymanEngine100_CU);
  end;
end;


procedure TfrmRunDesktop.WMUserFormShown(var Message: TMessage);
begin
  if FContinueSetup then
    DoInstall(False, True);  // I3355   // I3500
end;

procedure TfrmRunDesktop.Status(const Text: WideString = '');
begin
  if Text = '' then
  begin
    progress.Visible := False;
    cbLanguage.Visible := cbLanguage.Items.Count > 1;
  end
  else
  begin
    cbLanguage.Visible := False;
    progress.Visible := True;
    if progress.Max <> StatusMax then
    begin
      progress.Position := 1;
      progress.Step := 1;
      progress.Max := StatusMax;
    end
    else
      progress.StepIt;
  end;
  lblGlobe.Visible := cbLanguage.Visible;

  lblStatus.Caption := Text;
  lblStatus.Update;
  Update;
end;

procedure TfrmRunDesktop.GetDefaultSettings; // I2651
begin
  FStartWithWindows := True; // I2607
  FCheckForUpdates := True;  // I2609

  try
    with CreateHKCURegistry do  // I2749
    try
      if OpenKeyReadOnly(SRegKey_KeymanDesktop_CU) then
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := ValueExists(SRegValue_UpgradeRunKeyman) or
          (OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists(SRegValue_WindowsRun_Keyman));

      end
      else if FCanUpgrade10 and OpenKeyReadOnly(SRegKey_KeymanEngine100_ProductOptions_Desktop_CU) then   // I4293
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('desktop_pro.pxx');
      end
      else if FCanUpgrade9 and OpenKeyReadOnly(SRegKey_KeymanEngine90_ProductOptions_Desktop_Pro_CU) then   // I4293
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('desktop_pro.pxx');
      end
      else if FCanUpgrade8 and OpenKeyReadOnly(SRegKey_KeymanEngine80_ProductOptions_Desktop_Pro_CU) then   // I4293
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('desktop_pro.pxx');
      end
      else if FCanUpgrade7 and OpenKeyReadOnly(SRegKey_KeymanEngine70_ProductOptions_Desktop_Pro_CU) then
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('desktop_pro.pxx');
      end
      else if FCanUpgrade7 and OpenKeyReadOnly(SRegKey_KeymanEngine70_ProductOptions_Desktop_Light_CU) then
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('desktop_light.pxx');
      end
      else if FCanUpgrade6 and OpenKeyReadOnly(SRegKey_Keyman60_CU) then
      begin
        FCheckForUpdates := ValueExists(SRegValue_CheckForUpdates) and ReadBool(SRegValue_CheckForUpdates);
        FStartWithWindows := OpenKeyReadOnly('\' + SRegKey_WindowsRun_CU) and ValueExists('keyman.exe');
      end;
    finally
      Free;
    end;
  except
    on E:EOSError do
    begin
      // Trouble getting shell user details, we'll just use defaults
      GetRunTools.LogError('Failed to read shell user''s preferences, using defaults: '+E.Message+' ('+IntToStr(E.ErrorCode)+')', False);
    end;
  end;

end;

end.
