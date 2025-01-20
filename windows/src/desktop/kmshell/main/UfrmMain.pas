(*
  Name:             UfrmMain
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
                    01 Aug 2006 - mcdurdin - Rework as HTML interface
                    30 Aug 2006 - mcdurdin - Use Unicode strings for command parameters
                    14 Sep 2006 - mcdurdin - Fix context menu and themes for web browser
                    06 Oct 2006 - mcdurdin - Add package welcome link
                    06 Oct 2006 - mcdurdin - Refactor web browser into parent class
                    06 Oct 2006 - mcdurdin - Save activate page state and reload
                    06 Oct 2006 - mcdurdin - Add download keyboard
                    04 Dec 2006 - mcdurdin - Use T-frmWebContainer;
                    04 Dec 2006 - mcdurdin - Add keyboard_download, package_welcome, footer_buy, select_uilanguage
                    05 Dec 2006 - mcdurdin - Refactor using XML-Renderer
                    05 Dec 2006 - mcdurdin - Localize additional messages
                    12 Dec 2006 - mcdurdin - Capitalize form name; start on Keyboards page, not options
                    04 Jan 2007 - mcdurdin - Proxy support
                    16 May 2007 - mcdurdin - I773 - Fix bug with manual installation/uninstallation of On Screen Keyboards
                    30 May 2007 - mcdurdin - Refactored install keyboard dialog out of UfrmMain
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    30 May 2007 - mcdurdin - I858 - Added Help and Getting Started links
                    13 Jul 2007 - mcdurdin - I926 - Close down Keyman when doing an update patch.
                    23 Aug 2007 - mcdurdin - I956 - Refresh locales after package installation
                    14 Sep 2007 - mcdurdin - I956 - Ensure select language dialog appears after installing locale
                    14 Sep 2007 - mcdurdin - I1061 - "Uninstall keyboard xxx" was not translated (when click Menu/Uninstall keyboard)
                    12 Oct 2007 - mcdurdin - I1084, I1005 - Handle installation of an invalid KVK file
                    05 Nov 2007 - mcdurdin - I1087 - Add language switching hotkeys (Pro only)
                    05 Nov 2007 - mcdurdin - I991 - Fix crash when uninstalling on screen keyboard
                    06 Nov 2007 - mcdurdin - I1140 - Addin configuration
                    27 Mar 2008 - mcdurdin - I1256 - Design hint system (specifically hint options)
                    27 Mar 2008 - mcdurdin - I1338 - Eliminate unnecessary re-render in Keyman Configuration
                    27 Mar 2008 - mcdurdin - I1201 - Fix crash uninstalling admin-installed keyboards/packages
                    14 Jun 2008 - mcdurdin - I1400.9 Check Language settings through Diagnostics
                    14 Jun 2008 - mcdurdin - I1487 - Improve performance of installing keyboards
                    16 Jan 2009 - mcdurdin - I1730 - Online update of keyboards
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    22 Oct 2010 - mcdurdin - Integrate purchase wizard into main form
                    05 Nov 2010 - mcdurdin - I2530 - Getting Started link in Configuration is broken
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade E-mbeddedWB (also I2393)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2720 - Prevent multiple instances of Keyman Configuration
                    28 Feb 2011 - mcdurdin - I2539 - After activation, Keyman Configuration does not refresh its buttons
                    18 Mar 2011 - mcdurdin - I2807 - Enable/disable addins
                    18 Mar 2011 - mcdurdin - I2392 - Improve integration of activation server client
                    18 Mar 2011 - mcdurdin - I2789 - Lang Config tasks needs to refresh configuration
                    18 Mar 2011 - mcdurdin - I2786 - Keyman Configuration app title is "Keyman D_esktop" not "Keyman D_esktop Configuration"
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    02 Dec 2012 - mcdurdin - I3630 - V9.0 - Configuration crashes due to missing options koTurnOnSurrogates
                    01 Jan 2013 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
                    01 Jan 2013 - mcdurdin - I3717 - V9.0 - Need ability to select base keyboard in Keyman Configuration
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
*)
unit UfrmMain;  // I3306   // I4248

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.AppEvnts,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Winapi.Messages,
  Winapi.Windows,

  keymanapi_TLB,
  Sentry.Client,
  XMLRenderer,
  KeyboardListXMLRenderer,
  UfrmKeymanBase,
  UfrmWebContainer;

type
  TfrmMain = class(TfrmWebContainer)
    AppEvents: TApplicationEvents;
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure TntFormClose(Sender: TObject; var Action: TCloseAction);
    procedure TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);

  private
    FPageTag: Integer;
    FClosing: Boolean;
    wm_keyman_refresh: Cardinal;
    LastRefreshToken: System.IntPtr;

    procedure cefBeforeBrowse(Sender: TObject; const Url: string;
      isPopup, wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string;
      isPopup: Boolean; out Handled: Boolean);

    procedure Keyboard_Install;
    procedure Keyboard_Uninstall(Params: TStringList);
    procedure Keyboard_Options(Params: TStringList);
    procedure Keyboards_Init;
    procedure Keyboard_Download;
    procedure KeyboardLanguage_Install(Params: TStringList);   // I3624
    procedure KeyboardLanguage_Uninstall(Params: TStringList);   // I3624
    function GetKeyboardLanguageFromParams(params: TStringList;
      out kbdlang: IKeymanKeyboardLanguageInstalled): Boolean;   // I3624
    procedure Keyboard_ClickCheck(params: TStringList);
    function GetKeyboardFromParams(params: TStringList; out kbd: IKeymanKeyboardInstalled): Boolean;

    function GetPackageFromParams(params: TStringList; out pkg: IKeymanPackageInstalled): Boolean;
    procedure Package_Uninstall(Params: TStringList);
    procedure Package_Welcome(Params: TStringList);

    procedure Options_Init;
    procedure Options_ClickCheck(params: TStringList);
    function GetOptionFromParams(params: TStringList; out option: IKeymanOption): Boolean;
    procedure Options_ResetHints;
    procedure Options_BaseKeyboard;   // I4169
    procedure Options_SettingsManager;

    procedure Hotkey_Set(params: TStringList);
    procedure Hotkey_Clear(params: TStringList);
    function GetHotkeyFromParams(params: TStringList;
      out hotkey: IKeymanHotkey): Boolean;
    function GetHotkeyLanguageFromParams(params: TStringList; out lang: IKeymanLanguage): Boolean;

    procedure Support_Diagnostics;
    procedure Support_Online;
    procedure Support_ProxyConfig;
    procedure Support_ContactSupport(params: TStringList);   // I4390

    procedure OpenSite(params: TStringList);
    procedure DoApply;
    procedure DoRefresh;
    procedure Update_CheckNow;
    procedure Update_ApplyNow;

  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
    class function ShouldRegisterWindow: Boolean; override;  // I2720
    function ShouldSetAppTitle: Boolean; override;   // I2786
  public
    procedure Do_Content_Render; override;
  end;

implementation

{$R *.DFM}

uses
  System.StrUtils,
  System.Variants,
  Winapi.ShellApi,

  BaseKeyboards,
  ErrorControlledRegistry,
  GetOSVersion,
  Hints,
  HotkeyUtils,
  initprog,
  Keyman.Configuration.System.TIPMaintenance,
  Keyman.Configuration.UI.UfrmDiagnosticTests,
  KeymanOptionNames,
  KeymanVersion,
  KeyNames,
  custinterfaces,
  HotkeysXMLRenderer,
  KLog,
  kmint,
  LanguagesXMLRenderer,
  MessageIdentifierConsts,
  MessageIdentifiers,
  Keyman.System.ExecutionHistory,
  Keyman.System.KeymanSentryClient,
  Keyman.System.RemoteUpdateCheck,
  OptionsXMLRenderer,
  Keyman.Configuration.System.UmodWebHttpServer,
  Keyman.Configuration.System.HttpServer.App.ConfigMain,
  Keyman.Configuration.UI.InstallFile,
  Keyman.Configuration.UI.UfrmSettingsManager,
  Keyman.Configuration.UI.UfrmStartInstallNow,
  RegistryKeys,
  SupportXMLRenderer,
  UfrmChangeHotkey,
  UfrmHTML,
  UfrmInstallKeyboardFromWeb,
  UfrmInstallKeyboardLanguage,
  UfrmKeyboardOptions,
  UfrmProxyConfiguration,
  UfrmSplash,
  UfrmTextEditor,
  uninstall,
  Upload_Settings,
  UpdateXMLRenderer,
  utildir,
  utilexecute,
  utilkmshell,
  utilhttp,
  utiluac,
  utilxml,
  KeymanPaths;

type
  PHKL = ^HKL;

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.TntFormCreate(Sender: TObject);
begin
  inherited;

  // Yes, there is a disconnect between the registered name and the variable
  // name. Everywhere else we use this we do the same thing -- probably should
  // update the variable name one day.
  wm_keyman_refresh := RegisterWindowMessage('WM_KEYMANREFRESH');

  // Prevents keep-in-touch opening in browser
  cef.ShouldOpenRemoteUrlsInBrowser := False;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;

  Icon.ReleaseHandle;
  Icon.Handle := DuplicateIcon(hInstance, Application.Icon.Handle);

  Keyboards_Init;
  Options_Init;

  FRenderPage := 'keyman'; // TODO: rename to 'configmain'

  Do_Content_Render;
end;

procedure TfrmMain.TntFormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FClosing := True;
  Action := caFree;
end;

procedure TfrmMain.TntFormDestroy(Sender: TObject);
begin
  Application.OnActivate := nil;
  if FPageTag > 0 then
    modWebHttpServer.SharedData.Remove(FPageTag);
  inherited;
end;

{-------------------------------------------------------------------------------
 - Form-level functions                                                        -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Do_Content_Render;
var
  sharedDataIntf: IConfigMainSharedData;
  sharedData: TConfigMainSharedData;
  s: string;
  xml: string;
  FKeyboardXMLRenderer: TKeyboardListXMLRenderer;
  FXMLRenderers: TXMLRenderers;
begin
  if FPageTag > 0 then
  begin
    modWebHttpServer.SharedData.Remove(FPageTag);
    FPageTag := 0;
  end;

  // TODO: Unlike other forms, we currently render the XML here in the window in
  // order to avoid loading the Keyman Configuration data for every page view. A
  // better approach would be shared data but the threading concerns for keyman
  // API mean that this is a bigger job

  sharedData := TConfigMainSharedData.Create;
  sharedDataIntf := sharedData;

  FPageTag := modWebHttpServer.SharedData.Add(sharedDataIntf);

  s := Format('<PageTag>%d</PageTag><state>%s</state><basekeyboard id="%08.8x">%s</basekeyboard>', [
    FPageTag, // This is used in the XSL transform
    XMLEncode(sharedDataIntf.State),
    Cardinal(kmcom.Options[KeymanOptionName(koBaseLayout)].Value),
    XMLEncode(TBaseKeyboards.GetName(kmcom.Options[KeymanOptionName(koBaseLayout)].Value))
  ]) + DefaultServersXMLTags + DefaultVersionXMLTags;

  FXMLRenderers := TXMLRenderers.Create;
  try
    FXMLRenderers.RenderTemplate := 'Keyman.xsl';
    FKeyboardXMLRenderer := TKeyboardListXMLRenderer.Create(FXMLRenderers);
    FXMLRenderers.Add(FKeyboardXMLRenderer);
    FXMLRenderers.Add(THotkeysXMLRenderer.Create(FXMLRenderers));
    FXMLRenderers.Add(TOptionsXMLRenderer.Create(FXMLRenderers));
    FXMLRenderers.Add(TLanguagesXMLRenderer.Create(FXMLRenderers));
    FXMLRenderers.Add(TSupportXMLRenderer.Create(FXMLRenderers));
    FXMLRenderers.Add(TUpdateXMLRenderer.Create(FXMLRenderers));

    xml := FXMLRenderers.RenderToString(s);
    sharedData.Init(
      FXMLRenderers.TempPath,
      xml,
      FKeyboardXMLRenderer.FileReferences.ToStringArray
    );
  finally
    FXMLRenderers.Free;
  end;

  Content_Render('tag='+IntToStr(FPageTag));
end;

procedure TfrmMain.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'keyboard_install' then Keyboard_Install
  else if command = 'keyboard_download' then Keyboard_Download

  else if command = 'keyboard_uninstall' then Keyboard_Uninstall(params)
  else if command = 'keyboard_options' then Keyboard_Options(params)
  else if command = 'keyboard_clickcheck' then Keyboard_ClickCheck(params)

  else if command = 'keyboardlanguage_install' then KeyboardLanguage_Install(params)   // I3624
  else if command = 'keyboardlanguage_uninstall' then KeyboardLanguage_Uninstall(params)   // I3624

  else if command = 'package_uninstall' then Package_Uninstall(params)
  else if command = 'package_welcome' then Package_Welcome(params)

  else if command = 'options_clickcheck' then Options_ClickCheck(params)
  else if command = 'options_resethints' then Options_ResetHints
  else if command = 'options_basekeyboard' then Options_BaseKeyboard   // I4169
  else if command = 'options_settingsmanager' then Options_SettingsManager

  else if command = 'language_underlyingkeyboard' then Options_BaseKeyboard

  else if command = 'hotkey_set' then Hotkey_Set(params)
  else if command = 'hotkey_clear' then Hotkey_Clear(params)

  else if command = 'support_diagnostics' then Support_Diagnostics
  else if command = 'support_online' then Support_Online
  else if command = 'support_proxyconfig' then Support_ProxyConfig

  else if command = 'update_checknow' then Update_CheckNow
  else if command = 'update_applynow' then Update_ApplyNow

  else if command = 'contact_support' then Support_ContactSupport(params)   // I4390

  else if command = 'opensite' then OpenSite(params)

  else if command = 'help' then DoOpenHelp

  else inherited;
end;

{-------------------------------------------------------------------------------
 - Parsing parameters from url callback in web page                            -
 ------------------------------------------------------------------------------}

function TfrmMain.GetKeyboardFromParams(params: TStringList; out kbd: IKeymanKeyboardInstalled): Boolean;
var
  n: Integer;
begin
  n := kmcom.Keyboards.IndexOf(params.Values['id']);
  if n < 0 then Exit(False);
  kbd := kmcom.Keyboards[n];
  Result := True;
end;

function TfrmMain.GetKeyboardLanguageFromParams(params: TStringList; out kbdlang: IKeymanKeyboardLanguageInstalled): Boolean;   // I3624
var
  n: Integer;
  kbd: IKeymanKeyboardInstalled;
  bcp47code: string;
begin
  n := kmcom.Keyboards.IndexOf(params.Values['id']);
  if n < 0 then Exit(False);
  kbd := kmcom.Keyboards[n];
  bcp47code := params.Values['bcp47code'];
  for n := 0 to kbd.Languages.Count - 1 do
  begin
    if kbd.Languages[n].BCP47Code = bcp47code then
    begin
      kbdlang := kbd.Languages[n];
      Exit(True);
    end;
  end;
  Result := False;
end;

function TfrmMain.GetPackageFromParams(params: TStringList; out pkg: IKeymanPackageInstalled): Boolean;
var
  n: Integer;
begin
  n := kmcom.Packages.IndexOf(params.Values['id']);
  if n < 0 then Exit(False);
  pkg := kmcom.Packages[n];
  Result := True;
end;

function TfrmMain.GetOptionFromParams(params: TStringList; out option: IKeymanOption): Boolean;
var
  n: Integer;
begin
  n := kmcom.Options.IndexOf(params.Values['id']);
  if n < 0 then Exit(False);
  option := kmcom.Options[n];
  Result := True;
end;

function TfrmMain.GetHotkeyFromParams(params: TStringList; out hotkey: IKeymanHotkey): Boolean;
var
  n, i: Integer;
begin
  hotkey := nil;
  if Copy(params.Values['index'], 1, 7) <> 'hotkey_' then Exit(False);
  n := StrToIntDef(Copy(params.Values['index'], 8, 100), -1);
  if n < 0 then Exit(False);

  for i := 0 to kmcom.Hotkeys.Count - 1 do
  begin
    if kmcom.Hotkeys[i].Target = KeymanHotkeyTarget(n) then
    begin
      hotkey := kmcom.Hotkeys[i];
      Exit(True);
    end;
  end;

  Result := False;
end;

function TfrmMain.GetHotkeyLanguageFromParams(params: TStringList; out lang: IKeymanLanguage): Boolean;
var
  n: Integer;
begin
  if Copy(Params.Values['index'], 1, 12) <> 'hotkey_lang_' then Exit(False);
  n := StrToIntDef(Copy(params.Values['index'], 13, 100), -1);
  if n < 0 then Exit(False);
  lang := kmcom.Languages[n];
  Result := True;
end;

{-------------------------------------------------------------------------------
 - Keyboards tab                                                               -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Keyboard_Download;
begin
  with TfrmInstallKeyboardFromWeb.Create(Self) do
  try
    if ShowModal = mrOk then
    begin
      DoRefresh;
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.Keyboard_Install;
begin
  if TInstallFile.BrowseAndInstallKeyboardFromFile(Self) then
  begin
    DoRefresh;
  end;
end;

procedure TfrmMain.Keyboard_Uninstall(Params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
  kbdID: WideString;
begin
  if GetKeyboardFromParams(params, kbd) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    kbdID := kbd.ID;
    kbd := nil;
    if UninstallKeyboard(Self, kbdID, False) then
    begin
      DoRefresh;
    end;
  end;
end;


procedure TfrmMain.KeyboardLanguage_Uninstall(Params: TStringList);   // I3624
var
  kbdlang: IKeymanKeyboardLanguageInstalled;
begin
  if GetKeyboardLanguageFromParams(params, kbdlang) then
  begin
    // TODO: refactor this away?
    if UninstallKeyboardLanguage(GUIDToString(kbdlang.ProfileGUID), False) then
    begin
      DoRefresh;
    end;
  end;
end;

procedure TfrmMain.KeyboardLanguage_Install(Params: TStringList);   // I3624
var
  kbd: IKeymanKeyboardInstalled;
begin
  if GetKeyboardFromParams(params, kbd) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    with TfrmInstallKeyboardLanguage.Create(Self) do
    try
      Keyboard := kbd;
      if ShowModal = mrOk then
        DoRefresh;
    finally
      Free;
    end;
  end;
end;


procedure TfrmMain.Package_Uninstall(Params: TStringList);
var
  pkg: IKeymanPackageInstalled;
  pkgID: WideString;
begin
  if GetPackageFromParams(params, pkg) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    pkgID := pkg.ID;
    pkg := nil;
    if UninstallPackage(Self, pkgID, False) then
    begin
      DoRefresh;
    end;
  end;
end;

procedure TfrmMain.Package_Welcome(Params: TStringList);
var
  pkg: IKeymanPackageInstalled;
begin
  if GetPackageFromParams(params, pkg) then
  begin
    DoShowPackageWelcome(Self, pkg, True);
  end
  else
    ShowMessage(MsgFromIdFormat(SKPackageDoesNotIncludeWelcome, ['']));
end;

procedure TfrmMain.Keyboards_Init;
begin
end;

procedure TfrmMain.Keyboard_ClickCheck(params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
  lang: IKeymanKeyboardLanguageInstalled2;
  loaded: Boolean;
begin
  if GetKeyboardFromParams(params, kbd) then
  begin
    loaded := StrToBool(params.Values['value']);
    if (params.Values['install'] <> '') and (kbd.Languages.Count > 0) then
    begin
      // For Keyman for FirstVoices, we will install the suggested language
      // only when the checkbox is ticked, not when the keyboard is installed.
      kbd.Loaded := True;
      lang := kbd.Languages.Items[0] as IKeymanKeyboardLanguageInstalled2;
      if loaded and not lang.IsInstalled then
      begin
        if not TTipMaintenance.DoInstall(kbd.ID, lang.WindowsBCP47Code) then
        begin
          // We'll need to refresh in this situation as the checkbox state will
          // not match reality
          ShowMessage(MsgFromIdFormat(SKInstallLanguageTransientLimit, [lang.WindowsBCP47Code]));
          kbd.Loaded := False;
          DoApply;
          DoRefresh;
        end;
      end
      else if not loaded and lang.IsInstalled then
      begin
        lang.Uninstall;
        kbd.Loaded := False;
      end;
    end
    else
      kbd.Loaded := loaded;
    DoApply; // No need to refresh page (checkbox state will already be correct)
  end;
end;

procedure TfrmMain.Keyboard_Options(Params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
  kbdID: WideString;
begin
  if GetKeyboardFromParams(params, kbd) then
  begin
    if not Assigned(kbd.OwnerPackage) or not FileExists(ExtractFilePath(kbd.OwnerPackage.Filename) + 'options.htm') then
    begin
      //WideShowMessage(MsgFromId(SKKeyboardHasNoOptions);
      Exit;
    end;

    kbdID := kbd.ID;
    kbd := nil;
    ShowKeyboardOptions(Self, kbdID);
  end;
end;

procedure TfrmMain.AppEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  inherited;
  if (Msg.message = wm_keyman_refresh) and (Msg.WParam = KR_SETTINGS_CHANGED) then
  begin
    // If we generated this refresh ourselves, ignore it
    if (kmcom <> nil) and (Msg.lParam <> KeymanEngineControl.LastRefreshToken) then
    begin
      // The refresh event was generated by another process or thread
      //
      // We may receive the notification multiple times because it is broadcast
      // to all top-level windows and we probably have a bunch of them.
      // So, if we've already processed it, don't do so again.
      if Msg.lParam <> LastRefreshToken then
      begin
        LastRefreshToken := Msg.lParam;
        DoRefresh;
      end;
    end;
  end;
end;

procedure TfrmMain.cefBeforeBrowse(Sender: TObject; const Url: string;
  isPopup, wasHandled: Boolean);
begin
  if isPopup then
    if not TUtilExecute.URL(Url) then
      ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmMain.cefBeforeBrowseSync(Sender: TObject; const Url: string;
  isPopup: Boolean; out Handled: Boolean);
begin
  Handled := isPopup;
end;

{-------------------------------------------------------------------------------
 - Options tab                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Options_BaseKeyboard;   // I4169
begin
  WaitForElevatedConfiguration(Handle, '-basekeyboard');
  // Refresh will be triggered by elevated process
end;

procedure TfrmMain.Options_SettingsManager;
begin
  if kmcom.SystemInfo.IsAdministrator then
  begin
    if not TfrmSettingsManager.Execute then
      Exit;
    // Settings manager tells everyone to refresh but we ignore it because
    // it is same thread, so do it ourselves
    DoRefresh;
  end
  else
  begin
    WaitForElevatedConfiguration(Handle, '-settings');
    // Refresh will be triggered by elevated process
  end;
end;

procedure TfrmMain.Options_ClickCheck(params: TStringList);
var
  option: IKeymanOption;
begin
  if GetOptionFromParams(params, option) and option.Enabled then
  begin
    option.Value := not option.Value;

    if (option.ID = 'koDebugging') and option.Value then
      ShowMessage(MsgFromId(SKDebuggingWarning));

    kmcom.Errors.Clear;
    DoApply;
    if kmcom.Errors.Count > 0 then
    begin
      // We may not have permission or ability to set an option. In this case,
      // we'll need to reload the page because the checkbox state may now
      // be invalid
      ShowMessage(kmcom.Errors[0].Description);
      DoRefresh;
    end;
  end
  else
    ShowMessage(params.Text);
end;

procedure TfrmMain.Options_Init;
begin
end;

procedure TfrmMain.Options_ResetHints;
begin
  ResetAllHints;
  ShowMessage(MsgFromId(SKHintsReset));
end;

{-------------------------------------------------------------------------------
 - Hotkeys tab                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Hotkey_Set(params: TStringList);
var
  hotkey: IKeymanHotkey;
  lang2: IKeymanLanguage;
begin
  if GetHotkeyLanguageFromParams(params, lang2) then
  begin
    if not ChangeHotkey(Self, MsgFromIdFormat(SKSetHotkey_Language, [lang2.LocaleName + ' ('+lang2.LayoutName+')']), lang2.Hotkey) then
      Exit;
    DoApply; // ChangeHotkey does not currently do an Apply
    DoRefresh;
  end
  else if GetHotkeyFromParams(params, hotkey) then
  begin
    if not ChangeHotkey(Self, MsgFromId(SKSetHotkey_Interface), hotkey) then
      Exit;
    DoApply; // ChangeHotkey does not currently do an Apply
    DoRefresh;
  end
  else
  begin
    ShowMessage(params.Text);
    Exit;
  end;
end;

procedure TfrmMain.Hotkey_Clear(params: TStringList);
var
  hotkey: IKeymanHotkey;
  lang2: IKeymanLanguage;
begin
  if GetHotkeyLanguageFromParams(params, lang2) then
  begin
    lang2.Hotkey.Clear;
  end
  else if GetHotkeyFromParams(params, hotkey) then
  begin
    hotkey.Clear;
  end
  else
    Exit;

  DoApply;
  DoRefresh;
end;

{-------------------------------------------------------------------------------
 - Support tab                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Support_ContactSupport(params: TStringList);   // I4390
begin
end;

procedure TfrmMain.Support_Diagnostics;
begin
  // Show the internal debug diagnostic tests form if Ctrl+Shift is down
  if(GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0)
    then TfrmDiagnosticTests.Run
    else kmcom.Control.OpenDiagnostics;
end;

procedure TfrmMain.Support_Online;
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_Support));  // I3349
end;

procedure TfrmMain.Support_ProxyConfig;
begin
  with TfrmProxyConfiguration.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmMain.Update_CheckNow;
// TODO: epic-windows-update
// Get an instance to the state machine and call handle check so the state can change to update
// available.
var UpdateCheck : TRemoteUpdateCheck;
begin
  UpdateCheck := TRemoteUpdateCheck.Create(True);
  try
    UpdateCheck.Run;
  finally
    UpdateCheck.Free;
  end;
  DoRefresh;
end;

procedure TfrmMain.Update_ApplyNow;
var
  ShellPath : string;
  FResult, InstallNow: Boolean;
  frmStartInstallNow: TfrmStartInstallNow;
begin
  InstallNow := True;
  // Confirm User is ok that this will require a reset
  if HasKeymanRun then
  begin
    frmStartInstallNow := TfrmStartInstallNow.Create(nil);
    try
      if frmStartInstallNow.ShowModal = mrOk then
        InstallNow := True
      else
        InstallNow := False;
    finally
      frmStartInstallNow.Free;
    end;
  end;

  if InstallNow = True then
  begin
    ShellPath := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
    FResult := TUtilExecute.Shell(0, ShellPath, '', '-an');
    if not FResult then
      TKeymanSentryClient.Client.MessageEvent(Sentry.Client.SENTRY_LEVEL_ERROR,
        'TrmfMain: Shell Execute Update_ApplyNow Failed');
  end;
end;

procedure TfrmMain.TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := True;
end;

procedure TfrmMain.OpenSite(params: TStringList);
begin
  TUtilExecute.URL(params.Values['site']);  // I3349
end;

class function TfrmMain.ShouldRegisterWindow: Boolean; // I2720
begin
  Result := True;
end;

function TfrmMain.ShouldSetAppTitle: Boolean;  // I2786
begin
  Result := True;
end;

procedure TfrmMain.DoApply;
begin
  kmcom.Apply;
end;

procedure TfrmMain.DoRefresh;
begin
  kmcom.Refresh;
  Do_Content_Render;
end;

end.

