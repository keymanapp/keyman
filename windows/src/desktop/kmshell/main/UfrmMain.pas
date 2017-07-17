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
                    04 Dec 2006 - mcdurdin - Use TfrmWebContainer;
                    04 Dec 2006 - mcdurdin - Add keyboard_download, package_welcome, footer_buy, select_uilanguage
                    05 Dec 2006 - mcdurdin - Refactor using XMLRenderer
                    05 Dec 2006 - mcdurdin - Localize additional messages
                    12 Dec 2006 - mcdurdin - Capitalize form name; start on Keyboards page, not options
                    04 Jan 2007 - mcdurdin - Proxy support
                    16 May 2007 - mcdurdin - I773 - Fix bug with manual installation/uninstallation of On Screen Keyboards
                    30 May 2007 - mcdurdin - Refactored install keyboard dialog out of UfrmMain
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    30 May 2007 - mcdurdin - I858 - Added Help and Getting Started links
                    13 Jul 2007 - mcdurdin - I926 - Close down Keyman Desktop when doing an update patch.
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
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade EmbeddedWB (also I2393)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2720 - Prevent multiple instances of Keyman Configuration
                    28 Feb 2011 - mcdurdin - I2539 - After activation, Keyman Configuration does not refresh its buttons
                    18 Mar 2011 - mcdurdin - I2807 - Enable/disable addins
                    18 Mar 2011 - mcdurdin - I2392 - Improve integration of activation server client
                    18 Mar 2011 - mcdurdin - I2789 - Lang Config tasks needs to refresh configuration
                    18 Mar 2011 - mcdurdin - I2786 - Keyman Configuration app title is "Keyman Desktop" not "Keyman Desktop Configuration"
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
  System.Contnrs,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Types, Graphics, Controls, Forms, Dialogs,
  keymanapi_TLB, UfrmKeymanBase, OleCtrls, SHDocVw, EmbeddedWB,
  MSHTML_TLB, UfrmWebContainer,
  XMLRenderer, SHDocVw_EWB, EwbCore,
  KeymanEmbeddedWB;

type
  TfrmMain = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure TntFormClose(Sender: TObject; var Action: TCloseAction);
    procedure TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    FClosing: Boolean;

    DebuggingChecked: Boolean;   // I3630

    dlgOpenAddin: TOpenDialog;
    dlgOpenVisualKeyboard: TOpenDialog;

    FState: string;

    procedure dlgOpenVisualKeyboardCanClose(Sender: TObject; var CanClose: Boolean);

    procedure Keyboard_Install;
    procedure Keyboard_Uninstall(Params: TStringList);
    procedure Keyboard_Options(Params: TStringList);

    procedure Keyboards_Init;
    function Keyboards_Save: Boolean;
    function Options_Save: Boolean;
    procedure Options_Init;
    procedure InitNonVisualComponents;
    procedure Keyboard_ClickCheck(params: TStringList);
    function GetKeyboardFromParams(params: TStringList; out kbd: IKeymanKeyboardInstalled): Boolean;
    function GetPackageFromParams(params: TStringList; out pkg: IKeymanPackageInstalled): Boolean;
    procedure Package_Uninstall(Params: TStringList);
    procedure Package_Welcome(Params: TStringList);
    procedure Footer_Ok;
    procedure Footer_Cancel;

    procedure Keyboard_InstallVisualKeyboard(params: TStringList);
    procedure Keyboard_UninstallVisualKeyboard(params: TStringList);

    procedure Options_ClickCheck(params: TStringList);
    function GetOptionFromParams(params: TStringList; out option: IKeymanOption): Boolean;
    procedure Options_ResetHints;
    procedure Options_BaseKeyboard;   // I4169

    function GetHotkeyFromParams(params: TStringList;
      out hotkey: IKeymanHotkey): Boolean;
    procedure Hotkey_Set(params: TStringList);
    procedure Hotkey_Clear(params: TStringList);
    function Hotkeys_Save: Boolean;
    procedure Support_Diagnostics;
    procedure Support_Online;
    procedure Support_UpdateCheck;
    procedure Support_ProxyConfig;
    procedure Support_ContactSupport(params: TStringList);   // I4390
    procedure OpenSite(params: TStringList);
    procedure RefreshKeymanConfiguration;
    procedure SaveState;
    procedure Keyboard_Download;
    function GetHotkeyLanguageFromParams(params: TStringList; out lang: IKeymanLanguage): Boolean;
    function MustReboot: Boolean;
    function SaveAll: Boolean;
    procedure KeyboardLanguage_Install(Params: TStringList);   // I3624
    procedure KeyboardLanguage_Uninstall(Params: TStringList);   // I3624
    function GetKeyboardLanguageFromParams(params: TStringList;
      out kbdlang: IKeymanKeyboardLanguageInstalled): Boolean;   // I3624

  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
    class function ShouldRegisterWindow: Boolean; override;  // I2720
    function ShouldSetAppTitle: Boolean; override;  // I2786
  public
    procedure Do_Content_Render(FRefreshKeyman: Boolean); override;
  end;

implementation

{$R *.DFM}

uses
  axctrls,
  ActiveX,
  BaseKeyboards,
  ComObj,
  GetOSVersion,
  Hints,
  HotkeyUtils,
  HotkeysXMLRenderer,
  KeyboardListXMLRenderer,
  Imm,
  initprog,
  KeymanOptionNames,
  KeyNames,
  LanguagesXMLRenderer,
  custinterfaces,
  KLog,
  kmint,
  MessageIdentifierConsts,
  MessageIdentifiers,
  onlineconstants,
  OnlineUpdateCheck,
  OptionsXMLRenderer,
  //GlobalProxySettings,
  //Registration,
  ErrorControlledRegistry,
  RegistryKeys,
  ShellApi,
  StrUtils,
  SupportXMLRenderer,
  UfrmChangeHotkey,
  ExternalExceptionHandler,
  UfrmHTML,
  UfrmInstallKeyboard,
  UfrmInstallKeyboardFromWeb,
  UfrmInstallKeyboardLanguage,
  UfrmKeyboardOptions,
  UfrmProxyConfiguration,
  UfrmSplash,
  UfrmTextEditor,
  uninstall,
  Upload_Settings,
  utildir,
  utilexecute,
  utilkmshell,
  utilhttp,
  utiluac,
  utilxml,
  Variants;

type
  PHKL = ^HKL;

{-------------------------------------------------------------------------------
 - Form events                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.TntFormCreate(Sender: TObject);
begin
  inherited;

  kmcom.AutoApply := False;
  web.UserInterfaceOptions := web.UserInterfaceOptions + [EnableThemes];

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanDesktop) and ValueExists(SRegValue_ConfigurationState)
      then FState := ReadString(SRegValue_ConfigurationState)
      else FState := '0';
  finally
    Free;
  end;

  InitNonVisualComponents;

  XMLRenderers.RenderTemplate := 'Keyman.xsl';
  XMLRenderers.Add(TKeyboardListXMLRenderer.Create);
  XMLRenderers.Add(THotkeysXMLRenderer.Create);
  XMLRenderers.Add(TOptionsXMLRenderer.Create(Self));
  XMLRenderers.Add(TLanguagesXMLRenderer.Create);
  XMLRenderers.Add(TSupportXMLRenderer.Create);

  Icon.ReleaseHandle;
  Icon.Handle := DuplicateIcon(hInstance, Application.Icon.Handle);

  Keyboards_Init;
  Options_Init;

  Do_Content_Render(False);
end;

procedure TfrmMain.TntFormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FClosing := True;

  KL.Log('Testing kmcom.errors.rebootrequired: %s', [booltostr(kmcom.SystemInfo.RebootRequired)]);
  if kmcom.SystemInfo.RebootRequired then
    RunReboot('Windows must be restarted for changes to complete.  Restart now?',
      'Windows did not initiate the restart successfully.  You will need to restart manually.');

  Action := caFree;
end;

procedure TfrmMain.TntFormDestroy(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) then
      WriteString(SRegValue_ConfigurationState, FState);
  finally
    Free;
  end;

  Application.OnActivate := nil;
  //xmlstylesheet := nil;
  inherited;
end;

{-------------------------------------------------------------------------------
 - Form-level functions                                                        -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Do_Content_Render(FRefreshKeyman: Boolean);
var
  s: string;
begin
  SaveState;

  s := '<state>'+FState+'</state>';
  s := s + '<basekeyboard id="'+IntToHex(kmcom.Options[KeymanOptionName(koBaseLayout)].Value,8)+'">'+
    XMLEncode(TBaseKeyboards.GetName(kmcom.Options[KeymanOptionName(koBaseLayout)].Value))+
    '</basekeyboard>';   // I4169

  Content_Render(FRefreshKeyman, s);
end;

procedure TfrmMain.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'keyboard_install' then Keyboard_Install
  else if command = 'keyboard_download' then Keyboard_Download

  else if command = 'keyboard_uninstall' then Keyboard_Uninstall(params)
  else if command = 'keyboard_options' then Keyboard_Options(params)
  else if command = 'keyboard_clickcheck' then Keyboard_ClickCheck(params)
  else if command = 'keyboard_installvisualkeyboard' then Keyboard_InstallVisualKeyboard(params)
  else if command = 'keyboard_uninstallvisualkeyboard' then Keyboard_UninstallVisualKeyboard(params)

  else if command = 'keyboardlanguage_install' then KeyboardLanguage_Install(params)   // I3624
  else if command = 'keyboardlanguage_uninstall' then KeyboardLanguage_Uninstall(params)   // I3624

  else if command = 'package_uninstall' then Package_Uninstall(params)
  else if command = 'package_welcome' then Package_Welcome(params)

  else if command = 'options_clickcheck' then Options_ClickCheck(params)
  else if command = 'options_resethints' then Options_ResetHints
  else if command = 'options_basekeyboard' then Options_BaseKeyboard   // I4169
  else if command = 'language_underlyingkeyboard' then Options_BaseKeyboard

  else if command = 'hotkey_set' then Hotkey_Set(params)
  else if command = 'hotkey_clear' then Hotkey_Clear(params)

  else if command = 'support_diagnostics' then Support_Diagnostics
  else if command = 'support_online' then Support_Online
  else if command = 'support_updatecheck' then Support_UpdateCheck
  else if command = 'support_proxyconfig' then Support_ProxyConfig

  else if command = 'contact_support' then Support_ContactSupport(params)   // I4390


  else if command = 'opensite' then OpenSite(params)

  else if command = 'footer_ok' then Footer_OK
  else if command = 'footer_cancel' then Footer_Cancel

  else if command = 'help' then Application.HelpJump('context_'+lowercase(DialogName))

  else inherited;
end;

procedure TfrmMain.Footer_Cancel;
begin
  Close;
  // Instantiate a new IKeyman, and Apply to force all changes to be consistent
  CoKeyman.Create.Apply;
end;

function TfrmMain.SaveAll: Boolean;  // I2789
begin
  Result :=
    Keyboards_Save and
    Options_Save and
    Hotkeys_Save;
end;

procedure TfrmMain.Footer_Ok;
begin
  kmcom.AutoApply := False;

  if not SaveAll then Exit; // I2789

  Close;

  kmcom.Apply; // I1338 - eliminate unneccesary re-render
end;

procedure TfrmMain.InitNonVisualComponents;
begin
  dlgOpenAddin := TOpenDialog.Create(Self);
  dlgOpenAddin.Filter :=
      'Keyman add-ins (*.kma, *.kmp)|*.kma;*.kmp|Keyman add-in files (*' +
      '.kma)|*.kma|Keyman packaged add-ins (*.kmp)|*.kmp|All files (*.*' +
      ')|*.*';
  dlgOpenAddin.Title := 'Install Add-in';

  dlgOpenVisualKeyboard := TOpenDialog.Create(Self);
  dlgOpenVisualKeyboard.Filter := 'On screen keyboard files (*.kvk)|*.kvk|All files (*.*)|*.*';
  dlgOpenVisualKeyboard.Title := 'Install Keyman Keyboard';
  dlgOpenVisualKeyboard.OnCanClose := dlgOpenVisualKeyboardCanClose;
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
begin
  n := kmcom.Keyboards.IndexOf(params.Values['id']);
  if n < 0 then Exit(False);
  kbd := kmcom.Keyboards[n];
  n := StrToIntDef(params.Values['index'], -1);
  if (n < 0) or (n >= kbd.Languages.Count) then Exit(False);
  kbdlang := kbd.Languages[n];
  Result := True;
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
      //SelectLanguage(False);
      RefreshKeymanConfiguration;
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.Keyboard_Install;
begin
  if MustReboot then Exit;  // I2789

  if InstallKeyboardFromFile(Self) then
  begin
    RefreshKeymanConfiguration;
  end;
end;

procedure TfrmMain.Keyboard_Uninstall(Params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
  kbdID: WideString;
begin
  if MustReboot then Exit;  // I2789

  if GetKeyboardFromParams(params, kbd) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    kbdID := kbd.ID;
    kbd := nil;
    if UninstallKeyboard(Self, kbdID, False) then
    begin
      RefreshKeymanConfiguration;
    end;
  end;
end;


procedure TfrmMain.KeyboardLanguage_Uninstall(Params: TStringList);   // I3624
var
  kbdlang: IKeymanKeyboardLanguageInstalled;
begin
  if MustReboot then Exit;  // I2789

  if GetKeyboardLanguageFromParams(params, kbdlang) then
  begin
    if UninstallKeyboardLanguage(Self, GUIDToString(kbdlang.ProfileGUID), False) then
    begin
      RefreshKeymanConfiguration;
    end;
  end;
end;

procedure TfrmMain.KeyboardLanguage_Install(Params: TStringList);   // I3624
var
  kbd: IKeymanKeyboardInstalled;
begin
  if MustReboot then Exit;  // I2789

  if GetKeyboardFromParams(params, kbd) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    with TfrmInstallKeyboardLanguage.Create(Self) do
    try
      Keyboard := kbd;
      if ShowModal = mrOk then
        RefreshKeymanConfiguration;
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
  if MustReboot then Exit;  // I2789

  if GetPackageFromParams(params, pkg) then
  begin
    { I1201 - Fix crash uninstalling admin-installed keyboards and packages }
    pkgID := pkg.ID;
    pkg := nil;
    if UninstallPackage(Self, pkgID, False) then
    begin
      RefreshKeymanConfiguration;
    end;
  end;
end;

function TfrmMain.MustReboot: Boolean;
begin
  KL.Log('Testing kmcom.errors.rebootrequired before install/uninstall of keyboards: %s', [booltostr(kmcom.SystemInfo.RebootRequired)]);
  if kmcom.SystemInfo.RebootRequired then
  begin
    Result := True;
    RunReboot('Windows must be restarted for changes to complete before you can install or uninstall any more keyboards.  Restart now?',
      'Windows did not initiate the restart successfully.  You will need to restart manually.');
  end
  else
    Result := False;
end;

procedure TfrmMain.Package_Welcome(Params: TStringList);
var
  pkg: IKeymanPackageInstalled;
begin
  if GetPackageFromParams(params, pkg) then
  begin
    DoShowPackageWelcome(pkg, True);
  end
  else
    ShowMessage(MsgFromIdFormat(SKPackageDoesNotIncludeWelcome, ['']));
end;

procedure TfrmMain.Keyboards_Init;
begin
end;

function TfrmMain.Keyboards_Save: Boolean;
begin
  kmcom.Keyboards.Apply;
  Result := True;
end;

procedure TfrmMain.Keyboard_ClickCheck(params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
begin
  if GetKeyboardFromParams(params, kbd) then
    kbd.Loaded := StrToBool(params.Values['value']); //, 'true');
end;

procedure TfrmMain.Keyboard_InstallVisualKeyboard(params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
begin
  if GetKeyboardFromParams(params, kbd) and not Assigned(kbd.VisualKeyboard) then
    if dlgOpenVisualKeyboard.Execute then
    begin
      try
        kbd.InstallVisualKeyboard(dlgOpenVisualKeyboard.FileName);
      except
        on E:EOleException do // I1084, I1005 - Installing an invalid KVK file
          ShowMessage(E.Message);
      end;
      kbd := nil; // I773 - mcd - because Do_Content_Render reloads the keyman objects when Refresh-Keyman=true
      Do_Content_Render(True);
    end;
  kbd := nil;
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

procedure TfrmMain.Keyboard_UninstallVisualKeyboard(params: TStringList);
var
  kbd: IKeymanKeyboardInstalled;
  vk: IKeymanVisualKeyboard;
begin
  if GetKeyboardFromParams(params, kbd) then
  begin
    vk := kbd.VisualKeyboard;
    if Assigned(vk) and (MessageDlg(MsgFromIdFormat(SKUninstallOnScreenKeyboard, [kbd.Name]),
      mtConfirmation, mbOkCancel, 0) = mrOk) then
    begin
      try
        vk.Uninstall;
        vk := nil;
      except
        on E:EOleException do // I1084, I1005 - Installing an invalid KVK file
          ShowMessage(E.Message);
      end;
      kbd := nil; // I773 - mcd - because Do_Content_Render reloads the keyman objects when Refresh-Keyman=true
      Do_Content_Render(True);
    end;
    kbd := nil;
  end;
end;

procedure TfrmMain.dlgOpenVisualKeyboardCanClose(Sender: TObject; var CanClose: Boolean);
begin
{  CanClose := False;
  if FileExists(dlgOpenVisualKeyboard.FileName) then
  begin
    try
      with TVisualKeyboard.Create do
      try
        LoadFromFile(dlgOpenVisualKeyboard.FileName);
        if LowerCase(Header.AssociatedKeyboard) =
            LowerCase(SelectedRegKeyboard.Name) then
          CanClose := True
        else if MessageDlg('This visual keyboard is not designed for the keyboard you have selected.  Use it anyway?',
            mtConfirmation, mbOkCancel, 0) = mrOk then
          CanClose := True;
      finally
        Free;
      end;
    except
      on E:Exception do
      begin
        WideShowMessage(E.Message);
        CanClose := False;
      end;
    end;
    //if IsVisualKeyboardAssociatedWithCorrectFile then CanClose := True;
  end;}
end;

{-------------------------------------------------------------------------------
 - Options tab                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Options_BaseKeyboard;   // I4169
begin
  WaitForElevatedConfiguration(Handle, '-basekeyboard');
  kmcom.Options.Refresh;
  Do_Content_Render(True);
end;

procedure TfrmMain.Options_ClickCheck(params: TStringList);
var
  option: IKeymanOption;
begin
  if GetOptionFromParams(params, option) and option.Enabled then
  begin
    option.Value := not option.Value;
  end
  else
    ShowMessage(params.Text);
end;

procedure TfrmMain.Options_Init;
begin
  DebuggingChecked := kmcom.Options['koDebugging'].Value;   // I3630
end;

procedure TfrmMain.Options_ResetHints;
begin
  ResetAllHints;
  ShowMessage(MsgFromId(SKHintsReset));
end;

function TfrmMain.Options_Save: Boolean;
begin
  if (kmcom.Options['koDebugging'].Value <> DebuggingChecked) and kmcom.Options['koDebugging'].Value then
    ShowMessage(MsgFromId(SKDebuggingWarning));

  kmcom.Options.Apply;

  DebuggingChecked := kmcom.Options['koDebugging'].Value;

  Result := True;
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
    if ChangeHotkey(Self, MsgFromIdFormat(SKSetHotkey_Language, [lang2.LocaleName + ' ('+lang2.LayoutName+')']), lang2.Hotkey) then
    begin
      Do_Content_Render(False);
    end;
  end
  else if GetHotkeyFromParams(params, hotkey) then
  begin
    if ChangeHotkey(Self, MsgFromId(SKSetHotkey_Interface), hotkey) then
    begin
      Do_Content_Render(False);
    end;
  end
  else
    ShowMessage(params.Text);
end;

procedure TfrmMain.Hotkey_Clear(params: TStringList);
var
  hotkey: IKeymanHotkey;
  lang2: IKeymanLanguage;
begin
  if GetHotkeyLanguageFromParams(params, lang2) then
  begin
    lang2.Hotkey.Clear;
    Do_Content_Render(False);
  end
  else if GetHotkeyFromParams(params, hotkey) then
  begin
    hotkey.Clear;
    Do_Content_Render(False);
  end
end;

function TfrmMain.Hotkeys_Save: Boolean;
begin
  kmcom.Hotkeys.Apply;
  Result := True;
end;

{-------------------------------------------------------------------------------
 - Support tab                                                                 -
 ------------------------------------------------------------------------------}

procedure TfrmMain.Support_ContactSupport(params: TStringList);   // I4390
begin
end;

procedure TfrmMain.Support_Diagnostics;
begin
  kmcom.Control.OpenDiagnostics;
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

procedure TfrmMain.Support_UpdateCheck;
begin
  with TOnlineUpdateCheck.Create(True, False) do
  try
    case Run of
      oucShutDown:
        begin
          try
            if kmcom.Control.IsKeymanRunning then
            try
              kmcom.Control.StopKeyman;
            except
              on E:Exception do KL.Log(E.Message);
            end;
          except
            on E:Exception do KL.Log(E.Message);
          end;
        end;
      oucSuccess, oucSuccessReboot:
        RefreshKeymanConfiguration;
    end
  finally
    Free;
  end;
end;

procedure TfrmMain.TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  SaveState;
  CanClose := True;
end;

procedure TfrmMain.OpenSite(params: TStringList);
begin
  TUtilExecute.URL(params.Values['site']);  // I3349
end;

procedure TfrmMain.RefreshKeymanConfiguration;
begin
  Do_Content_Render(True);
end;

procedure TfrmMain.SaveState;
var
  elem: IHTMLElement;
begin
  try
    if Assigned(web.Document) then
    begin
      elem:= (web.Document as IHTMLDocument3).getElementById('state');
      if elem <> nil then
        FState := elem.innerText;
      elem := nil;
    end;
  except
    FState := '';
  end;
end;

class function TfrmMain.ShouldRegisterWindow: Boolean; // I2720
begin
  Result := True;
end;

function TfrmMain.ShouldSetAppTitle: Boolean;  // I2786
begin
  Result := True;
end;

end.

