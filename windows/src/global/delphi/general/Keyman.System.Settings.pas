unit Keyman.System.Settings;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  KeymanVersion,
  RegistryKeys;

type
  TKeymanSettings = class;

  TKeymanSettingType = (kstString, kstInteger);

  TKeymanSettingBase = record
    ID: string;         // unique ID which follows the debug settings style
    Name: string;       // The registry value name, varies due to history
    RootKey: HKEY;
    Key: string;
    Description: string;
    DefaultDesc: string;
    DefaultStr: string;
    DefaultInt: Integer;
    ValueType: TKeymanSettingType;
    IsCustom: Boolean;
    function DefaultAsString: string;
    function RequiresAdministrator: Boolean;
  end;

  TKeymanSetting = class
  private
    FBase: TKeymanSettingBase;
    FValueStr: string;
    FValueInt: Integer;
    FModified: Boolean;
    procedure SetValueInt(const Value: Integer);
    procedure SetValueStr(const Value: string);
  public
    constructor Create(ABase: TKeymanSettingBase);
    constructor CreateCustom_TSFApp(const ID: string);
    function IsEmpty: Boolean;
    function Equals(Obj: TObject): Boolean; override;
    procedure Reset;
    procedure ClearModified;
    property Base: TKeymanSettingBase read FBase;
    property ValueStr: string read FValueStr write SetValueStr;
    property ValueInt: Integer read FValueInt write SetValueInt;
    property Modified: Boolean read FModified;
  end;

  TKeymanSettings = class(TObjectList<TKeymanSetting>)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function Modified: Boolean;
    function Find(const ID: string): TKeymanSetting;
  end;

const
  HKCU = HKEY_CURRENT_USER;
  HKLM = HKEY_LOCAL_MACHINE;

// TODO: we could in theory rework the existing settings which are scattered throughout Keyman to use this
// framework. For now, it is better to have this as a separate map on top of the existing settings so at
// least we can have an 'advanced' view of the settings

// TODO: add non-debug settings into this framework -- e.g. KeymanOptions from kmcomapi, Developer user settings

//
// ID follows the style name pattern of '<scope>.[scope...].setting'. The following scopes are currently defined:
//
//    developer.
//    development.paths.[developer.]
//    development.urls.[developer.]
//    engine.diagnostics.
//    engine.compatibility.
//

const
//  KeymanSetting_TSFApps_ID = 'engine.compatibility.text_services_framework_apps';
  KeymanSetting_TSF_ID = 'engine.compatibility.text_services_framework';

const
  CustomKeymanSetting_TSFApp: TKeymanSettingBase = (
      ID: 'engine.compatibility.text_services_framework.';
      Name: '';
      RootKey: HKLM;
      Key: SRegKey_AppIntegration;
      Description: 'If present, 0=disabled; 1=enabled. Learn more at '+
                   'https://help.keyman.com/kb/94';
      DefaultInt: 2; // This means fall back to system default, but actually falls back to 'enabled'
      ValueType: kstInteger
  );

  BaseKeymanSettings: array[0..31] of TKeymanSettingBase = (

    // TIKE:UTikeDebugMode.TikeDebugMode
    (
      ID: 'developer.debug_mode';
      Name: 'TikeDebugMode';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Set to 1 for debug mode of Keyman Developer, enables '+
                   'extra logging';
      ValueType: kstInteger
    ),

    // kmshell:XMLRenderer.TXMLRenderers.RenderToFile
    (
      ID: 'development.debug_xml_renderer';
      Name: 'Debug_XMLRenderer';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Set to non-empty value to have intermediate .xml files '+
                   'saved to %temp% when rendering Keyman UI'
    ),

    //
    // development.paths
    //

    // kmshell:initprog.RunKMCOM
    (
      ID: 'development.paths.app_icon';
      Name: 'appicon.ico';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to find appicon.ico; you may need to set this value '+
                   'when running debug builds of Keyman Engine'
    ),

    // TIKE:RedistFiles.GetCEFPath
    (
      ID: 'development.paths.cef';
      Name: 'Debug_CEFPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to CEF (Chromium Embedded Framework) distributables'
    ),

    // kmshell:initprog.RunKMCOM
    (
      ID: 'development.paths.cfg_icon';
      Name: 'cfgicon.ico';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to find cfgicon.ico; you may need to set this value '+
                   'when running debug builds of Keyman Configuration'
    ),

    // KeymanPaths.TKeymanPaths.KeyboardsInstallPath
    (
      ID: 'development.paths.keyboards_home';
      Name: 'Keyboards';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for keyboards';
      DefaultDesc: '%ProgramData%\Keyman\Keyman Engine\Keyboard'
    ),

    // kmcomapi:keymancontrol.TKeymanControl.LoadKeyman32.GetKeymanInstallPath
    // keyman32_int.GetKeymanInstallPath
    (
      ID: 'development.paths.keyman32';
      Name: 'Debug_Keyman32Path';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for keyman32.dll; use this when debugging Keyman Core'
    ),

    // KeymanPaths.TKeymanPaths.KeymanEngineInstallPath
    (
      ID: 'development.paths.keyman_engine_home';
      Name: 'KeymanEngine';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for keyman.exe and related resources; use this when '+
                   'debugging Keyman Configuration';
      DefaultDesc: '%CommonProgramFiles(x86)%\Keyman\Keyman Engine'
    ),

    // KeymanPaths.TKeymanPaths.KeymanDesktopInstallPath
    (
      ID: 'development.paths.keyman_home';
      Name: 'KeymanDesktop';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for kmshell.exe and related resources; use this when '+
                   'debugging Keyman Configuration';
      DefaultDesc: '%ProgramFiles(x86)%\Keyman\Keyman Desktop'
    ),

    // VisualKeyboardExportHTML.GetOSKXSLPath
    (
      ID: 'development.paths.osk_templates';
      Name: 'Debug_OSKXSLPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for xml\osk templates for visual keyboard export'
    ),

    // kmshell:KeymanPaths.TKeymanPaths.KeymanConfigStaticHttpFilesPath
    (
      ID: 'development.paths.renderer_template';
      Name: 'KeymanConfigStaticHttpFilesPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to xml UI templates for Keyman; you may need '+
                   'to set this value when running debug builds of Keyman '+
                   'Configuration'
    ),

    // UfrmTouchKeyboard.GetTouchKeyboardPath
    (
      ID: 'development.paths.touch_keyboard_files';
      Name: 'Debug_TouchKeyboardPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path for touch keyboard files (not currently implemented)'
    ),

    // TIKE:RedistFiles.GetUnicodeDataSourcePath
    (
      ID: 'development.paths.unicode_data';
      Name: 'Debug_UnicodeDataSourcePath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Source files for Unicode data'
    ),

    // TIKE:RedistFiles.GetRedistAddinsPath
    (
      ID: 'development.paths.developer.redistributable_addins';
      Name: 'Debug_RedistAddinsPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Redistributable addins path'
    ),

    // TIKE:RedistFiles.GetRedistDesktopPath
    (
      ID: 'development.paths.developer.redistributable_installer';
      Name: 'Debug_RedistDesktopPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Redistributable Desktop install merge module path'
    ),

    // TIKE:RedistFiles.GetRedistSetupPath
    (
      ID: 'development.paths.developer.redistributable_setup_exe';
      Name: 'Debug_RedistSetupPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Redistributable setup.exe path'
    ),

    // TIKE:RedistFiles.GetRedistProjectTemplatePath
    (
      ID: 'development.paths.developer.redistributable_templates';
      Name: 'Debug_RedistTemplatePath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Redistributable project templates path'
    ),

    // TIKE:RedistFiles.GetRedistUIPath
    (
      ID: 'development.paths.developer.redistributable_ui';
      Name: 'Debug_RedistUIPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Redistributable UI files'
    ),

    // TIKE:RedistFiles.GetStockKCTPath
    (
      ID: 'development.paths.developer.stock_files';
      Name: 'Debug_StockPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Stock files for branding'
    ),

    // TIKE:RedistFiles.GetWixPath
    (
      ID: 'development.paths.developer.wix';
      Name: 'Debug_WixPath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to WiX executables'
    ),

    // TIKE:RedistFiles.GetXMLTemplatePath
    (
      ID: 'development.paths.developer.xml_templates';
      Name: 'Debug_XMLTemplatePath';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Path to Developer xml templates\'
    ),

    //
    // development.urls
    //

    // Upload_Settings.API_Protocol
    (
      ID: 'development.urls.api_keyman_com_protocol';
      Name: 'Debug_APIProtocol';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Protocol for api.keyman.com; use this when debugging '+
                   'Keyman with a local api.keyman.com instance';
      DefaultDesc: 'https'
    ),

    // Upload_Settings.API_Server
    (
      ID: 'development.urls.api_keyman_com_host';
      Name: 'Debug_APIServer';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Host for api.keyman.com; use this when debugging '+
                   'Keyman with a local api.keyman.com instance';
      DefaultDesc: 'api.keyman.com'
    ),

    // Upload_Settings.KeymanCom_Protocol_Server
    (
      ID: 'development.urls.keyman_com';
      Name: 'Debug_KeymanCom';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'URL to keyman.com; use this when debugging Keyman with a '+
                   'local keyman.com instance';
      DefaultDesc: 'https://keyman.com'
    ),

    // TIKE:RedistFiles.GetHelpURL
    (
      ID: 'development.urls.developer.help';
      Name: 'Debug_HelpURL';
      RootKey: HKCU;
      Key: SRegKey_KeymanDebug_CU;
      Description: 'Base URL for Keyman Developer documentation';
      DefaultDesc: 'https://keyman.com/go/developer/'+SKeymanVersion+'/docs'
    ),

    //
    // engine.compatibility
    //

    // kmcomapi:keymancontrol.TKeymanControl.LoadKeyman32.GetKeymanInstallPath
    // keyman32_int.GetKeymanInstallPath
    (
      ID: 'engine.compatibility.text_services_framework';
      Name: 'deep tsf integration';
      RootKey: HKLM;
      Key: SRegKey_KeymanEngine_LM;
      Description: 'If present, 0=disabled; 1=enabled. Learn more at '+
                   'https://help.keyman.com/kb/94';
      DefaultInt: 1;
      ValueType: kstInteger
    ),

    // keyman:TfrmKeyman7Main.RegisterHotkeys,
    // keyman:TfrmKeyman7Main.UnregisterHotkeys
    // keyman32:k32_lowlevelkeyboardhook

    (
      ID: 'engine.compatibility.use_keyman_core';
      Name: SRegValue_Flag_UseKeymanCore;
      RootKey: HKCU;
      Key: SRegKey_KeymanEngineDebug_CU;
      Description: 'When set to 1, uses the new Keyman Core common library; '+
                   'if compatibility issues are encountered, set to 0 '+
                   'to use the legacy Keyman for Windows core.';
      DefaultInt: 1;
      ValueType: kstInteger
    ),

    // keyman32:keyman32.Initialise_Flag_ShouldSerializeInput
    (
      ID: 'engine.compatibility.serialize_input';
      Name: SRegValue_Flag_ShouldSerializeInput;
      RootKey: HKCU;
      Key: SRegKey_KeymanEngineDebug_CU;
      Description: 'Set to 0 to avoid serializing input. See '+
                   'https://blog.keyman.com/2018/10/the-keyman-keyboard-input-'+
                   'pipeline/ for details';
      DefaultInt: 1;
      ValueType: kstInteger
    ),

    // kmshell:Keyman.System.KeymanStartTask
    (
      ID: 'engine.compatibility.use_autostart_task';
      Name: SRegValue_Flag_UseAutoStartTask;
      RootKey: HKCU;
      Key: SRegKey_KeymanEngineDebug_CU;
      Description: 'Set to 1 to enable the autostart task that starts '+
                   'Keyman when a Keyman TIP is selected. After making this '+
                   'change, you will need to open Keyman Configuration once '+
                   'for it to take effect.';
      DefaultInt: 0;
      ValueType: kstInteger
    ),

    // kmcomapi::TKeymanKeyboardsInstalled.TriggerWindowsLanguageSync
    (
      ID: 'engine.compatibility.sync_languages_to_cloud';
      Name: SRegValue_Flag_SyncLanguagesToCloud;
      RootKey: HKCU;
      Key: SRegKey_KeymanEngineDebug_CU;
      Description: 'Set to 0 to prevent Keyman from calling the experimental '+
                   'kmrefresh program that triggers Windows language '+
                   'synchronisation in Windows 10. This program ensures that '+
                   'language settings are not lost when you restart Windows.';
      DefaultInt: 1;
      ValueType: kstInteger
    ),

    //
    // engine.diagnostics
    //

    // keyman32:k32_globals.Globals::LoadDebugSettings
    (
      ID: 'engine.diagnostics.debug_log';
      Name: 'debug';
      RootKey: HKCU;
      Key: SRegKey_KeymanEngine_CU;
      Description: 'Set to 1 to enable debug logging';
      ValueType: kstInteger
    ),

    (
      ID: 'engine.diagnostics.debug_log_to_console';
      Name: 'debug to console';
      RootKey: HKCU;
      Key: SRegKey_KeymanEngine_CU;
      Description: 'Set to 1 to enable debug logging to debug console';
      ValueType: kstInteger
    )
  );

implementation

uses
  System.SysUtils;

{ TKeymanSetting }

procedure TKeymanSetting.ClearModified;
begin
  FModified := False;
end;

constructor TKeymanSetting.Create(ABase: TKeymanSettingBase);
begin
  inherited Create;
  FBase := ABase;
end;

constructor TKeymanSetting.CreateCustom_TSFApp(const ID: string);
begin
  inherited Create;
  FBase := CustomKeymanSetting_TSFApp;
  FBase.ID := ID;
  FBase.Name := ID.Substring(CustomKeymanSetting_TSFApp.ID.Length);
  ValueInt := FBase.DefaultInt;
end;

function TKeymanSetting.Equals(Obj: TObject): Boolean;
var
  k: TKeymanSetting;
begin
  if Obj is TKeymanSetting then
  begin
    k := Obj as TKeymanSetting;
    case Base.ValueType of
      kstString: Result := k.ValueStr.Trim = ValueStr.Trim;
      kstInteger: Result := k.ValueInt = ValueInt;
      else Result := False;
    end;
  end
  else
    Result := inherited Equals(Obj);
end;

function TKeymanSetting.IsEmpty: Boolean;
begin
  case Base.ValueType of
    kstString: Result := (ValueStr.Trim = Base.DefaultStr);
    kstInteger: Result := (ValueInt = Base.DefaultInt);
    else Result := True;
  end;
end;

procedure TKeymanSetting.Reset;
begin
  if (ValueStr <> Base.DefaultStr) or (ValueInt <> Base.DefaultInt) then
  begin
    case Base.ValueType of
      kstString: ValueStr := Base.DefaultStr;
      kstInteger: ValueInt := Base.DefaultInt;
    end;
  end;
end;

procedure TKeymanSetting.SetValueInt(const Value: Integer);
begin
  Assert(Base.ValueType = kstInteger);
  if FValueInt <> Value then
  begin
    FValueInt := Value;
    FModified := True;
  end;
end;

procedure TKeymanSetting.SetValueStr(const Value: string);
begin
  Assert(Base.ValueType = kstString);
  if FValueStr <> Value then
  begin
    FValueStr := Value;
    FModified := True;
  end;
end;

{ TKeymanSettings }

constructor TKeymanSettings.Create;
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to High(BaseKeymanSettings) do
    Add(TKeymanSetting.Create(BaseKeymanSettings[i]));
end;

destructor TKeymanSettings.Destroy;
begin
  inherited Destroy;
end;

function TKeymanSettings.Find(const ID: string): TKeymanSetting;
begin
  for Result in Self do
    if SameText(Result.Base.ID, ID) then
      Exit;
  Result := nil;
end;

function TKeymanSettings.Modified: Boolean;
var
  s: TKeymanSetting;
begin
  for s in Self do
    if s.Modified then
      Exit(True);
  Result := False;
end;

procedure TKeymanSettings.Reset;
var
  s: TKeymanSetting;
begin
  for s in Self do
    s.Reset;
end;

{ TKeymanSettingBase }

function TKeymanSettingBase.DefaultAsString: string;
begin
  if DefaultDesc <> '' then
    Result := DefaultDesc
  else
  begin
    case ValueType of
      kstString: Result := DefaultStr;
      kstInteger: Result := IntToStr(DefaultInt);
      else Result := '';
    end;
  end;
end;

function TKeymanSettingBase.RequiresAdministrator: Boolean;
begin
  Result := RootKey = HKEY_LOCAL_MACHINE;
end;

end.
