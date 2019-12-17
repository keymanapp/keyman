unit Keyman.System.Settings;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,

  KeymanVersion,
  RegistryKeys;

type
  TKeymanSettingGroup = (ksgGeneral, ksgDebug, ksgDebugKeymanDeveloper);
  TKeymanSettingType = (kstString, kstInteger);

  TKeymanSettingBase = record
    Group: TKeymanSettingGroup;
    Name: string;
    RootKey: HKEY;
    Key: string;
    Description: string;
    DefaultDesc: string;
    DefaultStr: string;
    DefaultInt: Integer;
    ValueType: TKeymanSettingType;
    function DefaultAsString: string;
    function RequiresAdministrator: Boolean;
  end;

  TKeymanSetting = class
    Base: TKeymanSettingBase;
    ValueStr: string;
    ValueInt: Integer;
    function IsEmpty: Boolean;
    function Equals(Obj: TObject): Boolean; override;
    procedure Reset;
  end;

  TKeymanSettings = class(TObjectList<TKeymanSetting>)
    constructor Create;
    procedure Reset;
  end;

const
  HKCU = HKEY_CURRENT_USER;
  HKLM = HKEY_LOCAL_MACHINE;

// TODO: we could in theory rework the existing settings which are scattered throughout Keyman to use this
// framework. For now, it is better to have this as a separate map on top of the existing settings so at
// least we can have an 'advanced' view of the settings

// TODO: add non-debug settings into this framework -- e.g. KeymanOptions from kmcomapi, Developer user settings

const
  BaseKeymanSettings: array[0..31] of TKeymanSettingBase = (

    // keyman32:k32_globals.Globals::LoadDebugSettings
    (Group: ksgDebug; Name: 'debug'; RootKey: HKCU; Key: SRegKey_KeymanEngine_CU; Description: 'Set to 1 to enable debug logging'; ValueType: kstInteger),
    (Group: ksgDebug; Name: 'debug to console'; RootKey: HKCU; Key: SRegKey_KeymanEngine_CU; Description: 'Set to 1 to enable debug logging to debug console'; ValueType: kstInteger),

    // keyman32:keyman32.Initialise_Flag_ShouldSerializeInput
    (Group: ksgDebug; Name: SRegValue_Flag_ShouldSerializeInput; RootKey: HKCU; Key: SRegKey_KeymanEngineDebug_CU; Description: 'Set to 0 to avoid serializing input'; DefaultInt: 1; ValueType: kstInteger),

    // kmshell:initprog.RunKMCOM
    (Group: ksgDebug; Name: 'appicon.ico'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to find appicon.ico'),
    (Group: ksgDebug; Name: 'cfgicon.ico'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to find cfgicon.ico'),

    // kmshell:XMLRenderer.TXMLRenderers.RenderToFile
    (Group: ksgDebug; Name: 'Debug_XMLRenderer'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Set to non-empty value to have intermediate .xml files saved to %temp% when rendering Keyman UI'),

    // kmshell:XMLRenderer.TXMLRenderers.OldXMLTemplatePath
    (Group: ksgDebug; Name: 'xmltemplate desktop_pro.kxx'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to xml UI templates for Keyman Desktop'),

    // Upload_Settings.KeymanCom_Protocol_Server
    (Group: ksgDebug; Name: 'Debug_KeymanCom'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'URL to keyman.com'; DefaultStr: 'https://keyman.com'),

    // Upload_Settings.API_Protocol
    (Group: ksgDebug; Name: 'Debug_APIProtocol'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Protocol for api.keyman.com'; DefaultStr: 'https'),

    // Upload_Settings.API_Server
    (Group: ksgDebug; Name: 'Debug_APIServer'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Host for api.keyman.com'; DefaultStr: 'api.keyman.com'),

    // KeymanPaths.TKeymanPaths.KeymanDesktopInstallPath
    (Group: ksgDebug; Name: 'KeymanDesktop'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for kmshell.exe and related resources'; DefaultDesc: '%ProgramFiles(x86)%\Keyman\Keyman Desktop'),

    // KeymanPaths.TKeymanPaths.KeymanEngineInstallPath
    (Group: ksgDebug; Name: 'KeymanEngine'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for keyman.exe and related resources'; DefaultDesc: '%CommonProgramFiles(x86)%\Keyman\Keyman Engine'),

    // KeymanPaths.TKeymanPaths.KeyboardsInstallPath
    (Group: ksgDebug; Name: 'Keyboards'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for keyboards'; DefaultDesc: '%ProgramData%\Keyman\Keyman Engine\Keyboard'),

    // VisualKeyboardExportHTML.GetOSKXSLPath
    (Group: ksgDebug; Name: 'Debug_OSKXSLPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for xml\osk templates for visual keyboard export'),

    // UfrmTouchKeyboard.GetTouchKeyboardPath
    (Group: ksgDebug; Name: 'Debug_TouchKeyboardPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for touch keyboard files (not currently implemented)'),

    // keyman:TfrmKeyman7Main.RegisterHotkeys,
    // keyman:TfrmKeyman7Main.UnregisterHotkeys
    (Group: ksgDebug; Name: SRegValue_Flag_UseRegisterHotkey; RootKey: HKCU; Key: SRegKey_KeymanEngineDebug_CU; Description: 'Set to 1 for old RegisterHotkey pathway'; ValueType: kstInteger),

    // kmcomapi:keymancontrol.TKeymanControl.LoadKeyman32.GetKeymanInstallPath
    // keyman32_int.GetKeymanInstallPath
    (Group: ksgDebug; Name: 'Debug_Keyman32Path'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path for keyman32.dll'),

    // kmcomapi:keymancontrol.TKeymanControl.LoadKeyman32.GetKeymanInstallPath
    // keyman32_int.GetKeymanInstallPath
    (Group: ksgDebug; Name: 'deep tsf integration'; RootKey: HKLM; Key: SRegKey_KeymanEngine_LM; Description: 'If present, 0=disabled; 1=enabled'; DefaultInt: 1; ValueType: kstInteger),

    // TIKE:RedistFiles.GetUnicodeDataSourcePath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_UnicodeDataSourcePath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Source files for Unicode data'),

    // TIKE:RedistFiles.GetHelpURL
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_HelpURL'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Base URL for Keyman Developer documentation'; DefaultStr: 'https://keyman.com/go/developer/'+SKeymanVersion+'/docs'),

    // TIKE:RedistFiles.GetStockKCTPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_StockPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Stock files for branding'),

    // TIKE:RedistFiles.GetRedistUIPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_RedistUIPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Redistributable UI files'),

    // TIKE:RedistFiles.GetRedistAddinsPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_RedistAddinsPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Redistributable addins path'),

    // TIKE:RedistFiles.GetRedistDesktopPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_RedistDesktopPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Redistributable Desktop install merge module path'),

    // TIKE:RedistFiles.GetRedistSetupPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_RedistSetupPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Redistributable setup.exe path'),

    // TIKE:RedistFiles.GetWixPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_WixPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to WiX executables'),

    // TIKE:RedistFiles.GetRedistProjectTemplatePath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_RedistTemplatePath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Redistributable project templates path'),

    // TIKE:RedistFiles.GetDebugKMCmpDllPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_KMCMPDLLPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to debug version of kmcmpdll.dll'),

    // TIKE:RedistFiles.GetUITemplatePath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_UITemplatePath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to UI templates'),

    // TIKE:RedistFiles.GetXMLTemplatePath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_XMLTemplatePath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to Developer xml templates\'),

    // TIKE:RedistFiles.GetCEFPath
    (Group: ksgDebugKeymanDeveloper; Name: 'Debug_CEFPath'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Path to CEF distributables'),

    // TIKE:UTikeDebugMode.TikeDebugMode
    (Group: ksgDebugKeymanDeveloper; Name: 'TikeDebugMode'; RootKey: HKCU; Key: SRegKey_KeymanDebug_CU; Description: 'Set to 1 for debug mode of Keyman Developer, enables extra logging'; ValueType: kstInteger)
  );

implementation

uses
  System.SysUtils;

{ TKeymanSetting }

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
  ValueStr := Base.DefaultStr;
  ValueInt := Base.DefaultInt;
end;

{ TKeymanSettings }

constructor TKeymanSettings.Create;
var
  i: Integer;
  s: TKeymanSetting;
begin
  inherited Create;

  for i := 0 to High(BaseKeymanSettings) do
  begin
    s := TKeymanSetting.Create;
    s.Base := BaseKeymanSettings[i];
    Add(s);
  end;
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
