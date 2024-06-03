(*
  Name:             TikeOptions
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add AllowMultipleInstances
                    23 Aug 2007 - mcdurdin - Remove unused options; I927 - add external editor
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber and remove old Keyman Developer options
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    04 Nov 2014 - mcdurdin - I4506 - V9.0 - Add command to send email with targets
                    22 Jun 2015 - mcdurdin - I4751 - Add "open in code view" default option for keyboards
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit KeymanDeveloperOptions;

interface

uses
  System.JSON,
  System.SysUtils,
  Winapi.Windows,

  ErrorControlledRegistry,
  RegistryKeys,
  Keyman.System.KeymanSentryClient;

type
  TKeymanDeveloperOptions = class
  private
    FUseOldDebugger: Boolean;
    FUseTabChar: Boolean;
    FLinkFontSizes: Boolean;
    FIndentSize: Integer;
    reg: TRegistryErrorControlled;  // I2890
    FDebuggerBreakWhenExitingLine: Boolean;
    FDebuggerSingleStepAfterBreak: Boolean;
    FDebuggerShowStoreOffset: Boolean;
    FCharMapAutoLookup: Boolean;
    FCharMapDisableDatabaseLookups: Boolean;
    FDebuggerAutoRecompileWithDebugInfo: Boolean;
    FExternalEditorPath: WideString;
    FServerDefaultPort: Integer;   // I4021
    FSMTPServer: string;   // I4506
    FTestEmailAddresses: string;   // I4506
    FOpenKeyboardFilesInSourceView: Boolean;   // I4751
    FDisplayTheme: string;
    FEditorTheme: string;
    FFix183_LadderLength: Integer;
    FDefaultProjectPath: string;
    FDebuggerAutoResetBeforeCompiling: Boolean;
    FAutoSaveBeforeCompiling: Boolean;
    FOSKAutoSaveBeforeImporting: Boolean;
    FReportErrors: Boolean;
    FReportUsage: Boolean;
    FServerUseLocalAddresses: Boolean;
    FServerUseNgrok: Boolean;
    FServerServerShowConsoleWindow: Boolean;
    FServerNgrokToken: string;
    FServerKeepAlive: Boolean;
    FToolbarVisible: Boolean;
    FStartupProjectPath: string;
    FPromptToUpgradeProjects: Boolean;
    json: TJSONObject;
    procedure OpenOptionsFileOrRegistry;
    function optReadString(const nm, def: string): string;
    function optReadBool(const nm: string; def: Boolean): Boolean;
    function optReadInt(const nm: string; def: Integer): Integer;
    procedure optWriteString(const nm, value: string);
    procedure optWriteBool(const nm: string; value: Boolean);
    procedure optWriteInt(const nm: string; value: Integer);
    procedure WriteServerConfigurationJson;
    class function Get_Initial_DefaultProjectPath: string; static;
  public
    procedure Read;
    procedure Write;

    class function DefaultEditorThemeItemIndex(s: string): Integer;
    class function IsDefaultEditorTheme(s: string): Boolean;

    property UseTabChar: Boolean read FUseTabChar write FUseTabChar;
    property IndentSize: Integer read FIndentSize write FIndentSize;
    property LinkFontSizes: Boolean read FLinkFontSizes write FLinkFontSizes;
    property UseOldDebugger: Boolean read FUseOldDebugger write FUseOldDebugger;
    property EditorTheme: string read FEditorTheme write FEditorTheme;

    property CharMapAutoLookup: Boolean read FCharMapAutoLookup write FCharMapAutoLookup;
    property CharMapDisableDatabaseLookups: Boolean read FCharMapDisableDatabaseLookups write FCharMapDisableDatabaseLookups;

    property Fix183_LadderLength: Integer read FFix183_LadderLength write FFix183_LadderLength;
    property DebuggerBreakWhenExitingLine: Boolean read FDebuggerBreakWhenExitingLine write FDebuggerBreakWhenExitingLine;
    property DebuggerSingleStepAfterBreak: Boolean read FDebuggerSingleStepAfterBreak write FDebuggerSingleStepAfterBreak;
    property DebuggerShowStoreOffset: Boolean read FDebuggerShowStoreOffset write FDebuggerShowStoreOffset;
    property DebuggerAutoRecompileWithDebugInfo: Boolean read FDebuggerAutoRecompileWithDebugInfo write FDebuggerAutoRecompileWithDebugInfo;
    property DebuggerAutoResetBeforeCompiling: Boolean read FDebuggerAutoResetBeforeCompiling write FDebuggerAutoResetBeforeCompiling;
    property AutoSaveBeforeCompiling: Boolean read FAutoSaveBeforeCompiling write FAutoSaveBeforeCompiling;
    property OSKAutoSaveBeforeImporting: Boolean read FOSKAutoSaveBeforeImporting write FOSKAutoSaveBeforeImporting;
    property PromptToUpgradeProjects: Boolean read FPromptToUpgradeProjects write FPromptToUpgradeProjects;

    property ReportErrors: Boolean read FReportErrors write FReportErrors;
    property ReportUsage: Boolean read FReportUsage write FReportUsage;

    property ServerDefaultPort: Integer read FServerDefaultPort write FServerDefaultPort;   // I4021
    property ServerKeepAlive: Boolean read FServerKeepAlive write FServerKeepAlive;
    property ServerUseLocalAddresses: Boolean read FServerUseLocalAddresses write FServerUseLocalAddresses;

    property ServerNgrokToken: string read FServerNgrokToken write FServerNgrokToken;
    property ServerUseNgrok: Boolean read FServerUseNgrok write FServerUseNgrok;
    property ServerServerShowConsoleWindow: Boolean read FServerServerShowConsoleWindow write FServerServerShowConsoleWindow;

    property OpenKeyboardFilesInSourceView: Boolean read FOpenKeyboardFilesInSourceView write FOpenKeyboardFilesInSourceView;   // I4751
    property DefaultProjectPath: string read FDefaultProjectPath write FDefaultProjectPath;

    property DisplayTheme: string read FDisplayTheme write FDisplayTheme;   // I4796

    property ExternalEditorPath: WideString read FExternalEditorPath write FExternalEditorPath;
    property SMTPServer: string read FSMTPServer write FSMTPServer;   // I4506
    property TestEmailAddresses: string read FTestEmailAddresses write FTestEmailAddresses;   // I4506

    property ToolbarVisible: Boolean read FToolbarVisible write FToolbarVisible;
    property StartupProjectPath: string read FStartupProjectPath write FStartupProjectPath;
  end;

function FKeymanDeveloperOptions: TKeymanDeveloperOptions;
function LoadKeymanDeveloperSentryFlags: TKeymanSentryClientFlags;
//procedure CreateKeymanDeveloperOptions;
//procedure DestroyKeymanDeveloperOptions;

type
  TDefaultEditorTheme = record
    Name: string;
    Desc: string;
  end;
const
  SDefaultEditorThemes: array[0..2] of TDefaultEditorTheme = (
    (Name: 'vs'; Desc: 'Light (vs)'),
    (Name: 'vs-dark'; Desc: 'Dark (vs-dark)'),
    (Name: 'hc-black'; Desc: 'High Constrast (hc-black)')
  );

const
  CDefaultProjectPath = 'Keyman Developer\Projects\';

implementation

uses
  System.Classes,
  System.Math,
  System.StrUtils,
  Winapi.ShlObj,

  JsonUtil,
  Keyman.Developer.System.KeymanDeveloperPaths,
  utilsystem,
  GetOSVersion;

const
  Max_Retries = 5;

const
  { SRegKey_IDEOptions values }
  SRegKey_IDEOptions_CU          = SRegKey_IDE_CU                 + '\Options';             // CU

  { SRegKey_IDEOptions values }

  SRegValue_IDEOptLinkFontSizes    = 'link font sizes';                            // CU
  SRegValue_IDEOptUseTabCharacter  = 'use tab char';                               // CU
  SRegValue_IDEOptIndentSize       = 'indent size';                                // CU
  SRegValue_IDEOptDocVirusCheck    = 'warn if packaging doc files';                // CU
  SRegValue_IDEOptUseSyntaxHighlighting = 'use syntax highlighting';               // CU
  SRegValue_IDEOptToolbarVisible   = 'toolbar visible';                            // CU
  SRegValue_IDEOptUseOldDebugger   = 'use old debugger';                           // CU
  SRegValue_IDEOptEditorTheme      = 'editor theme';                               // CU

  SRegValue_IDEOptDebuggerBreakWhenExitingLine = 'debugger break when exiting line';    // CU
  SRegValue_IDEOptDebuggerSingleStepAfterBreak = 'debugger single step after break';    // CU
  SRegValue_IDEOptDebuggerShowStoreOffset      = 'debugger show store offset';          // CU
  SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo = 'debugger recompile with debug info'; // CU

  SRegValue_IDEOptDebuggerAutoResetBeforeCompiling = 'debugger auto reset before compilng'; // CU
  SRegValue_IDEOptAutoSaveBeforeCompiling = 'auto save before compiling'; // CU
  SRegValue_IDEOptOSKAutoSaveBeforeImporting = 'osk auto save before importing'; // CU
  SRegValue_IDEOptPromptToUpgradeProjects = 'prompt to upgrade projects'; // CU

  // Note: keeping 'web host port' reg value name to ensure settings maintained
  //       from version 14.0 and earlier of Keyman Developer. Other values are
  //       new with Keyman Developer 15.0
  SRegValue_IDEOptServerPort = 'web host port';   // I4021
  SRegValue_IDEOptServerKeepAlive = 'server keep alive';
  SRegValue_IDEOptServerNgrokToken = 'server ngrok token';
  SRegValue_IDEOptServerUseLocalAddresses = 'server use local addresses';
  SRegValue_IDEOptServerUseNgrok = 'server use ngrok';
  SRegValue_IDEOptServerShowConsoleWindow = 'server show console window';

  SRegValue_IDEOptCharMapDisableDatabaseLookups = 'char map disable database lookups';  // CU
  SRegValue_IDEOptCharMapAutoLookup             = 'char map auto lookup';               // CU

  SRegValue_IDEOptOpenKeyboardFilesInSourceView = 'open keyboard files in source view';  // CU   // I4751

  SRegValue_IDEDisplayTheme = 'display theme';   // I4796

  SRegValue_IDEOptExternalEditorPath = 'external editor path';                      // CU

  SRegValue_IDEOptSMTPServer = 'smtp server';                                       // CU   // I4506
  SRegValue_IDEOptTestEmailAddresses = 'test email addresses';                      // CU   // I4506

  SRegValue_IDEOpt_WebLadderLength = 'web ladder length';                           // CU
  CRegValue_IDEOpt_WebLadderLength_Default = 100;

  SRegValue_IDEOpt_DefaultProjectPath = 'default project path';


var
  AFKeymanDeveloperOptions: TKeymanDeveloperOptions = nil;

procedure CreateKeymanDeveloperOptions;
begin
  AFKeymanDeveloperOptions := TKeymanDeveloperOptions.Create;
  AFKeymanDeveloperOptions.Read;
end;

procedure DestroyKeymanDeveloperOptions;
begin
  FreeAndNil(AFKeymanDeveloperOptions);
end;

function FKeymanDeveloperOptions: TKeymanDeveloperOptions;
begin
  if not Assigned(AFKeymanDeveloperOptions) then
    CreateKeymanDeveloperOptions;
  Result := AFKeymanDeveloperOptions;
end;


procedure TKeymanDeveloperOptions.OpenOptionsFileOrRegistry;
var
  count, offset: Integer;
  OptionsPath: string;
begin
  FreeAndNil(reg);
  FreeAndNil(json);

  // The JSON file ~/.keymandeveloper/options.json replaces the registry
  // settings from 17.0 onwards. There is an transition from the registry
  // to the file on the first run of Keyman Developer
  OptionsPath := TKeymanDeveloperPaths.OptionsPath + TKeymanDeveloperPaths.S_OptionsJson;
  if FileExists(OptionsPath) then
  begin
    try
      offset := 0;
      count := 0;
      repeat
        try
          json := LoadJSONFromFile(OptionsPath, offset);
          Break;
        except
          on E:EOSError do
          begin
            if E.ErrorCode = ERROR_ACCESS_DENIED then
            begin
              // Back off and retry 0.5 sec later, it's probably another
              // instance happening to write to the settings file
              Inc(count);
              Sleep(500);
            end
            else
              raise;
          end
          else
            raise;
        end;
      until count >= Max_Retries;
      if count >= Max_Retries then
      begin
        // In this instance, we'll treat the file as unreadable and fall back to
        // registry. This isn't great but there are no great options if this
        // happens
        if TKeymanSentryClient.Instance <> nil then
          TKeymanSentryClient.Instance.ReportMessage('TKeymanDeveloperOptions: Failed to read settings after 5 tries');
        json := nil;
      end;
    except
      on E:Exception do
      begin
        // There's not much we can do here, except log this to Sentry.
        // Showing a message is not workable because we don't have UI here.
        if TKeymanSentryClient.Instance <> nil then
          TKeymanSentryClient.Instance.ReportHandledException(E);
        json := nil;
      end;
    end;
  end;

  if not Assigned(json) then
  begin
    // No JSON file found, so we import from old registry settings, if present
    reg := TRegistryErrorControlled.Create;  // I2890
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKeyReadOnly(SRegKey_IDEOptions_CU) then
    begin
      // In this scenario, it's a new install of 17.0+, so we'll work from the
      // empty JSON object
      FreeAndNil(reg);
    end;

    // we always have a valid, if empty json object, even if reg is created
    json := TJSONObject.Create;
  end;
end;

procedure TKeymanDeveloperOptions.Read;
var
  FDefaultDisplayTheme: string;
begin
  OpenOptionsFileOrRegistry;
  try
    FUseTabChar     := optReadBool(SRegValue_IDEOptUseTabCharacter, False);
    FLinkFontSizes  := optReadBool(SRegValue_IDEOptLinkFontSizes,   True);
    FIndentSize     := optReadInt (SRegValue_IDEOptIndentSize,      4);
    FUseOldDebugger := optReadBool(SRegValue_IDEOptUseOldDebugger,  False);
    FEditorTheme    := optReadString(SRegValue_IDEOptEditorTheme,   '');

    FDebuggerBreakWhenExitingLine  := optReadBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, True);
    FDebuggerSingleStepAfterBreak  := optReadBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, False);
    FDebuggerShowStoreOffset       := optReadBool(SRegValue_IDEOptDebuggerShowStoreOffset,      False);
    FDebuggerAutoRecompileWithDebugInfo := optReadBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, False);
    FDebuggerAutoResetBeforeCompiling := optReadBool(SRegValue_IDEOptDebuggerAutoResetBeforeCompiling, False);
    FAutoSaveBeforeCompiling := optReadBool(SRegValue_IDEOptAutoSaveBeforeCompiling, False);
    FOSKAutoSaveBeforeImporting := optReadBool(SRegValue_IDEOptOSKAutoSaveBeforeImporting, False);
    FPromptToUpgradeProjects := optReadBool(SRegValue_IDEOptPromptToUpgradeProjects, True);

    FServerDefaultPort := optReadInt(SRegValue_IDEOptServerPort, 8008);
    FServerKeepAlive := optReadBool(SRegValue_IDEOptServerKeepAlive, False);
    FServerUseLocalAddresses := optReadBool(SRegValue_IDEOptServerUseLocalAddresses, True);

    FServerNgrokToken := optReadString(SRegValue_IDEOptServerNgrokToken, '');
    FServerUseNgrok := optReadBool(SRegValue_IDEOptServerUseNgrok, False);
    FServerServerShowConsoleWindow := optReadBool(SRegValue_IDEOptServerShowConsoleWindow, False);

    FCharMapDisableDatabaseLookups := optReadBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, False);
    FCharMapAutoLookup             := optReadBool(SRegValue_IDEOptCharMapAutoLookup,             True);

    FOpenKeyboardFilesInSourceView  := optReadBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  False);   // I4751

    if GetOS in [osWin10, osOther]
      then FDefaultDisplayTheme := 'Windows10'
      else FDefaultDisplayTheme := 'Sapphire Kamri';

    FDisplayTheme := optReadString(SRegValue_IDEDisplayTheme, FDefaultDisplayTheme);   // I4796

    FExternalEditorPath := optReadString(SRegValue_IDEOptExternalEditorPath, '');
    FSMTPServer := optReadString(SRegValue_IDEOptSMTPServer, '');   // I4506
    FTestEmailAddresses := optReadString(SRegValue_IDEOptTestEmailAddresses, '');   // I4506

    FFix183_LadderLength := optReadInt(SRegValue_IDEOpt_WebLadderLength, CRegValue_IDEOpt_WebLadderLength_Default);

    FDefaultProjectPath := IncludeTrailingPathDelimiter(optReadString(SRegValue_IDEOpt_DefaultProjectPath, Get_Initial_DefaultProjectPath));
    if (FDefaultProjectPath = '\') or IsRelativePath(FDefaultProjectPath) then
    begin
      // #11554
      FDefaultProjectPath := Get_Initial_DefaultProjectPath;
    end;

    // for consistency with Keyman.System.KeymanSentryClient, we need to use
    // reg.ReadInteger, as regReadInt, which in the dim dark past started
    // to read integers as a REG_SZ type and that's far too messy to change now.
    if Assigned(reg) then
    begin
      FReportErrors := not reg.ValueExists(SRegValue_AutomaticallyReportErrors) or
        (reg.ReadInteger(SRegValue_AutomaticallyReportErrors) <> 0);
      FReportUsage := not reg.ValueExists(SRegValue_AutomaticallyReportUsage) or
        (reg.ReadInteger(SRegValue_AutomaticallyReportUsage) <> 0);
      FToolbarVisible := optReadString(SRegValue_IDEOptToolbarVisible, '1') <> '0';
    end
    else
    begin
      FReportErrors := optReadBool(SRegValue_AutomaticallyReportErrors, True);
      FReportUsage := optReadBool(SRegValue_AutomaticallyReportUsage, True);
      FToolbarVisible := optReadBool(SRegValue_IDEOptToolbarVisible, True);
    end;

    FStartupProjectPath := optReadString(SRegValue_ActiveProject, '');

    if Assigned(reg) then
    begin
      // If we loaded from the registry, then we want to save the results to our
      // new ~/.keymandeveloper/options.json file
      Write;
    end;
  finally
    FreeAndNil(json);
    FreeAndNil(reg);
  end;
end;

procedure TKeymanDeveloperOptions.Write;
var
  count: Integer;
begin
  // If the output file does not exist, this creates it. Note that we load
  // from the existing file before writing out our set of options, which is
  // usually going to be irrelevant
  json := TJSONObject.Create;

  try
    optWriteBool(SRegValue_IDEOptUseTabCharacter, FUseTabChar);
    optWriteBool(SRegValue_IDEOptLinkFontSizes,   FLinkFontSizes);
    optWriteInt (SRegValue_IDEOptIndentSize,      FIndentSize);
    optWriteBool(SRegValue_IDEOptUseOldDebugger,  FUseOldDebugger);
    optWriteString(SRegValue_IDEOptEditorTheme,   FEditorTheme);

    optWriteBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, FDebuggerBreakWhenExitingLine);
    optWriteBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, FDebuggerSingleStepAfterBreak);
    optWriteBool(SRegValue_IDEOptDebuggerShowStoreOffset,      FDebuggerShowStoreOffset);
    optWriteBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, FDebuggerAutoRecompileWithDebugInfo);

    optWriteBool(SRegValue_IDEOptDebuggerAutoResetBeforeCompiling, FDebuggerAutoResetBeforeCompiling);
    optWriteBool(SRegValue_IDEOptAutoSaveBeforeCompiling, FAutoSaveBeforeCompiling);
    optWriteBool(SRegValue_IDEOptOSKAutoSaveBeforeImporting, FOSKAutoSaveBeforeImporting);
    optWriteBool(SRegValue_IDEOptPromptToUpgradeProjects, FPromptToUpgradeProjects);

    optWriteInt(SRegValue_IDEOptServerPort, FServerDefaultPort);
    optWriteBool(SRegValue_IDEOptServerKeepAlive, FServerKeepAlive);
    optWriteBool(SRegValue_IDEOptServerUseLocalAddresses, FServerUseLocalAddresses);

    optWriteString(SRegValue_IDEOptServerNgrokToken, FServerNgrokToken);
    optWriteBool(SRegValue_IDEOptServerUseNgrok, FServerUseNgrok);
    optWriteBool(SRegValue_IDEOptServerShowConsoleWindow, FServerServerShowConsoleWindow);


    optWriteBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, FCharMapDisableDatabaseLookups);
    optWriteBool(SRegValue_IDEOptCharMapAutoLookup,             FCharMapAutoLookup);

    optWriteBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  FOpenKeyboardFilesInSourceView);   // I4751

    optWriteString(SRegValue_IDEDisplayTheme, FDisplayTheme);   // I4796

    optWriteString(SRegValue_IDEOptExternalEditorPath,          FExternalEditorPath);
    optWriteString(SRegValue_IDEOptSMTPServer,                  FSMTPServer);   // I4506
    optWriteString(SRegValue_IDEOptTestEmailAddresses,          FTestEmailAddresses);   // I4506

    optWriteInt(SRegValue_IDEOpt_WebLadderLength, FFix183_LadderLength);

    optWriteString(SRegValue_IDEOpt_DefaultProjectPath, FDefaultProjectPath);

    optWriteBool(SRegValue_AutomaticallyReportErrors, FReportErrors);
    optWriteBool(SRegValue_AutomaticallyReportUsage, FReportUsage);
    optWriteBool(SRegValue_IDEOptToolbarVisible, FToolbarVisible);

    optWriteString(SRegValue_ActiveProject, FStartupProjectPath);

    ForceDirectories(TKeymanDeveloperPaths.OptionsPath);

    try
      count := 0;
      repeat
        try
          SaveJSONToFile(TKeymanDeveloperPaths.OptionsPath + TKeymanDeveloperPaths.S_OptionsJson, json);
          Break;
        except
          on E:EOSError do
          begin
            if E.ErrorCode = ERROR_ACCESS_DENIED then
            begin
              // Back off and retry 0.5 sec later, it's probably another
              // instance happening to read or write the settings file
              Inc(count);
              Sleep(500);
            end
            else
              raise;
          end
          else
            raise;
        end;
      until count >= Max_Retries;
      if count >= Max_Retries then
      begin
        if TKeymanSentryClient.Instance <> nil then
          TKeymanSentryClient.Instance.ReportMessage('TKeymanDeveloperOptions: Failed to write settings after 5 tries');
      end;
    except
      on E:Exception do
      begin
        // There's not much we can do here, except log this to Sentry.
        // Showing a message is not workable because we don't have UI here.
        if TKeymanSentryClient.Instance <> nil then
          TKeymanSentryClient.Instance.ReportHandledException(E);
      end;
    end;
  finally
    FreeAndNil(json);
  end;

  WriteServerConfigurationJson;
end;

procedure TKeymanDeveloperOptions.WriteServerConfigurationJson;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  try
    o.AddPair('port', TJSONNumber.Create(FServerDefaultPort));
    o.AddPair('ngrokToken', FServerNgrokToken);
    o.AddPair('useNgrok', TJSONBool.Create(FServerUseNgrok));
    o.AddPair('ngrokVisible', TJSONBool.Create(FServerServerShowConsoleWindow));
    SaveJSONToFile(TKeymanDeveloperPaths.ServerDataPath + TKeymanDeveloperPaths.S_ServerConfigJson, o);
  finally
    o.Free;
  end;
end;

procedure TKeymanDeveloperOptions.optWriteBool(const nm: string; value: Boolean);
begin
  json.AddPair(nm, TJSONBool.Create(value));
end;

procedure TKeymanDeveloperOptions.optWriteInt(const nm: string; value: Integer);
begin
  json.AddPair(nm, TJSONNumber.Create(value));
end;

procedure TKeymanDeveloperOptions.optWriteString(const nm, value: string);
begin
  json.AddPair(nm, value);
end;

function TKeymanDeveloperOptions.optReadBool(const nm: string; def: Boolean): Boolean;
var
  v: TJSONValue;
begin
  Result := def;

  if Assigned(reg) then
  begin
    if not reg.ValueExists(nm) then Exit;
    try
      Result := reg.ReadString(nm) = '1';
    except
    end;
  end;

  v := json.GetValue(nm);
  if Assigned(v) then
  begin
    v.TryGetValue<Boolean>(Result);
  end;
end;

function TKeymanDeveloperOptions.optReadInt(const nm: string; def: Integer): Integer;
var
  v: TJSONValue;
begin
  Result := def;
  if Assigned(reg) then
  begin
    if not reg.ValueExists(nm) then Exit;
    try
      Result := StrToInt(reg.ReadString(nm));
    except
    end;
  end;

  v := json.GetValue(nm);
  if Assigned(v) then
  begin
    v.TryGetValue<Integer>(Result);
  end;
end;

function TKeymanDeveloperOptions.optReadString(const nm, def: string): string;
var
  v: TJSONValue;
begin
  Result := def;
  if Assigned(reg) then
  begin
    if not reg.ValueExists(nm) then Exit;
    try
      Result := reg.ReadString(nm);
    except
    end;
    Exit;
  end;

  v := json.GetValue(nm);
  if Assigned(v) then
  begin
    v.TryGetValue<string>(Result);
  end;
end;

class function TKeymanDeveloperOptions.DefaultEditorThemeItemIndex(s: string): Integer;
var
  i: Integer;
begin
  for i := Low(SDefaultEditorThemes) to High(SDefaultEditorThemes) do
    if SDefaultEditorThemes[i].Name = s then
      Exit(i);

  Result := -1;
end;

class function TKeymanDeveloperOptions.IsDefaultEditorTheme(s: string): Boolean;
begin
  Result := DefaultEditorThemeItemIndex(s) >= 0;
end;

class function TKeymanDeveloperOptions.Get_Initial_DefaultProjectPath: string;
begin
  Result := GetFolderPath(CSIDL_PERSONAL) + CDefaultProjectPath;
end;

function LoadKeymanDeveloperSentryFlags: TKeymanSentryClientFlags;
begin
  Result := [kscfCaptureExceptions, kscfShowUI, kscfTerminate];
  if FKeymanDeveloperOptions.ReportErrors then
    Include(Result, kscfReportExceptions);
  if FKeymanDeveloperOptions.ReportUsage then
    Include(Result, kscfReportMessages);
end;

initialization
finalization
  DestroyKeymanDeveloperOptions;
end.
