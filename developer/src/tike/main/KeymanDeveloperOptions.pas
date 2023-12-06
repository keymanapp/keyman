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
  System.SysUtils,
  Winapi.Windows,

  ErrorControlledRegistry,
  RegistryKeys;

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
    FServerNgrokRegion: string;
    FServerKeepAlive: Boolean;
    FPromptToUpgradeProjects: Boolean;
    procedure CloseRegistry;
    procedure OpenRegistry;
    function regReadString(const nm, def: string): string;
    function regReadBool(const nm: string; def: Boolean): Boolean;
    function regReadInt(const nm: string; def: Integer): Integer;
    procedure regWriteString(const nm, value: string);
    procedure regWriteBool(const nm: string; value: Boolean);
    procedure regWriteInt(const nm: string; value: Integer);
    procedure WriteServerConfigurationJson;
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
    property ServerNgrokRegion: string read FServerNgrokRegion write FServerNgrokRegion;
    property ServerUseNgrok: Boolean read FServerUseNgrok write FServerUseNgrok;
    property ServerServerShowConsoleWindow: Boolean read FServerServerShowConsoleWindow write FServerServerShowConsoleWindow;

    property OpenKeyboardFilesInSourceView: Boolean read FOpenKeyboardFilesInSourceView write FOpenKeyboardFilesInSourceView;   // I4751
    property DefaultProjectPath: string read FDefaultProjectPath write FDefaultProjectPath;

    property DisplayTheme: string read FDisplayTheme write FDisplayTheme;   // I4796

    property ExternalEditorPath: WideString read FExternalEditorPath write FExternalEditorPath;
    property SMTPServer: string read FSMTPServer write FSMTPServer;   // I4506
    property TestEmailAddresses: string read FTestEmailAddresses write FTestEmailAddresses;   // I4506
  end;

function FKeymanDeveloperOptions: TKeymanDeveloperOptions;
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
  System.JSON,
  System.Math,
  Winapi.ShlObj,

  JsonUtil,
  Keyman.Developer.System.KeymanDeveloperPaths,
  utilsystem,
  GetOSVersion;

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


procedure TKeymanDeveloperOptions.OpenRegistry;
begin
  reg := TRegistryErrorControlled.Create;  // I2890
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKey(SRegKey_IDEOptions_CU, True);
end;

procedure TKeymanDeveloperOptions.CloseRegistry;
begin
  reg.Free;
  reg := nil;
end;

procedure TKeymanDeveloperOptions.Read;
var
  FDefaultDisplayTheme: string;
begin
  OpenRegistry;
  try
    FUseTabChar     := regReadBool(SRegValue_IDEOptUseTabCharacter, False);
    FLinkFontSizes  := regReadBool(SRegValue_IDEOptLinkFontSizes,   True);
    FIndentSize     := regReadInt (SRegValue_IDEOptIndentSize,      4);
    FUseOldDebugger := regReadBool(SRegValue_IDEOptUseOldDebugger,  False);
    FEditorTheme    := regReadString(SRegValue_IDEOptEditorTheme,   '');

    FDebuggerBreakWhenExitingLine  := regReadBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, True);
    FDebuggerSingleStepAfterBreak  := regReadBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, False);
    FDebuggerShowStoreOffset       := regReadBool(SRegValue_IDEOptDebuggerShowStoreOffset,      False);
    FDebuggerAutoRecompileWithDebugInfo := regReadBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, False);
    FDebuggerAutoResetBeforeCompiling := regReadBool(SRegValue_IDEOptDebuggerAutoResetBeforeCompiling, False);
    FAutoSaveBeforeCompiling := regReadBool(SRegValue_IDEOptAutoSaveBeforeCompiling, False);
    FOSKAutoSaveBeforeImporting := regReadBool(SRegValue_IDEOptOSKAutoSaveBeforeImporting, False);
    FPromptToUpgradeProjects := regReadBool(SRegValue_IDEOptPromptToUpgradeProjects, True);

    FServerDefaultPort := regReadInt(SRegValue_IDEOptServerPort, 8008);
    FServerKeepAlive := regReadBool(SRegValue_IDEOptServerKeepAlive, False);
    FServerUseLocalAddresses := regReadBool(SRegValue_IDEOptServerUseLocalAddresses, True);

    FServerNgrokToken := regReadString(SRegValue_IDEOptServerNgrokToken, '');
    FServerNgrokRegion := regReadString(SRegValue_IDEOptServerNgrokRegion, 'us');
    FServerUseNgrok := regReadBool(SRegValue_IDEOptServerUseNgrok, False);
    FServerServerShowConsoleWindow := regReadBool(SRegValue_IDEOptServerShowConsoleWindow, False);

    FCharMapDisableDatabaseLookups := regReadBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, False);
    FCharMapAutoLookup             := regReadBool(SRegValue_IDEOptCharMapAutoLookup,             True);

    FOpenKeyboardFilesInSourceView  := regReadBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  False);   // I4751

    if GetOS in [osWin10, osOther]
      then FDefaultDisplayTheme := 'Windows10'
      else FDefaultDisplayTheme := 'Sapphire Kamri';

    FDisplayTheme := regReadString(SRegValue_IDEDisplayTheme, FDefaultDisplayTheme);   // I4796

    FExternalEditorPath := regReadString(SRegValue_IDEOptExternalEditorPath, '');
    FSMTPServer := regReadString(SRegValue_IDEOptSMTPServer, '');   // I4506
    FTestEmailAddresses := regReadString(SRegValue_IDEOptTestEmailAddresses, '');   // I4506

    FFix183_LadderLength := regReadInt(SRegValue_IDEOpt_WebLadderLength, CRegValue_IDEOpt_WebLadderLength_Default);

    FDefaultProjectPath := IncludeTrailingPathDelimiter(regReadString(SRegValue_IDEOpt_DefaultProjectPath, GetFolderPath(CSIDL_PERSONAL) + CDefaultProjectPath));

    // for consistency with Keyman.System.KeymanSentryClient, we need to use
    // reg.ReadInteger, as regReadInt, which in the dim dark past started
    // to read integers as a REG_SZ type and that's far too messy to change now.
    FReportErrors := not reg.ValueExists(SRegValue_AutomaticallyReportErrors) or
      (reg.ReadInteger(SRegValue_AutomaticallyReportErrors) <> 0);
    FReportUsage := not reg.ValueExists(SRegValue_AutomaticallyReportUsage) or
      (reg.ReadInteger(SRegValue_AutomaticallyReportUsage) <> 0);
  finally
    CloseRegistry;
  end;
end;

procedure TKeymanDeveloperOptions.Write;
begin
  OpenRegistry;
  try
    regWriteBool(SRegValue_IDEOptUseTabCharacter, FUseTabChar);
    regWriteBool(SRegValue_IDEOptLinkFontSizes,   FLinkFontSizes);
    regWriteInt (SRegValue_IDEOptIndentSize,      FIndentSize);
    regWriteBool(SRegValue_IDEOptUseOldDebugger,  FUseOldDebugger);
    regWriteString(SRegValue_IDEOptEditorTheme,   FEditorTheme);

    regWriteBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, FDebuggerBreakWhenExitingLine);
    regWriteBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, FDebuggerSingleStepAfterBreak);
    regWriteBool(SRegValue_IDEOptDebuggerShowStoreOffset,      FDebuggerShowStoreOffset);
    regWriteBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, FDebuggerAutoRecompileWithDebugInfo);

    regWriteBool(SRegValue_IDEOptDebuggerAutoResetBeforeCompiling, FDebuggerAutoResetBeforeCompiling);
    regWriteBool(SRegValue_IDEOptAutoSaveBeforeCompiling, FAutoSaveBeforeCompiling);
    regWriteBool(SRegValue_IDEOptOSKAutoSaveBeforeImporting, FOSKAutoSaveBeforeImporting);
    regWriteBool(SRegValue_IDEOptPromptToUpgradeProjects, FPromptToUpgradeProjects);

    regWriteInt(SRegValue_IDEOptServerPort, FServerDefaultPort);
    regWriteBool(SRegValue_IDEOptServerKeepAlive, FServerKeepAlive);
    regWriteBool(SRegValue_IDEOptServerUseLocalAddresses, FServerUseLocalAddresses);

    regWriteString(SRegValue_IDEOptServerNgrokToken, FServerNgrokToken);
    regWriteString(SRegValue_IDEOptServerNgrokRegion, FServerNgrokRegion);
    regWriteBool(SRegValue_IDEOptServerUseNgrok, FServerUseNgrok);
    regWriteBool(SRegValue_IDEOptServerShowConsoleWindow, FServerServerShowConsoleWindow);


    regWriteBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, FCharMapDisableDatabaseLookups);
    regWriteBool(SRegValue_IDEOptCharMapAutoLookup,             FCharMapAutoLookup);

    regWriteBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  FOpenKeyboardFilesInSourceView);   // I4751

    regWriteString(SRegValue_IDEDisplayTheme, FDisplayTheme);   // I4796

    regWriteString(SRegValue_IDEOptExternalEditorPath,          FExternalEditorPath);
    regWriteString(SRegValue_IDEOptSMTPServer,                  FSMTPServer);   // I4506
    regWriteString(SRegValue_IDEOptTestEmailAddresses,          FTestEmailAddresses);   // I4506

    regWriteInt(SRegValue_IDEOpt_WebLadderLength, FFix183_LadderLength);

    regWriteString(SRegValue_IDEOpt_DefaultProjectPath, FDefaultProjectPath);

    // for consistency with Keyman.System.KeymanSentryClient, we need to use
    // reg.WriteInteger, as regWriteInt, which in the dim dark past started
    // to write integers as a REG_SZ type and that's far too messy to change now.
    reg.WriteInteger(SRegValue_AutomaticallyReportErrors, IfThen(FReportErrors, 1, 0));
    reg.WriteInteger(SRegValue_AutomaticallyReportUsage, IfThen(FReportUsage, 1, 0));
  finally
    CloseRegistry;
  end;

  WriteServerConfigurationJson;
end;

procedure TKeymanDeveloperOptions.WriteServerConfigurationJson;
var
  o: TJSONObject;
  s: TStringList;
  ss: TStringStream;
begin
  o := TJSONObject.Create;
  try
    o.AddPair('port', TJSONNumber.Create(FServerDefaultPort));
    o.AddPair('ngrokToken', FServerNgrokToken);
    o.AddPair('ngrokRegion', FServerNgrokRegion);
    o.AddPair('useNgrok', TJSONBool.Create(FServerUseNgrok));
    o.AddPair('ngrokVisible', TJSONBool.Create(FServerServerShowConsoleWindow));
    s := TStringList.Create;
    try
      PrettyPrintJSON(o, s, 2);
      ss := TStringStream.Create(s.Text, TEncoding.UTF8);
      try
        ss.SaveToFile(TKeymanDeveloperPaths.ServerDataPath + TKeymanDeveloperPaths.S_ServerConfigJson);
      finally
        ss.Free;
      end;
    finally
      s.Free;
    end;
    o.ToJSON
  finally
    o.Free;
  end;
end;

procedure TKeymanDeveloperOptions.regWriteBool(const nm: string; value: Boolean);
begin
  if value then reg.WriteString(nm, '1') else reg.WriteString(nm, '0');
end;

procedure TKeymanDeveloperOptions.regWriteInt(const nm: string; value: Integer);
begin
  reg.WriteString(nm, IntToStr(value));
end;

procedure TKeymanDeveloperOptions.regWriteString(const nm, value: string);
begin
  reg.WriteString(nm, value);
end;

function TKeymanDeveloperOptions.regReadBool(const nm: string; def: Boolean): Boolean;
begin
  Result := def;
  if not reg.ValueExists(nm) then Exit;
  try
    Result := reg.ReadString(nm) = '1';
  except
  end;
end;

function TKeymanDeveloperOptions.regReadInt(const nm: string; def: Integer): Integer;
begin
  Result := def;
  if not reg.ValueExists(nm) then Exit;
  try
    Result := StrToInt(reg.ReadString(nm));
  except
  end;
end;

function TKeymanDeveloperOptions.regReadString(const nm, def: string): string;
begin
  Result := def;
  if not reg.ValueExists(nm) then Exit;
  try
    Result := reg.ReadString(nm);
  except
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

initialization
finalization
  DestroyKeymanDeveloperOptions;
end.
