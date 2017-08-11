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
    FShowStartupDialog: Boolean;
    //FShowStartupHelperDialog: Boolean;
    FCharMapAutoLookup: Boolean;
    FCharMapDisableDatabaseLookups: Boolean;
    FDebuggerAutoRecompileWithDebugInfo: Boolean;
    FAllowMultipleInstances: Boolean;
    FExternalEditorPath: WideString;
    FWebHostDefaultPort: Integer;   // I4021
    FSMTPServer: string;   // I4506
    FTestEmailAddresses: string;   // I4506
    FOpenKeyboardFilesInSourceView: Boolean;   // I4751
    FDisplayTheme: string;
    procedure CloseRegistry;
    procedure OpenRegistry;
    function regReadString(const nm, def: string): string;
    function regReadBool(const nm: string; def: Boolean): Boolean;
    function regReadInt(const nm: string; def: Integer): Integer;
    procedure regWriteString(const nm, value: string);
    procedure regWriteBool(const nm: string; value: Boolean);
    procedure regWriteInt(const nm: string; value: Integer);
  public
    procedure Read;
    procedure Write;
    property UseTabChar: Boolean read FUseTabChar write FUseTabChar;
    property IndentSize: Integer read FIndentSize write FIndentSize;
    property LinkFontSizes: Boolean read FLinkFontSizes write FLinkFontSizes;
    property UseOldDebugger: Boolean read FUseOldDebugger write FUseOldDebugger;

    property CharMapAutoLookup: Boolean read FCharMapAutoLookup write FCharMapAutoLookup;
    property CharMapDisableDatabaseLookups: Boolean read FCharMapDisableDatabaseLookups write FCharMapDisableDatabaseLookups;

    property DebuggerBreakWhenExitingLine: Boolean read FDebuggerBreakWhenExitingLine write FDebuggerBreakWhenExitingLine;
    property DebuggerSingleStepAfterBreak: Boolean read FDebuggerSingleStepAfterBreak write FDebuggerSingleStepAfterBreak;
    property DebuggerShowStoreOffset: Boolean read FDebuggerShowStoreOffset write FDebuggerShowStoreOffset;
    property DebuggerAutoRecompileWithDebugInfo: Boolean read FDebuggerAutoRecompileWithDebugInfo write FDebuggerAutoRecompileWithDebugInfo;

    property WebHostDefaultPort: Integer read FWebHostDefaultPort write FWebHostDefaultPort;   // I4021
    property ShowStartupDialog: Boolean read FShowStartupDialog write FShowStartupDialog;
    //property ShowStartupHelperDialog: Boolean read FShowStartupHelperDialog write FShowStartupHelperDialog;

    property AllowMultipleInstances: Boolean read FAllowMultipleInstances write FAllowMultipleInstances;

    property OpenKeyboardFilesInSourceView: Boolean read FOpenKeyboardFilesInSourceView write FOpenKeyboardFilesInSourceView;   // I4751

    property DisplayTheme: string read FDisplayTheme write FDisplayTheme;   // I4796

    property ExternalEditorPath: WideString read FExternalEditorPath write FExternalEditorPath;
    property SMTPServer: string read FSMTPServer write FSMTPServer;   // I4506
    property TestEmailAddresses: string read FTestEmailAddresses write FTestEmailAddresses;   // I4506
  end;

function FKeymanDeveloperOptions: TKeymanDeveloperOptions;
//procedure CreateKeymanDeveloperOptions;
//procedure DestroyKeymanDeveloperOptions;

implementation

uses
  OnlineConstants,
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
  reg.OpenKey(SRegKey_IDEOptions, True);
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

    FDebuggerBreakWhenExitingLine  := regReadBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, True);
    FDebuggerSingleStepAfterBreak  := regReadBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, False);
    FDebuggerShowStoreOffset       := regReadBool(SRegValue_IDEOptDebuggerShowStoreOffset,      False);
    FDebuggerAutoRecompileWithDebugInfo := regReadBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, False);

    FWebHostDefaultPort := regReadInt(SRegValue_IDEOptWebHostPort, 8008);
    FShowStartupDialog             := regReadBool(SRegValue_IDEOptShowStartupDialog,            True);

    FCharMapDisableDatabaseLookups := regReadBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, False);
    FCharMapAutoLookup             := regReadBool(SRegValue_IDEOptCharMapAutoLookup,             True);

    FAllowMultipleInstances := regReadBool(SRegValue_IDEOptMultipleInstances, False);

    FOpenKeyboardFilesInSourceView  := regReadBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  False);   // I4751

    if GetOS in [osWin10, osOther]
      then FDefaultDisplayTheme := 'Windows10'
      else FDefaultDisplayTheme := 'Sapphire Kamri';

    FDisplayTheme := regReadString(SRegValue_IDEDisplayTheme, FDefaultDisplayTheme);   // I4796

    FExternalEditorPath := regReadString(SRegValue_IDEOptExternalEditorPath, '');
    FSMTPServer := regReadString(SRegValue_IDEOptSMTPServer, '');   // I4506
    FTestEmailAddresses := regReadString(SRegValue_IDEOptTestEmailAddresses, '');   // I4506
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

    regWriteBool(SRegValue_IDEOptDebuggerBreakWhenExitingLine, FDebuggerBreakWhenExitingLine);
    regWriteBool(SRegValue_IDEOptDebuggerSingleStepAfterBreak, FDebuggerSingleStepAfterBreak);
    regWriteBool(SRegValue_IDEOptDebuggerShowStoreOffset,      FDebuggerShowStoreOffset);
    regWriteBool(SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo, FDebuggerAutoRecompileWithDebugInfo);

    regWriteInt(SRegValue_IDEOptWebHostPort, FWebHostDefaultPort);

    regWriteBool(SRegValue_IDEOptShowStartupDialog,            FShowStartupDialog);
    //regWriteBool(SRegValue_IDEOptShowStartupHelperDialog,      FShowStartupHelperDialog);

    regWriteBool(SRegValue_IDEOptCharMapDisableDatabaseLookups, FCharMapDisableDatabaseLookups);
    regWriteBool(SRegValue_IDEOptCharMapAutoLookup,             FCharMapAutoLookup);

    regWriteBool(SRegValue_IDEOptMultipleInstances,             FAllowMultipleInstances);

    regWriteBool(SRegValue_IDEOptOpenKeyboardFilesInSourceView,  FOpenKeyboardFilesInSourceView);   // I4751

    regWriteString(SRegValue_IDEDisplayTheme, FDisplayTheme);   // I4796

    regWriteString(SRegValue_IDEOptExternalEditorPath,          FExternalEditorPath);
    regWriteString(SRegValue_IDEOptSMTPServer,                  FSMTPServer);   // I4506
    regWriteString(SRegValue_IDEOptTestEmailAddresses,          FTestEmailAddresses);   // I4506
  finally
    CloseRegistry;
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

initialization
finalization
  DestroyKeymanDeveloperOptions;
end.
