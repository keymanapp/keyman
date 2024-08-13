unit Keyman.Developer.System.KMConvertParameters;

interface

uses
  UKeymanTargets;

type
  TKMConvertMode = (cmImportWindows, cmTemplate, cmLexicalModel, cmLdmlKeyboard);

  TKMConvertParameters = record
  private
    FKLID: string;
    FDestination: string;
    FKeyboardID: string;
    FName: string;
    FCopyright: string;
    FFullCopyright: string;
    FVersion: string;
    FBCP47Tags: string;
    FAuthor: string;
    FTargets: TKeymanTargets;
    FMode: TKMConvertMode;
    FNoLogo: Boolean;
    FModelIdAuthor: string;
    FModelIdLanguage: string;
    FModelIdUniq: string;
    FEmitUsage: Boolean;
    FDescription: string;

    function CheckParam(name, value: string): Boolean;
    function SetKLID(const value: string): Boolean;
    function SetAuthor(const value: string): Boolean;
    function SetBCP47Tags(const value: string): Boolean;
    function SetCopyright(const value: string): Boolean;
    function SetDestination(const value: string): Boolean;
    function SetFullCopyright(const value: string): Boolean;
    function SetKeyboardID(const value: string): Boolean;
    function SetModelIdAuthor(const value: string): Boolean;
    function SetModelIdLanguage(const value: string): Boolean;
    function SetModelIdUniq(const value: string): Boolean;
    function SetName(const value: string): Boolean;
    function SetTargets(const value: string): Boolean;
    function SetVersion(const value: string): Boolean;
    function IsValidModelComponent(const component, value: string): Boolean;
    function ValidateBCP47Tag(const component, tag: string): Boolean;
    procedure OutputText(const msg: string = '');
    function SetDescription(const value: string): Boolean;
  public
    type TOutputTextProc = reference to procedure(msg: string);
    var OnOutputText: TOutputTextProc;

    procedure WriteUsage;
    function CheckParams(Params: TArray<string>): Boolean;

    property EmitUsage: Boolean read FEmitUsage;

    property KLID: string read FKLID;
    property Destination: string read FDestination;
    property KeyboardID: string read FKeyboardID;
    property Name: string read FName;
    property Copyright: string read FCopyright;
    property FullCopyright: string read FFullCopyright;
    property Version: string read FVersion;
    property BCP47Tags: string read FBCP47Tags;
    property Author: string read FAuthor;
    property Description: string read FDescription;
    property Targets: TKeymanTargets read FTargets;
    property Mode: TKMConvertMode read FMode;
    property NoLogo: Boolean read FNoLogo;

    property ModelIdAuthor: string read FModelIdAuthor;
    property ModelIdLanguage: string read FModelIdLanguage;
    property ModelIdUniq: string read FModelIdUniq;
  end;

implementation

uses
  System.SysUtils,

  BCP47Tag,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.KeyboardUtils,
  Keyman.System.LanguageCodeUtils,
  Keyman.System.LexicalModelUtils;

function TKMConvertParameters.CheckParams(Params: TArray<string>): Boolean;
var
  ModeString: string;
  i: Integer;
begin
  FEmitUsage := False;

  FDestination := '.';
  FCopyright := 'Copyright '+Char($00A9 {copyright});
  FFullCopyright := 'Copyright '+Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now);
  FVersion := '1.0';
  FTargets := [ktAny];

  if Length(Params) < 1 then
  begin
    FEmitUsage := True;
    Exit(False);
  end;

  if SameText(Params[0], '-h') or
      SameText(Params[0], '-help') or
      SameText(Params[0], '-?') then
  begin
    FEmitUsage := True;
    Exit(False);
  end;

  ModeString := Params[0].ToLower;
  if ModeString = 'import-windows' then
    FMode := cmImportWindows
  else if ModeString = 'template' then
    FMode := cmTemplate
  else if ModeString = 'lexical-model' then
    FMode := cmLexicalModel
  else if ModeString = 'ldml-keyboard' then
    FMode := cmLdmlKeyboard
  else
  begin
    FEmitUsage := True;
    Exit(False);
  end;

  i := 1;
  while i < Length(Params) do
  begin
    if Params[i] = '-nologo' then
    begin
      FNoLogo := True;
      Inc(i);
      Continue;
    end;
    if CheckParam(Params[i], Params[i+1]) then
    begin
      Inc(i, 2);
      Continue;
    end;

    Exit(False);
  end;

  Result := True;
end;

procedure TKMConvertParameters.WriteUsage;
begin
  OutputText('kmconvert import-windows -klid <source-klid> [additional-options]');
  OutputText('  Imports a Windows keyboard into a new Keyman keyboard project');
  OutputText;
  OutputText('kmconvert template -id <keyboard_id> [additional-options]');
  OutputText('  Creates a basic keyboard project in the repository template format');
  OutputText;
  OutputText('kmconvert ldml-keyboard -id <keyboard_id> [additional-options]');
  OutputText('  Creates an LDML keyboard project in the repository template format');
  OutputText;
  OutputText('kmconvert lexical-model -id-author <id-author> -id-language <id-language> -id-uniq <id-uniq> [additional-options]');
  OutputText('  Creates a wordlist lexical model project in the repository template format');
  OutputText;
  OutputText('Parameters:');
  OutputText('  -nologo                Don''t show the program description and copyright banner');
  OutputText('  -klid <source-klid>    The KLID of the keyboard to import, per LoadKeyboardLayout');
  OutputText('  -id <keyboard_id>      The id of the keyboard to create');
  OutputText('                         (in `import-windows` mode, can be a format string)');
  OutputText('  -o <destination>       The target folder to write the project into, defaults to "."');
  OutputText('  -author <data>         Name of author of the keyboard/model, no default');
  OutputText('  -description <data>    Short plain-text description of the keyboard/model, no default');
  OutputText('  -name <data>           Name of the keyboard/model, e.g. "My First Keyboard", "%s Basic" ');
  OutputText('                         (format strings are only valid in `import-windows` mode)');
  OutputText('  -copyright <data>      Copyright string for the keyboard/model, defaults to "Copyright (C)"');
  OutputText('  -full-copyright <data> Longer copyright string for the keyboard/model, defaults to "Copyright (C) yyyy"');
  OutputText('  -version <data>        Version number of the keyboard/model, defaults to "1.0"');
  OutputText('  -languages <data>      Space-separated list of BCP 47 tags, e.g. "en-US tpi-PG"');
  OutputText('  -targets <data>        Space-separate list of targets, e.g. "linux windows mobile"');
  OutputText;
  // Model parameters
  OutputText('Note: model identifiers are constructed from params: <id-author>.<id-language>.<id-uniq>');
  OutputText('  -id-author <data>      Identifier for author of model');
  OutputText('  -id-language <data>    Single BCP 47 tag identifying primary language of model');
  OutputText('  -id-uniq <data>        Unique name for the model');
end;

{ TKMConvertParameters }

function TKMConvertParameters.CheckParam(name, value: string): Boolean;
begin
  if name = '-klid' then Result := SetKLID(value)
  else if name = '-o' then Result := SetDestination(value)
  else if name = '-id' then Result := SetKeyboardID(value)
  else if name = '-name' then Result := SetName(value)
  else if name = '-copyright' then Result := SetCopyright(value)
  else if name = '-full-copyright' then Result := SetFullCopyright(value)
  else if name = '-version' then Result := SetVersion(value)
  else if name = '-languages' then Result := SetBCP47Tags(value)
  else if name = '-author' then Result := SetAuthor(value)
  else if name = '-description' then Result := SetDescription(value)
  else if name = '-targets' then Result := SetTargets(value)
  else if name = '-id-author' then Result := SetModelIdAuthor(value)
  else if name = '-id-language' then Result := SetModelIdLanguage(value)
  else if name = '-id-uniq' then Result := SetModelIdUniq(value)
  else
  begin
    OutputText('Invalid parameter: '+name);
    Result := False;
  end;
end;

function TKMConvertParameters.SetKLID(const value: string): Boolean;
var
  x: Integer;
begin
  if not TryStrToInt('$'+value, x) then
  begin
    OutputText('Invalid -klid value "'+value+'": must be a hexadecimal value');
    Exit(False);
  end;
  FKLID := Value;
  Result := True;
end;

function TKMConvertParameters.SetDestination(const value: string): Boolean;
begin
  FDestination := Value;
  Result := True;
end;

function TKMConvertParameters.SetKeyboardID(const value: string): Boolean;
begin
  if not TKeyboardUtils.IsValidKeyboardID(value, True) then
  begin
    OutputText('Invalid -id value "'+value+'": must be a clean keyboard id (a-z, 0-9, and _ characters only)');
    OutputText('Suggested value: "'+TKeyboardUtils.CleanKeyboardID(value)+'"');
    Exit(False);
  end;
  FKeyboardID := Value;
  Result := True;
end;

function TKMConvertParameters.SetName(const value: string): Boolean;
begin
  FName := Value;
  Result := True;
end;

function TKMConvertParameters.SetCopyright(const value: string): Boolean;
begin
  FCopyright := Value;
  Result := True;
end;

function TKMConvertParameters.SetFullCopyright(const value: string): Boolean;
begin
  FFullCopyright := Value;
  Result := True;
end;

function TKMConvertParameters.SetVersion(const value: string): Boolean;
begin
  if not TKeyboardUtils.IsValidVersionString(value) then
  begin
    OutputText('Invalid -version value "'+value+'": must be a version string, e.g. "1.0.2"');
    Exit(False);
  end;
  FVersion := Value;
  Result := True;
end;

function TKMConvertParameters.ValidateBCP47Tag(const component, tag: string): Boolean;
var
  b: TBCP47Tag;
  msg: string;
begin
  b := TBCP47Tag.Create(tag);
  try
    if not b.IsValid(True) then
    begin
      OutputText('Invalid '+component+' value: "'+tag+'" is not a valid BCP-47 tag');
      Exit(False);
    end;
  finally
    b.Free;
  end;

  if not TCanonicalLanguageCodeUtils.IsCanonical(tag, msg, False, False) then
  begin
    // Just a warning, because it's not illegal, just generates a warning
    // kmc-side
    OutputText('Warning: '+component+': '+msg);
  end;

  Result := True;
end;

function TKMConvertParameters.SetBCP47Tags(const value: string): Boolean;
var
  tags: TArray<string>;
  tag: string;
begin
  tags := value.Split([' ']);
  if Length(Tags) = 0 then
  begin
    OutputText('Invalid -languages value "'+value+'": must be at least one tag');
    Exit(False);
  end;

  for tag in tags do
  begin
    if not ValidateBCP47Tag('-languages', tag) then
      Exit(False);
  end;

  FBCP47Tags := Value;
  Result := True;
end;

function TKMConvertParameters.SetAuthor(const value: string): Boolean;
begin
  FAuthor := Value;
  Result := True;
end;

function TKMConvertParameters.SetDescription(const value: string): Boolean;
begin
  FDescription := Value;
  Result := True;
end;

function TKMConvertParameters.SetTargets(const value: string): Boolean;
begin
  FTargets := StringToKeymanTargets(Value);
  if FTargets = [] then
  begin
    OutputText('Invalid -targets value "'+value+'": no valid targets found');
    Exit(False);
  end;
  Result := True;
end;

function TKMConvertParameters.IsValidModelComponent(const component, value: string): Boolean;
begin
  Result := TLexicalModelUtils.IsCleanLexicalModelIDComponent(value);
  if not Result then
  begin
    OutputText('Invalid '+component+' value "'+value+'"');
    OutputText('Suggested value: "'+TLexicalModelUtils.CleanLexicalModelIDComponent(value)+'"');
  end;
end;

procedure TKMConvertParameters.OutputText(const msg: string);
begin
  if Assigned(OnOutputText) then
    OnOutputText(msg)
  else
    writeln(msg);
end;

function TKMConvertParameters.SetModelIdAuthor(const value: string): Boolean;
begin
  if not IsValidModelComponent('-id-author', value) then
    Exit(False);

  FModelIdAuthor := Value;
  Result := True;
end;

function TKMConvertParameters.SetModelIdLanguage(const value: string): Boolean;
begin
  if not ValidateBCP47Tag('-id-language', value) then
    Exit(False);

  FModelIdLanguage := Value;
  Result := True;
end;

function TKMConvertParameters.SetModelIdUniq(const value: string): Boolean;
begin
  if not IsValidModelComponent('-id-uniq', value) then
    Exit(False);

  FModelIdUniq := Value;
  Result := True;
end;

end.
