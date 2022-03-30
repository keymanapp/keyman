unit Keyman.Developer.System.KMConvertParameters;

interface

uses
  UKeymanTargets;

type
  TKMConvertMode = (cmImportWindows, cmTemplate, cmLexicalModel);

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
  public
    procedure WriteUsage;
    function CheckParams: Boolean;

    property KLID: string read FKLID;
    property Destination: string read FDestination;
    property KeyboardID: string read FKeyboardID;
    property Name: string read FName;
    property Copyright: string read FCopyright;
    property FullCopyright: string read FFullCopyright;
    property Version: string read FVersion;
    property BCP47Tags: string read FBCP47Tags;
    property Author: string read FAuthor;
    property Targets: TKeymanTargets read FTargets;
    property Mode: TKMConvertMode read FMode;
    property NoLogo: Boolean read FNoLogo;

    property ModelIdAuthor: string read FModelIdAuthor;
    property ModelIdLanguage: string read FModelIdLanguage;
    property ModelIdUniq: string read FModelIdUniq;
  end;

implementation

uses
  System.SysUtils;

function TKMConvertParameters.CheckParams: Boolean;
var
  ModeString: string;
  i: Integer;
begin
  FDestination := '.';
  FCopyright := 'Copyright (C)';
  FFullCopyright := 'Copyright (C) '+FormatDateTime('yyyy', Now);
  FVersion := '1.0';
  FTargets := [ktAny];

  if ParamCount < 2 then
    Exit(False);

  ModeString := LowerCase(ParamStr(1));
  if ModeString = 'import-windows' then
    FMode := cmImportWindows
  else if ModeString = 'template' then
    FMode := cmTemplate
  else if ModeString = 'lexical-model' then
    FMode := cmLexicalModel
  else
    Exit(False);

  i := 2;
  while i <= ParamCount do
  begin
    if ParamStr(i) = '-nologo' then
    begin
      FNoLogo := True;
      Inc(i);
      Continue;
    end;
    if CheckParam(ParamStr(i), ParamStr(i+1)) then
    begin
      Inc(i, 2);
      Continue;
    end;

    writeln('Invalid parameter: '+ParamStr(i));
    Exit(False);
  end;

  Result := True;
end;

procedure TKMConvertParameters.WriteUsage;
begin
  writeln('kmconvert import-windows -klid <source-klid> [additional-options]');
  writeln('  Imports a Windows keyboard into a new Keyman keyboard project');
  writeln;
  writeln('kmconvert template -id <keyboard_id> [additional-options]');
  writeln('  Creates a basic keyboard project in the repository template format');
  writeln;
  writeln('kmconvert lexical-model -id-author <id-author> -id-language <id-language> -id-uniq <id-uniq> [additional-options]');
  writeln('  Creates a wordlist lexical model project in the repository template format');
  writeln;
  writeln('Parameters:');
  writeln('  -nologo                Don''t show the program description and copyright banner');
  writeln('  -klid <source-klid>    The KLID of the keyboard to import, per LoadKeyboardLayout');
  writeln('  -id <keyboard_id>      The id of the keyboard to create');
  writeln('                         (in `import-windows` mode, can be a format string)');
  writeln('  -o <destination>       The target folder to write the project into, defaults to "."');
  writeln('  -author <data>         Name of author of the keyboard/model, no default');
  writeln('  -name <data>           Name of the keyboard/model, e.g. "My First Keyboard", "%s Basic" ');
  writeln('                         (format strings are only valid in `import-windows` mode)');
  writeln('  -copyright <data>      Copyright string for the keyboard/model, defaults to "Copyright (C)"');
  writeln('  -full-copyright <data> Longer copyright string for the keyboard/model, defaults to "Copyright (C) yyyy"');
  writeln('  -version <data>        Version number of the keyboard/model, defaults to "1.0"');
  writeln('  -languages <data>      Space-separated list of BCP 47 tags, e.g. "en-US tpi-PG"');
  writeln('  -targets <data>        Space-separate list of targets, e.g. "linux windows mobile"');
  writeln;
  // Model parameters
  writeln('Note: model identifiers are constructed from params: <id-author>.<id-language>.<id-uniq>');
  writeln('  -id-author <data>      Identifier for author of model');
  writeln('  -id-language <data>    Single BCP 47 tag identifying primary language of model');
  writeln('  -id-uniq <data>        Unique name for the model');
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
  else if name = '-targets' then Result := SetTargets(value)
  else if name = '-id-author' then Result := SetModelIdAuthor(value)
  else if name = '-id-language' then Result := SetModelIdLanguage(value)
  else if name = '-id-uniq' then Result := SetModelIdUniq(value)
  else Result := False;
end;

function TKMConvertParameters.SetKLID(const value: string): Boolean;
begin
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
  FVersion := Value;
  Result := True;
end;

function TKMConvertParameters.SetBCP47Tags(const value: string): Boolean;
begin
  FBCP47Tags := Value;
  Result := True;
end;

function TKMConvertParameters.SetAuthor(const value: string): Boolean;
begin
  FAuthor := Value;
  Result := True;
end;

function TKMConvertParameters.SetTargets(const value: string): Boolean;
begin
  FTargets := StringToKeymanTargets(Value);
  Result := True;
end;

function TKMConvertParameters.SetModelIdAuthor(const value: string): Boolean;
begin
  FModelIdAuthor := Value;
  Result := True;
end;

function TKMConvertParameters.SetModelIdLanguage(const value: string): Boolean;
begin
  FModelIdLanguage := Value;
  Result := True;
end;

function TKMConvertParameters.SetModelIdUniq(const value: string): Boolean;
begin
  FModelIdUniq := Value;
  Result := True;
end;

end.
