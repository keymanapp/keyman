unit Keyman.Developer.System.KMConvertParameters;

interface

uses
  UKeymanTargets;

type
  TKMConvertMode = (cmImportWindows, cmTemplate, cmLexicalModel);

  TKMConvertParameters = record
    KLID: string;
    Destination: string;
    KeyboardID: string;
    Name: string;
    Copyright: string;
    FullCopyright: string;
    Version: string;
    BCP47Tags: string;
    Author: string;
    Targets: TKeymanTargets;
    Mode: TKMConvertMode;
    NoLogo: Boolean;

    ModelIdAuthor, ModelIdLanguage, ModelIdUniq: string;
  private
    function CheckParam(name, value: string): Boolean;
  public
    procedure WriteUsage;
    function CheckParams: Boolean;
  end;

implementation

uses
  System.SysUtils;

function TKMConvertParameters.CheckParams: Boolean;
var
  ModeString: string;
  i: Integer;
begin
  Destination := '.';
  Copyright := 'Copyright (C)';
  FullCopyright := 'Copyright (C) '+FormatDateTime('yyyy', Now);
  Version := '1.0';
  Targets := [ktAny];

  if ParamCount < 2 then
    Exit(False);

  ModeString := LowerCase(ParamStr(1));
  if ModeString = 'import-windows' then
    Mode := cmImportWindows
  else if ModeString = 'template' then
    Mode := cmTemplate
  else if ModeString = 'lexical-model' then
    Mode := cmLexicalModel
  else
    Exit(False);

  i := 2;
  while i < ParamCount do
  begin
    if ParamStr(i) = '-nologo' then
    begin
      NoLogo := True;
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
  writeln('  -nologo               Don''t show the program description and copyright banner');
  writeln('  -klid <source-klid>   The KLID of the keyboard to import, per LoadKeyboardLayout');
  writeln('  -id <keyboard_id>     The id of the keyboard to create');
  writeln('                        (in `import-windows` mode, can be a format string)');
  writeln('  -o <destination>      The target folder to write the project into, defaults to "."');
  writeln('  -author <data>        Name of author of the keyboard/model, no default');
  writeln('  -name <data>          Name of the keyboard/model, e.g. "My First Keyboard", "%s Basic" ');
  writeln('                        (format strings are only valid in `import-windows` mode)');
  writeln('  -copyright <data>     Copyright string for the keyboard/model, defaults to "Copyright (C)"');
  writeln('  -fullcopyright <data> Longer copyright string for the keyboard/model, defaults to "Copyright (C) yyyy"');
  writeln('  -version <data>       Version number of the keyboard/model, defaults to "1.0"');
  writeln('  -languages <data>     Space-separated list of BCP 47 tags, e.g. "en-US tpi-PG"');
  // Model parameters
  writeln('Note: model identifiers are constructed from params: <id-author>.<id-language>.<id-uniq>');
  writeln('  -id-author <data>     Identifier for author of model');
  writeln('  -id-language <data>   Single BCP 47 tag identifying primary language of model');
  writeln('  -id-uniq <data>       Unique name for the model');
end;

{ TKMConvertParameters }

function TKMConvertParameters.CheckParam(name, value: string): Boolean;
begin
  if name = '-klid' then KLID := value
  else if name = '-o' then Destination := value
  else if name = '-id' then KeyboardID := value
  else if name = '-name' then Self.Name := value
  else if name = '-copyright' then Copyright := value
  else if name = '-full-copyright' then FullCopyright := value
  else if name = '-version' then Version := value
  else if name = '-languages' then BCP47Tags := value
  else if name = '-author' then Author := value
  else if name = '-targets' then Targets := StringToKeymanTargets(value)
  else if name = '-id-author' then ModelIdAuthor := value
  else if name = '-id-language' then ModelIdLanguage := value
  else if name = '-id-uniq' then ModelIdUniq := value

  else Exit(False);
  Result := True;
end;

end.
