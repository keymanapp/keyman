unit Keyman.Developer.System.LexicalModelParser;

interface

uses
  System.Classes,
  System.RegularExpressions,

  Keyman.Developer.System.LexicalModelParserTypes;

type
  TLexicalModelParser = class
  private
    FText: TStrings;
    FModified: Boolean;
    FComment: string;
    FFormat: TLexicalModelFormat;
    FWordlists: TStrings;
    FWordBreaker: TLexicalModelWordBreaker;
    FIsEditable: Boolean;
    function GetText: string;
    procedure PrepareText;
    procedure SetComment(const Value: string);
    procedure SetFormat(const Value: TLexicalModelFormat);
    procedure SetWordBreaker(const Value: TLexicalModelWordBreaker);
    procedure Parse;
    procedure Modify;
    function ReplaceFormat(const m: TMatch): string;
    function ReplaceWordBreaker(const m: TMatch): string;
    function ReplaceSources(const m: TMatch): string;
    function ReplaceComment(const m: TMatch): string;
  public
    constructor Create(Source: string);
    destructor Destroy; override;
    property Text: string read GetText;
    property Format: TLexicalModelFormat read FFormat write SetFormat;
    property WordBreaker: TLexicalModelWordBreaker read FWordBreaker write SetWordBreaker;
    property Wordlists: TStrings read FWordlists;
    property Comment: string read FComment write SetComment;
    property IsEditable: Boolean read FIsEditable;
  end;

implementation

uses
  System.SysUtils;

{ TLexicalModelParser }

const
  SComment = '^\s*\/\*(.+?)\*\/';
  SFormat = 'format\s*:\s*([''"])(.+?)(\1)';
  SWordBreaker = 'wordBreaker\s*:\s*([''"])(.+?)(\1)';
  SSources = 'sources\s*:\s*\[(.+?)\]';
  SSource = '\s*([''"])(.+?)(\1)\s*(,?)';

constructor TLexicalModelParser.Create(Source: string);
begin
  inherited Create;
  FWordlists := TStringList.Create;
  FText := TStringList.Create;
  FText.Text := Source;
  FModified := False;
  Parse;
end;

destructor TLexicalModelParser.Destroy;
begin
  FreeAndNil(FWordlists);
  FreeAndNil(FText);
  inherited Destroy;
end;

function TLexicalModelParser.GetText: string;
begin
  PrepareText;
  Result := FText.Text;
end;

procedure TLexicalModelParser.Modify;
begin
  FModified := True;
end;

procedure TLexicalModelParser.Parse;
var
  re: TRegEx;
  m: TMatch;
  s: string;
begin
  // Long term, we should probably consider using the TypeScript compiler
  // to real-time generate an AST and work from that. But that introduces a
  // layer of glue to the Delphi code that I don't want to get into just now.
  FIsEditable := True;
  FFormat := lmfUnknown;
  FWordBreaker := lmwbUnknown;
  FComment := '';

  s := FText.Text;

  // /* Comment */
  m := TRegEx.Match(s, SComment, [roSingleLine]);
  if m.Success then
    FComment := m.Groups[1].Value;

  // format:
  m := TRegEx.Match(s, SFormat, [roMultiLine]);
  if m.Success then
    FFormat := TLexicalModelParserTypes.FormatFromText(m.Groups[2].Value);

  // wordBreaker:
  m := TRegEx.Match(s, SWordBreaker, [roMultiLine]);
  if m.Success then
    FWordBreaker := TLexicalModelParserTypes.WordBreakerFromText(m.Groups[2].Value);

  // sources:
  FWordlists.Clear;
  m := TRegEx.Match(s, SSources, [roMultiLine]);
  if m.Success then
  begin
    re := TRegEx.Create(SSource, [roMultiLine]);
    m := re.Match(m.Groups[1].Value);
    while m.Success do
    begin
      FWordlists.Add(m.Groups[2].Value);
      m := m.NextMatch;
    end;
  end
  else
    FIsEditable := False;

  FIsEditable := FIsEditable and (FFormat <> lmfUnknown) and (FWordBreaker <> lmwbUnknown);
end;

function TLexicalModelParser.ReplaceComment(const m: TMatch): string;
begin
  if FComment = ''
    then Result := ''
    else Result := '/*'+FComment+'*/';
end;

function TLexicalModelParser.ReplaceFormat(const m: TMatch): string;
begin
  Result := m.Value.Substring(0, m.Groups[2].Index - m.Index) +
    TLexicalModelParserTypes.FormatToText(FFormat) +
    m.Value.Substring(m.Groups[2].Index - m.Index + m.Groups[2].Length);
end;

function TLexicalModelParser.ReplaceWordBreaker(const m: TMatch): string;
begin
  Result := m.Value.Substring(0, m.Groups[2].Index - m.Index) +
    TLexicalModelParserTypes.WordBreakerToText(FWordBreaker) +
    m.Value.Substring(m.Groups[2].Index - m.Index + m.Groups[2].Length);
end;

function TLexicalModelParser.ReplaceSources(const m: TMatch): string;
var
  sources: string;
  source: string;
begin
  sources := '';
  for source in FWordlists do
  begin
    if sources <> '' then
      sources := sources + ', ';
    sources := sources + '''' + source + ''''; // assuming no quote characters
  end;

  Result := m.Value.Substring(0, m.Groups[1].Index - m.Index) +
    sources +
    m.Value.Substring(m.Groups[1].Index - m.Index + m.Groups[1].Length);
end;

procedure TLexicalModelParser.PrepareText;
var
  s: string;
begin
  if not FModified then Exit;

  s := FText.Text;

  // Replace tokens
  s := TRegEx.Replace(s, SFormat, ReplaceFormat, [roMultiLine]);
  s := TRegEx.Replace(s, SWordBreaker, ReplaceWordBreaker, [roMultiLine]);
  s := TRegEx.Replace(s, SSources, ReplaceSources, [roMultiLine]);

  // Replace, remove or add comment
  if TRegEx.IsMatch(s, SComment, [roSingleLine]) then
    s := TRegEx.Replace(s, SComment, ReplaceComment, [roSingleLine])
  else if FComment <> '' then
    s := '/*'+FComment+'*/'#13#10+s;


  FText.Text := s;
end;

procedure TLexicalModelParser.SetComment(const Value: string);
begin
  Assert(FIsEditable);
  if FComment <> Value then
  begin
    FComment := Value;
    Modify;
  end;
end;

procedure TLexicalModelParser.SetFormat(const Value: TLexicalModelFormat);
begin
  Assert(FIsEditable);
  if FFormat <> Value then
  begin
    FFormat := Value;
    Modify;
  end;
end;

procedure TLexicalModelParser.SetWordBreaker(const Value: TLexicalModelWordBreaker);
begin
  Assert(FIsEditable);
  if FWordBreaker <> Value then
  begin
    FWordBreaker := Value;
    Modify;
  end;
end;

end.
