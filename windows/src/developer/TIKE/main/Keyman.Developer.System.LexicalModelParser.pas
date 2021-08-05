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
    FInsertAfterWord: string;
    FIsRTL: Boolean;
    FOpenQuote: string;
    FCloseQuote: string;
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
    procedure WordlistsChange(Sender: TObject);
    function ReplaceInsertAfterWord(const punctuation: string): string;
    function ReplaceIsRTL(const punctuation: string): string;
    function ReplaceQuotesForKeepSuggestion(const punctuation: string): string;
    procedure SetCloseQuote(const Value: string);
    procedure SetInsertAfterWord(const Value: string);
    procedure SetIsRTL(const Value: Boolean);
    procedure SetOpenQuote(const Value: string);

    function JSONEncodeString(const s: string): string;
    function JSONDecodeString(s: string): string;
  public
    constructor Create(Source: string);
    destructor Destroy; override;
    property Text: string read GetText;
    property Format: TLexicalModelFormat read FFormat write SetFormat;
    property WordBreaker: TLexicalModelWordBreaker read FWordBreaker write SetWordBreaker;
    property Wordlists: TStrings read FWordlists;
    property Comment: string read FComment write SetComment;
    property IsEditable: Boolean read FIsEditable;
    property IsRTL: Boolean read FIsRTL write SetIsRTL;
    property OpenQuote: string read FOpenQuote write SetOpenQuote;
    property CloseQuote: string read FCloseQuote write SetCloseQuote;
    property InsertAfterWord: string read FInsertAfterWord write SetInsertAfterWord;
  public
    // These defaults are defined in common/lexical-model-types/index.d.ts
    const
      CDefaultInsertAfterWord = ' ';
      CDefaultIsRTL = False;
      CDefaultOpenQuote = Char($201c);
      CDefaultCloseQuote = Char($201d);
  end;

implementation

uses
  System.JSON,
  System.SysUtils;

{ TLexicalModelParser }

const
  SComment = '^\s*\/\*(.+?)\*\/';
  SFormat = 'format\s*:\s*([''"])(.+?)(\1)';
  SWordBreaker = 'wordBreaker\s*:\s*([''"])(.+?)(\1)';
  SSources = 'sources\s*:\s*\[(.+?)\]';
  SSource = '\s*([''"])(.+?)(\1)\s*(,?)';

  SEndOfObject = '\s*};';
  SPunctuationID = 'punctuation';
  SPunctuation =
    ',?\s*'+SPunctuationID+'\s*:\s*{\s*(('+
      '(insertAfterWord\s*:\s*(([''"]).*?(\5)))|'+
      '(isRTL\s*:\s*(true|false))|'+
      '(quotesForKeepSuggestion\s*:\s*{\s*'+
        'open\s*:\s*(([''"]).*?(\11))\s*,\s*'+
        'close\s*:\s*(([''"]).*?(\14))\s*})'+
      '),?\s*)*}';

  // These may delete the entry so we need to take the `,` into account
  SInsertAfterWordID = 'insertAfterWord';
  SInsertAfterWord = ',?\s*'+SInsertAfterWordID+'\s*:\s*(([''"]).*?(\2))';

  SIsRTLID = 'isRTL';
  SIsRTL = ',?\s*'+SIsRTLID+'\s*:\s*(true|false)';

  SQuotesForKeepSuggestionID = 'quotesForKeepSuggestion';
  SQuotesForKeepSuggestion =
    ',?\s*'+SQuotesForKeepSuggestionID+'\s*:\s*{\s*'+
      'open\s*:\s*(([''"]).*?(\2))\s*,\s*'+
      'close\s*:\s*(([''"]).*?(\5))\s*'+
    '}';

constructor TLexicalModelParser.Create(Source: string);
begin
  inherited Create;
  FWordlists := TStringList.Create;
  (FWordlists as TStringList).OnChange := WordlistsChange;
  FText := TStringList.Create;
  FText.Text := Source;
  Parse;
  FModified := False;
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

function TLexicalModelParser.JSONDecodeString(s: string): string;
var
  v: TJSONValue;
begin
  if s[1] = '''' then
  begin
    // Javascript can have ' but JSON only "
    s[1] := '"';
    s[Length(s)] := '"';
  end;
  v := TJSONObject.ParseJSONValue(s);
  try
    if not v.TryGetValue<string>(Result) then
      Result := '';
  finally
    v.Free;
  end;
end;

function TLexicalModelParser.JSONEncodeString(const s: string): string;
var
  j: TJSONString;
begin
  j := TJSONString.Create(s);
  try
    Result := j.ToJSON;
  finally
    j.Free;
  end;
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

  FInsertAfterWord := CDefaultInsertAfterWord;
  FIsRTL := CDefaultIsRTL;
  FOpenQuote := CDefaultOpenQuote;
  FCloseQuote := CDefaultCloseQuote;

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

  // insertAfterWord:
  m := TRegEx.Match(s, SInsertAfterWord, [roMultiLine]);
  if m.Success then
    FInsertAfterWord := JSONDecodeString(m.Groups[1].Value)
  else if TRegEx.Match(s, SInsertAfterWordID, [roMultiLine]).Success then
    FIsEditable := False;


  // isRTL: true
  m := TRegEx.Match(s, SIsRTL, [roMultiLine]);
  if m.Success then
    FIsRTL := m.Groups[1].Value = 'true'
  else if TRegEx.Match(s, SIsRTLID, [roMultiLine]).Success then
    FIsEditable := False;

  // quotesForKeepSuggestion: We assume that both open and close are defined,
  // in that order.
  m := TRegEx.Match(s, SQuotesForKeepSuggestion, [roMultiLine]);
  if m.Success then
  begin
    FOpenQuote := JSONDecodeString(m.Groups[1].Value);
    FCloseQuote := JSONDecodeString(m.Groups[4].Value);
  end
  else if TRegEx.Match(s, SQuotesForKeepSuggestionID, [roMultiLine]).Success then
    FIsEditable := False;

  if TRegEx.IsMatch(s, SPunctuationID) and not TRegEx.IsMatch(s, SPunctuation) then
    FIsEditable := False;


  FIsEditable := FIsEditable and
    TRegEx.IsMatch(s, SEndOfObject) and
    (FFormat <> lmfUnknown) and
    (FWordBreaker <> lmwbUnknown);
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

function TLexicalModelParser.ReplaceInsertAfterWord(const punctuation: string): string;
begin
  if punctuation = ''
    then Result := ''
    else Result := punctuation + ','#13#10;

  Result := Result +
    '    insertAfterWord: '+JSONEncodeString(FInsertAfterWord);
end;

function TLexicalModelParser.ReplaceIsRTL(const punctuation: string): string;
begin
  if punctuation = ''
    then Result := ''
    else Result := punctuation + ','#13#10;

  Result := Result +
    '    isRTL: true';
end;

function TLexicalModelParser.ReplaceQuotesForKeepSuggestion(const punctuation: string): string;
begin
  if punctuation = ''
    then Result := ''
    else Result := punctuation + ','#13#10;

  Result := Result +
    '    quotesForKeepSuggestion: {'#13#10+
    '      open: '+JSONEncodeString(FOpenQuote)+','#13#10+
    '      close: '+JSONEncodeString(FCloseQuote)+#13#10+
    '    }';
end;

procedure TLexicalModelParser.PrepareText;
var
  punctuation, s: string;
  m: TMatch;
begin
  if not FModified then Exit;

  s := FText.Text;

  // Replace tokens
  s := TRegEx.Replace(s, SFormat, ReplaceFormat, [roMultiLine]);
  s := TRegEx.Replace(s, SWordBreaker, ReplaceWordBreaker, [roMultiLine]);
  s := TRegEx.Replace(s, SSources, ReplaceSources, [roMultiLine]);

  s := TRegEx.Replace(s, SPunctuation, '', [roMultiLine]);

  punctuation := '';

  if FInsertAfterWord <> CDefaultInsertAfterWord then
    punctuation := ReplaceInsertAfterWord(punctuation);

  if FIsRTL <> CDefaultIsRTL then
    punctuation := ReplaceIsRTL(punctuation);

  if (FOpenQuote <> CDefaultOpenQuote) or (FCloseQuote <> CDefaultCloseQuote) then
    punctuation := ReplaceQuotesForKeepSuggestion(punctuation);

  if punctuation <> '' then
  begin
    punctuation :=
      ','#13#10+
      '  punctuation: {'+#13#10+
           punctuation + #13#10+
      '  }';
    m := TRegEx.Match(s, SEndOfObject, [roMultiLine]);
    if m.Success then
      s := s.Substring(0, m.Index - 1) + punctuation + s.Substring(m.Index);
  end;

  // Replace, remove or add comment
  if TRegEx.IsMatch(s, SComment, [roSingleLine]) then
    s := TRegEx.Replace(s, SComment, ReplaceComment, [roSingleLine])
  else if FComment <> '' then
    s := '/*'+FComment+'*/'#13#10+s;


  FText.Text := s;
end;

procedure TLexicalModelParser.SetCloseQuote(const Value: string);
begin
  Assert(FIsEditable);
  if FCloseQuote <> Value then
  begin
    FCloseQuote := Value;
    Modify;
  end;
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

procedure TLexicalModelParser.SetInsertAfterWord(const Value: string);
begin
  Assert(FIsEditable);
  if FInsertAfterWord <> Value then
  begin
    FInsertAfterWord := Value;
    Modify;
  end;
end;

procedure TLexicalModelParser.SetIsRTL(const Value: Boolean);
begin
  Assert(FIsEditable);
  if FIsRTL <> Value then
  begin
    FIsRTL := Value;
    Modify;
  end;
end;

procedure TLexicalModelParser.SetOpenQuote(const Value: string);
begin
  Assert(FIsEditable);
  if FOpenQuote <> Value then
  begin
    FOpenQuote := Value;
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

procedure TLexicalModelParser.WordlistsChange(Sender: TObject);
begin
  Modify;
end;

end.
