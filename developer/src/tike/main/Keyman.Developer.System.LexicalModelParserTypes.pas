unit Keyman.Developer.System.LexicalModelParserTypes;

interface

type
  TLexicalModelFormat = (lmfUnknown, lmfTrie10, lmfCustom10);
  TLexicalModelWordBreaker = (lmwbUnknown, lmwbDefault, lmwbAscii, lmwbCustom);

  TLexicalModelParserTypes = class sealed
  private
    const
      SLexicalModelFormats: array[TLexicalModelFormat] of string = ('?', 'trie-1.0', 'custom-1.0');
      SLexicalModelWordBreakers: array[TLexicalModelWordBreaker] of string = ('?', 'default', 'ascii', 'custom');
      LexicalModelFormats: set of TLexicalModelFormat = [lmfUnknown..lmfCustom10];
      LexicalModelWordBreakers: set of TLexicalModelWordBreaker = [lmwbUnknown..lmwbCustom];
  public
    class function FormatFromText(const Value: string): TLexicalModelFormat; static;
    class function FormatToText(const Value: TLexicalModelFormat): string; static;
    class function WordBreakerFromText(const Value: string): TLexicalModelWordBreaker; static;
    class function WordBreakerToText(const Value: TLexicalModelWordBreaker): string; static;
  end;

implementation

uses
  System.SysUtils;

class function TLexicalModelParserTypes.FormatFromText(const Value: string): TLexicalModelFormat;
begin
  for Result in LexicalModelFormats do
    if SLexicalModelFormats[Result].Equals(Value) then Exit;
  Result := lmfUnknown;
end;

class function TLexicalModelParserTypes.FormatToText(const Value: TLexicalModelFormat): string;
begin
  Result := SLexicalModelFormats[Value];
end;

class function TLexicalModelParserTypes.WordBreakerFromText(const Value: string): TLexicalModelWordBreaker;
begin
  for Result in LexicalModelWordBreakers do
    if SLexicalModelWordBreakers[Result].Equals(Value) then Exit;
  Result := lmwbUnknown;
end;

class function TLexicalModelParserTypes.WordBreakerToText(const Value: TLexicalModelWordBreaker): string;
begin
  Result := SLexicalModelWordBreakers[Value];
end;

end.
