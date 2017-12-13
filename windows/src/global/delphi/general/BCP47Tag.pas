// Adapted from https://github.com/gagle/node-bcp47 [MIT]
// TODO: BCP47: Does not do full canonicalization
unit BCP47Tag;

interface

uses
  System.SysUtils;

type
  EBCP47Tag = class(Exception);

  TBCP47Tag = class
  private
    FLanguage: string;
    FPrivateUse: string;
    FVariant: string;
    FExtension: string;
    FScript: string;
    FGrandfathered: string;
    FRegion: string;
    FExtLang: string;
    FLangTag_PrivateUse: string;
    FOriginalTag: string;
    procedure SetExtension(const Value: string);
    procedure SetExtLang(const Value: string);
    procedure SetGrandfathered(const Value: string);
    procedure SetLanguage(const Value: string);
    procedure SetPrivateUse(const Value: string);
    procedure SetRegion(const Value: string);
    procedure SetScript(const Value: string);
    procedure SetTag(const Value: string);
    procedure SetVariant(const Value: string);
    procedure SetLangTag_PrivateUse(const Value: string);
    function GetTag: string;
  public
    constructor Create(tag: string);

    procedure Clear;

    function IsValid: Boolean; overload;
    function IsValid(var msg: string): Boolean; overload;

    function IsCanonical: Boolean; overload;
    function IsCanonical(var msg: string): Boolean; overload;

    property Language: string read FLanguage write SetLanguage;
    property ExtLang: string read FExtLang write SetExtLang;
    property Script: string read FScript write SetScript;
    property Region: string read FRegion write SetRegion;
    property Variant: string read FVariant write SetVariant;
    property Extension: string read FExtension write SetExtension;
    property LangTag_PrivateUse: string read FLangTag_PrivateUse write SetLangTag_PrivateUse;

    property PrivateUse: string read FPrivateUse write SetPrivateUse;
    property Grandfathered: string read FGrandfathered write SetGrandfathered;

    property OriginalTag: string read FOriginalTag;
    property Tag: string read GetTag write SetTag;
  end;

implementation

uses
  System.RegularExpressions,

  Keyman.System.KMXFileLanguages;

{ TBCP47Tag }

procedure TBCP47Tag.Clear;
begin
  FLanguage := '';
  FExtLang := '';
  FScript := '';
  FRegion := '';
  FVariant := '';
  FExtension := '';
  FPrivateUse := '';
  FGrandfathered := '';
  FLangTag_PrivateUse := '';
end;

constructor TBCP47Tag.Create(tag: string);
begin
  SetTag(tag);
end;

function TBCP47Tag.GetTag: string;
begin
  if FGrandfathered <> '' then
    Result := FGrandfathered
  else if FPrivateUse <> '' then
    Result := 'x-'+FPrivateUse
  else
  begin
    Result := FLanguage;
    if FExtLang <> '' then
      Result := Result + '-' + FExtLang;
    if FScript <> '' then
      Result := Result + '-' + FScript;
    if FRegion <> '' then
      Result := Result + '-' + FRegion;
    if FVariant <> '' then
      Result := Result + '-' + FVariant;
    if FExtension <> '' then
      Result := Result + '-' + FExtension;
    if FLangTag_PrivateUse <> '' then
      Result := Result + '-x-' + FLangTag_PrivateUse;
  end;
end;

function TBCP47Tag.IsValid(var msg: string): Boolean;
begin
  Result := SameText(OriginalTag, Tag);
  if not Result then
  begin
    msg := '''' + OriginalTag + ''' is not a valid BCP 47 tag';
  end;
end;

function TBCP47Tag.IsValid: Boolean;
var
  msg: string;
begin
  Result := IsValid(msg);
end;

function TBCP47Tag.IsCanonical(var msg: string): Boolean;
var
  c: string;
begin
  // Assumes that the tag is valid.

  // Test language subtag for canonical value
  c := TKMXFileLanguages.TranslateISO6393ToBCP47(Language);
  Result := SameText(c, Language);
  if not Result then
  begin
    msg := '''' + OriginalTag + ''' is a valid tag but is not canonical: '''+Language+''' should be '''+c+'''';
  end;
end;

function TBCP47Tag.IsCanonical: Boolean;
var
  msg: string;
begin
  Result := IsCanonical(msg);
end;

procedure TBCP47Tag.SetExtension(const Value: string);
begin
  FExtension := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetExtLang(const Value: string);
begin
  FExtLang := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetGrandfathered(const Value: string);
begin
  Clear;
  FGrandfathered := LowerCase(Value, TLocaleOptions.loInvariantLocale);
end;

procedure TBCP47Tag.SetLangTag_PrivateUse(const Value: string);
begin
  FLangTag_PrivateUse := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetLanguage(const Value: string);
begin
  FLanguage := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetPrivateUse(const Value: string);
begin
  Clear;
  FPrivateUse := LowerCase(Value, TLocaleOptions.loInvariantLocale);
end;

procedure TBCP47Tag.SetRegion(const Value: string);
begin
  FRegion := UpperCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetScript(const Value: string);
begin
  FScript :=
    UpperCase(Copy(Value, 1, 1), TLocaleOptions.loInvariantLocale)+
    LowerCase(Copy(Value, 2, 3), TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

function Merge(s: array of string; sep: string): string;
var
  i: Integer;
begin
  if Length(s) = 0 then
    Exit('');
  Result := s[0];
  for i := Low(s)+1 to High(s) do
    Result := Result + sep+s[i];
end;

procedure TBCP47Tag.SetTag(const Value: string);
var
  m: TMatch;
  s: TArray<string>;
begin
  FOriginalTag := Value;

  Clear;

  with TRegEx.Create(
      '^(?:(en-GB-oed|i-ami|i-bnn|i-default|i-enochian|i-hak|i-klingon|i-lux|i-mingo|i-navajo|i-pwn|i-tao|i-tay|i-tsu|'+
      'sgn-BE-FR|sgn-BE-NL|sgn-CH-DE)|(art-lojban|cel-gaulish|no-bok|no-nyn|zh-guoyu|zh-hakka|zh-min|zh-min-nan|zh-xiang))$'+
      '|^((?:[a-z]{2,3}(?:(?:-[a-z]{3}){1,3})?)|[a-z]{4}|[a-z]{5,8})(?:-([a-z]{4}))?(?:-([a-z]{2}|\d{3}))?((?:-(?:[\da-z]{5,8}'+
      '|\d[\da-z]{3}))*)?((?:-[\da-wy-z](?:-[\da-z]{2,8})+)*)?(-x(?:-[\da-z]{1,8})+)?$|^(x(?:-[\da-z]{1,8})+)$', [roIgnoreCase]) do
  begin
    m := Match(LowerCase(Value, TLocaleOptions.loInvariantLocale));

    if (m.Groups.Count > 3) and (m.Groups[3].Value <> '') then
    begin
      // langtag language
      s := TRegEx.Split(m.Groups[3].Value, '-');
      FLanguage := s[0]; Delete(s, 0, 1);
      FExtLang := Merge(s, '-');
    end;

    if m.Groups.Count > 4 then
      SetScript(m.Groups[4].Value); // get casing

    if m.Groups.Count > 5 then
      SetRegion(m.Groups[5].Value); // get casing

    // langtag variant
    if m.Groups.Count > 6 then
      FVariant := Copy(m.Groups[6].Value, 2, MaxInt); // Delete initial -

    // langtag extension
    if m.Groups.Count > 7 then
      FExtension := Copy(m.Groups[7].Value, 2, MaxInt); // Delete initial -

    // langtag privateuse
    if m.Groups.Count > 8 then
      FLangTag_PrivateUse := Copy(m.Groups[8].Value, 4, MaxInt); // Delete initial -x-

    if m.Groups.Count > 9 then
      FPrivateUse := Copy(m.Groups[9].Value, 3, MaxInt); // Delete initial x-

    if m.Groups.Count > 1 then
      if (m.Groups[1].Value = '') and (m.Groups.Count > 2)
        then FGrandfathered := m.Groups[2].Value
        else FGrandfathered := m.Groups[1].Value;

//      raise EBCP47Tag.Create('Invalid tag');
  end;
end;

procedure TBCP47Tag.SetVariant(const Value: string);
begin
  FVariant := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

end.
