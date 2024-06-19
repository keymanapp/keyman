// Adapted from https://github.com/gagle/node-bcp47 [MIT]
// TODO: BCP47: <NoAction> Does not do full canonicalization; however
// the ISO639-3 -> ISO639-1 mapping is done, which is probably
// sufficient for our needs at this time.
// See https://tools.ietf.org/html/bcp47#section-4.5
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
    FIsValid: Boolean;
    FIsValidated: Boolean;
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

    function IsValid(Constrained: Boolean): Boolean; overload;
    function IsValid(Constrained: Boolean; var msg: string): Boolean; overload;




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
  System.RegularExpressions;


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
  FIsValidated := True;
  FIsValid := False;
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

function TBCP47Tag.IsValid(Constrained: Boolean; var msg: string): Boolean;
var
  NewTag: TBCP47Tag;
begin
  if not FIsValid and FIsValidated then
  begin
    if OriginalTag = '' then
    begin
      msg := 'A valid BCP 47 tag must contain at least a language subtag';
    end
    else
    begin
      msg := '''' + OriginalTag + ''' is not a valid BCP 47 tag';
    end;
    Exit(False);
  end;

  if Tag = '' then
  begin
    msg := 'A valid BCP 47 tag must contain at least a language subtag';
    Exit(False);
  end;

  NewTag := TBCP47Tag.Create(Tag);
  try
    // Test each component, because we want to clarify that a tag constructed with
    // wrong components shouldn't be treated as valid, even though it might form
    // a 'valid' string. e.g. if we set .Language to "LO" and .Script to "LA", that
    // will construct to LO-LA, which is valid, but the component "LA" is actually
    // only valid as a region, and not a script, resulting in incorrect assumptions.
    Result :=
      SameText(NewTag.Language, Language) and
      SameText(NewTag.ExtLang, ExtLang) and
      SameText(NewTag.Script, Script) and
      SameText(NewTag.Region, Region) and
      SameText(NewTag.Variant, Variant) and
      SameText(NewTag.Extension, Extension) and
      SameText(NewTag.PrivateUse, PrivateUse) and
      SameText(NewTag.Grandfathered, Grandfathered) and
      SameText(NewTag.LangTag_PrivateUse, LangTag_PrivateUse);
  finally
    NewTag.Free;
  end;

  if not Result then
  begin
    msg := '''' + Tag + ''' is not a valid BCP 47 tag';
  end;

  if Result and Constrained then
  begin
    NewTag := TBCP47Tag.Create(Tag);
    try
      // For our constrained use of tags, we allow only Language-Script-Region.
      // We only allow 2 or 3 character IDs.
      Result :=
        SameText(NewTag.Language, Language) and
        (NewTag.ExtLang = '') and
        SameText(NewTag.Script, Script) and
        SameText(NewTag.Region, Region) and
        (NewTag.Variant = '') and
        (NewTag.Extension = '') and
        (NewTag.PrivateUse = '') and
        (NewTag.Grandfathered = '') and
        (NewTag.LangTag_PrivateUse = '');

      if not Result then
        msg := '''' + Tag + ''' must contain only Language-Script-Region'
      else
      begin
        Result := Length(NewTag.Language) <= 3;
        if not Result then
          msg := '''' + Tag + ''' language component must be no longer than 3 letters';
      end;
    finally
      NewTag.Free;
    end;
  end;
end;

function TBCP47Tag.IsValid(Constrained: Boolean): Boolean;
var
  msg: string;
begin
  Result := IsValid(Constrained, msg);
end;

procedure TBCP47Tag.SetExtension(const Value: string);
begin
  FExtension := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FIsValidated := False;
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetExtLang(const Value: string);
begin
  FIsValidated := False;
  FExtLang := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetGrandfathered(const Value: string);
begin
  Clear;
  FIsValidated := False;
  FGrandfathered := LowerCase(Value, TLocaleOptions.loInvariantLocale);
end;

procedure TBCP47Tag.SetLangTag_PrivateUse(const Value: string);
begin
  FIsValidated := False;
  FLangTag_PrivateUse := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetLanguage(const Value: string);
begin
  FIsValidated := False;
  FLanguage := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetPrivateUse(const Value: string);
begin
  Clear;
  FIsValidated := False;
  FPrivateUse := LowerCase(Value, TLocaleOptions.loInvariantLocale);
end;

procedure TBCP47Tag.SetRegion(const Value: string);
begin
  FIsValidated := False;
  FRegion := UpperCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

procedure TBCP47Tag.SetScript(const Value: string);
begin
  FIsValidated := False;
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

    FIsValid := m.Success;

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
  FIsValidated := True;
end;

procedure TBCP47Tag.SetVariant(const Value: string);
begin
  FIsValidated := False;
  FVariant := LowerCase(Value, TLocaleOptions.loInvariantLocale);
  FGrandfathered := '';
  FPrivateUse := '';
end;

end.
