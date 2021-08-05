unit Keyman.System.CanonicalLanguageCodeUtils;

interface

type
  TCanonicalLanguageCodeUtils = class
    class function FindBestTag(const Tag: string; AddRegion, AddScriptIfNotSuppressed: Boolean): string;
    class function IsCanonical(const Tag: string; AddRegion, AddScriptIfNotSuppressed: Boolean): Boolean; overload;
    class function IsCanonical(const Tag: string; var Msg: string; AddRegion, AddScriptIfNotSuppressed: Boolean): Boolean; overload;
    class function GetFullTagList(const Tag: string): TArray<string>;
  end;

implementation

uses
  BCP47Tag,
  Keyman.System.LanguageCodeUtils,
  Keyman.System.Standards.LangTagsRegistry,
  System.SysUtils;

///
///<summary>Find a language code with appropriate script and region subtags</summary>
///<remarks>
///  Canonicalizes the given tag using data in langtags.json. Will add region and
///  script if appropriate and if the AddRegion or AddScriptIfNotSuppressed flags
///  are passed. Will never add a script where suppress-script is present for the
///  language. Will translate ISO639-3 to ISO639-1 as well.
///
///  A special case is made for und-fonipa as the canonical version
///  of that is und-Zyyy-fonipa, but we don't need the Zyyy script.
///</remarks>
class function TCanonicalLanguageCodeUtils.FindBestTag(const Tag: string; AddRegion, AddScriptIfNotSuppressed: Boolean): string;
var
  t: TBCP47Tag;
  LangTag: TLangTag;
begin
  if Tag = '' then
    Exit('');

  t := TBCP47Tag.Create(Tag);
  try
    if t.Tag = '' then
      Exit('');

    // Special case for IPA keyboards; otherwise we'd have und-Zyyy-fonipa
    if (t.Language = 'und') and (t.Variant = 'fonipa') then
      Exit('und-fonipa');

    // First, canonicalize any unnecessary ISO639-3 codes
    t.Language := TLanguageCodeUtils.TranslateISO6393ToBCP47(t.Language);


    // Lookup the tag first, canonicalize to the base tag for known tags
    if TLangTagsMap.AllTags.TryGetValue(t.Tag, Result) then
    begin
      t.Tag := Result;
    end;

    if not TLangTagsMap.LangTags.TryGetValue(t.Tag, LangTag) then
    begin
      // Not a known tag but perhaps it's a custom language
      // We'll make no further assumptions
      Exit(t.Tag);
    end;

    // Then, lookup the lang-script and see if there is a suppress-script
    // Or add the default script in if it is missing and not a suppress-script
    if (t.Script = '') and not LangTag.suppress and AddScriptIfNotSuppressed then
      // AddScriptIfNotSuppressed will be True for Windows scenarios;
      // for other systems and for registry systems it will be False
      t.Script := LangTag.script;

    // Add the region if not specified
    // For Windows scenarios, we may want to add a region. For cross-platform,
    // we probably don't want to.
    if (t.Region = '') and AddRegion then
      t.Region := LangTag.region;

    Exit(t.Tag);
  finally
    t.Free;
  end;
end;

class function TCanonicalLanguageCodeUtils.IsCanonical(const Tag: string; AddRegion, AddScriptIfNotSuppressed: Boolean): Boolean;
begin
  Result := SameText(Tag, FindBestTag(Tag, AddRegion, AddScriptIfNotSuppressed));
end;

///
///<summary>Iterate through the language dictionary and find all tags that use this base tag</summary>
///<remarks>
///  This will canonicalize known tags, then apply rules to ensure script subtag
///  is present if not suppressed, and add a default region if none given.
///</remarks>
class function TCanonicalLanguageCodeUtils.GetFullTagList(
  const Tag: string): TArray<string>;
var
  t: TBCP47Tag;
  key: string;
begin
  SetLength(Result, 0);

  if Tag = '' then
    Exit;

  t := TBCP47Tag.Create(Tag);
  try
    if t.Tag = '' then
      Exit;

    // First, canonicalize any unnecessary ISO639-3 codes
    t.Language := TLanguageCodeUtils.TranslateISO6393ToBCP47(t.Language);

    for key in TLangTagsMap.LangTags.Keys do
    begin
      if key.StartsWith(t.Language) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := key;
      end;
    end;
  finally
    t.Free;
  end;
end;

class function TCanonicalLanguageCodeUtils.IsCanonical(const Tag: string;
  var Msg: string; AddRegion, AddScriptIfNotSuppressed: Boolean): Boolean;
var
  c: string;
begin
  c := FindBestTag(Tag, AddRegion, AddScriptIfNotSuppressed);
  Result := SameText(c, Tag);
  if not Result then
  begin
    msg := '''' + Tag + ''' is a valid tag but is not canonical: it should be '''+c+'''';
  end;
end;

end.
