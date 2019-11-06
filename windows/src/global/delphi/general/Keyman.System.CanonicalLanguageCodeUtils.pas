unit Keyman.System.CanonicalLanguageCodeUtils;

interface

type
  TCanonicalLanguageCodeUtils = class
    class function FindBestTag(const Tag: string): string;
  end;

implementation

uses
  BCP47Tag,
  Keyman.System.LanguageCodeUtils,
  System.SysUtils;

//
// Find a language code with appropriate script and region subtags
//
// 1. if suppress-script is present and equal to the script of the tag, remove script
// 2. otherwise, if suppress-script is not present, and no script is given, find
//    appropriate script from alltags.
// 3. finally, if no appropriate script is given, return '', because we'll need user
//    to figure out the script themselves
//
// TODO: This code is not really 100% correct. We need to revisit how language tags
//       are constructed and the point at which we do canonicalization. There are
//       also competing requirements; for instance, Windows requires a script in
//       some language tags where other operating systems might now.
//
class function TCanonicalLanguageCodeUtils.FindBestTag(const Tag: string): string;
var
  t: TBCP47Tag;
  script: string;
var
  v: TArray<string>;

  function GetBestTagFromArray(a: TArray<string>): string;
  var
    i: Integer;
  begin
    // If the tag is in the list of valid tags, use it. #2046.
    for i := 0 to High(a) do
      if SameText(a[i], Tag) then
        Exit(a[i]);

    // Otherwise, use the most complete tag
    for i := High(a) downto 0 do
    begin
      with TBCP47Tag.Create(a[i]) do
      try
        if not IsCanonical then
          Continue;
      finally
        Free;
      end;
      Exit(a[i]);
    end;
    Result := '';
  end;
begin
  if Tag = '' then
    Exit('');

  t := TBCP47Tag.Create(Tag);
  try
    if t.Tag = '' then
      Exit('');
    if TLanguageCodeUtils.SuppressScripts.TryGetValue(t.Language, script) then
    begin
      if SameText(t.Script, script) or (t.Script = '') then
      begin
        // The script should be suppressed because it is known to Windows (**really?)
        // We won't do a region check because this is not really required.
        t.Script := '';
        Exit(t.Tag);
      end;
    end;

    //
    // Lookup the full tag first.
    //
    if TLanguageCodeUtils.AllTags.TryGetValue(t.Tag, v) then
      Exit(GetBestTagFromArray(v));

    //
    // Full tag not found, so reduce to Lang-Script
    //
    if (t.Script <> '') and TLanguageCodeUtils.AllTags.TryGetValue(t.Language+'-'+t.Script, v) then
      Exit(GetBestTagFromArray(v));

    //
    // Check Lang-Region
    //
    if (t.Region <> '') and TLanguageCodeUtils.AllTags.TryGetValue(t.Language+'-'+t.Region, v) then
      Exit(GetBestTagFromArray(v));

    //
    // Finally, check just Lang
    //
    if TLanguageCodeUtils.AllTags.TryGetValue(t.Language, v) then
      Exit(GetBestTagFromArray(v));
  finally
    t.Free;
  end;

  Result := '';
end;

end.
