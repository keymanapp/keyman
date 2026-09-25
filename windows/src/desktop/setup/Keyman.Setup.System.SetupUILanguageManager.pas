unit Keyman.Setup.System.SetupUILanguageManager;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Math,

  Keyman.System.UILanguageManager,
  SetupStrings;

type
  TLanguageName = record
    OrgLanguageName: string;     // original name (not necessarily latin based)
    ExtendedLanguageName: string;// name being used for displaying
    LanguageName4Sort: string;   // name being used for sorting
  end;
  TSetupUILanguageManager = class(TUILanguageManager)
  public type
    TLocaleArray = array[TInstallInfoText] of string;
    TSetupLocales = TDictionary<string,TLanguageName>;
  private class var
    FStrings: TDictionary<string, TLocaleArray>;
    FActiveLocale: string;
    FLocales: TSetupLocales;
    FIndexMapping: TDictionary<Integer,Integer>;
    FHasChanged: Boolean;
    FSortedValues: TList< TPair<string,TLanguageName>>;
  private
    class procedure CreateStatic;
    class procedure DestroyStatic;
  public
    class function GetSetupUILanguages: TStrings; static;
    class function SetDefault: Boolean; static;

    class function Get(id: TInstallInfoText): string;
    class function Locales: TSetupLocales; static;
    class function IndexMapping: TDictionary<Integer,Integer>; static;
    class function SortedValues: TList< TPair<string,TLanguageName>>; static;
    class property ActiveLocale: string read FActiveLocale write FActiveLocale;
    class procedure RegisterSetupStrings(const tag: string; const locale: TLocaleArray);
    class procedure Reorder;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

 (** 
 * Checks whether a string contains only Latin-based characters.
 *
 *  The following Unicode ranges are considered Latin-based:
 *  [$0000,$007F] (Basic Latin)
 *  [$0080,$00FF] (Latin-1 Supplement)
 *
 *  The following ranges are **not** considered Latin-based:
 *  [$0100,$017F] (Latin Extended-A)
 *  [$0180,$024F] (Latin Extended-B)
 *  [$1E00,$1EFF] (Latin Extended Additional)
 *
 * @param  name  The string to check.
 * @return True  if all characters are Latin-based; False otherwise.
 *)
  function isLatinBasedOnly(name: string): Boolean;
  var
    ch: Char;
    code: Integer;
  begin
    Result := True;
    for ch in name do
    begin
      code := Ord(ch);
      Result := Result and (InRange(code, $0000, $007F) or // Basic Latin
                              InRange(code, $0080, $00FF)  // Latin-1 Supplement
                           );
    end;
  end;
  
class procedure TSetupUILanguageManager.CreateStatic;
begin
  if not Assigned(FStrings) then
  begin
    FStrings := TDictionary<string,TLocaleArray>.Create;
    FLocales := TSetupLocales.Create;
    FIndexMapping := TDictionary<Integer,Integer>.Create;
  end;
end;

class function TSetupUILanguageManager.Locales: TSetupLocales;
begin
  if FHasChanged then
  begin
    Reorder;
    FHasChanged := False;
  end;
  Result := FLocales;
end;

(*
  Returns the mapping between sorted index positions and original index positions.
  The mapping is regenerated in Reorder.
  @returns A dictionary mapping sorted index (key) to original index (value)
*)
class function TSetupUILanguageManager.IndexMapping: TDictionary<Integer,Integer>;
begin
  Result := FIndexMapping;
end;

(**
 * Returns the list of locale key-value pairs where 
 * Key = BCP 47 locale tag, and Value = TLanguageName record)
 * This list is regenerated in Reorder.
 *  @returns A list of sorted locale pairs.
 *)
class function TSetupUILanguageManager.SortedValues: TList< TPair<string,TLanguageName>>;
begin
  if FHasChanged then
  begin
    Reorder;
    FHasChanged := False;
  end;
  Result := FSortedValues;
end;

class procedure TSetupUILanguageManager.DestroyStatic;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FLocales);
  FreeAndNil(FIndexMapping);
  FreeAndNil(FSortedValues);
end;

/// Sets the Setup UI language to the first matching user preferred UI language, if any found.
class function TSetupUILanguageManager.SetDefault: Boolean;
var
  tag: string;
  stags, utags: TStrings;
begin
  stags := GetSetupUILanguages;
  utags := GetUserUILanguages;
  try
    tag := Find(stags, utags);
  finally
    stags.Free;
    utags.Free;
  end;

  Result := tag <> '';
  if Result then
    ActiveLocale := tag;
end;

/// Returns list of BCP 47 tags (lower cased) of Setup's available UI languages
class function TSetupUILanguageManager.GetSetupUILanguages: TStrings;
var
  tag: string;
begin
  Result := TStringList.Create;
  for tag in FLocales.Keys.ToArray do
    Result.Add(tag);
end;

class function TSetupUILanguageManager.Get(id: TInstallInfoText): string;
begin
  CreateStatic;
  Result := FStrings[FActiveLocale][id];
end;

class procedure TSetupUILanguageManager.RegisterSetupStrings(const tag: string;
  const locale: TLocaleArray);
var
  languageTriple : TLanguageName;
begin
  CreateStatic;
  if FActiveLocale = '' then
    FActiveLocale := tag;
  FStrings.Add(tag, locale);
  languageTriple.OrgLanguageName := locale[ssLanguageName];
  if isLatinBasedOnly(locale[ssLanguageName]) then
  begin
    languageTriple.ExtendedLanguageName := locale[ssLanguageName];
    languageTriple.LanguageName4Sort := locale[ssLanguageName];
  end
  else
  begin
    languageTriple.ExtendedLanguageName := locale[ssLanguageName] + ' (' + locale[ssLanguageNameInEnglish] + ')' ;
    languageTriple.LanguageName4Sort := locale[ssLanguageNameInEnglish];
  end;
  FLocales.Add(tag, languageTriple);
  FHasChanged := True;
end;
(**
 * Rebuilds and sorts FSortedValues based on the LanguageName4Sort field.
 * By using this field, the sort order will be by the original language names for latin script  
 * languages, and the English name for non- Latin script languages.
 *)
class procedure TSetupUILanguageManager.Reorder;
var
  item: TPair<string,TLanguageName>;
  i: integer;
  j: integer;
  sortedPairA: TList< TPair<string,TLanguageName>>;
begin
  sortedPairA := TList< TPair<string,TLanguageName>>.Create();
  for item in FLocales do
  begin
    sortedPairA.Add(item);
  end;
  sortedPairA.Sort(
    TComparer<TPair<string, TLanguageName>>.Construct(
      function(const Left, Right: TPair<string, TLanguageName>): Integer
      begin
        // Sort by the Key value
        Result := CompareText(Left.Value.LanguageName4Sort, Right.Value.LanguageName4Sort);
      end
    )
  );
  FreeAndNil(FSortedValues);
  FSortedValues := sortedPairA;
  i := 0;
  FIndexMapping.Clear;
  for item in FLocales do
  begin
     j := sortedPairA.IndexOf(item) ;
     FIndexMapping.Add(j,i);
     i := i+1;
  end;
end;

initialization
finalization
  TSetupUILanguageManager.DestroyStatic;
end.

