unit Keyman.Setup.System.SetupUILanguageManager;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,

  Keyman.System.UILanguageManager,
  SetupStrings;

type
  TSetupUILanguageManager = class(TUILanguageManager)
  public type
    TLocaleArray = array[TInstallInfoText] of string;
    TSetupLocales = TDictionary<string,string>;
  private class var
    FStrings: TDictionary<string, TLocaleArray>;
    FActiveLocale: string;
    FLocales: TSetupLocales;
    FIndexMapping: TDictionary<Integer,Integer>;
    FHasChanged: Boolean;
    FSortedValues: TList< TPair<string,string>>;
  private
    class procedure CreateStatic;
    class procedure DestroyStatic;
  public
    class function GetSetupUILanguages: TStrings; static;
    class function SetDefault: Boolean; static;

    class function Get(id: TInstallInfoText): string;
    class function Locales: TSetupLocales; static;
    class function IndexMapping: TDictionary<Integer,Integer>; static;
    class function SortedValues: TList< TPair<string,string>>; static;
    class property ActiveLocale: string read FActiveLocale write FActiveLocale;
    class procedure RegisterSetupStrings(const tag: string; const locale: TLocaleArray);
    class procedure Reorder;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

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

(*

  Returns the list of locale key-value pairs sorted by their locale names (i.e. value).
  This list is regenerated in Reorder.
  @returns A list of sorted locale pairs (Key = xxxxx, Value = yyyyy)
*)
class function TSetupUILanguageManager.SortedValues: TList< TPair<string,string>>;
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
begin
  CreateStatic;
  if FActiveLocale = '' then
    FActiveLocale := tag;
  FStrings.Add(tag, locale);
  FLocales.Add(tag, locale[ssLanguageName]);
  FHasChanged := True;
end;

class procedure TSetupUILanguageManager.Reorder;
var
  item: TPair<string,string>;
  i: integer;
  j: integer;
  sortedPairA: TList< TPair<string,string>>;
begin
  sortedPairA := TList< TPair<string,string>>.Create();
  for item in FLocales do
  begin
    sortedPairA.Add(item);
  end;
  sortedPairA.Sort(
    TComparer<TPair<string, string>>.Construct(
      function(const Left, Right: TPair<string, string>): Integer
      begin
        // Sort by the Key value
        Result := CompareText(Left.Value, Right.Value);
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

