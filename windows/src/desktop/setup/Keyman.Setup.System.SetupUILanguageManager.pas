unit Keyman.Setup.System.SetupUILanguageManager;

interface

uses
  System.Classes,
  System.Generics.Collections,

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
  private
    class procedure CreateStatic;
    class procedure DestroyStatic;
  public
    class function GetSetupUILanguages: TStrings; static;
    class function SetDefault: Boolean; static;

    class function Get(id: TInstallInfoText): string;
    class function Locales: TSetupLocales; static;
    class property ActiveLocale: string read FActiveLocale write FActiveLocale;
    class procedure RegisterSetupStrings(const tag: string; const locale: TLocaleArray);
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
  end;
end;

class function TSetupUILanguageManager.Locales: TSetupLocales;
begin
  Result := FLocales;
end;

class procedure TSetupUILanguageManager.DestroyStatic;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FLocales);
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
end;

initialization
finalization
  TSetupUILanguageManager.DestroyStatic;
end.

