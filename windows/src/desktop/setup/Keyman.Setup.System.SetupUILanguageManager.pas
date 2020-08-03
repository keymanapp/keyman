unit Keyman.Setup.System.SetupUILanguageManager;

interface

uses
  System.Classes,

  Keyman.System.UILanguageManager;

type
  TSetupUILanguageManager = class(TUILanguageManager)
  private
  public
    class function GetKeymanUILanguages: TStrings; static;
    class function Execute: Boolean; static;
  end;

implementation

uses
  Winapi.Windows,

  SetupStrings;

/// Sets the Setup UI language to the first matching user preferred UI language, if any found.
class function TSetupUILanguageManager.Execute: Boolean;
var
  tag: string;
  ktags, utags: TStrings;
begin
  ktags := GetKeymanUILanguages;
  utags := GetUserUILanguages;
  try
    tag := Find(ktags, utags);
  finally
    ktags.Free;
    utags.Free;
  end;

  Result := tag <> '';
  if Result then
    TLocaleManager.ActiveLocale := tag;
end;

/// Returns list of BCP 47 tags (lower cased) of Keyman's available UI languages
class function TSetupUILanguageManager.GetKeymanUILanguages: TStrings;
var
  tag: string;
begin
  Result := TStringList.Create;
  for tag in SetupStrings.TLocaleManager.Locales.Keys.ToArray do
    Result.Add(tag);
end;

end.

