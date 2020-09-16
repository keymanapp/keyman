unit Keyman.Configuration.System.KeymanUILanguageManager;

interface

uses
  System.Classes,

  Keyman.System.UILanguageManager;

type
  TKeymanUILanguageManager = class(TUILanguageManager)
  private
  public
    class function GetKeymanUILanguages: TStrings; static;
    class function Execute: Boolean; static;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  kmint;

/// Sets the Keyman UI language to the first matching user preferred UI language, if any found.
class function TKeymanUILanguageManager.Execute: Boolean;
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
    kmint.KeymanCustomisation.CustMessages.LanguageCode := tag;
end;

/// Returns list of BCP 47 tags (lower cased) of Keyman's available UI languages
class function TKeymanUILanguageManager.GetKeymanUILanguages: TStrings;
begin
  Result := TStringList.Create;
  Result.Text := LowerCase(KeymanCustomisation.CustMessages.GetAvailableLanguages);
  Result.Insert(0, LowerCase(KeymanCustomisation.CustMessages.MessageFromID('SKDefaultLanguageCode')));
end;

end.
