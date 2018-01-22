unit Keyman.System.KMXFileLanguages;

interface

uses
  System.Generics.Collections,
  System.Types,

  Keyman.System.LanguageCodeUtils,
  kmxfile;

type
  TKMXFileLanguages = class(TLanguageCodeUtils)
  public
    class function GetKMXFileBCP47Codes(const filename: string): TStringDynArray;
    class function GetKeyboardInfoBCP47Codes(const ki: TKeyboardInfo): TStringDynArray; static;
  end;

implementation

{ TKMXFileLanguages }

class function TKMXFileLanguages.GetKeyboardInfoBCP47Codes(const ki: TKeyboardInfo): TStringDynArray;
begin
  if ki.ISO6393Languages <> '' then
    Result := TranslateISO6393ToBCP47(EthnologueCodeListToArray(ki.ISO6393Languages))
  else if ki.WindowsLanguages <> '' then
    Result := TranslateWindowsLanguagesToBCP47(WindowsLanguageListToArray(ki.WindowsLanguages))
  else
    Result := [];
end;

class function TKMXFileLanguages.GetKMXFileBCP47Codes(
  const filename: string): TStringDynArray;
var
  ki: TKeyboardInfo;
begin
  GetKeyboardInfo(filename, False, ki, False);
  Result := GetKeyboardInfoBCP47Codes(ki);
end;

end.
