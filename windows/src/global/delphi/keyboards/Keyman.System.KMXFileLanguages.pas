unit Keyman.System.KMXFileLanguages;

interface

uses
  System.Generics.Collections,
  System.Types,

  kmxfile;

type
  TKMXFileLanguages = class
  private
    class var
    FISO6393ToBCP47: TDictionary<string,string>;
    FLCIDToBCP47: TDictionary<Integer,string>;

    class procedure BuildISO6393ToBCP47;
    class procedure BuildLCIDToBCP47; static;
  public
    class function GetKMXFileBCP47Codes(const filename: string): TStringDynArray;
    class function GetKeyboardInfoBCP47Codes(const ki: TKeyboardInfo): TStringDynArray; static;

    class function TranslateISO6393ToBCP47(codes: TStringDynArray): TStringDynArray; overload; static;
    class function TranslateISO6393ToBCP47(code: string): string; overload; static;

    class function TranslateWindowsLanguagesToBCP47(codes: TIntegerDynArray): TStringDynArray; overload; static;
    class function TranslateWindowsLanguagesToBCP47(code: Integer): string; overload; static;

    class function EthnologueCodeListToArray(codes: string): TStringDynArray; static;
    class function WindowsLanguageListToArray(codes: string): TIntegerDynArray; static;
  end;

implementation

uses
  System.SysUtils,
  Keyman.System.Standards.ISO6393ToBCP47Registry,
  Keyman.System.Standards.LCIDToBCP47Registry,

  utilstr;
//  Keyman.System.LanguageSubtagRegistry;

{ TKMXFileLanguages }

class function TKMXFileLanguages.TranslateISO6393ToBCP47(codes: TStringDynArray): TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(codes));
  for i := Low(codes) to High(codes) do
    Result[i] := TranslateISO6393ToBCP47(codes[i]);
end;

class function TKMXFileLanguages.TranslateISO6393ToBCP47(code: string): string;
begin
  if not Assigned(FISO6393ToBCP47) then
    BuildISO6393ToBCP47;

  if not FISO6393ToBCP47.TryGetValue(code, Result) then
    Result := code;
end;

class function TKMXFileLanguages.TranslateWindowsLanguagesToBCP47(codes: TIntegerDynArray): TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(codes));
  for i := Low(codes) to High(codes) do
    Result[i] := TranslateWindowsLanguagesToBCP47(codes[i]);
end;

class function TKMXFileLanguages.TranslateWindowsLanguagesToBCP47(code: Integer): string;
begin
  if not Assigned(FLCIDToBCP47) then
    BuildLCIDToBCP47;

  if not FLCIDToBCP47.TryGetValue(code, Result) then
    Result := '';
end;

class procedure TKMXFileLanguages.BuildISO6393ToBCP47;
begin
  FISO6393ToBCP47 := TDictionary<string,string>.Create;
  TISO6393ToBCP47Map.Fill(FISO6393ToBCP47);
end;

class procedure TKMXFileLanguages.BuildLCIDToBCP47;
begin
  FLCIDToBCP47 := TDictionary<Integer,string>.Create;
  TLCIDToBCP47Map.Fill(FLCIDToBCP47);
end;

class function TKMXFileLanguages.EthnologueCodeListToArray(codes: string): TStringDynArray;
var
  code: string;
begin
  Result := [];
  codes := Trim(codes);
  while codes <> '' do
  begin
    code := StrToken(codes, ' ');
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := code;
    codes := Trim(codes);
  end;
end;

class function TKMXFileLanguages.WindowsLanguageListToArray(codes: string): TIntegerDynArray;
var
  code: string;
  FLanguageID: Integer;
begin
  Result := [];
  codes := Trim(codes);
  while codes <> '' do
  begin
    code := StrToken(codes, ' ');
    Delete(code, 1, 1); // 'x'
    if TryStrToInt('$'+code, FLanguageID) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := FLanguageID;
    end;
    codes := Trim(codes);
  end;
end;

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

initialization
finalization
  FreeAndNil(TKMXFileLanguages.FISO6393ToBCP47);
  FreeAndNil(TKMXFileLanguages.FLCIDToBCP47);
end.
