unit Keyman.System.LanguageCodeUtils;

interface

uses
  System.Generics.Collections,
  System.Types;

type
  TLanguageCodeUtils = class
  private
    class var
    FISO6393ToBCP47: TDictionary<string,string>;
    FLCIDToBCP47: TDictionary<Integer,string>;
    FBCP47Languages: TDictionary<string,string>;
    FBCP47Scripts: TDictionary<string,string>;
    FBCP47Regions: TDictionary<string,string>;
    FSuppressScripts: TDictionary<string,string>;

    class procedure BuildISO6393ToBCP47; static;
    class procedure BuildLCIDToBCP47; static;
    class procedure BuildSubtagRegistry; static;
    class function GetBCP47Languages: TDictionary<string, string>; static;
    class function GetBCP47Regions: TDictionary<string, string>; static;
    class function GetBCP47Scripts: TDictionary<string, string>; static;
    class function GetSuppressScripts: TDictionary<string, string>; static;
    class procedure BuildSuppressScripts; static;
  public
    class function TranslateISO6393ToBCP47(codes: TStringDynArray): TStringDynArray; overload; static;
    class function TranslateISO6393ToBCP47(code: string): string; overload; static; // single code
    class function TranslateMultipleISO6393ToBCP47(codes: string): string; static; // multiple space/punct separated codes

    class function TranslateWindowsLanguagesToBCP47(codes: TIntegerDynArray): TStringDynArray; overload; static;
    class function TranslateWindowsLanguagesToBCP47(code: Integer): string; overload; static;

    class function EthnologueCodeListToArray(codes: string): TStringDynArray; static;
    class function WindowsLanguageListToArray(codes: string): TIntegerDynArray; static;

    class function LanguageName(const LanguageName, ScriptName, RegionName: string): string;

    class property BCP47Languages: TDictionary<string,string> read GetBCP47Languages;
    class property BCP47Scripts: TDictionary<string,string> read GetBCP47Scripts;
    class property BCP47Regions: TDictionary<string,string> read GetBCP47Regions;
    class property SuppressScripts: TDictionary<string,string> read GetSuppressScripts;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Keyman.System.Standards.BCP47SubtagRegistry,
  Keyman.System.Standards.BCP47SuppressScriptRegistry,
  Keyman.System.Standards.ISO6393ToBCP47Registry,
  Keyman.System.Standards.LCIDToBCP47Registry,

  utilstr;

{ TLanguageCodeUtils }

class function TLanguageCodeUtils.TranslateISO6393ToBCP47(codes: TStringDynArray): TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(codes));
  for i := Low(codes) to High(codes) do
    Result[i] := TranslateISO6393ToBCP47(codes[i]);
end;

class function TLanguageCodeUtils.TranslateISO6393ToBCP47(code: string): string;
begin
  BuildISO6393ToBCP47;

  if not FISO6393ToBCP47.TryGetValue(code, Result) then
    Result := code;
end;

class function TLanguageCodeUtils.TranslateMultipleISO6393ToBCP47(codes: string): string;
var
  t: string;
begin
  Result := '';
  t := StrToken(codes, ' ,.;:/');
  while t <> '' do
  begin
    Result := Result + TLanguageCodeUtils.TranslateISO6393ToBCP47(t) + ' ';
    t := StrToken(codes, ' ,.;:/');
  end;
  Result := Trim(Result);

end;

class function TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(codes: TIntegerDynArray): TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(codes));
  for i := Low(codes) to High(codes) do
    Result[i] := TranslateWindowsLanguagesToBCP47(codes[i]);
end;

class function TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(code: Integer): string;
begin
  BuildLCIDToBCP47;

  if not FLCIDToBCP47.TryGetValue(code, Result) then
    Result := '';
end;

class procedure TLanguageCodeUtils.BuildISO6393ToBCP47;
begin
  if not Assigned(FISO6393ToBCP47) then
  begin
    FISO6393ToBCP47 := TDictionary<string,string>.Create;
    TISO6393ToBCP47Map.Fill(FISO6393ToBCP47);
  end;
end;

class procedure TLanguageCodeUtils.BuildLCIDToBCP47;
begin
  if not Assigned(FLCIDToBCP47) then
  begin
    FLCIDToBCP47 := TDictionary<Integer,string>.Create;
    TLCIDToBCP47Map.Fill(FLCIDToBCP47);
  end;
end;

class procedure TLanguageCodeUtils.BuildSubtagRegistry;
begin
  if not Assigned(FBCP47Languages) then
  begin
    FBCP47Languages := TDictionary<string,string>.Create;
    TBCP47SubtagRegistry.FillLanguages(FBCP47Languages);

    FBCP47Scripts := TDictionary<string,string>.Create;
    TBCP47SubtagRegistry.FillScripts(FBCP47Scripts);

    FBCP47Regions := TDictionary<string,string>.Create;
    TBCP47SubtagRegistry.FillRegions(FBCP47Regions);
  end;
end;

class function TLanguageCodeUtils.EthnologueCodeListToArray(codes: string): TStringDynArray;
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

class function TLanguageCodeUtils.GetBCP47Languages: TDictionary<string, string>;
begin
  BuildSubtagRegistry;
  Result := FBCP47Languages;
end;

class function TLanguageCodeUtils.GetBCP47Scripts: TDictionary<string, string>;
begin
  BuildSubtagRegistry;
  Result := FBCP47Scripts;
end;

class function TLanguageCodeUtils.GetSuppressScripts: TDictionary<string, string>;
begin
  BuildSuppressScripts;
  Result := FSuppressScripts;
end;

class procedure TLanguageCodeUtils.BuildSuppressScripts;
var
  FScripts: TStringList;
  i: Integer;
begin
  if not Assigned(FSuppressScripts) then
  begin
    FSuppressScripts := TDictionary<string,string>.Create;
    FScripts := TStringList.Create;
    try
      FScripts.Text := SuppressScriptSubtagRegistry;
      for i := 0 to FScripts.Count - 1 do
        FSuppressScripts.Add(FScripts.Names[i], FScripts.ValueFromIndex[i]);
    finally
      FScripts.Free;
    end;
  end;
end;

class function TLanguageCodeUtils.LanguageName(const LanguageName, ScriptName, RegionName: string): string;
begin
  Result := LanguageName;
  if ScriptName <> '' then
  begin
    Result := Result + ' ('+ScriptName;
    if RegionName <> '' then
      Result := Result + ', '+RegionName;
    Result := Result + ')';
  end
  else if RegionName <> '' then
    Result := Result + ' ('+RegionName+')';
end;

class function TLanguageCodeUtils.GetBCP47Regions: TDictionary<string, string>;
begin
  BuildSubtagRegistry;
  Result := FBCP47Regions;
end;

class function TLanguageCodeUtils.WindowsLanguageListToArray(codes: string): TIntegerDynArray;
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

initialization
finalization
  FreeAndNil(TLanguageCodeUtils.FISO6393ToBCP47);
  FreeAndNil(TLanguageCodeUtils.FLCIDToBCP47);
  FreeAndNil(TLanguageCodeUtils.FBCP47Languages);
  FreeAndNil(TLanguageCodeUtils.FBCP47Regions);
  FreeAndNil(TLanguageCodeUtils.FBCP47Scripts);
end.

