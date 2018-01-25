unit Keyman.System.KeyboardUtils;

interface

type
  TKeyboardUtils = class
  public
    class function KeyboardFileNameToID(filename: string): string;
    class function CleanKeyboardID(const Name: WideString): WideString; static;
    class function GetKeymanWebCompiledFileName(const FileName: WideString): WideString; static;
    class function GetKeymanWebCompiledNameFromFileName(const FileName: WideString): WideString; static;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

{ TKeyboardUtil }

class function TKeyboardUtils.CleanKeyboardID(const Name: WideString): WideString;
var
  i: Integer;
const
  ValidChars: WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_';
begin
  Result := LowerCase(Name);
  if Length(Result) = 0 then Exit;
  if Pos(Result[1], '0123456789') > 0 then Result := '_'+Result; // Can't have a number as initial
  for i := 1 to Length(Result) do
    if Pos(Result[i], ValidChars) = 0 then Result[i] := '_';
end;

class function TKeyboardUtils.GetKeymanWebCompiledNameFromFileName(const FileName: WideString): WideString;
begin
  Result := CleanKeyboardID(ChangeFileExt(ExtractFileName(FileName), ''));
end;

class function TKeyboardUtils.GetKeymanWebCompiledFileName(const FileName: WideString): WideString;   // I4140
begin
  Result :=
    ExtractFilePath(FileName) +
    GetKeymanWebCompiledNameFromFileName(FileName) +
    '.js';
end;

class function TKeyboardUtils.KeyboardFileNameToID(filename: string): string;
var
  ext: string;
begin
  ext := ExtractFileExt(FileName);
  if SameText(ext, '.kmx') or SameText(ext, '.kmp') or SameText(ext, '.js') then
  begin
    Result := CleanKeyboardID(ChangeFileExt(ExtractFileName(FileName), ''));
  end
  else
    Result := '';
end;

end.
