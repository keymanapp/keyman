unit Keyman.System.KeyboardUtils;

interface

type
  TKeyboardUtils = class
  public
    class function KeyboardFileNameToID(filename: string): string;
    class function CouldJavascriptFileBeAKeyboardFile(filename: string): Boolean;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

{ TKeyboardUtil }

class function TKeyboardUtils.CouldJavascriptFileBeAKeyboardFile(
  filename: string): Boolean;
var
  s: string;
begin
  s := ChangeFileExt(ExtractFileName(FileName), '');
  Result := TRegEx.IsMatch(s, '/^[a-z0-9_]+-([0-9]+)(\.[0-9+])+$/', [roIgnoreCase]);
end;

class function TKeyboardUtils.KeyboardFileNameToID(filename: string): string;
var
  ext: string;
begin
  ext := ExtractFileExt(FileName);
  if SameText(ext, '.kmx') or SameText(ext, '.kmp') then
    Result := ChangeFileExt(ExtractFileName(FileName), '')
  else if SameText(ext, '.js') then
  begin
    Result := ChangeFileExt(ExtractFileName(FileName), '');
    if CouldJavascriptFileBeAKeyboardFile(Result)
      then Result := Copy(Result, 1, Pos('-', Result)-1)
      else Result := '';
  end
  else
    Result := '';
end;

end.
