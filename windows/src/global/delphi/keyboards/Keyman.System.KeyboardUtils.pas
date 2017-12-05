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
  Result := TRegEx.IsMatch(s, '^[a-z0-9_]+-([0-9]+)(\.[0-9+])+$', [roIgnoreCase]);
end;

class function TKeyboardUtils.KeyboardFileNameToID(filename: string): string;
var
  ext: string;
begin
  ext := ExtractFileExt(FileName);
  if SameText(ext, '.kmx') or SameText(ext, '.kmp') then
    Result := ChangeFileExt(ExtractFileName(FileName), '')
  else if SameText(ext, '.js') and CouldJavascriptFileBeAKeyboardFile(FileName) then
  begin
    Result := ChangeFileExt(ExtractFileName(FileName), '');
    Result := Copy(Result, 1, Pos('-', Result)-1);
  end
  else
    Result := '';
end;

end.
