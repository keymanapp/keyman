unit Keyman.System.AndroidStringToKeymanLocaleString;

interface

type
  TAndroidStringToKeymanLocaleString = class sealed
  public
    class function Transform(text: string): string; static;
  end;

implementation

uses
  System.SysUtils;

class function TAndroidStringToKeymanLocaleString.Transform(text: string): string;
var
  i, j: Integer;
begin
  // strings.xml is not completely identical to the older locale.xml format we use

  // 1. Trim the text first (to avoid this, use "  text  ")
  text := text.Trim;

  // 2. remove surrounding quotes from string
  if text.StartsWith('"') and text.EndsWith('"') then
  begin
    text := text.Substring(1, text.Length-2);
  end;

  // 3. Remove escapes
  i := 1; j := 1;
  SetLength(Result, text.Length);
  while i <= text.Length do
  begin
    if text[i] = '\' then
    begin
      Inc(i);
      case text[i] of
        't': Result[j] := #9;
        'n': begin Result[j] := #13; Inc(j); Result[j] := #10; end;
        else Result[j] := text[i];
      end;
    end
    else if (i <= text.Length - 3) and
      (text[i] = '%') and
      (text[i+1] >= '1') and (text[i+1] <= '9') and
      (text[i+2] = '$') and
      CharInSet(text[i+3], ['d','u','e','f','g','n','m','p','s','x']) then
    begin
      // 4. Transform positional parameters
      // convert from %1$d to %0:d; Delphi parameters are 0-based
      // note: only supports up to 9 positional parameters
      Result[j] := text[i]; Inc(i); Inc(j);
      Result[j] := Char(Ord(text[i])-1); Inc(i); Inc(j);
      Result[j] := ':'; Inc(i); Inc(j);
      Result[j] := text[i];
    end
    else
      Result[j] := text[i];
    Inc(i); Inc(j);
  end;

  SetLength(Result, j-1);
end;

end.
