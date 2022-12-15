(*
  Name:             utilhttp
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    28 Aug 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    23 Aug 2006 - mcdurdin - Use Unicode strings
                    06 Oct 2006 - mcdurdin - Add GetParamsFromURL function
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options - form posting support for keyboard option pages
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    04 May 2012 - mcdurdin - I3308 - V9.0 - Start to move towards Delphi namespaces
                    04 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
*)
unit utilhttp;  // I3306  // I3308  // I3309

interface

uses
  System.Classes,
  System.SysUtils,
  System.WideStrUtils,
  System.NetEncoding;

function GetParamsFromURL(URL: WideString; var Params: TStringList): Boolean;
function GetParamsFromURLEx(URL: WideString; var Params: TStringList): Boolean;
procedure DecodeAndSetParams(const AValue: WideString; Params: TStringList);
function URLDecode(ASrc: ansistring): string;   // I4390
function URLEncode(Asrc: string): string;

function ConvertFileURLToPath(s: WideString): WideString;
function ConvertPathToFileURL(s: WideString): WideString;

implementation

// Source:
//   https://marc.durdin.net/2015/08/an-update-for-encodeuricomponent/
// Stack Overflow:
//   https://stackoverflow.com/questions/776302/standard-url-encode-function
function URLEncode(ASrc: string): string;
const
  HexMap: string = '0123456789ABCDEF';

  function IsSafeChar(ch: Byte): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then Result := True    // 0-9
    else if (ch >= 65) and (ch <= 90) then Result := True  // A-Z
    else if (ch >= 97) and (ch <= 122) then Result := True  // a-z
    else if (ch = 33) then Result := True // !
    else if (ch >= 39) and (ch <= 42) then Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then Result := True // -.
    else if (ch = 95) then Result := True // _
    else if (ch = 126) then Result := True // ~
    else Result := False;
  end;

var
  I, J: Integer;
  Bytes: TBytes;
begin
  Result := '';

  Bytes := TEncoding.UTF8.GetBytes(ASrc);

  I := 0;
  J := Low(Result);

  SetLength(Result, Length(Bytes) * 3); // space to %xx encode every byte

  while I < Length(Bytes) do
  begin
    if IsSafeChar(Bytes[I]) then
    begin
      Result[J] := Char(Bytes[I]);
      Inc(J);
    end
    else
    begin
      Result[J] := '%';
      Result[J+1] := HexMap[(Bytes[I] shr 4) + Low(HexMap)];
      Result[J+2] := HexMap[(Bytes[I] and 15) + Low(HexMap)];
      Inc(J,3);
    end;
    Inc(I);
  end;

  SetLength(Result, J-Low(Result));
end;

function URLDecode(ASrc: AnsiString): string;
begin
  Result := TNetEncoding.URL.Decode(string(ASrc));
end;

procedure DecodeAndSetParams(const AValue: WideString; Params: TStringList);
var
  i, j : Integer;
  s: WideString;
begin
  // Convert special characters
  // ampersand '&' separates values    {Do not Localize}
  Params.BeginUpdate;
  try
    Params.Clear;
    i := 1;
    while i <= length(AValue) do
    begin
      j := i;
      while (j <= length(AValue)) and (AValue[j] <> '&') do
      begin
        inc(j);
      end;
      s := copy(AValue, i, j-i);
      // See RFC 1866 section 8.2.1. TP
      s := StringReplace(s, '+', ' ', [rfReplaceAll]);  {do not localize}
      Params.Add(URLDecode(AnsiString(s)));
      i := j + 1;
    end;
  finally
    Params.EndUpdate;
  end;
end;

function GetParamsFromURL(URL: WideString; var Params: TStringList): Boolean;
var
  Command: WideString;
  s: WideString;
  n: Integer;
begin
  s := URL;
  if Copy(s,1,7) = 'keyman:' then
  begin
    params := TStringList.Create;
    Delete(s,1,7);
    n := Pos('?',s);
    if n > 0 then
    begin
      Command := Copy(s,1,n-1);
      DecodeAndSetParams(Copy(s,n+1,Length(s)), params);
    end
    else
      Command := s;
    params.Insert(0, command);
    Result := True;
  end
  else
    Result := False;
end;

function GetParamsFromURLEx(URL: WideString; var Params: TStringList): Boolean;
var
  Command: WideString;
  s: WideString;
  n: Integer;
begin
  s := URL;

  params := TStringList.Create;
  Delete(s,1,7);
  n := Pos('?',s);
  if n > 0 then
  begin
    Command := Copy(s,1,n-1);
    DecodeAndSetParams(Copy(s,n+1,Length(s)), params);
  end
  else
    Command := s;
  params.Insert(0, URL);
  params.Insert(0, command);

  Result := True;
end;

function ConvertFileURLToPath(s: WideString): WideString;
begin
  s := URLDecode(AnsiString(s));
  if Copy(s, 1, 8) = 'file:///'
    then Result := WideReplaceText(Copy(s, 9, MAXINT), '/', '\')
    else Result := s;
end;

function ConvertPathToFileURL(s: WideString): WideString;
begin
  Result := 'file:///'+UrlEncode(WideReplaceText(s, '\', '/'));
end;


end.
