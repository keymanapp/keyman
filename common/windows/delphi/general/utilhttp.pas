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

function URLEncode(ASrc: string): string;
begin
  Result := TNetEncoding.URL.Encode(ASrc);
end;

function URLDecode(ASrc: AnsiString): string;
begin
  Result := TNetEncoding.URL.Decode(ASrc);
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
