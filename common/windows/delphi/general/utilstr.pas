(*
  Name:             utilstr
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    23 Aug 2006 - mcdurdin - Add StringToExtString and WideQuotedStr functions
                    14 Sep 2006 - mcdurdin - Add RectToString, StringToRect, use widestrings for some functions
                    16 May 2007 - mcdurdin - Fix CommaToken to widestring
                    12 Mar 2010 - mcdurdin - I891 - Search for U+22 as well as U+0022
                    25 Mar 2011 - mcdurdin - I2705 - Inserting supplementary plane characters into Layout tab generates surrogate pairs
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit utilstr;  // I3306

interface

uses
  System.Classes,
  System.Types,
  System.WideStrings;

const
  CRLF = #13#10;

function RectToString(r: TRect): string;
function StringToRect(s: string): TRect;

function wchr(n: Integer): WideChar;
function StrToken(var s: string; const tokens: string): string;
function WStrToken(var s: WideString; const tokens: WideString): WideString; deprecated;  // I3310
function CommaToken(var s: WideString): WideString;

procedure TagsToStringList(s: WideString; str: TWideStrings);
procedure SplitString(const instr: string; var outstr1, outstr2: string; const split: string);
function FormatUnicode(s: WideString): string;
function FormatANSI(s: string): string;

function ExtNumToInt(s: string): Integer;
function IntToExtNum(n, base: Integer): string;

function ExtStringToString(s: string; out FError: Boolean): string;  // I3310
function StringToExtString(const s: string; FormatUnicode: Boolean): string;  // I3310

function TrimQuotes(s: string): string;

function GetTokenFromCaret(line: string; var selx, sellen: Integer): string;

function WideQuotedStr(const str: WideString): WideString; deprecated;  // I3310



implementation

uses
  System.SysUtils,

  Unicode;

function CommaToken(var s: WideString): WideString;
var
  n: Integer;
begin
  while (s <> '') and (Pos(s[1], ' '#9#13#10) > 0) do Delete(s,1,1);

  if s = '' then
  begin
    Result := '';
    Exit;
  end;
  
  if s[1] = '"' then
  begin
    Delete(s,1,1);
    n := Pos('"', s);
    if n = 0 then raise Exception.Create('CommaToken: Unmatched opening quote in input text');
    Result := Trim(Copy(s, 1, n-1));
    Delete(s, 1, n);
    if s <> '' then
      if s[1] <> ','
        then raise Exception.Create('CommaToken: Expected but did not find comma or EOL after closing quote')
        else Delete(s,1,1);
  end
  else
  begin
    n := Pos(',', s);
    if n = 0 then n := Length(s)+1;
    Result := Trim(Copy(s, 1, n-1));
    Delete(s, 1, n);
  end;

  while (s <> '') and (Pos(s[1], ' '#9#13#10) > 0) do Delete(s,1,1);
end;

procedure TagsToStringList(s: WideString; str: TWideStrings);
var
  n: Integer;
  t: WideString;
begin
  s := Trim(s);
  while s <> '' do
  begin
    if s[1] <> '<' then Exit;
    Delete(s,1,1);
    n := Pos('>', s);
    if n = 0 then Exit;
    t := Copy(s, 1, n-1);
    Delete(s,1,n);
    n := Pos('</'+t+'>', s);
    if n = 0 then Exit;
    str.Add(t + '=' + Copy(s, 1, n-1));
    Delete(s, 1, n+Length(t)+2);
    s := Trim(s);
  end;
end;

procedure SplitString(const instr: string; var outstr1, outstr2: string; const split: string);
var
  n: Integer;
begin
  n := Pos(split, instr);
  if n > 0 then
  begin
    outstr1 := Copy(instr,1,n-1);
    outstr2 := Copy(instr,n+1,Length(instr));
  end
  else
  begin
    outstr1 := instr;
    outstr2 := instr;
  end;
end;

function FormatANSI(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + Format('d%d ', [Ord(s[i])]);
  Result := Trim(Result);
end;

function FormatUnicode(s: WideString): string;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(s) do
  begin
    if Uni_IsSurrogate1(s[i]) and (i < Length(s)) then
    begin
      Result := Result + Format('U+%4.4X ', [Uni_SurrogateToUTF32(s[i], s[i+1])]);
      Inc(i);
    end
    else
      Result := Result + Format('U+%4.4X ', [Ord(s[i])]);
    Inc(i);
  end;
  Result := Trim(Result);
end;

function ExtNumToInt(s: string): Integer;
begin
  Result := 0;
  s := LowerCase(Trim(s));
  if (s <> '') then
  begin
    if s[1] = 'x' then
      Result := StrToIntDef('$' + Copy(s,2,32), 0)
    else if s[1] = 'd' then
      Result := StrToIntDef(Copy(s,2,32), 0)
    else if Copy(s,1,2) = 'u+' then
    begin
      if Length(s) < 4 then Exit; // I891 - we want U+E01 to find U+0E01 for simplicity
      Result := StrToIntDef('$' + Copy(s,3,32), 0);
    end
    else if CharInSet(s[1], ['0'..'9']) then
      Result := StrToIntDef(s, 0);
  end;
end;

function IntToExtNum(n, base: Integer): string;
begin
  if base = 16 then
    Result := 'x' + IntToHex(n, 1)
  else
    Result := 'd' + IntToStr(n);
end;

function TrimQuotes(s: string): string;
begin
  s := Trim(s);

  if Length(s) < 2 then
  begin
    Result := s;
    Exit;
  end;

  if CharInSet(s[1], ['"', '''']) and (s[Length(s)] = s[1]) then
  begin
    Delete(s, 1, 1);
    Delete(s, Length(s), 1);
  end;

  Result := s;
end;

function isspace(s: Char): Boolean;
begin
  Result := CharInSet(s, [' ', #9, #13, #10]);
end;

function GetTokenFromCaret(line: string; var selx, sellen: Integer): string;
begin
  Result := '';
  if selx > Length(line)+1 then Exit;
  Dec(selx);
  while selx >= 1 do
  begin
    if isspace(line[selx]) then Break;
    Dec(selx);
  end;
  if selx > 0 then Delete(line, 1, selx);
  sellen := 0;
  while (sellen < Length(line)) and not isspace(line[sellen+1]) do
    Inc(sellen);
  Delete(line, sellen+1, Length(line));
  Result := line;
end;

function StrToken(var s: string; const tokens: string): string;
var
  n: Integer;
begin
  Result := '';

  if s = '' then Exit;

  while (s <> '') and (Pos(s[1], tokens) > 0) do
  begin
    Delete(s, 1, 1);
  end;

  if s = '' then Exit;

  n := 1;
  while (n <= Length(s)) and (Pos(s[n], tokens) = 0) do
  begin
    Inc(n);
  end;

  Result := Copy(s, 1, n-1);
  Delete(s, 1, n);
end;

function WStrToken(var s: WideString; const tokens: WideString): WideString;
var
  n: Integer;
begin
  Result := '';

  if s = '' then Exit;

  while (s <> '') and (Pos(s[1], tokens) > 0) do
  begin
    Delete(s, 1, 1);
  end;

  if s = '' then Exit;

  n := 1;
  while (n <= Length(s)) and (Pos(s[n], tokens) = 0) do
  begin
    Inc(n);
  end;

  Result := Copy(s, 1, n-1);
  s := Copy(s, n+1, Length(s));
end;

function wchr(n: Integer): WideChar;
begin
  Result := WideChar(n);
end;

function StringToExtString(const s: string; FormatUnicode: Boolean): string;  // I3310
var
  n, v: Integer;
begin
  Result := '';
  n := 1;
  while n <= Length(s) do
  begin
    v := Ord(s[n]);
    if FormatUnicode then
    begin
      if (n < Length(s)) and Uni_IsSurrogate1(s[n]) and Uni_IsSurrogate2(s[n+1]) then
      begin
        v := Uni_SurrogateToUTF32(s[n], s[n+1]);
        Inc(n);
      end;
      Result := Result + 'U+'+IntToHex(v, 4)+' ';
    end
    else
      Result := Result + 'd'+IntToStr(v)+' ';
    Inc(n);
  end;
  Result := Trim(Result);
end;


function ExtStringToString(s: string; out FError: Boolean): string;  // I3310
var
  t: string;
  opch: string;
  n: Integer;
begin
  Result := '';
  t := '';
  FError := True;
  while s <> '' do
  begin
    case s[1] of
      ' ', #9, #13, #10: Delete(s,1,1);
      'U', 'u':
        begin
          if Length(s) < 2 then Exit;
          if s[2] <> '+' then Exit;
          Delete(s,1,2);
          opch := '$' + StrToken(s, ' '#9#13#10);  // I3310
          if not (Length(opch) in [5, 6, 7]) then Exit;
          n := StrToIntDef(opch, 0);
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + Uni_UTF32CharToUTF16(n); // I2705
        end;
      'd':
        begin
          Delete(s,1,1);
          n := StrToIntDef(StrToken(s, ' '#9#13#10), 0);  // I3310
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + Uni_UTF32CharToUTF16(n); // I2705
        end;
      'x':
        begin
          Delete(s,1,1);
          n := StrToIntDef('$' + StrToken(s, ' '#9#13#10), 0);  // I3310
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + Uni_UTF32CharToUTF16(n); // I2705
        end;
      '0'..'7':
        begin
          n := StrToIntDef(StrToken(s, ' '#9#13#10), 0);  // I3310
          if (n < 40) or (n > 4177777) then Exit;
          t := t + Uni_UTF32CharToUTF16(n); // I2705
        end;
      '''', '"':
        begin
          opch := s[1];
          Delete(s,1,1);
          while (s <> '') and (s[1] <> opch) do
          begin
            t := t + s[1]; Delete(s, 1, 1);
          end;
          if s = '' then Exit;
          Delete(s, 1, 1);
        end;
      else
        Exit;
    end;
  end;

  FError := False;
  Result := t;
end;

function WideQuotedStr(const str: WideString): WideString;
begin
  Result := StringReplace(str, '''', '''''', [rfReplaceAll])
end;

function RectToString(r: TRect): string;
begin
  Result := Format('%d,%d,%d,%d', [r.Left, r.Top, r.Right, r.Bottom]);
end;

function StringToRect(s: string): TRect;
var
  n: Integer;
begin
  n := Pos(',', s);
  if n = 0 then Exit;
  Result.Left := StrToIntDef(Copy(s,1,n-1), 0);
  Delete(s,1,n);

  n := Pos(',', s);
  if n = 0 then Exit;
  Result.Top := StrToIntDef(Copy(s,1,n-1), 0);
  Delete(s,1,n);

  n := Pos(',', s);
  if n = 0 then Exit;
  Result.Right := StrToIntDef(Copy(s,1,n-1), 0);
  Delete(s,1,n);

  Result.Bottom := StrToIntDef(s, 0);
end;

end.
