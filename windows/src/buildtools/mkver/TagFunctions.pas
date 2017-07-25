(*
  Name:             TagFunctions
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2012

  Modified Date:    4 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    04 May 2012 - mcdurdin - I3308 - V9.0 - Start to move towards Delphi namespaces
*)
unit TagFunctions;  // I3306  // I3308

interface

uses System.SysUtils;

function GetTag(s: string; i: Integer): string;
function GetTagValue(s: string; i: Integer; tags: array of string): Integer;
function UpdateTag(s: string; i: Integer; tag: string): string;

implementation

function GetTag(s: string; i: Integer): string;
var
  n: Integer;
  InQuotes: Boolean;
begin
  s := Trim(s);
  n := 1;
  InQuotes := False;
  while n <= Length(s) do
  begin
    if CharInSet(s[n], [' ', #9]) and not InQuotes then
      if i > 1 then
      begin
        Dec(i);
        Delete(s, 1, n);
        s := Trim(s);
        InQuotes := False;
        n := 0;
      end
      else
        Break
    else if s[n] = '"' then InQuotes := not InQuotes;

    Inc(n);
  end;
  Result := Copy(s, 1, n-1);
end;

function GetTagValue(s: string; i: Integer; tags: array of string): Integer;
var
  t: string;
begin
  t := GetTag(s, i);
  for i := Low(tags) to High(tags) do
    if UpperCase(tags[i]) = UpperCase(t) then
    begin
      Result := i - Low(tags) + 1;
      Exit;
    end;
  Result := 0;
end;

function UpdateTag(s: string; i: Integer; tag: string): string;
var
  InQuotes, InTag: Boolean;
  n: Integer;
begin
  n := 1;
  InQuotes := False; InTag := False;
  while n <= Length(s) do
  begin
    if CharInSet(s[n], [' ', #9]) and not InQuotes then
    begin
      if InTag then
      begin
        InTag := False;
        Dec(i);
      end
    end
    else 
    begin
      if s[n] = '"' then InQuotes := not InQuotes;
      InTag := True;
      if i = 1 then Break;
    end;

    Inc(n);
  end;

  if n > Length(s) then
    raise Exception.Create('Tag for line ' + s + ' not found.');

  Result := Copy(s, 1, n-1);
  Delete(s, 1, n-1);

  n := 1;
  {InTag := True;} InQuotes := False;
  while n <= Length(s) do
  begin
    if CharInSet(s[n], [' ', #9]) and not InQuotes then
      Break
    else
    begin
      //InTag := True;
      if s[n] = '"' then InQuotes := not InQuotes;
    end;
    Inc(n);
  end;

  Result := Result + tag + Copy(s, n, Length(s));
end;

end.
 
