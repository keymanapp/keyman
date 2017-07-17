(*
  Name:             kmxfileutils
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    22 Mar 2010 - mcdurdin - I2243 - Support for nul in LHS of rule
                    27 Aug 2012 - mcdurdin - I3442 - V9.0 - Update xstring utils in Delphi to support v8 and v9
*)
unit kmxfileutils;

interface

uses
  Windows;
  
function incxstr(p: PWideChar): PWideChar;
function xstrlen(p: PWideChar): Integer;
function xstrlen_printing(p: PWideChar): Integer;
function GetSuppChar(p: PWideChar): DWord;

implementation

uses
  kmxfile,
  kmxfileconsts,
  Unicode;

function incxstr(p: PWideChar): PWideChar;
begin
  Result := p;
  if Result^ = #0 then Exit;

  if PWord(Result)^ <> UC_SENTINEL then
  begin
		if (PWord(Result)^ >= $D800) and (PWord(Result)^ <= $DBFF) then
    begin
      Inc(Result);
      if (PWord(Result)^ >= $DC00) and (PWord(Result)^ <= $DFFF) then Inc(Result);
    end
    else Inc(Result);
		Exit;
  end;

  Inc(Result);

  case PWord(Result)^ of
    CODE_ANY:      Inc(Result, 2);
    CODE_INDEX:    Inc(Result, 3);
    CODE_USE:      Inc(Result, 2);
    CODE_DEADKEY:  Inc(Result, 2);
    CODE_EXTENDED: begin Inc(Result, 3); while (PWord(Result)^ <> UC_SENTINEL_EXTENDEDEND) and (Result^ <> #0) do Inc(Result); Inc(Result); end;
    CODE_CALL:     Inc(Result, 2);
    CODE_CONTEXTEX: Inc(Result, 2);
    CODE_NOTANY:   Inc(Result, 2);

    CODE_CLEARCONTEXT: Inc(Result, 2);  // I3442
    CODE_IFOPT:    Inc(Result, 4);  // I3442
    CODE_IFSYSTEMSTORE: Inc(Result, 4);  // I3442
    CODE_SETOPT:   Inc(Result, 3);  // I3442
    CODE_SETSYSTEMSTORE: Inc(Result, 3);  // I3442
    CODE_RESETOPT: Inc(Result, 2);  // I3442
    CODE_SAVEOPT:  Inc(Result, 2);  // I3442

    else Inc(Result);
  end;
end;

function xstrlen(p: PWideChar): Integer;
begin
  Result := 0;
  while p^ <> #0 do
  begin
    p := incxstr(p);
    Inc(Result);
  end;
end;

function xstrlen_printing(p: PWideChar): Integer;
var
  q: PWideChar;
begin
  Result := 0;
  while p^ <> #0 do
  begin
    if PWord(p)^ = UC_SENTINEL then
    begin
      q := p;
      Inc(q);
      case PWord(q)^ of  // I3442
        CODE_DEADKEY,
        CODE_NUL,
        CODE_IFOPT,
        CODE_IFSYSTEMSTORE:
          Dec(Result);
      end;
    end;
    p := incxstr(p);
    Inc(Result);
  end;
end;

function GetSuppChar(p: PWideChar): DWord;
var
  ch1: WideChar;
begin
  Result := Ord(p^);
  if Uni_IsSurrogate1(p^) then
  begin
    ch1 := p^;
    Inc(p);
    if Uni_IsSurrogate2(p^) then
      Result := Uni_SurrogateToUTF32(ch1, p^);
  end;
end;

end.
