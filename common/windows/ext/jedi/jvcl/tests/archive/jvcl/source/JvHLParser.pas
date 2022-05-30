{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHLParser.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

class       : TJvIParser
description : text parser

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]

-----------------------------------------------------------------------------}

{$I JVCL.Inc}
unit JvHLParser;

interface

uses
  SysUtils, Classes;

const
  ieBadRemark = 1;

type
  TIParserStyle = (psNone, psPascal, psCpp, psPython, psVB, psHtml, psPerl, psCocoR, psPhp);

  TJvIParser = class
  protected
    FpcProgram: PChar;
    FpcPos: PChar; // Current position [translated]
    FHistory: TStringList;
    FHistorySize: Integer;
    FHistoryPtr: Integer;
    FStyle: TIParserStyle;
    FReturnComments: Boolean;
    function HistoryInd(Index: Integer): Integer;
    function GetHistory(Index: Integer): string;
    function GetPosBeg(Index: Integer): Integer;
    function GetPosEnd(Index: Integer): Integer;
    procedure SetHistorySize(Size: Integer);
    function GetPos: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Returns the following token; shifts a current position [translated] }
    function Token: string;
    { Returns the following token to the left of a current position shifts
      a current position to the left [translated]
    function TokenL : string; - It is devilishly difficult to make it *;-( [translated] }
    { Rollback back on the indicated quantity of tokens [translated] }
    procedure RollBack(Index: Integer);
    property History[Index: Integer]: string read GetHistory;
    property PosBeg[Index: Integer]: Integer read GetPosBeg;
    property PosEnd[Index: Integer]: Integer read GetPosEnd;
    property HistorySize: Integer read FHistorySize write SetHistorySize;
    property Pos: Integer read GetPos;
    // (rom) name change needed
    property pcPos: PChar read FpcPos write FpcPos;
    property pcProgram: PChar read FpcProgram write FpcProgram;
    property Style: TIParserStyle read FStyle write FStyle;
    property ReturnComments: Boolean read FReturnComments write FReturnComments;
  end;

  EJvIParserError = class(Exception)
  public
    ErrCode: Integer;
    Pos: Cardinal;
    constructor Create(AErrCode: Integer; APos: Cardinal);
  end;

function IsStringConstant(const St: string): Boolean;
function IsIntConstant(const St: string): Boolean;
function IsRealConstant(const St: string): Boolean;
function IsIdentifier(const ID: string): Boolean;
function GetStringValue(const St: string): string;
procedure ParseString(const S: string; Ss: TStrings);

implementation

uses
  JvCtlConst;

{$IFDEF Delphi}
type
  TSetOfChar = set of Char;
{$ENDIF Delphi}
{$IFDEF CBUILDER}
type
  TSetOfChar = string;
{$ENDIF CBUILDER}

function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean;
begin
  {$IFDEF Delphi}
  Result := Ch in SetOfChar;
  {$ENDIF Delphi}
  {$IFDEF CBUILDER}
  Result := Pos(Ch, SetOfChar) > 0;
  {$ENDIF CBUILDER}
end;

//=== EJvIParserError ========================================================

constructor EJvIParserError.Create(AErrCode: Integer; APos: Cardinal);
begin
  ErrCode := AErrCode;
  Pos := APos;
end;

//=== TJvIParser =============================================================

constructor TJvIParser.Create;
begin
  inherited Create;
  FHistory := TStringList.Create;
  HistorySize := 10;
  Style := psPascal;
end;

destructor TJvIParser.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

function TJvIParser.Token: string;
const
  {$IFDEF Delphi}
  StSkip = [' ', #10, #13];
  {$ENDIF Delphi}
  {$IFDEF CBUILDER}
  StSkip = ' '#10#13;
  {$ENDIF CBUILDER}
var
  P, F: PChar;
  F1: PChar;
  i: Integer;

  function SkipComments: Boolean;
  begin
    SkipComments := True;
    case P[0] of
      '{':
        if FStyle = psPascal then
        begin
          F := StrScan(P + 1, '}');
          if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
            Exit;
          P := F + 1;
        end;
      '}':
        if FStyle = psPascal then //IParserError(ieBadRemark, P - FpcProgram);
          Exit;
      '(':
        if (FStyle in [psPascal, psCocoR]) and (P[1] = '*') then
        begin
          F := P + 2;
          while True do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = ')' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
          end;
          P := F + 1;
        end;
      '*':
        if FStyle in [psPascal, psCocoR] then
        begin
          if (P[1] = ')') then
            //IParserError(ieBadRemark, P - FpcProgram)
            Exit;
        end
        else
        if FStyle in [psCpp, psPhp] then
          if P[1] = '/' then //IParserError(ieBadRemark, P - FpcProgram);
            Exit;
      '/':
        if (FStyle in [psPascal, psCpp, psCocoR, psPhp]) and (P[1] = '/') then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end
        else
        if (FStyle in [psCpp, psCocoR, psPhp]) and (P[1] = '*') then
        begin
          F := P + 2;
          while True do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = '/' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
          end;
          P := F + 1;
        end;
      '#':
        if (FStyle in [psPython, psPerl]) { and
           ((P = FpcProgram) or (P[-1] in [#10, #13])) }then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end;
      '''':
        if FStyle = psVB then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end;
    end;
    SkipComments := False;
  end;

  procedure Return;
  begin
    FpcPos := P;
    FHistory[FHistoryPtr] := Result;
    FHistory.Objects[FHistoryPtr] := TObject(Pos - 1);
    Inc(FHistoryPtr);
    if FHistoryPtr > FHistorySize - 1 then
      FHistoryPtr := 0;
  end;

begin
  { New Token - To begin reading a new token [translated] }
  F := FpcPos;
  P := FpcPos;
  { Firstly skip spaces and remarks }
  repeat
    while CharInSet(P[0], StSkip) do
      Inc(P);
    F1 := P;
    try
      if SkipComments then
        P := StrEnd(F1);
    except
      on E: EJvIParserError do
        if (E.ErrCode = ieBadRemark) and ReturnComments then
          P := StrEnd(F1)
        else
          raise;
    end;
    if ReturnComments and (P > F1) then
    begin
      SetString(Result, F1, P - F1);
      Return;
      Exit;
    end;
    while CharInSet(P[0], StSkip) do
      Inc(P);
  until F1 = P;

  F := P;
  if FStyle <> psHtml then
  begin
    if CharInSet(P[0], StIdFirstSymbols) then
    { token }
    begin
      while CharInSet(P[0], StIdSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if CharInSet(P[0], StConstSymbols10) then
    { number }
    begin
      while CharInSet(P[0], StConstSymbols10) or (P[0] = '.') do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPascal) and (P[0] = '$') and
      CharInSet(P[1], StConstSymbols) then
    { pascal hex number }
    begin
      Inc(P);
      while CharInSet(P[0], StConstSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPerl) and (P[0] in ['$', '@', '%', '&']) then
    { perl identifier }
    begin
      Inc(P);
      while CharInSet(P[0], StIdSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = '''' then
    { pascal string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '''' then
          if P[1] = '''' then
            Inc(P)
          else
            Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
      i := 2;
      while i < Length(Result) - 1 do
      begin
        if Result[i] = '''' then
          Delete(Result, i, 1);
        Inc(i);
      end;
    end
    else
    if (FStyle in [psCpp, psCocoR]) and (P[0] = '"') then
    { C++ string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if (P[0] = '"') and (P[-1] <> '\') then
          Break;
        if (P[0] = '"') and (P[-1] = '\') then
        begin
         // count the backslashes, on even backslahses it is a string end
          i := 1;
          while (P - 1 - i > F) and (P[-1 - i] = '\') do inc(i);
          if i and $01 = 0 then Break;  { same but faster than: if i mod 2 = 0 then Break; }
        end;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if ((FStyle in [psPython, psVB, psHtml]) and (P[0] = '"')) or
      ((FStyle in [psPerl, psPhp]) and (P[0] = '"') and ((P = FpcPos) or (P[-1] <> '/'))) then
    { Python, VB, Html, Perl string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = #0 then
      Result := ''
    else
    begin
      Result := P[0];
      Inc(P);
    end;
  end
  else { html }
  begin
    if (P[0] in ['=', '<', '>']) or
      ((P <> pcProgram) and (P[0] = '/') and (P[-1] = '<')) then
    begin
      Result := P[0];
      Inc(P);
    end
    else
    if P[0] = '"' then
    { Html string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    begin
      while not (P[0] in [#0, ' ', '=', '<', '>']) do
        Inc(P);
      SetString(Result, F, P - F);
    end;
  end;
  Return;
end;

function TJvIParser.HistoryInd(Index: Integer): Integer;
begin
  Result := FHistoryPtr - 1 - Index;
  if Result < 0 then
    Result := Result + FHistorySize;
end;

function TJvIParser.GetHistory(Index: Integer): string;
begin
  Result := FHistory[HistoryInd(Index)];
end;

function TJvIParser.GetPosEnd(Index: Integer): Integer;
begin
  Result := Integer(FHistory.Objects[HistoryInd(Index)]) + 1;
end;

function TJvIParser.GetPosBeg(Index: Integer): Integer;
var
  i: Integer;
  S: string;
begin
  i := HistoryInd(Index);
  S := FHistory[i];
  Result := Integer(FHistory.Objects[i]) - Length(S) + 1;
  case FStyle of
    psPascal:
      if S[1] = '''' then
        for i := 2 to Length(S) - 1 do
          if S[i] = '''' then
            Dec(Result);
  end;
end;

procedure TJvIParser.SetHistorySize(Size: Integer);
{$IFDEF DEBUG}
var
  i: Integer;
{$ENDIF}
begin
  while Size > FHistorySize do
  begin
    FHistory.Add('');
    Inc(FHistorySize);
  end;
  while Size < FHistorySize do
  begin
    FHistory.Delete(0);
    Dec(FHistorySize);
  end;
  {$IFDEF DEBUG}
  for i := 0 to FHistorySize - 1 do
    FHistory[i] := '';
  {$ENDIF}
  FHistoryPtr := 0;
end;

function TJvIParser.GetPos: Integer;
begin
  Result := pcPos - FpcProgram;
end;

procedure TJvIParser.RollBack(Index: Integer);
begin
  FpcPos := PosEnd[Index] + FpcProgram;
  Dec(FHistoryPtr, Index);
  if FHistoryPtr < 0 then
    FHistoryPtr := FHistorySize + FHistoryPtr;
end;

procedure ParseString(const S: string; Ss: TStrings);
var
  Parser: TJvIParser;
  Token: string;
begin
  Ss.Clear;
  Parser := TJvIParser.Create;
  try
    Parser.pcProgram := PChar(S);
    Parser.pcPos := Parser.pcProgram;
    Token := Parser.Token;
    while Token <> '' do
    begin
      Ss.Add(Token);
      Token := Parser.Token;
    end;
  finally
    Parser.Free;
  end;
end;

function IsStringConstant(const St: string): Boolean;
var
  LS: Integer;
begin
  LS := Length(St);
  Result := (LS >= 2) and (((St[1] = '''') and (St[LS] = '''')) or
    ((St[1] = '"') and (St[LS] = '"')));
end;

function IsRealConstant(const St: string): Boolean;
var
  i, j: Integer;
  Point: Boolean;
begin
  Result := False;
  if (St = '.') or (St = '') then
    Exit;
  if St[1] = '-' then
    if Length(St) = 1 then
      Exit
    else
      j := 2
  else
    j := 1;
  Point := False;
  for i := j to Length(St) do
    if St[i] = '.' then
      if Point then
        Exit
      else
        Point := True
    else
    if (St[i] < '0') or (St[i] > '9') then
      Exit;
  Result := True;
end;

function IsIntConstant(const St: string): Boolean;
var
  i, j: Integer;
  Sym: TSetOfChar;
begin
  Result := False;
  if (Length(St) = 0) or ((Length(St) = 1) and (St[1] = '$')) then
    Exit;
  Sym := StConstSymbols10;
  if (St[1] = '-') or (St[1] = '$') then
  begin
    if Length(St) = 1 then
      Exit
    else
      j := 2;
    if St[1] = '$' then
      Sym := StConstSymbols;
  end
  else
    j := 1;
  for i := j to Length(St) do
    if not CharInSet(St[i], Sym) then
      Exit;
  Result := True;
end;

function IsIdentifier(const ID: string): Boolean;
var
  i, L: Integer;
begin
  Result := False;
  L := Length(ID);
  if L = 0 then
    Exit;
  if not CharInSet(ID[1], StIdFirstSymbols) then
    Exit;
  for i := 1 to L do
  begin
    if not CharInSet(ID[1], StIdSymbols) then
      Exit;
  end;
  Result := True;
end;

function GetStringValue(const St: string): string;
begin
  if IsStringConstant(St) then
    Result := Copy(St, 2, Length(St) - 2)
  else
    Result := St;
end;

end.

