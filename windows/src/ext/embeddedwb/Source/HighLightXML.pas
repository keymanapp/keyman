  { I got Permition to add the XML HighLighter unit and to change the code
   by my needs from Ajay Tandon
   We thank him for that.
   bsalsa }

{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.0                       }
{ Author:                                               }
{ Ajay Tandon                                           }
{                              }
{ bsalsa made some minor code changes                   }
{*******************************************************}
{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterXML.pas, released 2000-11-20.
The Original Code is based on the SynHighlighterHTML.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Jeff Rafter.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: HighLightXML.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

History:
-------------------------------------------------------------------------------
2000-11-30 Removed mHashTable and MakeIdentTable per Michael Hieke

Known Issues:
-------------------------------------------------------------------------------}
//Nothing is really constrained (properly) to valid name chars
//Entity Refs are not constrained to valid name chars
//Support for "Combining Chars and Extender Chars" in names are lacking
//The internal DTD is not parsed (and not handled correctly)
//$Id: HighLightXML.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

{
@abstract(Provides an XML highlighter for SynEdit)
@author(Jeff Rafter-- Phil 4:13, based on SynHighlighterHTML by Hideo Koiso, converted to SynEdit by Michael Hieke)
@created(2000-11-17)
@lastmod(2000-11-17)
The SynHighlighterXML unit provides SynEdit with an XML highlighter. }

unit HighLightXML;

interface

{$IFDEF UNICODE}
{$WARN WIDECHAR_REDUCED OFF}
{$ENDIF UNICODE}

{$I EWB.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, HighLightRichSyntax;

type

  TtkTokenKind = (tkNull, tkElement, tkAttribute, tkText, tkCDATA, tkEntityRef,
    tkProcessingInstruction, tkComment, tkQuoteAttrValue, tkAposAttrValue,
    tkQuoteEntityRef, tkAposEntityRef, tkSymbol, tkSpace, tkEqual, tknsAttribute,
    tknsQuoteAttrValue, tknsAposAttrValue, tknsQuoteEntityRef, tknsAposEntityRef,
    tknsEqual, tkDocType);

  TRangeState = (rsElement, rsEqual, rsAttribute, rsQuoteAttrValue, rsAposAttrValue,
    rsQuoteEntityRef, rsAPosEntityRef, rsText, rsCDATA, rsEntityRef, rsProcessingInstruction,
    rsComment, rsnsEqual, rsnsQuoteAttrValue, rsnsAposAttrValue, rsnsQuoteEntityRef,
    rsnsAPosEntityRef, rsDocType);

  TProcTableProc = procedure of object;

  THighlightXML = class(THighlightRichSyntax)
  private
    fRange: TRangeState;
    fLine: PChar;
    Run: Longint;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    procedure NullProc;
    procedure CarriageReturnProc;
    procedure LineFeedProc;
    procedure SpaceProc;
    procedure LessThanProc;
    procedure GreaterThanProc;
    procedure CommentProc;
    procedure ProcessingInstructionProc;
    procedure DocTypeProc;
    procedure CDATAProc;
    procedure TextProc;
    procedure ElementProc;
    procedure AttributeProc;
    procedure QAttributeValueProc;
    procedure AAttributeValueProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure MakeMethodTables;
    function NextTokenIs(T: string): Boolean;
    procedure EntityRefProc;
    procedure QEntityRefProc;
    procedure AEntityRefProc;
  protected
    function GetEol: Boolean; override;
    function GetRange: Pointer;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: integer; override;
    function GetTokenKind: integer;
    function GetTokenPos: Integer;
    procedure Next; override;
    procedure SetRange(Value: Pointer);
    procedure ReSetRange;
    procedure PrepareToken(var AToken: string); override;
    function PrepareOutput(Attr: integer; AToken: string): string; override;
  public
    constructor Create; override;
    procedure SetupDefaultColors; override;
  end;

implementation

const
  SYNS_FilterXML = 'XML Document (*.xml,*.xsd,*.xsl,*.xslt)|*.xml;*.xsd;*.xsl;*.xslt';
  SYNS_LangXML = 'XML Document';

  NameChars: set of char = ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', ':', '-'];

constructor THighlightXML.Create;
begin
  inherited Create;
  inherited Create;
  MakeMethodTables;
  CreateColorTable([clGreen, //1
    clMaroon, //2
      clBlack, //3
      clBlue, //4
      clBlack, //5
      clGreen, //6
      clBlue, //7
      clBlack, //8
      clRed, //9
      clBlack, //10
      clBlack]); //11
  fRange := rsText;
end;

procedure THighlightXML.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
  begin
    case i of
      #0:
        begin
          fProcTable[i] := NullProc;
        end;
      #10:
        begin
          fProcTable[i] := LineFeedProc;
        end;
      #13:
        begin
          fProcTable[i] := CarriageReturnProc;
        end;
      #1..#9, #11, #12, #14..#32:
        begin
          fProcTable[i] := SpaceProc;
        end;
      '<':
        begin
          fProcTable[i] := LessThanProc;
        end;
      '>':
        begin
          fProcTable[i] := GreaterThanProc;
        end;
    else
      fProcTable[i] := IdentProc;
    end;
  end;
end;

procedure THighlightXML.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure THighlightXML.NullProc;
begin
  fTokenID := tkNull;
end;

procedure THighlightXML.CarriageReturnProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure THighlightXML.LineFeedProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure THighlightXML.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do
  begin
    if fLine[Run] in [#0, #9, #10, #13] then
      break;
    Inc(Run);
  end;
end;

procedure THighlightXML.LessThanProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') then
    Inc(Run);

  if (fLine[Run] = '!') then
  begin
    if NextTokenIs('--') then
    begin
      fTokenID := tkSymbol;
      fRange := rsComment;
      Inc(Run, 3);
    end
    else
      if NextTokenIs('DOCTYPE') then
      begin
        fTokenID := tkDocType;
        fRange := rsDocType;
        Inc(Run, 7);
      end
      else
        if NextTokenIs('[CDATA[') then
        begin
          fTokenID := tkCDATA;
          fRange := rsCDATA;
          Inc(Run, 7);
        end
        else
        begin
          fTokenID := tkSymbol;
          fRange := rsElement;
          Inc(Run);
        end;
  end
  else
    if fLine[Run] = '?' then
    begin
      fTokenID := tkProcessingInstruction;
      fRange := rsProcessingInstruction;
      Inc(Run);
    end
    else
    begin
      fTokenID := tkSymbol;
      fRange := rsElement;
    end;
end;

procedure THighlightXML.GreaterThanProc;
begin
  fTokenId := tkSymbol;
  fRange := rsText;
  Inc(Run);
end;

procedure THighlightXML.CommentProc;
begin
  if (fLine[Run] = '-') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '>')
    then
  begin
    fTokenID := tkSymbol;
    fRange := rsText;
    Inc(Run, 3);
    Exit;
  end;

  fTokenID := tkComment;

  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '-') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '>')
      then
    begin
      fRange := rsComment;
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightXML.ProcessingInstructionProc;
begin
  fTokenID := tkProcessingInstruction;
  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '?')
      then
    begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightXML.DocTypeProc;
begin
  fTokenID := tkDocType;
  fRange := rsDocType;

  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run - 1] = ']') and (fLine[Run] = '>') then
    begin
      fRange := rsAttribute;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightXML.CDATAProc;
begin
  fTokenID := tkCDATA;
  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = ']')
      then
    begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightXML.ElementProc;
begin
  if fLine[Run] = '/' then
    Inc(Run);
  while (fLine[Run] in NameChars) do
    Inc(Run);
  fRange := rsAttribute;
  fTokenID := tkElement;
end;

procedure THighlightXML.AttributeProc;
begin
  //Check if we are starting on a closing quote
  if (fLine[Run] in [#34, #39]) then
  begin
    fTokenID := tkSymbol;
    fRange := rsAttribute;
    Inc(Run);
    Exit;
  end;
  //Read the name
  while (fLine[Run] in NameChars) do
    Inc(Run);
  //Check if this is an xmlns: attribute
  if (Pos('xmlns', GetToken) > 0) then
  begin
    fTokenID := tknsAttribute;
    fRange := rsnsEqual;
  end
  else
  begin
    fTokenID := tkAttribute;
    fRange := rsEqual;
  end;
end;

procedure THighlightXML.EqualProc;
begin
  if fRange = rsnsEqual then
    fTokenID := tknsEqual
  else
    fTokenID := tkEqual;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '/') then
    begin
      fTokenID := tkSymbol;
      fRange := rsElement;
      Inc(Run);
      Exit;
    end
    else
      if (fLine[Run] = #34) then
      begin
        if fRange = rsnsEqual then
          fRange := rsnsQuoteAttrValue
        else
          fRange := rsQuoteAttrValue;
        Inc(Run);
        Exit;
      end
      else
        if (fLine[Run] = #39) then
        begin
          if fRange = rsnsEqual then
            fRange := rsnsAPosAttrValue
          else
            fRange := rsAPosAttrValue;
          Inc(Run);
          Exit;
        end;
    Inc(Run);
  end;
end;

procedure THighlightXML.QAttributeValueProc;
begin
  if fRange = rsnsQuoteAttrValue then
    fTokenID := tknsQuoteAttrValue
  else
    fTokenID := tkQuoteAttrValue;

  while not (fLine[Run] in [#0, #10, #13, '&', #34]) do
    Inc(Run);

  if fLine[Run] = '&' then
  begin
    if fRange = rsnsQuoteAttrValue then
      fRange := rsnsQuoteEntityRef
    else
      fRange := rsQuoteEntityRef;
    Exit;
  end
  else
    if fLine[Run] <> #34 then
    begin
      Exit;
    end;

  fRange := rsAttribute;
end;

procedure THighlightXML.AAttributeValueProc;
begin
  if fRange = rsnsAPosAttrValue then
    fTokenID := tknsAPosAttrValue
  else
    fTokenID := tkAPosAttrValue;

  while not (fLine[Run] in [#0, #10, #13, '&', #39]) do
    Inc(Run);

  if fLine[Run] = '&' then
  begin
    if fRange = rsnsAPosAttrValue then
      fRange := rsnsAPosEntityRef
    else
      fRange := rsAPosEntityRef;
    Exit;
  end
  else
    if fLine[Run] <> #39 then
    begin
      Exit;
    end;

  fRange := rsAttribute;
end;

procedure THighlightXML.TextProc;
const
  StopSet = [#0..#31, '<', '&'];
begin
  if fLine[Run] in (StopSet - ['&']) then
  begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkText;
  while not (fLine[Run] in StopSet) do
    Inc(Run);

  if (fLine[Run] = '&') then
  begin
    fRange := rsEntityRef;
    Exit;
  end;
end;

procedure THighlightXML.EntityRefProc;
begin
  fTokenID := tkEntityRef;
  fRange := rsEntityRef;
  while not (fLine[Run] in [#0..#32, ';']) do
    Inc(Run);
  if (fLine[Run] = ';') then
    Inc(Run);
  fRange := rsText;
end;

procedure THighlightXML.QEntityRefProc;
begin
  if fRange = rsnsQuoteEntityRef then
    fTokenID := tknsQuoteEntityRef
  else
    fTokenID := tkQuoteEntityRef;

  while not (fLine[Run] in [#0..#32, ';']) do
    Inc(Run);
  if (fLine[Run] = ';') then
    Inc(Run);

  if fRange = rsnsQuoteEntityRef then
    fRange := rsnsQuoteAttrValue
  else
    fRange := rsQuoteAttrValue;
end;

procedure THighlightXML.AEntityRefProc;
begin
  if fRange = rsnsAPosEntityRef then
    fTokenID := tknsAPosEntityRef
  else
    fTokenID := tkAPosEntityRef;

  while not (fLine[Run] in [#0..#32, ';']) do
    Inc(Run);
  if (fLine[Run] = ';') then
    Inc(Run);

  if fRange = rsnsAPosEntityRef then
    fRange := rsnsAPosAttrValue
  else
    fRange := rsAPosAttrValue;
end;

procedure THighlightXML.IdentProc;
begin
  case fRange of
    rsElement: ElementProc;
    rsAttribute: AttributeProc;
    rsEqual, rsnsEqual: EqualProc;
    rsQuoteAttrValue, rsnsQuoteAttrValue: QAttributeValueProc;
    rsAposAttrValue, rsnsAPosAttrValue: AAttributeValueProc;
    rsQuoteEntityRef, rsnsQuoteEntityRef: QEntityRefProc;
    rsAposEntityRef, rsnsAPosEntityRef: AEntityRefProc;
    rsEntityRef:
      begin
        EntityRefProc;
      end;
  else
    ;
  end;
end;

procedure THighlightXML.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsText: TextProc;
    rsComment: CommentProc;
    rsProcessingInstruction: ProcessingInstructionProc;
    rsDocType: DocTypeProc;
    rsCDATA: CDATAProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function THighlightXML.NextTokenIs(T: string): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(T);
  for I := 1 to Len do
    if (fLine[Run + I] <> T[I]) then
    begin
      Result := False;
      Break;
    end;
end;

function THighlightXML.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function THighlightXML.GetToken: string;
var
  len: Longint;
begin
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function THighlightXML.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function THighlightXML.GetTokenAttribute: integer;
begin
  case fTokenID of
    tkElement: Result := 1; //fElementAttri;
    tkAttribute: Result := 2; //fAttributeAttri;
    tknsAttribute: Result := 3; //fnsAttributeAttri;
    tkEqual: Result := 4; //fSymbolAttri;
    tknsEqual: Result := 5; //fSymbolAttri;
    tkQuoteAttrValue: Result := 6; //fAttributeValueAttri;
    tkAPosAttrValue: Result := 7; //fAttributeValueAttri;
    tknsQuoteAttrValue: Result := 8; //fnsAttributeValueAttri;
    tknsAPosAttrValue: Result := 9; //fnsAttributeValueAttri;
    tkText: Result := 10; //fTextAttri;
    tkCDATA: Result := 11; //fCDATAAttri;
    tkEntityRef: Result := 1; //fEntityRefAttri;
    tkQuoteEntityRef: Result := 2; //fEntityRefAttri;
    tkAposEntityRef: Result := 3; //fEntityRefAttri;
    tknsQuoteEntityRef: Result := 4; //fEntityRefAttri;
    tknsAposEntityRef: Result := 5; //fEntityRefAttri;
    tkProcessingInstruction: Result := 6; //fProcessingInstructionAttri;
    tkComment: Result := 7; //fCommentAttri;
    tkDocType: Result := 8; //fDocTypeAttri;
    tkSymbol: Result := 9; //fSymbolAttri;
    tkSpace: Result := 10; //fSpaceAttri;
  else
    Result := 1; //nil;
  end;
end;

function THighlightXML.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function THighlightXML.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function THighlightXML.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure THighlightXML.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure THighlightXML.ReSetRange;
begin
  fRange := rsText;
end;

function THighlightXML.PrepareOutput(Attr: integer; AToken: string): string;
var
  A: integer;
begin
  A := Attr;
  if Pos('//', Trim(AToken)) = 1 then
    A := 2;
  if ((Attr = 10) and (Pos('"', AToken) = 1)) then
    A := 6;
  Result := Format('\cf%d %s', [A, AToken]);
end;

procedure THighlightXML.PrepareToken(var AToken: string);
begin
  AToken := StringReplace(AToken, '\', '\\', [rfReplaceAll]);
end;

procedure THighlightXML.SetupDefaultColors;
begin
  CreateColorTable([clGreen, //1
    clMaroon, //2
      clBlack, //3
      clBlue, //4
      clBlack, //5
      clGreen, //6
      clBlue, //7
      clBlack, //8
      clRed, //9
      clBlack, //10
      clBlack]); //11
end;

end.
