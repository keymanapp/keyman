{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.0                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{   bsalsa made some minor code changes                 }
{*******************************************************}

{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHTML.pas, released 2000-04-10.
The Original Code is based on the hkHTMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
All Rights Reserved.
The Original Code can be obtained from http://synedit.sourceforge.net/
}
//$Id: HighLightHTML.pas,v 1.2 2006/11/15 21:01:41 sergev Exp $

unit HighLightHTML;

{$IFDEF UNICODE}
{$WARN WIDECHAR_REDUCED OFF}
{$ENDIF UNICODE}

interface


{$I EWB.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, HighLightRichSyntax;

const
  MAX_ESCAPEAMPS = 151;

  EscapeAmps: array[0..MAX_ESCAPEAMPS - 1] of PChar = (
    ('&amp;'), {   &   }
    ('&lt;'), {   >   }
    ('&gt;'), {   <   }
    ('&quot;'), {   "   }
    ('&trade;'), {   ™   }
    ('&nbsp;'), { space }
    ('&copy;'), {   ©   }
    ('&reg;'), {   ®   }
    ('&Agrave;'), {   À   }
    ('&Aacute;'), {   Á   }
    ('&Acirc;'), {   Â   }
    ('&Atilde;'), {   Ã   }
    ('&Auml;'), {   Ä   }
    ('&Aring;'), {   Å   }
    ('&AElig;'), {   Æ   }
    ('&Ccedil;'), {   Ç   }
    ('&Egrave;'), {   È   }
    ('&Eacute;'), {   É   }
    ('&Ecirc;'), {   Ê   }
    ('&Euml;'), {   Ë   }
    ('&Igrave;'), {   Ì   }
    ('&Iacute;'), {   Í   }
    ('&Icirc;'), {   Î   }
    ('&Iuml;'), {   Ï   }
    ('&ETH;'), {   Ð   }
    ('&Ntilde;'), {   Ñ   }
    ('&Ograve;'), {   Ò   }
    ('&Oacute;'), {   Ó   }
    ('&Ocirc;'), {   Ô   }
    ('&Otilde;'), {   Õ   }
    ('&Ouml;'), {   Ö   }
    ('&Oslash;'), {   Ø   }
    ('&Ugrave;'), {   Ù   }
    ('&Uacute;'), {   Ú   }
    ('&Ucirc;'), {   Û   }
    ('&Uuml;'), {   Ü   }
    ('&Yacute;'), {   Ý   }
    ('&THORN;'), {   Þ   }
    ('&szlig;'), {   ß   }
    ('&agrave;'), {   à   }
    ('&aacute;'), {   á   }
    ('&acirc;'), {   â   }
    ('&atilde;'), {   ã   }
    ('&auml;'), {   ä   }
    ('&aring;'), {   å   }
    ('&aelig;'), {   æ   }
    ('&ccedil;'), {   ç   }
    ('&egrave;'), {   è   }
    ('&eacute;'), {   é   }
    ('&ecirc;'), {   ê   }
    ('&euml;'), {   ë   }
    ('&igrave;'), {   ì   }
    ('&iacute;'), {   í   }
    ('&icirc;'), {   î   }
    ('&iuml;'), {   ï   }
    ('&eth;'), {   ð   }
    ('&ntilde;'), {   ñ   }
    ('&ograve;'), {   ò   }
    ('&oacute;'), {   ó   }
    ('&ocirc;'), {   ô   }
    ('&otilde;'), {   õ   }
    ('&ouml;'), {   ö   }
    ('&oslash;'), {   ø   }
    ('&ugrave;'), {   ù   }
    ('&uacute;'), {   ú   }
    ('&ucirc;'), {   û   }
    ('&uuml;'), {   ü   }
    ('&yacute;'), {   ý   }
    ('&thorn;'), {   þ   }
    ('&yuml;'), {   ÿ   }
    ('&iexcl;'), {   ¡   }
    ('&cent;'), {   ¢   }
    ('&pound;'), {   £   }
    ('&curren;'), {   ¤   }
    ('&yen;'), {   ¥   }
    ('&brvbar;'), {   ¦   }
    ('&sect;'), {   §   }
    ('&uml;'), {   ¨   }
    ('&ordf;'), {   ª   }
    ('&laquo;'), {   «   }
    ('&shy;'), {   ¬   }
    ('&macr;'), {   ¯   }
    ('&deg;'), {   °   }
    ('&plusmn;'), {   ±   }
    ('&sup2;'), {   ²   }
    ('&sup3;'), {   ³   }
    ('&acute;'), {   ´   }
    ('&micro;'), {   µ   }
    ('&middot;'), {   ·   }
    ('&cedil;'), {   ¸   }
    ('&sup1;'), {   ¹   }
    ('&ordm;'), {   º   }
    ('&raquo;'), {   »   }
    ('&frac14;'), {   ¼   }
    ('&frac12;'), {   ½   }
    ('&frac34;'), {   ¾   }
    ('&iquest;'), {   ¿   }
    ('&times;'), {   ×   }
    ('&divide'), {   ÷   }
    ('&euro;'), {   €   }
    //used by very old HTML editors
    ('&#9;'), {  TAB  }
    ('&#127;'), {      }
    ('&#128;'), {   €   }
    ('&#129;'), {      }
    ('&#130;'), {   ‚   }
    ('&#131;'), {   ƒ   }
    ('&#132;'), {   „   }
    ('&ldots;'), {   …   }
    ('&#134;'), {   †   }
    ('&#135;'), {   ‡   }
    ('&#136;'), {   ˆ   }
    ('&#137;'), {   ‰   }
    ('&#138;'), {   Š   }
    ('&#139;'), {   ‹   }
    ('&#140;'), {   Œ   }
    ('&#141;'), {      }
    ('&#142;'), {   Ž   }
    ('&#143;'), {      }
    ('&#144;'), {      }
    ('&#152;'), {   ˜   }
    ('&#153;'), {   ™   }
    ('&#154;'), {   š   }
    ('&#155;'), {   ›   }
    ('&#156;'), {   œ   }
    ('&#157;'), {      }
    ('&#158;'), {   ž   }
    ('&#159;'), {   Ÿ   }
    ('&#161;'), {   ¡   }
    ('&#162;'), {   ¢   }
    ('&#163;'), {   £   }
    ('&#164;'), {   ¤   }
    ('&#165;'), {   ¥   }
    ('&#166;'), {   ¦   }
    ('&#167;'), {   §   }
    ('&#168;'), {   ¨   }
    ('&#170;'), {   ª   }
    ('&#175;'), {   »   }
    ('&#176;'), {   °   }
    ('&#177;'), {   ±   }
    ('&#178;'), {   ²   }
    ('&#180;'), {   ´   }
    ('&#181;'), {   µ   }
    ('&#183;'), {   ·   }
    ('&#184;'), {   ¸   }
    ('&#185;'), {   ¹   }
    ('&#186;'), {   º   }
    ('&#188;'), {   ¼   }
    ('&#189;'), {   ½   }
    ('&#190;'), {   ¾   }
    ('&#191;'), {   ¿   }
    ('&#215;') {   Ô   }
    );

type
  TtkTokenKind = (tkAmpersand, tkASP, tkComment, tkIdentifier, tkKey, tkNull,
    tkSpace, tkString, tkSymbol, tkText, tkUndefKey, tkValue);

  TRangeState = (rsAmpersand, rsASP, rsComment, rsKey, rsParam, rsText,
    rsUnKnown, rsValue);

  TProcTableProc = procedure of object;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  THighlightHTML = class(THighlightRichSyntax)
  private
    fAndCode: Integer;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: Longint;
    Temp: PChar;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..243] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fLineNumber: Integer;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func1: TtkTokenKind;
    function Func2: TtkTokenKind;
    function Func8: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func10: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func14: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func159: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func161: TtkTokenKind;
    function Func162: TtkTokenKind;
    function Func163: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func170: TtkTokenKind;
    function Func171: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func175: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func179: TtkTokenKind;
    function Func180: TtkTokenKind;
    function Func183: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func192: TtkTokenKind;
    function Func198: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func202: TtkTokenKind;
    function Func203: TtkTokenKind;
    function Func204: TtkTokenKind;
    function Func205: TtkTokenKind;
    function Func207: TtkTokenKind;
    function Func209: TtkTokenKind;
    function Func211: TtkTokenKind;
    function Func212: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func214: TtkTokenKind;
    function Func215: TtkTokenKind;
    function Func216: TtkTokenKind;
    function Func227: TtkTokenKind;
    function Func229: TtkTokenKind;
    function Func236: TtkTokenKind;
    function Func243: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure InitIdent;
    procedure MakeMethodTables;
    procedure ASPProc;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure AmpersandProc;
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

var
  mHashTable: array[#0..#255] of Integer;


procedure MakeIdentTable;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      'a'..'z', 'A'..'Z':
        mHashTable[i] := (Ord(UpCase(i)) - 64);
      '!':
        mHashTable[i] := $7B;
      '/':
        mHashTable[i] := $7A;
    else
      mHashTable[Char(i)] := 0;
    end;
end;

procedure THighlightHTML.InitIdent;
var
  i: Integer;
begin
  for i := 0 to 243 do
    case i of
      1: fIdentFuncTable[i] := Func1;
      2: fIdentFuncTable[i] := Func2;
      8: fIdentFuncTable[i] := Func8;
      9: fIdentFuncTable[i] := Func9;
      10: fIdentFuncTable[i] := Func10;
      11: fIdentFuncTable[i] := Func11;
      12: fIdentFuncTable[i] := Func12;
      13: fIdentFuncTable[i] := Func13;
      14: fIdentFuncTable[i] := Func14;
      16: fIdentFuncTable[i] := Func16;
      17: fIdentFuncTable[i] := Func17;
      18: fIdentFuncTable[i] := Func18;
      19: fIdentFuncTable[i] := Func19;
      20: fIdentFuncTable[i] := Func20;
      21: fIdentFuncTable[i] := Func21;
      23: fIdentFuncTable[i] := Func23;
      24: fIdentFuncTable[i] := Func24;
      25: fIdentFuncTable[i] := Func25;
      26: fIdentFuncTable[i] := Func26;
      27: fIdentFuncTable[i] := Func27;
      28: fIdentFuncTable[i] := Func28;
      29: fIdentFuncTable[i] := Func29;
      30: fIdentFuncTable[i] := Func30;
      31: fIdentFuncTable[i] := Func31;
      32: fIdentFuncTable[i] := Func32;
      33: fIdentFuncTable[i] := Func33;
      35: fIdentFuncTable[i] := Func35;
      37: fIdentFuncTable[i] := Func37;
      38: fIdentFuncTable[i] := Func38;
      39: fIdentFuncTable[i] := Func39;
      40: fIdentFuncTable[i] := Func40;
      41: fIdentFuncTable[i] := Func41;
      42: fIdentFuncTable[i] := Func42;
      43: fIdentFuncTable[i] := Func43;
      46: fIdentFuncTable[i] := Func46;
      47: fIdentFuncTable[i] := Func47;
      48: fIdentFuncTable[i] := Func48;
      49: fIdentFuncTable[i] := Func49;
      50: fIdentFuncTable[i] := Func50;
      52: fIdentFuncTable[i] := Func52;
      53: fIdentFuncTable[i] := Func53;
      55: fIdentFuncTable[i] := Func55;
      56: fIdentFuncTable[i] := Func56;
      57: fIdentFuncTable[i] := Func57;
      58: fIdentFuncTable[i] := Func58;
      61: fIdentFuncTable[i] := Func61;
      62: fIdentFuncTable[i] := Func62;
      64: fIdentFuncTable[i] := Func64;
      65: fIdentFuncTable[i] := Func65;
      66: fIdentFuncTable[i] := Func66;
      67: fIdentFuncTable[i] := Func67;
      70: fIdentFuncTable[i] := Func70;
      76: fIdentFuncTable[i] := Func76;
      78: fIdentFuncTable[i] := Func78;
      80: fIdentFuncTable[i] := Func80;
      81: fIdentFuncTable[i] := Func81;
      82: fIdentFuncTable[i] := Func82;
      83: fIdentFuncTable[i] := Func83;
      84: fIdentFuncTable[i] := Func84;
      85: fIdentFuncTable[i] := Func85;
      87: fIdentFuncTable[i] := Func87;
      89: fIdentFuncTable[i] := Func89;
      90: fIdentFuncTable[i] := Func90;
      91: fIdentFuncTable[i] := Func91;
      92: fIdentFuncTable[i] := Func92;
      93: fIdentFuncTable[i] := Func93;
      94: fIdentFuncTable[i] := Func94;
      105: fIdentFuncTable[i] := Func105;
      107: fIdentFuncTable[i] := Func107;
      114: fIdentFuncTable[i] := Func114;
      121: fIdentFuncTable[i] := Func121;
      123: fIdentFuncTable[i] := Func123;
      124: fIdentFuncTable[i] := Func124;
      130: fIdentFuncTable[i] := Func130;
      131: fIdentFuncTable[i] := Func131;
      132: fIdentFuncTable[i] := Func132;
      133: fIdentFuncTable[i] := Func133;
      134: fIdentFuncTable[i] := Func134;
      135: fIdentFuncTable[i] := Func135;
      136: fIdentFuncTable[i] := Func136;
      138: fIdentFuncTable[i] := Func138;
      139: fIdentFuncTable[i] := Func139;
      140: fIdentFuncTable[i] := Func140;
      141: fIdentFuncTable[i] := Func141;
      143: fIdentFuncTable[i] := Func143;
      145: fIdentFuncTable[i] := Func145;
      146: fIdentFuncTable[i] := Func146;
      149: fIdentFuncTable[i] := Func149;
      150: fIdentFuncTable[i] := Func150;
      151: fIdentFuncTable[i] := Func151;
      152: fIdentFuncTable[i] := Func152;
      153: fIdentFuncTable[i] := Func153;
      154: fIdentFuncTable[i] := Func154;
      155: fIdentFuncTable[i] := Func155;
      157: fIdentFuncTable[i] := Func157;
      159: fIdentFuncTable[i] := Func159;
      160: fIdentFuncTable[i] := Func160;
      161: fIdentFuncTable[i] := Func161;
      162: fIdentFuncTable[i] := Func162;
      163: fIdentFuncTable[i] := Func163;
      164: fIdentFuncTable[i] := Func164;
      168: fIdentFuncTable[i] := Func168;
      169: fIdentFuncTable[i] := Func169;
      170: fIdentFuncTable[i] := Func170;
      171: fIdentFuncTable[i] := Func171;
      172: fIdentFuncTable[i] := Func172;
      174: fIdentFuncTable[i] := Func174;
      175: fIdentFuncTable[i] := Func175;
      177: fIdentFuncTable[i] := Func177;
      178: fIdentFuncTable[i] := Func178;
      179: fIdentFuncTable[i] := Func179;
      180: fIdentFuncTable[i] := Func180;
      183: fIdentFuncTable[i] := Func183;
      186: fIdentFuncTable[i] := Func186;
      187: fIdentFuncTable[i] := Func187;
      188: fIdentFuncTable[i] := Func188;
      192: fIdentFuncTable[i] := Func192;
      198: fIdentFuncTable[i] := Func198;
      200: fIdentFuncTable[i] := Func200;
      202: fIdentFuncTable[i] := Func202;
      203: fIdentFuncTable[i] := Func203;
      204: fIdentFuncTable[i] := Func204;
      205: fIdentFuncTable[i] := Func205;
      207: fIdentFuncTable[i] := Func207;
      209: fIdentFuncTable[i] := Func209;
      211: fIdentFuncTable[i] := Func211;
      212: fIdentFuncTable[i] := Func212;
      213: fIdentFuncTable[i] := Func213;
      214: fIdentFuncTable[i] := Func214;
      215: fIdentFuncTable[i] := Func215;
      216: fIdentFuncTable[i] := Func216;
      227: fIdentFuncTable[i] := Func227;
      229: fIdentFuncTable[i] := Func229;
      236: fIdentFuncTable[i] := Func236;
      243: fIdentFuncTable[i] := Func243;
    else
      fIdentFuncTable[i] := AltFunc;
    end;
end;

function THighlightHTML.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while (ToHash^ in ['a'..'z', 'A'..'Z', '!', '/']) do
  begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  while (ToHash^ in ['0'..'9']) do
  begin
    Inc(Result, (Ord(ToHash^) - Ord('0')));
    Inc(ToHash);
  end;
  fStringLen := (ToHash - fToIdent);
end;

function THighlightHTML.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
begin
  Temp := fToIdent;
  if (Length(aKey) = fStringLen) then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if (mHashTable[Temp^] <> mHashTable[aKey[i]]) then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function THighlightHTML.Func1: TtkTokenKind;
begin
  if KeyComp('A') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func2: TtkTokenKind;
begin
  if KeyComp('B') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func8: TtkTokenKind;
begin
  if KeyComp('DD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func9: TtkTokenKind;
begin
  if KeyComp('I') or KeyComp('H1') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func10: TtkTokenKind;
begin
  if KeyComp('H2') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func11: TtkTokenKind;
begin
  if KeyComp('H3') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func12: TtkTokenKind;
begin
  if KeyComp('H4') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func13: TtkTokenKind;
begin
  if KeyComp('H5') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func14: TtkTokenKind;
begin
  if KeyComp('H6') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func16: TtkTokenKind;
begin
  if KeyComp('DL') or KeyComp('P') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func17: TtkTokenKind;
begin
  if KeyComp('KBD') or KeyComp('Q') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func18: TtkTokenKind;
begin
  if KeyComp('BIG') or KeyComp('EM') or KeyComp('HEAD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func19: TtkTokenKind;
begin
  if KeyComp('S') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func20: TtkTokenKind;
begin
  if KeyComp('BR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func21: TtkTokenKind;
begin
  if KeyComp('DEL') or KeyComp('LI') or KeyComp('U') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func23: TtkTokenKind;
begin
  if KeyComp('ABBR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func24: TtkTokenKind;
begin
  if KeyComp('DFN') or KeyComp('DT') or KeyComp('TD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func25: TtkTokenKind;
begin
  if KeyComp('AREA') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func26: TtkTokenKind;
begin
  if KeyComp('HR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func27: TtkTokenKind;
begin
  if KeyComp('BASE') or KeyComp('CODE') or KeyComp('OL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func28: TtkTokenKind;
begin
  if KeyComp('TH') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func29: TtkTokenKind;
begin
  if KeyComp('EMBED') or KeyComp('IMG') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func30: TtkTokenKind;
begin
  if KeyComp('COL') or KeyComp('MAP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func31: TtkTokenKind;
begin
  if KeyComp('DIR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func32: TtkTokenKind;
begin
  if KeyComp('LABEL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func33: TtkTokenKind;
begin
  if KeyComp('UL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func35: TtkTokenKind;
begin
  if KeyComp('DIV') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func37: TtkTokenKind;
begin
  if KeyComp('CITE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func38: TtkTokenKind;
begin
  if KeyComp('THEAD') or KeyComp('TR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func39: TtkTokenKind;
begin
  if KeyComp('META') or KeyComp('PRE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func40: TtkTokenKind;
begin
  if KeyComp('TABLE') or KeyComp('TT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func41: TtkTokenKind;
begin
  if KeyComp('var') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func42: TtkTokenKind;
begin
  if KeyComp('INS') or KeyComp('SUB') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func43: TtkTokenKind;
begin
  if KeyComp('FRAME') or KeyComp('WBR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func46: TtkTokenKind;
begin
  if KeyComp('BODY') or KeyComp('LINK') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func47: TtkTokenKind;
begin
  if KeyComp('LEGend') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func48: TtkTokenKind;
begin
  if KeyComp('BLINK') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func49: TtkTokenKind;
begin
  if KeyComp('NOBR') or KeyComp('PARAM') or KeyComp('SAMP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func50: TtkTokenKind;
begin
  if KeyComp('SPAN') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func52: TtkTokenKind;
begin
  if KeyComp('FORM') or KeyComp('IFRAME') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func53: TtkTokenKind;
begin
  if KeyComp('HTML') or KeyComp('MENU') or KeyComp('XMP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func55: TtkTokenKind;
begin
  if KeyComp('FONT') or KeyComp('object') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func56: TtkTokenKind;
begin
  if KeyComp('SUP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func57: TtkTokenKind;
begin
  if KeyComp('SMALL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func58: TtkTokenKind;
begin
  if KeyComp('NOEMBED') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func61: TtkTokenKind;
begin
  if KeyComp('LAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func62: TtkTokenKind;
begin
  if KeyComp('SPACER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func64: TtkTokenKind;
begin
  if KeyComp('SELECT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func65: TtkTokenKind;
begin
  if KeyComp('CENTER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func66: TtkTokenKind;
begin
  if KeyComp('TBODY') or KeyComp('TITLE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func67: TtkTokenKind;
begin
  if KeyComp('KEYGEN') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func70: TtkTokenKind;
begin
  if KeyComp('ADDRESS') or KeyComp('APPLET') or KeyComp('ILAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func76: TtkTokenKind;
begin
  if KeyComp('NEXTID') or KeyComp('TFOOT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func78: TtkTokenKind;
begin
  if KeyComp('CAPTION') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func80: TtkTokenKind;
begin
  if KeyComp('FIELDSET') or KeyComp('INPUT') or KeyComp('MARQUEE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func81: TtkTokenKind;
begin
  if KeyComp('STYLE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func82: TtkTokenKind;
begin
  if KeyComp('BASEFONT') or KeyComp('BGSOUND') or KeyComp('STRIKE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func83: TtkTokenKind;
begin
  if KeyComp('COMMENT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func84: TtkTokenKind;
begin
  if KeyComp('ISINDEX') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func85: TtkTokenKind;
begin
  if KeyComp('SCRIPT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func87: TtkTokenKind;
begin
  if KeyComp('SERVER') or KeyComp('FRAMESET') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func89: TtkTokenKind;
begin
  if KeyComp('ACRONYM') or KeyComp('OPTION') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func90: TtkTokenKind;
begin
  if KeyComp('LISTING') or KeyComp('NOLAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func91: TtkTokenKind;
begin
  if KeyComp('NOFRAMES') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func92: TtkTokenKind;
begin
  if KeyComp('BUTTON') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func93: TtkTokenKind;
begin
  if KeyComp('STRONG') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func94: TtkTokenKind;
begin
  if KeyComp('TEXTAREA') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func105: TtkTokenKind;
begin
  if KeyComp('MULTICOL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func107: TtkTokenKind;
begin
  if KeyComp('COLGROUP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func114: TtkTokenKind;
begin
  if KeyComp('NOSCRIPT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func121: TtkTokenKind;
begin
  if KeyComp('BLOCKQUOTE') or KeyComp('PLAINTEXT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func123: TtkTokenKind;
begin
  if KeyComp('/A') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func124: TtkTokenKind;
begin
  if KeyComp('/B') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func130: TtkTokenKind;
begin
  if KeyComp('/DD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func131: TtkTokenKind;
begin
  if KeyComp('/I') or KeyComp('/H1') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func132: TtkTokenKind;
begin
  if KeyComp('/H2') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func133: TtkTokenKind;
begin
  if KeyComp('/H3') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func134: TtkTokenKind;
begin
  if KeyComp('/H4') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func135: TtkTokenKind;
begin
  if KeyComp('/H5') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func136: TtkTokenKind;
begin
  if KeyComp('/H6') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func138: TtkTokenKind;
begin
  if KeyComp('/DL') or KeyComp('/P') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func139: TtkTokenKind;
begin
  if KeyComp('/KBD') or KeyComp('/Q') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func140: TtkTokenKind;
begin
  if KeyComp('/BIG') or KeyComp('/EM') or KeyComp('/HEAD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func141: TtkTokenKind;
begin
  if KeyComp('/S') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func143: TtkTokenKind;
begin
  if KeyComp('/DEL') or KeyComp('/LI') or KeyComp('/U') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func145: TtkTokenKind;
begin
  if KeyComp('/ABBR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func146: TtkTokenKind;
begin
  if KeyComp('/DFN') or KeyComp('/DT') or KeyComp('/TD') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func149: TtkTokenKind;
begin
  if KeyComp('/CODE') or KeyComp('/OL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func150: TtkTokenKind;
begin
  if KeyComp('/TH') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func151: TtkTokenKind;
begin
  if KeyComp('/EMBED') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func152: TtkTokenKind;
begin
  if KeyComp('/MAP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func153: TtkTokenKind;
begin
  if KeyComp('/DIR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func154: TtkTokenKind;
begin
  if KeyComp('/LABEL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func155: TtkTokenKind;
begin
  if KeyComp('/UL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func157: TtkTokenKind;
begin
  if KeyComp('/DIV') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func159: TtkTokenKind;
begin
  if KeyComp('/CITE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func160: TtkTokenKind;
begin
  if KeyComp('/THEAD') or KeyComp('/TR') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func161: TtkTokenKind;
begin
  if KeyComp('/PRE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func162: TtkTokenKind;
begin
  if KeyComp('/TABLE') or KeyComp('/TT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func163: TtkTokenKind;
begin
  if KeyComp('/var') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func164: TtkTokenKind;
begin
  if KeyComp('/INS') or KeyComp('/SUB') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func168: TtkTokenKind;
begin
  if KeyComp('/BODY') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func169: TtkTokenKind;
begin
  if KeyComp('/LEGend') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func170: TtkTokenKind;
begin
  if KeyComp('/BLINK') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func171: TtkTokenKind;
begin
  if KeyComp('/NOBR') or KeyComp('/SAMP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func172: TtkTokenKind;
begin
  if KeyComp('/SPAN') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func174: TtkTokenKind;
begin
  if KeyComp('/FORM') or KeyComp('/IFRAME') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func175: TtkTokenKind;
begin
  if KeyComp('/HTML') or KeyComp('/MENU') or KeyComp('/XMP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func177: TtkTokenKind;
begin
  if KeyComp('/FONT') or KeyComp('/object') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func178: TtkTokenKind;
begin
  if KeyComp('/SUP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func179: TtkTokenKind;
begin
  if KeyComp('/SMALL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func180: TtkTokenKind;
begin
  if KeyComp('/NOEMBED') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func183: TtkTokenKind;
begin
  if KeyComp('/LAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func186: TtkTokenKind;
begin
  if KeyComp('/SELECT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func187: TtkTokenKind;
begin
  if KeyComp('/CENTER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func188: TtkTokenKind;
begin
  if KeyComp('/TBODY') or KeyComp('/TITLE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func192: TtkTokenKind;
begin
  if KeyComp('/ADDRESS') or KeyComp('/APPLET') or KeyComp('/ILAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func198: TtkTokenKind;
begin
  if KeyComp('/TFOOT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func200: TtkTokenKind;
begin
  if KeyComp('/CAPTION') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func202: TtkTokenKind;
begin
  if KeyComp('/FIELDSET') or KeyComp('/MARQUEE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func203: TtkTokenKind;
begin
  if KeyComp('/STYLE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func204: TtkTokenKind;
begin
  if KeyComp('/STRIKE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func205: TtkTokenKind;
begin
  if KeyComp('/COMMENT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func207: TtkTokenKind;
begin
  if KeyComp('/SCRIPT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func209: TtkTokenKind;
begin
  if KeyComp('/FRAMESET') or KeyComp('/SERVER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func211: TtkTokenKind;
begin
  if KeyComp('/ACRONYM') or KeyComp('/OPTION') or KeyComp('!DOCTYPE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func212: TtkTokenKind;
begin
  if KeyComp('/LISTING') or KeyComp('/NOLAYER') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func213: TtkTokenKind;
begin
  if KeyComp('/NOFRAMES') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func214: TtkTokenKind;
begin
  if KeyComp('/BUTTON') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func215: TtkTokenKind;
begin
  if KeyComp('/STRONG') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func216: TtkTokenKind;
begin
  if KeyComp('/TEXTAREA') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func227: TtkTokenKind;
begin
  if KeyComp('/MULTICOL') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func229: TtkTokenKind;
begin
  if KeyComp('/COLGROUP') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func236: TtkTokenKind;
begin
  if KeyComp('/NOSCRIPT') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.Func243: TtkTokenKind;
begin
  if KeyComp('/BLOCKQUOTE') then
  begin
    Result := tkKey;
  end
  else
  begin
    Result := tkUndefKey;
  end;
end;

function THighlightHTML.AltFunc: TtkTokenKind;
begin
  Result := tkUndefKey;
end;

procedure THighlightHTML.MakeMethodTables;
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
          fProcTable[i] := LFProc;
        end;
      #13:
        begin
          fProcTable[i] := CRProc;
        end;
      #1..#9, #11, #12, #14..#32:
        begin
          fProcTable[i] := SpaceProc;
        end;
      '&':
        begin
          fProcTable[i] := AmpersandProc;
        end;
      '"':
        begin
          fProcTable[i] := StringProc;
        end;
      '<':
        begin
          fProcTable[i] := BraceOpenProc;
        end;
      '>':
        begin
          fProcTable[i] := BraceCloseProc;
        end;
      '=':
        begin
          fProcTable[i] := EqualProc;
        end;
    else
      fProcTable[i] := IdentProc;
    end;
  end;
end;

constructor THighlightHTML.Create;
begin
  inherited Create;
  InitIdent;
  MakeMethodTables;
  CreateColorTable([clGreen, clMaroon, clBlack, clBlue, clBlack, clGreen,
    clBlue, clBlack, clRed, clBlack, clBlack]);
end;

procedure THighlightHTML.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure THighlightHTML.ASPProc;
begin
  fTokenID := tkASP;
  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '%')
      then
    begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightHTML.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  Inc(Run);
end;

procedure THighlightHTML.CommentProc;
begin
  fTokenID := tkComment;

  if (fLine[Run] in [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '-') and (fLine[Run - 2] = '-')
      then
    begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure THighlightHTML.BraceOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '!') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-')
    then
  begin
    fRange := rsComment;
    fTokenID := tkComment;
    Inc(Run, 3);
  end
  else
  begin

    if fLine[Run] = '%' then
    begin
      fRange := rsASP;
      fTokenID := tkASP;
      Inc(Run);
    end
    else
    begin
      fRange := rsKey;
      fTokenID := tkSymbol;
    end;

  end;

end;

procedure THighlightHTML.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure THighlightHTML.EqualProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

function THighlightHTML.IdentKind(MayBe: PChar): TtkTokenKind;
var
  hashKey: Integer;
begin
  fToIdent := MayBe;
  hashKey := KeyHash(MayBe);
  if (hashKey < 244) then
  begin
    Result := fIdentFuncTable[hashKey];
  end
  else
  begin
    Result := tkIdentifier;
  end;
end;

procedure THighlightHTML.IdentProc;
begin
  case fRange of
    rsKey:
      begin
        fRange := rsParam;
        fTokenID := IdentKind((fLine + Run));
        Inc(Run, fStringLen);
      end;
    rsValue:
      begin
        fRange := rsParam;
        fTokenID := tkValue;
        repeat
          Inc(Run);
        until (fLine[Run] in [#0..#32, '>']);
      end;
  else
    fTokenID := tkIdentifier;
    repeat
      Inc(Run);
    until (fLine[Run] in [#0..#32, '=', '"', '>']);
  end;
end;

procedure THighlightHTML.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure THighlightHTML.NullProc;
begin
  fTokenID := tkNull;
end;

procedure THighlightHTML.TextProc;
const
  StopSet = [#0..#31, '<', '&'];
var
  i: Integer;
begin
  if fLine[Run] in (StopSet - ['&']) then
  begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkText;
  while True do
  begin
    while not (fLine[Run] in StopSet) do
      Inc(Run);

    if (fLine[Run] = '&') then
    begin
      for i := Low(EscapeAmps) to High(EscapeAmps) do
      begin
        if (StrLIComp((fLine + Run), PChar(EscapeAmps[i]), StrLen(EscapeAmps[i])) = 0) then
        begin
          fAndCode := i;
          fRange := rsAmpersand;
          Exit;
        end;
      end;

      Inc(Run);
    end
    else
    begin
      Break;
    end;
  end;

end;

procedure THighlightHTML.AmpersandProc;
begin
  case fAndCode of
    Low(EscapeAmps)..High(EscapeAmps):
      begin
        fTokenID := tkAmpersand;
        Inc(Run, StrLen(EscapeAmps[fAndCode]));
      end;
  end;
  fAndCode := -1;
  fRange := rsText;
end;

procedure THighlightHTML.SpaceProc;
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

procedure THighlightHTML.StringProc;
begin
  if (fRange = rsValue) then
  begin
    fRange := rsParam;
    fTokenID := tkValue;
  end
  else
  begin
    fTokenID := tkString;
  end;
  Inc(Run); // first '"'
  while not (fLine[Run] in [#0, #10, #13, '"']) do
    Inc(Run);
  if fLine[Run] = '"' then
    Inc(Run); // last '"'
end;

procedure THighlightHTML.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsText:
      begin
        TextProc;
      end;
    rsComment:
      begin
        CommentProc;
      end;
    rsASP:
      begin
        ASPProc;
      end;
  else
    fProcTable[fLine[Run]];
  end;
end;

function THighlightHTML.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function THighlightHTML.GetToken: string;
var
  len: Longint;
begin
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function THighlightHTML.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function THighlightHTML.GetTokenAttribute: integer;
begin
  case fTokenID of
    tkAmpersand: Result := 11;
    tkASP: Result := 1;
    tkComment: Result := 2;
    tkIdentifier: Result := 3;
    tkKey: Result := 4;
    tkSpace: Result := 5;
    tkString: Result := 6;
    tkSymbol: Result := 7;
    tkText: Result := 8;
    tkUndefKey: Result := 9;
    tkValue: Result := 10;
  else
    Result := 11;
  end;

end;

function THighlightHTML.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function THighlightHTML.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function THighlightHTML.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure THighlightHTML.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure THighlightHTML.ReSetRange;
begin
  fRange := rsText;
end;

function THighlightHTML.PrepareOutput(Attr: integer; AToken: string): string;
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

procedure THighlightHTML.PrepareToken(var AToken: string);
begin
  AToken := StringReplace(AToken, '\', '\\', [rfReplaceAll]);
  AToken := StringReplace(AToken, '{', '\{', [rfReplaceAll]);
  AToken := StringReplace(AToken, '}', '\}', [rfReplaceAll]);
end;

procedure THighlightHTML.SetupDefaultColors;
begin
  CreateColorTable([clGreen, clMaroon, clBlack, clBlue, clBlack, clGreen,
    clBlue, clBlack, clRed, clBlack, clBlack]);
end;

initialization
  MakeIdentTable;
end.
