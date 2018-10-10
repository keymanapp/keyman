(*
  Name:             Unicode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add Uni_UTF32CharToUTF16
                    14 Sep 2006 - mcdurdin - Add IsUTF8Stream, IsUTF16Stream
                    23 Aug 2007 - mcdurdin - I1012 - Add IsUTF16FileEx function
                    14 Sep 2007 - mcdurdin - I1012 - Tweak IsUTF16FileEx to ignore UTF16 BOM in testing if desired
                    27 Mar 2008 - mcdurdin - I1374 - font helper - Uni_IsIgnorable - to avoid testing those chrs in fonts
                    18 Feb 2011 - mcdurdin - I2712 - Replace use of UTF8Encode and UTF8Decode with TNT versions
                    04 Nov 2011 - mcdurdin - I3125 - When invalid supplementary pair found in UTF16_NextChar return UNICODE_NOCHAR instead of 0
                    04 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit Unicode;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows;

{ We use these functions to avoid warnings around explicit string casts.  Can easily therefore audit usage }
function String_AtoU(const S: AnsiString): UnicodeString;  // I3310
function String_UtoA(const S: UnicodeString): AnsiString;  // I3310

function UTF16ToUTF8(s: WideString): ansistring; deprecated 'Use UTF8Encode';  // I3310
function UTF8ToUTF16(s: ansistring): WideString; deprecated 'Use UTF8ToString';  // I3310
function UTF16_String(ch: DWord): WideString; deprecated 'Use TCharacter.ConvertFromUtf32';  // I3310
function UTF8_String(ch: DWord): ansistring; deprecated;  // I3310
function UTF8_NextChar(var s: ansistring): DWord; deprecated;  // I3310
function UTF16_NextChar(var s: WideString): DWord; deprecated;  // I3310
function IsUTF8File(const FFileName: string): Boolean; deprecated;  // I3310
function IsUTF16File(const FFileName: string): Boolean; deprecated;  // I3310
function IsUTF8Stream(Stream: TStream): Boolean; deprecated;  // I3310
function IsUTF16Stream(Stream: TStream): Boolean; deprecated;  // I3310
function IsUTF16FileEx(const FFileName: WideString; FFalseOnBOM: Boolean = False): Boolean; deprecated;  // I3310

// Copies len Unicode codepoints from the UTF-16 string starting at code unit offset n
// Handles surrogate pairs. Returned string length may be between len and len*2
function CopyChar(s: string; n, len: Integer): string;

const UTF8Signature: ansistring = #$EF#$BB#$BF deprecated;  // I3310
const UTF16Signature: ansistring = #$FF#$FE deprecated;  // I3310
const UTF16SignatureW: WideString = #$FEFF deprecated;  // I3310

function Uni_IsSurrogate(ch: DWord): Boolean;
function Uni_IsSurrogate1(ch: WideChar): Boolean;
function Uni_IsSurrogate2(ch: WideChar): Boolean;

function Uni_SurrogateToUTF32(ch, cl: WideChar): DWord;

function Uni_UTF32ToSurrogate1(ch: DWord): WideChar;
function Uni_UTF32ToSurrogate2(ch: DWord): WideChar;
function Uni_UTF32CharToUTF16(ch: DWord): WideString;

function Uni_IsIgnorable(ch: DWord): Boolean;

implementation

function UTF16ToUTF8(s: WideString): ansistring;
begin
  Result := UTF8Encode(s);
end;
(*
//TODO: // TODO: Find new System.UnicodeToUtf8 function that supports surrogates.  bleagh.  Then fixup the Tnt strings unit to use it.
//TODO: // also need to fix problem with inserting SMP characters from Font Helper or Usage Page
var
  n: DWord;
begin
  Result := '';
  n := UTF16_NextChar(s);
  while n <> 0 do
  begin
    Result := Result + UTF8_String(n);
    n := UTF16_NextChar(s);
  end;
end;
*)

function UTF8ToUTF16(s: ansistring): WideString;
begin
  Result := UTF8ToWideString(s);  // I3309
end;

(*
var
  n: DWord;
begin
  Result := '';
  n := UTF8_NextChar(s);
  while n <> 0 do
  begin
    Result := Result + UTF16_String(n);
    n := UTF8_NextChar(s);
  end;
end;
*)

function UTF16_String(ch: DWord): WideString;
begin
  if ch < $10000 then Result := WideChar(ch)
  else
  begin
    Result := '  ';
    Result[1] := WideChar(((ch-$10000) div $400) + $D800);
    Result[2] := WideChar(((ch-$10000) mod $400) + $DC00);
  end;
end;

const
  firstByteMark: array[0..6] of Byte = ($00, $00, $C0, $E0, $F0, $F8, $FC);
  trailingBytesForUTF8: array[0..255] of Byte = (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);
  offsetsFromUTF8: array[0..5] of DWord = (
    $00000000, $00003080, $000E2080,
		$03C82080, $FA082080, $82082080);
  UNI_REPLACEMENT_CHAR: DWord = $0000FFFD;


function UTF8_String(ch: DWord): ansistring;
const
  UNI_SUR_HIGH_START: DWord = $D800;
  UNI_SUR_LOW_END: DWord = $DBFF;
  byteMask: DWord = $BF;
	byteMark: DWord = $80;
var
  firstBM: Byte;
  bytesToWrite: Integer;
begin
  { surrogates of any stripe are not legal UTF32 characters }

  if (ch >= UNI_SUR_HIGH_START) and (ch <= UNI_SUR_LOW_END) then
  begin
    Result := '';
    Exit;
  end;

  { Figure out how many bytes the result will require }
  case ch of
    $0..$7F:         bytesToWrite := 1;
    $80..$7FF:       bytesToWrite := 2;
    $800..$FFFF:     bytesToWrite := 3;
		$10000..$1FFFFF: bytesToWrite := 4;
	else begin bytesToWrite := 2; ch := UNI_REPLACEMENT_CHAR; end;
  end;

  SetLength(Result, bytesToWrite);

  firstBM := firstByteMark[bytesToWrite];
  while bytesToWrite > 0 do
  begin
    if bytesToWrite = 1
      then Result[bytesToWrite] := AnsiChar(ch or firstBM)
      else begin Result[bytesToWrite] := AnsiChar((ch or byteMark) and byteMask); ch := ch shr 6; end;
    Dec(bytesToWrite);
  end;
end;

function isLegalUTF8(ss: ansistring; len: Integer): Boolean;
var
  s: array[1..4] of Integer;
  i: Integer;
begin
  Result := False;
  if Length(s) < len then Exit;

  for i := 1 to len do s[i] := Ord(ss[i]);

  if len = 4 then if (s[len] < $80) or (s[len] > $BF) then Exit else Dec(len);
  if len = 3 then if (s[len] < $80) or (s[len] > $BF) then Exit else Dec(len);
  if len = 2 then
  begin
    if (s[2] > $BF) then Exit;
    case s[1] of
      $E0: if s[2] < $A0 then Exit;
      $F0: if s[2] < $90 then Exit;
      $F4: if s[2] > $8F then Exit;
    else if s[2] < $80 then Exit;
    end;
    Dec(len);
  end;
  if len = 1 then
  begin
    if (s[1] >= $80) and (s[1] < $C2) then Exit;
    if s[1] > $F4 then Exit;
  end;

  Result := True;
end;

function UTF8_NextChar(var s: ansistring): DWord;
const
  UNI_MAX_UTF16: DWord = $10FFFF;
var
  tmpBTR, extraBytesToRead: Integer;
  ch: DWord;
begin
  Result := 0;
  if s = '' then Exit;
  ch := Ord(s[1]);
  extraBytesToRead := trailingBytesForUTF8[ch];
  if Length(s) <= extraBytesToRead then Exit;
  if not isLegalUTF8(s, extraBytesToRead+1) then
  begin
    Delete(s,1,extraBytesToRead+1);
    Result := UTF8_NextChar(s);
    Exit;
  end;

  ch := 0; tmpBTR := extraBytesToRead;
  while tmpBTR >= 0 do
  begin
    ch := ch + Ord(s[1]); Delete(s,1,1);
    if tmpBTR > 0 then ch := ch shl 6;
    Dec(tmpBTR);
  end;
  ch := ch - offsetsFromUTF8[extraBytesToRead];
  if ch <= UNI_MAX_UTF16 then Result := ch
  else Result := UNI_REPLACEMENT_CHAR;
end;

function UTF16_NextChar(var s: WideString): DWord;
var
  r: DWord;
begin
  if s = '' then begin Result := 0; Exit; end;
  Result := Ord(s[1]);
  Delete(s,1,1);
  if (Result >= $D800) and (Result < $DC00) then
  begin
    if s = '' then begin Result := 0; Exit; end;
    r := Ord(s[1]); Delete(s,1,1);
    if (r >= $DC00) and (r < $E000)
      then Result := (Result - $D800) * $400 + (r - $DC00) + $10000
      else Result := UNICODE_NOCHAR;   // I3125
  end;
end;

function IsUTF8File(const FFileName: string): Boolean;
var
  tf: TextFile;
  s: ansistring;
begin
  AssignFile(tf, FFileName);
  Reset(tf);
  readln(tf, s);
  CloseFile(tf);
  Result := Copy(s,1,3) = UTF8Signature;
end;

function IsUTF16File(const FFileName: string): Boolean;
var
  tf: TextFile;
  s: ansistring;
begin
  AssignFile(tf, FFileName);
  Reset(tf);
  readln(tf, s);
  CloseFile(tf);
  Result := Copy(s,1,2) = UTF16Signature;
end;

function IsUTF16FileEx(const FFileName: WideString; FFalseOnBOM: Boolean = False): Boolean;
var
  buf: array[0..1024] of ansichar;  // I3310
  len: Integer;
  tests: Integer;
const
  IS_TEXT_UNICODE_UNICODE_MASK = $F;
  IS_TEXT_UNICODE_SIGNATURE = $8;
begin
  with TFileStream.Create(FFileName, fmOpenRead) do
  try
    len := Read(buf[0], 1024);
    tests := IS_TEXT_UNICODE_UNICODE_MASK;
    Result := IsTextUnicode(@buf[0], len, @tests);
    if Result and ((tests AND IS_TEXT_UNICODE_SIGNATURE) = IS_TEXT_UNICODE_SIGNATURE) then Result := not FFalseOnBOM;
  finally
    Free;
  end;
end;

function IsUTF8Stream(Stream: TStream): Boolean;
var
  buf: array[0..2] of ansichar;
  n: Integer;
begin
  n := Stream.Read(buf, 3);
  if n > 0 then Stream.Seek(-n, soFromCurrent);
  if n = 3
    then Result := Copy(buf, 1, 3) = UTF8Signature
    else Result := False;
end;

function IsUTF16Stream(Stream: TStream): Boolean;
var
  buf: array[0..2] of ansichar;
  n: Integer;
begin
  n := Stream.Read(buf, 2);
  if n > 0 then Stream.Seek(-n, soFromCurrent);
  if n = 2
    then Result := Copy(buf, 1, 2) = UTF16Signature
    else Result := False;
end;

function Uni_IsSurrogate(ch: DWord): Boolean;
begin
  Result := ch > $FFFF;
end;

function Uni_IsSurrogate1(ch: WideChar): Boolean;
begin
  Result := (Ord(ch) >= $D800) and (Ord(ch) <= $DBFF);
end;

function Uni_IsSurrogate2(ch: WideChar): Boolean;
begin
  Result := (Ord(ch) >= $DC00) and (Ord(ch) <= $DFFF);
end;

function Uni_SurrogateToUTF32(ch, cl: WideChar): DWord;
begin
  Result := (DWord(Ord(ch))-$D800) * $400 + (DWord(Ord(cl)) - $DC00) + $10000;
end;

function Uni_UTF32ToSurrogate1(ch: DWord): WideChar;
begin
  Result := WideChar((ch - $10000) div $400 + $D800);
end;

function Uni_UTF32ToSurrogate2(ch: DWord): WideChar;
begin
  Result := WideChar((ch - $10000) mod $400 + $DC00);
end;

function Uni_UTF32CharToUTF16(ch: DWord): WideString;
begin
  if ch > $FFFF
    then Result := Uni_UTF32ToSurrogate1(ch) + Uni_UTF32ToSurrogate2(ch)
    else Result := WideChar(ch);
end;

function Uni_IsIgnorable(ch: DWord): Boolean;
type
  TIgnorable = record a,b: DWord; end;
const
  Ignorable: array[0..59] of TIgnorable = (
    (a: $0000; b: $0008  ),  // ; Default_Ignorable_Code_Point # Cc   [9] <control-0000>; b: $<control-0008>

    (a: $0009; b: $000D  ),  //; White_Space # Cc   [5] <control-0009>..<control-000D>
    (a: $000E; b: $001F  ),  // ; Default_Ignorable_Code_Point # Cc  [18] <control-000E>; b: $<control-001F>

    (a: $0020; b: $0020 ),   //       ; White_Space # Zs       SPACE

    (a: $007F; b: $0084  ),  // ; Default_Ignorable_Code_Point # Cc   [6] <control-007F>; b: $<control-0084>
    (a: $0085; b: $0085 ),   //       ; White_Space # Cc       <control-0085>

    (a: $0086; b: $009F  ),  // ; Default_Ignorable_Code_Point # Cc  [26] <control-0086>; b: $<control-009F>
    (a: $00A0; b: $00A0  ),   //          ; White_Space # Zs       NO-BREAK SPACE

    (a: $00AD; b: $00AD        ),  // ; Default_Ignorable_Code_Point # Cf       SOFT HYPHEN
    (a: $034F; b: $034F        ),  // ; Default_Ignorable_Code_Point # Mn       COMBINING GRAPHEME JOINER
    (a: $0600; b: $0603  ),  // ; Default_Ignorable_Code_Point # Cf   [4] ARABIC NUMBER SIGN; b: $ARABIC SIGN SAFHA
    (a: $06DD; b: $06DD        ),  // ; Default_Ignorable_Code_Point # Cf       ARABIC END OF AYAH
    (a: $070F; b: $070F        ),  // ; Default_Ignorable_Code_Point # Cf       SYRIAC ABBREVIATION MARK
    (a: $115F; b: $1160  ),  // ; Default_Ignorable_Code_Point # Lo   [2] HANGUL CHOSEONG FILLER; b: $HANGUL JUNGSEONG FILLER
    (a: $1680; b: $1680 ),   //       ; White_Space # Zs       OGHAM SPACE MARK

    (a: $17B4; b: $17B5  ),  // ; Default_Ignorable_Code_Point # Cf   [2] KHMER VOWEL INHERENT AQ; b: $KHMER VOWEL INHERENT AA
    (a: $180B; b: $180D  ),  // ; Default_Ignorable_Code_Point # Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE; b: $MONGOLIAN FREE VARIATION SELECTOR THREE
    (a: $180E; b: $180E ), //          ; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
    (a: $2000; b: $200A ), //   ; White_Space # Zs  [11] EN QUAD..HAIR SPACE

    (a: $200B; b: $200F  ),  // ; Default_Ignorable_Code_Point # Cf   [5] ZERO WIDTH SPACE; b: $RIGHT-TO-LEFT MARK
    (a: $2028; b: $2028  ),  //    ; White_Space # Zl       LINE SEPARATOR
    (a: $2029; b: $2029  ),  //          ; White_Space # Zp       PARAGRAPH SEPARATOR

    (a: $202A; b: $202E  ),  // ; Default_Ignorable_Code_Point # Cf   [5] LEFT-TO-RIGHT EMBEDDING; b: $RIGHT-TO-LEFT OVERRIDE
    (a: $202F; b: $202F  ), //          ; White_Space # Zs       NARROW NO-BREAK SPACE

    (a: $205F; b: $205F  ),  //          ; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
    (a: $2060; b: $2063  ),  // ; Default_Ignorable_Code_Point # Cf   [4] WORD JOINER; b: $INVISIBLE SEPARATOR
    (a: $2064; b: $2069  ),  // ; Default_Ignorable_Code_Point # Cn   [6] <reserved-2064>; b: $<reserved-2069>
    (a: $206A; b: $206F  ),  // ; Default_Ignorable_Code_Point # Cf   [6] INHIBIT SYMMETRIC SWAPPING; b: $NOMINAL DIGIT SHAPES
    (a: $3000; b: $3000  ), //          ; White_Space # Zs       IDEOGRAPHIC SPACE
    (a: $3164; b: $3164        ),  // ; Default_Ignorable_Code_Point # Lo       HANGUL FILLER
    (a: $D800; b: $DFFF  ),  // ; Default_Ignorable_Code_Point # Cs [2048] <surrogate-D800>; b: $<surrogate-DFFF>
    (a: $FDD0; b: $FDEF  ),  // ; Default_Ignorable_Code_Point # Cn  [32] <noncharacter-FDD0>; b: $<noncharacter-FDEF>
    (a: $FE00; b: $FE0F  ),  // ; Default_Ignorable_Code_Point # Mn  [16] VARIATION SELECTOR-1; b: $VARIATION SELECTOR-16
    (a: $FEFF; b: $FEFF        ),  // ; Default_Ignorable_Code_Point # Cf       ZERO WIDTH NO-BREAK SPACE
    (a: $FFA0; b: $FFA0        ),  // ; Default_Ignorable_Code_Point # Lo       HALFWIDTH HANGUL FILLER
    (a: $FFF0; b: $FFF8  ),  // ; Default_Ignorable_Code_Point # Cn   [9] <reserved-FFF0>; b: $<reserved-FFF8>
    (a: $FFFE; b: $FFFF  ),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-FFFE>; b: $<noncharacter-FFFF>
    (a: $1D173; b: $1D17A),  // ; Default_Ignorable_Code_Point # Cf   [8] MUSICAL SYMBOL BEGIN BEAM; b: $MUSICAL SYMBOL END PHRASE
    (a: $1FFFE; b: $1FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-1FFFE>; b: $<noncharacter-1FFFF>
    (a: $2FFFE; b: $2FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-2FFFE>; b: $<noncharacter-2FFFF>
    (a: $3FFFE; b: $3FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-3FFFE>; b: $<noncharacter-3FFFF>
    (a: $4FFFE; b: $4FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-4FFFE>; b: $<noncharacter-4FFFF>
    (a: $5FFFE; b: $5FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-5FFFE>; b: $<noncharacter-5FFFF>
    (a: $6FFFE; b: $6FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-6FFFE>; b: $<noncharacter-6FFFF>
    (a: $7FFFE; b: $7FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-7FFFE>; b: $<noncharacter-7FFFF>
    (a: $8FFFE; b: $8FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-8FFFE>; b: $<noncharacter-8FFFF>
    (a: $9FFFE; b: $9FFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-9FFFE>; b: $<noncharacter-9FFFF>
    (a: $AFFFE; b: $AFFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-AFFFE>; b: $<noncharacter-AFFFF>
    (a: $BFFFE; b: $BFFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-BFFFE>; b: $<noncharacter-BFFFF>
    (a: $CFFFE; b: $CFFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-CFFFE>; b: $<noncharacter-CFFFF>
    (a: $DFFFE; b: $E0000),  // ; Default_Ignorable_Code_Point # Cn   [3] <noncharacter-DFFFE>; b: $<reserved-E0000>
    (a: $E0001; b: $E0001       ),  // ; Default_Ignorable_Code_Point # Cf       LANGUAGE TAG
    (a: $E0002; b: $E001F),  // ; Default_Ignorable_Code_Point # Cn  [30] <reserved-E0002>; b: $<reserved-E001F>
    (a: $E0020; b: $E007F),  // ; Default_Ignorable_Code_Point # Cf  [96] TAG SPACE; b: $CANCEL TAG
    (a: $E0080; b: $E00FF),  // ; Default_Ignorable_Code_Point # Cn [128] <reserved-E0080>; b: $<reserved-E00FF>
    (a: $E0100; b: $E01EF),  // ; Default_Ignorable_Code_Point # Mn [240] VARIATION SELECTOR-17; b: $VARIATION SELECTOR-256
    (a: $E01F0; b: $E0FFF),  // ; Default_Ignorable_Code_Point # Cn [3600] <reserved-E01F0>; b: $<reserved-E0FFF>
    (a: $EFFFE; b: $EFFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-EFFFE>; b: $<noncharacter-EFFFF>
    (a: $FFFFE; b: $FFFFF),  // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-FFFFE>; b: $<noncharacter-FFFFF>
    (a: $10FFFE; b: $10FFFF));
var
  I: Integer; // ; Default_Ignorable_Code_Point # Cn   [2] <noncharacter-10FFFE>; b: $<noncharacter-10FFFF>

begin
  for I := 0 to High(Ignorable) do
    if Ignorable[I].b >= ch then
    begin
      Result := Ignorable[I].a <= ch;
      Exit;
    end;
  Result := True;
end;

{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN EXPLICIT_STRING_CAST OFF}
function String_AtoU(const S: AnsiString): UnicodeString;  // I3310
begin
  Result := UnicodeString(S);
end;

function String_UtoA(const S: UnicodeString): AnsiString;  // I3310
begin
  Result := AnsiString(S);
end;
{$WARN EXPLICIT_STRING_CAST_LOSS DEFAULT}
{$WARN EXPLICIT_STRING_CAST DEFAULT}

function CopyChar(s: string; n, len: Integer): string;
var
  ch: Char;
begin
  Result := '';

  if n <= 0 then
    Exit('');

  while len > 0 do
  begin
    if n > Length(s) then
      Exit;
    ch := s[n];
    if Uni_IsSurrogate1(ch) and (n < Length(s)) and Uni_IsSurrogate2(s[n+1]) then
    begin
      Inc(n);
      Result := Result + ch + s[n];
    end
    else
      Result := Result + ch;
    Inc(n);
    Dec(len);
  end;
end;

end.

