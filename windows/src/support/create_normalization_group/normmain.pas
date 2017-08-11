(*
  Name:             normmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jul 2008

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jul 2008 - mcdurdin - Initial version
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit normmain;

interface

uses
  Classes,
  kmxfileusedchars,
  StrUtils,
  SysUtils,
  TntClasses,
  Contnrs,
  Windows;

procedure Run;

type
  TUnicodeCharacter = class
    CodeValue: Integer;               // 0 means record not found
    CharacterName: WideString;
    GeneralCategory: string;
    CanonicalCombiningClasses: Integer;
    BidirectionalCategory: string;
    CharacterDecompositionMapping: string; // '' is not specified
    DecimalDigitValue: Integer;       // -1 is not specified
    DigitValue: Integer;              // -1 is not specified
    NumericValue: string;             // -1 is not specified
    Mirrored: Boolean;
    Unicode10Name: string;            // '' is not specified
    _10646Comment: string;            // '' is not specified
    UpperCaseMapping: Integer;        // 0 is not specified
    LowerCaseMapping: Integer;        // 0 is not specified
    TitleCaseMapping: Integer;        // 0 is not specified
  end;

  TUnicodeCharacters = class(TObjectBucketList)
  public
    function FindChar(ch: WideChar; out uc: TUnicodeCharacter): Boolean; overload;
    function FindChar(ch: Integer; out uc: TUnicodeCharacter): Boolean; overload;
    function FindChar(ch: string; out uc: TUnicodeCharacter): Boolean; overload;
  end;
  {private
    function GetItem(Index: Integer): TUnicodeCharacter;
    procedure SetItem(Index: Integer; const Value: TUnicodeCharacter);
  published
  public
    property Items[Index: Integer]: TUnicodeCharacter read GetItem write SetItem; default;
  end;}

type
  TNormalizationGroupGenerator = class
  private
    FKMXChars: WideString;
    FKMXFileName: string;
    FCharNamePrefix: string;
    FUnicodeSourcePath: string;
    FUnicodeCharacters: TUnicodeCharacters;
    procedure ImportChars;
    procedure GenerateDecompositions;
    procedure GenerateOrdering;
    procedure GenerateCompositions;
    procedure LoadKeyboardCharacters;
  public
    constructor Create(AKMXFileName, ACharNamePrefix: string);
    destructor Destroy; override;
    procedure Execute;
    property CharNamePrefix: string read FCharNamePrefix;
  end;

  TUnicodeDataFormat = class
{
    class function GeneralCategory(GC: string): string;
    class function CanonicalCombiningClass(CCC: Integer): string;
    class function BidirectionalCategory(BC: string): string;
    class function CharacterDecompositionMapping(CDM: string): string;
    class function DecimalDigitValue(DDV: Integer): string;
    class function DigitValue(DV: Integer): string;
    class function NumericValue(NV: string): string;
    class function Mirrored(M: Boolean): string;
    class function Unicode10Name(U1N: string): string;
    class function _10646Comment(_1C: string): string;
    class function UpperCaseMapping(UCM: Integer): string;
    class function LowerCaseMapping(LCM: Integer): string;
    class function TitleCaseMapping(LCM: Integer): string;
}
    class function CleanCharacterName(nm: string): string;
  end;


implementation

procedure Run;
var
  CharNamePrefix: string;
  FileName: string;
begin
  if ParamCount < 2 then
  begin
    writeln('Usage: normgrp [-p <CharacterNamePrefix>] -k <KMXFileName>');
    Exit;
  end;

  //if ParamStr(1) <> '-p' then Exit;
  if ParamStr(1) <> '-k' then Exit;

  CharNamePrefix := ''; //ParamStr(2);
  FileName := ParamStr(2);

  with TNormalizationGroupGenerator.Create(FileName, CharNamePrefix) do
  try
    Execute;
  finally
    Free;
  end;
end;

{ TNormalizationGroupGenerator }

constructor TNormalizationGroupGenerator.Create(AKMXFileName, ACharNamePrefix: string);
begin
  inherited Create;
  FKMXFileName := AKMXFileName;
  FCharNamePrefix := ACharNamePrefix;
  FUnicodeCharacters := TUnicodeCharacters.Create;
end;

destructor TNormalizationGroupGenerator.Destroy;
begin
  FUnicodeCharacters.Free;
  inherited Destroy;
end;

procedure TNormalizationGroupGenerator.Execute;
begin
  try
    ImportChars;
  except
          Exit;
  end;
  LoadKeyboardCharacters;
  GenerateDecompositions;
  GenerateOrdering;
  GenerateCompositions;
end;

procedure ReadStreamLine(fs: TFileStream; var s: string);
var
  ch: array[0..100] of ansichar;  // I3310
  str: string;
  {nCR,} iCR, iLF, n: Integer;
begin
  s := '';
  while fs.Position < fs.Size do
  begin
    n := fs.Read(ch, 100);
    str := Copy(ch, 1, n);

    iCR := Pos(#13, str);

    iLF := Pos(#10, str);
    if iLF > 0 then
    begin
      s := s + Copy(str, 1, iLF-1);
      if iCR = iLF then Inc(iLF);
      fs.Seek(-(Length(str)-iLF), soFromCurrent);
      s := AnsiReplaceText(s, #13, '');
      Exit;
    end;
    s := s + str;
  end;
end;

function IsSpecialChar(ch: Integer): Boolean;
begin
  Result :=
    ({(ch >= $0000) and} (ch <= $001F)) or      // Controls  15.1
    ((ch >= $0080) and (ch <= $009F)) or      // Controls  15.1
    ((ch >= $D800) and (ch <= $DFFF)) or      // Surrogates 15.5
    ((ch >= $FE00) and (ch <= $FE0F)) or      // Variation selectors 15.x
    ((ch >= $E0100) and (ch <= $E01EF)) or    // Variation selectors 15.x
    (((ch and $FFFF) = $FFFE) or ((ch and $FFFF) = $FFFF)) or  // Non-characters U+[??]FFFE/FFFF 15.8
    ((ch >= $FDD0) and (ch <= $FDEE)) or // non-characters within Arabic Presentations Forms A block 15.8
    ((ch >= $FFF0) and (ch <= $FFFF)) or      // Specials  15.9
    (ch = $FEFF) or   // BOM reversed 15.9
    ((ch >= $E0000) and (ch <= $E007F));    // TAG characters 15.10
end;

function StrTok(var str: string; const tok: Char; var val: string): Boolean;
var
  n: Integer;
begin
  if str = '' then
    Result := False
  else
  begin
    Result := True;
    n := Pos(tok, str);
    if n = 0 then
    begin
      val := str;
      str := '';
    end
    else
    begin
      val := Copy(str, 1, n-1);
      Delete(str, 1, n);
    end;
  end;
end;

procedure TNormalizationGroupGenerator.ImportChars;
var
  fs: TFileStream;
  charname, s: string;
  val: array[0..14] of string;
  c: TUnicodeCharacter;
  ch, i: Integer;
begin
  fs := TFileStream.Create(FUnicodeSourcePath + 'unicodedata.txt', fmOpenRead); //DBPath+UnicodeDataTxtName, fmOpenRead);
  with fs do
  try
    { Import the charaters }
    while Position < Size do
    begin
      ReadStreamLine(fs, s);

      for i := 0 to 14 do
        StrTok(s, ';', val[i]);

      if Copy(val[1], 1, 1) = '<' then Continue; // not a code point, control or similar

      ch := StrToInt('$'+val[0]);
      if IsSpecialChar(ch) then Continue;

      charname := TUnicodeDataFormat.CleanCharacterName(Copy(val[1], 1, 240));
      if AnsiCompareText(Copy(charname, 1, Length(FCharNamePrefix)), FCharNamePrefix) <> 0 then Continue;

      c := TUnicodeCharacter.Create;
      c.CodeValue := ch;
      c.CharacterName := charname;
      c.GeneralCategory := Copy(val[2], 1, 2);
      if val[3] <> '' then c.CanonicalCombiningClasses := StrToInt(val[3]);
      c.BidirectionalCategory := Copy(val[4], 1, 3);
      c.CharacterDecompositionMapping := Copy(val[5], 1, 255);
      if val[6] <> '' then c.DecimalDigitValue             := StrToInt(val[6]);
      if val[7] <> '' then c.DigitValue                    := StrToInt(val[7]);
      c.NumericValue                  := Copy(val[8], 1, 20);
      if val[9] = '' then val[9] := 'N';
      c.Mirrored                      := UpCase(val[9][1]) = 'Y';
      c.Unicode10Name                 := Copy(val[10], 1, 240);
      c._10646Comment                  := Copy(val[11], 1, 255);
      if val[12] <> '' then       c.UpperCaseMapping              := StrToInt('$'+val[12]);
      if val[13] <> '' then c.LowerCaseMapping              := StrToInt('$'+val[13]);
      if val[14] <> '' then c.TitleCaseMapping              := StrToInt('$'+val[14]);

      FUnicodeCharacters.Add(Pointer(ch), c);
    end;
  finally
    fs.Free;
  end;
end;

procedure TNormalizationGroupGenerator.LoadKeyboardCharacters;
begin
  FKMXChars := KMXFile_GetUsedChars(FKMXFileName);
end;

{ TUnicodeDataFormat }

class function TUnicodeDataFormat.CleanCharacterName(nm: string): string;
var
  i: Integer;
begin
  for i := 1 to Length(nm) do
  begin
    case nm[i] of
      'a'..'z': nm[i] := Upcase(nm[i]);
      'A'..'Z', '0'..'9', '-', '_': ;
      else nm[i] := '_';
    end;
  end;
  Result := nm;
end;

procedure TNormalizationGroupGenerator.GenerateCompositions;
begin
  { run through the list of decompositions }
  
end;

function Unichar(ch: Integer): string; overload;
begin
  Result := 'U+'+IntToHex(ch,4);
end;

function Unichar(ch: WideChar): string; overload;
begin
  Result := Unichar(Ord(ch));
end;

function Unichar(ch: string): string; overload;
begin
  Result := Unichar(StrToInt('$'+ch));
end;

procedure TNormalizationGroupGenerator.GenerateDecompositions;
var
  I: Integer;
  uc: TUnicodeCharacter;
  s: string;
  decomp: string;
  decompch: Integer;
  decompuc: TUnicodeCharacter;
begin
  writeln('group(NormalizeDecompose)');
  writeln;

  { Do this first: Itemize the characters in the keyboard,  }
  I := 1;
  while I <= Length(FKMXChars) do
  begin
    if FUnicodeCharacters.FindChar(FKMXChars[I], uc) then
    begin
      if (uc.CharacterDecompositionMapping <> '') and (Copy(uc.CharacterDecompositionMapping, 1, 1) <> '<') then
      begin
        // We need to decompose this character
        write(Unichar(FKMXChars[I]) + ' > use(NormalizeDecompose)');
        s := uc.CharacterDecompositionMapping;
        while StrTok(s, ' ', decomp) do
        begin
          decompch := StrToInt('$'+decomp);
          write(' '+Unichar(decomp));
          if Pos(WideChar(decompch), FKMXChars) = 0 then
            FKMXChars := FKMXChars + WideChar(decompch);
          if FUnicodeCharacters.FindChar(decompch, decompuc) then
          begin
            if decompuc.CharacterDecompositionMapping <> '' then write(' use(NormalizeDecompose)');
          end;
        end;
        writeln;
      end;
    end;
    Inc(I);
  end;
end;

procedure TNormalizationGroupGenerator.GenerateOrdering;
var
  I: Integer;
  cls: TTntStringList;
  uc: TUnicodeCharacter;
  J: Integer;
begin
  writeln('group(NormalizeOrder)');
  writeln;

  cls := TTntStringList.Create;
  try
    for I := 1 to Length(FKMXChars) do
    begin
      if FUnicodeCharacters.FindChar(FKMXChars[I], uc) then
      begin
        if uc.CanonicalCombiningClasses > 0 then
        begin
          while cls.Count <= uc.CanonicalCombiningClasses do
            cls.Add('');
          cls.Strings[uc.CanonicalCombiningClasses] := cls.Strings[uc.CanonicalCombiningClasses] + FKMXChars[I];
        end;
      end;
    end;

    for I := 1 to cls.Count - 1 do
    begin
      if cls[I] = '' then Continue;

      write('store(NormalizeOrder'+IntToStr(I)+')');
      for J := 1 to Length(cls[I]) do
        write(' '+Unichar(cls[I][J]));
      writeln;
    end;

    for I := 1 to cls.Count - 2 do
    begin
      if cls[I] = '' then Continue;

      for J := I+1 to cls.Count - 1 do
      begin
        if cls[J] = '' then Continue;
        writeln('any(NormalizeOrder'+IntToStr(J)+') any(NormalizeOrder'+IntToStr(I)+') > use(NormalizeOrder) context(2) use(NormalizeOrder) context(1)');
      end;
    end;
  finally
    cls.Free;
  end;
end;

{ TUnicodeCharacters }

function TUnicodeCharacters.FindChar(ch: WideChar;
  out uc: TUnicodeCharacter): Boolean;
begin
  Result := FindChar(Ord(ch), uc);
end;

function TUnicodeCharacters.FindChar(ch: Integer;
  out uc: TUnicodeCharacter): Boolean;
var
  data: Pointer;
begin
  Result := Find(Pointer(ch), data);
  if Result
    then uc := TUnicodeCharacter(data)
    else uc := nil;
end;

function TUnicodeCharacters.FindChar(ch: string;
  out uc: TUnicodeCharacter): Boolean;
begin
  Result := FindChar(StrToInt('$'+ch), uc);
end;

end.
