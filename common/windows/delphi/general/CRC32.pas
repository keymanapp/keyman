(*
  Name:             CRC32
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      17 Aug 2012

  Modified Date:    17 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit CRC32;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows;

function CalculateBufferCRC(Count: LongWord; p: PByte): LongWord;  // I3310
function CalculateStringCRC(const s: ansistring): LongWord;  // I3310
function Dehash(s: PByteArray; sz: Integer): string;
procedure Hash(s: string; sz: Integer; outbuf: PByteArray);

function GetCRC32Xor(const filename: string; x: LongWord): LongWord;

implementation

{
 * Instead of performing a straightforward calculation of the 32 bit
 * CRC using a series of logical operations, this program uses the
 * faster table lookup method.  This routine is called once when the
 * program starts up to build the table which will be used later
 * when calculating the CRC values.
}

var
    CRCTable: array[0..255] of LongWord;
    TableBuilt: Boolean = False;

const
    CRC32_POLYNOMIAL: LongWord = $EDB88320;

procedure BuildCRCTable;
var
    i, j: Integer;
    crc: LongWord;
begin
  if not TableBuilt then
		for i := 0 to 255 do
    begin
			crc := i;

			for j := 8 downto 1 do
				if (crc and 1) = 1 then crc := (crc shr 1) xor CRC32_POLYNOMIAL else crc := crc shr 1;

			CRCTable[i] := crc;
		end;
end;


{
 * This routine calculates the CRC for a block of data using the
 * table lookup method. It accepts an original value for the crc,
 * and returns the updated value.
}

function CalculateBufferCRC(Count: LongWord; p: PByte): LongWord;  // I3310
var
    temp1, temp2, crc: LongWord;
begin
    crc := $FFFFFFFF;

    while Count <> 0 do
    begin
        temp1 := (crc shr 8) and $00FFFFFF;
        temp2 := CRCTable[(Integer(crc) xor Integer(p^)) and $ff];
        crc := temp1 xor temp2;
        Inc(p);
        Dec(Count);
    end;

	Result := crc;
end;

function CalculateStringCRC(const s: ansistring): LongWord;  // I3310
begin
    Result := CalculateBufferCRC(Length(s), PByte(PAnsiChar(s)));  // I3310
end;

procedure Hash(s: string; sz: Integer; outbuf: PByteArray);
var
  i: Integer;
begin
  for i := 1 to sz do outbuf[i-1] := Ord(s[i]) xor $6D;
end;

function Dehash(s: PByteArray; sz: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to sz-1 do
    Result := Result + Chr(s[i] xor $6D);
end;


function GetCRC32Xor(const filename: string; x: LongWord): LongWord;
begin
  with TMemoryStream.Create do
  try
    LoadFromFile(filename);
    Result := CalculateBufferCRC(Size, Memory) xor x;
  finally
    Free;
  end;
end;

initialization
    BuildCRCTable;
end.

