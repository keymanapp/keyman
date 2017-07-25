(*
  Name:             ttinfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2006

  Modified Date:    4 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Oct 2006 - mcdurdin - Rework for cleaner CMap reading
                    22 Jan 2007 - mcdurdin - Add support for .ttc files
                    27 Mar 2008 - mcdurdin - I1301 - Fixup supplementary plane font view in Charmap
                    18 Mar 2011 - mcdurdin - I2794 - Handle leaks
                    04 Nov 2011 - mcdurdin - I3124 - Add support for reading true type font by font name
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit TTInfo;

interface

uses Windows, Classes, SysUtils, Graphics;

type
  TTTHeader = record
    iTTVersion: Integer;
    iHeaderOffset, iHeaderLength: Integer;
    iNameOffset, iNameLength: Integer;
    iCMapOffset, iCMapLength: Integer;
    iMetricsLength: Integer;
    iNumTables: Integer;
    iNumStrings: Integer;
    iStringsOffset: Integer;
    iTag: Integer;
    iStringOffset: array[0..7] of Integer;
    iStringLength: array[0..7] of Integer;
    iPlatformID, iEncodingID, iLanguageID: Integer;
    iNameID: Integer;
  end;

  TTTInfoFlags = set of (tfNames, tfCMap, tfNameIsFontName);  // I3124

  TTTCMapTableItem = record
    StartCode, EndCode: Integer;
  end;

  TTTCMapTable = class
  private
    Ranges: array of TTTCMapTableItem;
    function GetCount: Integer;
    function GetItem(Index: Integer): TTTCMapTableItem;
  public
    constructor Create;
    procedure Assign(Source: TTTCMapTable);
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TTTCMapTableItem read GetItem;
  end;

  TTTInfo = class
  private
    procedure ReadCMap12;
    procedure ReadCMap4;
    function LoadFontToStream(AFontName: string): TMemoryStream;  // I3124
  protected
    fs: TStream;
    hdr: TTTHeader;
    FIsGlyphIndex: Boolean;

    procedure ReadNames;
  protected
    sFileName: string;  // I3310
    iTTVersion: Integer;
    iDirectionality: Integer;
    sCopyright, sFamilyName, sStyle, sUniqueID, sFullName,
    sFontVersion, sPostscriptName, sTrademark: WideString;

    function ReadBELong(fs: TStream): Integer;
    function ReadBEShort(fs: TStream): Word;
    procedure ReverseChars(var buf: array of WideChar);

  public
    CMapTable: TTTCMapTable;
    constructor Create(const AFileName: string; AFlags: TTTInfoFlags);
    destructor Destroy; override;

    property Copyright: WideString read sCopyright;
    property FamilyName: WideString read sFamilyName;
    property Style: WideString read sStyle;
    property UniqueID: WideString read sUniqueID;
    property FullName: WideString read sFullName;
    property FontVersion: WideString read sFontVersion;
    property PostscriptName: WideString read sPostscriptName;
    property Trademark: WideString read sTrademark;

    property Directionality: Integer read iDirectionality;
    property TTVersion: Integer read iTTVersion;
    property FileName: string read sFileName;  // I3310

    procedure ReadCMap;
  end;

  TTTInfoFromFont = class(TTTInfo)
  public
    constructor Create(const AFontName: string);
  end;

implementation

{ TTTInfo }

function TTTInfo.LoadFontToStream(AFontName: string): TMemoryStream;  // I3124
var
  oldfont, font, dc: THandle;
  FSize: Cardinal;
begin
  dc := GetDC(0);
  if dc = 0 then
    RaiseLastOSError;
  try
    font := CreateFont(0, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH or FF_DONTCARE, PChar(AFontName));
    if font = 0 then
      RaiseLastOSError;
    oldfont := SelectObject(dc, font);
    try
      FSize := GetFontData(dc, 0, 0, nil, 0);
      if (FSize = GDI_ERROR) or (FSize = 0) then
        RaiseLastOSError;
      Result := TMemoryStream.Create;
      Result.SetSize(FSize);
      FSize := GetFontData(dc, 0, 0, Result.Memory, FSize);
      if (FSize = GDI_ERROR) or (FSize = 0) then
      begin
        Result.Free;
        RaiseLastOSError;
      end;
    finally
      SelectObject(dc, oldfont);
      DeleteObject(font);
    end;
  finally
    ReleaseDC(0, dc);
  end;
end;

constructor TTTInfo.Create(const AFileName: string; AFlags: TTTInfoFlags);
var
  i : Integer;
begin
  inherited Create;

  CMapTable := nil;

  if tfNameIsFontName in AFlags then  // I3124
  begin
    fs := LoadFontToStream(AFileName);
  end
  else
    fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

  sFileName := AFileName;
  hdr.iTTVersion := ReadBELong(fs);

  if hdr.iTTVersion = $74746366 then
  begin
    // A TTC file
    ReadBELong(fs); // TTC header version
    i := ReadBELong(fs); // number of fonts
    if i = 0 then
      raise Exception.Create('Invalid TTC file');
    i := ReadBELong(fs);
    fs.Seek(i, soFromBeginning);
    hdr.iTTVersion := ReadBELong(fs);
  end;

  hdr.iNumTables := ReadBEShort(fs);

  //Skip the rest of the Offset table
  fs.Seek(6, soFromCurrent);

  hdr.iNameLength := -1;
  hdr.iHeaderLength := -1;
  hdr.iMetricsLength := -1;
  hdr.iNameOffset := -1;
  hdr.iHeaderOffset := -1;

  //Now read the Table Directory
  for i := 1 to hdr.iNumTables do
  begin
      hdr.iTag := ReadBELong(fs);
      if hdr.iTag = $6E616D65 then    //name table
      begin
          fs.Seek(4, soFromCurrent);
          hdr.iNameOffset := ReadBELong(fs);
          hdr.iNameLength := ReadBELong(fs);
      end
      else if hdr.iTag = $68656164 then   //head table
      begin
          fs.Seek(4, soFromCurrent);
          hdr.iHeaderOffset := ReadBELong(fs);
          hdr.iHeaderLength := ReadBELong(fs);
      end
      else if hdr.iTag = $4F532F32 then   //OS/2 table
      begin
          fs.Seek( 4, soFromCurrent );
          {iMetricsOffset :=} ReadBELong(fs);
          hdr.iMetricsLength := ReadBELong(fs);
      end
      else if hdr.iTag = $636D6170 then   //cmap table
      begin
          fs.Seek( 4, soFromCurrent );
          hdr.iCMapOffset := ReadBELong(fs);
          hdr.iCMapLength := ReadBELong(fs);
      end
      else
          fs.Seek(12, soFromCurrent);
  end;

  if (hdr.iNameLength = -1) or (hdr.iHeaderLength = -1) or (hdr.iMetricsLength = -1) then
      raise Exception.Create('Invalid TrueType Font File');

  //Read the Directionality
  fs.Seek(hdr.iHeaderOffset + 32, soFromBeginning);
  iDirectionality := ReadBELong(fs);

  if tfNames in AFlags then ReadNames;
  if tfCMap in AFlags then ReadCMap;
end;

destructor TTTInfo.Destroy;
begin
  fs.Free;
  CMapTable.Free;
  inherited Destroy;
end;

procedure TTTInfo.ReverseChars(var buf: array of WideChar);   // Swaps endianness of words - slowly...
var
    p, q: PAnsiChar;
    c: AnsiChar;
begin
    p := PAnsiChar(@buf); q := p; Inc(q);  // I3310
    while (p^ <> #0) or (q^ <> #0) do
    begin
        c := p^; p^ := q^; q^ := c;
        Inc(p, 2);
        Inc(q, 2);
    end;
end;

procedure TTTInfo.ReadNames;
var
  i, iNameID: Integer;
  cBuf: array[0..255] of WideChar;
begin
  //Read the Naming Table

  fs.Seek(hdr.iNameOffset + 2, soFromBeginning);
  hdr.iNumStrings := ReadBEShort(fs);
  hdr.iStringsOffset := ReadBEShort(fs);

  for i := 0 to 7 do hdr.iStringLength[i] := -1;

  for i := 0 to hdr.iNumStrings - 1 do
  begin
    hdr.iPlatformID := ReadBEShort(fs);
    hdr.iEncodingID := ReadBEShort(fs);
    hdr.iLanguageID := ReadBEShort(fs);
    iNameID := ReadBEShort(fs);

    if (iNameID >= 0) and (iNameID <= 7) and {(hdr.iLanguageID = $0409) and}
      {(hdr.iEncodingID = 1) and} (hdr.iPlatformID = 3) then
    begin
      if (hdr.iLanguageID = $0409) or (hdr.iStringLength[iNameID] = -1) then
      begin
        hdr.iStringLength[iNameID] := ReadBEShort(fs);
        hdr.iStringOffset[iNameID] := ReadBEShort(fs);
      end
      else
        fs.Seek(4, soFromCurrent);
    end
    else
      fs.Seek(4, soFromCurrent);
  end;


  //Here we actually read the names
  for i := 0 to 7 do
  begin
    if hdr.iStringLength[i] <> -1 then
    begin
      fs.Seek(hdr.iNameOffset + hdr.iStringsOffset + hdr.iStringOffset[i], soFromBeginning);

      if hdr.iStringLength[i] < 512 then
      begin
        fs.ReadBuffer(cBuf, hdr.iStringLength[i]);
        cBuf[hdr.iStringLength[i] div 2] := #0;
      end
      else
      begin
        fs.ReadBuffer(cBuf, 512);
        fs.Seek(hdr.iStringLength[i] - 512, soFromCurrent);
        cBuf[High(cBuf)] := #0;
      end;

      ReverseChars(cBuf);

      case i of
        0: sCopyright := cBuf;
        1: sFamilyName := cBuf;
        2: sStyle := cBuf;
        3: sUniqueID := cBuf;
        4: sFullName := cBuf;
        5: sFontVersion := cBuf;
        6: sPostscriptName := cBuf;
        7: sTrademark := cBuf;
      end;
    end;
  end;
end;


procedure TTTInfo.ReadCMap;
var
  i, nTables: Integer;
  iPlatformID, iPlatEncodingID, iOffset: Integer;
  FCMap4, FCMap12: Integer;
begin
  fs.Seek(hdr.iCMapOffset + 2, soFromBeginning);

  nTables := ReadBEShort(fs);
  FCMap12 := -1; FCMap4 := -1;

  for i := 0 to nTables - 1 do
  begin
    iPlatformID := ReadBEShort(fs);
    iPlatEncodingID := ReadBEShort(fs);
    iOffset := ReadBELong(fs);

    if (iPlatformID = 3) and (iPlatEncodingID = 1) then FCMap4 := iOffset;
    if (iPlatformID = 3) and (iPlatEncodingID = 10) then FCMap12 := iOffset;
  end;

  if (FCMap4 = -1) and (FCMap12 = -1) then Exit;

  if FCMap12 > -1
    then begin fs.Seek(hdr.iCMapOffset + FCMap12, soFromBeginning); ReadCMap12; end
    else begin fs.Seek(hdr.iCMapOffset + FCMap4, soFromBeginning); ReadCMap4; end;
end;

procedure TTTInfo.ReadCMap12;
var
  i, nGroups: Integer;
begin
  FIsGlyphIndex := True;

  if ReadBEShort(fs) <> 12 then Exit;    // Format 12 is required
  {iReserved := } ReadBEShort(fs);
  {iLength := } ReadBELong(fs);
  {iLanguage := } ReadBELong(fs);

  nGroups := ReadBELong(fs);

  CMapTable := TTTCMapTable.Create;
  SetLength(CMapTable.Ranges, nGroups);

  for i := 0 to nGroups - 1 do
  begin
    CMapTable.Ranges[i].StartCode := ReadBELong(fs);
    CMapTable.Ranges[i].EndCode := ReadBELong(fs);
    {Glyph index := } ReadBELong(fs); // I1301 - Missing long when reading font
  end;
end;

procedure TTTInfo.ReadCMap4;
var
  i: Integer;
  segcountX2, segcount: Integer;
begin
  if ReadBEShort(fs) <> 4 then Exit;    // Format 4 is required
  {iLength := } ReadBEShort(fs);
  //if ReadBEShort(fs) <> 0 then Exit;    // Version 0 is the only version understood
  ReadBEShort(fs);
  segcountX2 := ReadBEShort(fs); segcount := segcountX2 div 2;

  ReadBEShort(fs);  //searchRange
  ReadBEShort(fs);  //entrySelector
  ReadBEShort(fs);  //rangeShift

  CMapTable := TTTCMapTable.Create;
  SetLength(CMapTable.Ranges, segcount);

  for i := 0 to segcount - 1 do
    CMapTable.Ranges[i].EndCode := ReadBEShort(fs);

  ReadBEShort(fs); //reservedPad

  for i := 0 to segcount - 1 do
    CMapTable.Ranges[i].StartCode := ReadBEShort(fs);
end;

function TTTInfo.ReadBELong(fs: TStream): Integer;
type
    PInteger = ^Integer;
var
    buf: array[0..3] of Byte;
    ch: Byte;
begin
    fs.ReadBuffer(buf, 4);
    ch := buf[0]; buf[0] := buf[3]; buf[3] := ch;
    ch := buf[1]; buf[1] := buf[2]; buf[2] := ch;
    Result := PInteger(@buf)^;
end;

function TTTInfo.ReadBEShort(fs: TStream): Word;
type
    PSmallInt = ^SmallInt;
var
    buf: array[0..1] of Byte;
    ch: Byte;
begin
    fs.ReadBuffer(buf, 2);
    ch := buf[0]; buf[0] := buf[1]; buf[1] := ch;
    Result := PWord(@buf)^;
end;

{ TTTInfoFromFont }

constructor TTTInfoFromFont.Create(const AFontName: string);
var
  sz: DWord;
  FHDC: THandle;
  Canvas: TCanvas;
begin
  fs := TMemoryStream.Create;
  Canvas := TCanvas.Create;
  try
    FHDC := GetDC(GetDesktopWindow);  // I2794
    try
      Canvas.Handle := FHDC;
      try
        Canvas.Font.Name := AFontName;

        { Read CMAP }
        sz := GetFontData(Canvas.Handle, $70616D63, 0, nil, 0);
        if sz = GDI_ERROR then Exit;

        fs.Size := sz;
        GetFontData(Canvas.Handle, $70616D63, 0, (fs as TMemoryStream).Memory, sz);
        hdr.iCMapOffset := 0;
        ReadCMap;
      finally
        Canvas.Handle := 0;
      end;
    finally
      ReleaseDC(GetDesktopWindow, FHDC); // I2794
    end;
  finally
    Canvas.Free;
  end;
end;

{ TTTCMapTable }

procedure TTTCMapTable.Assign(Source: TTTCMapTable);
begin
  //Ranges.SetLength(SizeOf(Source.Ranges));
  Ranges := Source.Ranges;
//  for I := 0 to High(Source.Ranges) do
  //  Ranges[I] := Source.Ranges[I];
end;

constructor TTTCMapTable.Create;
begin
  inherited Create;
end;

function TTTCMapTable.GetCount: Integer;
begin
  Result := High(Ranges) + 1;
end;

function TTTCMapTable.GetItem(Index: Integer): TTTCMapTableItem;
begin
  Result := Ranges[Index];
end;

end.

