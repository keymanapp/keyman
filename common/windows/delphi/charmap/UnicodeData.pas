(*
  Name:             UnicodeData
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    10 Jun 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Rework for Keyman Developer 7 Character map
                    23 Aug 2006 - mcdurdin - Add ShortName property and FindDataByName function
                    23 Aug 2006 - mcdurdin - Add GetUnicodeRangeFromFilter function
                    14 Sep 2006 - mcdurdin - Moved to new location
                    06 Oct 2006 - mcdurdin - Add font ranges
                    06 Oct 2006 - mcdurdin - Fix CRLF issue with importing text files
                    06 Oct 2006 - mcdurdin - Optimize cached characters in database
                    04 Jan 2007 - mcdurdin - Don't change space to underscore when filtering block names
                    23 Aug 2007 - mcdurdin - I1014 - Fix character map database rebuild
                    12 Mar 2010 - mcdurdin - I1891 - Improve character search
                    12 Mar 2010 - mcdurdin - I2210 - Filter should support numeric ranges better
                    18 Mar 2011 - mcdurdin - I2794 - Fix memory leaks
                    18 Mar 2011 - mcdurdin - I2476 - Keyman Developer 8.0 cannot build character map database
                    18 Mar 2011 - mcdurdin - I2299 - Keyman Developer tries to write to UnicodeData.mdb which is stored in Program Files
                    25 Mar 2011 - mcdurdin - I2845 - After fixing Developer Unicode data references, BuildUniData fails due to path overrides
                    04 May 2012 - mcdurdin - I3308 - V9.0 - Start to move towards Delphi namespaces
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    08 Oct 2012 - mcdurdin - I3465 - V9.0 - Filtering is not working in character map
                    10 Jun 2014 - mcdurdin - I4257 - V9.0 - Add ethnologue language codes to unicodedata.mdb
*)
unit UnicodeData;  // I3308  // I3306

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  Winapi.Windows,

  ADODB_TLB,
  ADOX_TLB,
  TTInfo;
  //UfrmUnicodeDataStatus;

type
  TUnicodeCharacter = record
    CodeValue: Integer;               // 0 means record not found
    CharacterName: WideString;
{    GeneralCategory: string;
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
}
  end;

  {TUnicodeChar = record
    Char: DWord; //array[0..2] of WideChar;
  end;}

  //TUnicodeCharArray = array[0..1000000] of TUnicodeChar;
  //PUnicodeCharArray = ^TUnicodeCharArray;//

  TIntegerArray = array[0..1000000] of Integer;
  PIntegerArray = ^TIntegerArray;

  TUnicodeBlock = class
  private
    FName: WideString;
    FBlockID: Integer;
    FEndChar: Integer;
    FCacheCharData: PIntegerArray;
    FTag: Integer;
    FCacheCharCount: Integer;
    FStartChar: Integer;
    FShortName: WideString;
  public
    destructor Destroy; override;
    property BlockID: Integer read FBlockID;
    property Name: WideString read FName;
    property ShortName: WideString read FShortName;
    property StartChar: Integer read FStartChar;
    property EndChar: Integer read FEndChar;
    property CacheCharCount: Integer read FCacheCharCount;
    property CacheCharData: PIntegerArray read FCacheCharData;
    property Tag: Integer read FTag write FTag;
  end;

  TUnicodeBlockList = class(TObjectList)
  private
    FName: WideString;
    FShortName: WideString;
    function GetItem(Index: Integer): TUnicodeBlock;
    procedure SetItem(Index: Integer; const Value: TUnicodeBlock);
  public
    property Name: WideString read FName;
    property ShortName: WideString read FShortName;

    property Items[Index: Integer]: TUnicodeBlock read GetItem write SetItem; default;
  end;

  TUnicodeDataError = (
    udeCouldNotDeleteDatabaseForRebuild,
    udeCouldNotCreateDatabase);

  TUnicodeData = class;

  IUnicodeDataUIManager = interface
    procedure UDUI_Error(Sender: TUnicodeData; Error: TUnicodeDataError; const Details: WideString);
    procedure UDUI_UpdateStatus(const Msg: WideString; Pos, Max: Integer);
    function UDUI_StartRebuild(Callback: TNotifyEvent; AskFirst: Boolean): Boolean;
    function UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
  end;

  TUnicodeData = class
  private
    FBlocks: TUnicodeBlockList;
    FUnicodeSourcePath: WideString;
    FRefs: Integer;
    FDatabase: ADODB_TLB.Connection; // DaoDatabase;
    FDisabled: Boolean;
    FRebuilt: Boolean;
    FUnicodeDataUIManager: IUnicodeDataUIManager;
    FADBPath: string;
    FFontRanges: TTTCMapTable;
    FFontName: WideString;
    FForceDBPathToAppDataOnBuild: Boolean;  // I2845

    procedure DoError(ErrorCode: TUnicodeDataError; const Details: WideString = '');
    function ShouldBuildDatabase: Boolean;
    procedure CallbackBuildDatabase(Sender: TObject);
    procedure ImportBlocks;
    procedure ImportChars;

    procedure LoadBlocks;

    function IsHangulCode(code: Integer; var ch: TUnicodeCharacter): Boolean;
    function IsCJKCode(code: Integer; var ch: TUnicodeCharacter): Boolean;
    function IsHangulName(aname: string; var ch: TUnicodeCharacter): Boolean;
    function IsCJKName(aname: string; var ch: TUnicodeCharacter): Boolean;
    procedure LoadDatabase;
    procedure CloseDatabase;
    function IsSpecialChar(ch: Integer): Boolean;
    procedure CleanupFilter(var filter: WideString; FBlock: Boolean);
    function UpdateBlockAndGetID(ch: Integer): Integer;
    procedure ImportUniHan;
    procedure BuildFontCharTable(const AFontName: WideString);
    procedure SetFontName(const Value: WideString);
    procedure SetDBPathToAppData;
  public
    constructor Create(const SourcePath: WideString; AUnicodeDataUIManager: IUnicodeDataUIManager; const ADBPath: string = ''; AForceDBPathToAppDataOnBuild: Boolean = True); // I2845
    destructor Destroy; override;

    function DBPath: string;
    procedure BuildDatabase(const ASourcePath: WideString = '');
    function FindDataByCode(code: Integer): TUnicodeCharacter;
    function FindDataByName(aname: WideString): TUnicodeCharacter;
    function GetSearchBlocks(filter: WideString): TUnicodeBlockList;
    procedure RefreshOptions;
    property Rebuilt: Boolean read FRebuilt;
    property Blocks: TUnicodeBlockList read FBlocks;
    property FontName: WideString read FFontName write SetFontName;

    property UnicodeDataUIManager: IUnicodeDataUIManager read FUnicodeDataUIManager;
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
    class function CleanCharacterName(nm: AnsiString): AnsiString;  // I3310
  end;

const
  SKRebuildDatabase = 'Keyman Developer has a Unicode character database, for use in the Character Map, '+
    'that needs to be built.  This may take up to a minute to complete.'#13#10#13#10+
    'Do you want to do this now?';

var
  FUnicodeData: TUnicodeData = nil;

procedure CreateUnicodeData(const SourcePath: WideString; AUnicodeDataUIManager: IUnicodeDataUIManager; const ADBPath: string = '');
procedure FreeUnicodeData;
function GetUnicodeRangeFromFilter(filter: WideString; var RangeStart, RangeStop: Integer): Boolean;

const
  UnicodeDataTxtName = 'UnicodeData.txt';
  UnicodeDataMdbName = 'unicodedata.mdb';

implementation

uses
  System.AnsiStrings,
  System.StrUtils,
  System.Variants,
  Winapi.ShlObj,

  RegistryKeys,
  Unicode,
  utilstr,
  utilsystem;

var
  _: Variant;

procedure CreateUnicodeData(const SourcePath: WideString; AUnicodeDataUIManager: IUnicodeDataUIManager; const ADBPath: string = '');
begin
  if not Assigned(FUnicodeData) then FUnicodeData := TUnicodeData.Create(SourcePath, AUnicodeDataUIManager, ADBPath);
  Inc(FUnicodeData.FRefs);
end;

procedure FreeUnicodeData;
begin
  if not Assigned(FUnicodeData) then Exit;
  Dec(FUnicodeData.FRefs);
  if FUnicodeData.FRefs = 0 then FreeAndNil(FUnicodeData);
end;

function StrTok(var str: ansistring; const tok: AnsiChar; var val: ansistring): Boolean;  // I3310
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

{-------------------------------------------------------------------------------
 - TUnicodeData: Read character properties from database or thru algorithm     -
 ------------------------------------------------------------------------------}

procedure TUnicodeData.BuildDatabase(const ASourcePath: WideString = '');
var
  OldSourcePath: WideString;
begin
  OldSourcePath := FUnicodeSourcePath;
  if ASourcePath <> '' then
    FUnicodeSourcePath := ASourcePath;

  if FUnicodeDataUIManager.UDUI_StartRebuild(CallbackBuildDatabase, False) then
    FRebuilt := True;

  if ASourcePath <> '' then
    FUnicodeSourcePath := OldSourcePath;
end;

procedure ReadStreamLine(fs: TFileStream; var s: ansistring);  // I3310
var
  ch: array[0..100] of ansiChar;
  str: shortstring;
  {nCR,} iCR, iLF, n: Integer;
const
  chcr: shortstring = #13;
  chlf: shortstring = #10;
begin
  s := '';
  while fs.Position < fs.Size do
  begin
    n := fs.Read(ch, 100);
    str := Copy(ch, 1, n);
    //nCR := 0;
    iCR := Pos(chcr, str);  // I3310
    //if iCR > 0 then
    //begin
      //Delete(str, iCR, 1);
      //Inc(nCR);
    //end;
    iLF := Pos(chlf, str);  // I3310
    if iLF > 0 then
    begin
      s := s + Copy(str, 1, iLF-1);
      if iCR = iLF then Inc(iLF);
      fs.Seek(-(Length(str)-iLF), soFromCurrent);
      s := System.AnsiStrings.AnsiReplaceText(s, #13, '');  // I3310  // I3310
      Exit;
    end;
    s := s + str;
  end;
end;

procedure TUnicodeData.CallbackBuildDatabase(Sender: TObject);
const
  SQL_Create_eBlock =
    'CREATE TABLE eBlock (' +
    '  BlockID INT IDENTITY(1,1) CONSTRAINT PrimaryKey PRIMARY KEY,' +
    '  StartChar INT NOT NULL,' +
    '  EndChar INT NOT NULL,' +
    '  Name VARCHAR(240) NOT NULL,' +
    '  CacheCharCount INT NULL,' +
    '  CacheCharData IMAGE NULL)';

  SQL_Create_eCharacter =
    'CREATE TABLE eCharacter ('+
    '  CharacterID INT IDENTITY(1,1) CONSTRAINT PrimaryKey PRIMARY KEY,'+
    '  BlockID INT NOT NULL,'+
    '  CodeValue INT NOT NULL,'+
    '  NameIsDescription BIT,'+
    '  CharacterName VARCHAR(240) NOT NULL,'+
{    '  GeneralCategory VARCHAR(2),'+
    '  CanonicalCombiningClasses INT NOT NULL,'+
    '  BidirectionalCategory VARCHAR(3) NOT NULL,'+
    '  CharacterDecompositionMapping VARCHAR(255),'+
    '  DecimalDigitValue INT,'+
    '  DigitValue INT,'+
    '  NumericValue VARCHAR(20),'+
    '  Mirrored BIT,'+
    '  Unicode10Name VARCHAR(240),'+
    '  A10646Comment VARCHAR(255),'+
    '  UpperCaseMapping INT,'+
    '  LowerCaseMapping INT,'+
    '  TitleCaseMapping INT,' +}
    '  CONSTRAINT FK_Character_Block FOREIGN KEY (BlockID) REFERENCES eBlock (BlockID))';

  SQL_Create_ByCodeValue =
    'CREATE UNIQUE INDEX ByCodeValue ON eCharacter (CodeValue) WITH DISALLOW NULL';

  SQL_Create_ByCharacterName =
    'CREATE INDEX ByCharacterName ON eCharacter (CharacterName) WITH DISALLOW NULL';

  SQL_Create_qCharacterByCode =
    'CREATE PROCEDURE qCharacterByCode (prmCode INT) AS SELECT * FROM eCharacter WHERE CodeValue = prmCode;';

  SQL_Create_qCharacterByName =
    'CREATE PROCEDURE qCharacterByName (prmName VARCHAR(240)) AS SELECT * FROM eCharacter WHERE CharacterName Like prmName;';

  SQL_Create_qBlocks =
    'CREATE PROCEDURE qBlocks AS SELECT * FROM eBlock ORDER BY StartChar;';

  SQL_Create_qFilterCharacters =
    'CREATE PROCEDURE qFilterCharacters (prmFilter VARCHAR(240)) AS '+
    '  SELECT CodeValue FROM eCharacter WHERE CharacterName LIKE prmFilter ORDER BY CodeValue';

  SQL_Create_qFilterCharactersBlock =
    'CREATE PROCEDURE qFilterCharactersBlock (prmFilter VARCHAR(240)) AS '+
    '  SELECT CodeValue FROM eCharacter INNER JOIN eBlock ON eCharacter.BlockID = eBlock.BlockID WHERE eBlock.Name LIKE prmFilter ORDER BY CodeValue';

  SQL_Create_qFilterCharactersRange =
    'CREATE PROCEDURE qFilterCharactersRange (prmStart INT, prmStop INT) AS '+
    '  SELECT CodeValue FROM eCharacter WHERE (CodeValue >= prmStart AND CodeValue <= prmStop) ORDER BY CodeValue';

  SQL_Create_qFilterCharactersSingle =
    'CREATE PROCEDURE qFilterCharactersSingle (prmCode INT) AS '+
    '  SELECT CodeValue FROM eCharacter WHERE CodeValue = prmCode';

  // Language table   // I4257

  SQL_Create_eLanguage =
    'CREATE TABLE eLanguage ( '+
    '  LanguageCode CHAR(3) NOT NULL CONSTRAINT LanguagePrimaryKey PRIMARY KEY, ' +
    '  Name VARCHAR(128) NOT NULL, ' +
    '  Country CHAR(2) NOT NULL, '+
    '  Status CHAR NOT NULL)';

  SQL_Create_LanguageByCodeValue =
    'CREATE UNIQUE INDEX LanguageByCodeValue ON eLanguage (LanguageCode) WITH DISALLOW NULL';

  SQL_Create_LanguageByName =
    'CREATE INDEX LanguageByName ON eLanguage (Name) WITH DISALLOW NULL';

  SQL_Create_qLanguageByCode =
    'CREATE PROCEDURE qLanguageByCode (prmCode CHAR(3)) AS SELECT * FROM eLanguage WHERE LanguageCode = prmCode;';

  SQL_Create_qLanguagesByName =
    'CREATE PROCEDURE qLanguagesByName (prmName VARCHAR(128)) AS SELECT * FROM eLanguage WHERE Name Like prmName;';
var
  //s: string;
  //val: array[0..14] of string;
  //i: Integer;
  vRecords: OleVariant;
  FCatalog: ADOX_TLB.Catalog;
begin
  CloseDatabase;
  if FForceDBPathToAppDataOnBuild then SetDBPathToAppData;  // I2476, I2299, I2845
  if FileExists(DBPath+UnicodeDataMdbName) then
    if not System.SysUtils.DeleteFile(DBPath+UnicodeDataMdbName) then
    begin
      DoError(udeCouldNotDeleteDatabaseForRebuild);
      //WideShowMessage('Could not delete database '''+DBPath+UnicodeDataMdbName+''': '+SysErrorMessage(GetLastError));
      Exit;
    end;

  { Create the database }

  FUnicodeDataUIManager.UDUI_UpdateStatus('Creating database', 0, 0);

  try
    FCatalog := CoCatalog.Create;
    FCatalog.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+DBPath+UnicodeDataMdbName);
    FCatalog := nil;

    FDatabase := CoConnection.Create;
    {$R-} {$WARNINGS OFF} // Constant expr violates subrange bounds for adConnectUnspecified
    FDatabase.Open('Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+DBPath+UnicodeDataMdbName, '', '', adConnectUnspecified);
    {$R+} {$WARNINGS ON} // Constant expr violates subrange bounds for adConnectUnspecified

    FDatabase.Execute(SQL_Create_eBlock, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_eCharacter, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_ByCodeValue, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_ByCharacterName, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qCharacterByCode, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qCharacterByName, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qBlocks, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qFilterCharacters, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qFilterCharactersBlock, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qFilterCharactersRange, vRecords, adExecuteNoRecords);
    FDatabase.Execute(SQL_Create_qFilterCharactersSingle, vRecords, adExecuteNoRecords);

    FDatabase.Execute(SQL_Create_eLanguage, vRecords, adExecuteNoRecords);   // I4257
    FDatabase.Execute(SQL_Create_LanguageByCodeValue, vRecords, adExecuteNoRecords);   // I4257
    FDatabase.Execute(SQL_Create_LanguageByName, vRecords, adExecuteNoRecords);   // I4257

    FDatabase.Execute(SQL_Create_qLanguageByCode, vRecords, adExecuteNoRecords);   // I4257
    FDatabase.Execute(SQL_Create_qLanguagesByName, vRecords, adExecuteNoRecords);   // I4257
  except
    on E:Exception do
    begin
      try
        FDatabase.Close;
      except
        ;
      end;
      FDatabase := nil;
      System.SysUtils.DeleteFile(DBPath+UnicodeDataMdbName);
      DoError(udeCouldNotCreateDatabase);
      FDisabled := True;
      Exit;
    end;
  end;

  FUnicodeDataUIManager.UDUI_UpdateStatus('Filling database', 0, 0);

  { Fill the database }

  //oldpos := 0;

  try
    ImportBlocks;
    ImportChars;
    ImportUnihan;
  except
    on E:Exception do
    begin
      DoError(udeCouldNotCreateDatabase, E.Message);
      FDisabled := True;
    end;
  end;
end;

procedure TUnicodeData.SetFontName(const Value: WideString);
begin
  FFontName := Value;
  BuildFontCharTable(FFontName);
end;

function TUnicodeData.ShouldBuildDatabase: Boolean;
var
  txtdate, datdate: TDateTime;
begin
  Result := False;
  if not FileAge(FUnicodeSourcePath+UnicodeDataTxtName, txtdate) then Exit; // UnicodeData.txt does not exist
  if FileAge(DBPath+UnicodeDataMdbName, datdate) then
    if datdate >= txtdate then Exit;
  Result := True; // The txt file is newer than the dat file, or the dat file does not exist
end;

constructor TUnicodeData.Create(const SourcePath: WideString; AUnicodeDataUIManager: IUnicodeDataUIManager; const ADBPath: string = ''; AForceDBPathToAppDataOnBuild: Boolean = True); // I2845
begin
  inherited Create;
  FADBPath := ADBPath;
  FForceDBPathToAppDataOnBuild := AForceDBPathToAppDataOnBuild; // I2845
  FUnicodeDataUIManager := AUnicodeDataUIManager;
  FUnicodeSourcePath := IncludeTrailingPathDelimiter(SourcePath);

  FBlocks := TUnicodeBlockList.Create;

  //if not CanUseDatabases then Exit;

  RefreshOptions;
end;

procedure TUnicodeData.ImportBlocks;
var
  fs: TFileStream;
  s: ansistring;
  range, rname: ansistring;
  FUnicodeSourceFile: string;
  //ur: TUnicodeBlock;
  n: Integer;
  eblock: ADODB_TLB.Recordset;
begin
  FUnicodeSourceFile := FUnicodeSourcePath + 'blocks.txt';
  fs := TFileStream.Create(FUnicodeSourceFile, fmOpenRead); //DBPath+UnicodeDataTxtName, fmOpenRead);
  with fs do
  try
    eblock := CoRecordset.Create;
    eblock.Open('eBlock', FDatabase, adOpenForwardOnly, adLockOptimistic, adCmdTable);// Execute('eCharacter', vRecords, adCmdTable);
    try
      while Position < Size do
      begin
        ReadStreamLine(fs, s);
        n := System.AnsiStrings.PosEx('#', s);  // I3310  // I3310
        if n > 0 then Delete(s,n,Length(s));
        if System.AnsiStrings.Trim(s) = '' then Continue;  // I3310  // I3310

        if not StrTok(s, ';', range) then Continue;
        if not StrTok(s, ';', rname) then Continue;

        range := System.AnsiStrings.Trim(range);  // I3310  // I3310
        rname := System.AnsiStrings.Trim(rname);  // I3310  // I3310

        n := System.AnsiStrings.PosEx('..', range); if n = 0 then Continue;  // I3310  // I3310

        eblock.AddNew(_, _);
        eblock.Collect['StartChar'] := StrToInt(String_AtoU('$'+Copy(range,1,n-1)));  // I3310  // I3310
        eblock.Collect['EndChar'] := StrToInt(String_AtoU('$'+Copy(range,n+2,Length(range))));  // I3310  // I3310
        eblock.Collect['Name'] := String_AtoU(rname);  // I3310  // I3310
        eblock.Update(_, _);
      end;
    finally
      eblock := nil;
    end;
  finally
    fs.Free;
  end;
  LoadBlocks;
end;

function TUnicodeData.IsSpecialChar(ch: Integer): Boolean;
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

function TUnicodeData.UpdateBlockAndGetID(ch: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to FBlocks.Count - 1 do
    if (FBlocks[i].StartChar <= ch) and (ch <= FBlocks[i].EndChar) then
    begin
      Result := FBlocks[i].BlockID;
      if (FBlocks[i].FCacheCharCount mod 1000) = 0 then
      begin
        ReallocMem(FBlocks[i].FCacheCharData, (FBlocks[i].FCacheCharCount + 1000) * SizeOf(Integer));
      end;
        //SetLength(FBlocks[i].FCacheCharData, FBlocks[i].FCacheCharCount + 1000);
      Inc(FBlocks[i].FCacheCharCount);
      FBlocks[i].FCacheCharData[FBlocks[i].CacheCharCount-1] := ch;
      Exit;
    end;
  Result := 0;
end;

procedure TUnicodeData.ImportChars;
var
  fs: TFileStream;
  echar, eblocks: ADODB_TLB.RecordSet;
  s: ansistring;
  val: array[0..14] of ansistring;
  oldpos, ch, i: Integer;
  b: Variant;
  p: PByte;
begin
  fs := TFileStream.Create(FUnicodeSourcePath + 'unicodedata.txt', fmOpenRead); //DBPath+UnicodeDataTxtName, fmOpenRead);
  with fs do
  try
    { Import the charaters }
    oldpos := 0;
    echar := CoRecordset.Create;
    echar.Open('eCharacter', FDatabase, adOpenForwardOnly, adLockOptimistic, adCmdTable);// Execute('eCharacter', vRecords, adCmdTable);
    try
      while Position < Size do
      begin
        ReadStreamLine(fs, s);
        for i := 0 to 14 do
          StrTok(s, ';', val[i]);

        if Copy(val[1], 1, 1) = '<' then Continue; // not a code point, control or similar

        ch := StrToInt(String_AtoU('$'+val[0]));  // I3310  // I3310
        if IsSpecialChar(ch) then Continue;

        echar.AddNew(_, _);
        echar.Collect['CodeValue']                     := ch;
        echar.Collect['BlockID']                       := UpdateBlockAndGetID(ch);
        echar.Collect['CharacterName']                 := String_AtoU(TUnicodeDataFormat.CleanCharacterName(Copy(val[1], 1, 240)));  // I3310  // I3310
        echar.Collect['NameIsDescription']  := False;

{
        echar.Collect['GeneralCategory']               := Copy(val[2], 1, 2);
        echar.Collect['CanonicalCombiningClasses']     := StrToInt(val[3]);
        echar.Collect['BidirectionalCategory']         := Copy(val[4], 1, 3);
        if val[5] <> ''  then echar.Collect['CharacterDecompositionMapping'] := Copy(val[5], 1, 255);
        if val[6] <> ''  then echar.Collect['DecimalDigitValue']             := StrToInt(val[6]);
        if val[7] <> ''  then echar.Collect['DigitValue']                    := StrToInt(val[7]);
        if val[8] <> ''  then echar.Collect['NumericValue']                  := Copy(val[8], 1, 20);
        if val[9] = '' then val[9] := 'N';
        echar.Collect['Mirrored']                      := UpCase(val[9][1]) = 'Y';
        if val[10] <> '' then echar.Collect['Unicode10Name']                 := Copy(val[10], 1, 240);
        if val[11] <> '' then echar.Collect['A10646Comment']                  := Copy(val[11], 1, 255);
        if val[12] <> '' then echar.Collect['UpperCaseMapping']              := StrToInt('$'+val[12]);
        if val[13] <> '' then echar.Collect['LowerCaseMapping']              := StrToInt('$'+val[13]);
        if val[14] <> '' then echar.Collect['TitleCaseMapping']              := StrToInt('$'+val[14]);
}

        echar.Update(_, _);

        if Position - oldpos > Size div 200 then
        begin
          FUnicodeDataUIManager.UDUI_UpdateStatus('Filling database (U+'+String_AtoU(val[0])+')', Position, Size);  // I3310  // I3310
          oldpos := Position;
        end;
      end;
    finally
      echar := nil;
    end;

    { Save the block cache data }
    eblocks := CoRecordset.Create;
    eblocks.Open('eBlock', FDatabase, adOpenForwardOnly, adLockOptimistic, adCmdTable);
    try
      while not eblocks.EOF do
      begin
        for i := 0 to FBlocks.Count - 1 do
          if FBlocks[i].BlockID = eblocks.Collect['BlockID'] then
          begin
            eblocks.Collect['CacheCharCount'] := FBlocks[i].CacheCharCount;

            {aADOStream := CoStream.Create;
            aADOStream.Type_ := adTypeBinary;
            aADOStream.Open(EmptyParam, adModeUnknown, adOpenStreamUnspecified, '', '');}

            b := VarArrayCreate([0,(FBlocks[i].CacheCharCount)*SizeOf(Integer)-1], varByte);
            p := PByte(VarArrayLock(b));
            CopyMemory(p, FBlocks[i].CacheCharData, (FBlocks[i].CacheCharCount)*SizeOf(Integer));
            VarArrayUnlock(b);

            eblocks.Fields['CacheCharData'].AppendChunk(b);// AppendChunk(b);
            //eblocks.Collect['CacheCharData'] := IntArrayToStr(FBlocks[i].CacheCharData);
            Break;
          end;
        eblocks.MoveNext;
      end;
    finally
      eblocks := nil;
    end;
  finally
    fs.Free;
  end;
end;

procedure TUnicodeData.ImportUniHan;
var
  fs: TFileStream;
  echar, eblocks: ADODB_TLB.RecordSet;
  t, s: ansistring;

  val: array[0..14] of ansistring;
  oldpos, ch, i: Integer;
  curch: Integer;
  ft, fdesc: WideString;
  fch: WideChar;
  fdefinition: WideString;
  b: Variant;
  p: PByte;
begin
  curch := 0;

  if not FileExists(FUnicodeSourcePath + 'unihan.txt') then Exit; { Unihan is optional }

  fs := TFileStream.Create(FUnicodeSourcePath + 'unihan.txt', fmOpenRead); //DBPath+UnicodeDataTxtName, fmOpenRead);
  with fs do
  try
    { Import the charaters }
    oldpos := 0;
    echar := CoRecordset.Create;
    echar.Open('eCharacter', FDatabase, adOpenForwardOnly, adLockOptimistic, adCmdTable);// Execute('eCharacter', vRecords, adCmdTable);
    try
      while Position < Size do
      begin
        ReadStreamLine(fs, s);
        if (Copy(s,1,1) = '#') or (s = '') then Continue;

        t := s;

        for i := 0 to 2 do
          StrTok(s, #9, val[i]);

        if Copy(val[0],1,2) <> 'U+' then
        begin
          writeln('Invalid Line: '+t);
          ExitCode := 3;
          Exit;
        end;

        { File format is UTF-8 }

        Delete(val[0], 1, 2);
        ch := StrToInt('$'+String_AtoU(val[0]));  // I3310  // I3310

        if curch <> ch then
        begin
          if curch <> 0 then
          begin
            if ft <> ''
              then fdesc := ft + '; ' + fdefinition
              else fdesc := fdefinition;
            if Length(fdesc) > 237
              then echar.Collect['CharacterName'] := Copy(fdesc,1,237)+'...'
              else echar.Collect['CharacterName'] := fdesc;
            try
              echar.Update(_, _);
            except
              on E:Exception do
              begin
                echar.CancelUpdate;
                writeln('Error adding U+'+IntToHex(curch,4)+': '+E.Message);
              end;
            end;
          end;
          echar.AddNew(_, _);
          echar.Collect['CodeValue']                     := ch;
          echar.Collect['BlockID']                       := UpdateBlockAndGetID(ch);
          echar.Collect['NameIsDescription']  := True; { Disables the drag and drop of character name }
          curch := ch;
          fdefinition := ''; fdesc := ''; ft := '';
        end;

        fch := #0;
        if val[1] = 'kCantonese' then fch := 'C'
        else if val[1] = 'kDefinition' then
          fdefinition := UTF8ToString(val[2])  // I3310
        else if val[1] = 'kHangul' then
          fch := 'H'
        else if val[1] = 'kJapaneseKun' then
          fch := 'J'
        else if val[1] = 'kKorean' then
          fch := 'K'
        else if val[1] = 'kMandarin' then
          fch := 'M'
        else if val[1] = 'kVietnamese' then
          fch := 'V';

        if fch <> #0 then
        begin
          if ft <> '' then ft := ft + ' ';
          ft := ft + fch + ':'+UTF8ToString(val[2]);  // I3310
        end;

        if Position - oldpos > Size div 1000 then
        begin
          FUnicodeDataUIManager.UDUI_UpdateStatus('Filling database from Unihan (U+'+String_AtoU(val[0])+')', Position, Size);  // I3310  // I3310
          oldpos := Position;
        end;
      end;
    finally
      echar := nil;
    end;

    { Save the block cache data }
    eblocks := CoRecordset.Create;
    eblocks.Open('eBlock', FDatabase, adOpenForwardOnly, adLockOptimistic, adCmdTable);
    try
      while not eblocks.EOF do
      begin
        for i := 0 to FBlocks.Count - 1 do
          if FBlocks[i].BlockID = eblocks.Collect['BlockID'] then
          begin
            eblocks.Collect['CacheCharCount'] := FBlocks[i].CacheCharCount;

            b := VarArrayCreate([0,(FBlocks[i].CacheCharCount)*SizeOf(Integer)-1], varByte);
            p := PByte(VarArrayLock(b));
            CopyMemory(p, @FBlocks[i].CacheCharData, (FBlocks[i].CacheCharCount)*SizeOf(Integer));
            VarArrayUnlock(b);

            eblocks.Fields['CacheCharData'].AppendChunk(b);// AppendChunk(b);

            //eblocks.Collect['CacheCharData'] := IntArrayToStr(FBlocks[i].CacheCharData);
            Break;
          end;
        eblocks.MoveNext;
      end;
    finally
      eblocks := nil;
    end;
  finally
    fs.Free;
  end;
end;

(*
function TUnicodeData.IntArrayToStr(v: TIntegerArray): Variant;
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to High(v) do
  begin
    s := s + IntToHex(v[i], 8);
  end;
  Result := s;
{  TVarData(Result).VType := varInteger;
  TVarData(Result).VAny
  Result := VarArrayCreate([Low(v)+1, High(v)+1], varInteger);
  for i := Low(v) to High(v) do
    Result[i+1] := v[i];}
end;

function TUnicodeData.VarStrToIntArray(v: Variant): TIntegerArray;
var
  i: Integer;
  s: string;
begin
  s := v;
  SetLength(Result, Length(s) div 8);
  for i := 0 to (Length(s) div 8) - 1 do
  begin
    Result[i] := StrToInt('$'+Copy(s,i*8+1,8));
  end;
{  SetLength(Result, VarArrayHighBound(v, 1) - VarArrayLowBound(v, 1) + 1);
  for i := Low(Result) to High(Result) do
    Result[i] := v[i+1];}
end;
*)

procedure TUnicodeData.LoadBlocks;
var
  rec: ADODB_TLB.Recordset;
  vRecords: OleVariant;
  ub: TUnicodeBlock;
  b: OleVariant;
  p: PByte;
begin
  if not Assigned(FDatabase) then Exit;

  FBlocks.Clear;

  rec := FDatabase.Execute('qBlocks', vRecords, adCmdStoredProc);
  try
    while not rec.EOF do
    begin
      ub := TUnicodeBlock.Create;
      ub.FBlockID := rec.Collect['BlockID'];
      ub.FName := rec.Collect['Name'];
      ub.FShortName := ub.FName;
      ub.FStartChar := rec.Collect['StartChar'];
      ub.FEndChar := rec.Collect['EndChar'];
      if not VarIsNull(rec.Collect['CacheCharCount']) then
      begin
        ub.FCacheCharCount := rec.Collect['CacheCharCount'];
        if ub.FCacheCharCount > 0 then
        begin
          b := rec.Fields['CacheCharData'].GetChunk(ub.FCacheCharCount*SizeOf(Integer));
          p := PByte(VarArrayLock(b));
          ub.FCacheCharData := AllocMem(ub.CacheCharCount * SizeOf(Integer));
          //SetLength(ub.FCacheCharData, ub.FCacheCharCount);
          CopyMemory(ub.FCacheCharData, p, ub.FCacheCharCount*SizeOf(Integer));
          VarArrayUnlock(b);
        end
        else
          ub.FCacheCharData := nil;
          //SetLength(ub.FCacheCharData, 0);
        //ub.FCacheCharData := VarStrToIntArray(rec.Collect['CacheCharData']);
        //SetLength(ub.CacheCharData, ub.CacheCharCount);
      end;
      FBlocks.Add(ub);
      rec.MoveNext;
    end;
  finally
    rec := nil;
  end;
end;

procedure TUnicodeData.LoadDatabase;
begin
  CloseDatabase;

  if ShouldBuildDatabase then
  begin
    UnicodeDataUIManager.UDUI_StartRebuild(CallbackBuildDatabase, True);
    Exit;
  end;

  if FileExists(DBPath+UnicodeDataMdbName) then
  begin
    try
      FDatabase := CoConnection.Create;
      {$R-} {$WARNINGS OFF} // Constant expr violates subrange bounds for adConnectUnspecified
      FDatabase.Open('Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+DBPath+UnicodeDataMdbName, '', '', adConnectUnspecified);
      {$R+} {$WARNINGS ON} // Constant expr violates subrange bounds for adConnectUnspecified
      //FDatabase := dbEngine.OpenDatabase(DBPath+UnicodeDataMdbName, _, False, _);
      LoadBlocks;
    except
      on E:Exception do
      begin
        FDatabase := nil;
        FBlocks.Clear;
        if FUnicodeDataUIManager.UDUI_ShouldStartRebuildOnError(E.Message) then
          BuildDatabase;
      end;
    end;
  end;
end;

procedure TUnicodeData.RefreshOptions;
begin
  {with TKeymanDeveloperOptions.Create do
  try
    Read;
    FDisabled := CharMapDisableDatabaseLookups;
  finally
    Free;
  end;}
  FDisabled := False;
  if FDisabled
    then CloseDatabase
    else LoadDatabase;
end;

procedure TUnicodeData.CloseDatabase;
begin
  if Assigned(FDatabase) then FDatabase.Close;
  FDatabase := nil;
end;

destructor TUnicodeData.Destroy;
begin
  try
    CloseDatabase;
  except
    ;
  end;
  FBlocks.Free;
  FFontRanges.Free;
  inherited Destroy;
end;

procedure TUnicodeData.DoError(ErrorCode: TUnicodeDataError; const Details: WideString = '');
begin
  FUnicodeDataUIManager.UDUI_Error(Self, ErrorCode, Details);
end;

procedure TUnicodeData.SetDBPathToAppData;  // I2476, I2299
begin
  FADBPath := GetFolderPath(CSIDL_APPDATA) + SFolderSharedDatabases + '\';
  ForceDirectories(FADBPath);
end;

function TUnicodeData.DBPath: string;
var
  FAppDataPath: string;
begin
  FAppDataPath := GetFolderPath(CSIDL_APPDATA) + SFolderSharedDatabases + '\';  // I2476, I2299

  if FADBPath <> '' then
    Result := FADBPath
  else if FileExists(FAppDataPath+UnicodeDataMdbName) then  // I2476, I2299
    Result := FAppDataPath
  else if FileExists(FUnicodeSourcePath+UnicodeDataMdbName) then
    Result := FUnicodeSourcePath
  else
  begin
    Result := GetFolderPath(CSIDL_APPDATA) + SFolderSharedDatabases + '\';
    ForceDirectories(Result);
  end;
end;


function FillData(rec: ADODB_TLB.Recordset): TUnicodeCharacter; //rec: DaoRecordset): TUnicodeCharacter;
begin
  Result.CodeValue := rec.Collect['CodeValue'];

  Result.CharacterName := rec.Collect['CharacterName'];

{
  Result.GeneralCategory := rec.Collect['GeneralCategory'];

  Result.CanonicalCombiningClasses := rec.Collect['CanonicalCombiningClasses'];

  Result.BidirectionalCategory := rec.Collect['BidirectionalCategory'];

  if not VarIsNull(rec.Collect['CharacterDecompositionMapping'])
    then Result.CharacterDecompositionMapping := rec.Collect['CharacterDecompositionMapping']
    else Result.CharacterDecompositionMapping := '';

  if not VarIsNull(rec.Collect['DecimalDigitValue'])
    then Result.DecimalDigitValue := rec.Collect['DecimalDigitValue']
    else Result.DecimalDigitValue := -1;

  if not VarIsNull(rec.Collect['DigitValue'])
    then Result.DigitValue := rec.Collect['DigitValue']
    else Result.DigitValue := -1;

  if not VarIsNull(rec.Collect['NumericValue'])
    then Result.NumericValue := rec.Collect['NumericValue']
    else Result.NumericValue := '';

  Result.Mirrored := rec.Collect['Mirrored'];

  if not VarIsNull(rec.Collect['Unicode10Name'])
    then Result.Unicode10Name := rec.Collect['Unicode10Name']
    else Result.Unicode10Name := '';

  if not VarIsNull(rec.Collect['A10646Comment'])
    then Result._10646Comment := rec.Collect['A10646Comment']
    else Result._10646Comment := '';

  if not VarIsNull(rec.Collect['UpperCaseMapping'])
    then Result.UpperCaseMapping := rec.Collect['UpperCaseMapping']
    else Result.UpperCaseMapping := 0;

  if not VarIsNull(rec.Collect['LowerCaseMapping'])
    then Result.LowerCaseMapping := rec.Collect['LowerCaseMapping']
    else Result.LowerCaseMapping := 0;

  if not VarIsNull(rec.Collect['TitleCaseMapping'])
    then Result.TitleCaseMapping := rec.Collect['TitleCaseMapping']
    else Result.TitleCaseMapping := 0;
}
end;

function TUnicodeData.FindDataByCode(code: Integer): TUnicodeCharacter;
var
  //rec: DaoRecordset;
  rec: ADODB_TLB.Recordset;
  vRecords: OleVariant;
begin
  Result.CodeValue := 0;

  if FDisabled then Exit;

  if IsHangulCode(code, Result) then Exit;

  if not Assigned(FDatabase) then Exit;

  rec := FDatabase.Execute('EXECUTE qCharacterByCode '+IntToStr(code), vRecords, adCmdText);

  try
    if not rec.EOF then
      Result := FillData(rec)
    else
      if IsCJKCode(code, Result) then Exit
    else
    begin
      Result.CodeValue := code;
      Result.CharacterName := '';
    end;
  finally
    rec := nil;
  end;
end;

function TUnicodeData.FindDataByName(aname: WideString): TUnicodeCharacter;
var
  rec: ADODB_TLB.Recordset;
  vRecords: OleVariant;
begin
  Result.CodeValue := 0;

  CleanupFilter(aname, False);

  if FDisabled then Exit;

  if IsHangulName(aname, Result) then Exit;
  if IsCJKName(aname, Result) then Exit;

  if not Assigned(FDatabase) then Exit;

  rec := FDatabase.Execute('qCharacterByName '+QuotedStr(aname+'%'), vRecords, adCmdStoredProc);  // I3310  // I3465
  try
    if not rec.EOF then
      Result := FillData(rec);
  finally
    rec := nil;
  end;
end;

procedure TUnicodeData.CleanupFilter(var filter: WideString; FBlock: Boolean);
var
  I: Integer;
begin
  { Replace *, # and ? with appropriate search characters }
  I := 1;
  while I <= length(filter) do
  begin
    if filter[i] = '*' then filter[i] := '%'
    else if (filter[i] = '_') then
    begin
      if FBlock then
        filter[I] := ' '
      else
      begin
        filter[I] := '[';
        Insert('_]', filter, I+1);
        Inc(I, 2);
      end;
    end
    else if (filter[i] = ' ') then
    begin
      if not FBlock then filter[i] := '%'
    end
    else if (filter[i] = '#') or (filter[i] = '?') then filter[i] := '_';
    Inc(I);
  end;

  { Detect end-of-line delimiter }
  I := Pos('$', filter);
  if I > 0
    then Delete(filter, I, Length(filter))
    else filter := filter + '%';

  { Detect beginning-of-line delimiter }
  if Copy(filter, 1, 1) = '^'
    then Delete(filter, 1, 1)
    else filter := '%' + filter;
end;

function GetUnicodeRangeFromFilter(filter: WideString; var RangeStart, RangeStop: Integer): Boolean;  // I2210
  function UnicodeCharToValue(v: WideString): Integer;
  begin
    if Copy(v, 1, 2) = 'U+' then Result := StrToIntDef('$'+Copy(v,3,10), 0)
    else if Length(v) < 2 then Result := 0
    else Result := StrToIntDef('$'+v, 0);
  end;
var
  filterStart, filterStop: WideString;
  n: Integer;
  FRange: Boolean;
begin
  filter := Trim(UpperCase(filter));

  n := Pos('-', filter);
  if n > 0 then
  begin
    //[U+]xxxx-[U+]yyyy
    filterStart := Trim(Copy(filter, 1, n-1));
    filterStop := Trim(Copy(filter, n+1, Length(filter)));
    FRange := True;
  end
  else
  begin
    //[U+]xxxx
    filterStart := filter;
    filterStop := filter;
    FRange := False;
  end;

  RangeStart := UnicodeCharToValue(filterStart);
  RangeStop := UnicodeCharToValue(filterStop);

  if FRange and (RangeStop > 0) and (filterStart = '') then RangeStart := $20
  else if FRange and (filterStop = '') and (RangeStart > 0) then RangeStop := $10FFFF;

  Result := (RangeStart > 0);
end;

function TUnicodeData.GetSearchBlocks(filter: WideString): TUnicodeBlockList;
var
  //rec: DaoRecordset;
  rec: ADODB_TLB.Recordset;
  vRecords: OleVariant;
  cmd: WideString;
  FontCmapIndex, RangeStart, RangeStop: Integer;
  FCurrentFont, FRange, FBlock: Boolean;
  origfilter: WideString;
  CodeValue: Integer;
  ub: TUnicodeBlock;
  nBlock: Integer;
begin
  Result := nil;

  if FDisabled then Exit;

  if not Assigned(FDatabase) then Exit;

  filter := Trim(filter);

  FBlock := False;
  FRange := False;
  FCurrentFont := False;
  RangeStart := 0;
  RangeStop := 0;

  ub := nil;

  if Copy(Filter, 1, 1) = '>' then
  begin
    Delete(filter, 1, 1);
    FCurrentFont := Assigned(FFontRanges);

    if FCurrentFont and (filter = '') then
    begin
      { All characters from current font - no database search required }

      Result := TUnicodeBlockList.Create;
      Result.FName := FFontName+', all characters';
      Result.FShortName := '>';

      FontCmapIndex := 0;
      if FontCmapIndex >= FFontRanges.Count then Exit;

      nBlock := 0;

      while FontCmapIndex < FFontRanges.Count do
      begin
        for CodeValue := FFontRanges.Item[FontCmapIndex].StartCode to FFontRanges.Item[FontCmapIndex].EndCode do
        begin
          if not Assigned(ub) or (ub.EndChar < CodeValue) then
          begin
            while (nBlock < FBlocks.Count) and (FBlocks[nBlock].EndChar < CodeValue) do Inc(nBlock);
            if nBlock = FBlocks.Count then Break;

            ub := TUnicodeBlock.Create;
            ub.FName := FBlocks[nBlock].Name;
            ub.FShortName := FBlocks[nBlock].ShortName;
            ub.FBlockID := FBlocks[nBlock].BlockID;
            ub.FEndChar := FBlocks[nBlock].EndChar;
            ub.FStartChar := FBlocks[nBlock].StartChar;
            ub.FCacheCharCount := 0;
            Result.Add(ub);
          end;

          if (ub.FCacheCharCount mod 100) = 0 then
          begin
            ReallocMem(ub.FCacheCharData, (ub.FCacheCharCount+100) * SizeOf(Integer));
          end;
          ub.FCacheCharData[ub.FCacheCharCount] := CodeValue;
          Inc(ub.FCacheCharCount);
        end;
        Inc(FontCmapIndex);
      end;
      Exit;
    end;

  end;

  if GetUnicodeRangeFromFilter(filter, RangeStart, RangeStop) then
  begin
    FRange := True;
    if RangeStart = 0 then origfilter := '(invalid range)'
    else
    begin
      origfilter := 'U+'+IntToHex(RangeStart, 4);
      if RangeStop <> 0 then origfilter := origfilter+' - U+'+IntToHex(RangeStop,4);
    end;
  end
  else
  begin
    if Copy(filter, 1, 1) = '<' then
    begin
      Delete(filter,1,1);
      FBlock := True;
    end;
    origfilter := filter;
    CleanupFilter(filter, FBlock);
  end;

  if FBlock then
    cmd := WideFormat('qFilterCharactersBlock %s', [QuotedStr(filter)])  // I3310  // I3465
  else if FRange then
  begin
    if RangeStop = 0 then
      cmd := WideFormat('qFilterCharactersRange %d, %d', [RangeStart, RangeStart])
    else if RangeStart = 0 then
      Exit
    else
      cmd := WideFormat('qFilterCharactersRange %d, %d', [RangeStart, RangeStop])
  end
  else
    cmd := WideFormat('qFilterCharacters %s', [QuotedStr(filter)]);  // I3310  // I3465

  try
    rec := FDatabase.Execute(cmd, vRecords, adCmdStoredProc or adAsyncFetch); // or adAsyncExecute);
  except
    Exit; { ignore invalid filters for now }
  end;
  try
    Result := TUnicodeBlockList.Create;

    if FBlock then Result.FName := 'Blocks matching '+origfilter
    else if FRange then Result.FName := 'Unicode range '+origfilter
    else Result.FName := 'Characters matching '+origfilter;

    if FBlock
      then Result.FShortName := '<'+origfilter
      else Result.FShortName := origfilter;

    if FCurrentFont then
    begin
      if origfilter = '' then Result.FName := FFontName+', all characters'

      else if FBlock then Result.FName := FFontName+', blocks matching '+origfilter
      else if FRange then Result.FName := FFontName+', unicode range '+origfilter
      else Result.FName := FFontName+', characters matching '+origfilter;
      Result.FShortName := '>' + Result.FShortName;
    end;

    FontCmapIndex := 0;
    if FCurrentFont and (FontCmapIndex >= FFontRanges.Count) then Exit;

    nBlock := 0;
    //Result.FBlockID := 0;
    //Result.FCacheCharCount := 0;
    while not rec.EOF do
    begin
      CodeValue := rec.Collect['CodeValue'];
      if FCurrentFont then
      begin
        { Filter by current font }
        while (CodeValue > FFontRanges.Item[FontCmapIndex].EndCode) do
        begin
          Inc(FontCmapIndex);
          if FontCmapIndex >= FFontRanges.Count then Exit;
            { No more characters in current font, so just exit }
        end;
        if CodeValue < FFontRanges.Item[FontCmapIndex].StartCode then
        begin
          rec.MoveNext;
          Continue;
        end;
      end;

      if not Assigned(ub) or (ub.EndChar < CodeValue) then
      begin
        while (nBlock < FBlocks.Count) and (FBlocks[nBlock].EndChar < CodeValue) do Inc(nBlock);
        if nBlock = FBlocks.Count then Break;

        ub := TUnicodeBlock.Create;
        ub.FName := FBlocks[nBlock].Name;
        ub.FShortName := FBlocks[nBlock].ShortName;
        ub.FBlockID := FBlocks[nBlock].BlockID;
        ub.FEndChar := FBlocks[nBlock].EndChar;
        ub.FStartChar := FBlocks[nBlock].StartChar;
        ub.FCacheCharCount := 0;
        Result.Add(ub);
      end;

      if (ub.FCacheCharCount mod 100) = 0 then
      begin
        ReallocMem(ub.FCacheCharData, (ub.FCacheCharCount+100) * SizeOf(Integer));
      end;
      ub.FCacheCharData[ub.FCacheCharCount] := CodeValue;
      Inc(ub.FCacheCharCount);
      rec.MoveNext;
    end;
  finally
    rec := nil;
  end;
end;

{-------------------------------------------------------------------------------
 - Algorithms for finding CJK and Hangul character properties                  -
 ------------------------------------------------------------------------------}

{ CJK Unified Ideographs }

procedure FillCJK(code: Integer; var ch: TUnicodeCharacter);
begin
  ch.CodeValue := code;
  ch.CharacterName := 'CJK_UNIFIED_IDEOGRAPH_' + IntToHex(code, 4);
{  ch.GeneralCategory := 'Lo';
  ch.CanonicalCombiningClasses := 0;
  ch.BidirectionalCategory := 'L';
  ch.CharacterDecompositionMapping := '';
  ch.DecimalDigitValue := -1;
  ch.DigitValue := -1;
  ch.NumericValue := '';
  ch.Mirrored := False;

  ch.Unicode10Name := '';
  ch._10646Comment := 'Details generated by algorithm';
  ch.UpperCaseMapping := 0;
  ch.LowerCaseMapping := 0;
  ch.TitleCaseMapping := 0;}
end;

const
  CJKUnifiedBase  = $4E00;
  CJKUnifiedFinal = $BFFF;

function TUnicodeData.IsCJKCode(code: Integer; var ch: TUnicodeCharacter): Boolean;
begin
  Result := (code >= CJKUnifiedBase) and (code <= CJKUnifiedFinal);
  if Result then FillCJK(code, ch);
end;

function TUnicodeData.IsCJKName(aname: string; var ch: TUnicodeCharacter): Boolean;
begin
  Result := False;

	if UpperCase(Copy(aname, 1, 22)) <> 'CJK_UNIFIED_IDEOGRAPH_' then Exit;
	Delete(aname, 1, 22);

  if Length(aname) <> 4 then Exit;

	ch.CodeValue := StrToIntDef('$'+aname, 0);
  if ch.CodeValue = 0 then Exit;

  FillCJK(ch.CodeValue, ch);
  Result := True;
end;

{ Hangul }

const
 HangulSBase = $AC00;
 HangulLBase = $1100;
 HangulVBase = $1161;
 HangulTBase = $11A7;
 HangulLCount = 19;
 HangulVCount = 21;
 HangulTCount = 28;
 HangulNCount = HangulVCount * HangulTCount;   // 588
 HangulSCount = HangulLCount * HangulNCount;   // 11172

const
  Hangul_JAMO_L_TABLE: array[0..HangulLCount-1] of string = (
        'G', 'GG', 'N', 'D', 'DD', 'R', 'M', 'B', 'BB',
        'S', 'SS', '', 'J', 'JJ', 'C', 'K', 'T', 'P', 'H');

  Hangul_JAMO_V_TABLE: array[0..HangulVCount-1] of string = (
        'A', 'AE', 'YA', 'YAE', 'EO', 'E', 'YEO', 'YE', 'O',
        'WA', 'WAE', 'OE', 'YO', 'U', 'WEO', 'WE', 'WI',
        'YU', 'EU', 'YI', 'I');

  Hangul_JAMO_T_TABLE: array[0..HangulTCount-1] of string = (
        '', 'G', 'GG', 'GS', 'N', 'NJ', 'NH', 'D', 'L', 'LG', 'LM',
        'LB', 'LS', 'LT', 'LP', 'LH', 'M', 'B', 'BS',
        'S', 'SS', 'NG', 'J', 'C', 'K', 'T', 'P', 'H');

function HangulDecompose(s: Integer): string;
var
  SIndex, L, V, T: Integer;
begin
  SIndex := s - HangulSBase;
  if (SIndex < 0) or (SIndex >= HangulSCount) then
  begin
    Result := '';
    Exit;
  end;

  L := HangulLBase + SIndex div HangulNCount;
  V := HangulVBase + (SIndex mod HangulNCount) div HangulTCount;
  T := HangulTBase + SIndex mod HangulTCount;

  Result := IntToHex(L, 4) + ' ' + IntToHex(V, 4);
  if T <> HangulTBase then Result := Result + IntToHex(T, 4);
end;

function HangulName(s: Integer): string;
var
  SIndex, LIndex, VIndex, TIndex: Integer;
begin
  SIndex := s - HangulSBase;
  if (SIndex < 0) or (SIndex >= HangulSCount) then
  begin
    Result := '';
    Exit;
  end;

  LIndex := SIndex div HangulNCount;
  VIndex := (SIndex mod HangulNCount) div HangulTCount;
  TIndex := SIndex mod HangulTCount;
  Result := 'HANGUL_SYLLABLE_' + Hangul_JAMO_L_TABLE[LIndex]
          + Hangul_JAMO_V_TABLE[VIndex] + Hangul_JAMO_T_TABLE[TIndex];
end;

procedure FillHangul(code: Integer; var ch: TUnicodeCharacter);
begin
  ch.CodeValue := code;
  ch.CharacterName := HangulName(code);
{  ch.GeneralCategory := 'Lo';
  ch.CanonicalCombiningClasses := 0;
  ch.BidirectionalCategory := 'L';
  ch.CharacterDecompositionMapping := HangulDecompose(code);
  ch.DecimalDigitValue := -1;
  ch.DigitValue := -1;
  ch.NumericValue := '';
  ch.Mirrored := False;

  ch.Unicode10Name := '';
  ch._10646Comment := 'Details generated by algorithm';
  ch.UpperCaseMapping := 0;
  ch.LowerCaseMapping := 0;
  ch.TitleCaseMapping := 0;}
end;

function TUnicodeData.IsHangulCode(code: Integer; var ch: TUnicodeCharacter): Boolean;
begin
  Result := (code >= HangulSBase) and (code < HangulSBase + HangulSCount);
  if Result then
  begin
    FillHangul(code, ch);
  end;
end;

function TUnicodeData.IsHangulName(aname: string; var ch: TUnicodeCharacter): Boolean;
var
  code, i, LIndex, VIndex, TIndex: Integer;
  L, V, T: string;
begin
  Result := False;
  if Copy(UpperCase(aname), 1, 16) = 'HANGUL_SYLLABLE_' then
  begin
    Delete(aname, 1, 16);
    if Length(aname) < 1 then Exit;

    { Find initial }

    L := Copy(aname, 1, 2);
    if Length(L) > 1 then if L[1] <> L[2] then Delete(L,2,1); // Only doubles allowed
    if Pos(L[1],'GNDRMBSJCKTPH') = 0 then
      LIndex := 11
    else
    begin
      LIndex := -1;
      for i := 0 to High(Hangul_JAMO_L_TABLE) do
        if Hangul_JAMO_L_TABLE[i] = L then begin LIndex := i; Break; end;
      Delete(aname, 1, Length(L));
    end;

    if LIndex = -1 then Exit;

    { Find vowel }

    V := Copy(aname, 1, 3);
    if Length(V) >= 3 then if Pos(V[3], 'AEIOUWY') = 0 then Delete(V,3,1);
    if Length(V) >= 2 then if Pos(V[2], 'AEIOUWY') = 0 then Delete(V,2,1);

    VIndex := -1;
    for i := 0 to High(Hangul_JAMO_V_TABLE) do
      if Hangul_JAMO_V_TABLE[i] = V then begin VIndex := i; Break; end;

    if VIndex = -1 then Exit;

    Delete(aname, 1, Length(V));

    { Find final }

    TIndex := -1;
    T := aname;
    for i := 0 to High(Hangul_JAMO_T_TABLE) do
      if Hangul_JAMO_T_TABLE[i] = T then begin TIndex := i; Break; end;

    if TIndex = -1 then Exit;

    { Composition }

    code := (HangulSBase + (LIndex * HangulVCount + VIndex) * HangulTCount) + TIndex;

    Result := True;
    FillHangul(code, ch);
  end;
end;

{ Font Data Filling }

procedure TUnicodeData.BuildFontCharTable(const AFontName: WideString);
begin
  if AFontName = '' then
    FreeAndNil(FFontRanges)
  else
    with TTTInfoFromFont.Create(AFontName) do
    try
      if not Assigned(CMapTable) then
      begin
        { We need to build a new ansi character map from the current character set - get char set info from font }
        FFontRanges := TTTCMapTable.Create;
        //FFontRanges.BuildFromCodePage(CP_ACP);
      end
      else
      begin
        FFontRanges := TTTCMapTable.Create;
        FFontRanges.Assign(CMapTable);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
 - TUnicodeDataFormat: Formatting functions for Unicode data                   -
 ------------------------------------------------------------------------------}

class function TUnicodeDataFormat.CleanCharacterName(nm: AnsiString): AnsiString;  // I3310
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

(*class function TUnicodeDataFormat.GeneralCategory(GC: string): string;
const
  GeneralCategories: array[0..30, 0..1] of string = (
   ('Lu', 'Letter, Uppercase'),
   ('Ll', 'Letter, Lowercase'),
   ('Lt', 'Letter, Titlecase'),
   ('Lm', 'Letter, Modifier'),
   ('Lo', 'Letter, Other'),
   ('Mn', 'Mark, Non-Spacing'),
   ('Mc', 'Mark, Spacing Combining'),
   ('Me', 'Mark, Enclosing'),
   ('Nd', 'Number, Decimal Digit'),
   ('Nl', 'Number, Letter'),
   ('No', 'Number, Other'),
   ('Pc', 'Punctuation, Connector'),
   ('Pd', 'Punctuation, Dash'),
   ('Ps', 'Punctuation, Open'),
   ('Pe', 'Punctuation, Close'),
   ('Pi', 'Punctuation, Initial quote'),
   ('Pf', 'Punctuation, Final quote'),
   ('Po', 'Punctuation, Other'),
   ('Sm', 'Symbol, Math'),
   ('Sc', 'Symbol, Currency'),
   ('Sk', 'Symbol, Modifier'),
   ('So', 'Symbol, Other'),
   ('Zs', 'Separator, Space'),
   ('Zl', 'Separator, Line'),
   ('Zp', 'Separator, Paragraph'),
   ('Cc', 'Other, Control'),
   ('Cf', 'Other, Format'),
   ('Cs', 'Other, Surrogate'),
   ('Co', 'Other, Private Use'),
   ('Cn', 'Other, Not Assigned'),
   ('L&', 'Letter, Uppercase, Lowercase or Titlecase'));
var
  i: Integer;
begin
  for i := 0 to High(GeneralCategories) do
    if GeneralCategories[i, 0] = GC then
    begin
      Result := GeneralCategories[i, 1];
      Exit;
    end;
  Result := 'Unknown ('+GC+')';
end;

class function TUnicodeDataFormat.CanonicalCombiningClass(CCC: Integer): string;
begin
  case CCC of
    0:   Result := 'Spacing, split, enclosing, reordrant, and Tibetan subjoined';
    1:   Result := 'Overlays and interior';
    7:   Result := 'Nuktas';
    8:   Result := 'Hiragana/Katakana voicing marks';
    9:   Result := 'Viramas';
    10:  Result := 'Start of fixed position classes';
    199: Result := 'End of fixed position classes';
    200: Result := 'Below left attached';
    202: Result := 'Below attached';
    204: Result := 'Below right attached';
    208: Result := 'Left attached (reordrant around single base character)';
    210: Result := 'Right attached';
    212: Result := 'Above left attached';
    214: Result := 'Above attached';
    216: Result := 'Above right attached';
    218: Result := 'Below left';
    220: Result := 'Below';
    222: Result := 'Below right';
    224: Result := 'Left (reordrant around single base character)';
    226: Result := 'Right';
    228: Result := 'Above left';
    230: Result := 'Above';
    232: Result := 'Above right';
    233: Result := 'Double below';
    234: Result := 'Double above';
    240: Result := 'Below (iota subscript)';
  else   Result := 'Unknown ('+IntToStr(CCC)+')';
  end;
end;

class function TUnicodeDataFormat.BidirectionalCategory(BC: string): string;
const
  BidiCats: array[0..18, 0..1] of string = (
    ('L', 'Left-to-Right'),
    ('LRE', 'Left-to-Right Embedding'),
    ('LRO', 'Left-to-Right Override'),
    ('R', 'Right-to-Left'),
    ('AL', 'Right-to-Left Arabic'),
    ('RLE', 'Right-to-Left Embedding'),
    ('RLO', 'Right-to-Left Override'),
    ('PDF', 'Pop Directional Format'),
    ('EN', 'European Number'),
    ('ES', 'European Number Separator'),
    ('ET', 'European Number Terminator'),
    ('AN', 'Arabic Number'),
    ('CS', 'Common Number Separator'),
    ('NSM', 'Non-Spacing Mark'),
    ('BN', 'Boundary Neutral'),
    ('B', 'Paragraph Separator'),
    ('S', 'Segment Separator'),
    ('WS', 'Whitespace'),
    ('ON', 'Other Neutrals'));
var
  i: Integer;
begin
  for i := 0 to High(BiDiCats) do
    if BiDiCats[i, 0] = BC then
    begin
      Result := BiDiCats[i, 1];
      Exit;
    end;
  Result := 'Unknown ('+BC+')';
end;


class function TUnicodeDataFormat.CharacterDecompositionMapping(CDM: string): string;
begin
  Result := CDM;
end;

class function TUnicodeDataFormat.DecimalDigitValue(DDV: Integer): string;
begin
  if DDV >= 0
    then Result := IntToStr(DDV)
    else Result := '';
end;

class function TUnicodeDataFormat.DigitValue(DV: Integer): string;
begin
  if DV >= 0
    then Result := IntToStr(DV)
    else Result := '';
end;

class function TUnicodeDataFormat.NumericValue(NV: string): string;
begin
  Result := Trim(NV);
end;

class function TUnicodeDataFormat.Mirrored(M: Boolean): string;
begin
  if M
    then Result := 'Yes'
    else Result := 'No';
end;

class function TUnicodeDataFormat._10646Comment(_1C: string): string;
begin
  Result := Trim(_1C);
end;

class function TUnicodeDataFormat.Unicode10Name(U1N: string): string;
begin
  Result := Trim(U1N);
end;

class function TUnicodeDataFormat.UpperCaseMapping(UCM: Integer): string;
begin
  if UCM > 0
    then Result := 'U+'+IntToHex(UCM,4)
    else Result := '';
end;

class function TUnicodeDataFormat.LowerCaseMapping(LCM: Integer): string;
begin
  if LCM > 0
    then Result := 'U+'+IntToHex(LCM,4)
    else Result := '';
end;

class function TUnicodeDataFormat.TitleCaseMapping(LCM: Integer): string;
begin
  if LCM > 0
    then Result := 'U+'+IntToHex(LCM,4)
    else Result := '';
end;*)

{ TUnicodeBlockList }

function TUnicodeBlockList.GetItem(Index: Integer): TUnicodeBlock;
begin
  Result := inherited GetItem(Index) as TUnicodeBlock;
end;

procedure TUnicodeBlockList.SetItem(Index: Integer; const Value: TUnicodeBlock);
begin
  inherited SetItem(Index, Value);
end;

{ TUnicodeBlock }

destructor TUnicodeBlock.Destroy;
begin
  if Assigned(FCacheCharData) then  // I2794
    FreeMem(FCacheCharData);
  FCacheCharData := nil;
  inherited Destroy;
end;

initialization
  with TVarData(_) do begin
    vType:= varError;
    vError:= Integer($80020004); {DISP_E_PARAMNOTFOUND}
  end;
end.


