unit JvCsvData;

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is by Warren Postma. 

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

Last Modified: 2003-04-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCsvDataSet in-memory-dataset component usable by any VCL Data Aware Controls.
              TJvCsvDataSet appears in the 'Jv Data Access' tab of the Component Pallette.

Known Issues:
  May 26, 2003 - Fixed errors handling null date values.
               - Fixed improper memory access for ftBoolean.
                 Booleans are stored internaly as a 16bit WordBool, inside
                 DataSets and the component was reading/writing a 32 bit value,
                 which could caused all kinds of squirrelly things to happen
                 when the boolean (ftBoolean, csv type '!') was encountered.
                 Search for the WordBool to see the changes.
-----------------------------------------------------------------------------}

//------------------------------------------------------------------------
//
// TJvCSVDataSet
//
// An in-memory TDataSet component similar to TTable but with optional
// saving to CSV file, and which, unlike using TTable in CSV mode, does not
// utilize the BDE, or any external database access layers to do its work.
//
// Since this component inherits from TDataSource, you can use it with any
// standard VCL data aware components.  Remember to link to a DataSource,
// before you can link this to any data aware controls!
//
//
// TJvCsvCustomInMemoryDataSet
//
// Internally, we first define a TJvCsvCustomInMemoryDataSet a base class.
// Nothing published.  This exists so you can easily inherit from it
// and define your own version of the component, and publish whatever
// properties and methods you wish to publish, and you can hide or
// override any other elements you don't wish to publish. 
//
// How To Use:
// You *must* first set up the important Property
// called CsvFieldDef which describes the expected fields and their types
// since the CSV file itself contains insufficient information to guess the
// field types.
//
// TODO: Make it default to all string types if no field types
// are provided.
//
// Example CsvFieldDef string:
//   ABC:$80,DEFG:$140,HIJKLMN:%,OPQRST:@
//
//   $ = string (ftString) - also used if no character is given.
//   % = whole integer value (ftInteger)
//   & = floating point value (ftFloat)
//   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
//   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
//   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
//   ! = Boolean Field (0 in csv file=false, not 0 = true, blank = NULL)
//
// NOTE: YOU SHOULD PROBABLY JUST USE THE BUILT-IN PROPERTY EDITOR (CLICK ...)
// INSTEAD OF MEMORIZING ALL THIS FIELD TYPE STUFF.
//
// Originally written by Warren Postma
// Contact: warren.postma@sympatico.ca or warrenpstma@hotmail.com
//
// Donated to the Delphi JEDI Project.
// All Copyrights and Ownership donated to the Delphi JEDI Project.
//------------------------------------------------------------------------

//  { $ R  JVCSVDATA.DCR }

interface

uses Windows,
     Messages,
     Db,
     SysUtils,
     Classes,
     Graphics;

const
   MAXCOLUMNS    = 80;
   DEFAULT_CSV_STR_FIELD= 80;
   MAXLINELENGTH = 2048;
   COLUMN_ENDMARKER = $FFFF;
   ON_BOF_CRACK  = -1;
   ON_EOF_CRACK  = -2;

   { return values from CompareBookmarks: }
   Bookmark_Less = -1; // b1 < b2
   Bookmark_Gtr  = 1;  // b1 > b2
   Bookmark_Eql  = 0;  // b1 = b2


type

 PInteger = ^Integer;
 PDouble = ^Double;
 PBoolean = ^Boolean;

 EJvCsvDataSetError = class(EDatabaseError); // Subclass DB.EDatabaseError so we can work nicely with existing Delphi apps.

 EJvCsvKeyError = class(EDatabaseError); // Key Uniqueness or Key Problem

 {  Special Event Types }
 TJvCsvOnSpecialData = procedure(Sender : TObject; Index : Integer; NonCsvData : String) of Object;

 TJvCsvOnGetFieldData = procedure(Sender : TObject; UserTag:Integer; UserData:Pointer; FieldName:String; var Value:String) of Object;
 TJvCsvOnSetFieldData = procedure(Sender : TObject; UserTag:Integer; UserData:Pointer; FieldName:String; Value:String) of Object;

 { SPECIAL TYPES OF  DATABASE COLUMNS FOR THIS COMPONENT }
 { Columns are numeric, text, or one of two kinds of Specially Encoded date/time formats: }
 TJvCsvColumnFlag = (jcsvNull, jcsvString,jcsvNumeric,jcsvAsciiDateTime, jcsvGMTDateTime, jcsvTZDateTime);

 { pointer to special CSV COLUMN }
 PCsvColumn = ^TJvCsvColumn;
// PFieldDef = ^TFieldDef;
 
 TJvCsvColumn = record
     FFlag       : TJvCsvColumnFlag; // Column CSV Format Flags
     FKeyFlag    : Boolean;           // This column is part of the primary key! (new May 2003-WP) 
     FPhysical   : Integer;           // Physical Column Ordering 
     FFieldDef   : TFieldDef;         // Associated FieldDef     
  end;

  { CSV COLUMNS are stored in a TList-Collection }
  TJvCsvColumns = class (TList)
    public
      procedure AddColumn(Item:PCsvColumn);
      function  FindByFieldNo(FieldNo:Integer):PCsvColumn;
      procedure Clear; override;
      function  FindByName(FieldName:String):PCsvColumn;
   end;
   

   TJvCsvBookmark = record
       flag : TBookmarkFlag;
       data : Integer;
   end;

    { CSV Data File Row is not very dynamic in this version: }
  PtrToPtrToCsvRow = ^PCsvRow; // bookmark data = double pointer indirection! Fun fun fun!
  PCsvRow = ^TJvCsvRow; // a pointer to a record
  TJvCsvRow = record { this MUST be a record, not a class, and must be a flag data record type }
      fdirty   : Boolean; // record is dirty (needs to be written to disk)
      columns  : Integer;
      index    : Integer; // FData Index (-1 means not in FData)
      wordfield: array[0..MAXCOLUMNS+1]    of Word; // lookup field beginning, Column Data (column dirty bit+column length) }
      text     : array[0..MAXLINELENGTH]   of Char; // lookup actual character data.
      // bookmark
      bookmark : TJvCsvBookmark;

      // filter flag;
      filtered : Boolean; // row is hidden from view right now.
      recursionFlag:Boolean; // helps us fix endless recursion bug in GetFieldData callbacks.

  end;

  { Row collection }
  TJvCsvRows = class (TList)
    protected
      FEnquoteBackslash:Boolean;
      // Optional user data (only allocated if used, how efficient is that, eh.)
      FUserData:Array of Pointer;
      FUserTag:Array of Integer;
      FUserLength:Integer;

      function GetUserTag(index:Integer):Integer;
      procedure SetUserTag(index,value:Integer);

      function GetUserData(index:Integer):Pointer;
      procedure SetUserData(index:Integer;value:Pointer);

    public
      procedure AddRow     (const Item:PCsvRow);
      procedure InsertRow  (const position:Integer; const Item:PCsvRow);


      procedure AddRowStr  (const Item:String); // convert String->TJvCsvRow
      function  GetRowPtr  (const RowIndex:Integer):PCsvRow;
      function  GetRowStr  (const RowIndex:Integer):String;
      procedure SetRowStr  (const RowIndex:Integer;Value:String);
      procedure DeleteRow  (const RowIndex:Integer);
      procedure SetARowItem(const RowIndex,ColumnIndex:Integer; Value:String);
      function  GetARowItem(const RowIndex,ColumnIndex:Integer):String;
      procedure Clear; override;
      property EnquoteBackslash:Boolean read FEnquoteBackslash write FEnquoteBackslash;
      property UserTag[index:Integer]:Integer read GetUserTag write SetUserTag;
      property UserData[index:Integer]:Pointer read GetUserData write SetUserData;
   end;



  // Easily Customizeable Dataset descendant our CSV handler and
  // any other variants we create:
  TJvCsvCustomInMemoryDataSet = class(TDataSet)

  protected
    FStoreDefs: Boolean;
    FEnquoteBackslash:Boolean; // causes _Enquote to use Backslashes. NOT the default behaviour.
    FTimeZoneCorrection:Integer; // defaults to 0 (none)
    FFileDirty : Boolean; // file needs to be written back to disk?

    FCsvFieldDef: String;            // Our own "Csv Field Definition String"
    FCsvKeyDef:String;  // CSV Key Definition String. Required if FCsvUniqueKeys is true
    FCsvKeyCount:Integer; // Set by parsing FCsvKeyDef
    FCsvKeyFields:Array of PCsvColumn;

    FCsvUniqueKeys:Boolean; // CSV Key Uniqueness option.  Also requires that all fields that are part of the Unique Key be Non Null.
    FCsvCaseInsensitiveComparison:Boolean; // CSV Key Uniqueness and Key Comparisons - case insensitive mode if True, else case sensitive.


    FIsFiltered:Boolean; // Filter conditions have been set.

    FEmptyRowStr:String; // A string of just commas (used to add a new empty row)
    FHeaderRow:String; // first row of CSV file.
    FTableName: string; // CSV File Name
    FRecordPos: Integer;
    FRecordSize:Integer;
    FBufferSize:Integer;
    FCursorOpen:Boolean;
    FFilterBuffer: PChar; // used when we implement filtering (later)
    FReadOnly : Boolean;
    FLoadsFromFile:Boolean;
    FHasHeaderRow:Boolean;
    FSavesChanges:Boolean;
    FAutoBackupCount:Integer; // Keep Last N Copies the Old Csv File, updated before each save? 
    FInsertBlocked:Boolean; // internal way to block new records but allows editing of existing ones!
    FPostBlocked:Boolean; // internal way to block posting of changes, but allows inserting of new ones!

    { data record holder }
    FCsvColumns:TJvCsvColumns; // Column information
    FData:TJvCsvRows;// Rows are a Collection of data pointers.

    { temporary holding space only, for a tstringlist of the file contents }
    FCsvFileAsStrings : TStringList;

    {  event pointers }
    FOnSpecialData  : TJvCsvOnSpecialData;
    FOnGetFieldData : TJvCsvOnGetFieldData; // Helps to allow you to update the contents of your CSV data from some other object in memory.
    FOnSetFieldData : TJvCsvOnSetFieldData; // Helps to keep some other thing in sync with the contents of a changing CSV file.


    //  Internal Use Only Protected Methods
//    function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer:  PChar; virtual;
    procedure CsvRowInit(RowPtr:PCsvRow);

    // New filtering on cursor (GetRecord advances the cursor past
    // any hidden rows using InternalSkipForward).
    function InternalSkipFiltered( defaultResult:TGetResult; ForwardBackwardMode:Boolean ):TGetResult;

    procedure InternalClearFileStrings;
    function InternalLoadFileStrings:Boolean;
    // Internal methods used by sorting:
    function InternalFieldCompare( Column:PCsvColumn; Left,Right:PCsvRow ):Integer;
    function InternalCompare( SortColumns:Array of PCsvColumn; SortColumnCount:Integer; Left,Right:PCsvRow ):Integer;

    // key uniqueness needs this:
    function InternalFindByKey( row:PCsvRow ):Integer;


    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data:Pointer' stored in treeviews, etc)  
    function GetRowUserData:Pointer;
    procedure SetRowUserData(userdata:Pointer);
    
    function  GetRowTag:Integer;
    procedure SetRowTag(tagValue:Integer);


    // protected TDataSet base METHODS:
    procedure SetTableName(const Value: string); virtual;
    function  FieldDefsStored: Boolean; virtual;
    function  GetCanModify: Boolean; override; //already virtual!

    // internal calls:
    procedure ProcessCsvHeaderRow(const header:String);
    procedure ProcessCsvDataRow(const datarow:String;index:Integer);
    procedure SetCsvFieldDef( CsvFieldDefs:String);



    { Mandatory VCL TDataSet Overrides - Pure Virtual Methods of Base Class }
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ClearCalcFields(Buffer:PChar); override;


    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override; // on Insertion???
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;

    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
{    procedure InternalInsert; override; } {not needed.}

    // Misc methods:
    procedure InternalClose; override;
//    procedure DestroyFields; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    { dataset designer calls these }
    procedure DefChanged(Sender: TObject); override;


    // handling functions for enquoting,dequoting string fields in csv files.
    // handles using the default Excel method which is to double the quotes inside
    // quotes.

    function _Enquote(strVal:String):String; virtual; // puts whole string in quotes, escapes embedded commas and quote characters!
    function _Dequote(strValue:String):String; virtual; // removes quotes


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    // SELECT * FROM TABLE WHERE <fieldname> LIKE <pattern>:
    procedure SetFilter( fieldname,pattern:String); // Make Rows Visible Only if they match filterString
                                                   
    procedure ClearFilter; // Clear all previous SetFilters, shows All Rows.

    /// procedure FilteredDeletion(Inverted:Boolean); /// XXX TODO?
    /// procedure DeleteRowsMatchingFilter; /// XXX TODO?
    /// procedure DeleteRowsNotMatchingFilter; /// XXX TODO?

    // this is necessary to make bookmarks work as well:
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    // Additional procedures
    procedure EmptyTable;

      // Tells controls to redraw.
    procedure Refresh;


    // A fast row lookup function specific to this CSV table object.
    function FindByCsvKey( key:String):Boolean;

    // Sort the table:
    procedure Sort( SortFields:String; Ascending:Boolean);

    // All rows have a UserData and UserTag property, these
    // next two functions quickly set all the userdata and usertag
    // values for all rows, which is a good way to set defaults
    // without having to iterate through the dataset.
    procedure SetAllUserData(data:Pointer);
    procedure SetAllUserTags(tagValue:Integer);


    // The UserData/UserTag properties apply to the row that the
    // cursor is sitting on. Without visibly moving the cursor,
    // its handy to get/set the usertag and data values.
    function  GetUserTag(recno:Integer):Integer;
    procedure SetUserTag(recno,newValue:Integer);

    function  GetUserData(recno:Integer):Pointer;
    procedure SetUserData(recno:Integer;newValue:Pointer);



    function GetCsvHeader:String;

    {  Additional Public methods }
    procedure OpenWith(Strings:TStrings); virtual; // does AssignFromStrings then opens table.

    procedure AppendWith(Strings:TStrings); virtual;


    { Special declarations }
    // first time call OpenWith, then you can call AssignFromStrings subsequently,
    // as long as the field names and positions have not changed.
    procedure AssignFromStrings(const Strings:TStrings);virtual; // update String data directly.
    procedure AssignToStrings(Strings:TStrings);virtual;

    procedure DeleteRows( FromRow, ToRow :Integer);     // NEW: Quickly zap a bunch of rows:
    procedure ExportRows( Filename :String; FromRow, ToRow :Integer);     // NEW: Quickly save a bunch of rows:

    procedure ExportCsvFile(const Filename:String);virtual; // save out to a file. does NOT keep backups! If file exists, it will be
        // overwritten, and NO backups are made!

    procedure Flush;virtual; // Save CSV file to disk if file has changed and SavesChanges is true.
                  // Note: FLUSH will make backup copies if FAutoBackupCount>0!!!

    { Row Access as String }
    function GetRowAsString(const Index:Integer):String;virtual;
    function GetColumnsAsString:String;virtual;

    function IsKeyUnique:Boolean; // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!

    property InternalData:TJvCsvRows read FData write FData;

    // NO published properties! This is a base class only!

  end;

  // TJvCsvDataSet is just a TJvCsvCustomInMemoryDataSet with all properties and events exposed:
   TJvCsvDataSet = class(TJvCsvCustomInMemoryDataSet)

    public
      property TableName : String read FTableName; // Another name, albeit read only, for the FileName property!

      // Per-Record user-data fields:
      //    Each record can have a pointer (for associating each row with an object)
      property UserData:Pointer read GetRowUserData write SetRowUserData;
      //    Each record can have a tag (integer) (for help in marking rows as Selected/Unselected or some other
      //    end user task)
      property UserTag:Integer read GetRowTag write SetRowTag;

      
    published
     property FieldDefs stored FieldDefsStored;
     property Active;
     property FileName: string read FTableName write SetTableName;
     property BufferCount;
     property ReadOnly : Boolean read FReadOnly write FReadOnly default False;
     property BeforeOpen;
     property AfterOpen;
     property BeforeClose;
     property AfterClose;
     property BeforeInsert;
     property AfterInsert;
     property BeforeEdit;
     property AfterEdit;
     property BeforePost;
     property AfterPost;
     property BeforeCancel;
     property AfterCancel;
     property BeforeDelete;
     property AfterDelete;
     property BeforeScroll;
     property AfterScroll;
     property OnDeleteError;
     property OnEditError;
     property OnCalcFields;

     // Additional Properties
     property Changed:Boolean read FFileDirty write FFileDirty;
//     property DataFileSize: Integer read GetDataFileSize;

     // CSV Table definition properties:
     property CsvFieldDef:String read FCsvFieldDef write SetCsvFieldDef; // Our own "Csv Field Definition String"
     property CsvKeyDef:String read FCsvKeyDef write FCsvKeyDef;  // Primary key definition.
     property CsvUniqueKeys:Boolean read FCsvUniqueKeys write FCsvUniqueKeys; // Rows must be unique on the primary key.
     property HasHeaderRow:Boolean read FHasHeaderRow write FHasHeaderRow default true;
     property CaseInsensitive:Boolean read FCsvCaseInsensitiveComparison write FCsvCaseInsensitiveComparison;
     


     // Properties for Automatically Loading/Saving CSV file when Active property is set true/false:
     property LoadsFromFile:Boolean read FLoadsFromFile write FLoadsFromFile default true;
     property SavesChanges:Boolean read FSavesChanges write FSavesChanges default true;
     property AutoBackupCount:Integer read FAutoBackupCount write FAutoBackupCount; // >0 means Keep Last N Copies the Old Csv File, updated before each save? 


     // Do field definitions "persist"?
     // Ie: do they get stored in DFM Form file along with the component
     property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
     { Additional Events }
     property OnSpecialData : TJvCsvOnSpecialData read FOnSpecialData write FOnSpecialData;
     property OnGetFieldData : TJvCsvOnGetFieldData read FOnGetFieldData write FOnGetFieldData;
     property OnSetFieldData : TJvCsvOnSetFieldData read FOnSetFieldData write FOnSetFieldData;

     { value in seconds : to do GMT to EST (ie GMT-5) use value of (-3600*5)
       This is only useful if you use the Hex encoded date-time fields.
     }
     property TimeZoneCorrection:Integer read FTimeZoneCorrection write FTimeZoneCorrection default 0;

     { If false (default) we use the more normal CSV rendering of quotes, which is to double them in
       the csv file, but if this property is true, we use backslash-quote to render quotes in the file,
       which has the side-effect of also requiring all backslashes to themself be escaped by a backslash.
       So filenames would have to be in the form "c:\\directory\\names\\like\\c\\programmers\\do\\it".
       Not recommended behaviour, except when absolutely necessary! }
     property EnquoteBackslash :Boolean read FEnquoteBackslash write FEnquoteBackslash default false;
   end;

{ CSV String Processing Functions }
procedure  CsvRowToString( RowItem:PCsvRow ; var RowString:String );

{ modified! }
procedure StringToCsvRow( const RowString:String   ; RowItem:PCsvRow ; permitEscapeSequences,enquoteBackslash:Boolean );


function   CsvRowItemCopy( Source,Dest:PCsvRow; FieldIndex,FieldSize:Integer ):Boolean;
procedure  SetCsvRowItem ( pItem:PCsvRow; ColumnIndex:Integer; NewValue:String);
function   GetCsvRowItem ( pItem:PCsvRow; ColumnIndex:Integer):String;
procedure  CsvRowSetDirtyBit(row:pCsvRow;ColumnIndex:Integer);
procedure  CsvRowClearDirtyBit(row:pCsvRow;ColumnIndex:Integer);
function   CsvRowGetDirtyBit(row:pCsvRow;ColumnIndex:Integer):Boolean;
procedure  CsvRowSetColumnMarker(row:pCsvRow;ColumnIndex:Integer;ColumnMarker:Integer);
function   CsvRowGetColumnMarker(row:pCsvRow;ColumnIndex:Integer):Integer;


{ Date/Time String decoding functions }
function TimeTHexToDateTime( HexStr:String ; TimeZoneCorrection : Integer ) : TDateTime;
function TimeTAsciiToDateTime ( AsciiDateStr : String ) : TDateTime;

{ Date/Time string encoding functions }
function DateTimeToTimeToIsoAscii(aDateTime:TDateTime):String;
function DateTimeToTimeTHex(aDateTime:TDateTime; TimeZoneCorrection:Integer ):String;


{ Routine to keep backup copies of old data files around }
function JvCsvBackupPreviousFiles( filename:String; MaxFiles:Integer):Boolean;

//JvCsvWildcardMatch:
// Recursive wildcard (%=AnyString, ?=SingleChar) matching function with
// boolean sub expressions (|=or, &=and).
function JvCsvWildcardMatch( data, pattern:String ):Boolean;

implementation

uses BDE, DBTables, DBConsts, Forms, Controls, Dialogs, JvCsvParse;

var
  CallCount : Integer;

procedure JvCsvDatabaseError(const TableName,Message: string);
begin
    OutputDebugString( PChar('JvCsvDatabaseError in '+TableName+': '+Message));
    raise EJvCsvDataSetError.Create(TableName+':'+Message);
end;


    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data:Pointer' stored in treeviews, etc)
function TJvCsvCustomInMemoryDataSet.GetRowUserData:Pointer;
var
  recno:Integer;
begin
  recno := GetRecNo;
  Result := FData.GetUserData(recno);
end;

procedure TJvCsvCustomInMemoryDataSet.SetRowUserData(userdata:Pointer);
var
  recno:Integer;
begin
  recno := GetRecNo;
  FData.SetUserData(recno,userdata);
end;

function  TJvCsvCustomInMemoryDataSet.GetRowTag:Integer;
var
  recno:Integer;
begin
  recno := GetRecNo;
  result := FData.GetUserTag(recno);
end;

procedure TJvCsvCustomInMemoryDataSet.SetRowTag(tagValue:Integer);
var
  recno:Integer;
begin
  recno := GetRecNo;
  FData.SetUserTag(recno,tagValue);
end;

function _WildcardsMatchBoolOp( data, pattern:String; boolOp:Char ):Boolean;
var
  subPattern:Array [0..20] of String;
  t,count:Integer;
begin
  count := StrSplit( pattern, boolOp, {Chr(0)=No Quoting}Chr(0), subPattern, 20);
  if (count >0 ) then begin
      for t := 0 to count-1 do begin
          result := JvCsvWildcardMatch( data, subPattern[t]);
          // If ANY OR TRUE return TRUE;
          // if ANY AND FALSE return FALSE;
          if (boolOp = '|') = result then begin
             exit;
          end;
      end;
  end else begin // split failed...
      result := false;
      exit;
  end;
  // if we get here, no short circuit was possible.
  if (boolOp = '|') then
      result := false // NONE of the OR conditions were met!
  else
      result := true; // ALL of the AND condition were met!
end;

procedure TJvCsvCustomInMemoryDataSet.SetAllUserTags(tagValue:Integer);
var
  row:PCsvRow;
  t:Integer;
begin
 FData.SetUserTag(FData.Count-1,tagValue);
  for t := 0 to FData.Count-2 do begin
      FData.SetUserTag(t,tagValue);
  end;
end;


procedure TJvCsvCustomInMemoryDataSet.SetAllUserData(data:Pointer);
var
  row:PCsvRow;
  t:Integer;
begin
  FData.SetUserData(FData.Count-1,data); // Optimization. Ensures we only call SetLength ONCE!
  for t := 0 to FData.Count-2 do begin
     Fdata.SetUserData(t,data);
  end;
end;


function TJvCsvCustomInMemoryDataSet.GetUserTag(recno:Integer):Integer;
begin
  result := FData.GetUserTag(recno);
end;


procedure TJvCsvCustomInMemoryDataSet.SetUserTag(recno,newValue:Integer);
begin
  FData.SetUserTag(recno,newValue);
end;

function TJvCsvCustomInMemoryDataSet.GetUserData(recno:Integer):Pointer;
begin
  result := FData.GetUserData(recno);
end;

procedure TJvCsvCustomInMemoryDataSet.SetUserData(recno:Integer;newValue:Pointer);
begin
  FData.SetUserData(recno,newValue);
end;



// Recursive wildcard matching function
function JvCsvWildcardMatch( data, pattern:String ):Boolean;
var
  t:Integer;
  firstwildcard:Integer;
  datalength,patternlength,dataposition,patternposition :Integer;
  firstBoolCondition:Integer;
begin
  datalength := Length(data);
  patternlength := Length(pattern);
  // no data?
  if (datalength = 0) then begin
      if (pattern = '%') or (pattern = '') then begin
          result := true;
          exit;
      end;
      result := false;
      exit; // definitely no match.
  end;
  // no pattern?
  if (patternlength = 0) then begin
      result := true; //everything matches a non-pattern.
  end;
  // replace all '%%' -> '%' (don't put duplicate wildcards in)
  t := 1;
  while t < patternlength do begin
      if (pattern[t] = '%') and (pattern[t+1] = '%') then begin
          pattern := Copy(pattern,1,t)+Copy(pattern,t+2,patternlength);
          patternlength := Length(pattern);
      end else
          Inc(t);
  end;
  // find any | and split into two or more strings, and run ORs on them
  firstBoolCondition := Pos('&',pattern);
  if (firstBoolCondition>0) then begin
      result := _WildcardsMatchBoolOp( data, pattern, '&'); 
      exit;
  end;
  firstBoolCondition := Pos('|',pattern);
  if (firstBoolCondition>0) then begin
      result := _WildcardsMatchBoolOp( data, pattern, '|' );
      exit;
  end;



  firstwildcard := Pos('%',pattern); // wildcards?
  if (firstwildcard = 0) then
    firstwildcard := Pos('?',pattern); // other wildcard.

  if (firstwildcard<=0) then begin // no wildcard case.
      if (data = pattern) then
          result := true
      else
          result := false;
      exit; // simple match returns immediately.
  end;
  // wildcard tail?
  if ((firstwildcard = patternlength) and (pattern[1] <> '?')) then begin // prefix match
      if Copy(data,1,patternlength-1) = Copy(pattern,1,patternlength-1) then
          result := true
      else
          result := false;
      exit; // tail case is easy!
  end;
  // match literal characters until we hit wildcards,
  // then search for a wildcard resync, which continues
  // recursively.
  result := true;
  dataposition := 1;
  patternposition := 1;
  while ((dataposition <= datalength) and (patternposition <= patternlength)) do begin
    // WILDCARD HANDLER
    if (pattern[patternposition] = '?') then begin // match any one character or nothing.
        Inc(patternposition);
        Inc(dataposition);
    end else if (pattern[patternposition] = '%' ) then begin
       if (patternposition = patternlength) then begin // last byte!
            result := true;
            exit;
       end;
       // Resync after %:
       t := Pos( pattern[patternposition+1], data );
       while t>0 do  begin// possible resync point!
                result :=JvCsvWildcardMatch( Copy(data,t,Length(data)),
                                        Copy(pattern,patternposition+1,patternlength)
                                      );
                if (result) then
                    exit;   // found a resync, and rest of strings match
                data := Copy(data,t+1,datalength);
                datalength := Length(data);
                dataposition := 0;
                if (datalength = 0) then begin
                    result := false;
                    exit;
                end;
                t := Pos( pattern[patternposition+1], data );
       end;  { end while loop }
       // failed to resync
       result := false;
       exit;
    end else begin // NORMAL CHARACTER
      if (data[dataposition] <> pattern[patternposition]) then begin
            result := false; // failed.
            exit;
      end;
      Inc(dataposition);
      Inc(patternposition);
    end;
  end; {while}
  if (    (dataposition <= datalength)
      and (patternposition <= patternlength)
     ) then begin
        result := false; // there is pattern left over, or data left over.
       end;
end;

// NEW: TJvCsvCustomInMemoryDataSet.SetFilter
//
// XXX Simplest possible filtering routine. Not very flexible.
// XXX Todo: Make this more flexible.
// XXX Users can also subclass and write their own filter.
// XXX Perhaps a OnFilter event should be provided, and SetCustomFilter
// XXX method would allow us to do a row by row filtering scan, and then
// XXX hide rows that the user sets HideRow := true in the event handler.
// XXX
procedure TJvCsvCustomInMemoryDataSet.SetFilter( fieldname,pattern:String); // Make Rows Visible Only if they match filterString
var
  valueLen, t   : Integer;
  pRow          : PCsvRow;
  fieldRec      : PCsvColumn;
  fieldIndex    : Integer;
  fieldValue    : String;
begin
//  fieldIndex := Self.FCsvColumns.
  fieldRec := FCsvColumns.FindByName(fieldname);
  if not Assigned(fieldRec) then exit;
  fieldIndex := fieldRec^.FPhysical;
  valueLen := Length(pattern); // if valuelen is zero then we are searching for blank or nulls
  pattern := UpperCase(pattern); // make value case insensitive.

  // Now check if field value matches given pattern for this row.
  for t := 0 to FData.Count -1 do begin
       pRow := PCsvRow(FData[t]);
       if (not pRow^.filtered) then begin
              fieldValue := FData.GetARowItem(t,fieldindex);
              if (Length(fieldValue)>0) and (fieldValue[1] = '"') then
                   fieldValue := _Dequote(fieldValue); // remove quotes.
              if (valuelen = 0) then begin
                    if fieldValue <> '' then // if not empty, hide row.
                        pRow^.filtered := true;
              end else begin
                 fieldValue := UpperCase(fieldValue);
                  if not JvCsvWildcardMatch( fieldValue, pattern ) then // hide row if not same prefix
                      pRow^.filtered := true;
              end;

       end
   end;
   FIsFiltered := true;
   First; // redisplay controls
end;

procedure TJvCsvCustomInMemoryDataSet.ClearFilter; // Clear Previous Filtering.
var
  t:Integer;
  pRow:PCsvRow;
begin
   for t := 0 to FData.Count -1 do begin
       pRow := PCsvRow(FData[t]);
       if Assigned(pRow) then 
         pRow^.filtered := false; // clear all filter bits.
   end;
   FIsFiltered := false;
   First; // redisplay controls
end;




// note that file is not being locked!

{ TJvCsvCustomInMemoryDataSet }

constructor TJvCsvCustomInMemoryDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);


  // FRecordSize = size of a csv text buffer and the indexes pointing
  //               into that buffer:

  FRecordSize := SizeOf(TJvCsvRow) - Sizeof(TJvCsvBookmark);

  // FBuffer size includes CSV Text buffer, and the bookmark data, followed
  // by space for storing the binary form of a calculated-field:

  // initial FBufferSize size: My theory is that we should pick a conservative
  // estimate plus a margin for error:
  FBufferSize := SizeOf(TJvCsvRow) + 128; {CalcFieldsSize}; // our regular record + calculated field data.

  FReadOnly := false;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
  FLoadsFromFile := true;
  FSavesChanges := true;
  FHasHeaderRow := true;

  { Additional initialization }
  FCsvColumns:= TJvCsvColumns.Create;
  FData := TJvCsvRows.Create;
  FData.EnquoteBackslash := FEnquoteBackslash;

end;

destructor TJvCsvCustomInMemoryDataSet.Destroy;
begin
  InternalClearFileStrings; // delete file strings

 try
  if FCursorOpen then InternalClose;

 except
 end;
  if Assigned(FCsvColumns) then begin
    FCsvColumns.Clear;
    FCsvColumns.Free;
  end;
  if Assigned(FData) then begin
    FData.Clear;
    FData.Free;
  end;
  inherited Destroy;
end;


function TJvCsvCustomInMemoryDataSet.AllocRecordBuffer: PChar;
var
  RowPtr:PCsvRow;
begin
  RowPtr := AllocMem(FBufferSize{Sizeof(TJvCsvRow)});
//  Trace('AllocRecordBuffer result=$'+IntToHex(Integer(Pointer(RowPtr)),8));
  result := PChar(RowPtr);
end;

{ calc fields support }
procedure TJvCsvCustomInMemoryDataSet.ClearCalcFields(Buffer: PChar);
begin
     // Assumes that our buffer is a TJvCsvRow followed by
     // a dynamically resized buffer used for calculated field
     // storage:
     FillChar(Buffer[Sizeof(TJvCsvRow)],CalcFieldsSize,0);
end;

{ calc fields support and buffer support }
function TJvCsvCustomInMemoryDataSet.GetActiveRecordBuffer:  PChar;
begin
  case State of
    dsBrowse:
         if IsEmpty then
                    Result := nil
            else
                    Result := ActiveBuffer;

  dsCalcFields:
          Result := CalcBuffer;

  dsFilter:
          Result:=FFilterBuffer;
          
  dsEdit,dsInsert:
          Result:=ActiveBuffer;
  else
      Result:=nil;
  end;
end;


procedure TJvCsvCustomInMemoryDataSet.SetCsvFieldDef( CsvFieldDefs:String);
begin
 if (FCsvFieldDef <> CsvFieldDefs) then begin
    CheckInactive;
    FCsvFieldDef := CsvFieldDefs;
    FHeaderRow := ''; 
    FieldDefs.Clear;   // Clear VCL Database field definitions
    FCsvColumns.Clear; // Clear our own CSV related field data
    FData.Clear;       // Clear out data
 end;
end;

procedure TJvCsvCustomInMemoryDataSet.FreeRecordBuffer(var Buffer: PChar);
//var
//  RowPtr:PCsvRow;
begin
  //Trace( 'FreeRecordBuffer '+IntToHex(Integer(Buffer),8) );
// try 
    if Buffer <> NIL then
       FreeMem(Buffer);
// except
     //Trace( 'FreeRecordBuffer - Exception freeing '+IntToHex(Integer(Buffer),8) );
//  end;
//  //Trace('TJvCsvCustomInMemoryDataSet.FreeRecordBuffer');

end;

{ called after the record is allocated }
procedure TJvCsvCustomInMemoryDataSet.InternalInitRecord(Buffer: PChar);
var
  RowPtr:PCsvRow;
begin
  //Trace( 'InternalInitRecord '+IntToHex(Integer(Buffer),8) );

  FillChar ( Buffer^, FBufferSize, 0 );
  RowPtr := PCsvRow( Buffer ); // Zero out the buffer.
  CsvRowInit(RowPtr);
end;

// CsvRowInit
//
// Internal handy dandy function to set up a new csv row.
// which is intially full of just commas.
//
procedure TJvCsvCustomInMemoryDataSet.CsvRowInit(RowPtr:PCsvRow);
var
  T : Integer;
  ColCount:Integer;
begin
  RowPtr^.index := -1; // Not Yet Indexed
  RowPtr^.fdirty := false;
  RowPtr^.bookmark.flag := bfEOF;
  RowPtr^.bookmark.data := ON_BOF_CRACK; // no index into FData yet.
  CsvRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);
  

  ColCount := FCsvColumns.Count;
  if ColCount <= 0 then ColCount := 10;

  for t := 1 to ColCount do begin // create an empty line of just commas
    if (t <ColCount) then
        RowPtr^.text  [t-1] := ','
    else
        RowPtr^.text  [t-1] := Chr(0);
    RowPtr^.text  [t]   := Chr(0);
    CsvRowSetColumnMarker(RowPtr,{column}t-1,{marker value}t-1);
    CsvRowSetColumnMarker(RowPtr,{column}t,{marker value}COLUMN_ENDMARKER);
  end;
end;


function TJvCsvCustomInMemoryDataSet.IsKeyUnique:Boolean; // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
begin
  result := false; // not yet implemented! XXX
end;

function TJvCsvCustomInMemoryDataSet.InternalSkipFiltered( defaultResult:TGetResult; ForwardBackwardMode:Boolean ):TGetResult;
var
  LimitReached:Boolean;
  RowPtr:PCsvRow;
begin
 if (FRecordPos<0) then begin
     result := defaultResult;
     exit;
 end;
  LimitReached := false; // hit BOF or EOF?
 while not LimitReached do begin
    { no skippage required }
    RowPtr := PCsvRow(FData.GetRowPtr(FRecordPos));
    if (not RowPtr^.filtered) then begin
         result := defaultResult;
         exit;
    end;
    { skippage ensues }
    if (ForwardBackwardMode) then begin // ForwardSkip mode
      Inc(FRecordPos);
      if (FRecordPos>=FData.Count) then begin
                FRecordPos := ON_EOF_CRACK;
                result := grEOF;
                exit;
      end;
    end else begin // BackwardSkip mode
      Dec(FRecordPos);
      if (FRecordPos<0) then begin // hit BOF_CRACK
                FRecordPos := ON_BOF_CRACK;
                result := grBOF;
                exit;
      end;
    end;
 end;
end;

function TJvCsvCustomInMemoryDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowPtr:PCsvRow;
begin

 Buffer[0] := Chr(0);
 Result := grEOF;
 if FData.Count < 1 then begin
    //Trace(' GetRecord - called when data buffer empty.');
    exit;
 end;
 case GetMode of
      gmPrior:
       begin
        //Trace(' GetRecord( Buffer, gmPrior, DoCheck)');
        if FRecordPos = ON_BOF_CRACK then
           result := grBOF
        else if FRecordPos = ON_EOF_CRACK then begin
           FRecordPos := FData.Count-1;

          // NEW FILTERING
           if FIsFiltered then
               result := InternalSkipFiltered(grOk,false) // skipping backwards.
           else
               result := grOk;
        end else if FRecordPos > 0 then begin
           Dec(FRecordPos);

          // NEW FILTERING
           if FIsFiltered then
               result := InternalSkipFiltered(grOk,false) // skipping backwards.
           else
               result := grOk;

        end else
           result := grBOF;
       end;


      gmCurrent:
        begin

         //Trace(' GetRecord( Buffer, gmCurrent, DoCheck)');
         if (FRecordPos<0) then // BOF Crack or EOF Crack?
            result := grError
         else
            result := grOk;

          // NEW FILTERING
           if FIsFiltered then
               result := InternalSkipFiltered(result,true); // skipping forwards.

         end;
      gmNext:
        begin
         //Trace(' GetRecord( Buffer, gmNext, DoCheck)');
         if FRecordPos = ON_EOF_CRACK then
            result := grEOF
         else begin
          Inc(FRecordPos);

          if (FRecordPos >= FData.Count) then begin
            FRecordPos := ON_EOF_CRACK;
            result := grEOF
          end else begin
            // NEW FILTERING
            if FIsFiltered then
                result := InternalSkipFiltered(grOk,true) // skipping forwards.
            else
                result := grOk;
          end;

        end
       end;
        
      // default case:
      else
        JvCsvDatabaseError( FTableName, 'GetMode???');
    end; {end case}

    if Result = grOk then
    begin
       //Trace( ' GetRecord FRecordPos='+IntToStr(FRecordPos)+'result=grOk' );
      try
        { get a record into a buffer }
        RowPtr := PCsvRow(Buffer); // Cast to a Row Data Structure to our own type.
        Move( {source:}FData.GetRowPtr(FRecordPos)^,  {dest:}RowPtr^, SizeOf(TJvCsvRow) );
        RowPtr^.bookmark.flag := bfCurrent;
        RowPtr^.bookmark.data := FRecordPos;

        // Update calculated fields for this row:
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);

      except
        JvCsvDatabaseError(FTableName,'Problem reading row '+IntToStr(FRecordPos));
      end;
    end
    else begin

      // fudge: Get bookmark into a record for BOF and EOF records:
{      if RowPtr <> NIL then
          RowPtr^.bookmark.data := FRecordPos;}

      if (Result = grError) and DoCheck then
       JvCsvDatabaseError(FTableName,'No records');
    end;

//    if (Result = grError) then
          //Trace(' GetRecord result = grError');
//    if (Result = grEof) then
          //Trace(' GetRecord result = grEof');
//     if (Result = grBof) then
          //Trace(' GetRecord result = grBof');





end;

function TJvCsvCustomInMemoryDataSet._Enquote(strVal:String):String; // puts whole string in quotes, escapes embedded commas and quote characters!
var
  s:String;
  t,l:Integer;
  ch:Char;
begin
   s := '"';
   l := Length(strVal);
   for t := 1 to l do begin
        ch := strVal[t];
        if FEnquoteBackslash then begin // backslash quoting ( C/C++ )
        if ch = '\' then   // double each backslash
                s := s + '\\'
        else if ch = '"' then // escape quotes with a backslash
                s := s + '\"'
        else
                s := s + ch;
        end else begin
                // simpler method: doubled-quotes ( Pascal )
                if ch = '"' then // escape quotes by doubling them.
                        s := s + '""'
                else
                        s := s + ch;

        end;
   end;
   s := s + '"'; // end quote.
   result := s;
end;


function TJvCsvCustomInMemoryDataSet.GetRecordSize: Word;
begin
 // In create:
 //    FRecordSize := Sizeof(TJvCsvRow) - Sizeof(TJvCsvBookmark);
 result := FRecordSize;
end;

procedure TJvCsvCustomInMemoryDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RowPtr:PCsvRow;
  NewVal:String;
  PhysicalLocation:Integer;
  pDestination:PChar;
  CsvColumnData :PCsvColumn;
  DT:TDateTime;
begin

  //Trace( 'SetFieldData '+Field.FieldName );
 pDestination :=GetActiveRecordBuffer;
 RowPtr := PCsvRow(pDestination);
 
 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow)=0) then begin
      FHeaderRow := GetColumnsAsString;
      ProcessCsvHeaderRow(FHeaderRow);
  end;


 // If this is a calculated field or lookup field then...
 if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then begin
    if (Field.Offset = 0) then begin
         OutputDebugString('JvCsvData.SetFieldData: Invalid field.Offset in Calculated or Lookup field.');
         exit;
    end;
    Inc(pDestination,Sizeof(TJvCsvRow)+Field.Offset);
    Boolean(pDestination[0]):=(Buffer<>nil);
    if Boolean(pDestination[0]) then
        CopyMemory(@pDestination[1],Buffer,Field.DataSize);
    //result := true; {there is no return value, oops}
    exit;
 end;

 // If we get here, we are dealing with a physical record:

 // Set a field data, taking the physical to logical ordering translation into
 // account:
 CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
 if not Assigned(CsvColumnData) then
    exit;

 PhysicalLocation := CsvColumnData^.FPhysical;

 if PhysicalLocation < 0 then
     exit;

 if Buffer = NIL then
   NewVal := ''
 else
  case Field.DataType of
       ftString:
          begin
            NewVal := String(PChar(Buffer));
            //----------------------------------------------------------------------------------------------------
            // NEW RULE: If user displayed value contains a comma, a backslash, or a double quote character
            // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
            // and backslashes preceded by a backslash character.
            //----------------------------------------------------------------------------------------------------
            if     ( Pos(',',NewVal) > 0 )
                or ( Pos('"',NewVal) > 0 )
                or (( Pos('\',NewVal) > 0 ) and (FEnquoteBackslash))
                then begin
                  NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
                end;
          end;
       ftInteger:
          begin
           NewVal := IntToStr(PInteger(Buffer)^);
          end;
       ftFloat:
         begin
           NewVal := FloatToStr(PDouble(Buffer)^);
         end;
       ftBoolean:
          begin
                      NewVal := IntToStr( Ord(PWordBool(Buffer)^)); // bugfix May 26, 2003 - WP
          end;

         // There are two ways of handling date and time:
       ftDateTime:
         case CsvColumnData^.FFlag of
              // Localized time in Ascii
             jcsvAsciiDateTime:
                begin
                  DT := TimeStampToDateTime(
                                MSecsToTimeStamp(
                                    Double(Buffer^) ));
                  NewVal := DateTimeToTimeToIsoAscii(DT);
                  //OutputDebugString(PChar('date '+NewVal));

                end;

             // GMT Times are stored in HEX
             jcsvGMTDateTime:
                begin
                DT := TimeStampToDateTime(
                           MSecsToTimeStamp(
                                   Double(Buffer^) ));
                 NewVal := DateTimeToTimeTHex( DT, 0);

                end;

             jcsvTZDateTime: // Move a GMT time into a timezone:
                begin
                  DT := TimeStampToDateTime(
                                MSecsToTimeStamp(
                                    Double(Buffer^) ));
                 NewVal := DateTimeToTimeTHex( DT, FTimeZoneCorrection);

                end;

             else
              JvCsvDatabaseError(FTableName,'SetFieldData Error - TimeT-to-DateTime conversion error.');
         end;
       else
         JvCsvDatabaseError(FTableName,'SetFieldData Error - Field type not handled.');
  end;

 // Set new data value (NewVal = String)
 SetCsvRowItem( RowPtr, PhysicalLocation, NewVal );
 if Assigned(FOnSetFieldData) and (RowPtr^.index >= 0) then begin
    FOnSetFieldData(Self, FData.GetUserTag(RowPtr^.index), FData.GetUserData(RowPtr^.index), Field.FieldName, NewVal);
 end;

 // Set a dirty bit so we remember to write this later:
 CsvRowSetDirtyBit(PCsvRow(pDestination),PhysicalLocation);

 // Set the file-wide dirty bit:
 FFileDirty := true;

 // Notify controls of a field change:
 DataEvent(deFieldChange, LongInt(Field));

end;




// Removes first and last character of the string (assumes they are quotes,
// to be called byGetFieldData only!)
function TJvCsvCustomInMemoryDataSet._Dequote(strValue:String):String;
var
 s:String;
 t,l:Integer;
 ch:Char;
 skipFlag:Boolean;
begin
  l := Length(strValue);
  skipFlag := false;
  s := '';
  if Length(StrValue)<2 then begin
        result := s;
        exit;
  end;

  for t := 2 to l-1 do begin
        ch := strValue[t];
        if FEnquoteBackslash then begin
           if (not skipFlag) and ( ch = '\' ) then begin
                skipFlag := true; // whatever is after the backslash is an escaped literal character.
                continue;
           end else
                skipFlag := false;
        end else begin
                if (not skipFlag) and (ch = '"') and (t < (l-1)) and ( strValue[t+1] = '"') then begin
                        skipFlag := true;
                        continue; // skip first of the doubled quote characters.
                end else
                        skipFlag := false;
        end;
        s := s + ch;
  end;
  result := s;
end;

// Tells controls to redraw.
procedure TJvCsvCustomInMemoryDataSet.Refresh;
var
  m: TBookmark;
begin
  if State <> dsBrowse then exit;
  
  m := GetBookmark;  // This appears a bit silly but it works very well.
  First;  // Redraws all controls once to relocate to top.
  GotoBookmark(m); // Go back where we were. This could result in some odd scrolling behaviour but I haven't seen it yet.
end;

function TJvCsvCustomInMemoryDataSet.GetFieldData(Field: TField; Buffer: Pointer):Boolean;
var
  RowPtr:PCsvRow;
  {ActiveRowPtr:PCsvRow;}
  pSource:PChar;
  pTempBuffer:PChar;
  UserString,TempString:String;
  PhysicalLocation:Integer;
  CsvColumnData :PCsvColumn;
  aDateTime:TDateTime;
  l:Integer;
  ts:TTimeStamp;
begin
  result := false;
  //Trace( 'GetFieldData '+Field.FieldName );

  if not FCursorOpen then
    Exit;
  if Field = NIL then
    exit;

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow)=0) then begin
      FHeaderRow := GetColumnsAsString;
      ProcessCsvHeaderRow(FHeaderRow);
  end;


  pSource := GetActiveRecordBuffer;
  if (pSource=nil) then begin
    //JvCsvDatabaseError('CsvDataSet.GetFieldData: Unable to get active record buffer');
    Exit;
  end;
   
  //------------------------------------------------------------------------
  // Calculated and Lookup Field Handling
  //
  // direct memory copy into calculated field or lookup field data area
  //------------------------------------------------------------------------
  if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then begin
       if (Field.Offset = 0) then begin
            // Invalid offset!
            OutputDebugString('JvCsvData.GetFieldData: Invalid field.Offset in Calculated or Lookup field.');
       end;
      Inc(pSource,Sizeof(TJvCsvRow)+Field.Offset);
      if (pSource[0]=#0) or (Buffer=nil) then
         Exit
      else // Get the field data from the buffer:
         CopyMemory(Buffer, @pSource[1], Field.DataSize);
      result := true;
      exit;
  end;


  //------------------------------------------------------------------------
  // If we get here we must be dealing with a real column of data
  // that is part of the CSV file rather than a calculated or lookup
  // field that is just in internal-memory:
  //------------------------------------------------------------------------

  CsvColumnData := FCsvColumns.FindByFieldNo( Field.FieldNo );
  if NOT Assigned(CsvColumnData) then begin
    JvCsvDatabaseError(FTableName,'Unable to locate CSV file information for field '+Field.Name);
    exit;
  end;
  PhysicalLocation := CsvColumnData^.FPhysical;
  if PhysicalLocation < 0 then begin // does it really exist in the CSV Row?
    JvCsvDatabaseError(FTableName,'Physical location of CSV field '+Field.FieldName+' unknown.');
    exit;
  end;

  //------------------------------------------------------------------------
  // All items in the CSV table are natively stored as strings. Note that
  // an empty string is considered to be a NULL if the field type is anything
  // other than a ftString. There are no NULLs in ftString fields because
  // a CSV file can store an empty string but has no way of indicating a NULL.
  //------------------------------------------------------------------------

  RowPtr := PCsvRow(pSource);

  TempString:=GetCsvRowItem( RowPtr, PhysicalLocation);

  // Strip quotes first!
  if (Field.DataType = ftString) then begin
       l := Length(TempString);
       if (l>=2) then
           if (TempString[1] = '"') and (TempString[l] = '"') then begin // quoted string!
                  TempString := _Dequote(TempString);
           end;
  end;

  // Custom Get Method allows us to create a "Virtual DataSet" where the data
  // in the CSV rows is really just a mirror which can be updated when displayed
  // but which we really are fetching from somewhere else.
  if Assigned(FOnGetFieldData) and (RowPtr^.index>=0) and (not RowPtr^.recursionFlag )then begin
      RowPtr^.recursionFlag := true;
      UserString := TempString;
      FOnGetFieldData(Self, FData.GetUserTag(RowPtr^.index), FData.GetUserData(RowPtr^.index), Field.FieldName,UserString);
      if (UserString <> TempString) then begin
            // Write changed value back to row:
            SetCsvRowItem(RowPtr, PhysicalLocation, UserString);
            TempString := UserString;
             // Notify controls of a field change:
             //DataEvent(deFieldChange, LongInt(Field));
             // XXX Doesn't do what I needed. left here commented out
             // in case I ever go back and try to get something like this
             // working again.

      end;
      RowPtr^.recursionFlag := false;
  end;

    // NULL:  There are no "Real" NULLS in an ASCII flat file, however for anything
  // other than a string field, we will return "NULL" to indicate there is an
  // empty string in the field.
  if (Field.DataType <> ftString) then
     if Length(TempString) = 0 then
           exit; // NULL field.

  { If buffer is nil, then we are being asked to do a null check only.}
  if (Buffer = nil) then begin
     if Length(TempString)=0 then
        result := false
     else
        result := true;
     exit; { cannot actually copy data into nil buffer, so returns now. }
  end;


  //------------------------------------------------------------------------
  // If we get here Buffer must NOT be nil. Now we handle
  // some csv to TField conversions:
  //------------------------------------------------------------------------
 try
  case Field.DataType of
        // Basic string copy, convert from String to fixed-length
        // buffer, padded with NUL i.e. Chr(0):
       ftString:
          begin
               pTempBuffer := AllocMem(Field.Size+1);  // AllocMem fills with zeros
               StrCopy(pTempBuffer,PChar(TempString)); // we copy in the data portion
               Move(pTempBuffer^,Buffer^,Field.Size);  // Buffer^ is now zero padded.
               FreeMem(pTempBuffer);                   // Free the memory we allocated.
          end;

        // Standard Integer conversion:
       ftInteger:   PInteger(Buffer)^ := StrToInt(TempString);

        // Standard Double-precision Float conversion:
       ftFloat:
             begin
                   PDouble(Buffer)^ := StrToFloat(TempString)
             end;

       ftBoolean:
         begin
             if Length(TempString)=0 then begin
                 PInteger(Buffer)^ := 0;
             end else begin
             if StrToIntDef(TempString,0) <> 0 then
                 PWordBool(Buffer)^ := True // bugfix May 26, 2003 - WP
              else
                 PWordBool(Buffer)^ := False; // bugfix May 26, 2003 - WP
             end;
           end;
       ftDateTime:
         case CsvColumnData^.FFlag of
             // Ascii Date 1999/03/05 08:23:15
             jcsvAsciiDateTime:
             begin
               aDateTime := TimeTAsciiToDateTime(TempString);
               if aDateTime <= 1.0 then begin
                        result := false; { field is NULL, no date/time value }
                        exit;
               end;
               ts := DateTimeToTimeStamp(aDateTime);
               if (ts.Time = 0) and (ts.Date=0) then begin
                    OutputDebugString('DateTimeToTimeStamp internal failure.');
                    exit;
               end;
                // XXX Delphi Weirdness Ahead.  Read docs before you try to
                // understand this. We want to store 8 bytes at Buffer^, this
                // is how we do it.
               Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(aDateTime));

             end;

             // GMT Times are Stored in HEX:
             jcsvGMTDateTime:
                Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,0)));

             // Move GMT into a Timezone:
             jcsvTZDateTime:
                Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,FTimeZoneCorrection)));


         else
             JvCsvDatabaseError(FTableName, 'GetFieldData Error - TimeT-to-DateTime conversion error.');
         end;   {end case}
       else // not a valid ftXXXX type for this TDataSet descendant!?
           JvCsvDatabaseError(FTableName,'GetFieldData Error- Field type not handled.');
     end // end case.
  except
    on E:EConvertError do begin
        result := false; // return a NULL.
        exit;
    end;
  end;
  // All is Well.
  result := true;
end;



// Our bookmark data is a pointer to a PCsvData
procedure TJvCsvCustomInMemoryDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
//var
//  t:Integer;
begin
// t:= PCsvRow(Buffer)^.bookmark.data;
 PInteger(Data)^ := PCsvRow(Buffer)^.bookmark.data;
end;

function TJvCsvCustomInMemoryDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
   result := PCsvRow(Buffer)^.bookmark.flag;
end;

// nobody mentioned that I needed this to be overloaded, but I only found
// out when I found that DBGrid and other controls that compare bookmarks
// won't function if you don't provide a non-default implementation of this.
function TJvCsvCustomInMemoryDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
 v1,v2:Integer;
begin
 v1 := -999999;
 v2 := -999999;
 if Bookmark1 <> NIL then
    v1 := Integer(Bookmark1^);
 if Bookmark2 <> NIL then
    v2 := Integer(Bookmark2^);
 result := Bookmark_Eql;
 if v1 < v2 then
        result := Bookmark_Less
 else if v1 > v2 then
        result := Bookmark_Gtr;
end;

procedure TJvCsvCustomInMemoryDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCsvRow(Buffer)^.bookmark.flag := Value;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  {Bookmark is just pointer to integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FRecordPos := PCsvRow(Buffer)^.bookmark.data;//Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:
procedure TJvCsvCustomInMemoryDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PCsvRow(Buffer)^.bookmark.data := PInteger(Data)^; 
end;

procedure TJvCsvCustomInMemoryDataSet.InternalFirst;
begin
//  Eof := false;
  FRecordPos := ON_BOF_CRACK;
end;

// CsvFieldDef:
//
// A property of our Data Set called CsvFieldDef is treated as
// declaration of the fields in the CSV table.
//
//   <coldef>,<coldef>,...,<coldef>
//   <coldef> = columname:<data-type-character><size>
//
// See top of file!

procedure TJvCsvCustomInMemoryDataSet.InternalInitFieldDefs;
var
  CsvFieldRec     : TJvCsvRow; //record type.
  CsvFieldOption  : String;
  CsvFieldName    : String;
  pCsvFieldDef    : PCsvColumn;
  t, colnum, pos1 : Integer;
  // field options:
  FieldTypeChar:Char;
  VclFieldType : TFieldType;
  FieldLen     : Integer;
  FieldType : TJvCsvColumnFlag;
  aCsvFieldDef : String;
  CsvKeys:Array of String;
//  FDef:TFieldDef;
begin



//  FFieldsInitialized := true;
  FieldType := jcsvString;
  VclFieldType := ftString;

  // create FieldDefs which map to each field in the data record
  FieldDefs.Clear;   // Clear VCL Database field definitions
  FCsvColumns.Clear; // Clear our own CSV related field data


  aCsvFieldDef :=   FCsvFieldDef;
  if Length(aCsvFieldDef)=0 then begin
        if FHasHeaderRow and InternalLoadFileStrings then
                aCsvFieldDef := FCsvFileAsStrings[0];
  end;


  if Length(aCsvFieldDef) > 0 then begin
     StringToCsvRow(aCsvFieldDef,@CsvFieldRec, false,false);



     colnum := 0;
     while (CsvRowGetColumnMarker(@CsvFieldRec,colnum) <> COLUMN_ENDMARKER) do begin
       FieldLen := 80; // default.
       CsvFieldOption := GetCsvRowItem( @CsvFieldRec, colnum ); // get a string in the format COLUMNAME:Options


       // Look for Colon or Semicolon:
       pos1 := Pos(':',CsvFieldOption);
       if (pos1<=0) then pos1 := Pos(';',CsvFieldOption);

       if (pos1<=0) then begin
            CsvFieldName := CsvFieldOption;
            CsvFieldOption := '$';
            FieldTypeChar := '$';
       end else begin
          // extract field name:
          CsvFieldName := Copy(CsvFieldOption,1,pos1-1);
          // If character after the colon is a symbol character, grab
          // it, otherwise default to '$'.
          if Ord( CsvFieldOption[pos1+1] ) < Ord('A') then begin
             FieldTypeChar := CsvFieldOption[pos1+1];
             CsvFieldOption := Copy(CsvFieldOption,pos1+2,80);
          end else begin
             FieldTypeChar := '$';
             CsvFieldOption := Copy(CsvFieldOption,pos1+1,80);
          end;
          FieldLen := StrToIntDef(CsvFieldOption,DEFAULT_CSV_STR_FIELD);
       end;
       case FieldTypeChar of
          '$': begin // $=string
                 VclFieldType := ftString;
                 FieldType := jcsvString;
               end;
          '%': begin // %=Integervalue
                 VclFieldType := ftInteger;
                 FieldType := jcsvNumeric;
                 FieldLen := 0; // automatic.
               end;
          '&': begin // &=Float value
                 VclFieldType := ftFloat;
                 FieldType := jcsvNumeric;
                 FieldLen := 0; // automatic.
               end;
          '@': begin // @=Datetime as Ascii YYYY/MM/DD HH:MM:SS
                 VclFieldType := ftDateTime;
                 FieldType := jcsvAsciiDateTime;
                 FieldLen := 0; // automatic.
               end;
          '!': begin // != boolean field TRUE/FALSE
                 VclFieldType := ftBoolean; // boolean field in dataset
                 FieldType := jcsvNumeric;// numeric field in file
                 FieldLen := 0; // automatic.
               end;
          '#': begin // #=Datetime as Seconds since 1970 stored in HEX
                 VclFieldType := ftDateTime;
                 FieldType := jcsvGMTDateTime;
                 FieldLen := 0; // automatic.
               end;

          '-': begin // -=Datetime as Seconds since 1970 stored in HEX
                 VclFieldType := ftDateTime;
                 FieldType := jcsvTZDateTime;
                 FieldLen := 0; // automatic.
               end;

          else
               JvCsvDatabaseError(FTableName,'Invalid field type character: '+FieldTypeChar );
       end;


       if Length(CsvFieldName)=0 then begin
           JvCsvDatabaseError(FTableName,'Unexpected error parsing CSV Field Definitions');
           break;
       end;

       // sometime later: unpack the rest of the string
       // and declare ftString,ftFloat,ftInteger,ftDateTime, etc.
       // now add the field:
      Inc(colnum);

       // This may throw an exception. but we'll just allow
       // that as necessary:

        //Was: TFieldDef.Create(FieldDefs, ...., colnum );
      FieldDefs.Add( CsvFieldName, VclFieldType, FieldLen, False );

      // Now create our internal field data structure:
      pCsvFieldDef := AllocMem(Sizeof(TJvCsvColumn) {+ 8 BIGFudge});
      pCsvFieldDef^.FFlag := FieldType; {jcsvString}
      pCsvFieldDef^.FFieldDef :=  FieldDefs.Find(CsvFieldName);

      // Note: field order is established when we open the file (later)
      pCsvFieldDef^.FPhysical := -1; // not yet located in the physical file!
      FCsvColumns.AddColumn(pCsvFieldDef);
     end;

     // if the file doesn't contain this and we haven't
     // generated it yet, generate the header row:
     if (NOT FHasHeaderRow) and ( length(FHeaderRow)=0 ) then
        FHeaderRow := GetColumnsAsString;



     if Length(FHeaderRow) > 0 then
       ProcessCsvHeaderRow(FHeaderRow);
  end else
    JvCsvDatabaseError(FTableName,'Field Definition Error. CsvFieldDef, FieldDefs, and file contents must match.');

    if Length(FCsvKeyDef)=0 then begin
            FCsvKeyCount:= 0;
    end else begin
            SetLength(CsvKeys, FCsvColumns.Count );
            FCsvKeyCount := StrSplit( FCsvKeyDef,',', {Chr(0)=No Quoting}Chr(0), CsvKeys, FCsvColumns.Count);
            SetLength(FCsvKeyFields,FCsvKeyCount);
            if (FCsvKeyCount < 1) or (FCsvKeyCount > FCsvColumns.Count) then
                JvCsvDatabaseError(FTableName,'Invalid CsvKeyDef property. InternalInitFieldDefs failed.');
            for t := 0 to FCsvKeyCount-1 do begin
                 if ( CsvKeys[t] = '' ) then
                       JvCsvDatabaseError(FTableName,'Internal Error parsing CsvKeyDef. InternalInitFieldDefs failed.');
                 pCsvFieldDef := FCsvColumns.FindByName( CsvKeys[t] );
                 if not Assigned(pCsvFieldDef) then begin
                     JvCsvDatabaseError(FTableName,'CsvKeyDef contains field '''+CsvKeys[t]+''' which is not defined. InternalInitFieldDefs failed.' );
                 end else begin
                     pCsvFieldDef^.FKeyFlag := true;
                     FCsvKeyFields[t] := pCsvFieldDef;
                 end;
            end;
    end;



end; { InternalInitFieldDefs ends }



{ set our position onto the EOF Crack }
procedure TJvCsvCustomInMemoryDataSet.InternalLast;
begin
//  Eof := true;
  FRecordPos := ON_EOF_CRACK; // FData.Count;
end;

// At shutdown or on user-calling this method, check if data has changed,
// and write changes to the file.
procedure TJvCsvCustomInMemoryDataSet.Flush;
begin
  if Length(FTableName)=0 then
      raise Exception.Create('TJvCsvCustomInMemorYDataSet.FTableName is not set.');

  if FFileDirty and FSavesChanges and (Length(FTableName)>0) then begin
    // Make backup first, if enabled (>2)
    if (FAutoBackupCount>0) then begin
        if (FAutoBackupCount<10) then FAutoBackupCount := 10;// can't be between 1 and 9, must be at least 10.
        JvCsvBackupPreviousFiles( FTableName, FAutoBackupCount );
    end;
    // Now write new file.
    ExportCsvFile( FTableName );
    FFileDirty := false;
  end;
end;


{procedure TJvCsvCustomInMemoryDataSet.DestroyFields;
begin
 inherited DestroyFields;
 // Clear out local TCsvFieldDefs.
 FCsvColumns.Clear;
end;}

procedure TJvCsvCustomInMemoryDataSet.InternalClose;
begin
  Flush;
  BindFields(False);
  if DefaultFields then DestroyFields;
  FData.Clear;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalHandleException;
begin
  // standard implementation for this method:
  Application.HandleException(Self);
end;

procedure TJvCsvCustomInMemoryDataSet.InternalDelete;
begin
  if (FRecordPos >= 0) and (FRecordPos < FData.Count) then begin
      // FreeMem performed inside DeleteRow!
      FData.DeleteRow(FRecordPos);
  end;
  
  if FRecordPos >= FData.Count then
      FRecordPos := FData.Count-1;

  FFileDirty := True;
end;

{ returns -1 if not found, else returns record index }
function TJvCsvCustomInMemoryDataSet.InternalFindByKey(row:PCsvRow):Integer;
var
 t:Integer;
begin
  result := -1;
  for t := 0 to FData.Count-1 do begin
     if InternalCompare( FCsvKeyFields,FCsvKeyCount, {Left}row, {Right}FData.Items[t] ) = 0 then begin
        result := t;
        break;
     end;
  end;
end;

function TJvCsvCustomInMemoryDataSet.FindByCsvKey( key:String):Boolean;
var
  logical_row,physical_row:TJvCsvRow;
  t,recno:Integer;
  str:String;
begin
  result := false;
  StringToCsvRow( key+',', @logical_row, false, false ); // initialize row and put items in their logical order.
  CsvRowInit(@physical_row);
  // Move from Logical (TFieldDef order) to their physical (As found in CSV file) ordering:
  for t := 0 to FCsvKeyCount-1 do begin
      str:= GetCsvRowItem( @logical_row, t);
      SetCsvRowItem( @physical_row, FCsvKeyFields[t].FPhysical, str );
      //if (t <> FCsvKeyFields[t].FPhysical) then
      //    OutputDebugString('FindByCsvKey debug');
  end;
  recno := InternalFindByKey(@physical_row);
  if (recno < 0) then
        exit;

  FRecordPos := recno;
  Resync([]);
  result := true;
end;

{procedure TJvCsvCustomInMemoryDataSet.InternalInsert;
//var
//  pAddRec : pCsvRow;
begin
// pAddRec := AllocMem(Sizeof(TJvCsvRow));
// StringToCsvRow( FEmptyRowStr, pAddRec, false ); // initialize row.
// FData.AddRow(pAddRec);
// FCurrentRecord := -1;
// Resync([]);
  FRecordPos :=  ON_EOF_CRACK;
//FCurrentRecord := FData.Count;
end;}

procedure TJvCsvCustomInMemoryDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos  : Integer;
  pAddRec : pCsvRow;
  keyIndex:Integer;
begin

 if FInsertBlocked then begin
    JvCsvDatabaseError(FTableName,'InternalAddRecord Can''t Add. Insert blocked.');
    exit;
 end;

 if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
 if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;



 pAddRec := AllocMem(Sizeof(TJvCsvRow));
 if (Buffer <> NIL) then
     Move( PCsvRow(Buffer)^, pAddRec^, Sizeof(TJvCsvRow) );

 if ( StrLen(pAddRec.text) = 0 ) then
  StringToCsvRow( FEmptyRowStr, pAddRec, false, false ); // initialize row.


 pAddRec.FDirty := true;
 pAddRec.index := -1; // Was not loaded from the file!

 {
 if FCsvUniqueKeys then begin
    keyIndex := InternalFindByKey(pAddRec);
    if keyIndex >= 0 then begin
          JvCsvDatabaseError('Key value is not unique. Adding new record failed.');
          exit; // never get here, since normally JvCsvDatabaseError raises an exception.
    end;
 end;
  }
 FData.EnquoteBackslash := FEnquoteBackslash; // make sure FData is in the right mode.
 
 FFileDirty := True;
 if Append then begin //this is the parameter not a TDataSet method invocation!
     pAddRec^.index := FData.Count;
     FData.AddRow(pAddRec);
     InternalLast;
 end else begin
     if (FRecordPos = ON_EOF_CRACK) or (FRecordPos = ON_BOF_CRACK ) then begin
        pAddRec^.index := FData.Count;
        FData.AddRow(pAddRec);
        InternalLast;
     end else begin
        RecPos := FRecordPos;
        pAddRec^.index := RecPos;
        FData.Insert(RecPos,Pointer(pAddRec));
        // XXX Renumber everything else.

    end;
 end;
end;


{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This creates that TString List. }
function TJvCsvCustomInMemoryDataSet.InternalLoadFileStrings:Boolean;
begin
  result := false;
  if not FileExists(FTableName) then exit;
  if not FLoadsFromFile then exit;
  if Assigned(FCsvFileAsStrings) then begin
        if FCsvFileAsStrings.Count >0 then
                result := true; //loaded already
        exit; // don't repeat!
  end;

   try   // open data file
          FCsvFileAsStrings:= TStringList.Create;
          FCsvFileAsStrings.LoadFromFile(FTableName);
          if FCsvFileAsStrings.Count >0 then
                  result := true; // it worked!
   except
          //FTableName := '';
          FCsvFileAsStrings.Free;
          FCsvFileAsStrings := nil;
          raise; // pass exception on in.
    end; {end except}
end;


{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This cleans up that TString List. }
procedure TJvCsvCustomInMemoryDataSet.InternalClearFileStrings;
begin
  if Assigned(FCsvFileAsStrings) then begin
          FCsvFileAsStrings.Free;
          FCsvFileAsStrings := nil;
  end;
end;


procedure TJvCsvCustomInMemoryDataSet.InternalOpen;
var
  Strings : TStringList;
  TempBuf : Array[0..MAXCOLUMNS] of char;
begin
  if FCursorOpen then InternalClose; // close first!



  FFileDirty := False;
  if ( Length(FTableName) = 0 ) and FLoadsFromFile then
     JvCsvDatabaseError('noTableName','LoadFromFile=True, so a TableName is required');
  Strings:=NIL;



  InternalInitFieldDefs;             // initialize FieldDef objects



  // Create TField components when no persistent fields have been created
  if DefaultFields then
      CreateFields;
  BindFields(True);                 // bind FieldDefs to actual data



  if (FCsvColumns.Count>1) then begin
     // Create a null terminated string which is just a bunch of commas:
      FillChar(TempBuf, FCsvColumns.Count - 1, ',');
      TempBuf[FCsvColumns.Count-1] := Chr(0);
      // When adding an empty row, we add this string as the ascii equivalent:
      FEmptyRowStr := String(TempBuf);
  end else begin
      FEmptyRowStr := ''; // nothing.
  end;




  FBufferSize := SizeOf(TJvCsvRow) + CalcFieldsSize; // our regular record + calculated field data.
  FRecordPos := ON_BOF_CRACK;                  // initial record pos before BOF
  BookmarkSize := SizeOf(Integer);   // initialize bookmark size for VCL (Integer uses 4 bytes on 32 bit operating systems)

  //Trace( 'InternalOpen: FBufferSize='+IntToStr(FBufferSize) );
  //Trace( 'InternalOpen: CalcFieldsSize='+IntToStr(CalcFieldsSize) );
  //Trace( 'InternalOpen: FieldDefs.Count='+IntToStr(FieldDefs.Count) );

  if InternalLoadFileStrings then  // may load the strings if they weren't loaded already!
          AssignFromStrings(FCsvFileAsStrings); // load into memory.


  InternalClearFileStrings; // now unload 'em.

   FCursorOpen := true;
   if Length(FHeaderRow) >0 then
              ProcessCsvHeaderRow(FHeaderRow);

end;

procedure TJvCsvCustomInMemoryDataSet.InternalPost;
var
 pInsertRec:PCsvRow;
 RecPos:Integer;
 keyIndex:Integer; // If unique key enforcement is on, this is the key search result.
begin
  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;


  if FPostBlocked then begin
          JvCsvDatabaseError(FTableName,'Posting to this database has been blocked.');
          exit;
  end;

  { Unique Key Enforcement }
 if FCsvUniqueKeys then begin
    keyIndex := InternalFindByKey( PCsvRow(ActiveBuffer) );
    // If posting an update, keyIndex better be <0 or else equal to FRecordPos!
    // Otherwise, if adding, keyIndex better be <0.
    if keyIndex >= 0 then
       if (state = dsInsert) or (( state = dsEdit ) and ( keyIndex <> FRecordPos)) then begin
          raise EJvCsvKeyError.Create( FTableName +' - Key is not unique ');
          exit; // never get here, since normally JvCsvDatabaseError raises an exception.
       end;
 end;

  

 if State = dsEdit then begin
     FFileDirty := True;
     RecPos := FRecordPos;
     Move( PCsvRow(ActiveBuffer)^, FData.GetRowPtr(RecPos)^, Sizeof(TJvCsvRow) );
     FData.GetRowPtr(RecPos)^.FDirty := true;
 end else if State = dsInsert then begin
     if FInsertBlocked then begin
          JvCsvDatabaseError(FTableName,'Can''t insert new row. Insert blocked.');
          exit;
     end;
     FFileDirty := True;
     pInsertRec := AllocMem(Sizeof(TJvCsvRow));
     Move( PCsvRow(ActiveBuffer)^, pInsertRec^, Sizeof(TJvCsvRow));
     pInsertRec^.FDirty := true;
     FData.Insert(FRecordPos,Pointer(pInsertRec));
     FRecordPos := FData.IndexOf(Pointer(pInsertRec));
     pInsertRec^.bookmark.data := FRecordPos;
 end else
     JvCsvDatabaseError(FTableName,'Post: Can''t post. Not in not dsEdit or dsInsert mode');
end;


function TJvCsvCustomInMemoryDataSet.IsCursorOpen: Boolean;
begin
  // "Cursor" is open if data file is open.   File is open if FDataFile's
  // Mode includes the FileMode in which the file was open.
 {  Result := TFileRec(FDataFile).Mode <> 0; }
  Result := FCursorOpen; // bogus value: Valid field definition
end;

function TJvCsvCustomInMemoryDataSet.GetRecordCount: Integer;
begin
 if ( FData.Count > 0) then
  Result := FData.Count
 else
  Result := 0;
end;

function TJvCsvCustomInMemoryDataSet.GetRecNo: Integer; {RecNo := FRecordPos+1}
var
  BufPtr: PChar;
  
begin
 CheckActive;

 //  UpdateCursorPos; {FUDGE!?}

{
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 0 // 1  //FUDGE!?
  else
    Result := FRecordPos + 1;
 }

  if State = dsCalcFields then
    BufPtr := CalcBuffer else
    BufPtr := ActiveBuffer;

  Result := PCsvRow(BufPtr)^.bookmark.data; // Record number.

end;

procedure TJvCsvCustomInMemoryDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= FData.Count-1) then
  begin
    FRecordPos := Value - 1;
    if (RecordCount > 0) then Resync([]);
  end;
end;

procedure TJvCsvCustomInMemoryDataSet.SetTableName(const Value: string);
begin
  CheckInactive;
  FTableName := Value;
  if ExtractFileExt(FTableName) = '' then
    FTableName := FTableName + '.csv';

  { update internal filename table }
//  FBmkFileName:= ChangeFileExt(FTableName, '.bmk' ); // bookmark file
end;

(*function TJvCsvCustomInMemoryDataSet.GetDataFileSize: Integer;
//var
//  File1:TextFile;
begin
//  AssignFile(File1,FTableName);
//  Result := FileSize(File1);
//  CloseFile(File1);
  result := 8192; // not implemented yet.
end; *)

procedure TJvCsvCustomInMemoryDataSet.EmptyTable;
begin
   // Erase Rows.
   while ( FData.Count >0) do
       FData.DeleteRow(FData.Count-1);
   // Refresh controls.
   First;
end;



// InternalCompare of two records, of a specific field index. 
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right
function TJvCsvCustomInMemoryDataSet.InternalFieldCompare( Column:PCsvColumn; Left,Right:PCsvRow ):Integer;
var
  strLeft,strRight:String;
  numLeft,numRight,diff:Double;
begin

  strLeft  :=GetCsvRowItem( Left, Column^.FPhysical );
  strRight  :=GetCsvRowItem( Right, Column^.FPhysical );

  (*if (Length(strLeft)=0) or (length(strRight)=0) then begin
      OutputDebugString('Debugging problem in InternalFieldCompare');
      strLeft  :=GetCsvRowItem( Left, Column^.FPhysical );
  end;*)
  
  if (FCsvCaseInsensitiveComparison) then begin
    strLeft  := UpperCase(strLeft);
    strRight  :=Uppercase(strRight);
  end;
    
   // everything sorts via string sort (default) or numeric sort
   // (the only special case so far!)
  case Column^.FFlag of
      jcsvNumeric:
              begin
                numLeft := StrToFloatDef(strLeft,-99999.9);
                numRight := StrToFloatDef(strRight,-99999.9);
                diff := numLeft-numRight;
                if (diff < -0.02) then
                        result := -1
                else if (diff > 0.02)  then
                        result := 1
                else
                        result := 0; // For our purposes, .02 difference or less is a match.
                exit;
              end;
      else
          result := StrComp(PChar(strLeft),PChar(strRight));
  end;
end;

// InternalCompare of multiple fields.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right
function TJvCsvCustomInMemoryDataSet.InternalCompare( SortColumns:Array of PCsvColumn; SortColumnCount:Integer; Left,Right:PCsvRow ):Integer;
var
  t:Integer;
begin
  // null check, raise exception
  if (not Assigned(Left)) or (not Assigned(Right)) then begin
        JvCsvDatabaseError(FTableName,'InternalCompare. Nil value detected.');
  end;
  // now check each field:
  for t := 0 to SortColumnCount-1 do begin
        if not Assigned(SortColumns[t]) then
                JvCsvDatabaseError(FTableName,'InternalCompare. Nil value detected.'); // raise exception
        result := InternalFieldCompare( SortColumns[t],Left,Right);
        if (result <> 0) then begin
           // XXX REPEAT result := InternalFieldCompare( SortColumns[t],Left,Right);
           exit; // found greater or less than condition
        end;
  end;
  // now we have compared all fields, and if we get here, they were all
  // equal, and result is already set to 0.
end;

procedure TJvCsvCustomInMemoryDataSet.Sort( SortFields:String; Ascending:Boolean);
var
  Index:Array of Pointer;
  swap:Pointer;
  SortFieldNames:Array of String;
  SortColumns:Array of PCsvColumn;
  SortColumnCount:Integer;
  comparison,t,u,l:Integer;
begin
  // Create an indexed list which can be sorted more easily than
  // doing an item swap:
  l := FData.Count;
  SetLength(Index,  l);
  for t := 0 to l-1 do begin
     Index[t] :=  FData.Items[t]; // Initial values.
  end;

  SetLength(SortFieldNames, FCsvColumns.Count);
  SortColumnCount := StrSplit(SortFields,',',{Chr(0)=No Quoting}Chr(0),SortFieldNames, FCsvColumns.Count);
  SetLength(SortColumns,SortColumnCount);
  if (SortFields = '') or (SortColumnCount = 0) then
        JvCsvDatabaseError(FTableName,'Sort failed. You must give a comma separated list of field names.');
        
  // Now check if the fields exist, and find the pointers to the fields
  for t := 0 to SortColumnCount - 1 do begin
      if ( SortFieldNames[t] = '' ) then
         JvCsvDatabaseError(FTableName,'Sort failed. Unable to parse field names. ');
      SortColumns[t] := FCsvColumns.FindByName( SortFieldNames[t] );
      if not Assigned(SortColumns[t]) then
           JvCsvDatabaseError(FTableName,'Sort failed. Invalid field name in list: '+SortFieldNames[t]);
  end;        

  //  bubble sort, compare in the middle,
  //  yes I'm feeling lazy today, yes I know a qsort would be better. - WP
  for t := 0 to l-2 do begin
    for u := t+1 to l-1 do begin
        // Record comparison between two PCsvRows: 
        comparison := InternalCompare( SortColumns,
                                       SortColumnCount,
                                       PCsvRow(Index[t]),
                                       PCsvRow(Index[u])
                                     );
        if not Ascending then
                comparison := comparison * -1; // flip sign of comparison
        if (comparison > 0) then begin { bubble sort by swaps }
                  swap := Index[t];
                  Index[t] := Index[u];
                  Index[u] := swap;
        end;
    end;
  end;
  // Now build a new Data elements list:
  for t := 0 to l-1 do begin
     FData.Items[t] := Index[t]; // Rewrite pointers to new order!
  end;
  FFileDirty := true;
  First; // reposition!
end;


{ Support Delphi VCL TDataSetDesigner's field persistence }
procedure TJvCsvCustomInMemoryDataSet.DefChanged(Sender: TObject); //override;
begin
  FStoreDefs := true;
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }
function TJvCsvCustomInMemoryDataSet.FieldDefsStored: Boolean;
begin
  Result := FStoreDefs and (FieldDefs.Count > 0);
end;

function TJvCsvCustomInMemoryDataSet.GetCanModify: Boolean; //override;
begin
 result := NOT FReadOnly; // You can modify if it's NOT read only.
end;




{ CsvColumns dynamic array of pointers }

procedure TJvCsvColumns.AddColumn(Item:PCsvColumn);
begin
   Add(Pointer(Item));
end;


function  TJvCsvColumns.FindByName(FieldName:String):PCsvColumn;
var
 t:Integer;
begin
  try
   for t := 0 to Count-1 do begin
      result := PCsvColumn(Get(t));
      if Assigned(Result.FFieldDef) then
        // Case insensitive field name matching:
        if CompareText(Result.FFieldDef.Name,FieldName)=0 then
            exit; //return that field was found! 
   end;
  except
   // ignore exceptions
  end;
  result := NIL;
end;

function  TJvCsvColumns.FindByFieldNo(FieldNo:Integer):PCsvColumn;
var
 t:Integer;
begin
  result := NIL;
  try
   for t := 0 to Count-1 do begin
      result := PCsvColumn(Get(t));
      if Assigned(Result) then
        if Assigned(Result^.FFieldDef) then
          if Result^.FFieldDef.FieldNo = FieldNo then
            exit; //return that field was found!
   end;
  except
   // ignore exceptions
   result := NIL;
  end;
end;


procedure TJvCsvColumns.Clear;
var
 t:Integer;
begin
 for t := 0 to Count-1 do
        FreeMem(Self[t]);
 inherited;
end;


{ CsvRows: dynamic array of pointers }

function TJvCsvRows.GetUserTag(index:Integer):Integer;
begin
  if (index<0) or (index>=FUserLength) then
     result := 0
   else
     result := FUserTag[index];
end;



procedure TJvCsvRows.SetUserTag(index,value:Integer);
begin
 if (index<0) or (index >= Count) then exit;
 
 if (index>=FUserLength) then begin
    FUserLength := index+1;
    SetLength(FUserTag,FUserLength);
    SetLength(FUserData,FUserLength);
 end;
 FUserTag[index] := value;
end;


function TJvCsvRows.GetUserData(index:Integer):Pointer;
begin
   if (index<0) or (index>=FUserLength) then
     result := nil
   else
    result := FUserData[index];
end;

procedure TJvCsvRows.SetUserData(index:Integer;value:Pointer);
begin
 if (index<0) or (index >= Count) then exit;
 if (index>=FUserLength) then begin
    FUserLength := index+1;
    SetLength(FUserTag,FUserLength);
    SetLength(FUserData,FUserLength);
 end;
 FUserData[index] := value;
end;


procedure TJvCsvRows.AddRow(const Item:PCsvRow);
begin
 Add(Pointer(Item));
end;

procedure TJvCsvRows.InsertRow  (const position:Integer; const Item:PCsvRow);
begin
  Insert(position,Pointer(Item));
end;


procedure TJvCsvRows.AddRowStr(const Item:String); // convert String->TJvCsvRow
var
  pNewItem : PCsvRow;
begin
  pNewItem := AllocMem(Sizeof(TJvCsvRow));
  StringToCsvRow(Item, pNewItem, true, FEnquoteBackslash); // decode a csv line that can contain escape sequences
  AddRow(pNewItem);
end;

function TJvCsvRows.GetRowPtr(const RowIndex:Integer):PCsvRow;
begin
  result := PCsvRow(Get(RowIndex)); // return pointer to a row item.
end;

function  TJvCsvRows.GetRowStr(const RowIndex:Integer):String;
var
 ResultStr:String;
begin
  CsvRowToString( GetRowPtr(RowIndex), ResultStr );
  result := ResultStr;
end;

procedure TJvCsvRows.SetRowStr(const RowIndex:Integer;Value:String);
begin
  StringToCsvRow(Value,GetRowPtr(RowIndex), true, FEnquoteBackslash );
end;

procedure TJvCsvRows.DeleteRow(const RowIndex:Integer);
var
  p:Pointer;
begin
 if (RowIndex>=0) and (RowIndex < Count) then begin
     p := Self[RowIndex];
     if p <> NIL then FreeMem(p);
 end;
 Delete(RowIndex);
end;

procedure TJvCsvRows.SetARowItem(const RowIndex,ColumnIndex:Integer; Value:String);
begin
  SetCsvRowItem(GetRowPtr(RowIndex),ColumnIndex,Value);
end;

function  TJvCsvRows.GetARowItem(const RowIndex,ColumnIndex:Integer):String;
begin
  result := GetCsvRowItem(GetRowPtr(RowIndex),ColumnIndex);
end;


procedure TJvCsvRows.Clear;
var
    t:Integer;
begin
  for t := 0 to Count-1 do
      FreeMem(Self[t]);
  inherited;
end;



{ Call this one first, then AssignFromStrings on subsequent updates only.}
procedure TJvCsvCustomInMemoryDataSet.OpenWith(Strings:TStrings);
begin
  Active := false;
  AssignFromStrings(Strings); // parse strings
end;

{ Call this one first, then AssignFromStrings on subsequent updates only.}
procedure TJvCsvCustomInMemoryDataSet.AppendWith(Strings:TStrings);
//var
//x:Integer;
begin

  //Active := false;
  AssignFromStrings(Strings); // parse strings
  // Refresh.
//  DataEvent(deDataSetChange, 0);
//  DataEvent(deRecordChange, 0);
  Resync([]);
//  DataEvent(deUpdateState, 0);
 if Active then
  Last;

end;


{ Additional Custom Methods - internal use }
procedure TJvCsvCustomInMemoryDataSet.AssignFromStrings(const Strings:TStrings);
var
 HeaderRowFound:Boolean;
 t:Integer;
 indexCounter:Integer;
begin
// CheckInactive;
// if NOT FFieldsInitialized then
// InternalInitFieldDefs; // must know about field definitions first.
 if Strings = NIL then exit;
 FData.EnquoteBackslash := FEnquoteBackslash;

 indexCounter := 0;
 HeaderRowFound := false;
 for t := 0 to Strings.Count-1 do begin
       // for now ignore any trace or debug data unless
       // someone has defined an event to handle it.
       if Pos('>>',Strings[t]) = 1 then begin
           if Assigned(FOnSpecialData) then
              FOnSpecialData(Self,t,Strings[t]);
           continue;
       end;

       // Process the row:
       if (NOT HeaderRowFound) and FHasHeaderRow then begin
         HeaderRowFound := true;
         FHeaderRow := Strings[t];
         //Note: later we will call ProcessCsvHeaderRow(FHeaderRow);
       end else begin
         ProcessCsvDataRow(Strings[t],indexCounter);
         Inc(indexCounter);
       end;
 end;
 if Active then First;
end;

procedure TJvCsvCustomInMemoryDataSet.AssignToStrings(Strings:TStrings);
var
 t:Integer;
 Line:String;
begin
  // copy out the current data set to a TStringList.
  Strings.Clear;

  { Save header row with data rows? }
  if FHasHeaderRow then
    Strings.Add(GetColumnsAsString);

  for t := 0 to FData.Count-1 do begin
      CsvRowToString(FData.GetRowPtr(t),Line);
      Strings.Add(Line);
  end;
end;

function TJvCsvCustomInMemoryDataSet.GetRowAsString(const Index:Integer):String;
var
 ResultString:String;
begin
 CsvRowToString( FData.GetRowPtr(Index), ResultString );
 result := ResultString;
end;


// Get names of all the columns as a comma-separated string:
function TJvCsvCustomInMemoryDataSet.GetColumnsAsString:String;
var
 ResultString:String;
 t:Integer;
begin
 // ColCount:
 if FCsvColumns.count = 0 then begin
     ResultString := '';
     result := ResultString;
     exit;
 end;
 // Build a list of column names: <item>, <item>,....,<item>
 ResultString := FieldDefs[0].Name;
 for t := 1 to FCsvColumns.count-1 do
     ResultString := ResultString + ',' + FieldDefs[t].Name;
 result := ResultString;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }
procedure TJvCsvCustomInMemoryDataSet.ProcessCsvHeaderRow(const header:String);
var
  CsvFieldRec : TJvCsvRow; // CSV Field record type.
  ptrCsvColumn  : PCsvColumn;
{  CsvFieldOption:String;}
  CsvFieldName:String;
{  pCsvFieldDef:PCsvColumn;}
  colnum, t:Integer;
begin
  if Length(header) = 0 then
     exit;

 // Initialize all CSV Column locations to a "not found yet" state:
 for t := 0 to FCsvColumns.Count-1 do
     PCsvColumn(FCsvColumns.Get(t))^.FPhysical := -1;

 StringToCsvRow(header,@CsvFieldRec, false, false);
 colnum := 0;
 while (CsvRowGetColumnMarker(@CsvFieldRec,colnum) <> COLUMN_ENDMARKER) do begin
   // Get a string in the format COLUMNAME:Options
   CsvFieldName := StrEatWhiteSpace(GetCsvRowItem( @CsvFieldRec, colnum ));

   if (Length(CsvFieldName)=0) then
      JvCsvDatabaseError(FTableName,'Error processing first line of CSV file.');

   ptrCsvColumn := FCsvColumns.FindByName(CsvFieldName);

   if (ptrCsvColumn=NIL) then begin // raise database exception:
     JvCsvDatabaseError(FTableName,'ProcessCsvHeaderRow:Field '+CsvFieldName+' found in file, but not in field definitions.');
     exit;
   end;
   
   try
     ptrCsvColumn^.FPhysical := colnum; // numbered from 0.
   except
     JvCsvDatabaseError(FTableName,'Csv field location error: '+CsvFieldName );
     break;
   end;
   inc(Colnum);
 end;

  // Check that everything was found and physically given a location
  // in the CSV file:

 for t := 0 to FCsvColumns.Count-1 do begin
    ptrCsvColumn := PCsvColumn(FCsvColumns[t]);
    if ptrCsvColumn^.FPhysical < 0 then begin
       JvCsvDatabaseError(FTableName,'Field '+ptrCsvColumn^.FFieldDef.Name+' not found in the data file.');
       exit;
    end;
 end;
end;

procedure TJvCsvCustomInMemoryDataSet.ProcessCsvDataRow(const datarow:String;index:integer);
var
 pNewRow:PCsvRow;
begin
  if (Length(DataRow)=0) then
          exit;
  if (Length(DataRow)>=(MAXLINELENGTH-1)) then begin
        raise Exception.Create('CSV String is too long: '+Copy(datarow,1,40)+'...' );
  end;
  pNewRow := AllocMem(sizeof(TJvCsvRow));
  StringToCsvRow(datarow,pNewRow, true, FEnquoteBackslash);
  pNewRow^.index := index;
  FData.AddRow(pNewRow);
end;


{ This function is handy to save a portion of a csv table that has
grown too large into a file, and then DeleteRows can be called to remove
that section of the file. }
procedure TJvCsvCustomInMemoryDataSet.ExportRows( Filename :String; FromRow, ToRow :Integer);
var
  t:Integer;
  StrList:TStringList;
begin
  StrList := TStringList.Create;
  StrList.Add( Self.FHeaderRow);
  try
    for t := FromRow to ToRow do begin
        StrList.Add( FData.GetRowStr(t) )
    end;
    StrList.SaveToFile(Filename);
  finally
    StrList.Free;
  end;
end;

procedure TJvCsvCustomInMemoryDataSet.DeleteRows( FromRow, ToRow :Integer);
var
  Count:Integer;
begin
  Count := (ToRow-FromRow)+1;
  while (Count > 0) do begin
        if (FromRow < FData.Count) then
            FData.DeleteRow(FromRow) // Everything moves down one every time we do this.
        else
            break;
        Dec(Count);
  end;
  // Force Redraw of Data Controls:
  if FRecordPos >= FData.Count then begin 
      FRecordPos := FData.Count-1;
      Last;
  end else
      First;
  FFileDirty := True;
end;

procedure TJvCsvCustomInMemoryDataSet.ExportCsvFile(const Filename:String); // save out to a file.
var
 Strings:TStringList;
begin
 Strings := TStringList.Create;
 AssignToStrings(Strings);
 Strings.SaveToFile(Filename);
 Strings.Free;
end;

function TJvCsvCustomInMemoryDataSet.GetCsvHeader:String;
var
  f:Text;
  FirstLine:String;
begin
  if (not FLoadsFromFile) or (not FHasHeaderRow) or not (FileExists(FTableName)) then begin
        result := '';
        exit;
  end;
  { How's this for an ancient Pascal code sequence, AssignFile+Reset is approximately equal to a C fopen() call }
  AssignFile(F, FTableName);
  Reset(F);
  { ReadLn is approximately a gets() call }
  ReadLn(F,FirstLine);
  { And finally, the pascal file close procedure }
  CloseFile(F);
  // return the first line of the file, without the junk
  result := StrStrip(FirstLine); // in JvCsvParse.pas
end;



{ PROCEDURES: }

// convert CSV Row buffer to a String
procedure CsvRowToString( RowItem:PCsvRow ; var RowString:String );
begin
 RowString := String(RowItem.Text);
end;

// convert String into a CSV Row buffer
procedure StringToCsvRow( const RowString:String   ; RowItem:PCsvRow ; permitEscapeSequences,enquoteBackslash:Boolean );
var
  t,l,col:Integer;
  quoteFlag:Boolean;
  skipFlag:Boolean;
  charsInColumn:Integer;
begin
 col := 0;
 RowItem^.wordfield[0] := 0; // zero out column marker and dirty bit!
 charsInColumn := 0;
 quoteFlag := false;
 skipFlag := false;
 l := Length(RowString); 
 for t := 1 to l do begin
    Inc(charsInColumn);
    if quoteFlag then begin
          // backslash permitted only if specifically enabled by FEnquoteBackslash:
          if permitEscapeSequences and (not skipFlag) and (enquoteBackslash) and (RowString[t] = '\') then begin
              skipFlag := true;
              continue;
          end;
          // doubled quotes handling:
          if permitEscapeSequences and (not skipFlag) and (RowString[t] = '"') and (t<l) and (RowString[t+1] = '"') then begin
              skipFlag := true;
              continue;
          end;
          // now if either of the above set the skipFlag true previously, we ALWAYS skip the next character here
          // and turn skipFlag back off
          if permitEscapeSequences and skipFlag then begin // skip next character, regardless.
              skipFlag := false;
              continue; // by skipping escaped quotes, we don't turn off quoteFlag in the middle of a string!
          end;
    end;
    //Now we know if we get this far, we are NOT dealing with any escaped characters
    //Any quotes we see here will turn on/off quote mode directly!
    if RowString[t] = '"' then begin
       if permitEscapeSequences then begin
        quoteFlag := not quoteFlag;
        if quoteFlag and (charsInColumn >1) then begin
             OutputDebugString('CsvDataSource.pas: StringToCsvRow - unescaped quote character in middle of string!');
        end;
             
       end else begin
              OutputDebugString('CsvDataSource.pas: StringToCsvRow - quote character found where no escape sequences are permitted!');
       end;
    end;

    if ((RowString[t] = ',') and (not quoteFlag))  then begin
       Inc(col);
       // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
       RowItem.wordfield[col] := (Word(t) AND $7FFF); {note that we're going from 1..length }
       charsInColumn := 0;
    end;
    if (col >= MAXCOLUMNS) OR (t >= MAXLINELENGTH) then begin
       raise ERangeError.Create('JvCsvData - Internal Limit of MAXCOLUMNS ('+IntToStr(MAXCOLUMNS)+') reached. CSV Data has too many columns');
      exit;
    end;
 end;    // end of string, new flag:
 Inc(col);
 if quoteFlag then begin
        OutputDebugString('CsvDataSource.pas: StringToCsvRow - Missing end quote character!');
 end;
 // Terminate the column-marker list with a special end-marker:
{RowItem.wordfield[col]   := Word(Length(RowString)+1)AND$7FFF; // length of string
 RowItem.wordfield[col+1] := COLUMN_ENDMARKER; // followed by an end marker}
 RowItem.wordfield[col] := COLUMN_ENDMARKER; // last one has no end marker 
 StrLCopy(RowItem.text, PChar(RowString), MAXLINELENGTH);
 RowItem.columns := col; // Check this later!
end;

// Copy a single column from one row buffer to another row buffer:
function CsvRowItemCopy( Source,Dest:PCsvRow; FieldIndex,FieldSize:Integer ):Boolean;
var
 TempStr:String;
begin
   TempStr := GetCsvRowItem(Source,FieldIndex);
   // length limiting feature:
   if (FieldSize>0) then
       if Length(TempStr)>FieldSize then
          TempStr := Copy(TempStr,1,FieldSize);
   SetCsvRowItem(Dest,FieldIndex,TempStr);
   result := true;
end;


// Copy an item into a csv row buffer:
procedure SetCsvRowItem( pItem:PCsvRow; ColumnIndex:Integer; NewValue:String);
var
  TempBuf:Array[0..MAXLINELENGTH] of Char;
  Copy1,Copy2:Integer;
  Dif,t,Old:Integer;
begin
 Dif := 0;
 if (ColumnIndex<0) OR (ColumnIndex>MAXCOLUMNS) then
    exit;
 Copy1 := CsvRowGetColumnMarker(pItem,ColumnIndex);
 if (Copy1 = COLUMN_ENDMARKER) then
              exit;

 if (Copy1 > MAXLINELENGTH) then
              exit;
 // copy initial part of the csv row:
 if (Copy1 > 0 ) then begin
   StrLCopy( TempBuf, pItem.text, Copy1 );
   StrLCat( TempBuf, PChar(NewValue), MAXLINELENGTH );
 end else
   StrLCopy( Tempbuf, PChar(NewValue), MAXLINELENGTH );

 Copy2 := CsvRowGetColumnMarker(pItem,ColumnIndex+1);
 if (Copy2 <> COLUMN_ENDMARKER) then begin
   // difference in length:
   Dec(Copy2); // subtract one.
   if (Copy2 < 0) then exit;
   if (Length(NewValue) = Copy2-Copy1) then
      Dif := 0
   else
      Dif := Length(NewValue) - (Copy2-Copy1);
   StrLCat( TempBuf, pItem^.text+Copy2,  MAXLINELENGTH );
 end;

 // Copy over the old memory buffer:
 StrLCopy( pItem^.text,  TempBuf, MAXLINELENGTH );

 // Now that we've copied a new item of a different length into the place of the old one
 // we have to update the positions of the columns after ColumnIndex:
 if (Dif <> 0) then
   for t := ColumnIndex+1 to MAXCOLUMNS do begin
     Old := CsvRowGetColumnMarker(pItem,t); 
     if (Old=COLUMN_ENDMARKER) then exit;
     CsvRowSetColumnMarker(pItem,t,Old+Dif );
   end;

end;

// Copy an item out of a csv row buffer:
function GetCsvRowItem( pItem:PCsvRow; ColumnIndex:Integer):String;
var
  TempBuf:Array[0..MAXLINELENGTH] of Char;
  Copy1,Copy2:Integer;
begin
 if (ColumnIndex<0) OR (ColumnIndex>MAXCOLUMNS) then begin
    result := '<ERROR>';
    exit;
 end;

 Copy1 := CsvRowGetColumnMarker(pItem,ColumnIndex);
 Copy2 := CsvRowGetColumnMarker(pItem,ColumnIndex+1);
 if (Copy1 = COLUMN_ENDMARKER) then begin
              result := '';
              exit;
 end;
 if (Copy2 = COLUMN_ENDMARKER) then // copy the rest of the line
    Copy2 := MAXLINELENGTH-Copy1 // All the characters left in the buffer
 else
    Dec(Copy2);
    
 if (Copy1 > MAXLINELENGTH) or (Copy2 > MAXLINELENGTH) then begin
     result := '';
     exit;
 end;

 // Copy out just one column from the string:
 StrLCopy( TempBuf, pItem.Text+Copy1, Copy2-Copy1 );
 PcharEatWs(@TempBuf[0]);
 result := String(TempBuf);
end;

{new}
  procedure CsvRowSetDirtyBit(row:pCsvRow;ColumnIndex:Integer);
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    row^.fdirty := true; // triggers search for 'dirty bit' in columns
    row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] OR $8000);
  end;

  procedure CsvRowClearDirtyBit(row:pCsvRow;ColumnIndex:Integer);
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] AND $7FFF);
  end;

  function CsvRowGetDirtyBit(row:pCsvRow;ColumnIndex:Integer):Boolean;
  begin
    result := false;
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    if row^.wordfield[ColumnIndex] = COLUMN_ENDMARKER then exit;
    result := (row^.wordfield[ColumnIndex] AND $8000) <> 0;
  end;

  procedure CsvRowSetColumnMarker(row:pCsvRow;ColumnIndex:Integer;ColumnMarker:Integer);
  var
   Old:Word;
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    if (ColumnMarker<0) then exit;

    if ColumnMarker = COLUMN_ENDMARKER then
        row^.wordfield[ColumnIndex] := COLUMN_ENDMARKER
    else begin
        Old := row^.wordfield[ColumnIndex];
        if Old = COLUMN_ENDMARKER then
            row^.wordfield[ColumnIndex] := ColumnMarker AND $7FFF // auto-clear Dirty bit
        else
            row^.wordfield[ColumnIndex] := ( Old AND $8000 ) // Keep Old Dirty Bit
                   OR ( Word( ColumnMarker ) AND $7FFF ); // new value.
    end;
  end;

  function CsvRowGetColumnMarker(row:pCsvRow;ColumnIndex:Integer):Integer;
  var
    w:Word;
  begin
    result := -1;
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    w := row^.wordfield[ColumnIndex];
    if  w = COLUMN_ENDMARKER then
      result := COLUMN_ENDMARKER
    else
      result := Integer( w and $7FFF );
  end;
{endnew}


 //------------------------------------------------------------------------------
 // TimeTHexToDateTime
 //
 // TDateTime is a whole number representing days since Dec 30, 1899.
 // A standard C library time is seconds since 1970. We compensate by
 // getting the base date (Jan 1, 1970) and adding 1 for every day since
 // then, and a fractional part representing the seconds.
 // By dividing the seconds by the number of seconds in a day (24*24*60=86400)
 // we obtain this result.
 // The incoming value in hex will roll over in mid-Janary 2038, and
 // hopefully by then this code won't be in use any more! :-)
 //
 // Note: TDateTime is really a Double floating-point and zero is considered
 // an Error code.
 //------------------------------------------------------------------------------
function TimeTHexToDateTime( HexStr:String ; TimeZoneCorrection:Integer ) : TDateTime;
var
 SecondsSince1970 : Double;
 Base:TDateTime;
{ DateTimeAsStr:String; //debug Code.}
begin
 result := 0.0;
 SecondsSince1970 := StrToIntDef('$' + HexStr,0)+TimeZoneCorrection;
 if (SecondsSince1970 <= 0.0) then exit;
 Base := EncodeDate(1970, 1, 1);
 Base := Base + (SecondsSince1970 / 86400.0);
{ DateTimeAsStr := FormatDateTime('yyyy/mm/dd hh:nn:ss',Base);}
 Inc(CallCount);
 result := Base;
end;


function TimeTAsciiToDateTime( AsciiDateStr:String ):TDateTime;
const
 Separators =  '// ::';  // separators in yyyy/mm/dd hh:mm:ss
 Separators2 =  '-- --';  // separators in yyyy/mm/dd hh:mm:ss
var
 Values       : Array [1..6] of Integer; //year,month,day,hour,minute,second in that order.
 ExpectLengths:Array [1..6] of Integer;
 MinValue:Array [1..6] of Integer;
 MaxValue:Array [1..6] of Integer;
 ch           : Char;
 t,u,len,index: Integer;
 Done:Boolean;
begin
 result := 0.0; // default result.
 len := Length(AsciiDateStr);
 Done := false;

 // validate ranges:
 MinValue[1] := 1990;
 MaxValue[1] := 2999; // This code suffers from the Y3K bug. If you care, get a life.

 MinValue[2] := 1;  // Hope they never add more months to the year, eh?
 MaxValue[2] := 12;

 MinValue[3] := 1;  // We don't bother about checking if the month has 31 days.
 MaxValue[3] := 31;

 MinValue[4] := 0;  // We use military time 00 is midnight, 23 is 11 pm.
 MaxValue[4] := 23;

 MinValue[5] := 0;  // Minute value is 00 to 59
 MaxValue[5] := 59;

 MinValue[6] := 0;  // Second value is 00 to 59
 MaxValue[5] := 59;

 // expect values with length of 4,2,2,2,2,2 ...
 ExpectLengths[1] := 4;
 values[1]        := 0;
 for t := 2 to 6 do begin
   ExpectLengths[t] := 2;
   values[t]        := 0;
 end;



 // T loops through each value we are looking for (1..6):
 index := 1; // what character in AsciiDateStr are we looking at?
 for t := 1 to 6 do begin
   if (t>=3) and (index >= len) then
        break; // as long as we at least got the date, we can continue.
   for u := 1 to ExpectLengths[t] do begin
      if (index > len) then begin
            Done := true; // reached end!
            break;
      end;
      ch := AsciiDateStr[index];
      if (ch < '0') OR (ch > '9') then begin
          OutputDebugString(PChar('JvCsvData:illegal character in datetime string: '+ch));
          exit; // failed:invalid character.
      end;
      values[t] := (values[t]*10)+ (Ord(ch)-Ord('0'));
      inc(Index);

      if (index > len) then begin
            Done := true; // reached end!
            break;
      end;
   end;
   
   // if we haven't reached the end of the string, then
   // check for a valid separator character:
   if (index<len) then
     if (AsciiDateStr[index] <> Separators[t])
        and (AsciiDateStr[index] <> Separators2[t]) then
        exit;

   // validate ranges:
   if (Values[t] < MinValue[t]) OR (Values[t] > MaxValue[t]) then
        exit; // a value is out of range.
   Inc(Index);
 end;

  // Now that we probably have a valid value we will try to encode it.
  // EncodeData will catch any invalid date values we have let slip through
  // such as trying to encode February 29 on a non-leap year, or the 31st
  // day of a month with only 30 days, etc.
 try
  result :=   EncodeDate( {year} Values[1], {month}  Values[2], {day}    Values[3] )
            + EncodeTime( {hour} Values[4], {minute} Values[5], {second} Values[6], {msec} 0 );
 except
   on E:EConvertError do begin
     result := 0.0; // catch any other conversion errors and just return 0.
   end;
 end;
end;


function DateTimeToTimeTHex(aDateTime:TDateTime; TimeZoneCorrection:Integer ):String;
var
  Base             : TDateTime;
{  DateTimeAsStr    : String; //debug Code. }
  SecondsSince1970 : Integer;
begin
  try
     Base := EncodeDate(1970, 1, 1);
     SecondsSince1970 := Trunc( (aDateTime-Base) * 86400.0 );
     result := IntToHex(SecondsSince1970-TimeZoneCorrection, 8);
  except
     // Catch Failures!
     result := '';
  end;
end;

function DateTimeToTimeToIsoAscii(aDateTime:TDateTime):String;
begin
  // ISO DATETIME FORMAT:
 result := FormatDateTime( 'yyyy-mm-dd hh:nn:ss', aDateTime );
end;


function JvFilePathSplit(filename:String; var path,filenameonly:String):Boolean;
var
  len,t:Integer;
begin
 len := Length(filename);
 result := false;
 path := '';
 filenameonly := '';
 for t := len downto 1 do begin
     if filename[t] = '\' then begin
        path := Copy(filename,1,t);
        filenameonly := Copy(filename,t+1,len);
        if (Length(filenameonly)>0) and (Length(path)>0) and DirectoryExists(Path) then
           result := true;
        exit;
     end;
 end;
end;

{ Routine to keep backup copies of old data files around }
function JvCsvBackupPreviousFiles( filename:String; MaxFiles:Integer):Boolean;
var
  BackupFolder,FileNameOnly,BackupFilename,RemoveFile:String;
  t:Integer;
  found:Boolean;
  function MakeFilename(index:Integer):String;
  begin
    result := BackupFolder + FileNameOnly + '.'+IntToStr(index)+'.bak';
  end;
begin
  result := false;

  if not FileExists(filename) then
    exit; // failed.
  if not JvFilePathSplit(filename,BackupFolder,FileNameOnly) then begin
      FileNameOnly := filename;
      GetDir(0,BackupFolder);
      BackupFolder := BackupFolder + '\';
  end;
  BackupFolder := BackupFolder + 'Backup\';
  if not DirectoryExists(BackupFolder) then
    CreateDirectory(PChar(BackupFolder),nil);
    
  found := false;
  for t := 0 to MaxFiles-1 do begin
     BackupFilename := MakeFilename(t);
     if not FileExists(BackupFilename) then begin
        RemoveFile := MakeFilename((t+1) mod MaxFiles );
        found := true;
        break;
     end;
  end;

  if not found then begin
      t := 1;
      BackupFilename :=   MakeFilename(t);
      RemoveFile := MakeFilename((t+1) mod MaxFiles );
  end;

  // We remove an old backup if necessary so that the next time we run
  // we will find the gap and know where to write the next numbered
  // backup. That means that anywhere from zero to 998 backups could exist
  // in a circular fashion. Without this logic, we wouldn't know the next
  // extension number to use.
  if FileExists(RemoveFile) then
     DeleteFile(RemoveFile);

  CopyFile(PChar(filename),PChar(BackupFilename),false);
  result := true;
end;



initialization
  CallCount := 0;
end.

