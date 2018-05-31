(*
  Name:             KeyboardParser
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    30 Aug 2006 - mcdurdin - Fix bug with parsing shift/ctrl/alt rules
                    28 Sep 2006 - mcdurdin - Move version line to start of file
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 Mar 2011 - mcdurdin - I2520 - Improve support for SMP characters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    19 Mar 2014 - mcdurdin - I4145 - V9.0 - Deadkeys are corrupted by KeyboardParser
                    13 Oct 2014 - mcdurdin - I4329 - V9.0 - Sometimes when saving, the list of &WINDOWSLANGUAGES is doubled
                    23 Oct 2014 - mcdurdin - I4369 - V9.0 - Add kmw_embedcss to feature support in Developer
                    04 Nov 2014 - mcdurdin - I4504 - V9.0 - Consolidate the compile action into single command
                    30 Apr 2015 - mcdurdin - I4681 - V9.0 - mnemoniclayout store should be enough to block design view
                    24 Jul 2015 - mcdurdin - I4797 - Convert to characters tool is inconsistent
                    23 Feb 2016 - mcdurdin - I4979 - Importing a KMN into a KVK does not work if includecodes are in the keyboard
*)
unit KeyboardParser;  // I3306

interface

uses
  System.Generics.Collections,
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.WideStrings,

  ExtShiftState,
  kmxfile,
  kmxfileconsts,
  UKeymanTargets,
  utilfiletypes;

{$ASSERTIONS ON}

type
  EKeyboardParser_TagRequired = class(Exception)
    constructor Create(ALine, ATag: WideString);
  end;

  TKeyboardParser = class;

  TKeyboardParser_Line = class
  private
    FLine: WideString;
    FOwner: TKeyboardParser;
    class function ExtShiftStateToString(s: TExtShiftState): WideString; static;
    procedure SetLine(Value: WideString);
  protected
    class function GetTag(var s: WideString): WideString; static;
    class procedure Require(var s: WideString; tagname: WideString); static;
    class procedure TagRequired(s, tagname: WideString); static;
    class function StrToXStr(s: WideString): WideString; static;
    class function ParseKey(s: WideString; var key: Integer; var shiftstate: TExtShiftState): Boolean; static;
    class function ParseKeyChar(s: WideString; var key: Integer; var shiftstate: TExtShiftState): Boolean; static;
    class function XStrToStr(s: WideString; var xstr: WideString): Boolean; static;   // I4797
    property Line: WideString read FLine write SetLine;
    procedure InitLine(ALine: WideString); virtual;
  public
    constructor Create(ALine: WideString); virtual;
    constructor CreateNew;
    procedure ConvertToANSI;
    function IsComplexLine: Boolean; virtual;
    class function IsType(ALine: WideString): Boolean; virtual;
    class procedure Register;

    class function GetXStr(var s, xstr: WideString): Boolean; static;   // I4797

    property Owner: TKeyboardParser read FOwner;
  end;

  TKeyboardParser_LineClass = class of TKeyboardParser_Line;

  TKeyboardParser_BlankLine = class(TKeyboardParser_Line)
  public
    function IsComplexLine: Boolean; override;
    class function IsType(ALine: WideString): Boolean; override;
  end;

  TKeyboardParser_SystemStore = class(TKeyboardParser_Line)
  private
    FSystemStoreType: TSystemStore;
    FValue: WideString;
    procedure SetValue(Value: WideString);
    class function IsSystemStoreName(Value: WideString): Boolean;
  protected
    procedure InitLine(ALine: WideString); override;
  public
    constructor CreateNew(ASystemStoreType: TSystemStore; AValue: WideString);
    function IsComplexLine: Boolean; override;
    class function IsType(ALine: WideString): Boolean; override;
    property SystemStoreType: TSystemStore read FSystemStoreType;
    property Value: WideString read FValue write SetValue;
  end;

  TKeyboardParser_Begin = class(TKeyboardParser_Line)
  private
    FIsUnicode: Boolean;
    FGroupName: WideString;
    procedure SetIsUnicode(const Value: Boolean);
    procedure SetGroupName(Value: WideString);
  protected
    procedure InitLine(ALine: WideString); override;
  public
    constructor CreateNew(AIsUnicode: Boolean; AGroupName: WideString);
    function IsComplexLine: Boolean; override;
    class function IsType(ALine: WideString): Boolean; override;
    property IsUnicode: Boolean read FIsUnicode write SetIsUnicode;
    property GroupName: WideString read FGroupName write SetGroupName;
  end;

  TKeyboardParser_Group = class(TKeyboardParser_Line)
  private
    FGroupName: WideString;
    FUsingKeys: Boolean;
    procedure SetGroupName(Value: WideString);
    procedure SetUsingKeys(const Value: Boolean);
    procedure UpdateLine;
  protected
    procedure InitLine(ALine: WideString); override;
  public
    constructor CreateNew(AGroupName: WideString; AUsingKeys: Boolean);
    class function IsType(ALine: WideString): Boolean; override;
    function IsComplexLine: Boolean; override;
    property GroupName: WideString read FGroupName write SetGroupName;
    property UsingKeys: Boolean read FUsingKeys write SetUsingKeys;
  end;

  TKeyboardParser_Comment = class(TKeyboardParser_Line)
  private
    FValue: WideString;
    procedure SetValue(Value: WideString);
  protected
    procedure InitLine(ALine: WideString); override;
  public
    constructor CreateNew(AValue: WideString; ANew: Boolean);
    class function IsType(ALine: WideString): Boolean; override;
    function IsComplexLine: Boolean; override;
    property Value: WideString read FValue write SetValue;
  end;

  TKeyboardParser_LayoutRule = class(TKeyboardParser_Line)
  private
    FShift: TExtShiftState;
    FOutput: WideString;
    FVKey: Integer;
    procedure UpdateLine;
    procedure SetOutput(Value: WideString);
    procedure SetShift(const Value: TExtShiftState);
    procedure SetVKey(const Value: Integer);
  protected
    procedure InitLine(ALine: WideString); override;
  public
    constructor CreateNew(AVKey: Integer; AShift: TExtShiftState; AOutput: WideString);
    class function IsType(ALine: WideString): Boolean; override;
    function IsComplexLine: Boolean; override;
    property VKey: Integer read FVKey write SetVKey;
    property Shift: TExtShiftState read FShift write SetShift;
    property Output: WideString read FOutput write SetOutput;
  end;

  TKeyboardParser_Lines = class(TObjectList)
  private
    FOwner: TKeyboardParser;
    function GetLine(Index: Integer): TKeyboardParser_Line;
    function GetString(Index: Integer): WideString;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(AOwner: TKeyboardParser);
    function IndexOfClass(AClassType: TKeyboardParser_LineClass; APreviousIndex: Integer = -1): Integer;
    property Lines[Index: Integer]: TKeyboardParser_Line read GetLine; default;
    property Strings[Index: Integer]: WideString read GetString;
  end;

  TKeyboardParser_LineTypes = class(TClassList)
  private
    function GetItems(Index: Integer): TKeyboardParser_LineClass;
    procedure SetItems(Index: Integer; const Value: TKeyboardParser_LineClass);
  public
    property Items[Index: Integer]: TKeyboardParser_LineClass read GetItems write SetItems; default;
  end;

  TKeyboardParser_WinLanguage = class   // I4021
  private
    FName: string;
    FID: Integer;
    FIsDefault: Boolean;
    FOwner: TKeyboardParser;
    procedure SetIsDefault(const Value: Boolean);
  public
    constructor Create(AOwner: TKeyboardParser; AID: Integer);
    property ID: Integer read FID;
    property Name: string read FName;
    property IsDefault: Boolean read FIsDefault write SetIsDefault;
  end;

  TKeyboardParser_WinLanguages = class(TObjectList<TKeyboardParser_WinLanguage>)   // I4021
  private
    FOwner: TKeyboardParser;
    constructor Create(AOwner: TKeyboardParser);
    procedure Fill;
    procedure UpdateSource;
  protected
    procedure Notify(const Value: TKeyboardParser_WinLanguage; Action: System.Generics.Collections.TCollectionNotification); override;
  public
    function Add(AID: Integer): Integer;
    function IndexOfID(AID: Integer): Integer;
    function IndexOfDefault: Integer;
  end;

  TKeyboardParser_FeatureID = (
    kfIcon, kfOSK, kfTouchLayout,
    kfEmbedJS, kfEmbedCSS, kfKMWHelp, kfIncludeCodes);   // I4369

  TKeyboardParser_Feature = class   // I4021
  private
    FOwner: TKeyboardParser;
    FFilename: string;
    FID: TKeyboardParser_FeatureID;
    function GetExpandedFilename: string;
  public
    constructor Create(AOwner: TKeyboardParser; AID: TKeyboardParser_FeatureID; const AFilename: string);
    property Filename: string read FFilename;
    property ExpandedFilename: string read GetExpandedFilename;
    property ID: TKeyboardParser_FeatureID read FID;
  end;

  TKeyboardParser_Features = class(TObjectDictionary<TKeyboardParser_FeatureID,TKeyboardParser_Feature>)   // I4021
  private
    FOwner: TKeyboardParser;
    constructor Create(AOwner: TKeyboardParser);
    procedure Fill;
    procedure UpdateSource;
    function GetDefaultFeatureFilename(ID: TKeyboardParser_FeatureID): string;
  protected
    procedure KeyNotify(const Key: TKeyboardParser_FeatureID; Action: System.Generics.Collections.TCollectionNotification); override;
    procedure ValueNotify(const Value: TKeyboardParser_Feature; Action: System.Generics.Collections.TCollectionNotification); override;
  public
    procedure Add(Key: TKeyboardParser_FeatureID); overload;
  end;

  TKeyboardParser = class
  private
    FLines: TKeyboardParser_Lines;
    FModified: Boolean;
    FOnChanged: TNotifyEvent;
    FWinLanguages: TKeyboardParser_WinLanguages;
    FLoading: Boolean;
    FFeatures: TKeyboardParser_Features;
    FFileName: string;
    function GetKeyboardText: WideString;
    procedure SetKeyboardText(Value: WideString);
    function GetIsComplex: Boolean;
    procedure SetModified(const Value: Boolean);
    function GetIsKeyboardUnicode: Boolean;
    function GetInitialComment: WideString;
    procedure SetInitialComment(Value: WideString);
    procedure GetCommentLines(var Start, Finish: Integer);
    function GetSystemStore(Index: TSystemStore): TKeyboardParser_SystemStore;
    procedure MoveVersionLineToStart;
  public
    constructor Create;
    destructor Destroy; override;

    function AddLayoutRule(AVKey: Integer; AShift: TExtShiftState; AOutput: WideString): TKeyboardParser_LayoutRule;
    //function AddSystemStore(ASystemStoreType: TSystemStore; const AValue: WideString): TKeyboardParser_SystemStore;
    procedure DeleteSystemStore(ASystemStoreType: TSystemStore);
    procedure SetSystemStoreValue(ASystemStoreType: TSystemStore; AValue: WideString);
    function GetSystemStoreValue(ASystemStoreType: TSystemStore): WideString;
    property SystemStores[Index: TSystemStore]: TKeyboardParser_SystemStore read GetSystemStore;

    function GetKeyboardTextRaw: string;   // I4979

    procedure AddRequiredLines;

    procedure LoadFromFile(FileName: WideString);

    property WinLanguages: TKeyboardParser_WinLanguages read FWinLanguages;

    property KeyboardText: WideString read GetKeyboardText write SetKeyboardText;
    property Lines: TKeyboardParser_Lines read FLines;

    property Features: TKeyboardParser_Features read FFeatures;
    property FileName: string read FFileName write FFileName; // only used for feature file names

    property Modified: Boolean read FModified write SetModified;

    property IsComplex: Boolean read GetIsComplex;

    property IsKeyboardUnicode: Boolean read GetIsKeyboardUnicode;
    property InitialComment: WideString read GetInitialComment write SetInitialComment;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

const
  KeyboardFeatureName: array[TKeyboardParser_FeatureID] of string = (
    'Icon', 'Desktop On Screen Keyboard', 'Touch-Optimised Keyboard',
    'Embedded Javascript', 'Embedded CSS', 'Web Help', 'Include Codes');   // I4369
  KeyboardFeatureStore: array[TKeyboardParser_FeatureID] of TSystemStore = (
    ssBitmap, ssVisualKeyboard, ssLayoutFile,
    ssKMW_EmbedJS, ssKMW_EmbedCSS, ssKMW_HelpFile, ssIncludeCodes);   // I4369
  KeyboardFeatureFilename: array[TKeyboardParser_FeatureID] of string = (
    '%s.ico', '%s'+Ext_VisualKeyboardSource, '%s'+Ext_KeymanTouchLayout,
    '%s-code.js', '%s.css', '%s-help.htm', '%s-codes.txt');   // I4369
  KeyboardFeatureTargets: array[TKeyboardParser_FeatureID] of TKeymanTargets = (   // I4504
    // Due to Delphi compiler limitation, need to copy the target values, can't
    // use the defined constants.
    [ktWindows, ktMacosx, ktDesktop], //KMXKeymanTargets
    [ktAny, ktWindows, ktMacosx, ktLinux, ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktDesktop, ktTablet], //AllKeymanTargets
    [ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktTablet], //TouchKeymanTargets
    [ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktTablet], //KMWKeymanTargets
    [ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktTablet], //KMWKeymanTargets
    [ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktTablet], //KMWKeymanTargets
    [ktAny, ktWindows, ktMacosx, ktLinux, ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet, ktMobile, ktDesktop, ktTablet] //AllKeymanTargets
  );

implementation

uses
  KeymanVersion,
  Unicode,
  utilstr,
  utilsystem,
  VKeys,
  WindowsLanguages;

var
  FLineTypes: TKeyboardParser_LineTypes = nil;

{ TKeyboardParser_Line }

procedure TKeyboardParser_Line.ConvertToANSI;
begin
  InitLine(FLine);
end;

constructor TKeyboardParser_Line.Create(ALine: WideString);
begin
  inherited Create;
  InitLine(ALine);
end;

class function TKeyboardParser_Line.GetTag(var s: WideString): WideString;
var
  n: Integer;
  ch: WideChar;
    function IsWhiteSpace: Boolean;
    begin
      Result := CharInSet(s[n], [#1..#32]);  // I3310
    end;
    function Finished: Boolean;
    begin
      Result := n > Length(s);
    end;
    function IsPunctuation: Boolean;
    begin
      Result := CharInSet(s[n], ['(',')','[',']','>']);  // I3310
    end;
    function IsQuote: Boolean;
    begin
      Result := CharInSet(s[n], ['''','"']);  // I3310
    end;
    function IsNumber: Boolean;
    begin
      Result := CharInSet(s[n], ['0'..'9']);  // I3310
    end;
begin
  Result := '';
  n := 1;
  while not Finished and IsWhiteSpace do Inc(n);
  if Finished then Exit;

  Delete(s,1,n-1); n := 1;

  { The next character determines the tag type }

  case s[n] of
    'a'..'z','A'..'Z', '$', '&', '_': { a symbol name or potentially xvvv dvvv U+vvvv - allow any characters apart from ' " ( ) [ ] (whitespace) to continue it }
    begin
      while not Finished and not IsWhiteSpace and not IsPunctuation and not IsQuote do Inc(n);
      Dec(n);
    end;
    '''','"': { a string }
    begin
      ch := s[n]; Inc(n);
      while not Finished and (s[n] <> ch) do Inc(n);
    end;
    '0'..'9': { an 'octal' value (usually) }
    begin
      while not Finished and IsNumber do Inc(n);
      Dec(n);
    end;
    {else punctuation - do nothing}
  end;

  Result := Copy(s,1,n);
  Delete(s,1,n);
end;

constructor TKeyboardParser_Line.CreateNew;
begin
  inherited Create;
end;

class function TKeyboardParser_Line.ExtShiftStateToString(s: TExtShiftState): WideString;
begin
  Result := '';
  if essShift in s then Result := Result + 'SHIFT ';
  if essCtrl in s then  Result := Result + 'CTRL ';
  if essLCtrl in s then Result := Result + 'LCTRL ';
  if essRCtrl in s then Result := Result + 'RCTRL ';
  if essAlt in s then   Result := Result + 'ALT ';
  if essLAlt in s then  Result := Result + 'LALT ';
  if essRAlt in s then  Result := Result + 'RALT ';
end;

class function TKeyboardParser_Line.XStrToStr(s: WideString; var xstr: WideString): Boolean;
    function OctToDec(n: Integer): Integer;
    var
      i, j: Integer;
    begin
      Result := 0;
      i := 10; j := 1; {ugly and does not check 8,9 but can't be bothered doing it nicely for this infrequent use}
      while i div 10 <= n do
      begin
        Result := Result + (n mod i) div (i div 10) * j;
        j := j * 8;
        i := i * 10;
      end;
    end;
var
  n: Integer;
begin
  Result := s <> '';
  if Result then
    case s[1] of
      'U', 'u':
        begin
          if not (Length(s) in [6,7,8]) then Exit(False);   // I4145
          if s[2] <> '+' then Exit;
          n := StrToIntDef('$'+Copy(s,3,6), 0);
          if (n < 32) or (n > $10FFFF) then Exit(False);   // I4145
          xstr := xstr + Uni_UTF32CharToUTF16(n);
        end;
      'd':
        begin
          n := StrToIntDef(Copy(s,2,10), 0);
          if (n < 32) or (n > $10FFFF) then Exit(False);   // I4145
          xstr := xstr + Uni_UTF32CharToUTF16(n);
        end;
      'x':
        begin
          n := StrToIntDef('$'+Copy(s,2,10), 0);
          if (n < 32) or (n > $10FFFF) then Exit(False);   // I4145
          xstr := xstr + Uni_UTF32CharToUTF16(n);
        end;
      '0'..'7':
        begin
          n := StrToIntDef(s, 0);
          if (n < 40) or (n > 4177777) then Exit(False);   // I4145
          xstr := xstr + Uni_UTF32CharToUTF16(OctToDec(n));
        end;
      '''', '"':
        begin
          xstr := xstr + Copy(s, 2, Length(s)-2);
        end;
      else
        Result := False;
    end;
end;

class function TKeyboardParser_Line.GetXStr(var s, xstr: WideString): Boolean;
var
  t: WideString;
begin
  Result := False;
  xstr := '';
  t := GetTag(s);
  while XStrToStr(t, xstr) do begin Result := True; t := GetTag(s); end;
end;

procedure TKeyboardParser_Line.InitLine(ALine: WideString);
begin
  FLine := ALine;
end;

function TKeyboardParser_Line.IsComplexLine: Boolean;
begin
  Result := True;
end;

class function TKeyboardParser_Line.IsType(ALine: WideString): Boolean;
begin
  Result := False;
end;

class procedure TKeyboardParser_Line.Register;
begin
  FLineTypes.Add(Self);
end;

class procedure TKeyboardParser_Line.Require(var s: WideString; tagname: WideString);
var
  t: WideString;
begin
  t := s;
  if not WideSameText(GetTag(s), tagname) then
    TagRequired(t, tagname);
end;

class function TKeyboardParser_Line.ParseKey(s: WideString; var key: Integer; var shiftstate: TExtShiftState): Boolean;
var
  u: WideString;
begin
  Result := False;

  Assert(GetTag(s) = '[');

  shiftstate := [];
  key := $FFFF;
  u := UpperCase(GetTag(s));
  while u <> ']' do
  begin
    if u = 'SHIFT' then Include(shiftstate, essShift)
    else if u = 'CTRL'  then Include(shiftstate, essCtrl)
    else if u = 'ALT'   then Include(shiftstate, essAlt)
    else if u = 'LCTRL' then Include(shiftstate, essLCtrl)
    else if u = 'LALT'  then Include(shiftstate, essLAlt)
    else if u = 'RCTRL' then Include(shiftstate, essRCtrl)
    else if u = 'RALT'  then Include(shiftstate, essRAlt)
    else
    begin
      if key <> $FFFF then Exit;
      key := FindVKeyName(u);
      if key = $FFFF then Exit;
    end;
    u := UpperCase(GetTag(s));
  end;

  Result := key <> $FFFF;
end;

type
  TKey10To11 = record
    Char, Shifted: WideChar;
    VKey: WideString;
  end;

class function TKeyboardParser_Line.ParseKeyChar(s: WideString; var key: Integer; var shiftstate: TExtShiftState): Boolean;
const
  Key10To11: array[0..47] of TKey10To11 = (
    (Char: '`'; Shifted: '~'; VKey: 'K_BKQUOTE'),
    (Char: '1'; Shifted: '!'; VKey: 'K_1'),
    (Char: '2'; Shifted: '@'; VKey: 'K_2'),
    (Char: '3'; Shifted: '#'; VKey: 'K_3'),
    (Char: '4'; Shifted: '$'; VKey: 'K_4'),
    (Char: '5'; Shifted: '%'; VKey: 'K_5'),
    (Char: '6'; Shifted: '^'; VKey: 'K_6'),
    (Char: '7'; Shifted: '&'; VKey: 'K_7'),
    (Char: '8'; Shifted: '*'; VKey: 'K_8'),
    (Char: '9'; Shifted: '('; VKey: 'K_9'),
    (Char: '0'; Shifted: ')'; VKey: 'K_0'),
    (Char: '-'; Shifted: '_'; VKey: 'K_HYPHEN'),
    (Char: '='; Shifted: '+'; VKey: 'K_EQUAL'),
    (Char: 'q'; Shifted: 'Q'; VKey: 'K_Q'),
    (Char: 'w'; Shifted: 'W'; VKey: 'K_W'),
    (Char: 'e'; Shifted: 'E'; VKey: 'K_E'),
    (Char: 'r'; Shifted: 'R'; VKey: 'K_R'),
    (Char: 't'; Shifted: 'T'; VKey: 'K_T'),
    (Char: 'y'; Shifted: 'Y'; VKey: 'K_Y'),
    (Char: 'u'; Shifted: 'U'; VKey: 'K_U'),
    (Char: 'i'; Shifted: 'I'; VKey: 'K_I'),
    (Char: 'o'; Shifted: 'O'; VKey: 'K_O'),
    (Char: 'p'; Shifted: 'P'; VKey: 'K_P'),
    (Char: '['; Shifted: '{'; VKey: 'K_LBRKT'),
    (Char: ']'; Shifted: '}'; VKey: 'K_RBRKT'),
    (Char: '\'; Shifted: '|'; VKey: 'K_BKSLASH'),
    (Char: 'a'; Shifted: 'A'; VKey: 'K_A'),
    (Char: 's'; Shifted: 'S'; VKey: 'K_S'),
    (Char: 'd'; Shifted: 'D'; VKey: 'K_D'),
    (Char: 'f'; Shifted: 'F'; VKey: 'K_F'),
    (Char: 'g'; Shifted: 'G'; VKey: 'K_G'),
    (Char: 'h'; Shifted: 'H'; VKey: 'K_H'),
    (Char: 'j'; Shifted: 'J'; VKey: 'K_J'),
    (Char: 'k'; Shifted: 'K'; VKey: 'K_K'),
    (Char: 'l'; Shifted: 'L'; VKey: 'K_L'),
    (Char: ';'; Shifted: ':'; VKey: 'K_COLON'),
    (Char: ''''; Shifted: '"'; VKey: 'K_QUOTE'),
    (Char: 'z'; Shifted: 'Z'; VKey: 'K_Z'),
    (Char: 'x'; Shifted: 'X'; VKey: 'K_X'),
    (Char: 'c'; Shifted: 'C'; VKey: 'K_C'),
    (Char: 'v'; Shifted: 'V'; VKey: 'K_V'),
    (Char: 'b'; Shifted: 'B'; VKey: 'K_B'),
    (Char: 'n'; Shifted: 'N'; VKey: 'K_N'),
    (Char: 'm'; Shifted: 'M'; VKey: 'K_M'),
    (Char: ','; Shifted: '<'; VKey: 'K_COMMA'),
    (Char: '.'; Shifted: '>'; VKey: 'K_PERIOD'),
    (Char: '/'; Shifted: '?'; VKey: 'K_SLASH'),
    (Char: ' '; Shifted: ' '; VKey: 'K_SPACE')
  );
var
  j: Integer;
begin
  for j := 0 to High(Key10To11) do
  begin
    if Key10To11[j].Shifted = s then
      shiftstate := [essShift]
    else if Key10To11[j].Char = s then
      shiftstate := []
    else Continue;
    key := FindVKeyName(Key10To11[j].VKey);
    Result := True;
    Exit;
  end;
  Result := False;
end;

procedure TKeyboardParser_Line.SetLine(Value: WideString);
begin
  FLine := Value;
  if Assigned(FOwner) then FOWner.Modified := True;
end;

class function TKeyboardParser_Line.StrToXStr(s: WideString): WideString;
var
  n: Integer;
  inquote: Boolean;
begin
  inquote := False;
  Result := '';
  for n := 1 to Length(s) do
  begin
    if (Ord(s[n]) < 32) or (s[n] = '''') then //WideChar(s[n]) in [#1..#31, ''''] then
    begin
      if inquote then
      begin
        inquote := False;
        Result := Result + ''' ';
      end;
      Result := Result + 'U+'+IntToHex(Ord(s[n]), 4)+' ';
    end
    else
    begin
      if not inquote then
      begin
        inquote := True;
        Result := Result + '''';
      end;
      Result := Result + s[n];
    end;
  end;
  if inquote then
    Result := Result + '''';
  Result := Trim(Result);

  if Result = '' then
    Result := '""';
end;

class procedure TKeyboardParser_Line.TagRequired(s, tagname: WideString);
begin
  raise EKeyboardParser_TagRequired.Create(s, tagname);
end;

{ TKeyboardParser_Begin }

procedure TKeyboardParser_Begin.InitLine(ALine: WideString);
var
  t: WideString;
begin
  inherited InitLine(ALine);
  Assert(WideSameText(GetTag(ALine), 'begin'));
  t := GetTag(ALine);
  FIsUnicode := WideSameText(t, 'unicode');

  if WideSameText(t, 'ansi') or FIsUnicode then Require(ALine, '>')
  else if t <> '>' then TagRequired(ALine, '>');
  Require(ALine, 'use');
  Require(ALine, '(');
  FGroupName := GetTag(ALine);
  Require(ALine, ')');
end;

constructor TKeyboardParser_Begin.CreateNew(AIsUnicode: Boolean;
  AGroupName: WideString);
begin
  inherited CreateNew;
  FGroupName := AGroupName;
  IsUnicode := AIsUnicode; // Also sets Line
end;

function TKeyboardParser_Begin.IsComplexLine: Boolean;
begin
  Result := not WideSameText(FGroupName, 'main');
end;

class function TKeyboardParser_Begin.IsType(ALine: WideString): Boolean;
var
  t: WideString;
begin
  Result := WideSameText(GetTag(ALine), 'begin');
  if Result then
  begin
    t := GetTag(ALine);
    if WideSameText(t, 'ansi') or WideSameText(t, 'unicode') then t := GetTag(ALine);

    Result := (t = '>') and WideSameText(GetTag(ALine), 'use') and (GetTag(ALine) = '(');
    if Result then
    begin
      GetTag(ALine);
      Result := GetTag(ALine) = ')';
    end;
  end;
end;

procedure TKeyboardParser_Begin.SetGroupName(Value: WideString);
begin
  FGroupName := Value;
end;

procedure TKeyboardParser_Begin.SetIsUnicode(const Value: Boolean);
begin
  FIsUnicode := Value;
  if FIsUnicode
    then Line := 'begin Unicode > use('+FGroupName+')'
    else Line := 'begin ANSI > use('+FGroupName+')';
end;

{ TKeyboardParser_Comment }

procedure TKeyboardParser_Comment.InitLine(ALine: WideString);
begin
  inherited InitLine(ALine);
  Assert(WideSameText(GetTag(ALine), 'c'));
  FValue := Trim(ALine);
end;

constructor TKeyboardParser_Comment.CreateNew(AValue: WideString; ANew: Boolean);
begin
  inherited CreateNew;
  Value := AValue;
end;

function TKeyboardParser_Comment.IsComplexLine: Boolean;
begin
  Result := False;
end;

class function TKeyboardParser_Comment.IsType(ALine: WideString): Boolean;
begin
  Result := WideSameText(GetTag(ALine), 'c');
end;

procedure TKeyboardParser_Comment.SetValue(Value: WideString);
begin
  FValue := Value;
  Line := 'c '+FValue;
end;

{ EKeyboardParser_TagRequired }

constructor EKeyboardParser_TagRequired.Create(ALine, ATag: WideString);
begin
  inherited Create('Expected tag "'+ATag+'" at '+ALine);
end;

{ TKeyboardParser_SystemStore }

procedure TKeyboardParser_SystemStore.InitLine(ALine: WideString);
var
  s: WideString;
  v2: Integer;
  v1: Integer;
begin
  inherited InitLine(ALine);
  s := GetTag(ALine);
  if WideSameText(s, 'store') then
  begin
    Assert(WideSameText(GetTag(ALine), '('));
    FSystemStoreType := SystemStoreFromName(GetTag(ALine));
    Assert(FSystemStoreType <> ssNone);
    Assert(WideSameText(GetTag(ALine), ')'));
    Assert(GetXStr(ALine, FValue));
  end
  else
  begin
    if WideSameText(s, 'message') or WideSameText(s, 'copyright') or WideSameText(s, 'name') or WideSameText(s, 'hotkey') then
    begin
      if WideSameText(s, 'message') then FSystemStoreType := ssMessage
      else if WideSameText(s, 'copyright') then FSystemStoreType := ssCopyright
      else if WideSameText(s, 'name') then FSystemStoreType := ssName
      else if WideSameText(s, 'hotkey') then FSystemStoreType := ssHotkey;
      s := GetTag(ALine);
      Assert(Copy(s,1,1) = '"');
      Delete(s,1,1); if (s <> '') and (s[Length(s)] = '"') then Delete(s,Length(s),1);
      FValue := s;
    end
    else if WideSameText(s, 'version') then
    begin
      v1 := StrToInt(GetTag(ALine));
      Assert(GetTag(ALine) = '.');
      v2 := StrToInt(GetTag(ALine));
      FSystemStoreType := ssVersion;
      FValue := IntToStr(v1)+'.'+IntToStr(v2);
    end
    else if WideSameText(s, 'bitmap') then
    begin
      FSystemStoreType := ssBitmap;
      s := GetTag(ALine);
      if Copy(s,1,1) = '"' then
      begin
        Delete(s,1,1); if (s <> '') and (s[Length(s)] = '"') then Delete(s,Length(s),1);
      end;
      FValue := s;
    end
    else if WideSameText(s, 'shift') then
    begin
      Assert(WideSameText(GetTag(ALine), 'frees') and WideSameText(GetTag(ALine), 'caps'));
      FSystemStoreType := ssShiftFreesCaps;
      FValue := '1';
    end
    else if WideSameText(s, 'caps') then
    begin
      s := GetTag(ALine);
      if WideSameText(s, 'always') then
      begin
        Assert(WideSameText(GetTag(ALine), 'off'));
        FSystemStoreType := ssCapsAlwaysOff;
      end
      else
      begin
        Assert(WideSameText(s, 'on'));
        Assert(WideSameText(GetTag(ALine), 'only'));
        FSystemStoreType := ssCapsOnOnly;
      end;
      FValue := '1';
    end
    else if WideSameText(s, 'language') then
    begin
      v1 := StrToInt(GetTag(ALine));
      Assert(GetTag(ALine) = ',');
      v2 := StrToInt(GetTag(ALine));
      FSystemStoreType := ssLanguage;
      FValue := IntToStr(v1)+', '+IntToStr(v2);
    end;
  end;
end;

constructor TKeyboardParser_SystemStore.CreateNew(
  ASystemStoreType: TSystemStore; AValue: WideString);
begin
  inherited CreateNew;
  FSystemStoreType := ASystemStoreType;
  Value := AValue; // Also sets Line  
end;

function TKeyboardParser_SystemStore.IsComplexLine: Boolean;
begin
  Result := (FSystemStoreType = ssMnemonicLayout) and (Value = '1');   // I4681
end;

class function TKeyboardParser_SystemStore.IsSystemStoreName(Value: WideString): Boolean;
begin
  Result := SystemStoreFromName(Value) <> ssNone;
end;

class function TKeyboardParser_SystemStore.IsType(ALine: WideString): Boolean;
var
  t, s: WideString;
  v: Integer;
begin
  t := ALine;
  Result :=
    WideSameText(GetTag(ALine), 'store') and
    WideSameText(GetTag(ALine), '(') and
    IsSystemStoreName(GetTag(ALine)) and
    WideSameText(GetTag(ALine), ')') and
    GetXStr(ALine, s);

  if not Result then
  begin
    { read old header names }
    // MESSAGE, COPYRIGHT, NAME, BITMAP, SHIFT FREES CAPS, CAPS ON ONLY, HOTKEY, LANGUAGE, LAYOUT, VERSION, CAPS ALWAYS OFF
    ALine := t;
    s := GetTag(ALine);
    if WideSameText(s, 'message') or WideSameText(s, 'copyright') or WideSameText(s, 'name') or WideSameText(s, 'hotkey') then
    begin
      s := GetTag(ALine);
      if Copy(s,1,1) <> '"' then Exit;
    end
    else if WideSameText(s, 'version') then
    begin
      if not TryStrToInt(GetTag(ALine), v) or (GetTag(ALine) <> '.') or not TryStrToInt(GetTag(ALine), v) then Exit;
    end
    else if WideSameText(s, 'bitmap') then
    else if WideSameText(s, 'shift') then
    begin
      if not WideSameText(GetTag(ALine), 'frees') or not WideSameText(GetTag(ALine), 'caps') then Exit;
    end
    else if WideSameText(s, 'caps') then
    begin
      s := GetTag(ALine);
      if not ((WideSameText(s, 'always') and WideSameText(GetTag(ALine), 'off')) or
        (WideSameText(s, 'on') and WideSameText(GetTag(ALine), 'only'))) then Exit;
    end
    else if WideSameText(s, 'language') then
    begin
      if not TryStrToInt(GetTag(ALine), v) then Exit;
      if GetTag(ALine) <> ',' then Exit;
      if not TryStrToInt(GetTag(ALine), v) then Exit;
    end
    else Exit;
    Result := True;
  end;
end;

procedure TKeyboardParser_SystemStore.SetValue(Value: WideString);
begin
  FValue := Value;
  Line := 'store(&'+SystemStoreNames[FSystemStoreType]+') '+StrToXStr(FValue);
end;

{ TKeyboardParser_LayoutRule }

procedure TKeyboardParser_LayoutRule.InitLine(ALine: WideString);
var
  t: WideString;
  keystr: WideString;
begin
  inherited InitLine(ALine);
  Assert(GetTag(ALine) = '+');

  t := GetTag(ALine);
  if t = '[' then
  begin
    keystr := t;
    while (t <> '') and (t <> ']') do
    begin
      t := GetTag(ALine);
      keystr := keystr + t + ' ';
    end;
    Assert(t = ']');
    Assert(ParseKey(keystr, FVKey, FShift));
  end
  else if XStrToStr(t, keystr) then
  begin
    { alphanumeric key - mostly from KeymanWizard 1.0 }
    Assert(ParseKeyChar(keystr, FVKey, FShift));
  end
  else
    Assert(False);

  Assert(GetTag(ALine) = '>');
  Assert(GetXStr(ALine, FOutput));
end;

constructor TKeyboardParser_LayoutRule.CreateNew(AVKey: Integer; AShift: TExtShiftState; AOutput: WideString);
begin
  inherited CreateNew;
  FVKey := AVKey;
  FShift := AShift;
  FOutput := AOutput;
  UpdateLine;
end;

function TKeyboardParser_LayoutRule.IsComplexLine: Boolean;
begin
  Result := False;
end;

class function TKeyboardParser_LayoutRule.IsType(ALine: WideString): Boolean;
var
  keystr, t: WideString;
  VKey: Integer;
  shiftstate: TExtShiftState;
begin
  Result := (GetTag(ALine) = '+');
  if Result then
  begin
    t := GetTag(ALine);
    if t = '[' then
    begin
      keystr := t;
      while (t <> '') and (t <> ']') do
      begin
        t := GetTag(ALine);
        keystr := keystr + t + ' ';
      end;
      Result := ParseKey(keystr, VKey, shiftstate);
    end
    else if XStrToStr(t, keystr) then
    begin
      { alphanumeric key - mostly from KeymanWizard 1.0 }
      Result := ParseKeyChar(keystr, VKey, shiftstate);
    end
    else
      Result := False;

    if Result then
    begin
      Result := (GetTag(ALine) = '>') and GetXStr(ALine, keystr);
    end;
  end;
end;

procedure TKeyboardParser_LayoutRule.SetOutput(Value: WideString);
begin
  FOutput := Value;
  UpdateLine;
end;

procedure TKeyboardParser_LayoutRule.SetShift(const Value: TExtShiftState);
begin
  FShift := Value;
  UpdateLine;
end;

procedure TKeyboardParser_LayoutRule.SetVKey(const Value: Integer);
begin
  FVKey := Value;
  UpdateLine;
end;

procedure TKeyboardParser_LayoutRule.UpdateLine;
begin
  Line := '+ ['+ExtShiftStateToString(FShift)+VKeyNames[FVKey]+'] > '+StrToXStr(FOutput);
end;

{ TKeyboardParser_Lines }

constructor TKeyboardParser_Lines.Create(AOwner: TKeyboardParser);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TKeyboardParser_Lines.GetLine(Index: Integer): TKeyboardParser_Line;
begin
  Result := inherited GetItem(Index) as TKeyboardParser_Line;
end;

function TKeyboardParser_Lines.GetString(Index: Integer): WideString;
begin
  Result := Lines[Index].FLine;
end;

function TKeyboardParser_Lines.IndexOfClass(
  AClassType: TKeyboardParser_LineClass; APreviousIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := APreviousIndex+1 to Count - 1 do
  begin
    if Items[i] is AClassType then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TKeyboardParser_Lines.Notify(Ptr: Pointer; Action: TListNotification);
var
  line: TKeyboardParser_Line;
begin
  line := TKeyboardParser_Line(Ptr);
  if Action = lnAdded then
    if Assigned(line.FOwner)
      then raise EListError.Create('Line is already owned by another parser.')
      else line.FOwner := FOwner
  else if Action in [lnExtracted, lnDeleted] then
    line.FOwner := nil;

  FOwner.Modified := True;

  inherited Notify(Ptr, Action);
end;

{ TKeyboardParser_LineTypes }

function TKeyboardParser_LineTypes.GetItems(Index: Integer): TKeyboardParser_LineClass;
begin
  Result := TKeyboardParser_LineClass(inherited GetItems(Index));
end;

procedure TKeyboardParser_LineTypes.SetItems(Index: Integer; const Value: TKeyboardParser_LineClass);
begin
  inherited SetItems(Index, Value);
end;

{ TKeyboardParser }

procedure TKeyboardParser.AddRequiredLines;
var
  i: Integer;
  FLastSystemStore: Integer;
  FGroupLine, FBeginLine: Integer;
  FCommentEnd: Integer;
begin
  {
    begin > use(main)
    group(main) using keys
  }

  FGroupLine := FLines.IndexOfClass(TKeyboardParser_Group);

  FCommentEnd := -1; FBeginLine := -1; FLastSystemStore := -1;
  for i := 0 to FLines.Count - 1 do
  begin
    if FLines[i] is TKeyboardParser_SystemStore then
      FLastSystemStore := i
    else if (Lines[i] is TKeyboardParser_Begin) and (FBeginLine < 0) then
      FBeginLine := i
    else if not (Lines[i] is TKeyboardParser_Comment) and (FCommentEnd < 0) then
      FCommentEnd := i;
  end;

  if FCommentEnd < 0 then FCommentEnd := FLines.Count;

  if FLastSystemStore < 0 then i := FCommentEnd
  else i := FLastSystemStore + 1;

  if FGroupLine < 0 then
  begin
      FLines.Insert(i, TKeyboardParser_Group.CreateNew('main', True));
      FLines.Insert(i, TKeyboardParser_BlankLine.CreateNew);
  end;

  if FBeginLine < 0 then
  begin
    FLines.Insert(i, TKeyboardParser_Begin.CreateNew(True, 'main'));
    FLines.Insert(i, TKeyboardParser_BlankLine.CreateNew);
  end;
end;

procedure TKeyboardParser.MoveVersionLineToStart;
var
  FFirstNonBlankCommentLine, FVersionLine: Integer;
  i: Integer;
begin
  FFirstNonBlankCommentLine := -1;
  FVersionLine := -1;
  for i := 0 to FLines.Count - 1 do
  begin
    if (FLines[i] is TKeyboardParser_SystemStore) and ((FLines[i] as TKeyboardParser_SystemStore).SystemStoreType = ssVersion) then
      FVersionLine := i;
    if (FFirstNonBlankCommentLine < 0) and not (FLines[i] is TKeyboardParser_BlankLine) and not (FLines[i] is TKeyboardParser_Comment) then
      FFirstNonBlankCommentLine := i;
  end;

  if (FVersionLine > FFirstNonBlankCommentLine) then
  begin
    if FFirstNonBlankCommentLine < 0
      then FLines.Move(FVersionLine, 0)
      else FLines.Move(FVersionLine, FFirstNonBlankCommentLine);
  end;
end;

function TKeyboardParser.AddLayoutRule(AVKey: Integer; AShift: TExtShiftState; AOutput: WideString): TKeyboardParser_LayoutRule;
var
  n: Integer;
begin
  AddRequiredLines;

  Result := TKeyboardParser_LayoutRule.CreateNew(AVKey, AShift, AOutput);
  n := FLines.IndexOfClass(TKeyboardParser_Group);
  FLines.Insert(n+1, Result);
end;

constructor TKeyboardParser.Create;
begin
  inherited Create;
  FLines := TKeyboardParser_Lines.Create(Self);
  FWinLanguages := TKeyboardParser_WinLanguages.Create(Self);
  FFeatures := TKeyboardParser_Features.Create(Self);
end;

procedure TKeyboardParser.DeleteSystemStore(ASystemStoreType: TSystemStore);
var
  i: Integer;
begin
  for i := 0 to FLines.Count - 1 do
    if (FLines[i] is TKeyboardParser_SystemStore) and
      ((FLines[i] as TKeyboardParser_SystemStore).SystemStoreType = ASystemStoreType) then
    begin
      FLines.Delete(i);
      Exit;
    end;
end;

destructor TKeyboardParser.Destroy;
begin
  FOnChanged := nil;
  FLines.Free;
  FWinLanguages.Free;
  FFeatures.Free;
  inherited Destroy;
end;

procedure TKeyboardParser.GetCommentLines(var Start, Finish: Integer);
begin
  Start := 0;
  Finish := -1;
  while Start < FLines.Count do
  begin
    if FLines[Start] is TKeyboardParser_BlankLine then Inc(Start)
    else if FLines[Start] is TKeyboardParser_Comment then Break
    else
    begin
      Start := -1;
      Exit; { no comment at start of file }
    end;
  end;

  if Start = FLines.Count then
  begin
    Start := -1;
    Exit; { Blank file }
  end;

  Finish := Start + 1;
  while Finish < FLines.Count do
  begin
    if not (FLines[Finish] is TKeyboardParser_Comment) then Break;
    Inc(Finish);
  end;
  Dec(Finish);
end;

function TKeyboardParser.GetInitialComment: WideString;
var
  FStart, FEnd: Integer;
  i: Integer;
begin
  Result := '';
  FStart := 0;
  GetCOmmentLines(FStart, FEnd);
  if FStart = -1 then Exit;

  for i := FStart to FEnd do
    Result := Result + (FLines[i] as TKeyboardParser_Comment).Value + #13#10;
end;

function TKeyboardParser.GetIsComplex: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FLines.Count - 1 do
    if FLines[i].IsComplexLine then Exit;

  Result := False;
end;

function TKeyboardParser.GetIsKeyboardUnicode: Boolean;
var
  n: Integer;
begin
  AddRequiredLines;
  n := FLines.IndexOfClass(TKeyboardParser_Begin);
  Result := (FLines[n] as TKeyboardParser_Begin).IsUnicode;
end;

function TKeyboardParser.GetKeyboardText: WideString;
var
  i: Integer;
begin
  FLoading := True;
  try
    FWinLanguages.UpdateSource;
    FFeatures.UpdateSource;
  finally
    FLoading := False;
  end;

  Result := '';
  for i := 0 to FLines.Count - 1 do
    Result := Result + FLines[i].FLine + #13#10;
end;

function TKeyboardParser.GetKeyboardTextRaw: string;   // I4979
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FLines.Count - 1 do
    Result := Result + FLines[i].FLine + #13#10;
end;

procedure TKeyboardParser.SetInitialComment(Value: WideString);
var
  FStart, FEnd: Integer;
  i: Integer;
begin
  with TWideStringList.Create do
  try
    Text := Trim(Value);

    GetCommentLines(FStart, FEnd);

    if FStart >= 0 then
    begin
      for i := FStart to FEnd do
        FLines.Delete(FStart);
    end;
    { Add new lines }
    for i := 0 to Count-1 do
    begin
      FLines.Insert(i, TKeyboardParser_Comment.CreateNew(Strings[i], True));
    end;
  finally
    Free;
  end;
end;

procedure TKeyboardParser.SetKeyboardText(Value: WideString);
var
  s: TWideStrings;
  i, j: Integer;
  line: TKeyboardParser_Line;
begin
  FModified := False;
  FLoading := True;
  try
    FLines.Clear;

    if Trim(Value) = '' then
    begin
      FLines.Add(TKeyboardParser_Begin.CreateNew(True, 'main'));
    end
    else
    begin
      s := TWideStringList.Create;
      try
        s.Text := Value;
        for i := 0 to s.Count - 1 do
        begin
          line := nil;
          for j := 0 to FLineTypes.Count - 1 do
            if FLineTypes[j].IsType(s[i]) then
            begin
              line := FLineTypes[j].Create(s[i]);
              Break;
            end;
          if not Assigned(line) then
            // not a recognised line - just add it anyway
            line := TKeyboardParser_Line.Create(s[i]);
          FLines.Add(line);
        end;
      finally
        s.Free;
      end;
    end;

    MoveVersionLineToStart;

    FWinLanguages.Fill;
    FFeatures.Fill;
  finally
    FLoading := False;
  end;
end;

procedure TKeyboardParser.SetModified(const Value: Boolean);
begin
  if FLoading then
    Exit;

  if FModified <> Value then
  begin
    FModified := Value;
  end;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TKeyboardParser.SetSystemStoreValue(ASystemStoreType: TSystemStore; AValue: WideString);
begin
  if AValue = ''
    then DeleteSystemStore(ASystemStoreType)
    else SystemStores[ASystemStoreType].Value := AValue;
end;

function TKeyboardParser.GetSystemStore(Index: TSystemStore): TKeyboardParser_SystemStore;
var
  FCommentEnd, FBeginLine, i: Integer;
begin
  AddRequiredLines;

  FCommentEnd := -1; FBeginLine := -1;
  for i := 0 to FLines.Count - 1 do
  begin
    if (Lines[i] is TKeyboardParser_SystemStore) and ((Lines[i] as TKeyboardParser_SystemStore).SystemStoreType = Index) then
    begin
      Result := Lines[i] as TKeyboardParser_SystemStore;
      Exit;
    end
    else if (Lines[i] is TKeyboardParser_Begin) and (FBeginLine < 0) then
      FBeginLine := i
    else if not (Lines[i] is TKeyboardParser_Comment) and (FCommentEnd < 0) then
      FCommentEnd := i;
  end;

  if FCommentEnd = -1 then FCommentEnd := FLines.Count;

  Result := TKeyboardParser_SystemStore.CreateNew(Index, '');
  if FBeginLine >= 0 then
  begin
    if (FBeginLine > 0) and (FLines[FBeginLine - 1] is TKeyboardParser_BlankLine) then Dec(FBeginLine);
    FLines.Insert(FBeginLine, Result);
  end
  else
    FLines.Insert(FCommentEnd, Result)
end;

function TKeyboardParser.GetSystemStoreValue(ASystemStoreType: TSystemStore): WideString;
var
  i: Integer;
begin
  for i := 0 to FLines.Count - 1 do
    if (FLines[i] is TKeyboardParser_SystemStore) and
      ((FLines[i] as TKeyboardParser_SystemStore).SystemStoreType = ASystemStoreType) then
    begin
      Result := (FLines[i] as TKeyboardParser_SystemStore).Value;
      Exit;
    end;
  Result := '';
end;

procedure TKeyboardParser.LoadFromFile(FileName: WideString);
begin
  if FileExists(FileName) then
  begin
    with TStringList.Create do
    try
      LoadFromFile(FileName);  // Let prolog decide encoding
      KeyboardText := Text;
    finally
      Free;
    end;
  end
  else
  begin
    FLines.Clear;
    AddRequiredLines;
  end;
end;

{ TKeyboardParser_BlankLine }

function TKeyboardParser_BlankLine.IsComplexLine: Boolean;
begin
  Result := False;
end;

class function TKeyboardParser_BlankLine.IsType(ALine: WideString): Boolean;
begin
  Result := Trim(ALine) = '';
end;

{ TKeyboardParser_Group }

procedure TKeyboardParser_Group.InitLine(ALine: WideString);
var
  t: WideString;
begin
  inherited InitLine(ALine);
  Assert(WideSameText(GetTag(ALine), 'group'));
  Assert(WideSameText(GetTag(ALine), '('));
  FGroupName := GetTag(ALine);
  Assert(WideSameText(GetTag(ALine), ')'));
  t := GetTag(ALine);
  if t = '' then FUsingKeys := False
  else if not WideSameText(t, 'c') then
  begin
    Assert(WideSameText(t, 'using'));
    Assert(WideSameText(GetTag(ALine), 'keys'));
    FUsingKeys := True;
  end;
end;

constructor TKeyboardParser_Group.CreateNew(AGroupName: WideString;
  AUsingKeys: Boolean);
begin
  inherited CreateNew;
  FGroupName := AGroupName;
  FUsingKeys := AUsingKeys; 
  UpdateLine;
end;

function TKeyboardParser_Group.IsComplexLine: Boolean;
begin
  Result := not FUsingKeys or not WideSameText(FGroupName, 'main');
end;

class function TKeyboardParser_Group.IsType(ALine: WideString): Boolean;
var
  t: WideString;
begin
  Result := WideSameText(GetTag(ALine), 'group') and
    WideSameText(GetTag(ALine), '(') and
    (GetTag(ALine) <> '') and
    WideSameText(GetTag(ALine), ')');
  if Result then
  begin
    t := GetTag(ALine);
    Result := (t = '') or WideSameText(t, 'c') or
      (WideSameText(t, 'using') and WideSameText(GetTag(ALine), 'keys'));
  end;
end;

procedure TKeyboardParser_Group.SetGroupName(Value: WideString);
begin
  FGroupName := Value;
  UpdateLine;
end;

procedure TKeyboardParser_Group.SetUsingKeys(const Value: Boolean);
begin
  FUsingKeys := Value;
end;

procedure TKeyboardParser_Group.UpdateLine;
begin
  Line := 'group('+FGroupName+')';
  if FUsingKeys then Line := Line + ' using keys';
end;

{ Utility functions }

function OctToDec(p: WideString): Integer;
var
  i, j, n: Integer;
begin
  n := StrToIntDef(p, 0);
  Result := 0;
  i := 10; j := 1; {ugly and does not check 8,9 but can't be bothered doing it nicely for this infrequent use}
  while i div 10 <= n do
  begin
    Result := Result + (n mod i) div (i div 10) * j;
    j := j * 8;
    i := i * 10;
  end;
end;

function xatoi(p: WideString): Integer;
begin
  Result := 0;
  if p = '' then Exit;

  case UpCase(Char(p[1])) of
    'U':
      begin
        if Copy(p,2,1) <> '+' then Exit;
        Result := StrToIntDef('$'+Copy(p,3,6), 0);
      end;
    'X':
      Result := StrToIntDef('$'+Copy(p,2,6), 0);
    'D':
      Result := StrToIntDef(Copy(p,2,6), 0);
    '0'..'7':
      Result := OctToDec(p);
  end;
end;

{ TKeyboardParser_WinLanguages }

function TKeyboardParser_WinLanguages.Add(AID: Integer): Integer;
begin
  Result := inherited Add(TKeyboardParser_WinLanguage.Create(FOwner, AID));
end;

constructor TKeyboardParser_WinLanguages.Create(AOwner: TKeyboardParser);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TKeyboardParser_WinLanguages.Fill;
var
  FWinCodes: string;
  FDefaultWinCode: string;
  FPriLangID: Integer;
  FSecondary: string;
  FLangID: Integer;
  s: string;
  PriLangID, SubLangID: Integer;
  n: Integer;
begin
  Clear;   // I4329
  FWinCodes := FOwner.GetSystemStoreValue(ssWindowsLanguages);
  FDefaultWinCode := FOwner.GetSystemStoreValue(ssLanguage);

  { Parse the FDefaultWinCode }

  if FDefaultWinCode <> '' then
  begin
    FPriLangID := xatoi(StrToken(FDefaultWinCode, ', '));
    FSecondary := StrToken(FDefaultWinCode, ' c'#10);
    if FSecondary = '' then
    begin
      FLangID := FPriLangID;
    end
    else
    begin
      PriLangID := FPriLangID;
      SubLangID := xatoi(FSecondary);
      FLangID := PriLangID or (SubLangID shl 10);
    end;
  end
  else
    FLangID := 0;

  s := StrToken(FWinCodes, ' ');
  while s <> '' do
  begin
    n := xatoi(s);
    if n = 0 then
      Break;
    Add(n);
    s := StrToken(FWinCodes, ' ');
  end;

  if FLangID > 0 then
  begin
    n := IndexOfID(FLangID);
    if n < 0 then
      n := Add(FLangID);

    Items[n].FIsDefault := True;
  end;
end;

function TKeyboardParser_WinLanguages.IndexOfDefault: Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].IsDefault then
      Exit(i);
  Exit(-1);
end;

function TKeyboardParser_WinLanguages.IndexOfID(AID: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ID = AID then
      Exit(i);
  Exit(-1);
end;

procedure TKeyboardParser_WinLanguages.Notify(
  const Value: TKeyboardParser_WinLanguage;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  FOwner.Modified := True;
end;

procedure TKeyboardParser_WinLanguages.UpdateSource;
var
  i, n: Integer;
  FLanguage: string;
begin
  n := IndexOfDefault;

  if n < 0 then
    FOwner.DeleteSystemStore(ssLanguage)
  else
  begin
    FLanguage := 'x'+IntToHex(Items[n].ID, 4);
    FOwner.SetSystemStoreValue(ssLanguage, FLanguage);
  end;

  FLanguage := '';
  for i := 0 to Count - 1 do
  begin
    if i > 0 then FLanguage := FLanguage + ' ';
    FLanguage := FLanguage + 'x'+IntToHex(Items[i].ID, 4);
  end;

  if FLanguage = ''
    then FOwner.DeleteSystemStore(ssWindowsLanguages)
    else FOwner.SetSystemStoreValue(ssWindowsLanguages, FLanguage);
end;

{ TKeyboardParser_WinLanguage }

constructor TKeyboardParser_WinLanguage.Create(AOwner: TKeyboardParser; AID: Integer);
var
  wl: TWindowsLanguage;
begin
  inherited Create;

  FOwner := AOwner;

  FID := AID;

  wl := GetWindowsLanguage(ID);

  if wl.Name = ''
    then FName := 'Unknown language x'+IntToHex(AID, 4)
    else FName := wl.Name;
end;

procedure TKeyboardParser_WinLanguage.SetIsDefault(const Value: Boolean);
begin
  FIsDefault := Value;
  FOwner.Modified := True;
end;

{ TKeyboardParser_Features }

procedure TKeyboardParser_Features.Add(Key: TKeyboardParser_FeatureID);
begin
  inherited Add(Key, TKeyboardParser_Feature.Create(FOwner, Key, GetDefaultFeatureFilename(Key)));
end;

function TKeyboardParser_Features.GetDefaultFeatureFilename(ID: TKeyboardParser_FeatureID): string;
begin
  Result := Format(KeyboardFeatureFilename[ID], [ChangeFileExt(ExtractFileName(FOwner.FileName), '')]);
end;

constructor TKeyboardParser_Features.Create(AOwner: TKeyboardParser);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TKeyboardParser_Features.Fill;
var
  kf: TKeyboardParser_FeatureID;
  FValue: string;
begin
  Clear;
  for kf := Low(TKeyboardParser_FeatureID) to High(TKeyboardParser_FeatureID) do
  begin
    FValue := FOwner.GetSystemStoreValue(KeyboardFeatureStore[kf]);
    if FValue <> '' then
    begin
      Add(kf, TKeyboardParser_Feature.Create(FOwner, kf, FValue));
    end;
  end;
end;

procedure TKeyboardParser_Features.KeyNotify(
  const Key: TKeyboardParser_FeatureID;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  FOwner.Modified := True;
end;

procedure TKeyboardParser_Features.UpdateSource;
var
  kf: TKeyboardParser_FeatureID;
begin
  for kf := Low(TKeyboardParser_FeatureID) to High(TKeyboardParser_FeatureID) do
  begin
    if ContainsKey(kf) then
      FOwner.SetSystemStoreValue(KeyboardFeatureStore[kf], Items[kf].Filename)
    else
      FOwner.DeleteSystemStore(KeyboardFeatureStore[kf]);
  end;
end;

procedure TKeyboardParser_Features.ValueNotify(
  const Value: TKeyboardParser_Feature;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  FOwner.Modified := True;
end;

{ TKeyboardParser_Feature }

constructor TKeyboardParser_Feature.Create(AOwner: TKeyboardParser;
  AID: TKeyboardParser_FeatureID;
  const AFilename: string);
begin
  inherited Create;
  FOwner := AOwner;
  FID := AID;
  FFilename := AFilename;
end;

function TKeyboardParser_Feature.GetExpandedFilename: string;
begin
  if FID = kfIcon then
    if ExtractFileExt(FFilename) = '' then FFilename := FFilename + '.bmp';

  Result := ExpandFileNameEx(FOwner.FileName, FFilename);
end;

initialization
  FLineTypes := TKeyboardParser_LineTypes.Create;
  TKeyboardParser_SystemStore.Register;
  TKeyboardParser_Begin.Register;
  TKeyboardParser_Comment.Register;
  TKeyboardParser_LayoutRule.Register;
  TKeyboardParser_BlankLine.Register;
  TKeyboardParser_Group.Register;
finalization
  FreeAndNil(FLineTypes);
end.
