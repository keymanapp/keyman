(*
  Name:             VisualKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2006 - mcdurdin - Add ExtShiftStateToVkShiftState function
                    19 Mar 2007 - mcdurdin - I699 - Fix crash when exporting on screen keyboard with some shift states
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    06 Apr 2010 - mcdurdin - I2200 - Mnemonic layout and OSK
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit VisualKeyboard;  // I3306

interface

uses
  Windows,
  Contnrs,
  Graphics,
  SysUtils,
  Classes,
  ExtShiftState;

const
  KVKS_NORMAL =  0;
  KVKS_SHIFT  =  1;
  KVKS_CTRL   =  2;
  KVKS_ALT    =  4;
  KVKS_LCTRL  =  8;
  KVKS_RCTRL  = 16;
  KVKS_LALT   = 32;
  KVKS_RALT   = 64;

function ExtShiftStateToVkShiftState(ShiftState: TExtShiftState): WORD;
function VkShiftStateToKmxShiftState(ShiftState: WORD): WORD;

type
  TVisualKeyboardKeyFlags = set of (kvkkBitmap, kvkkUnicode);  // if not set, is kvkkText, kvkkANSI resp.
  TVisualKeyboardHeaderFlags = set of (kvkh102, kvkhDisplayUnderlying, kvkhUseUnderlying, kvkhAltGr);
  TVisualKeyboardSaverFormat = (kvksfBinary, kvksfXML);

  TVisualKeyboardKey = class
  private
    FBitmap: TBitmap;
    FFlags: TVisualKeyboardKeyFlags;
    FShift: Word;
    FText: WideString;
    FVKey: Word;
    procedure SetBitmap(const Value: TBitmap);
    function GetVKeyCap: WideChar;
  public
    destructor Destroy; override;
    function HasShift(s: Word): Boolean;
    property Flags: TVisualKeyboardKeyFlags read FFlags write FFlags;
    property VKey: Word read FVKey write FVKey;
    property VKeyCap: WideChar read GetVKeyCap;
    property Shift: Word read FShift write FShift;
    property Text: WideString read FText write FText;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  end;

  TVisualKeyboardKeyList = class(TObjectList)
  protected
    function Get(Index: Integer): TVisualKeyboardKey;
    procedure Put(Index: Integer; Item: TVisualKeyboardKey);
  public
    property Items[Index: Integer]: TVisualKeyboardKey read Get write Put; default;
    function IndexOf(VK, Shift: Word): Integer;
    function Add(Item: TVisualKeyboardKey): Integer;
    procedure Sort;
  end;

  TVisualKeyboardHeader = class
  private
    FAssociatedKeyboard: string;
    FANSIFont: TFont;
    FUnicodeFont: TFont;
    FFlags: TVisualKeyboardHeaderFlags;
    FUnderlyingLayout: string;
    procedure SetANSIFont(const Value: TFont);
    procedure SetUnicodeFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Flags: TVisualKeyboardHeaderFlags read FFlags write FFlags;
    property ANSIFont: TFont read FANSIFont write SetANSIFont;
    property UnicodeFont: TFont read FUnicodeFont write SetUnicodeFont;
    property AssociatedKeyboard: string read FAssociatedKeyboard write FAssociatedKeyboard;
    property UnderlyingLayout: string read FUnderlyingLayout write FUnderlyingLayout;
  end;

  TVisualKeyboard = class
  private
    FHeader: TVisualKeyboardHeader;
    FKeys: TVisualKeyboardKeyList;
    FLoadedFileFormat: TVisualKeyboardSaverFormat;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(FFileName: WideString; Format: TVisualKeyboardSaverFormat);
    procedure LoadFromFile(FFileName: WideString);
    procedure SaveToStream(Stream: TStream; Format: TVisualKeyboardSaverFormat);
    procedure LoadFromStream(Stream: TStream);
    class function IsBinaryFile(Stream: TStream): Boolean; static;
    procedure Clear;
    function HasShiftState(Shift: Integer): Boolean;
    property Header: TVisualKeyboardHeader read FHeader;
    property Keys: TVisualKeyboardKeyList read FKeys;
    property LoadedFileFormat: TVisualKeyboardSaverFormat read FLoadedFileFormat;
  end;

  { Visual keyboard load/save routines -- round trippable, single file }

  TVisualKeyboardLoader = class
  protected
    FKbd: TVisualKeyboard;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure LoadFromFile(const Filename: string);
    constructor Create(AKbd: TVisualKeyboard); virtual;
  end;

  TVisualKeyboardSaver = class
  protected
    FKbd: TVisualKeyboard;
  public
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SaveToFile(const Filename: string);
    constructor Create(AKbd: TVisualKeyboard); virtual;
  end;

  { Importing and Exporting abstract classes -- file dependencies, non round-trippable }

  TVisualKeyboardImport = class
  protected
    FKbd: TVisualKeyboard;
  public
    procedure ImportFromFile(FileName: WideString); virtual; abstract;
    constructor Create(AKbd: TVisualKeyboard); virtual;
  end;

  TVisualKeyboardExport = class
  protected
    FKbd: TVisualKeyboard;
  public
    procedure ExportToFile(FileName: WideString); virtual; abstract;
    constructor Create(AKbd: TVisualKeyboard); virtual;
  end;

  TVisualKeyboardExportClass = class of TVisualKeyboardExport;

  EVisualKeyboardLoader = class(Exception);
  EVisualKeyboardExport = class(Exception);

type
  TVKLegalShiftState = record
    Desc, Name: string;
    Shift: Integer;
    VKeys: array[0..2] of Word;
  end;

const
  MaxLegalShiftStates = 24;
  VKLegalShiftStates: array[0..MaxLegalShiftStates-1] of TVKLegalShiftState = (
    (Desc: 'Unshifted';                  Name: '';      Shift: KVKS_NORMAL; VKeys: (0,0,0)),    //1

    (Desc: 'Shift';                      Name: 'S';     Shift: KVKS_SHIFT; VKeys: (VK_SHIFT,0,0)),
    (Desc: 'Ctrl';                       Name: 'C';     Shift: KVKS_CTRL; VKeys: (VK_CONTROL,0,0)),
    (Desc: 'Alt';                        Name: 'A';     Shift: KVKS_ALT; VKeys: (VK_MENU,0,0)),
    (Desc: 'Shift+Ctrl';                 Name: 'SC';    Shift: KVKS_SHIFT or KVKS_CTRL; VKeys: (VK_SHIFT,VK_CONTROL,0)),
    (Desc: 'Shift+Alt';                  Name: 'SA';    Shift: KVKS_SHIFT or KVKS_ALT; VKeys: (VK_SHIFT,VK_MENU,0)),
    (Desc: 'Ctrl+Alt';                   Name: 'CA';    Shift: KVKS_CTRL or KVKS_ALT; VKeys: (VK_CONTROL,VK_MENU,0)),
    (Desc: 'Shift+Ctrl+Alt';             Name: 'SCA';   Shift: KVKS_SHIFT or KVKS_CTRL or KVKS_ALT; VKeys: (VK_SHIFT,VK_CONTROL,VK_MENU)),  //7

    (Desc: 'Left Ctrl';                  Name: 'LC';    Shift: KVKS_LCTRL; VKeys: (0,0,0)),
    (Desc: 'Right Ctrl';                 Name: 'RC';    Shift: KVKS_RCTRL; VKeys: (0,0,0)),
    (Desc: 'Left Alt';                   Name: 'LA';    Shift: KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Right Alt';                  Name: 'RA';    Shift: KVKS_RALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Left Ctrl';            Name: 'SLC';   Shift: KVKS_SHIFT or KVKS_LCTRL; VKeys: (0,0,0)),
    (Desc: 'Shift+Right Ctrl';           Name: 'SRC';   Shift: KVKS_SHIFT or KVKS_RCTRL; VKeys: (0,0,0)),
    (Desc: 'Shift+Left Alt';             Name: 'SLA';   Shift: KVKS_SHIFT or KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Right Alt';            Name: 'SRA';   Shift: KVKS_SHIFT or KVKS_RALT; VKeys: (0,0,0)),
    (Desc: 'Left Ctrl+Left Alt';         Name: 'LCLA';  Shift: KVKS_LCTRL or KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Left Ctrl+Right Alt';        Name: 'LCRA';  Shift: KVKS_LCTRL or KVKS_RALT; VKeys: (0,0,0)),
    (Desc: 'Right Ctrl+Left Alt';        Name: 'RCLA';  Shift: KVKS_RCTRL or KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Right Ctrl+Right Alt';       Name: 'RCRA';  Shift: KVKS_RCTRL or KVKS_RALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Left Ctrl+Left Alt';   Name: 'SLCLA'; Shift: KVKS_SHIFT or KVKS_LCTRL or KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Left Ctrl+Right Alt';  Name: 'SLCRA'; Shift: KVKS_SHIFT or KVKS_LCTRL or KVKS_RALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Right Ctrl+Left Alt';  Name: 'SRCLA'; Shift: KVKS_SHIFT or KVKS_RCTRL or KVKS_LALT; VKeys: (0,0,0)),
    (Desc: 'Shift+Right Ctrl+Right Alt'; Name: 'SRCRA'; Shift: KVKS_SHIFT or KVKS_RCTRL or KVKS_RALT; VKeys: (0,0,0)) );  //16

function GetVKLegalShiftStateIndex(Shift: Integer): Integer;

implementation

uses
  kmxfileconsts,
  VisualKeyboardLoaderBinary,
  VisualKeyboardLoaderXML,
  VisualKeyboardSaverBinary,
  VisualKeyboardSaverXML,
  VKeyChars;

const
  VKLegalShiftStateIndex: array[0..127] of Integer =
    (0, 1, 2, 4, 3, 5, 6, 7, 8, 12, -1, -1, -1, -1, -1, -1, 9, 13,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 14,
    -1, -1, -1, -1, -1, -1, 16, 20, -1, -1, -1, -1, -1, -1, 18, 22,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 15,
    -1, -1, -1, -1, -1, -1, 17, 21, -1, -1, -1, -1, -1, -1, 19, 23,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);

function GetVKLegalShiftStateIndex(Shift: Integer): Integer;
begin
  if (Shift < 0) or (Shift > 127) then Result := -1
  else Result := VKLegalShiftStateIndex[Shift];
end;

{-------------------------------------------------------------------------------
 - TVisualKeyboard                                                            -
 ------------------------------------------------------------------------------}

procedure TVisualKeyboard.Clear;
begin
  FHeader.Clear;
  FKeys.Clear;
end;

constructor TVisualKeyboard.Create;
begin
  inherited Create;
  FHeader := TVisualKeyboardHeader.Create;
  FKeys := TVisualKeyboardKeyList.Create;
end;

destructor TVisualKeyboard.Destroy;
begin
  FHeader.Free;
  FKeys.Free;
  inherited;
end;

function TVisualKeyboard.HasShiftState(Shift: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].Shift = Shift) and (kvkkUnicode in Keys[i].Flags) then
    begin
      if (Keys[i].Text <> '') or Assigned(Keys[i].Bitmap) then
        Exit(True);
    end;
  end;

  Result := False;
end;

procedure TVisualKeyboard.LoadFromFile(FFileName: WideString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

class function TVisualKeyboard.IsBinaryFile(Stream: TStream): Boolean;
var
  ch: array[0..4] of ansichar;  // I3310
begin
  stream.Position := 0;
  stream.Read(ch, 4); ch[4] := #0;
  stream.Position := 0;
  Result := ch = 'KVKF';
end;

procedure TVisualKeyboard.LoadFromStream(Stream: TStream);
begin
  try
    Clear;
    if not IsBinaryFile(Stream) then
    begin
      FLoadedFileFormat := kvksfXML;
      // We'll attempt to load this as a v10 KVK XML
      with TVisualKeyboardLoaderXML.Create(Self) do
      try
        LoadFromStream(stream);
      finally
        Free;
      end;
    end
    else
    begin
      FLoadedFileFormat := kvksfBinary;
      with TVisualKeyboardLoaderBinary.Create(Self) do
      try
        LoadFromStream(stream);
      finally
        Free;
      end;
    end;
  except
    on E:Exception do
      raise EVisualKeyboardLoaderBinary.Create(E.Message);
  end;
end;

procedure TVisualKeyboard.SaveToFile(FFileName: WideString; Format: TVisualKeyboardSaverFormat);
begin
  if Format = kvksfBinary then
    with TVisualKeyboardSaverBinary.Create(Self) do
    try
      SaveToFile(FFilename);
    finally
      Free;
    end
  else
    with TVisualKeyboardSaverXML.Create(Self) do
    try
      SaveToFile(FFilename);
    finally
      Free;
    end;
end;

procedure TVisualKeyboard.SaveToStream(Stream: TStream; Format: TVisualKeyboardSaverFormat);
begin
  if Format = kvksfBinary then
    with TVisualKeyboardSaverBinary.Create(Self) do
    try
      SaveToStream(Stream);
    finally
      Free;
    end
  else
    with TVisualKeyboardSaverXML.Create(Self) do
    try
      SaveToStream(Stream);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
 - TVisualKeyboardHeader                                                      -
 ------------------------------------------------------------------------------}

constructor TVisualKeyboardHeader.Create;
begin
  inherited Create;
  FANSIFont := TFont.Create;
  FUnicodeFont := TFont.Create;
  Clear;
end;

procedure TVisualKeyboardHeader.Clear;
begin
  FANSIFont.Name := 'Arial';
  FANSIFont.Size := -12;
  FUnicodeFont.Assign(FANSIFont);
  FFlags := [];
  FAssociatedKeyboard := '';
  FUnderlyingLayout := '';
end;

destructor TVisualKeyboardHeader.Destroy;
begin
  FANSIFont.Free;
  FUnicodeFont.Free;
  inherited;
end;

procedure TVisualKeyboardHeader.SetANSIFont(const Value: TFont);
begin
  FANSIFont.Assign(Value);
end;

procedure TVisualKeyboardHeader.SetUnicodeFont(const Value: TFont);
begin
  FUnicodeFont.Assign(Value);
end;

{-------------------------------------------------------------------------------
 - TVisualKeyboardKey                                                         -
 ------------------------------------------------------------------------------}

destructor TVisualKeyboardKey.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TVisualKeyboardKey.GetVKeyCap: WideChar;
begin
  { Lookup USVKeyCap from VKey+Shift }
  if ((FShift = 0) or (FShift = KVKS_SHIFT)) and (FVKey <= 255)
    then Result := SUSVkeyChars[FShift,FVKey]
    else Result := #0;
end;

function TVisualKeyboardKey.HasShift(s: Word): Boolean;
begin
  Result := (FShift and s) <> 0;
end;


procedure TVisualKeyboardKey.SetBitmap(const Value: TBitmap);
begin
  if not Assigned(Value) then
    FreeAndNil(FBitmap)
  else
  begin
    if not Assigned(FBitmap) then FBitmap := TBitmap.Create;
    FBitmap.Assign(Value);
  end;
end;

{-------------------------------------------------------------------------------
 - TVisualKeyboardKeyList                                                     -
 ------------------------------------------------------------------------------}

function TVisualKeyboardKeyList.Add(Item: TVisualKeyboardKey): Integer;
begin
  Result := inherited Add(Item);
end;

function TVisualKeyboardKeyList.Get(Index: Integer): TVisualKeyboardKey;
begin
  Result := inherited Items[Index] as TVisualKeyboardKey;
end;

function TVisualKeyboardKeyList.IndexOf(VK, Shift: Word): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[i].FVKey = VK) and (Items[i].FShift = Shift) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TVisualKeyboardKeyList.Put(Index: Integer; Item: TVisualKeyboardKey);
begin
  inherited Put(Index, Item);
end;

function SortVisualKeyboardKeyList(Item1, Item2: Pointer): Integer;
var
  k1, k2: TVisualKeyboardKey;
begin
  k1 := TVisualKeyboardKey(Item1);
  k2 := TVisualKeyboardKey(Item2);
  if k1.Shift = k2.Shift
    then Result := k1.VKey - k2.VKey
    else Result := k1.Shift - k2.Shift;
end;


procedure TVisualKeyboardKeyList.Sort;
begin
  inherited Sort(@SortVisualKeyboardKeyList);
end;

{ TVisualKeyboardImport }

constructor TVisualKeyboardImport.Create(AKbd: TVisualKeyboard);
begin
  inherited Create;
  FKbd := AKbd;
end;

{ TVisualKeyboardExport }

constructor TVisualKeyboardExport.Create(AKbd: TVisualKeyboard);
begin
  inherited Create;
  FKbd := AKbd;
end;

function ExtShiftStateToVkShiftState(ShiftState: TExtShiftState): WORD;
const
  //essShift, essCtrl, essAlt, essLCtrl, essRCtrl, essLAlt, essRAlt
  FVKShiftValue: array[TExtShiftStateValue] of WORD = (
    KVKS_SHIFT, KVKS_CTRL, KVKS_ALT, KVKS_LCTRL, KVKS_RCTRL, KVKS_LALT, KVKS_RALT);
var
  i: TExtShiftStateValue;
begin
  Result := 0;

  for i := Low(TExtShiftStateValue) to High(TExtShiftStateValue) do
    if i in ShiftState then
      Result := Result or FVKShiftValue[i];
end;

{ TVisualKeyboardLoader }

constructor TVisualKeyboardLoader.Create(AKbd: TVisualKeyboard);
begin
  inherited Create;
  FKbd := AKbd;
end;

procedure TVisualKeyboardLoader.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TVisualKeyboardSaver }

constructor TVisualKeyboardSaver.Create(AKbd: TVisualKeyboard);
begin
  inherited Create;
  FKbd := AKbd;
end;

procedure TVisualKeyboardSaver.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function VkShiftStateToKmxShiftState(ShiftState: WORD): WORD;
type
  TVKToKMX = record
    VK, KMX: Word;
  end;
const
  Map: array[0..6] of TVKToKMX = (
    (VK: KVKS_SHIFT; KMX: KMX_SHIFTFLAG),
    (VK: KVKS_CTRL;  KMX: KMX_CTRLFLAG),
    (VK: KVKS_ALT;   KMX: KMX_ALTFLAG),
    (VK: KVKS_LCTRL; KMX: KMX_LCTRLFLAG),
    (VK: KVKS_RCTRL; KMX: KMX_RCTRLFLAG),
    (VK: KVKS_LALT;  KMX: KMX_LALTFLAG),
    (VK: KVKS_RALT;  KMX: KMX_RALTFLAG)
  );
var
  i: Integer;
begin
  Result := 0;
  for i := Low(Map) to High(Map) do
    if (ShiftState and Map[i].VK) = Map[i].VK then
      Result := Result or Map[i].KMX;
end;

end.

