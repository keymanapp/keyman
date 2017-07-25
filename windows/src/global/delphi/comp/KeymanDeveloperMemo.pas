unit KeymanDeveloperMemo;

interface

{-$DEFINE USE_PLUSMEMO}

uses
  System.Classes,
  System.SysUtils,
{$IFDEF USE_PLUSMEMO}
  PlusMemo;
{$ELSE}
  Vcl.Graphics,
  Vcl.StdCtrls;
{$ENDIF}

type
{$IFDEF USE_PLUSMEMO}
  TKeymanDeveloperMemo = class(TPlusMemo)
{$ELSE}

  TpmUpdateMode = (umImmediate, umOnNeed, umBackground);

  TKeymanDeveloperMemoOption = (pmoKeepColumnPos, pmoPutExtraSpaces,
                     pmoWrapCaret, pmoInsertKeyActive,
                     pmoWideOverwriteCaret, pmoLargeWordSelect,
                     pmoAutoScrollBars, pmoNoDragnDrop,
                     pmoAutoIndent, pmoBackIndent,
                     pmoWindowsSelColors, pmoFullLineSelect,
                     pmoDiscardTrailingSpaces, pmoNoFineScroll,
                     pmoNoLineSelection, pmoNoDefaultPopup,
                     pmoBlockSelection, pmoAutoLineBreak,
                     pmoPlainClipboard, pmoPersistentBlocks,
                     pmoNoOverwriteBlocks, pmoSmartTabs, pmoKeepParBackgnd);

  TKeymanDeveloperMemoOptions = set of TKeymanDeveloperMemoOption;

  TMemoBeforeChangeEvent = procedure(Sender: TObject; var Txt: PWideChar) of object;

  TKeymanDeveloperMemo = class(TMemo)
  private
    FTabStops: Integer;
    FAltFont: TFont;
    FOverwrite: Boolean;
    FApplyStartStopKeys: Boolean;
    FRightLinePen: TPen;
    FShowEndParSelected: Boolean;
    FEndOfTextMark: TPen;
    FColumnWrap: Integer;
    FEnableHotkeys: Boolean;
    FSelTextColor: TColor;
    FSelBackColor: TColor;
    FSeparators: string;
    FOptions: TKeymanDeveloperMemoOptions;
    FJustified: Boolean;
    FCaretWidth: Integer;
    FDisplayOnly: Boolean;
    FSpecUnderline: TPen;
    FVersion: string;
    FUndoMaxSpace: Integer;
    FOnSelMove: TNotifyEvent;
    FOnBeforeChange: TMemoBeforeChangeEvent;
    FUpdateMode: TpmUpdateMode;
    function GetLinesArray(Index: Integer): string;
    function GetSelCol: Integer;
    function GetSelLine: Integer;
    procedure SetSelCol(const Value: Integer);
    procedure SetSelLine(const Value: Integer);
    procedure SetAltFont(const Value: TFont);
    function GetEncoding: TEncoding;
    procedure SetOverwrite(const Value: Boolean);
    function GetPargrphOffset(Index: Integer): Integer;
    function GetLineHeight: Integer;
    procedure SetLineHeight(const Value: Integer);
    procedure SetEndOfTextMark(const Value: TPen);
    procedure SetRightLinePen(const Value: TPen);
    procedure SetSpecUnderline(const Value: TPen);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redo;
    function CanRedo: Boolean;

    function FindTxt(SearchString: string; Down, MatchCase, WholeWord, Global: Boolean): Boolean;
    procedure ScrollInView;

    property Encoding: TEncoding read GetEncoding;
    property LinesArray[Index: Integer]: string read GetLinesArray;
    property PargrphOffset[Index: Integer]: Integer read GetPargrphOffset;
    property SelCol: Integer read GetSelCol write SetSelCol;
    property SelLine: Integer read GetSelLine write SetSelLine;
    property SelPar: Integer read GetSelLine write SetSelLine;
    function CaretX: Integer;
    function CaretY: Integer;
    function LineCount: Integer;
    function ParagraphCount: Integer;
    function CurrentWord: string;
    function GetTextPart(a, b: Integer): string;
  published
    { These are mostly unused but stop designer and runtime blowing up due to design-time properties }
    property AltFont: TFont read FAltFont write SetAltFont;
    property ApplyStartStopKeys: Boolean read FApplyStartStopKeys write FApplyStartStopKeys;
    property CaretWidth: Integer read FCaretWidth write FCaretWidth;
    property ColumnWrap: Integer read FColumnWrap write FColumnWrap;
    property DisplayOnly: Boolean read FDisplayOnly write FDisplayOnly;
    property EnableHotKeys: Boolean read FEnableHotkeys write FEnableHotkeys;
    property EndOfTextMark: TPen read FEndOfTextMark write SetEndOfTextMark;
    property Justified: Boolean read FJustified write FJustified;
    property LineHeight: Integer read GetLineHeight write SetLineHeight;
    property Options: TKeymanDeveloperMemoOptions read FOptions write FOptions;
    property Overwrite: Boolean read FOverwrite write SetOverwrite;
    property RightLinePen: TPen read FRightLinePen write SetRightLinePen;
    property SelBackColor: TColor read FSelBackColor write FSelBackColor;
    property SelTextColor: TColor read FSelTextColor write FSelTextColor;
    property Separators: string read FSeparators write FSeparators;
    property ShowEndParSelected: Boolean read FShowEndParSelected write FShowEndParSelected;
    property SpecUnderline: TPen read FSpecUnderline write SetSpecUnderline;
    property TabStops: Integer read FTabStops write FTabStops;
    property UndoMaxSpace: Integer read FUndoMaxSpace write FUndoMaxSpace;
    property UpdateMode: TpmUpdateMode read FUpdateMode write FUpdateMode;
    property Version: string read FVersion write FVersion;

    { Remapped events }
    property OnSelMove: TNotifyEvent read FOnSelMove write FOnSelMove;
    property OnBeforeChange: TMemoBeforeChangeEvent read FOnBeforeChange write FOnBeforeChange;
{$ENDIF}
  end;

procedure Register;

implementation

uses
  System.StrUtils,
  System.WideStrUtils,
  Winapi.Messages,
  Winapi.Windows;

procedure Register;
begin
  RegisterComponents('Keyman', [TKeymanDeveloperMemo]);
end;

{ TKeymanDeveloperMemo }

{$IFNDEF USE_PLUSMEMO}
function TKeymanDeveloperMemo.CanRedo: Boolean;
begin
  Result := False;
end;

procedure TKeymanDeveloperMemo.Redo;
begin
end;

procedure TKeymanDeveloperMemo.ScrollInView;
begin
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TKeymanDeveloperMemo.SetAltFont(const Value: TFont);
begin
  FAltFont.Assign(Value);
end;

procedure TKeymanDeveloperMemo.SetEndOfTextMark(const Value: TPen);
begin
  FEndOfTextMark.Assign(Value);
end;

procedure TKeymanDeveloperMemo.SetLineHeight(const Value: Integer);
begin
  ; // Set line height not supported for TMemo
end;

procedure TKeymanDeveloperMemo.SetOverwrite(const Value: Boolean);
begin
  ; // Overwrite not supported for TMemo
end;

procedure TKeymanDeveloperMemo.SetRightLinePen(const Value: TPen);
begin
  FRightLinePen.Assign(Value);
end;

procedure TKeymanDeveloperMemo.SetSelCol(const Value: Integer);
var
  x: Integer;
begin
  x := SelLine;
  x := SendMessage(Handle, EM_LINEINDEX, x, 0);
  SelStart := x + Value - 1;
end;

procedure TKeymanDeveloperMemo.SetSelLine(const Value: Integer);
begin
  SelStart := SendMessage(Handle, EM_LINEINDEX, Value, 0);
end;

procedure TKeymanDeveloperMemo.SetSpecUnderline(const Value: TPen);
begin
  FSpecUnderline.Assign(Value);
end;

function TKeymanDeveloperMemo.CaretX: Integer;
begin
  Result := Loword(SendMessage(Handle, EM_POSFROMCHAR, SelStart+SelLength, 0));
  if Result < 0 then
    Result := 0;
end;

function TKeymanDeveloperMemo.CaretY: Integer;
begin
  Result := Hiword(SendMessage(Handle, EM_POSFROMCHAR, SelStart+SelLength, 0));
  if Result < 0 then
    Result := 0;
end;

constructor TKeymanDeveloperMemo.Create(AOwner: TComponent);
begin
  inherited;
  FAltFont := TFont.Create;
  FTabStops := 4;
  FEndOfTextMark := TPen.Create;
  FRightLinePen := TPen.Create;
  FSpecUnderline := TPen.Create;
end;

function TKeymanDeveloperMemo.CurrentWord: string;
var
  s: string;
  n: Integer;
  EndN: Integer;
  StartN: Integer;
begin
  if SelLength <> 0 then
    Result := SelText
  else
  begin
    s := Text;
    n := SelStart;

    StartN := 1;
    while n > 0 do
    begin
      Dec(n);
      if not IsCharAlphaNumeric(s[n]) and (s[n] <> '_') then
      begin
        StartN := n+1;
        Break;
      end;
    end;
    n := StartN;

    EndN := Length(Text);
    while n < Length(Text) do
    begin
      Inc(n);
      if not IsCharAlphaNumeric(s[n]) and (s[n] <> '_') then
      begin
        EndN := n-1;
        Break;
      end;
    end;

    Result := Copy(Text, StartN, EndN-StartN+1);
  end;
end;

destructor TKeymanDeveloperMemo.Destroy;
begin
  FreeAndNil(FAltFont);
  FreeAndNil(FEndOfTextMark);
  FreeAndNil(FRightLinePen);
  FreeAndNil(FSpecUnderline);

  inherited Destroy;
end;

function TKeymanDeveloperMemo.FindTxt(SearchString: string; Down, MatchCase, WholeWord, Global: Boolean): Boolean;
var
  Buffer, P: PWideChar;
  Size: Word;
  SearchOptions: TStringSearchOptions;
  s: string;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := GetTextLen;
  if (Size = 0) then Exit;
  Buffer := WStrAlloc(Size + 1);
  try
    SearchOptions := [];
    if Down then
      Include(SearchOptions, soDown);
    if MatchCase then
      Include(SearchOptions, soMatchCase);
    if WholeWord then
      Include(SearchOptions, soWholeWord);

    s := Text;
    P := SearchBuf(PChar(s), Length(s), SelStart, SelLength, SearchString, SearchOptions);
    if P <> nil then
    begin
      SelStart := P - PWideChar(s);
      SelLength := Length(SearchString);
      Result := True;
    end;
  finally
    WStrDispose(Buffer);
  end;
end;

function TKeymanDeveloperMemo.GetEncoding: TEncoding;
begin
  Result := Lines.Encoding;
end;

function TKeymanDeveloperMemo.GetLinesArray(Index: Integer): string;
begin
  Result := Self.Lines[Index];
end;

function TKeymanDeveloperMemo.GetPargrphOffset(Index: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX, Index, 0);
end;

function TKeymanDeveloperMemo.GetSelCol: Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX, SelLine, 0);
  if Result = -1 then
    Exit(0);
  Result := SelStart - Result;
end;

function TKeymanDeveloperMemo.GetSelLine: Integer;
begin
  Result := SendMessage(Handle, EM_LINEFROMCHAR, -1, 0);
end;

function TKeymanDeveloperMemo.GetTextPart(a, b: Integer): string;
begin
  Result := Copy(Text, a, b-a+1);
end;

function TKeymanDeveloperMemo.LineCount: Integer;
begin
  Result := ParagraphCount;
end;

function TKeymanDeveloperMemo.GetLineHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  // When in the designer, we need to make sure the Delphi form designer doesn't ever 
  // hard-code a line height for the text edit. This happens because we are using both
  // TMemo and TPlusMemo behind the scenes, depending on the whether the encumbered
  // TPlusMemo is available. Setting line height to zero at design time means it
  // will be calculated at runtime.

  if csDesigning in ComponentState then
    Result := 0
  else 
  begin
    DC := GetDC(0);
    try
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
    finally
      ReleaseDC(0, DC);
    end;

    Result := Metrics.tmHeight;
  end;
end;

function TKeymanDeveloperMemo.ParagraphCount: Integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
end;

{$ENDIF}

end.
