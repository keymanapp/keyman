(*
  Name:             UfrmCharacterMapNew
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Initial version
                    23 Aug 2006 - mcdurdin - Implement filter dialog
                    23 Aug 2006 - mcdurdin - Mouse wheel scrolling and zooming
                    23 Aug 2006 - mcdurdin - Goto command
                    23 Aug 2006 - mcdurdin - Border on character map
                    23 Aug 2006 - mcdurdin - Header displays shorter text when narrow
                    23 Aug 2006 - mcdurdin - Polished character display performance
                    30 Aug 2006 - mcdurdin - Fix crash when sizing grid to less than 1 column
                    14 Sep 2006 - mcdurdin - Moved to new location
                    06 Oct 2006 - mcdurdin - Add current font character support
                    06 Oct 2006 - mcdurdin - Tweak cursor movement with keys
                    06 Oct 2006 - mcdurdin - Add block headers for searching as well as all characters
                    12 Dec 2006 - mcdurdin - Require Keyman's grids.pas which disables the atrocious auto-scroll problem
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - I1463 - Cancel focus on double click
                    14 Jun 2008 - mcdurdin - I1462 - Fix failure to activate filter correctly on mouse click
                    18 Mar 2011 - mcdurdin - I2794 - Fix memory leaks
                    18 Mar 2011 - mcdurdin - I2573 - Drag of character from charmap moves the selected character when starting drag
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    28 May 2014 - mcdurdin - I4222 - V9.0 - Deprecate osWin2000, osWinXP, osWin2003Server
                    25 Sep 2014 - mcdurdin - I4411 - V9.0 - Character map allows Ctrl+Click to insert character
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
*)
unit UfrmCharacterMapNew;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Contnrs, Types,
  CharMapInsertMode, FixedTrackbar, Menus, UnicodeData,
  ExtCtrls, ClearTypeDrawCharacter, ComCtrls, CharacterDragObject;

type
  TCharMapInsertCodeEvent = procedure(Sender: TObject; Control: TWinControl; DragObject: TCharacterDragObject) of object;
  TCharMapCanInsertCodeEvent = procedure(Sender: TObject; Control: TWinControl; var Result: Boolean) of object;

  TCharMapDirection = (mvUp, mvDown, mvLeft, mvRight, mvTop, mvBottom, mvStartOfRow, mvEndOfRow, mvPageUp, mvPageDown, mvUpBlock, mvDownBlock);

  TfrmCharacterMapNew = class(TForm)
    grid: TTntFixedDrawGrid;
    panHeader: TPanel;
    lblSection: TLabel;
    mnuPopup: TPopupMenu;
    mnuPopupFont: TMenuItem;
    mnuPopupInsertCode: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuPopupInsert: TMenuItem;
    mnuPopupInsertModeName: TMenuItem;
    mnuPopupInsertModeCharacter: TMenuItem;
    mnuPopupInsertModeCode: TMenuItem;
    mnuPopupQuality: TMenuItem;
    mnuPopupDisplayQualityCleartype: TMenuItem;
    mnuPopupDisplayQualityAntialiased: TMenuItem;
    mnuPopupDisplayQualityPlain: TMenuItem;
    dlgFont: TFontDialog;
    mnuSeparator2: TMenuItem;
    mnuGoto: TMenuItem;
    mnuPopupFilter: TMenuItem;
    shpBorder: TShape;
    panBottom: TPanel;
    panName: TPanel;
    editCharName: TEdit;
    editFilter: TEdit;
    tbSize: TTrackBar;
    cmdFilter: TButton;
    cmdShowFontsSplit: TMenuItem;
    cmdShowFonts: TMenuItem;
    procedure gridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tbSizeChange(Sender: TObject);
    procedure gridTopLeftChanged(Sender: TObject);
    procedure gridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure gridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnuPopupInsertCodeClick(Sender: TObject);
    procedure mnuPopupInsertModeItemClick(Sender: TObject);
    procedure mnuPopupPopup(Sender: TObject);
    procedure editFilterEnter(Sender: TObject);
    procedure editFilterExit(Sender: TObject);
    procedure editFilterChange(Sender: TObject);
    procedure mnuPopupDisplayQualityClick(Sender: TObject);
    procedure mnuPopupFontClick(Sender: TObject);
    procedure dlgFontApply(Sender: TObject; Wnd: HWND);
    procedure gridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure editFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure gridMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure gridMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure mnuGotoClick(Sender: TObject);
    procedure mnuPopupFilterClick(Sender: TObject);
    procedure panHeaderResize(Sender: TObject);
    procedure gridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure cmdFilterClick(Sender: TObject);
    procedure editFilterMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure gridMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure editCharNameMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure gridDblClick(Sender: TObject);
    procedure cmdShowFontsClick(Sender: TObject);
  private
    //FCharCount: Integer;
    //Chars: PUnicodeCharArray;
    FDrawChar: TCleartypeDrawCharacter;
    FDrawCellBitmap: TBitmap;
    //nicodeBlocks: TUnicodeBlockList;
    //pFontLinking: IMLangFontLink2;
    FSearchBlocks: TUnicodeBlockList;
    FFontName: TFontName;
    FLastActiveControl: TWinControl;
    FOnCancelFocus: TNotifyEvent;  // I1463 - cancel focus on double click
    FOnInsertCode: TCharMapInsertCodeEvent;
    FOnCanInsertCode: TCharMapCanInsertCodeEvent;
    DragStartPos: TPoint;
    FCharMapDrag: Boolean;
    FOnFilterEntered: TNotifyEvent;
    FOnFilterExited: TNotifyEvent;
    FSettingsKey: WideString;
    FFilterText: string;
    FCharNameNextToFilter: Boolean;
    FOnDialogCLosing: TNotifyEvent;
    FOnDialogOpening: TNotifyEvent;
    FIgnoreLastActiveControl: Boolean;
    FShouldCancelFocusOnDblClick: Boolean;  // I1463 - cancel focus on double click
    FCtrlClickIsDblClick: Boolean;   // I4411
    FOnShowFonts: TNotifyEvent;
    FOnFontSizeChanged: TNotifyEvent;

    procedure UpdateFont(AFontName: WideString);
    procedure LoadSettings(var SelectedCharacter: Integer);
    procedure SaveSettings;

    function GetCharFromCell(ACol, ARow: Integer): Integer;
    function GetHeaderCellProperties(ACol, ARow: Integer): TUnicodeBlock;
    procedure FormatGrid(LastChar: Integer = 0);
    procedure UpdateSectionLabel;
    //function GetCodeTextForInsert(ACol, ARow: Integer): WideString;
    function GetDisplayQuality: Integer;
    function GetInsertMode: TCharMapInsertMode;
    procedure SetDisplayQuality(const Value: Integer);
    procedure SetInsertMode(const Value: TCharMapInsertMode);
    function GetFilterText: string;
    procedure SetFilterText(Value: string);
    function GetDragObject: TCharacterDragObject;

    procedure ScreenActiveControlChange(Sender: TObject);

    procedure CancelFocus;  // I1463 - cancel focus on double click
    procedure InsertCode(Control: TWinControl; DragObject: TCharacterDragObject);
    function CanInsertCode(Control: TWinControl): Boolean;
    procedure FilterEntered;
    procedure FilterExited;
    procedure DialogOpening;
    procedure DialogClosing;
    procedure SetCharNameNextToFilter(const Value: Boolean);
    procedure AlignCharName;
    function Blocks: TUnicodeBlockList;
    procedure MoveCell(Direction: TCharMapDirection);
    function GetFontSize: Integer;   // I4807
    procedure SetFocusToGrid;
    procedure SetFocusToFilter;
  public
    constructor Create(AOwner: TComponent; ASettingsKey: WideString); reintroduce;
    procedure Reload;

    function FindCharacter(Code: Integer): Boolean; overload;
    function FindCharacter(code: WideString; var CodeValue: Integer): Boolean; overload;
    property DisplayQuality: Integer read GetDisplayQuality write SetDisplayQuality;
    property InsertMode: TCharMapInsertMode read GetInsertMode write SetInsertMode;
    property FilterText: string read GetFilterText write SetFilterText;

    property CharMapFontName: TFontName read FFontName;   // I4807
    property CharMapFontSize: Integer read GetFontSize;   // I4807

    property IgnoreLastActiveControl: Boolean read FIgnoreLastActiveControl write FIgnoreLastActiveControl;
    property ShouldCancelFocusOnDblClick: Boolean read FShouldCancelFocusOnDblClick write FShouldCancelFocusOnDblClick;  // I1463 - cancel focus on double click
    property CtrlClickIsDblClick: Boolean read FCtrlClickIsDblClick write FCtrlClickIsDblClick;   // I4411

    property CharNameNextToFilter: Boolean read FCharNameNextToFilter write SetCharNameNextToFilter;

    property CharMapDrag: Boolean read FCharMapDrag write FCharMapDrag default True;
    property OnCancelFocus: TNotifyEvent read FOnCancelFocus write FOnCancelFocus;  // I1463 - cancel focus on double click
    property OnCanInsertCode: TCharMapCanInsertCodeEvent read FOnCanInsertCode write FOnCanInsertCode;
    property OnInsertCode: TCharMapInsertCodeEvent read FOnInsertCode write FOnInsertCode;

    property OnFilterEntered: TNotifyEvent read FOnFilterEntered write FOnFilterEntered;
    property OnFilterExited: TNotifyEvent read FOnFilterExited write FOnFilterExited;

    property OnDialogOpening: TNotifyEvent read FOnDialogOpening write FOnDialogOpening;
    property OnDialogClosing: TNotifyEvent read FOnDialogClosing write FOnDialogClosing;

    property OnShowFonts: TNotifyEvent read FOnShowFonts write FOnShowFonts;
    property OnFontSizeChanged: TNotifyEvent read FOnFontSizeChanged write FOnFontSizeChanged;
  end;

var
  frmCharacterMapNew: TfrmCharacterMapNew;

implementation

uses
  GetOsVersion,
  ErrorControlledRegistry,
  RegistryKeys,
  UfrmCharacterMapFilter,
  Unicode,
  utilstr;

{$R *.dfm}

//const
//  Requires_Keyman_Grids_Pas = Keyman_Grids_Pas;

procedure TfrmCharacterMapNew.FormCreate(Sender: TObject);
var
  v: Integer;
begin
  FCharMapDrag := True;

  Screen.OnActiveControlChange := ScreenActiveControlChange;
  FDrawCellBitmap := TBitmap.Create;
  FDrawChar := TCleartypeDrawCharacter.Create;

  mnuPopupDisplayQualityCleartype.Checked := True;   // I4222

  FFontName := 'Arial'; { Load }
  LoadSettings(v);
  UpdateFont(FFontName);
  UpdateSectionLabel;

  FormatGrid(v);
  if FilterText <> '' then editFilterChange(editFilter);
end;

procedure TfrmCharacterMapNew.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FreeAndNil(FDrawCellBitmap);
  FreeUnicodeData;
  FreeAndNil(FDrawChar);
  FreeAndNil(FSearchBlocks);
  Screen.OnActiveControlChange := nil;
end;

procedure TfrmCharacterMapNew.FormResize(Sender: TObject);
begin
  FormatGrid;
  AlignCharName;
end;

function TfrmCharacterMapNew.Blocks: TUnicodeBlockList;
begin
  if Assigned(FSearchBlocks) then
    Result := FSearchBlocks
  else if Assigned(FUnicodeData) then
    Result := FUnicodeData.Blocks
  else
    Result := nil;
end;

procedure TfrmCharacterMapNew.FormatGrid(LastChar: Integer = 0);
var
  x, y, v, i, n: Integer;
  j: Integer;
  FNewRow, FNewCol: Integer;
begin
  SendMessage(grid.Handle, WM_SETREDRAW, 0, 0);
  if LastChar = 0 then
    LastChar := GetCharFromCell(grid.Col, grid.Row);

  FNewRow := -1; FNewCol := -1;
  grid.ColCount := 1; grid.RowCount := 1;
  grid.DefaultColWidth := tbSize.Position;

  { Decide if we need a vertical scroll bar or not }

  x := grid.Width div grid.DefaultColWidth;
  if x < 1 then x := 1;

  for v := 0 to 1 do
  begin
    y := -28; { We don't count the first header row }
    for i := 0 to Blocks.Count - 1 do
      y := y + 28 + ((Blocks[i].CacheCharCount - 1) div x + 1) * tbSize.Position;

    if y < grid.Height then
    begin
      grid.ScrollBars := ssNone;
      Break;
    end
    else
    begin
      grid.ScrollBars := ssVertical;
      x := (grid.Width - GetSystemMetrics(SM_CXVSCROLL)) div grid.DefaultColWidth;
      if x < 1 then x := 1;
    end;
  end;

  { Reorganise the grid }
  grid.ColCount := x;
  grid.DefaultRowHeight := tbSize.Position;

  FDrawCellBitmap.Width := grid.DefaultColWidth;
  FDrawCellBitmap.Height := grid.DefaultRowHeight;
  n := 0;

  for i := 0 to Blocks.Count - 1 do
  begin
    Blocks[i].Tag := -1; { Not represented }

    grid.RowCount := n+1; //FCharCount div grid.ColCount + 1;
    if n = 0
      // Top row must be 1 pixel in order to avoid DIV/0 in Grids.pas #11589
      then grid.RowHeights[n] := 1 {Don't show the first block title}
      else grid.RowHeights[n] := 28;
    Blocks[i].Tag := n;

    n := n + (Blocks[i].CacheCharCount - 1) div x + 1 + 1;
    if (Blocks[i].StartChar <= LastChar) and (LastChar <= Blocks[i].EndChar) then
    begin
      for j := 0 to Blocks[i].CacheCharCount - 1 do
        if Blocks[i].CacheCharData[j] = LastChar then
        begin
          FNewRow := j div x + Blocks[i].Tag + 1;
          FNewCol := j mod x;
          Break;
        end;
    end;
  end;
//  end;

  grid.RowCount := n;

  if FNewRow >= 0 then
  begin
    grid.Col := FNewCol;
    grid.Row := FNewRow;
  end;

  SendMessage(grid.Handle, WM_SETREDRAW, 1, 0);
  InvalidateRect(grid.Handle, nil, False);
  grid.Update;
  panHeader.Update;
  UpdateSectionLabel;
end;

procedure TfrmCharacterMapNew.tbSizeChange(Sender: TObject);
begin
  FormatGrid;
  if Assigned(FOnFontSizeChanged) then   // I4807
    FOnFontSizeChanged(Self);   // I4807
end;

function TfrmCharacterMapNew.GetCharFromCell(ACol, ARow: Integer): Integer;
var
  i, n: Integer;
begin
  Result := 0;
  if Blocks = nil then Exit;

  if (ACol >= 0) and (ARow >= 0) and (ACol < grid.ColCount) and (ARow < grid.RowCount) then
  begin
    for i := 0 to Blocks.Count do
      if (i < Blocks.Count) and (Blocks[i].Tag = ARow) then Exit
      else if (i > 0) and ((i=Blocks.Count) or (Blocks[i].Tag > ARow)) then
      begin
        n := ACol + (ARow - Blocks[i-1].Tag - 1) * grid.ColCount;
        if n < Blocks[i-1].CacheCharCount then
          Result := Blocks[i-1].CacheCharData[n];
        Exit;
      end;
  end;
end;

function TfrmCharacterMapNew.GetHeaderCellProperties(ACol, ARow: Integer): TUnicodeBlock;
var
  i: Integer;
begin
  for i := 0 to Blocks.Count - 1 do
    if Blocks[i].Tag = ARow then
    begin
      Result := Blocks[i];
      Exit;
    end;

  Result := nil;
end;

function TfrmCharacterMapNew.GetInsertMode: TCharMapInsertMode;
begin
  if mnuPopupInsertModeCode.Checked then
    Result := cmimCode
  else if mnuPopupInsertModeCharacter.Checked then
    Result := cmimCharacter
  else {if mnuPopupInsertModeName.Checked then}
    Result := cmimName;
end;

procedure TfrmCharacterMapNew.gridClick(Sender: TObject);
var
  ur: TUnicodeBlock;
  code: Integer;
  uc: TUnicodeCharacter;
begin
  ur := GetHeaderCellProperties(grid.Col, grid.Row);
  if not Assigned(ur) then
  begin
    code := GetCharFromCell(grid.Col, grid.Row);
    uc := FUnicodeData.FindDataByCode(code);
    if uc.CodeValue = 0 then editCharName.Text := ''
    else editCharName.Text := 'U+'+IntToHex(uc.CodeValue,4)+' '+uc.CharacterName;
  end;

  if FCtrlClickIsDblClick and (GetKeyState(VK_CONTROL) < 0) then   // I4411
    gridDblClick(grid);
end;

procedure TfrmCharacterMapNew.gridContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  ACol, ARow: Integer;
begin
  grid.MouseToCell(MousePos.X, MousePos.Y, ACol, ARow);
  cmdShowFonts.Visible := Assigned(FOnShowFonts);
  cmdShowFontsSplit.Visible := cmdShowFonts.Visible;
  if (ACol < 0) or (ARow < 0) or (GetCharFromCell(ACol, ARow) = 0) then
  begin
    mnuPopupInsertCode.Enabled := False;
    cmdShowFonts.Enabled := False;
  end
  else
  begin
    mnuPopupInsertCode.Enabled := True;
    cmdShowFonts.Enabled := True;
    grid.Col := ACol;
    grid.Row := ARow;
  end;
  with grid.ClientToScreen(MousePos) do
    mnuPopup.Popup(X, Y);
  Handled := True;
end;

procedure TfrmCharacterMapNew.dlgFontApply(Sender: TObject; Wnd: HWND);
begin
  UpdateFont(dlgFont.Font.Name);
  grid.Invalidate;
end;

procedure TfrmCharacterMapNew.editCharNameMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  SetForegroundWindow(GetParentForm(Self).Handle);  // I1462 - not activating correctly on mouse click
  Windows.SetFocus(0);
  Windows.SetFocus(editCharName.Handle);
end;

procedure TfrmCharacterMapNew.editFilterChange(Sender: TObject);
var
  LastChar: Integer;
begin
  if editFilter.Font.Color <> clGray then
  begin
    LastChar := GetCharFromCell(grid.Col, grid.Row);
    FreeAndNil(FSearchBlocks);
    if editFilter.Text <> '' then
      FSearchBlocks := FUnicodeData.GetSearchBlocks(editFilter.Text);
    FormatGrid(LastChar);
    gridClick(grid);
  end;
end;

procedure TfrmCharacterMapNew.editFilterEnter(Sender: TObject);
begin
  FilterEntered;
  if editFilter.Font.Color = clGray then
  begin
    editFilter.Text := '';
    editFilter.Font.Color := clWindowText;
  end;
end;

procedure TfrmCharacterMapNew.editFilterExit(Sender: TObject);
begin
  FFilterText := editFilter.Text;
  if editFilter.Text = '' then
  begin
    editFilter.Font.Color := clGray;
    editFilter.Text := 'Filter by';
  end;
  FilterExited;
end;

procedure TfrmCharacterMapNew.editFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    SetFocusToGrid
  else if Key = VK_RETURN then
    if ssCtrl in Shift
      then cmdFilterClick(nil)
      else SetFocusToGrid
  else
    Exit;
  Key := 0;
end;

procedure TfrmCharacterMapNew.editFilterMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  SetForegroundWindow(GetParentForm(Self).Handle);  // I1462 - not activating correctly on mouse click
  Windows.SetFocus(0);
  Windows.SetFocus(editFilter.Handle);
end;

function TfrmCharacterMapNew.FindCharacter(Code: Integer): Boolean;
var
  i, j: Integer;
  FNewRow, FNewCol: Integer;
begin
  FNewRow := -1; FNewCol := -1;
  for i := 0 to Blocks.Count - 1 do
  begin
    if (Blocks[i].StartChar <= Code) and (Code <= Blocks[i].EndChar) then
    begin
      for j := 0 to Blocks[i].CacheCharCount - 1 do
        if Blocks[i].CacheCharData[j] = Code then
        begin
          FNewRow := j div grid.ColCount + Blocks[i].Tag + 1;
          FNewCol := j mod grid.ColCount;
          Break;
        end;
    end;
  end;
  if FNewRow >= 0 then
  begin
    grid.Row := FNewRow;
    grid.Col := FNewCol;
    Result := True;
  end
  else
    Result := False;
end;

procedure TfrmCharacterMapNew.gridDblClick(Sender: TObject);
begin
  mnuPopupInsertCodeClick(mnuPopupInsertCode);
  if ShouldCancelFocusOnDblClick then
    CancelFocus;  // I1463 - cancel focus on double click
end;

procedure TfrmCharacterMapNew.gridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  ch: WideString;
  ub: TUnicodeBlock;
  uc: Integer;
  RectBmp: TRect;
begin
  if ARow = 0 then
  begin
    // #11589 -- we have a single pixel top row to avoid DIV/0, so format it as
    // 'invisible' by making it look the same as gridlines. It is not selectable
    // so there is no real behaviour change.
    grid.Canvas.Brush.Color := $909090;
    grid.Canvas.Font.Color := $909090;
    grid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, ' ');
    grid.Canvas.FillRect(Rect);
    Exit;
  end;

  ub := GetHeaderCellProperties(ACol, ARow);
  if Assigned(ub) then
  begin
    { Draw a header cell }
    grid.Canvas.Font.Name := 'Tahoma';
    grid.Canvas.Font.Size := 10;
    grid.Canvas.Font.Style := [fsBold];
    grid.Canvas.Font.Color := clWhite;
    Rect.Left := 0; // := grid.CellRect(0, ARow);
    Rect.Right := grid.ColCount * grid.DefaultColWidth;
    SetTextAlign(grid.Canvas.Handle, TA_LEFT or TA_TOP);
    if ARow > 0 then
    begin
      grid.Canvas.Brush.Color := clWindow;
      grid.Canvas.FillRect(Types.Rect(Rect.Left, Rect.Top, Rect.Right, Rect.Top + 8));
      Inc(Rect.Top, 8);
    end;
    grid.Canvas.Brush.Color := $909090;
    grid.Canvas.FillRect(Rect);
    grid.Canvas.TextRect(Rect, Rect.Left + 4, (Rect.Top+Rect.Bottom-grid.Canvas.TextHeight('a')) div 2, ub.Name);
    grid.Canvas.Brush.Color := clWindow;
    grid.Canvas.Font.Style := [];
  end
  else
  begin
    RectBmp := Types.Rect(0, 0, Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
    with FDrawCellBitmap.Canvas do
    begin
      { Draw a character cell }
      if gdSelected in State then
      begin
        if gdFocused in State then
        begin
          Font.Color := clHighlightText;
          Brush.Color := clHighlight;
        end
        else
        begin
          Font.Color := clHighlightText;
          Brush.Color := clGray;
        end;
      end
      else
      begin
        Font.Color := clWindowText;
        Brush.Color := clWindow;
      end;
      FillRect(RectBmp);

      { Retrieve character }

      uc := GetCharFromCell(ACol, ARow);

      if uc = 0 then
      begin
        grid.Canvas.Brush.Color := clWindow;
        grid.Canvas.FillRect(Rect);
        Exit;
      end;

      ch := Uni_UTF32CharToUTF16(uc);

      FDrawChar.SetFontDetails(CharMapFontName, CharMapFontSize);   // I4807
      FDrawChar.Color := Font.Color;
      case DisplayQuality of
        NONANTIALIASED_QUALITY: FDrawChar.DisplayQuality := ctPlain;
        ANTIALIASED_QUALITY: FDrawChar.DisplayQuality := ctAntialias;
        else FDrawChar.DisplayQuality := ctCleartype;
      end;
      FDrawChar.DrawText(Handle, TA_CENTER or TA_TOP, RectBmp.Right div 2, 0, RectBmp, ch);

      { Draw U+nnnn }

      if grid.DefaultColWidth > 35 then
      begin
        Font.Size := 7;
        Font.Name := 'Arial';
        SetTextAlign(Handle, TA_CENTER or TA_TOP);
        TextOut(RectBmp.Right div 2, RectBmp.Bottom - TextHeight('a'), 'U+'+IntToHex(uc,4));
      end;

      { Draw grid }

      Pen.Color := $c0c0c0;
      MoveTo(RectBmp.Right-1, 0);
      LineTo(RectBmp.Right-1, RectBmp.Bottom);
      MoveTo(0, RectBmp.Bottom-1);
      LineTo(RectBmp.Right, RectBmp.Bottom-1);
    end;

    grid.Canvas.Draw(Rect.Left, Rect.Top, FDrawCellBitmap);
  end;
end;

procedure TfrmCharacterMapNew.CancelFocus;  // I1463 - cancel focus on double click
begin
  if Assigned(FOnCancelFocus) then
    FOnCancelFocus(Self);
end;

procedure TfrmCharacterMapNew.gridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    mnuPopupInsertCodeClick(mnuPopupInsertCode)
  else if Key = VK_ESCAPE then
  begin
    CancelFocus;  // I1463 - cancel focus on double click or escape
  end
  else if Key = 107 { Numpad + } then
  begin
    if tbSize.Position < tbSize.Max then
      tbSize.Position := tbSize.Position + tbSize.PageSize;
  end
  else if Key = 109 { Numpad - } then
  begin
    if tbSize.Position > tbSize.Min then
      tbSize.Position := tbSize.Position - tbSize.PageSize;
  end
  else if (Key = Ord('F')) and (ssCtrl in Shift) and (ssShift in Shift) then
    mnuPopupFontClick(mnuPopupFont)
  else if (Key = Ord('F')) and (ssCtrl in Shift) then
    SetFocusToFilter
  else if (Key = Ord('G')) and (ssCtrl in Shift) then
    mnuGotoClick(mnuGoto)
  else if (Key = VK_UP) then
    if ssCtrl in Shift
      then MoveCell(mvUpBlock)
      else MoveCell(mvUp)
  else if (Key = VK_DOWN) then
    if ssCtrl in Shift
      then MoveCell(mvDownBlock)
      else MoveCell(mvDown)
  else if (Key = VK_RIGHT) then
    MoveCell(mvRight)
  else if (Key = VK_LEFT) then
    MoveCell(mvLeft)
  else if (Key = VK_HOME) then
    if ssCtrl in Shift
      then MoveCell(mvTop)
      else MoveCell(mvStartOfRow)
  else if (Key = VK_END) then
    if ssCtrl in Shift
      then MoveCell(mvBottom)
      else MoveCell(mvEndOfRow)
  else if (Key = VK_PRIOR) then
    MoveCell(mvPageUp)
  else if (Key = VK_NEXT) then
    MoveCell(mvPageDown)
  else
    Exit;
  Key := 0;
end;

procedure TfrmCharacterMapNew.MoveCell(Direction: TCharMapDirection);
      function CanSelect(ACol, ARow: Integer): Boolean;
      begin
        {if GetHeaderCellProperties(ACol, ARow) = nil) then
          Result := False
        else
        begin}
        Result :=
          (ACol >= 0) and (ARow >= 0) and (ACol < grid.ColCount) and (ARow < grid.RowCount) and
          (GetCharFromCell(ACol, ARow) <> 0);
      end;
var
  ACol: Integer;
  ARow: Integer;
begin
  ACol := grid.Col;
  ARow := grid.Row;
  case Direction of
    mvUp:
      begin
        Dec(ARow);
        while (ARow > 0) and (GetHeaderCellProperties(ACol, ARow) <> nil) do Dec(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvDown:
      begin
        Inc(ARow);
        while (ARow < grid.RowCount - 1) and (GetHeaderCellProperties(ACol, ARow) <> nil) do Inc(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvLeft:
      repeat
        Dec(ACol);
        if ACol < 0 then
        begin
          Dec(ARow); ACol := grid.ColCount - 1;
        end;
      until (ARow < 0) or CanSelect(ACol, ARow);
    mvRight:
      repeat
        Inc(ACol);
        if ACol >= grid.ColCount then
        begin
          Inc(ARow); ACol := 0;
        end;
      until (ARow >= grid.RowCount) or CanSelect(ACol, ARow);
    mvTop:
      begin
        ACol := 0; ARow := 1;
      end;
    mvBottom:
      begin
        ACol := grid.ColCount - 1;
        ARow := grid.RowCount - 1;
        while (ARow >= 0) and not CanSelect(ACol, ARow) do
        begin
          Dec(ACol);
          if ACol < 0 then
          begin
            Dec(ARow); ACol := grid.ColCount - 1;
          end;
        end;
      end;
    mvStartOfRow:
      ACol := 0;
    mvEndOfRow:
      begin
        ACol := grid.ColCount - 1;
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvPageUp:
      begin
        if ARow > grid.TopRow
          then ARow := grid.TopRow
          else ARow := ARow - grid.VisibleRowCount;

        if ARow < 1 then ARow := 1;
        while (ARow > 0) and not CanSelect(0, ARow) do Dec(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvPageDown:
      begin
        if ARow < grid.TopRow + grid.VisibleRowCount - 1
          then ARow := grid.TopRow + grid.VisibleRowCount - 1
          else ARow := ARow + grid.VisibleRowCount;

        if ARow >= grid.RowCount then ARow := grid.RowCount - 1;
        while (ARow < grid.RowCount - 1) and not CanSelect(0, ARow) do Inc(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvUpBlock:
      begin
        if (ARow > 0) and (GetHeaderCellProperties(ACol, ARow - 1) <> nil) then Dec(ARow);
        while (ARow > 0) and (GetHeaderCellProperties(ACol, ARow - 1) = nil) do Dec(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
    mvDownBlock:
      begin
        while (ARow < grid.RowCount - 1) and (GetHeaderCellProperties(ACol, ARow) = nil) do Inc(ARow);
        if (ARow < grid.RowCount - 1) then Inc(ARow);
        while (ACol > 0) and not CanSelect(ACol, ARow) do Dec(ACol);
      end;
  end;
  if CanSelect(ACol, ARow) then
  begin
    grid.Row := ARow;
    grid.Col := ACol;
    if grid.TopRow = 1 then grid.TopRow := 0;
  end;
end;

procedure TfrmCharacterMapNew.gridMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  SetForegroundWindow(GetParentForm(Self).Handle);  // I1462 - not activating correctly on mouse click
  Windows.SetFocus(0);
  Windows.SetFocus(grid.Handle);
end;

procedure TfrmCharacterMapNew.gridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DragStartPos := Point(X, Y);
end;

procedure TfrmCharacterMapNew.gridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    if not FCharMapDrag or (GetCharFromCell(grid.Col, grid.Row) = 0) then Exit;
    if not Mouse.IsDragging and ((Abs(DragStartPos.X - X) >= 4) or (Abs(DragStartPos.Y - Y) >= 4)) then
      grid.BeginDrag(True);
  end;
end;

procedure TfrmCharacterMapNew.gridMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    if tbSize.Position < tbSize.Max then
      tbSize.Position := tbSize.Position + tbSize.PageSize;
  end;
end;

procedure TfrmCharacterMapNew.gridMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    if tbSize.Position > tbSize.Min then
      tbSize.Position := tbSize.Position - tbSize.PageSize;
  end;
end;

procedure TfrmCharacterMapNew.gridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (ARow > 0) or (grid.RowCount = 1);
end;

function TfrmCharacterMapNew.GetDragObject: TCharacterDragObject;
var
  ch: Integer;
  uc: TUnicodeCharacter;
begin
  Result := nil;

  ch := GetCharFromCell(grid.Col, grid.Row);
  if ch = 0 then Exit;

  Result := TCharacterDragObject.Create;
  with Result do
  begin
    SetDragCursorOptions(InsertMode, FFontName);
    Text[cmimCode] := 'U+'+IntToHex(ch, 4);
    Text[cmimCharacter] := Uni_UTF32CharToUTF16(ch);
    uc := FUnicodeData.FindDataByCode(ch);
    if uc.CodeValue <> 0 then Text[cmimName] := '$'+uc.CharacterName;
    Text[cmimText] := Text[InsertMode];
  end;
end;

procedure TfrmCharacterMapNew.gridStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  ACol, ARow: Integer;
begin
  grid.MouseToCell(DragStartPos.X, DragStartPos.Y, ACol, ARow);  // I2573
  if (ACol >= 0) and (ARow >= 0) then
  begin
    grid.Col := ACol;
    grid.Row := ARow;
    DragObject := GetDragObject;
  end;
end;

procedure TfrmCharacterMapNew.gridTopLeftChanged(Sender: TObject);
begin
  UpdateSectionLabel;
end;

procedure TfrmCharacterMapNew.LoadSettings(var SelectedCharacter: Integer);
begin
  SelectedCharacter := 0;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(FSettingsKey) then
    begin
      if ValueExists(SRegValue_IDECharacterMap_CellSize)   then tbSize.Position   := ReadInteger(SRegValue_IDECharacterMap_CellSize);
      if ValueExists(SRegValue_IDECharacterMap_Filter)     then FilterText        := ReadString(SRegValue_IDECharacterMap_Filter);
      if ValueExists(SRegValue_IDECharacterMap_Font)       then FFontName         := ReadString(SRegValue_IDECharacterMap_Font);
      if ValueExists(SRegValue_IDECharacterMap_Quality)    then DisplayQuality    := ReadInteger(SRegValue_IDECharacterMap_Quality);
      if ValueExists(SRegValue_IDECharacterMap_InsertMode) then InsertMode        := TCharMapInsertMode(ReadInteger(SRegValue_IDECharacterMap_InsertMode));
      if ValueExists(SRegValue_IDECharacterMap_Character)  then SelectedCharacter := ReadInteger(SRegValue_IDECharacterMap_Character);
    end;
  finally
    Free;
  end;
end;

procedure TfrmCharacterMapNew.UpdateFont(AFontName: WideString);
begin
  FFontName := AFontName;
  FUnicodeData.FontName := FFontName;
  if FilterText <> '' then editFilterChange(editFilter);
end;

procedure TfrmCharacterMapNew.UpdateSectionLabel;
var
  i: Integer;
begin
  if Assigned(FSearchBlocks) then
    with Canvas do
    begin
      Font := lblSection.Font;
      if Canvas.TextWidth(FSearchBlocks.Name) > panHeader.Width - lblSection.Left
        then lblSection.Caption := FSearchBlocks.ShortName
        else lblSection.Caption := FSearchBlocks.Name;
    end
  else
  begin
    for i := 1 to Blocks.Count - 1 do
      if Blocks[i].Tag > grid.TopRow-1 then
      begin
        lblSection.Caption := Blocks[i-1].Name;
        Exit;
      end;
    lblSection.Caption := '';
  end;
end;

function TfrmCharacterMapNew.GetDisplayQuality: Integer;
begin
  if mnuPopupDisplayQualityPlain.Checked then Result := NONANTIALIASED_QUALITY
  else if mnuPopupDisplayQualityAntialiased.Checked then Result := ANTIALIASED_QUALITY
  else Result := 5; //CLEARTYPE_QUALITY;
end;

function TfrmCharacterMapNew.GetFilterText: string;
begin
  if editFilter.Focused
    then Result := editFilter.Text
    else Result := FFilterText;
  //if editFilter.Font.Color = clGray then Result := ''
  //else Result := editFilter.Text;
end;

function TfrmCharacterMapNew.GetFontSize: Integer;   // I4807
begin
  if grid.DefaultColWidth < 36
    then Result := grid.DefaultRowHeight
    else Result := grid.DefaultRowHeight - 12;
end;

procedure TfrmCharacterMapNew.InsertCode(Control: TWinControl; DragObject: TCharacterDragObject);
begin
  if Assigned(FOnInsertCode) then
    FOnInsertCode(Self, Control, DragObject);
end;

function TfrmCharacterMapNew.CanInsertCode(Control: TWinControl): Boolean;
begin
  Result := False;
  if Assigned(FOnCanInsertCode) then
    FOnCanInsertCode(Self, Control, Result);
end;

procedure TfrmCharacterMapNew.cmdFilterClick(Sender: TObject);
begin
  DialogOpening;
  with TfrmCharacterMapFilter.Create(Self) do
  try
    if Self.editFilter.Font.Color <> clGray
      then Filter := Self.editFilter.Text
      else Filter := '';
    if ShowModal = mrOk then
    begin
      Self.editFilter.Text := Filter;
      Self.editFilter.Font.Color := clWindowText;
      SetFocusToFilter;
      Self.editFilterChange(Self.editFilter);
    end;
  finally
    Free;
  end;
  DialogClosing;
end;

procedure TfrmCharacterMapNew.cmdShowFontsClick(Sender: TObject);
begin
  if Assigned(FOnShowFonts) then
    FOnShowFonts(Self);
end;

constructor TfrmCharacterMapNew.Create(AOwner: TComponent; ASettingsKey: WideString);
begin
  FSettingsKey := ASettingsKey;
  inherited Create(AOwner);
end;

procedure TfrmCharacterMapNew.DialogOpening;
begin
  if Assigned(FOnDialogOpening) then
    FOnDialogOpening(Self);
end;

procedure TfrmCharacterMapNew.DialogClosing;
begin
  if Assigned(FOnDialogClosing) then
    FOnDialogClosing(Self);
end;

procedure TfrmCharacterMapNew.mnuPopupInsertCodeClick(Sender: TObject);
var
  DragObject: TCharacterDragObject;
begin
  if not CanInsertCode(FLastActiveControl) then Exit;
  DragObject := GetDragObject;
  if not Assigned(DragObject) then Exit;
  try
    InsertCode(FLastActiveControl, DragObject);
  finally
    DragObject.Free;
  end;
end;

procedure TfrmCharacterMapNew.mnuPopupInsertModeItemClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
end;

procedure TfrmCharacterMapNew.mnuPopupPopup(Sender: TObject);
begin
  case InsertMode of
    cmimName: mnuPopupInsertCode.Caption := '&Insert Character Name';
    cmimCharacter: mnuPopupInsertCode.Caption := '&Insert Character';
    cmimCode: mnuPopupInsertCode.Caption := '&Insert Character Code';
  end;
  mnuPopupInsertCode.Enabled := (FIgnoreLastActiveControl or Assigned(FLastActiveControl)) and CanInsertCode(FLastActiveControl);
end;

procedure TfrmCharacterMapNew.panHeaderResize(Sender: TObject);
begin
  if True then
end;

procedure TfrmCharacterMapNew.Reload;
begin
  editFilterChange(nil);
  FormatGrid(-1);
end;

procedure TfrmCharacterMapNew.SaveSettings;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(FSettingsKey, True) then
    begin
      WriteInteger(SRegValue_IDECharacterMap_CellSize, tbSize.Position);
      WriteString(SRegValue_IDECharacterMap_Filter, FilterText);
      WriteString(SRegValue_IDECharacterMap_Font, FFontName);
      WriteInteger(SRegValue_IDECharacterMap_Quality, DisplayQuality);
      WriteInteger(SRegValue_IDECharacterMap_InsertMode, Integer(InsertMode));
      WriteInteger(SRegValue_IDECharacterMap_Character, GetCharFromCell(grid.Col, grid.Row));
    end;
  finally
    Free;
  end;
end;

procedure TfrmCharacterMapNew.ScreenActiveControlChange(Sender: TObject);
begin
  if not ContainsControl(Screen.ActiveControl) then
    FLastActiveControl := Screen.ActiveControl;
end;

procedure TfrmCharacterMapNew.AlignCharName;
begin
  if FCharNameNextToFilter then
  begin
    panName.Left := ClientWidth div 3;
    panName.Width := tbSize.Left - panName.Left;
    editFilter.Width := panName.Left - editFilter.Left - 1;
    cmdFilter.Left := editFilter.Left + editFilter.Width - cmdFilter.Width - 2;
  end;
end;

procedure TfrmCharacterMapNew.SetCharNameNextToFilter(const Value: Boolean);
begin
  if FCharNameNextToFilter <> Value then
  begin
    panBottom.AutoSize := False;
    FCharNameNextToFilter := Value;
    if FCharNameNextToFilter then
    begin
      editFilter.Anchors := [akLeft, akTop];
      cmdFilter.Anchors := [akLeft, akTop];

      tbSize.Left := ClientWidth - tbSize.Width;

      editFilter.Top := 0;
      cmdFilter.Top := 2;
      tbSize.Top := 0;
      panName.Top := 2;
      AlignCharName;
    end
    else
    begin
      panName.Left := 2;
      panName.Width := ClientWidth - 4;

      tbSize.Left := ClientWidth - tbSize.Width;

      panName.Top := 0;
      editFilter.Top := 18;
      cmdFilter.Top := 20;
      tbSize.Top := 18;

      editFilter.Width := tbSize.Left - editFilter.Left - 2;
      editFilter.Anchors := [akLeft, akRight, akTop];
      cmdFilter.Anchors := [akRight, akTop];
      cmdFilter.Left := editFilter.Left + editFilter.Width - cmdFilter.Width - 2;
    end;
    panBottom.AutoSize := True;
  end;
end;

procedure TfrmCharacterMapNew.SetDisplayQuality(const Value: Integer);
begin
  case Value of
    NONANTIALIASED_QUALITY: mnuPopupDisplayQualityPlain.Checked := True;
    ANTIALIASED_QUALITY:    mnuPopupDisplayQualityAntialiased.Checked := True;
    5:                      mnuPopupDisplayQualityCleartype.Checked := True;
  end;
end;

procedure TfrmCharacterMapNew.SetFilterText(Value: string);
begin
  FFilterText := Value;
  if (Value = '') and not editFilter.Focused then
  begin
    editFilter.Font.Color := clGray;
    editFilter.Text := 'Filter by';
  end
  else
  begin
    editFilter.Font.Color := clWindowText;
    editFilter.Text := Value;
  end;
end;

procedure TfrmCharacterMapNew.SetInsertMode(const Value: TCharMapInsertMode);
begin
  case Value of
    cmimCode: mnuPopupInsertModeCode.Checked := True;
    cmimCharacter: mnuPopupInsertModeCharacter.Checked := True;
    cmimName: mnuPopupInsertModeName.Checked := True;
  end;
end;

procedure TfrmCharacterMapNew.mnuGotoClick(Sender: TObject);
var
  s: string;
  v: Integer;
begin
  DialogOpening;
  try
    if InputQuery('Enter Unicode character value or name to find', 'Go To', s) then
    begin

      SetFocus;

      if SameStr(Copy(s,1,2), 'U+') then
      begin
        if ExtNumToInt(s) = 0 then
        begin
          ShowMessage('The code value '+s+' is not valid.');
          Exit;
        end;
      end
      else if ExtNumToInt('U+'+s) = 0 then
        s := String_AtoU('$'+TUnicodeDataFormat.CleanCharacterName(String_UtoA(s)))  // I3310
      else
        s := 'U+'+s;

      if not FindCharacter(s, v) then
      begin
        ShowMessage('The character '+s+' was not found.');
        SetFocus;
      end;
    end;
  finally
    DialogClosing;
  end;
end;

procedure TfrmCharacterMapNew.FilterEntered;
begin
  if Assigned(FOnFilterEntered) then
    FOnFilterEntered(Self);
end;

procedure TfrmCharacterMapNew.FilterExited;
begin
  if Assigned(FOnFilterExited) then
    FOnFilterExited(Self);
end;

function TfrmCharacterMapNew.FindCharacter(code: WideString; var CodeValue: Integer): Boolean;
var
  uc: TUnicodeCharacter;
begin
  if Copy(code, 1, 1) = '$'
    then uc := FUnicodeData.FindDataByName(Copy(code, 2, Length(code)))
    else uc := FUnicodeData.FindDataByCode(ExtNumToInt(code));

  CodeValue := uc.CodeValue;

  if uc.CodeValue > 0
    then Result := FindCharacter(uc.CodeValue)
    else Result := False;
end;

procedure TfrmCharacterMapNew.mnuPopupDisplayQualityClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  grid.Invalidate;
end;

procedure TfrmCharacterMapNew.mnuPopupFilterClick(Sender: TObject);
begin
  SetFocusToFilter;
end;

procedure TfrmCharacterMapNew.mnuPopupFontClick(Sender: TObject);
var
  AFontName, FOldFontName: TFontName;
begin
  DialogOpening;
  dlgFont.Font.Name := FFontName;
  FOldFontName := FFontName;

  if dlgFont.Execute
    then AFontName := dlgFont.Font.Name
    else AFontName := FOldFontName;

  UpdateFont(AFontName);
  grid.Invalidate;
  SetFocusToGrid;
  DialogClosing;
end;

{
 #7639: It appears that components within a docking form are unable to be
 focused in some, rare contexts. We don't want to crash, and the end
 result of not focusing  is not really all that tragic, so let's just
 mask the exception.
}

procedure TfrmCharacterMapNew.SetFocusToGrid;
begin
  try
    grid.SetFocus;
  except
    on E:EInvalidOperation do ;
  end;
end;

procedure TfrmCharacterMapNew.SetFocusToFilter;
begin
  try
    editFilter.SetFocus;
  except
    on E:EInvalidOperation do ;
  end;
end;

end.
