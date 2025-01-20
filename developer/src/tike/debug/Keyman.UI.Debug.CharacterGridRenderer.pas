unit Keyman.UI.Debug.CharacterGridRenderer;

interface

uses
  debugdeadkeys,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.Grids;

type
  TCharacterGridRenderer = class sealed
  public
    const
      CURSOR_COL_WIDTH = 5;

      CELL_DEADKEY = 1;
      CELL_CURSOR = 2;
      CELL_SELECTED = 4;

    class procedure Fill(grid: TStringGrid; const text: string;
      deadkeys: TDebugDeadkeyInfoList;
      SelStart, SelLength, SelAnchor: Integer;
      DeadkeysAreCalledMarkers: Boolean = False); static;
    class procedure Render(grid: TStringGrid; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState; CharFont: TFont); static;
    class procedure Size(grid: TStringGrid; CharFont: TFont); static;
  end;


implementation

uses
  Unicode;

class procedure TCharacterGridRenderer.Fill(grid: TStringGrid;
  const text: string; deadkeys: TDebugDeadkeyInfoList;
  SelStart, SelLength, SelAnchor: Integer;
  DeadkeysAreCalledMarkers: Boolean);
type
  TCellType = (ctChar, ctDeadkey);
  TCell = record
    case CellType: TCellType of
      ctChar: (ch: Integer);  // UTF-32
      ctDeadkey: (dk: TDeadKeyInfo);
    end;
  function Cut(Start, Finish: Integer): TArray<TCell>;
  var
    x, n: Integer;
  begin
    SetLength(Result, Finish-Start);
    x := Start; n := 0;
    while x < Finish do
    begin
      if (x < Finish - 1) and Uni_IsSurrogate1(text[x+1]) and Uni_IsSurrogate2(text[x+2]) then
      begin
        Result[n].CellType := ctChar;
        Result[n].ch := Uni_SurrogateToUTF32(text[x+1], text[x+2]);
        Inc(x);
      end
      else if text[x+1] = #$FFFC then
      begin
        Result[n].CellType := ctDeadkey;
        Result[n].dk := deadkeys.GetFromPosition(x);
      end
      else
      begin
        Result[n].CellType := ctChar;
        Result[n].ch := Ord(text[x+1]);
      end;
      Inc(n);
      Inc(x);
    end;
    SetLength(Result, n);
  end;

  procedure FillGrid(const cells: TArray<TCell>; flags: Integer; var x: Integer);
  var
    cell: TCell;
  begin
    for cell in cells do
    begin
      if cell.CellType = ctDeadkey then
      begin
        grid.Objects[x, 0] := Pointer(flags or CELL_DEADKEY);
        //Assert(Assigned(cell.dk));
        if not Assigned(cell.dk)
          then grid.Cells[x, 0] := '???'
          else grid.Cells[x, 0] := cell.dk.Deadkey.Name;
        if DeadkeysAreCalledMarkers
          then grid.Cells[x, 1] := 'Marker'
          else grid.Cells[x, 1] := 'Deadkey';
      end
      else
      begin
        grid.Objects[x, 0] := Pointer(flags);
        grid.Cells[x, 0] := Uni_UTF32CharToUTF16(cell.ch);
        grid.Cells[x, 1] := 'U+'+IntToHex(cell.ch, 4);
      end;
      grid.ColWidths[x] := grid.DefaultColWidth;
      Inc(x);
    end;
  end;

  procedure FillGridCursor(var x: Integer);
  begin
    grid.Objects[x,0] := Pointer(CELL_CURSOR);
    grid.Cells[x,0] := '|';
    grid.Cells[x,1] := '|';
    grid.ColWidths[x] := CURSOR_COL_WIDTH;
    Inc(x);
  end;

var
  MaxCols: Integer;
  FirstSel, LastSel: Integer;
  Before, Selection, After: TArray<TCell>;
  X: Integer;
  LB: Integer;
  LS: Integer;
  LA: Integer;
begin
  MaxCols := (grid.ClientWidth - CURSOR_COL_WIDTH - 1) div (grid.DefaultColWidth + 1);

  FirstSel := SelStart;
  LastSel := SelStart + SelLength;

  // We have three runs: before selection, in selection, after selection
  // There are three possible modes:
  // * No selection
  // * Selection, cursor at beginning
  // * Selection, cursor at end
  //
  // We also have a cursor cell (|), which is between two of the runs
  // When there are too many cells to fit, we want to show:
  // * No selection:
  //    <before[n..before.length]>|<after[1]>
  // * With selection, cursor at beginning:
  //    <before[n..before.length]>|<selection[1]>
  // * With selection, cursor at end:
  //    <before[n..before.length]><selection[m..selection.length]>|<after[1]>

  Before := Cut(0, FirstSel);
  Selection := Cut(FirstSel, LastSel);
  After := Cut(LastSel, Text.Length);

  // Trim arrays

  LB := Length(Before);
  LS := Length(Selection);
  LA := Length(After);
  while LB + LS + LA > MaxCols do
  begin
    if LA > 1 then
      Dec(LA)
    else if LB > 0 then
      Dec(LB)
    else if LS > 1 then
      Dec(LS);
  end;

  if LB < Length(Before) then Before := Copy(Before, Length(Before)-LB, LB);
  if LS < Length(Selection) then Selection := Copy(Selection, Length(Selection)-LS, LS);
  SetLength(After, LA);

  // Fill the grid

  grid.ColCount := LB + LS + LA + 1; // include cursor cell

  X := 0;
  FillGrid(Before, 0, X);
  if SelAnchor = SelStart then
    FillGridCursor(X);
  FillGrid(Selection, CELL_SELECTED, X);
  if SelAnchor <> SelStart then
    FillGridCursor(X);
  FillGrid(After, 0, X);

(*
  J := 0; I := SelStart + SelLength; // I is 1-based Delphi string index
  while (J < MaxCols) and (I > SelStart) do
  begin
    if (I > 1) and Uni_IsSurrogate2(s[I]) and Uni_IsSurrogate1(s[I-1]) then
      Dec(I);
    Inc(J); Dec(I);
  end;

  if J = 0 then
  begin
    sgChars.ColCount := 1;
    sgChars.Objects[0,0] := Pointer(0);
    sgChars.Cells[0,0] := '';
    sgChars.Cells[0,1] := '';
    Exit;
  end
  else
    sgChars.ColCount := J + 1;

  Inc(I);
  J := 0;
  while J < sgChars.ColCount do
  begin
    if I = memo.SelStart + 1 then
    begin
      sgChars.Objects[J, 0] := Pointer(2);
      sgChars.Cells[J, 0] := '|';
      sgChars.Cells[J, 1] := 'Cursor';
      Inc(J);
    end;
    if Ord(S[I]) = $FFFC then
    begin
      sgChars.Objects[J, 0] := Pointer(1);
      sgChars.Cells[J, 0] := '???';
      for K := 0 to FDeadkeys.Count-1 do
        if FDeadkeys[K].Position = I-1 then
        begin
          sgChars.Cells[J, 0] := FDeadkeys[K].Deadkey.Name;
          break;
        end;
      sgChars.Cells[J, 1] := 'Deadkey';
    end
    else if Uni_IsSurrogate1(s[I]) and (I < Length(s)) and Uni_IsSurrogate2(s[I+1]) then
    begin
      sgChars.Objects[J, 0] := Pointer(0);
      sgChars.Cells[J, 0] := Copy(s, I ,2);
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Uni_SurrogateToUTF32(s[I], s[I+1]), 5);
      Inc(I);
    end
    else
    begin
      sgChars.Objects[J, 0] := Pointer(0);
      sgChars.Cells[J, 0] := s[I];
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Ord(s[i]), 4);
    end;
    Inc(J); Inc(I);
  end;
*)
end;

class procedure TCharacterGridRenderer.Render(grid: TStringGrid; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState; CharFont: TFont);
var
  flags: Integer;
begin
  flags := Integer(grid.Objects[ACol, 0]);
  if (ARow = 0) and ((flags and CELL_DEADKEY) = 0)
    then grid.Canvas.Font := CharFont
    else grid.Canvas.Font := grid.Font;

  grid.Canvas.Font.Color := clWindowText;

  if (flags and CELL_SELECTED) <> 0 then
  begin
    grid.Canvas.Brush.Color := clHighlight;
    if (flags and CELL_DEADKEY) <> 0
      then grid.Canvas.Font.Color := clYellow
      else grid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    grid.Canvas.Brush.Color := clWindow;
    if (flags and CELL_DEADKEY) <> 0 then
    begin
      grid.Canvas.Font.Color := clRed;
    end
    else if (flags and CELL_CURSOR) <> 0 then
    begin
      grid.Canvas.Font.Color := clGreen;
      grid.Canvas.Brush.Color := clGreen;
    end;
  end;

  grid.Canvas.TextRect(Rect,
    (Rect.Right + Rect.Left - grid.Canvas.TextWidth(grid.Cells[ACol, ARow])) div 2,
    (Rect.Top + Rect.Bottom - grid.Canvas.TextHeight(grid.Cells[ACol, ARow])) div 2,
    grid.Cells[ACol, ARow]);
end;

class procedure TCharacterGridRenderer.Size(grid: TStringGrid; CharFont: TFont);
begin
  grid.Canvas.Font := CharFont;
  grid.RowHeights[0] := grid.Canvas.TextHeight('A') + 4;
  grid.Canvas.Font := grid.Font;
  grid.RowHeights[1] := grid.Canvas.TextHeight('A') + 4;
  grid.Height := grid.RowHeights[0] + grid.RowHeights[1] + 4;
end;

end.
