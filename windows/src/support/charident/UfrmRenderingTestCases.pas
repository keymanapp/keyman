(*
  Name:             UfrmRenderingTestCases
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      8 Jun 2012

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          
*)
unit UfrmRenderingTestCases;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntClasses, TntStdCtrls, Grids, TntGrids, utilcheckfontchars, ComCtrls, Contnrs;

type
  TRenderingException = class
    FontName: string;
    UnicodeString: WideString;
    Note: WideString;
  end;

  TRenderingExceptionList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRenderingException;
    procedure SetItem(Index: Integer; const Value: TRenderingException);
  published
  public
    property Items[Index: Integer]: TRenderingException read GetItem write SetItem; default;
  end;

  TfrmRenderingTestCases = class(TForm)
    gridResults: TStringGrid;
    memoSource: TMemo;
    cmdFindExamples: TButton;
    lblStatus: TLabel;
    tbFontSize: TTrackBar;
    lblFontSizeCaption: TLabel;
    lblFontSize: TLabel;
    cmdNew: TButton;
    cmdOpen: TButton;
    cmdSave: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    cmdExport: TButton;
    dlgSaveHTML: TSaveDialog;
    procedure cmdFindExamplesClick(Sender: TObject);
    procedure gridResultsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure tbFontSizeChange(Sender: TObject);
    procedure cmdNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmdOpenClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure gridResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure gridResultsMouseLeave(Sender: TObject);
    procedure gridResultsExit(Sender: TObject);
    procedure cmdExportClick(Sender: TObject);
  private
    FCheckFontsThread: TCheckFontsThread;
    FNewChars: WideString;
    FExceptions: TRenderingExceptionList;
    FLastCol: Integer;
    FLastRow: Integer;
    FHintWindow: THintWindow;
    FLockUpdateLevel: Integer;
    procedure ParseTestCases(Source: WideString);
    procedure ParseTestCase(TestCase: WideString);
    procedure GetBaseFontList(Source: WideString);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure ResizeGrid;
    function GetCellSize(ACol, ARow: Integer): TSize;
    procedure FillGrid;
    procedure RenderCell(Canvas: TCanvas; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    function GetCellText(ACol, ARow: Integer): WideString;
    function GetFontVersion(FontName: WideString): WideString;
    procedure LockUpdates;
    procedure UnlockUpdates;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRenderingTestCases: TfrmRenderingTestCases;

implementation

uses
  Math,
  PngImage,
  TntDialogs,
  TntGraphics,
  TntSystem,
  TTInfo,
  Unicode,
  utilxml,
  WideStrings,
  xmlintf,
  xmldoc;

{$R *.dfm}

function TfrmRenderingTestCases.GetFontVersion(FontName: WideString): WideString;
begin
  with TTTInfo.Create(FontName, [tfNames, tfNameIsFontName]) do
  try
    Result := FontVersion;
  finally
    Free;
  end;
end;

procedure TfrmRenderingTestCases.cmdExportClick(Sender: TObject);
var
  I: Integer;
  FBitmap: TPngObject;
  X: Integer;
  FRect: TRect;
  FX: Integer;
  FY: Integer;
  Y: Integer;
  e: TRenderingException;
  FClassName: WideString;
  ss: TStringStream;
  FHint: WideString;
begin
  if dlgSaveHTML.Execute then
  begin
    X := 0; Y := 0;
    for I := 1 to gridResults.ColCount - 1 do
      Inc(X, gridResults.ColWidths[I]);
    for I := 2 to gridResults.RowCount - 1 do
      Inc(Y, gridResults.RowHeights[I]);
    FBitmap := TPngObject.CreateBlank(COLOR_GRAYSCALE, 8, X, Y);
    FX := 0;
    for X := 1 to gridResults.ColCount - 1 do
    begin
      FY := 0;
      for Y := 2 to gridResults.RowCount - 1 do
      begin
        FRect := Rect(FX, FY, FX + gridResults.ColWidths[X], FY + gridResults.RowHeights[Y]);
        RenderCell(FBitmap.Canvas, X, Y, FRect, []);
        Inc(FY, gridResults.RowHeights[Y]);
      end;
      Inc(FX, gridResults.ColWidths[X]);
    end;
    FBitmap.SaveToFile(ChangeFileExt(dlgSaveHTML.FileName, '.png'));
    with TTntStringList.Create do
    try
      Add('<!DOCTYPE HTML>');
      Add('<html>');
      Add('  <head>');
      Add('    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />');
      Add('    <title>Rendering Test Cases</title>');
      Add('    <style>');
      Add('      table { border-collapse: collapse; }');
      Add('      #text { border: 4px solid #cccccc; border-radius: 6px; font-size: 16pt; margin: 16px 8px; padding: 8px; }');
      Add('      th, td { border: solid 1px #cccccc; font-family: Tahoma; }');
      Add('      tr.subinfo th { text-align: right; }');
      Add('      tr.subinfo td { font-weight: normal; }');
      Add('      thead td, thead th { background: #eeeeee; }');
      Add('      tbody th { font-weight: normal; padding: 0 4px; text-align: right }');
      Add('      tbody td .image { background: url("'+ChangeFileExt(ExtractFileName(dlgSaveHTML.FileName), '.png')+'") no-repeat; }');
      Add('      tbody td .image, tbody td .text { display: none; float: left; margin-left: 4px }');
      Add('      tbody td .hint { display: none; }');
      Add('      body.showText tbody td .text { display: block; }');
      Add('      body.showImages tbody td .image { display: block; }');
      for X := 1 to gridResults.ColCount - 1 do
        Add(Format('.font%d { font-family: "%s"; font-size: %dpt }', [X, gridResults.Cells[X, 0], tbFontSize.Position]));
      
      Add('    </style>');
      Add('  	<script>');
      Add('	  function UpdateVisibility()');
      Add('	  {');
      Add('		var s = "";');
      Add('		if(document.getElementById("chkText").checked) s += "showText ";');
      Add('		if(document.getElementById("chkImages").checked) s += "showImages ";');
      Add('	    document.body.className = s;');
      Add('	  }');
      Add('    </script>');
      Add('  </head>');
      Add('  <body class="showText showImages" onload="UpdateVisibility()">');
      Add('  <h1>Rendering Test Cases</h1>');
      Add('  <input type="checkbox" id="chkText" checked="checked" onclick="UpdateVisibility()" /><label for="chkText">Show text</label>');
      Add('  <input type="checkbox" id="chkImages" checked="checked" onclick="UpdateVisibility()" /><label for="chkImages">Show images</label>');
      Add('  <div id="text">'+XMLEncode(memoSource.Text)+'</div>');
      Add('  <table id="results">');
      Add('    <thead>');
      Add('      <tr>');
      Add('        <th>Unicode String</th>');
      for i := 1 to gridResults.ColCount - 1 do
        Add('        <th>'+XMLEncode(gridResults.Cells[i, 0])+'</th>');
      Add('      </tr>');
      Add('      <tr class="subinfo">');
      Add('        <th>Coverage:');
      for i := 1 to gridResults.ColCount - 1 do
        Add('        <td>'+XMLEncode(gridResults.Cells[i, 1])+'</td>');
      Add('      </tr>');
      Add('      <tr class="subinfo">');
      Add('        <th>Font Versions:');
      for i := 1 to gridResults.ColCount - 1 do
        Add('        <td>'+XMLEncode(GetFontVersion(gridResults.Cells[i, 0]))+'</td>');
      Add('      </tr>');
      Add('    </thead>');
      Add('    <tbody>');
      FY := 0; //-FBitmap.Height;
      for Y := 2 to gridResults.RowCount - 1 do
      begin
        Add('      <tr>');
        Add('      <th>'+XMLEncode(GetCellText(0, Y))+'</th>');
        FX := 0; //-FBitmap.Width;
        for X := 1 to gridResults.ColCount - 1 do
        begin
          e := gridResults.Objects[X, Y] as TRenderingException;
          if Assigned(e) then FClassName := ' exception' else FClassName := '';
          if Assigned(e) and (e.Note <> '') then FHint := ' title="'+XMLEncode(e.Note)+'"' else FHint := '';

          Add('      <td class="font'+IntToStr(X)+FClassName+'"'+FHint+'>');
          Add('        <div class="text">'+XMLEncode(GetCellText(X, Y))+'</div>');
          if Assigned(e) and (e.Note <> '') then
            Add('        <div class="hint">'+XMLEncode(e.Note)+'</div>');
          Add(Format('        <div class="image" style="background-position: %dpx %dpx; width: %dpx; height: %dpx"></div>', 
            [FX, FY, gridResults.ColWidths[X], gridResults.RowHeights[Y]]));
          Add('      </td>');
          Dec(FX, gridResults.ColWidths[X]);
        end;
        Add('      </tr>');
        Dec(FY, gridResults.RowHeights[Y]);
      end;
      Add('    </tbody>');
      Add('  </table>');
      Add('  </body>');
      Add('</html');

      with TFileStream.Create(dlgSaveHTML.FileName, fmCreate) do
      try
        ss := TStringStream.Create(Tnt_Utf8Encode(Text));
        try
          CopyFrom(ss, 0);
        finally
          ss.Free;
        end;
      finally
        Free;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmRenderingTestCases.cmdFindExamplesClick(Sender: TObject);
begin
  ParseTestCases(memoSource.Text);
  GetBaseFontList(memoSource.Text);
end;

procedure TfrmRenderingTestCases.cmdNewClick(Sender: TObject);
begin
  memoSource.Text := '';
  FExceptions.Clear;
  gridResults.RowCount := 1;
  gridResults.ColCount := 1;
end;

procedure TfrmRenderingTestCases.cmdOpenClick(Sender: TObject);
var
  FNode: IXMLNode;
  I: Integer;
  FException: TRenderingException;
  J: Integer;
  FFontNode: IXMLNode;
begin
  if dlgOpen.Execute then
  begin
    FExceptions.Clear;
    try
      with LoadXMLDocument(dlgOpen.FileName) do
      begin
        if DocumentElement.NodeName <> 'RenderingTestCase' then
          raise Exception.Create('RenderingTestCase root node not found');
        FNode := DocumentElement.ChildNodes.FindNode('Text');
        if not Assigned(FNode) then
          raise Exception.Create('Text node not found');
        memoSource.Text := FNode.NodeValue;
        ParseTestCases(FNode.NodeValue);

        FNode := DocumentElement.ChildNodes.FindNode('Fonts');
        if not Assigned(FNode) then
          raise Exception.Create('Fonts node not found');

        gridResults.ColCount := FNode.ChildNodes.Count + 1;

        for I := 0 to FNode.ChildNodes.Count - 1 do
        begin
          FFontNode := FNode.ChildNodes[I];
          gridResults.Cells[I+1, 0] := FFontNode.Attributes['Name'];
          gridResults.Cells[I+1, 1] := FFontNode.Attributes['Coverage'];

          for J := 0 to FFontNode.ChildNodes.Count - 1 do
          begin
            FException := TRenderingException.Create;
            FException.FontName := FFontNode.Attributes['Name'];
            FException.UnicodeString := FFontNode.ChildNodes[J].NodeValue;
            FException.Note := FFontNode.ChildNOdes[J].Attributes['Note'];
            FExceptions.Add(FException);
          end;
        end;

        FillGrid;

        FNode := DocumentElement.ChildNodes.FindNode('Options');
        if Assigned(FNode) then
        begin
          tbFontSize.Position := FNode.Attributes['FontSize'];
          tbFontSizeChange(tbFontSize);
        end;
      end;
    except
      on E:Exception do
      begin
        ShowMessage('Not a valid Rendering Test Case Project: '+E.Message);
        cmdNewClick(cmdNew);
      end;
    end;
  end;

  ResizeGrid;
end;

procedure TfrmRenderingTestCases.cmdSaveClick(Sender: TObject);
var
  FRootNode, FNode: IXMLNode;
  I: Integer;
  J: Integer;
  FFontNode: IXMLNode;
begin
  if dlgSave.Execute then
  begin
    with NewXMLDocument do
    try
      FRootNode := AddChild('RenderingTestCase');
      FRootNode.AddChild('Text').NodeValue := memoSource.Text;
      FRootNode.AddChild('Options').Attributes['FontSize'] := tbFontSize.Position;

      FNode := FRootNode.AddChild('Fonts');
      for I := 1 to gridResults.ColCount - 1 do
      begin
        FFontNode := FNode.AddChild('Font');
        FFontNode.Attributes['Name'] := gridResults.Cells[I, 0];
        FFontNode.Attributes['Coverage'] := gridResults.Cells[I, 1];
        for J := 0 to FExceptions.Count - 1 do
          if FExceptions[j].FontName = gridResults.Cells[I, 0] then
            with FFontNode.AddChild('Exception') do
            begin
              NodeValue := FExceptions[j].UnicodeString;
              Attributes['Note'] := FExceptions[j].Note;
            end;
      end;

      SaveToFile(dlgSave.FileName);
    except
      on E:Exception do
      begin
        ShowMessage('Could not save Rendering Test Case Project: '+E.Message);
      end;
    end;
  end;
end;

procedure TfrmRenderingTestCases.FormCreate(Sender: TObject);
begin
  FExceptions := TRenderingExceptionList.Create;
  FHintWindow := THintWindow.Create(Self);
end;

procedure TfrmRenderingTestCases.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FExceptions);
  FreeAndNil(FHintWindow);
end;

procedure TfrmRenderingTestCases.GetBaseFontList(Source: WideString);
var
  I: Integer;
  Data: WideString;
begin
  Data := '';
  for I := 1 to Length(Source) do
    if Char(Source[I]) in [#13, #10, #9, ' ', ','] then Continue
    else Data := Data + Source[I];

  FNewChars := '';
  if Assigned(FCheckFontsThread) then
  begin
    FNewChars := Data;
    FCheckFontsThread.Terminate;
    Exit; // Still looking up previous keyboard fonts, it will be checked shortly
  end;

  lblStatus.Caption := 'Searching...';

  FCheckFontsThread := TCheckFontsThread.Create;
  FCheckFontsThread.FreeOnTerminate := True;
  FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
  FCheckFontsThread.AddChars(Data);
  FCheckFontsThread.Resume;
end;

function TfrmRenderingTestCases.GetCellText(ACol, ARow: Integer): WideString;
var
  s, t: WideString;
  n: Cardinal;
begin
  s := gridResults.Cells[0, ARow];
  if ARow < 2 then
    Result := s
  else if ACol = 0 then
  begin
    { Draw U+... }
    n := UTF16_NextChar(s);
    t := '';
    while n > 0 do
    begin
      t := t + Format('U+%0.04X ', [n]);
      n := UTF16_NextChar(s);
    end;

    Result := t;
  end
  else
    Result := s;
end;

procedure TfrmRenderingTestCases.RenderCell(Canvas: TCanvas; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  n: Cardinal;
  t: WideString;
  s: WideString;
  pt: TSize;
  h: Integer;
begin
  if ARow < 2 then
  begin
    s := gridResults.Cells[ACol, ARow];
    Canvas.Font.Name := 'Tahoma';
    Canvas.Font.Size := -11;
    pt := WideCanvasTextExtent(Canvas, s);
    WideCanvasTextRect(Canvas, Rect, (Rect.Right + Rect.Left - pt.cx) div 2, (Rect.Bottom + Rect.Top - pt.cy) div 2, s);
    Exit;
  end;

  s := GetCellText(ACol, ARow);
  s := gridResults.Cells[0, ARow];

  if ACol = 0 then
  begin
    { Draw U+... }
    n := UTF16_NextChar(s);
    t := '';
    while n > 0 do
    begin
      t := t + Format('U+%0.04X ', [n]);
      n := UTF16_NextChar(s);
    end;

    Canvas.Font.Name := 'Tahoma';
    Canvas.Font.Size := -11;
    h := WideCanvasTextHeight(Canvas, t);
    WideCanvasTextRect(Canvas, Rect, Rect.Left + 2, (Rect.Bottom + Rect.Top - h) div 2, t);
  end
  else
  begin
    if Assigned(gridResults.Objects[ACol, ARow])
      then Canvas.Brush.Color := clYellow
      else Canvas.Brush.Color := clWhite;
    Canvas.Font.Name := gridResults.Cells[ACol, 0];
    Canvas.Font.Size := tbFontSize.Position;
    h := WideCanvasTextHeight(Canvas, s);
    WideCanvasTextRect(Canvas, Rect, Rect.Left + 2, (Rect.Bottom + Rect.Top - h) div 2, s);
  end;
end;

procedure TfrmRenderingTestCases.gridResultsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  RenderCell(gridResults.Canvas, ACol, ARow, Rect, State);
end;

procedure TfrmRenderingTestCases.gridResultsExit(Sender: TObject);
begin
  //FHintWindow.ReleaseHandle;
end;

procedure TfrmRenderingTestCases.gridResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  r: TRect;
  e: TRenderingException;
begin
  gridResults.MouseToCell(X, Y, ACol, ARow);
  if (ACol < 1) or (ARow < 2) then Exit;

  e := gridResults.Objects[ACol, ARow] as TRenderingException;
  if Assigned(e) then
  begin
    if Button = mbLeft then
    begin
      if (e.Note = '') or (WideMessageDlg('Clear highlight and note "'+e.Note+'" from this test case?', mtConfirmation, mbOkCancel, 0) = mrOk) then
      begin
        FExceptions.Remove(gridResults.Objects[ACol, ARow]);
        gridResults.Objects[ACol, ARow] := nil;
      end;
    end
    else if Button = mbRight then
      e.Note := WideInputBox('Note on test case', 'Enter note on test case', e.Note);
  end
  else
  begin
    e := TRenderingException.Create;
    e.FontName := gridResults.Cells[ACol, 0];
    e.UnicodeString := gridResults.Cells[0, ARow];
    if Button = mbRight then
      e.Note := WideInputBox('Note on test case', 'Enter note on test case', e.Note);
    gridResults.Objects[ACol, ARow] := e;
    FExceptions.Add(e);
  end;
  
  r := gridResults.CellRect(ACol, ARow);
  InvalidateRect(gridResults.Handle, @r, False);
end;

procedure TfrmRenderingTestCases.gridResultsMouseLeave(Sender: TObject);
begin
  FHintWindow.ReleaseHandle;
end;

procedure TfrmRenderingTestCases.gridResultsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  e: TRenderingException;
  r: TRect;
  pt: TPoint;
begin
  gridResults.MouseToCell(X, Y, ACol, ARow);
  if (FLastCol <> ACol) or (FLastRow <> ARow) then
  begin
    if (ACol < 1) or (ARow < 2) then
      FHintWindow.ReleaseHandle
    else
    begin
      e := gridResults.Objects[ACol, ARow] as TRenderingException;
      if Assigned(e) then
      begin
        r := gridResults.CellRect(ACol, ARow);
        pt := gridResults.ClientToScreen(Point(r.Left, r.Bottom));
        r := FHintWindow.CalcHintRect(640, e.Note, nil);
        OffsetRect(r, pt.X, pt.Y);
        FHintWindow.ActivateHint(r, e.Note);
      end
      else 
        FHintWindow.ReleaseHandle;
    end;
  end;
end;

procedure TfrmRenderingTestCases.CheckFontsThreadComplete(Sender: TObject);
var
  i, n: Integer;
  FCheckFontResult: TCheckFontResult;
begin
  LockUpdates;
  try
    lblStatus.Caption := '';
    try
      if FCheckFontsThread.Results.Count = 0 then Exit;
      FCheckFontResult := FCheckFontsThread.Results[0];

      gridResults.ColCount := FCheckFontResult.Fonts.Count + 2;
      n := 1;
      for I := 0 to FCheckFontResult.Fonts.Count - 1 do
      begin
        if FCheckFontResult.Fonts[i].Coverage = -1 then Continue;
        gridResults.Cells[n, 0] := FCheckFontResult.Fonts[i].FontName;
        gridResults.Cells[n, 1] := IntToStr(FCheckFontResult.Fonts[i].Coverage)+'%';
        Inc(n);
      end;
      gridResults.ColCount := n;
    finally
      FCheckFontsThread := nil;
      if FNewChars <> '' then GetBaseFontList(FNewChars);
    end;
    ResizeGrid;
  finally
    UnlockUpdates;
  end;
end;

procedure TfrmRenderingTestCases.FillGrid;
var
  I: Integer;
  x: Integer;
  y: Integer;
  Found: Boolean;
begin
  LockUpdates;
  try
    for x := 1 to gridResults.ColCount - 1 do
      for y := 2 to gridResults.RowCount - 1 do
        gridResults.Objects[x, y] := nil;
      
    for I := FExceptions.Count - 1 downto 0 do
    begin
      Found := False;
      for x := 1 to gridResults.ColCount - 1 do
      begin
        if gridResults.Cells[x, 0] = FExceptions[I].FontName then
        begin
          for y := 2 to gridResults.RowCount - 1 do
          begin
            if gridResults.Cells[0, y] = FExceptions[i].UnicodeString then
            begin
              gridResults.Objects[x, y] := FExceptions[i];
              Found := True;
              Break;
            end;
          end;
          if Found then Break;
        end;
      end;
      if not Found then FExceptions.Delete(I);
    end;
  finally
    UnlockUpdates;
  end;
end;

procedure TfrmRenderingTestCases.ParseTestCases(Source: WideString);
var
  n: Integer;
  FStart: Integer;
begin
  LockUpdates;
  try
    gridResults.RowCount := 2;
    gridResults.ColCount := 1;
    gridResults.Cells[0, 0] := 'Test Case';
    gridResults.Cells[1, 0] := 'Unicode';
    gridResults.Cells[0, 1] := 'Coverage -->';
    n := 1; FStart := 1;
    while n <= Length(Source) do
    begin
      if Char(Source[n]) in [#13, #10, #9, ' ', ','] then
      begin
        if FStart = n then
          Inc(FStart)
        else
        begin
          ParseTestCase(Copy(Source, FStart, n-FStart));
          FStart := n + 1;
        end;
      end;
      Inc(n);
    end;
    if FStart < n then
      ParseTestCase(Copy(Source, FStart, n-FStart));
  finally
    UnlockUpdates;
  end;
end;

procedure TfrmRenderingTestCases.LockUpdates;
begin
  Inc(FLockUpdateLevel);
  if FLockUpdateLevel = 1 then
    SendMessage(gridResults.Handle, WM_SETREDRAW, 0, 0);
end;

procedure TfrmRenderingTestCases.UnlockUpdates;
begin
  Dec(FLockUpdateLevel);
  if FLockUpdateLevel = 0 then
  begin
    SendMessage(gridResults.Handle, WM_SETREDRAW, 1, 0);
    gridResults.Invalidate;
  end;
end;

procedure TfrmRenderingTestCases.ParseTestCase(TestCase: WideString);
begin
  gridResults.RowCount := gridResults.RowCount + 1;
  gridResults.Cells[0, gridResults.RowCount - 1] := TestCase;
end;

procedure TfrmRenderingTestCases.ResizeGrid;
var
  x: Integer;
  y: Integer;
  w: Integer;
  sz: TSize;
  h: array of Integer;
begin
  LockUpdates;
  try
    SetLength(h, gridResults.RowCount);
    for y := 0 to gridResults.RowCount - 1 do
      h[y] := 0;

    for x := 0 to gridResults.ColCount - 1 do
    begin
      w := 0;

      for y := 0 to gridResults.RowCount - 1 do
      begin
        sz := GetCellSize(x, y);
        w := Max(sz.cx, w);
        h[y] := max(sz.cy, h[y]);
      end;

      gridResults.ColWidths[x] := w + 4;
    end;

    for y := 0 to gridResults.RowCount - 1 do
      gridResults.RowHeights[y] := h[y] + 2;
  finally
    UnlockUpdates;
  end;
end;

procedure TfrmRenderingTestCases.tbFontSizeChange(Sender: TObject);
begin
  lblFontSize.Caption := IntToStr(tbFontSize.Position) + 'pt';
  ResizeGrid;
end;

function TfrmRenderingTestCases.GetCellSize(ACol, ARow: Integer): TSize;
var
  n: Cardinal;
  t: WideString;
  s: WideString;
begin
  if ARow < 2 then
  begin
    s := gridResults.Cells[ACol, ARow];
    gridResults.Canvas.Font.Name := 'Tahoma';
    gridResults.Canvas.Font.Size := -11;
    Result := WideCanvasTextExtent(gridResults.Canvas, s);
    Exit;
  end;

  s := gridResults.Cells[0, ARow];

  if ACol = 0 then
  begin
    { Draw U+... }
    n := UTF16_NextChar(s);
    t := '';
    while n > 0 do
    begin
      t := t + Format('U+%0.04X ', [n]);
      n := UTF16_NextChar(s);
    end;

    gridResults.Canvas.Font.Name := 'Tahoma';
    gridResults.Canvas.Font.Size := -11;
    Result := WideCanvasTextExtent(gridResults.Canvas, t);
  end
  else
  begin
    gridResults.Canvas.Font.Name := gridResults.Cells[ACol, 0];
    gridResults.Canvas.Font.Size := tbFontSize.Position;
    Result := WideCanvasTextExtent(gridResults.Canvas, s);
  end;
end;

{ TRenderingExceptionList }

function TRenderingExceptionList.GetItem(Index: Integer): TRenderingException;
begin
  Result := inherited GetItem(Index) as TRenderingException;
end;

procedure TRenderingExceptionList.SetItem(Index: Integer;
  const Value: TRenderingException);
begin
  inherited SetItem(Index, Value);
end;

end.
