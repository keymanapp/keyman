{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCrossTable.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{
  ��������� ��������� �������� ��� ���������� Cross Tables, �������� �������
  ������� �� ��������� ������ ��� �� ������, ��� � �� ������.

  ��� ���������� ������� ���������� ������� �������� ����� ������(DataSet)
  � ��� ����: ColumnFieldName, RowFieldName � ValueFieldName, ������ ���
  �� ������� ��������� �� ���� �������� � ����� ��������������, � ���������
  ������������ ��� ���������� Cross Table.

  ������� ����� � ����������� ������� �������� ����������
  CaptColWidthInSantim, CaptRowHeightInSantim, ColWidthInSantim � RowHeightInSantim.

  ������� ��� ������ ������� ������������ ����������� �������� IndentsInSantim.

  ����� � ������ ����������, ����� � �������� �������� ������� �������������
  ����� �������� Colors � Fonts.

  ��������� ������ � ��������� ��� ������������ �������� ���������� Title �
  TitleAlignment ��������������. ������� ���������� �� ������ �������� ����� ���������
  � ����������� ������� OnPrintTableElement.

  ���������� ����� ���������� DataSet � ������������� ��������� Filter;
--------------
  �������� Optons:TPCTOptions;

  TPCTOptions = set of ( fcoIntermediateColResults, fcoIntermediateRowResults,
      fcoColResults, fcoRowResults,
     fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
     fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
     fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
     fcoShowPageNumbers, fcoVertColCaptionsFont );

   fcoIntermediateColResults		- ����� ������������� ������ �� ��������;
   fcoIntermediateRowResults		- ����� ������������� ������ �� �������;
   fcoColResults			- ����� ������ �� ��������;
   fcoRowResults			- ����� ������ �� �������;
   fcoIntermediateColCaptions		- ����������� ���������� �������� �� ������ �������� ��� ������ �� ������;
   fcoIntermediateRowCaptions		- ����������� ���������� ����� �� ������ ��������;
   fcoIntermediateLeftIndent		- ������������ ����� ������ �� ������ ��������;
   fcoIntermediateTopIndent		- �����-��;
   fcoIntermediateRightIndent		- �����-��;
   fcoIntermediateBottomIndent		- �����-��;
   fcoShowPageNumbers		- ����������� ������ �������� � ������������ � ����������;
   fcoVertColCaptionsFont		- ����� ���������� �������� ������������ �������;

--------------

___�������___

  OnPrintQuery	- ����������� � ����������� ���-�� ������� ��� ������ �������;
                  ��������� �������� ������;

  OnPrintNewPage	- ����������� � ������ ������ ��������� ��������; ��������� �������� ������;

  OnPrintTableElement	- ����������� � ������ ������� �������� ������� ( ���������, ��������� ������ );
                                       ��������� �������� �������� ������, ������ �������������� ����� � ���� ����,
                                       ���������� ��������� ������������ ������, �������� ������;
                                       �������� TableElement: TPCTableElement = ( teTitle, teCell, teColCapt, teRowCapt,  TeColIRes, teRowIRes, teColRes,
                                        teRowRes ); ��������� �� ��� ���������� ��������.

  OnCalcResult	- ���� ������ ������� ���������, �� ������ ������ �������������� ����. ������� ��������  �������� ������� ������ �
                                       �������� ������ ��� ������� ������� � ������. ���� ������� OnCalcResult �� ���������, ������ �����������
                                       �����������,   ������ ��������������, ��� �������� ������ ����� ������������� � �������� single � �����
                                       �������������� ��� ����� ��������������� �������� � �����.

  OnDuplicateCellValue  - ������� ������������, ���� �����-���� ���� �������� ������� � ������ ������������� ����� ������ ��������.

___������___

    procedure Print; - ��� ����.
    procedure PreviewTo( Canvas: TCanvas; PageWidth, PageHeight: integer );  - ����� ������� �� ������������ �����(Canvas) � ���������
                                                                               �������� �������� ������� � ������.

}
unit JvgCrossTable;

interface
uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  buttons,
  Dialogs,
  JvComponent,
  JvgTypes,
  JvgCommClasses,
  JvgUtils,
  StdCtrls,
  ExtCtrls,
  SysUtils,
  Forms,
  JvgBevel,
  DB,
  DBCtrls,
  Menus,
  DBTables,
  Printers;

const
  MAX_COLS = 1024;
  MAX_ROWS = 1024;
type
  TglPrintingStatus = (fpsContinue, fpsResume, fpsAbort);

  TPrintQueryEvent = procedure(Sender: TObject;
    ColPageCount, RowPageCount: cardinal;
    var CanPrint: Boolean) of object;

  TPrintNewPageEvent = procedure(Sender: TObject;
    ColPageNo, RowPageNo: cardinal;
    var PrintingStatus: TglPrintingStatus) of object;

  TDrawCellEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    Value: string;
    var CanPrint: Boolean) of object;

  TCalcResultEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    CellValue: string;
    IntermediateColResult,
    IntermediateRowResult,
    ColResult,
    RowResult: single) of object;

  TDuplicateCellValueEvent = procedure(Sender: TObject;
    ColNo, RowNo: cardinal;
    Value: string;
    var UseDuplicateValue: Boolean) of object;

  TPCTOptions = set of (fcoIntermediateColResults, fcoIntermediateRowResults,
    fcoColResults, fcoRowResults,
    fcoIntermediateColCaptions, fcoIntermediateRowCaptions,
    fcoIntermediateLeftIndent, fcoIntermediateTopIndent,
    fcoIntermediateRightIndent, fcoIntermediateBottomIndent,
    fcoShowPageNumbers, fcoVertColCaptionsFont);

  TPCTableElement = (teTitle, teCell, teColCapt, teRowCapt, teColIRes,
    teRowIRes, teColRes, teRowRes);

  TPrintTableElement = procedure(Sender: TObject;
    var Text: string;
    ColNo, RowNo: integer;
    TableElement: TPCTableElement;
    var Font: TFont;
    var Color: TColor;
    var AlignFlags: Word;
    var CanPrint: Boolean) of object;

  TJvgPrintCrossTableColors = class(TPersistent)
  private
    FCaptions, FCells, FResults, FIntermediateResults: TColor;
  published
    property Captions: TColor read FCaptions write FCaptions;
    property Cells: TColor read FCells write FCells;
    property Results: TColor read FResults write FResults;
    property IntermediateResults: TColor read FIntermediateResults write
      FIntermediateResults;
  end;

  TJvgPrintCrossTableFonts = class(TPersistent)
  private
    FColCaptions, FRowCaptions, FCells, FResults, FIntermediateResults,
      FTitles: TFont;
    procedure SetColCaptions(Value: TFont);
    procedure SetRowCaptions(Value: TFont);
    procedure SetCells(Value: TFont);
    procedure SetResults(Value: TFont);
    procedure SetIntermediateResults(Value: TFont);
    procedure SetTitles(Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Titles: TFont read FTitles write SetTitles;
    property ColCaptions: TFont read FColCaptions write SetColCaptions;
    property RowCaptions: TFont read FRowCaptions write SetRowCaptions;
    property Cells: TFont read FCells write SetCells;
    property Results: TFont read FResults write SetResults;
    property IntermediateResults: TFont read FIntermediateResults write
      SetIntermediateResults;
  end;

  TJvgPrintCrossTableIndents = class(TPersistent)
  private
    FLeft, FTop,
      FRight, FBottom: single;
  public
    //    constructor Create;
    //    destructor Destroy; override;
  published
    property _Left: single read FLeft write FLeft;
    property _Top: single read FTop write FTop;
    property _Right: single read FRight write FRight;
    property _Bottom: single read FBottom write FBottom;
  end;

  TJvgPrintCrossTable = class(TJvComponent)
  private
    FDataSet: TDataSet;
    FColumnFieldName: string;
    FRowFieldName: string;
    FValueFieldName: string;
    FVerticalGrid: boolean;
    FHorizontalGrid: boolean;
    FOptions: TPCTOptions;
    FPageWidth: integer;
    FPageHeight: integer;
    FColWidthInSantim: single;
    FRowHeightInSantim: single;
    FIndentsInSantim: TJvgPrintCrossTableIndents;
    FCaptColWidthInSantim: single;
    FCaptRowHeightInSantim: single;
    FFonts: TJvgPrintCrossTableFonts;
    FColors: TJvgPrintCrossTableColors;
    FTitle: string;
    FTitleAlignment: TAlignment;

    FOnPrintQuery: TPrintQueryEvent;
    FOnPrintNewPage: TPrintNewPageEvent;
    FOnPrintTableElement: TPrintTableElement;
    FOnCalcResult: TCalcResultEvent;
    FOnDuplicateCellValue: TDuplicateCellValueEvent;

    Font_: TFont;
    Color_: TColor;
    ColsSum: array[0..MAX_COLS] of single;
    RowsSum: array[0..MAX_ROWS] of single;
    FinalColsSum: array[0..MAX_COLS] of single;
    FinalRowsSum: array[0..MAX_ROWS] of single;
    Columnslist, RowsList: TStringList;
    ColsOnPage, RowsOnPage, TotalCols, TotalRows: integer;
    ColsOnPage1, RowsOnPage1: integer;
    ColsOnPageX, RowsOnPageX: integer;
    RowPageCount, ColPageCount: integer;
    //    LOGPIXELSX_, LOGPIXELSY_: integer;
    CaptColWidth, CaptRowHeight: integer;
    LeftIndent, TopIndent, RightIndent, BottomIndent: integer;
    ColWidth, RowHeight: integer;
    ColsOnCurrPage, RowsOnCurrPage: integer;

    procedure PrintTable(Canvas: TCanvas);
    procedure CalcResults(Str: string; ColNo, RowNo: integer);

    procedure SetColumnFieldName(Value: string);
    procedure SetRowFieldName(Value: string);
    procedure SetValueFieldName(Value: string);
    procedure SetDataSet(Value: TDataSet);
    procedure SetOptions(Value: TPCTOptions);

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Loaded; override;
    procedure DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo, ColsOnThisPage,
      RowsOnThisPage: integer);
    procedure DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo, ColNo, RowNo:
      integer; Str: string; Element: TPCTableElement);
    procedure DrawTitle(Canvas: TCanvas; RowPageNo: integer);
    function CalcColNo(ColPageNo: integer): integer;
    function CalcRowNo(RowPageNo: integer): integer;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print;
    procedure PreviewTo(Canvas: TCanvas; PageWidth, PageHeight: integer);
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property ColumnFieldName: string read FColumnFieldName write
      SetColumnFieldName;
    property RowFieldName: string read FRowFieldName write SetRowFieldName;
    property ValueFieldName: string read FValueFieldName write
      SetValueFieldName;
    property Options: TPCTOptions read FOptions write SetOptions;
    property PageWidth: integer read FPageWidth write FPageWidth;
    property PageHeight: integer read FPageHeight write FPageHeight;
    property ColWidthInSantim: single read FColWidthInSantim write
      FColWidthInSantim;
    property RowHeightInSantim: single read FRowHeightInSantim write
      FRowHeightInSantim;
    property IndentsInSantim: TJvgPrintCrossTableIndents read FIndentsInSantim
      write FIndentsInSantim;
    property CaptColWidthInSantim: single read FCaptColWidthInSantim write
      FCaptColWidthInSantim;
    property CaptRowHeightInSantim: single read FCaptRowHeightInSantim write
      FCaptRowHeightInSantim;
    property Fonts: TJvgPrintCrossTableFonts read FFonts write FFonts;
    property Colors: TJvgPrintCrossTableColors read FColors write FColors;

    property OnPrintQuery: TPrintQueryEvent read FOnPrintQuery write
      FOnPrintQuery;
    property OnPrintNewPage: TPrintNewPageEvent read FOnPrintNewPage write
      FOnPrintNewPage;
    property OnPrintTableElement: TPrintTableElement read FOnPrintTableElement
      write FOnPrintTableElement;
    property OnCalcResult: TCalcResultEvent read FOnCalcResult write
      FOnCalcResult;
    property OnDuplicateCellValue: TDuplicateCellValueEvent read
      FOnDuplicateCellValue write FOnDuplicateCellValue;
    property Title: string read FTitle write FTitle;
    property TitleAlignment: TAlignment read FTitleAlignment write
      FTitleAlignment;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

constructor TJvgPrintCrossTableFonts.Create;
begin
  FTitles := TFont.Create;
  FColCaptions := TFont.Create;
  FRowCaptions := TFont.Create;
  FCells := TFont.Create;
  FResults := TFont.Create;
  FIntermediateResults := TFont.Create;
end;

destructor TJvgPrintCrossTableFonts.Destroy;
begin
  FTitles.Free;
  FColCaptions.Free;
  FRowCaptions.Free;
  FCells.Free;
  FResults.Free;
  FIntermediateResults.Free;
  inherited;
end;

procedure TJvgPrintCrossTableFonts.SetTitles(Value: TFont);
begin
  FTitles.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetColCaptions(Value: TFont);
begin
  FColCaptions.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetRowCaptions(Value: TFont);
begin
  FRowCaptions.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetCells(Value: TFont);
begin
  FCells.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetResults(Value: TFont);
begin
  FResults.Assign(Value);
end;

procedure TJvgPrintCrossTableFonts.SetIntermediateResults(Value: TFont);
begin
  FIntermediateResults.Assign(Value);
end;

constructor TJvgPrintCrossTable.Create(AOwner: TComponent);
begin
  inherited;
  ColumnsList := TStringList.Create;
  RowsList := TStringList.Create;
  Colors := TJvgPrintCrossTableColors.Create;
  Fonts := TJvgPrintCrossTableFonts.Create;
  Font_ := TFont.Create;
  //  Fonts.Cells.Name := 'Arial';
  {$IFDEF FR_RUS}
  with Fonts do
  begin
    Titles.CharSet := RUSSIAN_CHARSET;
    ColCaptions.CharSet := RUSSIAN_CHARSET;
    RowCaptions.CharSet := RUSSIAN_CHARSET;
    Cells.CharSet := RUSSIAN_CHARSET;
    Results.CharSet := RUSSIAN_CHARSET;
    IntermediateResults.CharSet := RUSSIAN_CHARSET;
  end;
  {$ENDIF}
  FIndentsInSantim := TJvgPrintCrossTableIndents.Create;

  with FIndentsInSantim do
  begin
    _Left := 1;
    _Top := 1;
    _Right := 1;
    _Bottom := 1;
  end;

  ColWidthInSantim := 0.9;
  RowHeightInSantim := 0.5;

  FVerticalGrid := true;
  FHorizontalGrid := true;

  Colors.Captions := $00FFF2D2;
  Colors.Cells := clWhite;
  Colors.Results := $00C5DEC5;
  Colors.IntermediateResults := $00ABCCF1;

  Options := [fcoIntermediateColResults, fcoIntermediateRowResults,
    fcoColResults, fcoRowResults,
    fcoShowPageNumbers];
end;

destructor TJvgPrintCrossTable.Destroy;
begin
  inherited;
  ColumnsList.Free;
  RowsList.Free;
  Fonts.Free;
  Colors.Free;
  FIndentsInSantim.Free;
  Font_.Free;
end;

procedure TJvgPrintCrossTable.Loaded;
begin
  inherited;
  if fcoVertColCaptionsFont in Options then
  begin
    FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 900);
  end
  else
    FFonts.ColCaptions.Handle := CreateRotatedFont(Fonts.ColCaptions, 0);
end;

procedure TJvgPrintCrossTable.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = DataSet) then
    DataSet := nil;
end;

procedure TJvgPrintCrossTable.DrawGrid(Canvas: TCanvas; ColPageNo, RowPageNo,
  ColsOnThisPage, RowsOnThisPage: integer);
var
  Col, Row: integer;
begin
  exit;
  if ColPageNo = ColPageCount - 1 then
    inc(ColsOnThisPage);
  if RowPageNo = RowPageCount - 1 then
    inc(RowsOnThisPage);
  if FVerticalGrid then
    for Col := 0 to ColsOnThisPage + 1 do
      with Canvas do
      begin
        MoveToEx(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
          TopIndent, nil);
        Windows.LineTo(Handle, CaptColWidth + Col * ColWidth + LeftIndent,
          TopIndent + (RowsOnThisPage + 2) * RowHeight);
      end;
  if FHorizontalGrid then
    for Row := 0 to RowsOnThisPage + 1 do
      with Canvas do
      begin
        MoveToEx(Handle, LeftIndent, CaptRowHeight + Row * RowHeight +
          TopIndent, nil);
        Windows.LineTo(Handle, LeftIndent + (ColsOnThisPage + 2) * ColWidth,
          CaptRowHeight + Row * RowHeight + TopIndent);
      end;
end;

function TJvgPrintCrossTable.CalcColNo(ColPageNo: integer): integer;
begin
  if ColPageNo > 0 then
    Result := ColsOnPage1
  else
    Result := 0;
  if ColPageNo > 1 then
    inc(Result, ColsOnPageX * (ColPageNo - 1));
end;

function TJvgPrintCrossTable.CalcRowNo(RowPageNo: integer): integer;
begin
  if RowPageNo > 0 then
    Result := RowsOnPage1
  else
    Result := 0;
  if RowPageNo > 1 then
    inc(Result, RowsOnPageX * (RowPageNo - 1));
end;

procedure TJvgPrintCrossTable.DrawCell(Canvas: TCanvas; ColPageNo, RowPageNo,
  ColNo, RowNo: integer; Str: string; Element: TPCTableElement);
var
  R, R_: TRect;
  i, j: integer;
  AlignFlags: Word;
  fCanPrint: boolean;
const
  SingleLine: array[boolean] of integer = (DT_WORDBREAK,
    DT_SINGLELINE);
begin

  with Canvas do
  begin

    if (ColNo = -1) or (RowNo = -1) then //...Draw Caption
    begin
      i := max(0, ColNo);
      j := max(0, RowNo);
      if (RowNo = -1) then
      begin
        R.Left := i * ColWidth;
        R.Top := 0;
        if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
          inc(R.Left, LeftIndent);
        if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
          inc(R.Top, TopIndent);
        if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
          inc(R.Left, CaptColWidth);
        R.Right := R.Left + ColWidth;
        R.Bottom := R.Top + CaptRowHeight;
      end
      else
      begin
        R.Left := 0;
        R.Top := j * RowHeight;
        if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
          inc(R.Left, LeftIndent);
        if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
          inc(R.Top, TopIndent);
        if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
          inc(R.Top, CaptRowHeight);
        R.Right := R.Left + CaptColWidth;
        R.Bottom := R.Top + RowHeight;
      end;

    end
    else //...Draw Cell
    begin
      i := CalcColNo(ColPageNo);
      j := CalcRowNo(RowPageNo);

      R.Left := (ColNo - i + 1) * ColWidth - ColWidth;
      R.Top := (RowNo - j + 1) * RowHeight - RowHeight;

      if (ColPageNo = 0) or (fcoIntermediateLeftIndent in Options) then
        inc(R.Left, LeftIndent);
      if (RowPageNo = 0) or (fcoIntermediateTopIndent in Options) then
        inc(R.Top, TopIndent);

      if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
        inc(R.Top, CaptRowHeight);
      if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
        inc(R.Left, CaptColWidth);

      R.Right := R.Left + ColWidth;
      R.Bottom := R.Top + RowHeight;
      Inc(R.Bottom);
      Inc(R.Right);
    end;

    InflateRect(R, -2, -2);

    with Fonts, Brush do
      case Element of
        teCell:
          begin
            Font.Assign(Cells);
            Color := Colors.Cells;
          end;
        teColCapt:
          begin
            Font.Assign(ColCaptions);
            Color := Colors.Captions;
          end;
        teRowCapt:
          begin
            Font.Assign(RowCaptions);
            Color := Colors.Captions;
          end;
        teColIRes,
          teRowIRes:
          begin
            Font.Assign(IntermediateResults);
            Color := Colors.IntermediateResults;
          end;
        teColRes,
          teRowRes:
          begin
            Font.Assign(Results);
            Color := Colors.Results;
          end;
      end;

    AlignFlags := SingleLine[(ColNo <> -1) and (RowNo <> -1)] or DT_CENTER or
      DT_VCENTER;

    fCanPrint := true;
    if Assigned(FOnPrintTableElement) then
    begin
      Color_ := Brush.Color;
      Font_.Assign(Font);
      FOnPrintTableElement(self, Str, i + 1, j + 1, Element, Font_, Color_,
        AlignFlags, fCanPrint);
      Font.Assign(Font_);
    end;

    if not fCanPrint then
      exit;

    Canvas.FillRect(R);
    Brush.Color := 0;
    InflateRect(R, 1, 1);
    Canvas.FrameRect(R);
    SetBkMode(Handle, TRANSPARENT);

    if (fcoVertColCaptionsFont in Options) and (RowNo = -1) then
      ExtTextOut(Handle, R.Left + 5, R.Bottom - 2, ETO_CLIPPED, @r,
        PChar(Str), Length(Str), nil)
    else
    begin
      R_ := R;
      DrawText(Handle, PChar(Str), -1, R_, DT_CENTER or DT_WORDBREAK or
        DT_CALCRECT);
      R.Top := R.Top + max(0, (R.Bottom - R_.Bottom) div 2);
      DrawText(Handle, PChar(Str), -1, R, DT_CENTER or DT_WORDBREAK);
    end;

  end;
end;

procedure TJvgPrintCrossTable.DrawTitle(Canvas: TCanvas; RowPageNo: integer);
var
  fCanPrint: boolean;
  Str: string;
  AlignFlags: Word;
  R: TRect;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT,
    DT_CENTER);
begin
  with Canvas do
  begin
    Font.Assign(Fonts.Titles);
    if TopIndent < TextHeight('ky') then
      exit;
    if not ((RowPageNo = 0) or (fcoIntermediateTopIndent in Options)) then
      exit;

    fCanPrint := true;
    AlignFlags := (DT_SINGLELINE or DT_EXPANDTABS) or
      Alignments[FTitleAlignment];
    Str := FTitle;
    if Assigned(FOnPrintTableElement) then
    begin
      Color_ := Brush.Color;
      Font_.Assign(Font);
      FOnPrintTableElement(self, Str, -1, -1, teTitle, Font_, Color_,
        AlignFlags, fCanPrint);
      Font.Assign(Font_);
    end;
    if not fCanPrint then
      exit;
    R := Rect(LeftIndent, 10, PageWidth - RightIndent, PageHeight -
      BottomIndent);
    SetBkMode(Handle, TRANSPARENT);
    DrawText(Handle, PChar(Str), -1, R, AlignFlags);

  end;
end;

procedure TJvgPrintCrossTable.Print;
begin
  PrintTable(nil);
end;

procedure TJvgPrintCrossTable.PreviewTo(Canvas: TCanvas; PageWidth, PageHeight:
  integer);
begin
  Self.PageWidth := PageWidth;
  Self.PageHeight := PageHeight;
  PrintTable(Canvas);
end;

procedure TJvgPrintCrossTable.PrintTable(Canvas: TCanvas);
var
  i, j: integer;
  fPrint, fCanPrint, fUseDuplicateValue: boolean;
  ClientSize: TSize;
  PrintingStatus: TglPrintingStatus;
  str: string;
  TargetCanvas: TCanvas;
  ColumnField, RowField, ValueField: TField;
  ColPageNo, RowPageNo, ColNo, RowNo: integer;
  ClientR, CaptR, DataR, R: TRect;
  FilledRowNo: array[0..MAX_ROWS] of boolean;
  OldFilter: string;
  OldFiltered: boolean;
begin
  if not Assigned(FDataSet) then
    exit;
  FillChar(FinalColsSum, SizeOf(FinalColsSum), 0);
  FillChar(FinalRowsSum, SizeOf(FinalRowsSum), 0);
  with IndentsInSantim do
  try
    with FDataSet do
    begin

      OldFilter := Filter;
      OldFiltered := Filtered;

      Filtered := false;
      ColumnField := FieldByName(ColumnFieldName);
      RowField := FieldByName(RowFieldName);
      ValueField := FieldByName(ValueFieldName);
    end;

    fPrint := not Assigned(Canvas);
    if fPrint then
    begin
      TargetCanvas := Printer.Canvas;
      PageWidth := Printer.PageWidth;
      PageHeight := Printer.PageHeight;
    end
    else
    begin
      TargetCanvas := Canvas;
    end;
    if fPrint then
      Printer.BeginDoc;
    with TargetCanvas do
    begin
      //    LOGPIXELSX_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSX);
      //    LOGPIXELSY_ := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);

      ColWidth := SantimsToPixels(Handle, ColWidthInSantim, true);
      RowHeight := SantimsToPixels(Handle, RowHeightInSantim, false);
      LeftIndent := SantimsToPixels(Handle, _Left, true);
      TopIndent := SantimsToPixels(Handle, _Top, false);
      RightIndent := SantimsToPixels(Handle, _Right, true);
      BottomIndent := SantimsToPixels(Handle, _Bottom, false);
      CaptColWidth := SantimsToPixels(Handle, CaptColWidthInSantim, true);
      CaptRowHeight := SantimsToPixels(Handle, CaptRowHeightInSantim, false);

    end;
    //  CaptR := Rect( LeftIndent, TopIndent, PageWidth-RightIndent, PageHeight-BottomIndent );
    //  DataR := CaptR; InflateRect( DataR, -ColWidth, -RowHeight );
      //---------
    with FDataSet do
    begin
      ColumnsList.Clear;
      RowsList.Clear;

      First;
      while not EOF do
      begin
        ColumnsList.Add(ColumnField.AsString);
        RowsList.Add(RowField.AsString);
        Next;
      end;
    end;
    ColumnsList.Sort;
    RowsList.Sort;
    ColumnsList.Sorted := true;
    RowsList.Sorted := true;
    for i := ColumnsList.Count - 1 downto 1 do
      if ColumnsList[i - 1] = ColumnsList[i] then
        ColumnsList.Delete(i);

    for i := RowsList.Count - 1 downto 1 do
      if RowsList[i - 1] = RowsList[i] then
        RowsList.Delete(i);

    TotalCols := ColumnsList.Count + 1; //...+1 - final results
    TotalRows := RowsList.Count + 1;

    ClientSize.cx := PageWidth - LeftIndent - RightIndent;
    ClientSize.cy := PageHeight - TopIndent - BottomIndent;

    ColsOnPage1 := (ClientSize.cx - CaptColWidth) div ColWidth -
      integer(fcoIntermediateColResults in Options);
    RowsOnPage1 := (ClientSize.cy - CaptRowHeight) div RowHeight -
      integer(fcoIntermediateRowResults in Options);

    ClientSize.cx := PageWidth;
    ClientSize.cy := PageHeight;

    if (fcoIntermediateColCaptions in Options) then
      dec(ClientSize.cy, CaptRowHeight);
    if (fcoIntermediateRowCaptions in Options) then
      dec(ClientSize.cx, CaptColWidth);
    if (fcoIntermediateLeftIndent in Options) then
      dec(ClientSize.cx, LeftIndent);
    if (fcoIntermediateTopIndent in Options) then
      dec(ClientSize.cy, TopIndent);
    if (fcoIntermediateRightIndent in Options) then
      dec(ClientSize.cx, RightIndent);
    if (fcoIntermediateBottomIndent in Options) then
      dec(ClientSize.cy, BottomIndent);

    ColsOnPageX := (ClientSize.cx) div ColWidth -
      integer(fcoIntermediateColResults in Options);
    RowsOnPageX := (ClientSize.cy) div RowHeight -
      integer(fcoIntermediateRowResults in Options);

    RowPageCount := max(trunc(TotalRows / (RowsOnPageX + 1)), 1);
    ColPageCount := max(trunc(TotalCols / (ColsOnPageX + 1)), 1);

    //...EVENT OnPrintQuery
    fCanPrint := true;
    if Assigned(FOnPrintQuery) then
      FOnPrintQuery(self, ColPageCount, RowPageCount, fCanPrint);
    if not fCanPrint then
    begin
      if fPrint then
        Printer.Abort;
      exit;
    end;
    //...
    TargetCanvas.Font := Fonts.Cells;
    TargetCanvas.Brush.Color := 0;
    TargetCanvas.Font.Color := 0;
    SetBkMode(TargetCanvas.Handle, TRANSPARENT);

    for RowPageNo := 0 to RowPageCount - 1 do
    begin
      for ColPageNo := 0 to ColPageCount - 1 do
        with TargetCanvas do
        begin

          if ColPageNo = 0 then
            ColsOnPage := ColsOnPage1
          else
            ColsOnPage := ColsOnPageX;
          if RowPageNo = 0 then
            RowsOnPage := RowsOnPage1
          else
            RowsOnPage := RowsOnPageX;

          FillChar(ColsSum, SizeOf(ColsSum), 0);
          FillChar(RowsSum, SizeOf(RowsSum), 0);

          ColsOnCurrPage := min(ColsOnPage, TotalCols - ColPageNo *
            ColsOnPage);
          RowsOnCurrPage := min(RowsOnPage, TotalRows - RowPageNo *
            RowsOnPage);

          if ColPageNo = ColPageCount - 1 then
            dec(ColsOnCurrPage);
          if RowPageNo = RowPageCount - 1 then
            dec(RowsOnCurrPage);
          //...EVENT OnPrintNewPage
          PrintingStatus := fpsContinue;
          if Assigned(OnPrintNewPage) then
            OnPrintNewPage(self, ColPageNo, RowPageNo, PrintingStatus);
          if PrintingStatus = fpsAbort then
          begin
            if fPrint then
              Printer.Abort;
            exit;
          end;
          if PrintingStatus = fpsResume then
          begin
            if fPrint then
              Printer.EndDoc;
            exit;
          end;
          //...
          ClientR := Rect(0, 0, PageWidth, PageHeight);
          Brush.Color := clWhite;
          FillRect(ClientR);
          Brush.Color := 0;
          //      FrameRect(ClientR);
          DrawTitle(TargetCanvas, RowPageNo);
          CaptR := Rect(LeftIndent, TopIndent, LeftIndent + (ColsOnCurrPage
            + 2) * (ColWidth) + CaptColWidth,
            TopIndent + (RowsOnCurrPage + 2) * (RowHeight) +
            CaptRowHeight);
          DataR := CaptR;
          InflateRect(DataR, -CaptColWidth, -CaptRowHeight);

          Brush.Color := clWhite;
          FillRect(CaptR);
          Brush.Color := 0;
          //	  Brush.Color := $0080FFFF; if not fPrint then FillRect(DataR); Brush.Color := 0;
          //	  FrameRect(CaptR);
          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          if fcoShowPageNumbers in Options then
            TextOut(10, 10, '[ ' + IntToStr(RowPageNo + 1) + ' / ' +
              IntToStr(ColPageNo + 1) + ' ]');

          DrawGrid(TargetCanvas, ColPageNo, RowPageNo, ColsOnCurrPage,
            RowsOnCurrPage);
          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          if (RowPageNo = 0) or (fcoIntermediateColCaptions in Options) then
            for i := 0 to ColsOnCurrPage - 1 do //...captions_________________________
            try
              Str := ColumnsList[CalcColNo(ColPageNo) + i];
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, i, -1, Str,
                teColCapt);
            except break;
            end;

          if (ColPageNo = 0) or (fcoIntermediateRowCaptions in Options) then
            for i := 0 to RowsOnCurrPage - 1 do //...captions_________________________
            try
              Str := RowsList[CalcRowNo(RowPageNo) + i];
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, -1, i, Str,
                teRowCapt);
            except break;
            end;

          i := CalcColNo(ColPageNo);
          with FDataSet do
            for ColNo := i to i + ColsOnCurrPage - 1 do
            begin

              if ColNo >= ColumnsList.Count then
                break;
              if ColumnsList[ColNo] = '' then
                continue;
              Filtered := false;
              Filter := '[' + ColumnFieldName + ']=''' +
                ColumnsList[ColNo] + '''' + OldFilter;
              Filtered := true;
              First;

              FillChar(FilledRowNo, sizeof(FilledRowNo), 0);
              for i := 0 to RecordCount - 1 do
              begin

                if not RowsList.Find(RowField.AsString, RowNo) then
                begin
                  Next;
                  continue;
                end;

                if (RowNo < CalcRowNo(RowPageNo)) or (RowNo >=
                  CalcRowNo(RowPageNo) + RowsOnCurrPage) then
                begin
                  Next;
                  continue;
                end;
                //	  if not((RowNo >= RowPageNo*(RowsOnCurrPage))and(RowNo <= (RowPageNo+1)*(RowsOnPage)-1))then
                //	  begin Next; continue; end;

                fUseDuplicateValue := false;
                if FilledRowNo[RowNo] then //...duplicate
                begin
                  if Assigned(OnDuplicateCellValue) then
                    OnDuplicateCellValue(self, ColNo, RowNo,
                      valueField.AsString, fUseDuplicateValue);

                end;

                if (not FilledRowNo[RowNo]) or fUseDuplicateValue then
                begin
                  FilledRowNo[RowNo] := true;
                  Str := valueField.AsString;
                  DrawCell(TargetCanvas, ColPageNo, RowPageNo,
                    ColNo, RowNo, Str, teCell);
                  CalcResults(Str, ColNo, RowNo);

                end;

                Next;
              end;
            end;
          //...sums
          i := CalcColNo(ColPageNo);
          j := CalcRowNo(RowPageNo);
          if fcoIntermediateColResults in Options then
            for ColNo := i to i + ColsOnCurrPage - 1 do
            begin
              Str := FloatToStr(ColsSum[ColNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, j +
                RowsOnCurrPage, Str, teColIRes);
            end;
          if fcoIntermediateRowResults in Options then
            for RowNo := j to j + RowsOnCurrPage - 1 do
            begin
              Str := FloatToStr(RowsSum[RowNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, i +
                ColsOnCurrPage, RowNo, Str, teRowIRes);
            end;
          //...sums

          if RowPageNo = RowPageCount - 1 then
            for ColNo := i to i + ColsOnCurrPage - 1 do
            begin
              Str := FloatToStr(FinalColsSum[ColNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, ColNo, j +
                RowsOnCurrPage + integer(fcoIntermediateRowResults in
                Options), Str, teColRes);
            end;
          if ColPageNo = ColPageCount - 1 then
            for RowNo := j to j + RowsOnCurrPage - 1 do
            begin
              Str := FloatToStr(FinalRowsSum[RowNo]);
              DrawCell(TargetCanvas, ColPageNo, RowPageNo, i +
                ColsOnCurrPage + integer(fcoIntermediateColResults in
                Options), RowNo, Str, teRowRes);
            end;
          //...
          if fPrint and
            ((ColPageNo <> ColPageCount - 1) or
            (RowPageNo <> RowPageCount - 1)) then
            Printer.NewPage;
        end;
    end;
  finally
    DataSet.Filter := OldFilter;
    DataSet.Filtered := OldFiltered;
  end;
  //...
  if fPrint then
    Printer.EndDoc;

end;

{procedure TJvgPrintCrossTable.SetDataSource(Value: TDataSource);
begin
  FDataSource := Value;
end;}

procedure TJvgPrintCrossTable.CalcResults(Str: string; ColNo, RowNo: integer);
begin
  //...if event is assigned then user should calculates results himself
  if Assigned(OnCalcResult) then
  begin
    OnCalcResult(self,
      ColNo, RowNo,
      Str, {CellValue}
      ColsSum[ColNo], {IntermediateColResult}
      RowsSum[RowNo], {IntermediateRowResult}
      FinalColsSum[ColNo], {ColResult}
      FinalRowsSum[RowNo] {RowResult});
  end
  else
  begin
    try
      ColsSum[ColNo] := ColsSum[ColNo] + StrToFloat(Str);
    except
    end;
    try
      FinalColsSum[ColNo] := FinalColsSum[ColNo] + StrToFloat(Str);
    except
    end;
    try
      RowsSum[RowNo] := RowsSum[RowNo] + StrToFloat(Str);
    except
    end;
    try
      FinalRowsSum[RowNo] := FinalRowsSum[RowNo] + StrToFloat(Str);
    except
    end;
  end;
end;

procedure TJvgPrintCrossTable.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TJvgPrintCrossTable.SetColumnFieldName(Value: string);
begin
  FColumnFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetRowFieldName(Value: string);
begin
  FRowFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetValueFieldName(Value: string);
begin
  FValueFieldName := Value;
end;

procedure TJvgPrintCrossTable.SetOptions(Value: TPCTOptions);
begin
  FOptions := Value;
end;

end.
