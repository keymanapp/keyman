(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit UfrmCharacterIdentifier;  // I3323  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, utilcheckfontchars, Vcl.ComCtrls;

type
  TfrmCharacterIdentifier = class(TForm)
    sgChars: TStringGrid;
    dlgFont: TFontDialog;
    Panel1: TPanel;
    pmChars: TRichEdit;
    cmdFont: TButton;
    SpTBXButton2: TButton;
    Splitter1: TSplitter;
    gridFonts: TStringGrid;
    Splitter2: TSplitter;
    procedure pmCharsChange(Sender: TObject);
    procedure cmdFontClick(Sender: TObject);
    procedure sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure gridFontsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFont: TFont;
    FNewChars: WideString;
    FCheckFontsThread: TCheckFontsThread;
    LastText: string;
    procedure FillGrid;
    procedure UpdateFont;
    procedure FillFontList(Chars: WideString);
    procedure CheckFontsThreadComplete(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCharacterIdentifier: TfrmCharacterIdentifier;

implementation

{$R *.dfm}

procedure TfrmCharacterIdentifier.CheckFontsThreadComplete(Sender: TObject);
var
  i, n: Integer;
  FCheckFontResult: TCheckFontResult;
begin
  try
    if FCheckFontsThread.Results.Count = 0 then Exit;
    FCheckFontResult := FCheckFontsThread.Results[0];

    gridFonts.RowCount := FCheckFontResult.Fonts.Count + 1;
    gridFonts.Cells[0,0] := 'Font';
    gridFonts.Cells[1,0] := 'Coverage';
    n := 1;
    for i := 0 to FCheckFontResult.Fonts.Count - 1 do
    begin
      if FCheckFontResult.Fonts[i].Coverage = -1 then Continue;
      gridFonts.Cells[0,n] := FCheckFontResult.Fonts[i].FontName;
      gridFonts.Cells[1,n] := IntToStr(FCheckFontResult.Fonts[i].Coverage)+'%';
      Inc(n);
    end;
    gridFonts.RowCount := n + 1;
  finally
    FCheckFontsThread := nil;
    if FNewChars <> '' then FillFontList(FNewChars);
  end;

  // DisplayFonts...
end;

procedure TfrmCharacterIdentifier.cmdFontClick(Sender: TObject);
begin
  dlgFont.Font := pmChars.Font;
  if dlgFont.Execute then
  begin
    UpdateFont;
  end;
end;

procedure TfrmCharacterIdentifier.UpdateFont;
begin
  pmChars.Font := dlgFont.Font;
  pmChars.Font.Style := [];
  sgChars.Font := dlgFont.Font;
  sgChars.Font.Style := [];
  sgChars.Canvas.Font := sgChars.Font;
  sgChars.RowHeights[0] := sgChars.Canvas.TextHeight('A') + 4;
  sgChars.Canvas.Font := FFont;
  sgChars.RowHeights[1] := sgChars.Canvas.TextHeight('A') + 4;
end;

procedure TfrmCharacterIdentifier.pmCharsChange(Sender: TObject);
begin
  if pmChars.Text <> LastText then
  begin
    FillGrid;
    FillFontList(pmChars.Text);
    LastText := pmChars.Text;
  end;
end;

procedure TfrmCharacterIdentifier.sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if ARow = 1 then
    sgChars.Canvas.Font := FFont
  else
    sgChars.Canvas.Font := sgChars.Font;
  sgChars.Canvas.TextRect(Rect,
    (Rect.Right + Rect.Left - sgChars.Canvas.TextWidth(sgChars.Cells[ACol, ARow])) div 2,
    (Rect.Top + Rect.Bottom - sgChars.Canvas.TextHeight(sgChars.Cells[ACol, ARow])) div 2,
    sgChars.Cells[ACol, ARow]);
end;

procedure TfrmCharacterIdentifier.gridFontsClick(Sender: TObject);
begin
  if Assigned(FCheckFontsThread) then Exit;

  dlgFont.Font.Name := gridFonts.Cells[0, gridFonts.Row];
  UpdateFont;
end;

procedure TfrmCharacterIdentifier.FillFontList(Chars: WideString);
begin
  FNewChars := '';
  if Assigned(FCheckFontsThread) then
  begin
    FNewChars := Chars;
    FCheckFontsThread.Terminate;
    Exit; // Still looking up previous keyboard fonts, it will be checked shortly
  end;
  gridFonts.Cells[0,1] := 'Searching...';
  gridFonts.Cells[1,1] := '';
  gridFonts.RowCount := 2;
  gridFonts.FixedRows := 1;
  FCheckFontsThread := TCheckFontsThread.Create;
  FCheckFontsThread.FreeOnTerminate := True;
  FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
  FCheckFontsThread.AddChars(Chars);
  FCheckFontsThread.Start;
end;

procedure TfrmCharacterIdentifier.FillGrid;
var
  I: Integer;
  s: WideString;
begin
  s := pmChars.Text;
  sgChars.ColCount := Length(s);
  if Length(s) = 0 then
  begin
    sgChars.Cells[0,0] := '';
    sgChars.Cells[0,1] := '';
  end;

  for I := 1 to Length(s) do
  begin
    sgChars.Cells[I-1, 0] := s[I];
    sgChars.Cells[I-1, 1] := 'U+'+IntToHex(Ord(s[i]), 4);
  end;
end;

procedure TfrmCharacterIdentifier.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmCharacterIdentifier.FormCreate(Sender: TObject);
begin
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 8;

  dlgFont.Font := pmChars.Font;
  UpdateFont;
end;

end.
