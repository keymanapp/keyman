(*
  Name:             UfrmCharacterIdentifier
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jan 2009

  Modified Date:    4 Nov 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jan 2009 - mcdurdin - Initial version
                    30 Nov 2009 - mcdurdin - Add font lookup
                    04 Nov 2011 - mcdurdin - I3124 - Add support for rendering tests to charident
*)
unit UfrmCharacterIdentifier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  KeymanTrayIcon,
  Dialogs, StdCtrls, Grids, TntGrids, PlusmemoU, ExtCtrls, utilcheckfontchars,
  Menus;

type
  TfrmCharacterIdentifier = class(TForm)
    sgChars: TTntStringGrid;
    dlgFont: TFontDialog;
    Panel1: TPanel;
    pmChars: TPlusMemoU;
    cmdFont: TButton;
    cmdPaste: TButton;
    Splitter1: TSplitter;
    gridFonts: TStringGrid;
    Splitter2: TSplitter;
    mnuPopup: TPopupMenu;
    mnuOpen: TMenuItem;
    mnuAbout: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    N2: TMenuItem;
    cmdOpenRenderingTestCases: TMenuItem;
    procedure pmCharsChange(Sender: TObject);
    procedure cmdFontClick(Sender: TObject);
    procedure sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure gridFontsClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdPasteClick(Sender: TObject);
    procedure cmdOpenRenderingTestCasesClick(Sender: TObject);
  private
    FFont: TFont;
    FNewChars: WideString;
    FCheckFontsThread: TCheckFontsThread;
    tiCRM: TKeymanTrayIcon;
    ForceClose: Boolean;
    procedure tiCRMClick(Sender: TObject);
    procedure AppMinimize(Sender: TObject);
    procedure FillGrid;
    procedure UpdateFont;
    procedure FillFontList(Chars: WideString);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCharacterIdentifier: TfrmCharacterIdentifier;

implementation

uses
  TntGraphics, TntClipbrd, urlutil, UfrmRenderingTestCases;

{$R *.dfm}

procedure TfrmCharacterIdentifier.AppMinimize(Sender: TObject);
begin
  Hide;
end;

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

procedure TfrmCharacterIdentifier.cmdOpenRenderingTestCasesClick(
  Sender: TObject);
begin  // I3124
  frmRenderingTestCases.Show;
end;

procedure TfrmCharacterIdentifier.cmdPasteClick(Sender: TObject);
begin
  pmChars.SetTextBuf(PWideChar(TntClipboard.AsText));
end;

procedure TfrmCharacterIdentifier.UpdateFont;
begin
  pmChars.Font := dlgFont.Font;
  sgChars.Font := dlgFont.Font;
  with sgChars.Canvas do
  begin
    Font := sgChars.Font;
    sgChars.RowHeights[0] := TextHeight('A') + 4;
    Font := FFont;
    sgChars.RowHeights[1] := TextHeight('A') + 4;
  end;
end;

procedure TfrmCharacterIdentifier.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  Message.Result := 1;
  ForceClose := True;
end;

procedure TfrmCharacterIdentifier.pmCharsChange(Sender: TObject);
begin
  FillGrid;
  FillFontList(pmChars.WideText);
end;

procedure TfrmCharacterIdentifier.sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if ARow = 1 then
    sgChars.Canvas.Font := FFont;
  WideCanvasTextRect(sgChars.Canvas, Rect,
    (Rect.Right + Rect.Left - sgChars.Canvas.TextWidth(sgChars.Cells[ACol, ARow])) div 2,
    (Rect.Top + Rect.Bottom - sgChars.Canvas.TextHeight(sgChars.Cells[ACol, ARow])) div 2,
    sgChars.Cells[ACol, ARow]);
end;

procedure TfrmCharacterIdentifier.tiCRMClick(Sender: TObject);
begin
  Show;
  Application.BringToFront;
  Application.Restore;
  BringToFront;
end;

procedure TfrmCharacterIdentifier.gridFontsClick(Sender: TObject);
begin
  if Assigned(FCheckFontsThread) then Exit;

  dlgFont.Font.Name := gridFonts.Cells[0, gridFonts.Row];
  UpdateFont;
end;

procedure TfrmCharacterIdentifier.mnuAboutClick(Sender: TObject);
begin
  openurl('http://blog.tavultesoft.com/2011/07/character-identifier-tool.html');
end;

procedure TfrmCharacterIdentifier.mnuExitClick(Sender: TObject);
begin
  ForceClose := True;
  Close;
end;

procedure TfrmCharacterIdentifier.mnuOpenClick(Sender: TObject);
begin
  tiCRMClick(tiCRM);
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
  FCheckFontsThread.Resume;
end;

procedure TfrmCharacterIdentifier.FillGrid;
var
  I: Integer;
  s: WideString;
begin
  s := pmChars.WideText;
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

procedure TfrmCharacterIdentifier.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not ForceClose then
  begin
    CanClose := False;
    Hide;
  end
  else
    CanClose := True;
end;

procedure TfrmCharacterIdentifier.FormCreate(Sender: TObject);
begin
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 8;

  dlgFont.Font := pmChars.Font;
  UpdateFont;

  Application.OnMinimize := AppMinimize;
  tiCRM := TKeymanTrayIcon.Create(Self);
  tiCRM.OnClick := tiCRMClick;
  //tiCRM.Icons := ilTrayIcons;
  tiCRM.PopupMenu := mnuPopup;
  tiCRM.Icon := Application.Icon;
  tiCRM.AnimateInterval := 250;
  tiCRM.Visible := True;

end;

end.
