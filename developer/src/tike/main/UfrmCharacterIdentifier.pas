(*
  Name:             UfrmCharacterIdentifier
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jan 2009

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jan 2009 - mcdurdin - Initial version
                    30 Nov 2009 - mcdurdin - Add font lookup
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
                    09 Aug 2015 - mcdurdin - I4832 - Character Identifier cells are too narrow for entire U+xxxxxx caption
                    09 Aug 2015 - mcdurdin - I4834 - Character identifier usage is unclear because of blank font grid
                    
*)
unit UfrmCharacterIdentifier;  // I3323  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, utilcheckfontchars,
  CleartypeDrawCharacter,
  UfrmTIKEDock, JvComponentBase, JvDockControlForm;

type
  TfrmCharacterIdentifier = class(TTIKEDockForm)
    sgChars: TStringGrid;
    dlgFont: TFontDialog;
    gridFonts: TStringGrid;
    splitterBottom: TSplitter;
    procedure sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure gridFontsClick(Sender: TObject);
    procedure sgCharsEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgCharsDblClick(Sender: TObject);
    procedure sgCharsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
  private
    FNewChars: WideString;
    FDrawChar: TCleartypeDrawCharacter;
    FCheckFontsThread: TCheckFontsThread;
    FFontName: string;
    FChars: string;
    FDisplayedChars: string;
    FOnCancelFocus: TNotifyEvent;
    procedure FillGrid;
    procedure FillFontList(Chars: WideString);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure SetChars(const Value: string);
    procedure AdjustPanes;
    procedure RefreshFonts;
    procedure FontSizeChanged(Sender: TObject);
    procedure UpdateFont;
    procedure CancelFocus;
  protected
    function GetHelpTopic: string; override;
  public
    property Chars: string read FChars write SetChars;
    property OnCancelFocus: TNotifyEvent read FOnCancelFocus write FOnCancelFocus;
  end;

var
  frmCharacterIdentifier: TfrmCharacterIdentifier;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  UfrmCharacterMapNew,
  Unicode;

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
    gridFonts.RowCount := n;
  finally
    FCheckFontsThread := nil;
    if FNewChars <> '' then FillFontList(FNewChars);
  end;

  if FCheckFontsThread = nil then
  begin
    FFontName := gridFonts.Cells[0, gridFonts.Row];
    UpdateFont;
  end;

  AdjustPanes;
end;

procedure TfrmCharacterIdentifier.UpdateFont;
var
  sz: Integer;
begin
  sgChars.Font.Name := frmCharacterMapNew.CharMapFontName;
  sz := frmCharacterMapNew.CharMapFontSize;
  if sz < 42 then sz := 42;

  sgChars.Font.Height := sz;
  with sgChars.Canvas do
  begin
    Font := sgChars.Font;
    sgChars.RowHeights[0] := TextHeight('A') + 4;
    Font := Self.Font;
    sgChars.RowHeights[1] := TextHeight('A') + 4;
  end;
  sgChars.DefaultColWidth := sz;
  AdjustPanes;
end;

procedure TfrmCharacterIdentifier.SetChars(const Value: string);
begin
  FChars := Value;
  FillGrid;
  if sgChars.Focused or gridFonts.Focused then FillFontList(FChars);
end;

procedure TfrmCharacterIdentifier.sgCharsDblClick(Sender: TObject);
var
  ch: string;
begin
  ch := sgChars.Cells[sgChars.Col, 0];
  if ch = '' then Exit;

  if (Length(ch) > 1) and Uni_IsSurrogate1(ch[1]) and Uni_IsSurrogate2(ch[2])
    then frmCharacterMapNew.FindCharacter(Uni_SurrogateToUTF32(ch[1],ch[2]))
    else frmCharacterMapNew.FindCharacter(Ord(ch[1]));
end;

procedure TfrmCharacterIdentifier.sgCharsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  ext: TPoint;
  s: string;
begin
  if ARow = 0 then
  begin
    sgChars.Canvas.Brush.Color := clWindow;
    sgChars.Canvas.FillRect(Rect);

    if sgChars.Cells[ACol, ARow] = '' then
      Exit;

    FDrawChar.SetFontDetails(FFontName, sgChars.Font.Size);
    FDrawChar.Color := Font.Color;
    FDrawChar.DisplayQuality := ctCleartype;
    ext := FDrawChar.TextExtent(sgChars.Canvas.Handle, sgChars.Cells[ACol, ARow]);
    FDrawChar.DrawText(sgChars.Canvas.Handle, TA_CENTER, (Rect.Right+Rect.Left) div 2, (Rect.Bottom+Rect.Top - ext.Y) div 2, Rect, sgChars.Cells[ACol, ARow]);
  end
  else
  begin
    s := sgChars.Cells[ACol, ARow];   // I4832
    sgChars.Canvas.Font.Size := 7;
    sgChars.Canvas.Font.Name := 'Arial';
    if sgChars.DefaultColWidth <= 46 then Delete(s,1,2);   // I4832

    sgChars.Canvas.TextRect(Rect,
      (Rect.Right + Rect.Left - sgChars.Canvas.TextWidth(s)) div 2,
      (Rect.Top + Rect.Bottom - sgChars.Canvas.TextHeight(s)) div 2,
      s);
  end;
end;

procedure TfrmCharacterIdentifier.sgCharsEnter(Sender: TObject);
begin
  RefreshFonts;
end;

procedure TfrmCharacterIdentifier.CancelFocus;
begin
  if Assigned(FOnCancelFocus) then FOnCancelFocus(Self);
end;

procedure TfrmCharacterIdentifier.sgCharsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    CancelFocus;
    Key := 0;
  end;
end;

procedure TfrmCharacterIdentifier.RefreshFonts;
begin
  if FDisplayedChars <> FChars then
  begin
    FDisplayedChars := FChars;
    FillFontList(FChars);
  end;
end;

procedure TfrmCharacterIdentifier.gridFontsClick(Sender: TObject);
begin
  if Assigned(FCheckFontsThread) then Exit;

  FFontName := gridFonts.Cells[0, gridFonts.Row];
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
  I, J: Integer;
  s: WideString;
begin
  s := FChars;
  sgChars.ColCount := Length(s);
  if Length(s) = 0 then
  begin
    sgChars.Cells[0,0] := '';
    sgChars.Cells[0,1] := '';
  end;

  I := 1; J := 0;
  while I <= Length(s) do
  begin
    if Uni_IsSurrogate1(s[I]) and (I < Length(S)) and Uni_IsSurrogate2(s[I+1]) then
    begin
      sgChars.Cells[J, 0] := Copy(s,I,2);
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Uni_SurrogateToUTF32(S[I],S[I+1]),5);
      Inc(I);
    end
    else
    begin
      sgChars.Cells[J, 0] := s[I];
      sgChars.Cells[J, 1] := 'U+'+IntToHex(Ord(s[i]), 4);
    end;
    Inc(I); Inc(J);
  end;

  sgChars.ColCount := J;

  AdjustPanes;
end;

procedure TfrmCharacterIdentifier.FontSizeChanged(Sender: TObject);
begin
  UpdateFont;
end;

procedure TfrmCharacterIdentifier.FormActivate(Sender: TObject);
begin
  inherited;
  RefreshFonts;
end;

procedure TfrmCharacterIdentifier.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinHeight := 140;
end;

procedure TfrmCharacterIdentifier.FormCreate(Sender: TObject);
begin
  inherited;

  FDrawChar := TCleartypeDrawCharacter.Create;
  FFontName := frmCharacterMapNew.CharMapFontName;
  frmCharacterMapNew.OnFontSizeChanged := FontSizeChanged;

  gridFonts.RowCount := 2;   // I4834
  gridFonts.Cells[0, 1] := '  - Click to find fonts - ';   // I4834

  UpdateFont;
end;

procedure TfrmCharacterIdentifier.FormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(frmCharacterMapNew) then
    frmCharacterMapNew.OnFontSizeChanged := nil;
  FreeAndNil(FDrawChar);
end;

procedure TfrmCharacterIdentifier.FormResize(Sender: TObject);
begin
  inherited;
  AdjustPanes;
end;

function TfrmCharacterIdentifier.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_CharacterIdentifier;
end;

procedure TfrmCharacterIdentifier.AdjustPanes;
var
  h: Integer;
begin
  if sgChars.ColCount * (sgChars.DefaultColWidth+1) > sgChars.ClientWidth
    then h := GetSystemMetrics(SM_CYHSCROLL)
    else h := 0;
  gridFonts.Height := ClientHeight - splitterBottom.Height -
    sgChars.RowHeights[0] - sgChars.RowHeights[1] - 6 - h;

  gridFonts.ColWidths[1] := 64;
  gridFonts.ColWidths[0] := gridFonts.ClientWidth - gridFonts.ColWidths[1] - 1;
end;

end.
