(*
  Name:             BitmapEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Fix Margins reference
*)
unit BitmapEditor;

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons;

type
  TBitmapEditorMoveImage = (miLeft, miRight, miUp, miDown);
  TBitmapEditorDrawMode = (dmDot, dmLine, dmText, dmFill, dmBox, dmFillBox, dmCircle, dmFillCircle);

  TPCDirection = (pcdAcrossThenDown, pcdDownThenAcross);

  TBitmapEditorToolPalette = class;
  TBitmapEditorColourPalette = class;
  TBitmapEditorMovePalette = class;
  TBitmapEditorPreview = class;
  TBitmapEditor = class;

  TBitmapEditor = class(TCustomControl)
  private
    FDrawMode: TBitmapEditorDrawMode;
    FPreview: TBitmapEditorPreview;

    FTemp: Boolean;

    FFont: TFont;

    PrevButton: TMouseButton;
    PrevColour: TColor;

    X1, Y1: Integer;
    FUndoBitmap, FTempBitmap, FBitmap: TBitmap;
    FFGColour, FBGColour: TColor;

    FModified: Boolean;
    FOnModifiedChanged: TNotifyEvent;
    procedure CopyFromTemp;
    procedure CopyToTemp;
    procedure DrawTextToTemp;
    procedure SaveUndoBitmap;
    procedure SetModified(const Value: Boolean);
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SetBitmap(const Value: TBitmap);
  protected
    procedure MoveImage(mi: TBitmapEditorMoveImage);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property DrawMode: TBitmapEditorDrawMode read FDrawMode write FDrawMode;
    property FGColour: TColor read FFGColour write FFGColour;
    property BGColour: TColor read FBGColour write FBGColour;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Repaint; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Modified: Boolean read FModified write SetModified;
  published
    property OnModifiedChanged: TNotifyEvent read FOnModifiedChanged write FOnModifiedChanged;
  end;

  TBitmapEditorToolPalette = class(TCustomControl)
  private
    FBitmapEditor: TBitmapEditor;
    cmd: array[TBitmapEditorDrawMode] of TSpeedButton;
    FDirection: TPCDirection;
    procedure SetBitmapEditor(const Value: TBitmapEditor);
    procedure UpdateDrawMode;
    procedure PositionControls;
    procedure cmdClick(Sender: TObject);
    procedure SetDirection(const Value: TPCDirection);
  public
    procedure Resize; override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
  published
    property BitmapEditor: TBitmapEditor read FBitmapEditor write SetBitmapEditor;
    property Direction: TPCDirection read FDirection write SetDirection;
  end;

  TBitmapEditorColourPalette = class(TCustomControl)
  private
    panCol: array[1..18] of TPanel;
    FIsIconEditor: Boolean;
    FBitmapEditor: TBitmapEditor;
    FDirection: TPCDirection;
    procedure PositionControls;
    procedure SetIsIconEditor(const Value: Boolean);
    procedure SetBitmapEditor(const Value: TBitmapEditor);
    procedure DrawColours;
    procedure SetDirection(const Value: TPCDirection);
    procedure panColMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BitmapEditor: TBitmapEditor read FBitmapEditor write SetBitmapEditor;
    property Direction: TPCDirection read FDirection write SetDirection;
    property IsIconEditor: Boolean read FIsIconEditor write SetIsIconEditor;
  end;

  TBitmapEditorMovePalette = class(TCustomControl)
  private
    FBitmapEditor: TBitmapEditor;
    cmd: array[TBitmapEditorMoveImage] of TSpeedButton;
    cmdCenter: TSpeedButton;
    procedure cmdClick(Sender: TObject);
    procedure SetBitmapEditor(const Value: TBitmapEditor);
    procedure PositionControls;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BitmapEditor: TBitmapEditor read FBitmapEditor write SetBitmapEditor;
  end;

  TBitmapEditorPreview = class(TCustomControl)
  private
    FBitmapEditor: TBitmapEditor;
    procedure SetBitmapEditor(const Value: TBitmapEditor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BitmapEditor: TBitmapEditor read FBitmapEditor write SetBitmapEditor;
  end;

procedure Register;

implementation

{$R BitmapEditor.res}

procedure Register;
begin
  RegisterComponents('Keyman Bitmap Editor',
    [TBitmapEditor,
    TBitmapEditorToolPalette, TBitmapEditorColourPalette,
    TBitmapEditorMovePalette, TBitmapEditorPreview]);
end;

const
  BitmapEditorDrawModeNames: array[TBitmapEditorDrawMode] of string =
    ('Dot', 'Line', 'Text', 'Fill', 'Box', 'FillBox', 'Circle', 'FillCircle');

  BitmapEditorMoveImageNames: array[TBitmapEditorMoveImage] of string =
    ('Left', 'Right', 'Up', 'Down');

{ TBitmapEditor }

procedure DoPositionControls(Parent: TWinControl; Rect: TRect; AMargins: TPoint;
  ControlClass: TControlClass; Direction: TPCDirection = pcdAcrossThenDown);
var
  i: Integer;
  x, y: Integer;
begin
  x := Rect.Left;
  y := Rect.Top;
  for i := 0 to Parent.ControlCount - 1 do
  begin
    if Parent.Controls[i] is ControlClass then
      with Parent.Controls[i] do
      begin
        case Direction of
          pcdAcrossThenDown: if x+AMargins.x+Width >= Rect.Right then begin x := Rect.Left; Inc(y, Height+AMargins.y); end;
          pcdDownThenAcross: if y+AMargins.y+Height >= Rect.Bottom then begin y := Rect.Top; Inc(x, Width+AMargins.x); end;
        end;
        Left := x;
        Top := y;
        case Direction of
          pcdAcrossThenDown: Inc(x, Width+AMargins.x);
          pcdDownThenAcross: Inc(y, Height+AMargins.y);
        end;
      end;
  end;
end;

procedure TBitmapEditor.CopyFromTemp;
begin
  FBitmap.Assign(FTempBitmap);
end;

procedure TBitmapEditor.CopyToTemp;
begin
  FTempBitmap.Assign(FBitmap);
end;

constructor TBitmapEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TPicture.RegisterClipboardFormat(cf_Bitmap, TBitmap);

  FBitmap := TBitmap.Create;
  FBitmap.Width := 16;
  FBitmap.Height := 16;
  FBitmap.PixelFormat := pf4bit;

  FTempBitmap := TBitmap.Create;
  FTempBitmap.Width := 16;
  FTempBitmap.Height := 16;
  FTempBitmap.PixelFormat := pf4bit;

  FUndoBitmap := TBitmap.Create;
  FUndoBitmap.Assign(FBitmap);

  FFont := TFont.Create;
  FFont.Name := 'Small Fonts';
  FFont.Size := 7;

  PrevButton := mbMiddle;
  FGColour := clBlack;
  BGColour := clWhite;
end;

destructor TBitmapEditor.Destroy;
begin
  FBitmap.Free;
  FTempBitmap.Free;
  FUndoBitmap.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TBitmapEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PrevButton := Button;

  if Button = mbLeft then PrevColour := FGColour
  else if Button = mbRight then PrevColour := BGColour
  else Exit;

  X1 := X * 16 div ClientWidth;
  Y1 := Y * 16 div ClientHeight;

  Modified := True;
  SaveUndoBitmap;

  case FDrawMode of
    dmDot:
      FBitmap.Canvas.Pixels[X1, Y1] := PrevColour;
    dmLine, dmBox, dmFillBox, dmCircle, dmFillCircle:
      begin
        FTemp := True;
        CopyToTemp;
        FTempBitmap.Canvas.Pixels[X1, Y1] := PrevColour;
      end;
    dmFill:
      begin
        PrevButton := mbMiddle;
        FBitmap.Canvas.Brush.Color := PrevColour;
        FBitmap.Canvas.FloodFill(X1, Y1, FBitmap.Canvas.Pixels[X1,Y1], fsSurface);
      end;
    dmText:
      begin
        //if FTemp then CopyFromTemp;
        FTemp := True;
        PrevButton := mbMiddle;
        DrawTextToTemp;
      end;
  end;

  Repaint;
end;

procedure TBitmapEditor.Repaint;
begin
  inherited;
  if Assigned(FPreview) then FPreview.Repaint;
end;

procedure TBitmapEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  X2, Y2: Integer;
  r: TRect;
begin
  if PrevButton = mbMiddle then Exit;

  X2 := X * 16 div ClientWidth;
  Y2 := Y * 16 div ClientHeight;

//  X2 := X div ((panEdit.ClientWidth * Screen.PixelsPerInch div 96) div 16);
//  Y2 := Y div ((panEdit.ClientHeight * Screen.PixelsPerInch div 96) div 16);

  if X2 < X1 then begin r.left := x2; r.right := x1+1; end
  else if X1 = X2 then begin r.left := x1; r.right := x2+1; end
  else begin r.left := x1; r.right := x2+1; end;

  if Y2 < Y1 then begin r.top := y2; r.bottom := y1+1; end
  else if Y1 = Y2 then begin r.top := y1; r.bottom := y2+1; end
  else begin r.top := y1; r.bottom := y2+1; end;

  case FDrawMode of
    dmDot:
      begin
        FBitmap.Canvas.Pen.Color := PrevColour;
        FBitmap.Canvas.MoveTo(X1, Y1);
        FBitmap.Canvas.LineTo(X2, Y2);
        FBitmap.Canvas.Pixels[X2, Y2] := PrevColour;
        X1 := X2; Y1 := Y2;
      end;
    dmLine:
      begin
        CopyToTemp;
        FTempBitmap.Canvas.Pen.Color := PrevColour;
        FTempBitmap.Canvas.MoveTo(X1, Y1);
        FTempBitmap.Canvas.LineTo(X2, Y2);
        FTempBitmap.Canvas.Pixels[X2, Y2] := PrevColour;
      end;
    dmBox:
      begin
        CopyToTemp;
        FTempBitmap.Canvas.Brush.Style := bsClear;
        FTempBitmap.Canvas.Pen.Color := PrevColour;
        FTempBitmap.Canvas.Rectangle(r.left, r.top, r.right, r.bottom);
      end;
    dmFillBox:
      begin
        CopyToTemp;
        FTempBitmap.Canvas.Brush.Color := PrevColour;
        FTempBitmap.Canvas.FillRect(r);
        FTempBitmap.Canvas.Brush.Style := bsClear;
      end;
    dmCircle:
      begin
        CopyToTemp;
        FTempBitmap.Canvas.Brush.Style := bsClear;
        FTempBitmap.Canvas.Pen.Color := PrevColour;
        FTempBitmap.Canvas.Ellipse(r);
      end;
    dmFillCircle:
      begin
        CopyToTemp;
        FTempBitmap.Canvas.Brush.Color := PrevColour;
        FTempBitmap.Canvas.Ellipse(r);
        FTempBitmap.Canvas.Brush.Style := bsClear;
      end;
  end;

  Repaint;
end;

procedure TBitmapEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if PrevButton = mbMiddle then Exit;

  if FTemp then
  begin
    FTemp := False;
    CopyFromTemp;
    Repaint;
  end;
  PrevButton := mbMiddle;
end;

procedure TBitmapEditor.DrawTextToTemp;
begin

end;

procedure TBitmapEditor.MoveImage(mi: TBitmapEditorMoveImage);
begin
  FTemp := False;
  case mi of
    miLeft:
      begin
        BitBlt(FTempBitmap.Canvas.Handle, 0, 0, 15, 16, FBitmap.Canvas.Handle, 1, 0, SRCCOPY);
        BitBlt(FTempBitmap.Canvas.Handle, 15, 0, 1, 16, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    miUp:
      begin
        BitBlt(FTempBitmap.Canvas.Handle, 0, 0, 16, 15, FBitmap.Canvas.Handle, 0, 1, SRCCOPY);
        BitBlt(FTempBitmap.Canvas.Handle, 0, 15, 16, 1, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    miRight:
      begin
        BitBlt(FTempBitmap.Canvas.Handle, 1, 0, 15, 16, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
        BitBlt(FTempBitmap.Canvas.Handle, 0, 0,  1, 16, FBitmap.Canvas.Handle, 15, 0, SRCCOPY);
      end;
    miDown:
      begin
        BitBlt(FTempBitmap.Canvas.Handle, 0, 1, 16, 15, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
        BitBlt(FTempBitmap.Canvas.Handle, 0, 0, 16,  1, FBitmap.Canvas.Handle, 0, 15, SRCCOPY);
      end;
  end;
  FBitmap.Canvas.Draw(0, 0, FTempBitmap);
  Repaint;
end;

procedure TBitmapEditor.SaveUndoBitmap;
begin
  FUndoBitmap.Assign(FBitmap);
end;

procedure TBitmapEditor.SetModified(const Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if Assigned(FOnModifiedChanged) then FOnModifiedChanged(Self);
  end;
end;

procedure TBitmapEditor.Paint;
var
  i, y: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.Width := ClientWidth;
    bmp.Height := ClientHeight;
    bmp.PixelFormat := pf4Bit;
    with bmp.Canvas do
    begin
      if FTemp
        then StretchBlt(Handle, 0, 0, ClientWidth, ClientHeight,
          FTempBitmap.Canvas.Handle, 0, 0, 16, 16, SRCCOPY)
        else StretchBlt(Handle, 0, 0, ClientWidth, ClientHeight,
          FBitmap.Canvas.Handle, 0, 0, 16, 16, SRCCOPY);

      //x := panEdit.ClientWidth div 16; //* Screen.PixelsPerInch div 96) div 16;
      for i := 1 to 16 do
      begin
        MoveTo(i * ClientWidth div 16, 0);
        LineTo(i * ClientWidth div 16, ClientHeight);
        MoveTo(0, i * ClientHeight div 16);
        LineTo(ClientWidth, i * ClientHeight div 16);
      end;

      if (FDrawMode = dmText) and FTemp then
      begin
        MoveTo(X1 * ClientWidth div 16 + 1, Y1 * ClientHeight div 16 + 12);
        LineTo(X1 * ClientWidth div 16 + 1, Y1 * ClientHeight div 16 + 1);
        LineTo(X1 * ClientWidth div 16 + 13, Y1 * ClientHeight div 16 + 1);

        FBitmap.Canvas.Font.Assign(FFont);
        y := FBitmap.Canvas.TextHeight('A');
        MoveTo(X1 * ClientWidth div 16 + 1, (Y1+y) * ClientHeight div 16 - 14);
        LineTo(X1 * ClientWidth div 16 + 1, (Y1+y) * ClientHeight div 16 - 1);
        LineTo(X1 * ClientWidth div 16 + 13, (Y1+y) * ClientHeight div 16 - 1);
      end;
    end;
    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, bmp.Canvas.Handle, 0, 0, SRCCOPY);
    //panEdit.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TBitmapEditor.WMEraseBkgnd(var Message: TMessage);
begin
  ;
end;

procedure TBitmapEditor.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);

  FBitmap.Width := 16;
  FBitmap.Height := 16;
  FBitmap.PixelFormat := pf4Bit;

  Modified := False;
  FTemp := False;

  if Assigned(FPreview) then FPreview.Invalidate;
  Invalidate;
  
  FUndoBitmap.Assign(FBitmap);
end;

{ TBitmapEditorColourPalette }

constructor TBitmapEditorColourPalette.Create(AOwner: TComponent);
const
  stdcolors: array[1..18] of TColor =
  ( clBlack,  clWhite,
    clGray,   clSilver,
    clNavy,   clBlue,
    clMaroon, clRed,
    clGreen,  clLime,
    clPurple, clFuchsia,
    clOlive,  clYellow,
    clTeal,   clAqua,
    $e0ffff,  $a03820);
  stdfontcolors: array[1..18] of TColor =
  ( clWhite, clBlack,
    clWhite, clBlack,
    clWhite, clWhite,
    clWhite, clWhite,
    clWhite, clBlack,
    clWhite, clBlack,
    clWhite, clBlack,
    clWhite, clBlack,
    clBlack, clWhite);
var
  i: Integer;
begin
  inherited Create(AOwner);

  for i := 1 to 18 do
  begin
    if not Assigned(panCol[i]) then panCol[i] := TPanel.Create(Self);
    panCol[i].Parent := Self;
    panCol[i].Width := 25;
    panCol[i].Height := 25;
    panCol[i].Visible := True;

    panCol[i].Color := stdcolors[i];
    panCol[i].Font.Color := stdfontcolors[i];
    panCol[i].OnMouseDown := panColMouseDown;
  end;

  panCol[17].Caption := 'T';
  panCol[18].Caption := 'I';
  panCol[17].Visible := FIsIconEditor;
  panCol[18].Visible := FIsIconEditor;
  panCol[17].ControlStyle := panCol[17].ControlStyle + [csNoDesignVisible];
  panCol[18].ControlStyle := panCol[18].ControlStyle + [csNoDesignVisible];

  PositionControls;
end;

procedure TBitmapEditorColourPalette.DrawColours;
var
  i: Integer;
begin
  if not Assigned(FBitmapEditor) then Exit;

  for i := 1 to 18 do
  begin
    if panCol[i].Color = FBitmapEditor.FGColour then
    begin
      if FBitmapEditor.FGColour = FBitmapEditor.BGColour
        then panCol[i].Caption := 'FB'
        else panCol[i].Caption := 'FG';
      panCol[i].BevelOuter := bvLowered;
    end
    else if panCol[i].Color = FBitmapEditor.BGColour then
    begin
      panCol[i].BevelOuter := bvLowered;
      panCol[i].Caption := 'BG';
    end
    else
    begin
      panCol[i].BevelOuter := bvRaised;
      if i = 17 then panCol[i].Caption := 'T'
      else if i = 18 then panCol[i].Caption := 'I'
      else panCol[i].Caption := '';
    end;
  end;
end;

procedure TBitmapEditorColourPalette.panColMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col: TColor;
begin
  if not Assigned(FBitmapEditor) then Exit;

  with Sender as TPanel do col := Color;

  if Button = mbLeft then FBitmapEditor.FGColour := col
  else if Button = mbRight then FBitmapEditor.BGColour := col;

  //if GetDrawMode = dmText then DrawTextToTemp;

  DrawColours;
end;

procedure TBitmapEditorColourPalette.PositionControls;
begin
  DoPositionControls(Self, ClientRect, Point(2, 2), TPanel, FDirection);
  panCol[17].Visible := FIsIconEditor;
  panCol[18].Visible := FIsIconEditor;
end;

procedure TBitmapEditorColourPalette.Resize;
begin
  inherited Resize;
  PositionControls;
end;

procedure TBitmapEditorColourPalette.SetBitmapEditor(const Value: TBitmapEditor);
begin
  FBitmapEditor := Value;
  DrawColours;
end;

procedure TBitmapEditorColourPalette.SetDirection(const Value: TPCDirection);
begin
  FDirection := Value;
  PositionControls;
end;

procedure TBitmapEditorColourPalette.SetIsIconEditor(const Value: Boolean);
begin
  FIsIconEditor := Value;

  panCol[17].Visible := FIsIconEditor;
  panCol[18].Visible := FIsIconEditor;
end;

{ TBitmapEditorToolPalette }

procedure TBitmapEditorToolPalette.cmdClick(Sender: TObject);
begin
  UpdateDrawMode;
end;

constructor TBitmapEditorToolPalette.Create(AOwner: TComponent);
var
  i: TBitmapEditorDrawMode;
begin
  inherited Create(AOwner);
  for i := Low(TBitmapEditorDrawMode) to High(TBitmapEditorDrawMode) do
  begin
    cmd[i] := TSpeedButton.Create(Self);
    cmd[i].Parent := Self;
    cmd[i].Width := 23;
    cmd[i].Height := 22;
    cmd[i].OnClick := cmdClick;
    cmd[i].GroupIndex := 1;
    cmd[i].Glyph.LoadFromResourceName(HInstance, 'BitmapEditor_'+BitmapEditorDrawModeNames[i]);
  end;
  cmd[dmDot].Down := True;
  PositionControls;
end;

procedure TBitmapEditorToolPalette.Loaded;
begin
  inherited Loaded;
  PositionControls;
end;

procedure TBitmapEditorToolPalette.PositionControls;
begin
  DoPositionControls(Self, ClientRect, Point(0, 0), TSpeedButton, FDirection);
end;

procedure TBitmapEditorToolPalette.Resize;
begin
  inherited Resize;
  PositionControls;
end;

procedure TBitmapEditorToolPalette.SetBitmapEditor(const Value: TBitmapEditor);
begin
  FBitmapEditor := Value;
  UpdateDrawMode;
end;

procedure TBitmapEditorToolPalette.SetDirection(const Value: TPCDirection);
begin
  FDirection := Value;
  PositionControls;
end;

procedure TBitmapEditorToolPalette.UpdateDrawMode;
var
  FDrawMode: TBitmapEditorDrawMode;
begin
  if not Assigned(FBitmapEditor) then Exit;
  for FDrawMode := Low(TBitmapEditorDrawMode) to High(TBitmapEditorDrawMode) do
    if cmd[FDrawMode].Down then
    begin
      FBitmapEditor.DrawMode := FDrawMode;
      Exit;
    end;
  FBitmapEditor.DrawMode := dmDot;
end;

{ TBitmapEditorPreview }

constructor TBitmapEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBitmapEditorPreview.Destroy;
begin
  if Assigned(FBitmapEditor) then FBitmapEditor.FPreview := nil;
  inherited Destroy;
end;

procedure TBitmapEditorPreview.Paint;
var
  x, y: Integer;
begin
  if not Assigned(FBitmapEditor) then Exit;
  x := Width * (Screen.PixelsPerInch div 96);
  y := Height * (Screen.PixelsPerInch div 96);
  with Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(Rect(0,0,x,y));
    if FBitmapEditor.FTemp
      then Canvas.Draw((x-16) div 2, (y-16) div 2, FBitmapEditor.FTempBitmap)
      else Canvas.Draw((x-16) div 2, (y-16) div 2, FBitmapEditor.FBitmap);
  end;
end;

procedure TBitmapEditorPreview.SetBitmapEditor(const Value: TBitmapEditor);
begin
  if Value <> FBitmapEditor then
  begin
    if Assigned(FBitmapEditor) then FBitmapEditor.FPreview := nil;
    FBitmapEditor := Value;
    FBitmapEditor.FPreview := Self;
  end;
end;

{ TBitmapEditorMovePalette }

procedure TBitmapEditorMovePalette.cmdClick(Sender: TObject);
var
  i: TBitmapEditorMoveImage;
begin
  if not Assigned(FBitmapEditor) then Exit;
  for i := Low(TBitmapEditorMoveImage) to High(TBitmapEditorMoveImage) do
    if Sender = cmd[i] then
    begin
      FBitmapEditor.MoveImage(i);
      Exit;
    end;
end;

constructor TBitmapEditorMovePalette.Create(AOwner: TComponent);
var
  i: TBitmapEditorMoveImage;
begin
  inherited;
  for i := Low(TBitmapEditorMoveImage) to High(TBitmapEditorMoveImage) do
  begin
    cmd[i] := TSpeedButton.Create(Self);
    cmd[i].Parent := Self;
    cmd[i].Width := 17;
    cmd[i].Height := 17;
    cmd[i].OnClick := cmdClick;
    cmd[i].GroupIndex := 0;
    cmd[i].AllowAllUp := True;
    cmd[i].Glyph.LoadFromResourceName(HInstance, 'BitmapEditor_'+BitmapEditorMoveImageNames[i]);
  end;

  cmdCenter := TSpeedButton.Create(Self);
  cmdCenter.Parent := Self;
  cmdCenter.Width := 17;
  cmdCenter.Height := 17;
  cmdCenter.Enabled := False;

  PositionControls;
end;

procedure TBitmapEditorMovePalette.PositionControls;
begin
  cmdCenter.Left := ClientWidth div 2 - 8;
  cmdCenter.Top := ClientHeight div 2 - 8;

  cmd[miLeft].Left := cmdCenter.Left - 17;
  cmd[miLeft].Top := cmdCenter.Top;
  cmd[miUp].Left := cmdCenter.Left;
  cmd[miUp].Top := cmdCenter.Top - 17;
  cmd[miRight].Left := cmdCenter.Left + 17;
  cmd[miRight].Top := cmdCenter.Top;
  cmd[miDown].Left := cmdCenter.Left;
  cmd[miDown].Top := cmdCenter.Top + 17;
end;

procedure TBitmapEditorMovePalette.Resize;
begin
  inherited;
  PositionControls;
end;

procedure TBitmapEditorMovePalette.SetBitmapEditor(const Value: TBitmapEditor);
begin
  FBitmapEditor := Value;
  PositionControls;
end;

end.
