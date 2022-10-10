(*
  Name:             UframeBitmapEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Refactor menus
                    14 Sep 2006 - mcdurdin - Add support for icons, 256 colours, transparency (1 bit) and rework text tool
                    30 May 2007 - mcdurdin - I724 - Fixed error when path does not exist for bitmaps
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Support widestring filenames
                    18 Mar 2011 - mcdurdin - I2634 - Transparent colour does not always work on remote desktop
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jan 2012 - mcdurdin - I3147 - Handle errors importing invalid graphic files in bitmap editor
                    03 Nov 2012 - mcdurdin - I3508 - V9.0 - Merge of I3147 - Handle errors importing invalid graphic files in bitmap editor
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    07 Feb 2014 - mcdurdin - I4032 - V9.0 - Add support for Redo to Keyman Developer actions
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UframeBitmapEditor;  // I3306   // I4796

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, MenuImgList, Menus, StdCtrls, Buttons, ExtCtrls, PaintPanel,
  CleartypeDrawCharacter, mbColorPalette, mbColorPreview,
  ComCtrls, ActnList, PalUtils, KMDActionInterfaces, Vcl.ExtDlgs,
  Vcl.Imaging.PngImage,
  System.Actions, System.ImageList;

type
  TMoveImage = (miLeft, miRight, miUp, miDown);

  TDrawMode = (dmDot, dmLine, dmText, dmFill, dmBox, dmFillBox, dmCircle, dmFillCircle);

  TBitmapEditorBMType = (bebEdit, bebUndo, bebTemp);

  TBitmapEditorCurrentEdit = record
    IsMask, IsTemp: Boolean;
    Button: TMouseButton;
    Bitmap: TBitmap;
    Colour: DWord;
    //beb: array[0..1] of TBitmapEditorBMType;
  end;

  TframeBitmapEditor = class(TFrame, IKMDEditActions)
    panEdit: TPaintPanel;
    panColours: TPanel;
    panControls: TPanel;
    cmdPen: TSpeedButton;
    cmdLine: TSpeedButton;
    cmdText: TSpeedButton;
    cmdBox: TSpeedButton;
    cmdFillBox: TSpeedButton;
    panPreviewFrame: TPanel;
    lblPreview: TLabel;
    panPreview: TPaintPanel;
    panMove: TPanel;
    cmdMoveUp: TSpeedButton;
    cmdMoveLeft: TSpeedButton;
    cmdMoveRight: TSpeedButton;
    cmdMoveDown: TSpeedButton;
    cmdMoveNone: TSpeedButton;
    cmdCircle: TSpeedButton;
    cmdFillCircle: TSpeedButton;
    palColours: TmbColorPalette;
    cpForeground: TmbColorPreview;
    cpBackground: TmbColorPreview;
    ilCmds: TImageList;
    cmdFill: TSpeedButton;
    mnuColours: TPopupMenu;
    ActionList1: TActionList;
    dlgOpenPalette: TOpenDialog;
    actOpenPalette: TAction;
    Item1: TMenuItem;
    cmdImport: TButton;
    cmdExport: TButton;
    dlgImport: TOpenPictureDialog;
    dlgExport: TSavePictureDialog;
    imgTransparent: TImage;
    lblPixel: TLabel;
    panReadOnlyIcons: TPanel;
    lblReadOnlyIcons: TLabel;
    memoReadOnlyIcons: TMemo;
    ppReadOnlyIcons: TPaintPanel;
    panReadOnlyIconsTop: TPanel;
    cmdClearAll: TButton;
    procedure panEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure panEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure panEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure panPreviewPaint(Sender: TObject);
    procedure panEditPaint(Sender: TObject);
    procedure cmdClick(Sender: TObject);
    procedure cmdMoveUpClick(Sender: TObject);
    procedure cmdMoveLeftClick(Sender: TObject);
    procedure cmdMoveRightClick(Sender: TObject);
    procedure cmdMoveDownClick(Sender: TObject);
    procedure cmdPenClick(Sender: TObject);
    procedure cmdLineClick(Sender: TObject);
    procedure cmdFillClick(Sender: TObject);
    procedure cmdBoxClick(Sender: TObject);
    procedure cmdFillBoxClick(Sender: TObject);
    procedure cmdTextClick(Sender: TObject);
    procedure palColoursCellClick(Button: TMouseButton; Shift: TShiftState;
      Index: Integer; AColor: TColor; var DontCheck: Boolean);
    procedure actOpenPaletteExecute(Sender: TObject);
    procedure cmdExportClick(Sender: TObject);
    procedure cmdImportClick(Sender: TObject);
    procedure ppReadOnlyIconsPaint(Sender: TObject);
    procedure cmdClearAllClick(Sender: TObject);
  private
    FReadOnlyIcons: array of TGraphic;
    FRSP21318IsPngIcon: Boolean;
    FEdit: TBitmapEditorCurrentEdit;

    X1, Y1: Integer;
    FBitmaps: array[TBitmapEditorBMType] of TBitmap;
    FGColour, BGColour: TColor;

    FModified: Boolean;
    FOnModifiedChanged: TNotifyEvent;
    FLastInsertText: WideString;
    FLastInsertFont: TFont;
    FLastDisplayQuality: TCleartypeDisplayQuality;
    FTransparency: Boolean;
    FIconCanBeEdited: Boolean;
    procedure CopyFromTemp;
    procedure CopyToTemp;
    procedure DrawColours;
    procedure DrawTextToTemp(ADisplayQuality: TClearTypeDisplayQuality; AInsertFont: TFont; AInsertText: WideString);
    function GetDrawMode: TDrawMode;
    procedure MoveImage(mi: TMoveImage);
    procedure SaveUndoBitmap;
    procedure SetModified(const Value: Boolean);
    procedure DrawMasked(Canvas: TCanvas; x, y: Integer; Bitmap: TBitmap);
    procedure SetTransparency(const Value: Boolean);
    function CheckIfIconCanBeEdited(s: TStream): Boolean;
    procedure LoadReadOnlyIcon(Stream: TStream);
  protected

    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure SelectAll;
    procedure ClearSelection;
    function CanRedo: Boolean;   // I4032
    procedure Redo;
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanUndo: Boolean;
    function CanSelectAll: Boolean;
    function CanClearSelection: Boolean;

  public
    const
      TransparentReplacementColour = $100000;  // I2634

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: WideString);
    procedure SaveToFile(FileName: WideString);
    procedure LoadFromStream(Stream: TStream; IsIcon: Boolean);
    procedure SaveToStream(Stream: TStream; IsIcon: Boolean);
    procedure Clear;

    procedure SetFocus; override;

    property Transparency: Boolean read FTransparency write SetTransparency;

    property DrawMode: TDrawMode read GetDrawMode;
    property Modified: Boolean read FModified write SetModified;
    property OnModifiedChanged: TNotifyEvent read FOnModifiedChanged write FOnModifiedChanged;
  end;

implementation

{$R *.DFM}

uses
  Clipbrd,
  JPEG,
  UfrmBitmapEditorText;

constructor TframeBitmapEditor.Create(AOwner: TComponent);
var
  beb: TBitmapEditorBMType;
begin
  inherited Create(AOwner);

  FIconCanBeEdited := True;
  panReadOnlyIcons.Visible := not FIconCanBeEdited;

  TPicture.RegisterClipboardFormat(cf_Bitmap, TBitmap);

  for beb := Low(beb) to High(beb) do
  begin
    FBitmaps[beb] := TBitmap.Create;
    FBitmaps[beb].SetSize(16,16);
    FBitmaps[beb].PixelFormat := pf24Bit;
    FBitmaps[beb].TransparentColor := TransparentReplacementColour; // I2634
    FBitmaps[beb].Transparent := True;
  end;

  FLastInsertFont := TFont.Create;
  FLastInsertFont.Name := 'Arial';
  FLastInsertFont.Size := 8;

  panEdit.EraseBackground := False;
  panPreview.EraseBackground := False;
  panEdit.DoubleBuffered := True;
  panPreview.DoubleBuffered := True;

  palColours.HintFormat :=
    'RGB(%r, %g, %b)'#13#10+
    'Hex: %hex'#13#10+
    'Click for foreground'#13#10+
    'Shift+Click for background';

  FEdit.Button := mbMiddle;
  FGColour := clBlack;
  BGColour := clWhite;
  DrawColours;

  dlgImport.Filter :=
    'Supported graphics formats (*.bmp,*.ico,*.jpg,*.jpeg,*.png)|*.bmp;*.ico;*.jpg;*.jpeg;*.png|'+
    'Bitmap files (*.bmp)|*.bmp|'+
    'Icon files (*.ico)|*.ico|'+
    'JPEG files (*.jpg,*.jpeg)|*.jpg;*.jpeg|'+
    'Portable Network Graphic files (*.png)|*.png|'+
    'All files (*.*)|*.*';
  dlgExport.Filter := dlgImport.Filter;
end;

destructor TframeBitmapEditor.Destroy;
var
  beb: TBitmapEditorBMType;
  i: Integer;
begin
  for beb := Low(beb) to High(beb) do
    FBitmaps[beb].Free;

  for i := 0 to High(FReadOnlyIcons) do
  begin
    FReadOnlyIcons[i].Free;
  end;
  SetLength(FReadOnlyIcons, 0);

  FLastInsertFont.Free;
  inherited Destroy;
end;

procedure TframeBitmapEditor.panEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  panEdit.SetFocus;

  FEdit.Button := Button;

  if not (Button in [mbLeft, mbRight]) then Exit;

  case GetDrawMode of
    dmLine, dmBox, dmFillBox, dmCircle, dmFillCircle, dmText: begin FEdit.IsTemp := True; CopyToTemp; end;
    else FEdit.IsTemp := False;
  end;

  if FEdit.IsTemp
    then FEdit.Bitmap := FBitmaps[bebTemp]
    else FEdit.Bitmap := FBitmaps[bebEdit];

  if Button = mbLeft then
  begin
    FEdit.IsMask := FGColour = clTransparent;
    if FEdit.IsMask
      then FEdit.Colour := TransparentReplacementColour  // I2634
      else FEdit.Colour := FGColour;
  end
  else
  begin
    FEdit.IsMask := BGColour = clTransparent;
    if FEdit.IsMask
      then FEdit.Colour := TransparentReplacementColour  // I2634
      else FEdit.Colour := BGColour;
  end;

  X1 := X * 16 div panEdit.ClientWidth;
  Y1 := Y * 16 div panEdit.ClientHeight;

  Modified := True;
  SaveUndoBitmap;

  with FEdit.Bitmap.Canvas do
    case GetDrawMode of
      dmDot:
        SetPixel(Handle, X1, Y1, FEdit.Colour);
      dmLine, dmBox, dmFillBox, dmCircle, dmFillCircle:
        SetPixel(Handle, X1, Y1, FEdit.Colour);
      dmFill:
        begin
          FEdit.Button := mbMiddle;
          Brush.Color := FEdit.Colour;
          Windows.ExtFloodFill(Handle, X1, Y1, GetPixel(Handle, X1, Y1), FLOODFILLSURFACE);
        end;
    end;

  panEdit.RePaint;
  panPreview.RePaint;
end;

procedure TframeBitmapEditor.panEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  X2, Y2: Integer;
  r: TRect;
  hObject, hOldObject: Thandle;
begin
  X2 := X * 16 div panEdit.ClientWidth;
  Y2 := Y * 16 div panEdit.ClientHeight;

  if FEdit.Button = mbMiddle then
  begin
    with FBitmaps[bebEdit].Canvas do
      lblPixel.Caption := FormatHint('#%hex (%r %g %b)', Pixels[X2, Y2]);
    Exit;
  end;


//  X2 := X div ((panEdit.ClientWidth * Screen.PixelsPerInch div 96) div 16);
//  Y2 := Y div ((panEdit.ClientHeight * Screen.PixelsPerInch div 96) div 16);

  if X2 < X1 then begin r.left := x2; r.right := x1+1; end
  else if X1 = X2 then begin r.left := x1; r.right := x2+1; end
  else begin r.left := x1; r.right := x2+1; end;

  if Y2 < Y1 then begin r.top := y2; r.bottom := y1+1; end
  else if Y1 = Y2 then begin r.top := y1; r.bottom := y2+1; end
  else begin r.top := y1; r.bottom := y2+1; end;

  hObject := 0;
  hOldObject := 0;

  with FEdit.Bitmap.Canvas do
  begin
    case GetDrawMode of
      dmDot:
        begin
          Pen.Color := FEdit.Colour;
          MoveTo(X1, Y1);
          LineTo(X2, Y2);
          Pixels[X2, Y2] := FEdit.Colour;
          {hObject := CreatePen(PS_SOLID, 0, FEdit.Colour);
          hOldObject := SelectObject(Handle, hObject);
          Windows.MoveToEx(Handle, X1, Y1, nil);
          Windows.LineTo(Handle, X2, Y2);
          Windows.SetPixel(Handle, X2, Y2, FEdit.Colour);}
          X1 := X2; Y1 := Y2;
        end;
      dmLine:
        begin
          CopyToTemp;
          hObject := CreatePen(PS_SOLID, 0, FEdit.Colour);
          hOldObject := SelectObject(Handle, hObject);
          Windows.MoveToEx(Handle, X1, Y1, nil);
          Windows.LineTo(Handle, X2, Y2);
          Windows.SetPixel(Handle, X2, Y2, FEdit.Colour);
        end;
      dmBox:
        begin
          CopyToTemp;
          hObject := CreatePen(PS_SOLID, 0, FEdit.Colour);
          hOldObject := SelectObject(Handle, hObject);
          Brush.Style := bsClear;
          Windows.Rectangle(Handle, r.left, r.top, r.right, r.bottom);
        end;
      dmFillBox:
        begin
          CopyToTemp;
          hObject := CreateSolidBrush(FEdit.Colour);
          Windows.FillRect(Handle, r, hObject);
        end;
      dmCircle:
        begin
          CopyToTemp;
          Brush.Style := bsClear;
          hObject := CreatePen(PS_SOLID, 0, FEdit.Colour);
          hOldObject := SelectObject(Handle, hObject);
          Windows.Ellipse(Handle, r.Left, r.Top, r.Right, r.Bottom);
        end;
      dmFillCircle:
        begin
          CopyToTemp;
          hObject := CreateSolidBrush(FEdit.Colour);
          hOldObject := SelectObject(Handle, hObject);
          Windows.Ellipse(Handle, r.Left, r.Top, r.Right, r.Bottom);
        end;
      dmText:
        begin
          X1 := X2; Y1 := Y2;
        end;
    end;
    if hOldObject <> 0 then
      SelectObject(Handle, hOldObject);
    if hObject <> 0 then
      DeleteObject(hObject);
  end;

  panEdit.RePaint;
  panPreview.RePaint;
end;

procedure TframeBitmapEditor.panEditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  if FEdit.Button = mbMiddle then Exit;

  if GetDrawMode = dmText then
  begin
    FLastInsertFont.Color := FEdit.Colour;

    r := panEdit.BoundsRect;
    r.TopLeft := ClientToScreen(r.TopLeft);
    r.BottomRight := ClientToScreen(r.BottomRight);
    with TfrmBitmapEditorText.Create(Self) do
    try
      ExcludeDisplayRect := r;
      InsertFont := FLastInsertFont;
      InsertText := FLastInsertText;
      DisplayQuality := FLastDisplayQuality;
      OnDrawPreview := DrawTextToTemp;
      if ShowModal = mrOk
        then DrawTextToTemp(DisplayQuality, InsertFont, InsertText)
        else FEdit.IsTemp := False;

      FLastDisplayQuality := DisplayQuality;
      FLastInsertText := InsertText;
      FLastInsertFont.Assign(InsertFont);
    finally
      Free;
    end;
  end;

  if FEdit.IsTemp then
  begin
    FEdit.IsTemp := False;
    CopyFromTemp;
  end;
  panEdit.RePaint;
  panPreview.RePaint;
  FEdit.Button := mbMiddle;
end;

procedure TframeBitmapEditor.palColoursCellClick(Button: TMouseButton;
  Shift: TShiftState; Index: Integer; AColor: TColor; var DontCheck: Boolean);
begin
  if ssShift in Shift
    then BGColour := AColor
    else FGColour := AColor;
  DrawColours;
  panEdit.SetFocus;
end;

procedure TframeBitmapEditor.DrawMasked(Canvas: TCanvas; x, y: Integer; Bitmap: TBitmap);
begin
  Canvas.Draw(X, Y, Bitmap);
end;

procedure TframeBitmapEditor.panPreviewPaint(Sender: TObject);
var
  x, y: Integer;
begin
  x := panPreview.Width * (Screen.PixelsPerInch div 96);
  y := panPreview.Height * (Screen.PixelsPerInch div 96);
  with panPreview.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(Rect(0,0,x,y));
    if FEdit.IsTemp
      then DrawMasked(panPreview.Canvas, (x-16) div 2, (y-16) div 2, FBitmaps[bebTemp])
      else DrawMasked(panPreview.Canvas, (x-16) div 2, (y-16) div 2, FBitmaps[bebEdit]);
  end;
end;

procedure TframeBitmapEditor.panEditPaint(Sender: TObject);
var
  i: Integer;
  bmp: TBitmap;
  X, Y: Integer;
begin
  bmp := TBitmap.Create;
  try
    bmp.Width := panEdit.ClientWidth;
    bmp.Height := panEdit.ClientHeight;
    bmp.PixelFormat := pf24bit;
    with bmp.Canvas do
    begin

      { Draw the transparent swatch }
      for Y := 0 to bmp.Height div imgTransparent.Height + 1 do
        for X := 0 to bmp.Width div imgTransparent.Width + 1 do
          Draw(X * imgTransparent.Width, Y * imgTransparent.Height, imgTransparent.Picture.Bitmap);


      if FEdit.IsTemp
        then StretchDraw(Rect(0,0,panEdit.ClientWidth, panEdit.ClientHeight), FBitmaps[bebTemp])
        else StretchDraw(Rect(0,0,panEdit.ClientWidth, panEdit.ClientHeight), FBitmaps[bebEdit]);
{        StretchBlt(Handle, 0, 0, panEdit.ClientWidth, panEdit.ClientHeight,
          FBitmaps[bebTemp].Canvas.Handle, 0, 0, 16, 16, SRCCOPY)
        else StretchBlt(Handle, 0, 0, panEdit.ClientWidth, panEdit.ClientHeight,
          FBitmaps[bebEdit].Canvas.Handle, 0, 0, 16, 16, SRCCOPY);}
          
      //x := panEdit.ClientWidth div 16; //* Screen.PixelsPerInch div 96) div 16;
      for i := 1 to 16 do
      begin
        MoveTo(i * panEdit.ClientWidth div 16, 0);
        LineTo(i * panEdit.ClientWidth div 16, panEdit.ClientHeight);
        MoveTo(0, i * panEdit.ClientHeight div 16);
        LineTo(panEdit.ClientWidth, i * panEdit.ClientHeight div 16);
      end;

      if (GetDrawMode = dmText) and FEdit.IsTemp then
      begin
        MoveTo(X1 * panEdit.ClientWidth div 16 + 1, Y1 * panEdit.ClientHeight div 16 + 1);
        LineTo(X1 * panEdit.ClientWidth div 16 + 1, Y1 * panEdit.ClientHeight div 16 + 13);
        LineTo(X1 * panEdit.ClientWidth div 16 + 13, Y1 * panEdit.ClientHeight div 16 + 13);
      end;
    end;
    BitBlt(panEdit.Canvas.Handle, 0, 0, panEdit.ClientWidth, panEdit.ClientHeight, bmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    bmp.Free;
  end;
end;

procedure TframeBitmapEditor.cmdClearAllClick(Sender: TObject);
begin
  ClearSelection;
end;

procedure TframeBitmapEditor.cmdClick(Sender: TObject);
begin
  FEdit.IsTemp := False;
  panEdit.RePaint;
  panPreview.RePaint;
  panEdit.SetFocus;
end;

procedure IconFromBitmap(Bmp:TBitmap;Icon:TIcon);
var
  il:TImageList;
begin
  il := TImageList.CreateSize(bmp.Width,bmp.Height);
  try
    il.AddMasked(bmp,bmp.TransparentColor);
    il.GetIcon(0,Icon);
  finally
    il.Free;
  end;
end;

procedure TframeBitmapEditor.cmdExportClick(Sender: TObject);
var
  ext: WideString;
  ico: TIcon;
begin
  if dlgExport.Execute then
  begin
    ext := LowerCase(ExtractFileExt(dlgExport.FileName));
    if ext = '.ico' then
    begin
      ico := TIcon.Create;
      try
        IconFromBitmap(FBitmaps[bebEdit], ico);
        ico.SaveToFile(dlgExport.FileName);
      finally
        ico.Free;
      end;
    end
    else if (ext = '.jpg') or (ext = '.jpeg') then
    begin
      with TJPEGImage.Create do
      try
        CompressionQuality := 100;
        Assign(FBitmaps[bebEdit]);
        SaveToFile(dlgExport.FileName);
      finally
        Free;
      end;
    end
    else if (ext = '.bmp') then
      FBitmaps[bebEdit].SaveToFile(dlgExport.FileName)
    else if (ext = '.png') then
      with TPNGImage.Create do
      try
        Assign(FBitmaps[bebEdit]);
        SaveToFile(dlgExport.FileName);
      finally
        Free;
      end
    else
      ShowMessage('The file format '+ext+' is not supported.');
  end;
  panEdit.SetFocus;
end;

procedure TframeBitmapEditor.cmdMoveUpClick(Sender: TObject);
begin
  MoveImage(miUp);
end;

procedure TframeBitmapEditor.cmdMoveLeftClick(Sender: TObject);
begin
  MoveImage(miLeft);
end;

procedure TframeBitmapEditor.cmdMoveRightClick(Sender: TObject);
begin
  MoveImage(miRight);
end;

procedure TframeBitmapEditor.cmdMoveDownClick(Sender: TObject);
begin
  MoveImage(miDown);
end;

procedure TframeBitmapEditor.DrawColours;
begin
  if FGColour = clTransparent
    then cpForeground.Color := clNone
    else cpForeground.Color := FGColour;
  if BGColour = clTransparent
    then cpBackground.Color := clNone
    else cpBackground.Color := BGColour;
end;

procedure TframeBitmapEditor.CopyToTemp;
begin
  FBitmaps[bebTemp].Assign(FBitmaps[bebEdit]);
end;

procedure TframeBitmapEditor.CopyFromTemp;
begin
  FBitmaps[bebEdit].Assign(FBitmaps[bebTemp]);
end;

function TframeBitmapEditor.GetDrawMode: TDrawMode;
begin
  if cmdPen.Down then Result := dmDot
  else if cmdLine.Down then Result := dmLine
  else if cmdFill.Down then Result := dmFill
  else if cmdText.Down then Result := dmText
  else if cmdBox.Down then Result := dmBox
  else if cmdFillBox.Down then Result := dmFillBox
  else if cmdCircle.Down then Result := dmCircle
  else if cmdFillCircle.Down then Result := dmFillCircle
  else Result := dmDot;
end;

procedure TframeBitmapEditor.MoveImage(mi: TMoveImage);
begin
  FEdit.IsTemp := False;
  case mi of
    miLeft:
      begin
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 0, 15, 16, FBitmaps[bebEdit].Canvas.Handle, 1, 0, SRCCOPY);
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 15, 0, 1, 16, FBitmaps[bebEdit].Canvas.Handle, 0, 0, SRCCOPY);
      end;
    miUp:
      begin
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 0, 16, 15, FBitmaps[bebEdit].Canvas.Handle, 0, 1, SRCCOPY);
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 15, 16, 1, FBitmaps[bebEdit].Canvas.Handle, 0, 0, SRCCOPY);
      end;
    miRight:
      begin
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 1, 0, 15, 16, FBitmaps[bebEdit].Canvas.Handle, 0, 0, SRCCOPY);
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 0,  1, 16, FBitmaps[bebEdit].Canvas.Handle, 15, 0, SRCCOPY);
      end;
    miDown:
      begin
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 1, 16, 15, FBitmaps[bebEdit].Canvas.Handle, 0, 0, SRCCOPY);
        BitBlt(FBitmaps[bebTemp].Canvas.Handle, 0, 0, 16,  1, FBitmaps[bebEdit].Canvas.Handle, 0, 15, SRCCOPY);
      end;
  end;
  FBitmaps[bebEdit].Canvas.Draw(0, 0, FBitmaps[bebTemp]);
  panEdit.RePaint;
  panPreview.RePaint;
  panEdit.SetFocus;
end;

procedure TframeBitmapEditor.ppReadOnlyIconsPaint(Sender: TObject);
var
  i, x: Integer;
begin
  if FRSP21318IsPngIcon then
    Exit;

  x := 8;

  for i := 0 to High(FReadOnlyIcons) do
  begin
    ppReadOnlyIcons.Canvas.Draw(x, 8, FReadOnlyIcons[i]);
    Inc(x, FReadOnlyIcons[i].Width + 4);
  end;
end;

procedure TframeBitmapEditor.SaveUndoBitmap;
begin
  FBitmaps[bebUndo].Assign(FBitmaps[bebEdit]);
end;

procedure TframeBitmapEditor.DrawTextToTemp(ADisplayQuality: TClearTypeDisplayQuality; AInsertFont: TFont; AInsertText: WideString);
begin
  if not FEdit.IsTemp then Exit;
  CopyToTemp;
  SetBkMode(FBitmaps[bebTemp].Canvas.Handle, TRANSPARENT);
  with TClearTypeDrawCharacter.Create do
  try
    BeginUpdate;
    SetFontDetails(AInsertFont.Name, AInsertFont.Height, AInsertFont.Style);
    Color := AInsertFont.Color;
    DisplayQuality := ADisplayQuality;
    EndUpdate;
    DrawText(FBitmaps[bebTemp].Canvas.Handle, TA_LEFT or TA_BASELINE, X1, Y1, Rect(0,0,16,16), AInsertText);
  finally
    Free;
  end;
  panEdit.RePaint;
  panPreview.RePaint;
end;

procedure TframeBitmapEditor.Undo;
var
  b: TBitmap;
begin
  if (GetDrawMode = dmText) then FEdit.IsTemp := False;
  b := FBitmaps[bebEdit];
  FBitmaps[bebEdit] := FBitmaps[bebUndo];
  FBitmaps[bebUndo] := b;
  Modified := True;
  panEdit.RePaint;
  panPreview.RePaint;
end;

procedure TframeBitmapEditor.CopyToClipboard;
var
  MyFormat: Word;
  AData: THandle;
  APalette: HPalette;
begin
  FBitmaps[bebEdit].SaveToClipBoardFormat(MyFormat,AData,APalette);
  ClipBoard.SetAsHandle(MyFormat,AData);
end;

procedure TframeBitmapEditor.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TframeBitmapEditor.PasteFromClipboard;
var
  FClipBmp2, FClipBmp: TBitmap;
begin
  FClipBmp := TBitmap.Create;
  FClipBmp2 := TBitmap.Create;
  try
    FClipBmp2.Width := 16;
    FClipBmp2.Height := 16;
    if Clipboard.HasFormat(cf_Bitmap) then
      FClipBmp.LoadFromClipboardFormat(cf_BitMap,ClipBoard.GetAsHandle(cf_Bitmap),0)
    else
      Exit;

    SetStretchBltMode(FClipBmp2.Canvas.Handle, HALFTONE);
    StretchBlt(FClipBmp2.Canvas.Handle, 0, 0, 16, 16, FClipBmp.Canvas.Handle, 0, 0, FClipBmp.Width, FClipBmp.Height,
      SRCCOPY);
    BitBlt(FBitmaps[bebEdit].Canvas.Handle, 0, 0, 16, 16, FClipBmp2.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    FClipBmp2.Free;
    FClipBmp.Free;
  end;
  Modified := True;
  panEdit.RePaint;
  panPreview.RePaint;
end;

procedure TframeBitmapEditor.Redo;   // I4032
begin
end;

procedure TframeBitmapEditor.LoadFromFile(FileName: WideString);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
  except
    on E:EFileStreamError do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  try
    LoadFromStream(Stream, SameStr(ExtractFileExt(FileName), '.ico'));
  finally
    Stream.Free;
  end;
end;

function IsPNGSignature(const buf: array of byte): Boolean;
begin
  Result :=
    (buf[0] = $89) and
    (buf[1] = $50) and
    (buf[2] = $4E) and
    (buf[3] = $47);
end;

function TframeBitmapEditor.CheckIfIconCanBeEdited(s: TStream): Boolean;
var
  p: Int64;
  ci: TCursorOrIcon;
  header: TIconRec;
  buf: array[0..3] of byte;
begin
  p := s.Position;
  try
    if s.Read(ci, SizeOf(TCursorOrIcon)) <> SizeOf(TCursorOrIcon) then
      Exit(False);

    if (ci.wType <> rc3_Icon) or (ci.Count <> 1) then
      Exit(False);

    // We need to read the header of that icon
    if not s.Read(header, SizeOf(header)) = sizeof(header) then
      Exit(False);

    if (header.Width <> 16) or (header.Height <> 16) then
      Exit(False);

    // Check that it's not a PNG format image
    s.Position := header.DIBOffset;
    if s.Read(buf, 4) <> 4 then
      Exit(False);

    FRSP21318IsPngIcon := IsPNGSignature(buf);
    Result := not FRSP21318IsPngIcon;
  finally
    s.Position := p;
  end;
end;

procedure TframeBitmapEditor.LoadFromStream(Stream: TStream; IsIcon: Boolean);
var
  FIcon: TIcon;
begin
  if IsIcon then
  begin
    FIconCanBeEdited := CheckIfIconCanBeEdited(Stream);
    if not FIconCanBeEdited then
      LoadReadOnlyIcon(Stream);

    if not FRSP21318IsPngIcon then
    begin
      Transparency := True;
      FBitmaps[bebEdit].Canvas.Brush.Color := TransparentReplacementColour;  // I2634
      FBitmaps[bebEdit].Canvas.FillRect(Rect(0,0,16,16));
      FIcon := TIcon.Create;
      try
        try
          FIcon.LoadFromStream(Stream);
          DrawIconEx(FBitmaps[bebEdit].Canvas.Handle, 0, 0, FIcon.Handle, 16, 16, 0, 0, DI_NORMAL);
        except
          on E:EOutOfResources do
            ShowMessage(E.Message);
          on E:EInvalidGraphic do
            ShowMessage(E.Message);
        end;
      finally
        FIcon.Free;
      end;
    end;
  end
  else
  begin
    FIconCanBeEdited := True;
    Transparency := False;
    try
      FBitmaps[bebEdit].LoadFromStream(Stream);
    except
      on E:EInvalidGraphic do
        ShowMessage(E.Message);
    end;
    FBitmaps[bebEdit].PixelFormat := pf24Bit;
    FBitmaps[bebEdit].SetSize(16,16);
  end;
  Modified := False;
  FEdit.IsTemp := False;

  panReadOnlyIcons.Visible := not FIconCanBeEdited;
  panEdit.RePaint;
  panPreview.RePaint;
  SaveUndoBitmap;
end;

(**
  Loads a view of the icons available in a .ico file

  Some of this code mirrors Vcl.Graphics ReadIcon
  because WinAPI doesn't have an API to read .ico
  files that supports multiple images in a single file;
  while there are possible COM APIs available, we don't
  want to add a dependency just to read the icon data.
*)
procedure TframeBitmapEditor.LoadReadOnlyIcon(Stream: TStream);
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array[0..300] of TIconRec;
const
  InvalidIconMessage = 'This icon cannot be loaded; it may be corrupt or an unsupported format';
var
  ci: TCursorOrIcon;
  List: PIconRecArray;
  HeaderLen: Integer;
  i, w, h: Integer;
  cd: string;
  p: Int64;
  buf: array[0..3] of byte;
begin
  memoReadOnlyIcons.Clear;
  p := Stream.Position;
  Stream.ReadBuffer(ci, SizeOf(TCursorOrIcon));
  if (ci.Reserved <> 0) or (ci.wType <> rc3_Icon) or (ci.Count > 16) then
  begin
    // This is either not an icon file, or there are too many icons in the
    // file
    memoReadOnlyIcons.Lines.Add(InvalidIconMessage);
    Stream.Position := p;
    Exit;
  end;

  HeaderLen := SizeOf(TIconRec) * ci.Count;
  List := AllocMem(HeaderLen);
  try
    Stream.Read(List^, HeaderLen);
    SetLength(FReadOnlyIcons, ci.Count);
    for i := 0 to ci.Count - 1 do
    begin
      if List[i].Colors = 0
        then cd := '24 bit'
        else cd := IntToStr(List[i].Colors);
      if List[i].Width = 0 then w := 256 else w := List[i].Width;
      if List[i].Height = 0 then h := 256 else h := List[i].Height;

      if (Stream.Seek(List[i].DIBOffset, soBeginning) <> List[i].DIBOffset) or
        (Stream.Read(buf, 4) <> 4) then
      begin
        memoReadOnlyIcons.Lines.Add(InvalidIconMessage);
        Exit;
      end;
      if IsPNGSignature(buf) then
      begin
        // Loads a PNG-format icon from the offset of
        // the file for the specific icon
        Stream.Position := List[i].DIBOffset;
        FReadOnlyIcons[i] := TPngImage.Create;
        try
          FReadOnlyIcons[i].LoadFromStream(Stream);
        except
          on E:Exception do   // I don't want to use E:Exception but see http://marc.durdin.net/2012/01/how-not-to-do-exception-classes-in.html
          begin
            memoReadOnlyIcons.Lines.Add(InvalidIconMessage);
            Exit;
          end;
        end;
      end
      else
      begin
        // Uses Delphi's icon loading function to get the correct icon
        // from the stream
        Stream.Position := p;
        FReadOnlyIcons[i] := TIcon.Create;
        FReadOnlyIcons[i].SetSize(w, h);
        try
          FReadOnlyIcons[i].LoadFromStream(Stream);
        except
          on E:EInvalidGraphic do
          begin
            memoReadOnlyIcons.Lines.Add(InvalidIconMessage);
            Exit;
          end;
        end;
      end;

      memoReadOnlyIcons.Lines.Add(Format('Icon #%d: %d x %d, %s colour',
        [i+1, w, h, cd]));
    end;
  finally
    FreeMem(List);
    Stream.Position := p;
  end;
end;

procedure TframeBitmapEditor.SaveToFile(FileName: WideString);
var
  Stream: TStream;
begin
  if FIconCanBeEdited then
  begin
    try
      Stream := TFileStream.Create(FileName, fmCreate);
    except
      on E:EFileStreamError do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;

    try
      SaveToStream(Stream, SameStr(ExtractFileExt(FileName), '.ico'));
    finally
      Stream.Free;
    end;
  end;
end;

procedure TframeBitmapEditor.SaveToStream(Stream: TStream; IsIcon: Boolean);
var
  FIcon: TIcon;
begin
  if IsIcon then
  begin
    Transparency := True;
    FIcon := TIcon.Create;
    try
      IconFromBitmap(FBitmaps[bebEdit], FIcon);
      FIcon.SaveToStream(Stream);
    finally
      FIcon.Free;
    end;
  end
  else
  begin
    Transparency := False;
    FBitmaps[bebEdit].SaveToStream(Stream);
  end;
  Modified := False;
end;

procedure TframeBitmapEditor.SelectAll;
begin

end;

procedure TframeBitmapEditor.SetFocus;
begin
  panEdit.SetFocus;
end;

procedure TframeBitmapEditor.SetModified(const Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if Assigned(FOnModifiedChanged) then FOnModifiedChanged(Self);
  end;
end;

procedure TframeBitmapEditor.SetTransparency(const Value: Boolean);
var
  beb: TBitmapEditorBMType;
begin
  FTransparency := Value;

  if FTransparency then
  begin
    if palColours.Colors[0] <> 'clTransparent' then
      palColours.Colors.Insert(0, 'clTransparent');
  end
  else
  begin
    if palColours.Colors[0] = 'clTransparent' then
      palColours.Colors.Delete(0);
    if FGColour = clTransparent then FGColour := clBlack;
    if BGColour = clTransparent then BGColour := clBlack;
    DrawColours;
  end;

  for beb := Low(beb) to High(beb) do
    FBitmaps[beb].Transparent := FTransparency;
end;

procedure TframeBitmapEditor.cmdPenClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure TframeBitmapEditor.cmdLineClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure TframeBitmapEditor.cmdFillClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure IconToBitmap(Bitmap: TBitmap; Ico: TIcon);
begin
  Bitmap.Height := Ico.Height;
  Bitmap.Width := Ico.Width;
  Bitmap.Canvas.Draw(0, 0, Ico);
end;

procedure TframeBitmapEditor.cmdImportClick(Sender: TObject);
var
  ext: WideString;
  ico: TIcon;
  ji: TJPEGImage;
begin
  if not dlgImport.Execute then Exit;  // I3147   // I3508

  SaveUndoBitmap;
  FEdit.IsTemp := False;

  ext := LowerCase(ExtractFileExt(dlgImport.FileName));
  if ext = '.ico' then
  begin
    ico := TIcon.Create;
    try
      try
        ico.LoadFromFile(dlgImport.FileName);
      except
        on E:EInvalidGraphic do  // I3147   // I3508
        begin
          ShowMessage('The image file could not be imported: '+E.Message);
          Exit;
        end;
      end;
      IconToBitmap(FBitmaps[bebEdit], ico);
    finally
      ico.Free;
    end;
  end
  else if (ext = '.jpg') or (ext = '.jpeg') then
  begin
    ji := TJPEGImage.Create;
    with ji do
    try
      try
        LoadFromFile(dlgImport.FileName);
      except
        on E:EInvalidGraphic do  // I3147   // I3508
        begin
          ShowMessage('The image file could not be imported: '+E.Message);
          Exit;
        end;
      end;
      FBitmaps[bebEdit].Assign(ji);
    finally
      Free;
    end;
  end
  else if (ext = '.bmp') then
  try
    FBitmaps[bebEdit].LoadFromFile(dlgImport.FileName)
  except
    on E:EInvalidGraphic do  // I3147   // I3508
    begin
      ShowMessage('The image file could not be imported: '+E.Message);
      Exit;
    end;
  end
  else if (ext = '.png') then
    with TPNGImage.Create do
    try
      try
        LoadFromFile(dlgImport.FileName);
      except
        on E:Exception do   // I3147 - I don't want to use E:Exception but see http://marc.durdin.net/2012/01/how-not-to-do-exception-classes-in.html   // I3508
        begin
          ShowMessage('The image file could not be imported because it is not a valid PNG file: '+E.Message);
          Exit;
        end;
      end;
      AssignTo(FBitmaps[bebEdit]);
    finally
      Free;
    end
  else
  begin
    ShowMessage('The file format '+ext+' is not supported.');
    Exit;
  end;

  Modified := True;
  FBitmaps[bebEdit].PixelFormat := pf24Bit;
  SetStretchBltMode(FBitmaps[bebTemp].Canvas.Handle, HALFTONE);
  FBitmaps[bebTemp].Canvas.StretchDraw(Rect(0,0,16,16), FBitmaps[bebEdit]);
  CopyFromTemp;
  panEdit.Repaint;
  panPreview.Repaint;
  panEdit.SetFocus;
end;

procedure TframeBitmapEditor.cmdBoxClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure TframeBitmapEditor.cmdFillBoxClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure TframeBitmapEditor.cmdTextClick(Sender: TObject);
begin
  cmdClick(Sender);
end;

procedure TframeBitmapEditor.actOpenPaletteExecute(Sender: TObject);
begin
  if dlgOpenPalette.Execute then
    palColours.Palette := dlgOpenPalette.FileName;
end;

function TframeBitmapEditor.CanClearSelection: Boolean;
begin
  Result := True;
end;

function TframeBitmapEditor.CanCopy: Boolean;
begin
  Result := True;
end;

function TframeBitmapEditor.CanCut: Boolean;
begin
  Result := True;
end;

function TframeBitmapEditor.CanPaste: Boolean;
begin
  Result := Clipboard.HasFormat(cf_Bitmap);
end;

function TframeBitmapEditor.CanRedo: Boolean;   // I4032
begin
  Result := False;
end;

function TframeBitmapEditor.CanSelectAll: Boolean;
begin
  Result := False;
end;

function TframeBitmapEditor.CanUndo: Boolean;
begin
  Result := Modified;
end;

procedure TframeBitmapEditor.Clear;
begin
  ClearSelection;
  FEdit.IsTemp := False;
  SaveUndoBitmap;
end;

procedure TframeBitmapEditor.ClearSelection;
var
  hObject: THandle;
  FColor: DWord;
begin
  Modified := True;
  SaveUndoBitmap;

  if BGColour = clTransparent
    then FColor := TransparentReplacementColour
    else FColor := BGColour;

  hObject := CreateSolidBrush(FColor);
  Windows.FillRect(FBitmaps[bebEdit].Canvas.Handle, Rect(0,0,16,16), hObject);
  DeleteObject(hObject);

  panEdit.RePaint;
  panPreview.RePaint;
end;

end.
