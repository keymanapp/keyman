(*
  Name:             VisualKeyboardParameters
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    28 Feb 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Rework button rendering for new visual keyboard
                    14 Sep 2006 - mcdurdin - Increase cap font size by one pixel
                    19 Mar 2007 - mcdurdin - I717 - Fix handle leak for D
                    04 Jun 2007 - mcdurdin - I764 - Add European ENTER key support
                    06 Apr 2010 - mcdurdin - I2284 - Keyman 8 graphics
                    18 May 2012 - mcdurdin - I3311 - V9.0 - Change 'published' to 'public' on classes that don't need RTTI
                    28 Feb 2014 - mcdurdin - I4098 - V9.0 - OSK is still 8.0 style
*)
unit VisualKeyboardParameters;

interface

uses
  System.Types,
  Classes,
  Windows,
  Graphics,
  SysUtils;

type
  TKeyBtnParameters = record
    Name: WideString;
    Border: array[0..1] of TRect;                      // width of the border of the key, when key is up, down
    CapPos: array[0..1] of TPoint;        // left,top of std key text, up, down
    TextRect: array[0..1] of TRect;       // rect of glyph/output text position, up, down - aligned to bottom-right
    Overlap: TPoint;                      // amount by which keys overlap
    CapFont: string;                      // font name for key cap
    CapFontSize: Integer;                 // font size for key cap (TFont.Size)
    BitmapFileName: string;               // file name for the image (or a resource name if it starts with :)
    BitmapTransparentColor: TColor;
  end;

  TKeyBitmapInfo = record
    Bitmap: TBitmap;
    Params: TKeyBtnParameters;
  end;

  EKeyBtnParameters = class(Exception)
  end;

type
  TKeyBitmapDetails = record
    Name, ResourceName, ParamStr: string;
  end;
    
type
  TActiveKeyBtnBitmap = class
  private
    FBitmap: TBitmap;
    FParameters: TKeyBtnParameters;
  public  // I3311
    constructor Create(Parameters: TKeyBtnParameters);
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap;
    property Parameters: TKeyBtnParameters read FParameters;
  end;

const
  FKeyBitmapDetails: array[0..3] of TKeyBtnParameters = (
    (Name: 'Smooth';
     Border: ((Left:5; Top:5; Right:5; Bottom:5),(Left:5;Top:5;Right:5;Bottom:5));
     CapPos: ((X:5;Y:5),(X:6;Y:6));
     TextRect: ((Left:4;Top:10;Right:28;Bottom:27),(Left:4;Top:11;Right:28;Bottom:28));
     Overlap: (X:0;Y:0);
     CapFont: 'Tahoma';
     CapFontSize: 8;
     BitmapFileName: ':KEYCAPSMOOTH'),
    (Name: 'Keyman 7';
     Border: ((Left:6; Top:5; Right:6; Bottom:7),(Left:6;Top:6;Right:6;Bottom:6));
     CapPos: ((X:4;Y:3),(X:4;Y:4));
     TextRect: ((Left:4;Top:12;Right:30;Bottom:30),(Left:4;Top:13;Right:30;Bottom:31));
     Overlap: (X:0;Y:0);
     CapFont: 'Tahoma';
     CapFontSize: 8;
     BitmapFileName: ':ONSCREENKEYBOARD_BMP';
     BitmapTransparentColor: $FF00FF),
    (Name: 'Keyman 8';
     Border: ((Left:6; Top:5; Right:6; Bottom:7),(Left:6;Top:6;Right:6;Bottom:6));
     CapPos: ((X:4;Y:3),(X:4;Y:4));
     TextRect: ((Left:4;Top:12;Right:30;Bottom:30),(Left:4;Top:13;Right:30;Bottom:31));
     Overlap: (X:0;Y:0);
     CapFont: 'Tahoma';
     CapFontSize: 8;
     BitmapFileName: ':ONSCREENKEYBOARD_CHARCOAL_BMP';
     BitmapTransparentColor: $4f4536),
    (Name: 'Keyman 9';   // I4098
     Border: ((Left:6; Top:6; Right:6; Bottom:6),(Left:6;Top:6;Right:6;Bottom:6));
     CapPos: ((X:3;Y:3),(X:4;Y:4));
     TextRect: ((Left:3;Top:12;Right:30;Bottom:30),(Left:4;Top:13;Right:31;Bottom:31));
     Overlap: (X:0;Y:0);
     CapFont: 'Calibri';
     CapFontSize: 8;
     BitmapFileName: ':ONSCREENKEYBOARD_FLAT_BMP';
     BitmapTransparentColor: $f2f2f1)
    );

function KeyBtnParametersFromXML(Params: WideString): TKeyBtnParameters;
function KeyBtnParametersToXML(Params: TKeyBtnParameters): WideString;

//function KeyBtnParametersFromString(Params: string): TKeyBtnParameters;
//function KeyBtnParametersFromRecord(Params: TKeyBtnParameters): string;
procedure DrawKeyParam(Canvas: TCanvas; Border: TRect; X, Y, Width, Height, Index: Integer; FKeyBitmap: TBitmap);
procedure DrawEuropeanEnterKeyParam(Canvas: TCanvas; Border: TRect; X, Y, Width, Height, Index: Integer; FKeyBitmap: TBitmap; X2, Y2: Integer);

function OnScreen_KeyBitmap: TActiveKeyBtnBitmap;

implementation

uses
  xmldoc, xmlintf;

var
  FActiveKeyBitmap: TActiveKeyBtnBitmap = nil;

function OnScreen_KeyBitmap: TActiveKeyBtnBitmap;
begin
  if not Assigned(FActiveKeyBitmap) then
    FActiveKeyBitmap := TActiveKeyBtnBitmap.Create(FKeyBitmapDetails[3]);   // I4098
  Result := FActiveKeyBitmap;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
  hOldMemBmp: Cardinal;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    hOldMemBmp := CreateCompatibleBitmap(DstDC, 1, 1);
    MemBmp := SelectObject(MaskDC, hOldMemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      SelectObject(MaskDC, MemBmp);
      DeleteObject(hOldMemBmp);
    end;
    Exit;
  end;
  SavePal := 0;
  MemDC := CreateCompatibleDC(0);
  try
    MemBmp := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
    Save := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    SelectPalette(SrcDC, SavePal, False);
    if SavePal <> 0 then
      SavePal := SelectPalette(MemDC, SavePal, True)
    else
      SavePal := SelectPalette(MemDC, SystemPalette16, True);
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if Save <> 0 then SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if SavePal <> 0 then SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

function KeyBtnParametersFromXML(Params: WideString): TKeyBtnParameters;
    function RectElem(name: WideString; root: IXMLNode): TRect;
    var
      n: Integer;
    begin
      Result := Rect(0,0,0,0);
      n := root.ChildNodes.IndexOf(name);
      if n >= 0 then
      begin
        if root.ChildNodes[n].AttributeNodes.IndexOf('Left') >= 0 then Result.Left := root.ChildNodes[n].Attributes['Left'];
        if root.ChildNodes[n].AttributeNodes.IndexOf('Top') >= 0 then Result.Top := root.ChildNodes[n].Attributes['Top'];
        if root.ChildNodes[n].AttributeNodes.IndexOf('Right') >= 0 then Result.Right := root.ChildNodes[n].Attributes['Right'];
        if root.ChildNodes[n].AttributeNodes.IndexOf('Bottom') >= 0 then Result.Bottom := root.ChildNodes[n].Attributes['Bottom'];
      end;
    end;

    function PtElem(name: WideString; root: IXMLNode): TPoint;
    var
      n: Integer;
    begin
      Result := Point(0,0);
      n := root.ChildNodes.IndexOf(name);
      if n >= 0 then
      begin
        if root.ChildNodes[n].AttributeNodes.IndexOf('X') >= 0 then Result.X := root.ChildNodes[n].Attributes['X'];
        if root.ChildNodes[n].AttributeNodes.IndexOf('Y') >= 0 then Result.Y := root.ChildNodes[n].Attributes['Y'];
      end;
    end;
var
  root: IXMLNode;
  n: Integer;
begin
  Result := FKeyBitmapDetails[0];

  with LoadXMLData(Params) do
  begin
    root := DocumentElement.ChildNodes['OnScreenKey'];
    Result.Border[0] := RectElem('BorderUp', root);
    Result.Border[1] := RectElem('BorderDown', root);
    Result.CapPos[0] := PtElem('CapPosUp', root);
    Result.CapPos[1] := PtElem('CapPosDown', root);
    Result.TextRect[0] := RectElem('TextRectUp', root);
    Result.TextRect[1] := RectElem('TextRectDown', root);
    Result.Overlap := PtElem('Overlap', root);

    n := root.ChildNodes.IndexOf('CapFont');
    if n >= 0 then
    begin
      if root.ChildNodes[n].AttributeNodes.IndexOf('Name') >= 0 then Result.CapFont := root.ChildNodes[n].Attributes['Name'];
      if root.ChildNodes[n].AttributeNodes.IndexOf('Size') >= 0 then Result.CapFontSize := root.ChildNodes[n].Attributes['Size'];
    end;
    n := root.ChildNodes.IndexOf('Bitmap');
    if n >= 0 then
    begin
      if root.ChildNodes[n].AttributeNodes.IndexOf('FileName') >= 0 then Result.BitmapFilename := root.ChildNodes[n].Attributes['FileName'];
    end;
  end;
end;

function KeyBtnParametersToXML(Params: TKeyBtnParameters): WideString;
    procedure AddRect(root: IXMLNode; name: WideString; r: TRect);
    begin
      with root.AddChild(name) do
      begin
        Attributes['Left'] := r.Left;
        Attributes['Top'] := r.Top;
        Attributes['Right'] := r.Right;
        Attributes['Bottom'] := r.Bottom;
      end;
    end;

    procedure AddPt(root: IXMLNode; name: WideString; p: TPoint);
    begin
      with root.AddChild(name) do
      begin
        Attributes['X'] := p.X;
        Attributes['Y'] := p.Y;
      end;
    end;
var
  root: IXMLNode;
begin
  with NewXMLDocument do
  begin
    root := DocumentElement.AddChild('OnScreenKey');
    AddRect(root, 'BorderUp', Params.Border[0]);
    AddRect(root, 'BorderDown', Params.Border[1]);
    AddPt(root, 'CapPosUp', Params.CapPos[0]);
    AddPt(root, 'CapPosDown', Params.CapPos[1]);
    AddRect(root, 'TextRectUp', Params.TextRect[0]);
    AddRect(root, 'TextRectDown', Params.TextRect[1]);
    AddPt(root, 'Overlap', Params.Overlap);
    with root.AddChild('CapFont') do
    begin
      Attributes['Name'] := Params.CapFont;
      Attributes['Size'] := Params.CapFontSize;
    end;
    with root.AddChild('Bitmap') do Attributes['FileName'] := Params.BitmapFilename;
    SaveToXML(Result);
  end;
end;

function KeyBtnParametersFromRecord(Params: TKeyBtnParameters): string;
begin
  with Params do
    Result := Format('borderup(%d,%d,%d,%d) borderdown(%d,%d,%d,%d) cappos(%d,%d,%d,%d) textrectup(%d,%d,%d,%d) textrectdown (%d,%d,%d,%d)'+
      ' overlap(%d,%d) capfont(%s,%d)',
      [Border[0].Left, Border[0].Top, Border[0].Right, Border[0].Bottom,
      Border[1].Left, Border[1].Top, Border[1].Right, Border[1].Bottom,
      CapPos[0].X, CapPos[0].Y,
      CapPos[1].X, CapPos[1].Y,
      TextRect[0].Left, TextRect[0].Top, TextRect[0].Right, TextRect[0].Bottom,
      TextRect[1].Left, TextRect[1].Top, TextRect[1].Right, TextRect[1].Bottom,
      Overlap.X, Overlap.Y,
      CapFont, CapFontSize]);
end;

function KeyBtnParametersFromString(Params: string): TKeyBtnParameters;
    function GetTag(var txt, tag, res: string): Boolean;
    var
      n: Integer;
    begin
      Result := False;
      txt := Trim(txt);
      n := Pos('(', txt);
      if n = 0 then Exit;
      tag := LowerCase(Copy(txt, 1, n-1));
      Delete(txt,1,n);
      n := Pos(')', txt);
      if n = 0 then Exit;
      res := Copy(txt, 1, n-1);
      Delete(txt, 1, n);
      Result := True;
    end;

    function TagString(var tag: string): string;
    var
      n: Integer;
    begin
      tag := Trim(tag);
      n := Pos(',', tag);
      if n = 0 then
      begin
        Result := tag;
        tag := '';
      end
      else
      begin
        Result := Copy(tag, 1, n-1);
        Delete(tag, 1, n);
      end;
    end;

    function TagInteger(var tag: string): Integer;
    begin
      Result := StrToIntDef(TagString(tag), 0);
    end;

    function TagRect(var tag: string): TRect;
    begin
      Result.Left   := TagInteger(tag);
      Result.Top    := TagInteger(tag);
      Result.Right  := TagInteger(tag);
      Result.Bottom := TagInteger(tag);
    end;

    function TagPoint(var tag: string): TPoint;
    begin
      Result.X := TagInteger(tag);
      Result.Y := TagInteger(tag);
    end;
var
  v, s, t: string;
  FParameters: TKeyBtnParameters;
begin
  v := Params;
  FillChar(FParameters, sizeof(FParameters), 0);
  while GetTag(v, s, t) do
  begin
    if s = 'borderup' then        FParameters.Border[0] := TagRect(t)
    else if s = 'borderdown' then FParameters.Border[1] := TagRect(t)
    else if s = 'cappos' then    begin FParameters.CapPos[0] := TagPoint(t); FParameters.CapPos[1] := TagPoint(t); end
    else if s = 'textrectup' then   FParameters.TextRect[0] := TagRect(t)
    else if s = 'textrectdown' then   FParameters.TextRect[1] := TagRect(t)
    else if s = 'overlap' then    FParameters.Overlap := TagPoint(t)
    else if s = 'capfont' then    begin FParameters.CapFont := TagString(t); FParameters.CapFontSize := TagInteger(t); end 
    else
      raise EKeyBtnParameters.Create('Invalid parameters string.');
  end;
  Result := FParameters;
end;

procedure DrawKeyParam(Canvas: TCanvas; Border: TRect; X, Y, Width, Height, Index: Integer;
  FKeyBitmap: TBitmap);
    function CompareRect(r1, r2: TRect): Boolean;
    begin
      Result := (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and (r1.Bottom = r2.Bottom);
    end;

var
  bw, bh: Integer;
  Save: THandle;
  MaskDC: HDC;
begin
  bw := FKeyBitmap.Width div 17;
  bh := FKeyBitmap.Height;

  with Canvas do
  begin
{    if CompareRect(Border, Rect(0,0,0,0)) then
      StretchBlt(Handle, X, Y, Width, Height, FKeyBitmap.Canvas.Handle, Index*bw, 0, bw, bh, SRCCOPY)
    else
    begin}

      Save := 0;
      MaskDC := 0;
      try
        MaskDC := CreateCompatibleDC(0);
        Save := SelectObject(MaskDC, FKeyBitmap.MaskHandle);

        if Border.Left > 0 then
        begin
          if Border.Top > 0 then
            TransparentStretchBlt(  // top-left
              Handle,                   X,        Y, Border.Left, Border.Top,
              FKeyBitmap.Canvas.Handle, Index*bw, 0, Border.Left, Border.Top,
              MaskDC,                   Index*bw, 0);
          TransparentStretchBlt(  // left
            Handle,                   X,        Y+Border.Top, Border.Left, Height-Border.Top-Border.Bottom,
            FKeyBitmap.Canvas.Handle, Index*bw, Border.Top,   Border.Left, bh-Border.Top-Border.Bottom,
            MaskDC,                   Index*bw, Border.Top);
          if Border.Bottom > 0 then
            TransparentStretchBlt(  // bottom-left
              Handle,                   X,        Y+Height-Border.Bottom, Border.Left, Border.Bottom,
              FKeyBitmap.Canvas.Handle, Index*bw, bh-Border.Bottom,       Border.Left, Border.Bottom,
              MaskDC,                   Index*bw, bh-Border.Bottom);
        end;

        if Border.Top > 0 then
          TransparentStretchBlt(  // top
            Handle,                   X+Border.Left,          Y,  Width - Border.Right - Border.Left,  Border.Top,
            FKeyBitmap.Canvas.Handle, Index*bw+Border.Left,   0,  bw - Border.Right - Border.Left,     Border.Top,
            MaskDC,                   Index*bw+Border.Left,   0);

        TransparentStretchBlt(  // center
          Handle,                   X+Border.Left,        Y+Border.Top, Width - Border.Right - Border.Left, Height - Border.Bottom - Border.Top,
          FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, Border.Top,   bw - Border.Right - Border.Left,    bh - Border.Bottom - Border.Top,
          MaskDC,                   Index*bw+Border.Left, Border.Top);

        if Border.Bottom > 0 then
          TransparentStretchBlt(  // bottom
            Handle,                   X+Border.Left,        Y+Height-Border.Bottom, Width - Border.Right - Border.Left, Border.Bottom,
            FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, bh-Border.Bottom,       bw - Border.Right - Border.Left,    Border.Bottom,
            MaskDC,                   Index*bw+Border.Left, bh-Border.Bottom);

        if Border.Right > 0 then
        begin
          if Border.Top > 0 then
            TransparentStretchBlt(  // top-right
              Handle,                   X+Width - Border.Right,       Y, Border.Right, Border.Top,
              FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right, 0, Border.Right, Border.Top,
              MaskDC,                   Index*bw + bw - Border.Right, 0);
          TransparentStretchBlt(  // right
            Handle,                   X+Width - Border.Right,         Y+Border.Top, Border.Right, Height - Border.Bottom-Border.Top,
            FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right,   Border.Top,   Border.Right, bh - Border.Bottom - Border.Top,
            MaskDC,                   Index*bw + bw - Border.Right,   Border.Top);
          if Border.Bottom > 0 then
            TransparentStretchBlt(  // bottom-right
              Handle,                   X+Width - Border.Right,       Y+Height-Border.Bottom, Border.Right, Border.Bottom,
              FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right, bh-Border.Bottom,       Border.Right, Border.Bottom,
              MaskDC,                   Index*bw + bw - Border.Right, bh-Border.Bottom);
        end;
      finally
        if Save <> 0 then SelectObject(MaskDC, Save);
        if MaskDC <> 0 then DeleteDC(MaskDC);
      end;
//    end;
  end;
end;

procedure DrawEuropeanEnterKeyParam(Canvas: TCanvas; Border: TRect; X, Y, Width, Height, Index: Integer; FKeyBitmap: TBitmap; X2, Y2: Integer);
    function CompareRect(r1, r2: TRect): Boolean;
    begin
      Result := (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and (r1.Bottom = r2.Bottom);
    end;

var
  bw, bh: Integer;
  Save: THandle;
  MaskDC: HDC;
begin
  bw := FKeyBitmap.Width div 17;
  bh := FKeyBitmap.Height;

  with Canvas do
  begin
    Save := 0;
    MaskDC := 0;
    try
      MaskDC := CreateCompatibleDC(0);
      Save := SelectObject(MaskDC, FKeyBitmap.MaskHandle);

      if Border.Left > 0 then
      begin
        if Border.Top > 0 then
          TransparentStretchBlt(  // top-left corner
            Handle,                   X,        Y, Border.Left, Border.Top,
            FKeyBitmap.Canvas.Handle, Index*bw, 0, Border.Left, Border.Top,
            MaskDC,                   Index*bw, 0);
        TransparentStretchBlt(  // upper left side
          Handle,                   X,        Y+Border.Top, Border.Left, Y2-Y-Border.Top-Border.Bottom,
          FKeyBitmap.Canvas.Handle, Index*bw, Border.Top,   Border.Left, bh-Border.Top-Border.Bottom,
          MaskDC,                   Index*bw, Border.Top);

        TransparentStretchBlt(  // lower left side
          Handle,                   X2,       Y2,            Border.Left, Height-(Y2-Y)-Border.Bottom,
          FKeyBitmap.Canvas.Handle, Index*bw, Border.Top,    Border.Left, bh-Border.Top-Border.Bottom,
          MaskDC,                   Index*bw, Border.Top);

        if Border.Bottom > 0 then
        begin
          TransparentStretchBlt(  // bottom-left corner (top)
            Handle,                   X,        Y2-Border.Bottom,       Border.Left, Border.Bottom,
            FKeyBitmap.Canvas.Handle, Index*bw, bh-Border.Bottom,       Border.Left, Border.Bottom,
            MaskDC,                   Index*bw, bh-Border.Bottom);
          TransparentStretchBlt(  // bottom-left corner (bottom)
            Handle,                   X2,       Y+Height-Border.Bottom, Border.Left, Border.Bottom,
            FKeyBitmap.Canvas.Handle, Index*bw, bh-Border.Bottom,       Border.Left, Border.Bottom,
            MaskDC,                   Index*bw, bh-Border.Bottom);
        end;
      end;

      if Border.Top > 0 then
        TransparentStretchBlt(  // top
          Handle,                   X+Border.Left,          Y,  Width - Border.Right - Border.Left,  Border.Top,
          FKeyBitmap.Canvas.Handle, Index*bw+Border.Left,   0,  bw - Border.Right - Border.Left,     Border.Top,
          MaskDC,                   Index*bw+Border.Left,   0);

      TransparentStretchBlt(  // center (top half)
        Handle,                   X+Border.Left,        Y+Border.Top, Width - Border.Right - Border.Left, (Y2 - Y) - Border.Bottom - Border.Top,
        FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, Border.Top,   bw - Border.Right - Border.Left,    bh - Border.Bottom - Border.Top,
        MaskDC,                   Index*bw+Border.Left, Border.Top);

      TransparentStretchBlt(  // center (bottom half)
        Handle,                   X2+Border.Left,        Y2 - Border.Bottom, Width - (X2-X) - Border.Right - Border.Left, Height - (Y2-Y),
        FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, Border.Top,   bw - Border.Right - Border.Left,    bh - Border.Bottom - Border.Top,
        MaskDC,                   Index*bw+Border.Left, Border.Top);

      TransparentStretchBlt(  // center (bottom half)
        Handle,                   X2,                    Y2 - Border.Bottom, Border.Left, Border.Bottom,
        FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, Border.Top,   bw - Border.Right - Border.Left,    bh - Border.Bottom - Border.Top,
        MaskDC,                   Index*bw+Border.Left, Border.Top);

      if Border.Bottom > 0 then
      begin
        TransparentStretchBlt(  // bottom (top half)
          Handle,                   X+Border.Left,        Y2-Border.Bottom,       X2 - X - Border.Left, Border.Bottom,
          FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, bh-Border.Bottom,       bw - Border.Right - Border.Left,    Border.Bottom,
          MaskDC,                   Index*bw+Border.Left, bh-Border.Bottom);

        TransparentStretchBlt(  // bottom
          Handle,                   X2+Border.Left,        Y+Height-Border.Bottom, Width - (X2-X) - Border.Right - Border.Left, Border.Bottom,
          FKeyBitmap.Canvas.Handle, Index*bw+Border.Left, bh-Border.Bottom,       bw - Border.Right - Border.Left,    Border.Bottom,
          MaskDC,                   Index*bw+Border.Left, bh-Border.Bottom);
      end;

      if Border.Right > 0 then
      begin
        if Border.Top > 0 then
          TransparentStretchBlt(  // top-right
            Handle,                   X+Width - Border.Right,       Y, Border.Right, Border.Top,
            FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right, 0, Border.Right, Border.Top,
            MaskDC,                   Index*bw + bw - Border.Right, 0);
        TransparentStretchBlt(  // right
          Handle,                   X+Width - Border.Right,         Y+Border.Top, Border.Right, Height - Border.Bottom-Border.Top,
          FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right,   Border.Top,   Border.Right, bh - Border.Bottom - Border.Top,
          MaskDC,                   Index*bw + bw - Border.Right,   Border.Top);
        if Border.Bottom > 0 then
          TransparentStretchBlt(  // bottom-right
            Handle,                   X+Width - Border.Right,       Y+Height-Border.Bottom, Border.Right, Border.Bottom,
            FKeyBitmap.Canvas.Handle, Index*bw + bw - Border.Right, bh-Border.Bottom,       Border.Right, Border.Bottom,
            MaskDC,                   Index*bw + bw - Border.Right, bh-Border.Bottom);
      end;
    finally
      if Save <> 0 then SelectObject(MaskDC, Save);
      if MaskDC <> 0 then DeleteDC(MaskDC);
    end;
  end;
end;

{ TActiveKeyBtnBitmap }

constructor TActiveKeyBtnBitmap.Create(Parameters: TKeyBtnParameters);
begin
  FParameters := Parameters;
  FBitmap := TBitmap.Create;
  FBitmap.LoadFromResourceName(HInstance, Copy(Parameters.BitmapFileName, 2, Length(Parameters.BitmapFileName)));
  FBitmap.TransparentColor := Parameters.BitmapTransparentColor;
  if Parameters.BitmapTransparentColor <> clNone then FBitmap.Transparent := True;
end;

destructor TActiveKeyBtnBitmap.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

initialization
finalization
  FreeAndNil(FActiveKeyBitmap);
end.

