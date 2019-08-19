﻿(*
  Name:             UfrmKeymanMenu
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      25 May 2010

  Modified Date:    23 Oct 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 May 2010 - mcdurdin - I2226 - Improve Keyman menu
                    25 May 2010 - mcdurdin - I2377 - Keyman menu pops up and off-screen when taskbar is at top of screen
                    30 Nov 2010 - mcdurdin - I2542 - Rework menu styling as per Paul's design
                    10 Dec 2010 - mcdurdin - I2551, I2554 - Keyman crashes when closing menu
                    10 Dec 2010 - mcdurdin - I2555 - Transparency issues in Remote Desktop
                    17 Dec 2010 - mcdurdin - I2542 - Fix horizontal line
                    11 Jan 2011 - mcdurdin - I2582 - Keyman Desktop menu should pop over OSK
                    31 Jan 2011 - mcdurdin - I2639 - Fix handle and memory leak in Keyman menu
                    31 Jan 2011 - mcdurdin - I2582 - Re-fix for popping over OSK
                    17 Mar 2011 - mcdurdin - I2693 - Crash in Keyman menu with multiple screens and no 'nearest' screen returned
                    18 Mar 2011 - mcdurdin - I2820 - Crash in Keyman menu with multiple screens
                    03 May 2011 - mcdurdin - I2884 - Crash moving mouse over uninitialized menu
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    24 Jan 2012 - mcdurdin - I3213 - Crash when Keyman menu is visible and Ctrl+Alt+Del or Win+L pressed
                    03 Nov 2012 - mcdurdin - I3517 - V9.0 - Merge of I3213 - Crash when Keyman menu is visible and Ctrl+Alt+Del or Win+L pressed
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    11 Nov 2013 - mcdurdin - I3960 - V9.0 - Reskin the Keyman menu with 9.0 style
                    29 Nov 2013 - mcdurdin - I3990 - V9.0 - Keyman keyboards menu should be used on OSK toolbar
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    23 Oct 2014 - mcdurdin - I4429 - Resized window can sometimes cause menu popup to crash [CrashID:keyman.exe_9.0.470.0_2C404908_EAccessViolation]
*)

unit UfrmKeymanMenu;  // I3306   // I3614   // I3960

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, KeymanMenuItem, UserMessages, ExtCtrls,
  custinterfaces, Vcl.Imaging.pngimage;

type
  TfrmKeymanMenu = class(TForm)
    imgTitle: TImage;
    procedure FormClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TntFormHide(Sender: TObject);
    procedure TntFormShow(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    FLayerWidth, FLayerHeight: Integer;   // I4429

    hLayeredBitmap, hBuffer: THandle;
    pvBufferBits, pvLayeredBits: Pointer;  // I2554
    bmi: TBitmapInfo;
    ptPos, ptSrc: TPoint;
    szWnd: TSize;
    blend: TBlendFunction;

    MaxKeyboardWidth, MaxItemWidth,
    TotalKeyboardHeight, TotalItemHeight: Integer;

    Fmnu: TPopupMenu;
    FItemRects: array of TRect;
    FSelectedItemIndex: Integer;
    LayeredCanvas: TCanvas;
    dcLayered: Cardinal;
    FTransparency: DWord;  // I2555
    TitleAndStripeHeight: Integer;   // I3990
    FIsKeyboardMenu: Boolean;   // I3990
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Message: TMessage); message WM_CHAR;
    procedure FitToDesktop;
    procedure DrawLayer;
    procedure StartCanvas;
    procedure EndCanvas;
    { Private declarations }
  public
    { Public declarations }
    procedure PopupEx(mnu: TPopupMenu; X, Y: Integer; IconRect: TRect);   // I3990
  end;

implementation

uses
  CleartypeDrawCharacter,
  LayeredFormUtils,
  Math,
  KeymanControlMessages,
  KeymanPaths,
  KeymanVersion,
  keymanapi_TLB,
  UfrmKeyman7Main;

{$R *.dfm}

const
  // +-------------------------------+  H   // I3960
  // | [] Keyman Desktop             |  I
  // |===============================|  J
  // |                               |  K
  // |---[] keyboard---|---[] menu---|
  //          ...
  // |                               |  L
  // +-------------------------------+  M
  // A B             C D E         F G
  //
  // A = OuterBorderWidth
  // B = MenuPadding.Left
  // C = MenuPadding.Right
  // D = 0 (delineation between white and grey)
  // E = MenuPadding.Left
  // F = MenuPadding.Right
  // G = OuterBorderWidth
  // H = OuterBorderWidth
  // I = imgTitle.Height
  // J = StripeHeight
  // K = MenuPadding.Top
  // L = MenuPadding.Bottom
  // M = OuterBorderWidth

  OuterBorderWidth = 1;
  MenuPadding: TRect = (Left: 8; Top: 4; Right: 8; Bottom: 4);
  ItemMargin = 2;
  MenuSpacing = 8;
  StripeHeight = 5;

{ TfrmKeymanMenu }

procedure TfrmKeymanMenu.FormClick(Sender: TObject);
var
  i: Integer;
  pt: TPoint;
begin
  if not Assigned(Fmnu) or not Assigned(Fmnu.Items) then  // I2884
    Exit;

  if not GetCursorPos(pt) then Exit;  // I3213   // I3517
  pt := ScreenToClient(pt);
  for i := 0 to Fmnu.Items.Count - 1 do
  begin
    if PtInRect(FItemRects[i], pt) then
    begin
      Hide;
      Fmnu.Items[i].Click;
      Exit;
    end;
  end;
end;

procedure TfrmKeymanMenu.FormMouseLeave(Sender: TObject);
var
  pt: TPoint;
begin
  if (not GetCursorPos(pt) or not PtInRect(Rect(Left, Top, Left+Width, Top+Height), pt)) and Showing then // I2554 // I3213   // I3517
  begin
    StartCanvas;
    EndCanvas;
    FSelectedItemIndex := -1;
  end;
end;

procedure TfrmKeymanMenu.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i: Integer;
  kmi: TKeymanMenuItem;
begin
  if not Assigned(Fmnu) or not Assigned(Fmnu.Items) then  // I2884
    Exit;
    
  for i := 0 to Fmnu.Items.Count - 1 do
  begin
    if PtInRect(FItemRects[i], Point(X,Y)) then
    begin
      kmi := Fmnu.Items[i] as TKeymanMenuItem;

      if FSelectedItemIndex <> i then
      begin
        StartCanvas;
        try
          kmi.DrawItem80(LayeredCanvas, FItemRects[i], [odSelected]);
          FSelectedItemIndex := i;
        finally
          EndCanvas;
        end;
      end;
      Exit;
    end;
  end;

  FSelectedItemIndex := -1;
end;

procedure TfrmKeymanMenu.PopupEx(mnu: TPopupMenu; X, Y: Integer; IconRect: TRect);   // I3990
var
  kmi: TKeymanMenuItem;
  i, ItemWidth, ItemHeight: Integer;
  v: Integer;
begin
  Fmnu := mnu;

  MaxKeyboardWidth := 0;
  MaxItemWidth := 0;
  TotalKeyboardHeight := 0;
  TotalItemHeight := 0;

  SetLength(FItemRects, Fmnu.Items.Count);

  FIsKeyboardMenu := True;

  for i := 0 to Fmnu.Items.Count - 1 do
    if (Fmnu.Items[i] is TKeymanMenuItem) and
      ((Fmnu.Items[i] as TKeymanMenuItem).CMIItemType <> _mitKeyboardsList) then
    begin
      FIsKeyboardMenu := False;
      Break;
    end;

  if FIsKeyboardMenu then
  begin
    TitleAndStripeHeight := 0;
  end
  else
  begin
    TitleAndStripeHeight := imgTitle.Height + StripeHeight;
  end;


  for i := 0 to Fmnu.Items.Count - 1 do
  begin
    if Fmnu.Items[i] is TKeymanMenuItem then
    begin
      kmi := Fmnu.Items[i] as TKeymanMenuItem;
      kmi.OnMeasureItem(kmi, Canvas, ItemWidth, ItemHeight);

      if (kmi.CMIItemType = _mitKeyboardsList) then   // I3933
      begin
        FItemRects[i].Top := OuterBorderWidth + TitleAndStripeHeight + MenuPadding.Top + TotalKeyboardHeight;
        FItemRects[i].Bottom := FItemRects[i].Top + ItemHeight + 1;

        if MaxKeyboardWidth < ItemWidth then
          MaxKeyboardWidth := ItemWidth;

        Inc(TotalKeyboardHeight, ItemHeight);
      end
      else
      begin
        FItemRects[i].Top := OuterBorderWidth + TitleAndStripeHeight + MenuPadding.Top + TotalItemHeight + 1;
        FItemRects[i].Bottom := FItemRects[i].Top + ItemHeight + 1;

        if MaxItemWidth < ItemWidth then
          MaxItemWidth := ItemWidth;

        Inc(TotalItemHeight, ItemHeight + ItemMargin);
      end;
    end;
  end;

  if not FIsKeyboardMenu then
    Inc(MaxItemWidth, MenuSpacing);
  Inc(MaxKeyboardWidth, MenuSpacing);

  Dec(TotalItemHeight, ItemMargin); // No bottom margin on last item

  if not FIsKeyboardMenu then
    if MaxItemWidth + MaxKeyboardWidth + MenuPadding.Left * 2 + MenuPadding.Right * 2 < imgTitle.Width then
      MaxItemWidth := imgTitle.Width - MaxKeyboardWidth - MenuPadding.Left * 2 - MenuPadding.Right * 2;

  if FIsKeyboardMenu
    then Width := OuterBorderWidth + MenuPadding.Left + MaxKeyboardWidth + MenuPadding.Right + OuterBorderWidth
    else Width := OuterBorderWidth + MenuPadding.Left + MaxKeyboardWidth + MenuPadding.Right + MenuPadding.Left + MaxItemWidth + MenuPadding.Right + OuterBorderWidth;
  Height := OuterBorderWidth + TitleAndStripeHeight + MenuPadding.Top + Max(TotalKeyboardHeight, TotalItemHeight) + MenuPadding.Bottom + OuterBorderWidth;

  if not IconRect.IsEmpty then
  begin
    if IconRect.Top - Height > 0 then
      Top := IconRect.Top - Height
    else
      Top := IconRect.Bottom;

    if IconRect.Right - Width > 0 then
      Left := IconRect.Right - Width
    else
      Left := IconRect.Left;
  end
  else
  begin
    Left := X - Width;
    Top := Y - Height;
  end;

  v := OuterBorderWidth + TitleAndStripeHeight + MenuPadding.Top;

  for i := 0 to Fmnu.Items.Count - 1 do
  begin
    kmi := Fmnu.Items[i] as TKeymanMenuItem;
    if (kmi.CMIItemType = _mitKeyboardsList) then   // I3933
    begin
      FItemRects[i].Left := OuterBorderWidth + MenuPadding.Left;
      FItemRects[i].Right := FItemRects[i].Left + MaxKeyboardWidth;
    end
    else
    begin
      if kmi.CMIItemType = mitSeparator then
      begin
        v := Height - (Fmnu.Items.Count - i - 1) * (ItemHeight + ItemMargin) - MenuPadding.Bottom - OuterBorderWidth + ItemMargin;
        Continue;
      end;

      ItemHeight := FItemRects[i].Bottom - FItemRects[i].Top;
      FItemRects[i].Top := v;
      FItemRects[i].Bottom := FItemRects[i].Top + ItemHeight;

      Inc(v, ItemHeight + ItemMargin);

      FItemRects[i].Left := OuterBorderWidth + MenuPadding.Left + MaxKeyboardWidth + MenuPadding.Right + MenuPadding.Left;
      FItemRects[i].Right := FItemRects[i].Left + MaxItemWidth;
    end;
  end;

  FSelectedItemIndex := -1;

  FitToDesktop;
  Show;
end;

const
  CWindowBorder: Cardinal = ($404040);
  CWindowBackground: Cardinal = $E8E8E8;
  CKeyboardBackground: Cardinal = $FFFFFF;
  CTitleBackground: Cardinal = $FFFFFF;
  CStripeOrange = $FF8917; //$1789FF;
  CStripeRed = $BA2135; //$3521BA;
  CStripeBlue = $6AB8D3; //$D3B86A;
  CTransparencyMask: Cardinal = $F0000000;  // I2555
  CNoTransparencyMask: Cardinal = $FF000000;  // I2555

type
  CardinalArray  = array[0..$effffff] of Cardinal;
  PCardinalArray = ^CardinalArray;

{$RANGECHECKS OFF}
procedure TfrmKeymanMenu.DrawLayer;   // I3990
var
  kmi: TKeymanMenuItem;
  dc, dcScreen: THandle;
  hOldBitmap: THandle;
  p: PCardinalArray;

      procedure DrawBackgroundAndBorders;
      var
        x, y: Integer;
        dy: Integer;
        x1: Integer;
        x2: Integer;
      begin
        { Draw bottom border }
        y := 0;
        for x := 0 to FLayerWidth - 1 do p^[x + y] := CWindowBorder or FTransparency;  // I2555   // I4429

        { Draw top border }
        y := FLayerWidth * (FLayerHeight - 1);   // I4429
        for x := 0 to FLayerWidth - 1 do p^[x + y] := CWindowBorder or FTransparency;  // I2555   // I4429

        { Draw side borders }
        for y := 1 to FLayerHeight - 2 do   // I4429
        begin
          dy := FLayerWidth * y;   // I4429

          p^[dy] := CWindowBorder or FTransparency;
          p^[dy + FLayerWidth - 1] := CWindowBorder or FTransparency;   // I4429

          if not FIsKeyboardMenu and (y >= FLayerHeight - OuterBorderWidth - imgTitle.Height) then   // I4429
          begin
            for x := 1 to FLayerWidth - 2 do p^[dy + x] := CTitleBackground or FTransparency;   // I4429
          end
          else if not FIsKeyboardMenu and (y >= FLayerHeight - OuterBorderWidth - imgTitle.Height - StripeHeight) then   // I4429
          begin
            x1 := FLayerWidth * 5 div 8;   // I4429
            x2 := FLayerWidth * 7 div 8;
            for x := 1 to x1-1 do p^[dy + x] := CStripeOrange or FTransparency;
            for x := x1 to x2-1 do p^[dy + x] := CStripeRed or FTransparency;
            for x := x2 to FLayerWidth - 2 do p^[dy + x] := CStripeBlue or FTransparency;   // I4429
          end
          else
          begin
            for x := 1 to MenuPadding.Left + MaxKeyboardWidth + MenuPadding.Right do p^[dy + x] := CKeyboardBackground or FTransparency;
            for x := MenuPadding.Left + MaxKeyboardWidth + MenuPadding.Right + 1 to FLayerWidth - 2 do p^[dy + x] := CWindowBackground or FTransparency;   // I4429
          end;
        end;
      end;

      procedure DrawMenuItems;
      var
        cc: TCanvas;
        i: Integer;
        png: TPngImage;
      begin
        { draw the rest of the graphics into the dc }

        cc := TCanvas.Create;
        try
          cc.Handle := dc;
          cc.Brush.Color := $FFFFFF;

          if not FIsKeyboardMenu then
          begin
            png := imgTitle.Picture.Graphic as TPngImage;
            cc.Draw(OuterBorderWidth,OuterBorderWidth,png);
          end;

          for i := 0 to Fmnu.Items.Count - 1 do
          begin
            if Fmnu.Items[i] is TKeymanMenuItem then
            begin
              kmi := Fmnu.Items[i] as TKeymanMenuItem;
              if kmi.CMIItemType in [mitBreak, mitSeparator] then Continue;  // I2542
              kmi.DrawItem80(cc, FItemRects[i], []);
            end;
          end;
        finally
          cc.Handle := 0;
          cc.Free;
        end;
      end;

begin
  if not Assigned(Fmnu) or not Assigned(Fmnu.Items) then Exit;  // I2884

  dcScreen := GetDC(Handle);
  dc := CreateCompatibleDC(dcScreen);
  try
    // zero the memory for the bitmap info
    ZeroMemory(@bmi, sizeof(BITMAPINFO));

    FLayerWidth := ClientWidth;   // I4429
    FLayerHeight := ClientHeight;   // I4429

    // setup bitmap info
    bmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth := FLayerWidth;   // I4429
    bmi.bmiHeader.biHeight := FLayerHeight;   // I4429
    bmi.bmiHeader.biPlanes := 1;
    bmi.bmiHeader.biBitCount := 32;         // four 8-bit components
    bmi.bmiHeader.biCompression := BI_RGB;
    bmi.bmiHeader.biSizeImage := FLayerWidth * FLayerHeight * 4;   // I4429

    if hBuffer <> 0 then  // I2639
      DeleteObject(hBuffer);

    if hLayeredBitmap <> 0 then     // I2639
      DeleteObject(hLayeredBitmap);

    // create our DIB section and select the bitmap into the dc
    hBuffer := CreateDIBSection(dc, bmi, DIB_RGB_COLORS, pvBufferBits, 0, 0);
    hLayeredBitmap := CreateDIBSection(dc, bmi, DIB_RGB_COLORS, pvLayeredBits, 0, 0);  // I2555

    hOldBitmap := SelectObject(dc, hBuffer);
    try
      ptPos := Point(Left, Top);
      szWnd.cx := FLayerWidth;   // I4429
      szWnd.cy := FLayerHeight;   // I4429
      ptSrc := Point(0, 0);

      blend.BlendOp := AC_SRC_OVER;
      blend.BlendFlags := 0;
      blend.SourceConstantAlpha := 255;
      blend.AlphaFormat := AC_SRC_ALPHA;

      p := PCardinalArray(pvBufferBits);

      DrawBackgroundAndBorders;
      DrawMenuItems;
    finally
      SelectObject(dc, hOldBitmap);
    end;
  finally
    DeleteDC(dc);
    ReleaseDC(Handle, dcScreen);
  end;
  
  StartCanvas;
  EndCanvas;
end;

procedure TfrmKeymanMenu.FitToDesktop;
var
  m: TMonitor;
  wr, mr: TRect;
begin
  wr := Rect(Left,Top,Left+Width,Top+Height);
  m := Screen.MonitorFromRect(wr, mdNearest);
  if not Assigned(m)  // I2693, I2820
    then mr := Screen.WorkAreaRect
    else mr := m.WorkareaRect;
  if wr.Top < mr.Top then Top := mr.Top
  else if wr.Bottom > mr.Bottom then Top := mr.Bottom - (wr.Bottom-wr.Top);

  if wr.Left < mr.Left then Left := mr.Left
  else if wr.Right > mr.Right then Left := mr.Right - (wr.Right-wr.Left);
end;

procedure TfrmKeymanMenu.TntFormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  imgTitle.Picture.LoadFromFile(TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KeymanMenuTitleImage));
  imgTitle.AutoSize := True;
  //SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
  //UpdateLayeredWindow(Handle, 0, nil, nil, 0,
end;

procedure TfrmKeymanMenu.TntFormDestroy(Sender: TObject);
begin
  if hBuffer <> 0 then
    DeleteObject(hBuffer);
  hBuffer := 0;
  pvBufferBits := nil; // I2554

  if hLayeredBitmap <> 0 then
    DeleteObject(hLayeredBitmap);
  hLayeredBitmap := 0;
  pvLayeredBits := nil;  // I2554
end;

procedure TfrmKeymanMenu.TntFormHide(Sender: TObject);
begin
  if hBuffer <> 0 then
    DeleteObject(hBuffer);
  hBuffer := 0;
end;

procedure TfrmKeymanMenu.TntFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  FOldIndex: Integer;
  i: Integer;
begin
  if not Assigned(Fmnu) or not Assigned(Fmnu.Items) then Exit;  // I2884

  case Key of
    VK_UP:
      begin
        Key := 0;
        Dec(FSelectedItemIndex);
        if FSelectedItemIndex < 0 then FSelectedItemIndex := Fmnu.Items.Count - 1;
        while FSelectedItemIndex >= 0 do
          if Fmnu.Items[FSelectedItemIndex].IsLine then Dec(FselectedItemIndex) else Break;
      end;
    VK_DOWN:
      begin
        Key := 0;
        Inc(FSelectedItemIndex);
        if FSelectedItemIndex >= Fmnu.Items.Count then FSelectedItemIndex := 0;
        while FSelectedItemIndex < Fmnu.Items.Count - 1 do
          if Fmnu.Items[FSelectedItemIndex].IsLine then Inc(FselectedItemIndex) else Break;
      end;
    VK_LEFT:
      begin
        Key := 0;
        if FSelectedItemIndex < 0 then Exit;
        FOldIndex := FSelectedItemIndex;

        for i := 0 to FOldIndex - 1 do
          if (FItemRects[i].Right <= FItemRects[FOldIndex].Left) and
            (FItemRects[i].Bottom >= (FItemRects[FOldIndex].Top+FItemRects[FOldIndex].Bottom) div 2) then
          begin
            FSelectedItemIndex := i;
            Break;
          end;

        while FSelectedItemIndex < Fmnu.Items.Count - 1 do
          if Fmnu.Items[FSelectedItemIndex].IsLine then Inc(FselectedItemIndex) else Break;
      end;
    VK_RIGHT:
      begin
        Key := 0;
        if FSelectedItemIndex < 0 then Exit;
        FOldIndex := FSelectedItemIndex;

        for i := FOldIndex + 1 to Fmnu.Items.Count - 1 do
          if (FItemRects[i].Left >= FItemRects[FOldIndex].Right) and
            (FItemRects[i].Bottom >= (FItemRects[FOldIndex].Top+FItemRects[FOldIndex].Bottom) div 2) then
          begin
            FSelectedItemIndex := i;
            Break;
          end;

        while FSelectedItemIndex < Fmnu.Items.Count - 1 do
          if Fmnu.Items[FSelectedItemIndex].IsLine then Inc(FselectedItemIndex) else Break;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if (FSelectedItemIndex >= 0) then
        begin
          Hide;
          Fmnu.Items[FSelectedItemIndex].Click;
        end;
        Exit;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        Hide;
        frmKeyman7Main.SetLastFocus;
        Exit;
      end
    else Exit;
  end;

  StartCanvas;
  try
    if FSelectedItemIndex >= 0 then
      (Fmnu.Items[FSelectedItemIndex] as TKeymanMenuItem).DrawItem80(LayeredCanvas, FItemRects[FSelectedItemIndex], [odSelected]);
  finally
    EndCanvas;
  end;
end;

procedure TfrmKeymanMenu.StartCanvas;
var
  dcScreen: THandle;
begin
  Assert(not Assigned(LayeredCanvas));  // I2554

  if Assigned(pvLayeredBits) and Assigned(pvBufferBits) and (hLayeredBitmap <> 0) then  // I2554
  begin
    dcScreen := GetDC(Handle);
    dcLayered := CreateCompatibleDC(dcScreen);
    SelectObject(dcLayered, hLayeredBitmap);
    ReleaseDC(Handle, dcScreen);

    CopyMemory(pvLayeredBits, pvBufferBits, FLayerWidth * FLayerHeight * 4);   // I4429
  //  SetDIBits(dcLayered, hLayeredBitmap, 0, FLayerHeight, pvBufferBits, bmi, DIB_RGB_COLORS);

    LayeredCanvas := TCanvas.Create;
    LayeredCanvas.Handle := dcLayered;
  end;
end;

procedure TfrmKeymanMenu.EndCanvas;
var
  dcScreen: THandle;

      procedure DrawCornersAndMakeTransparent;
      const
        pels: array[0..4,0..4] of Byte =
          ((0,0,0,0,0),
           (0,0,0,1,1),
           (0,0,1,2,2),
           (0,1,2,3,3),
           (0,1,2,3,3));
      var
        v, x, y: Integer;
        X1, X2: Integer;
        p: PCardinalArray;
        FSelectionRect: TRect;
      begin
        { Draw corners }
        p := PCardinalArray(pvLayeredBits);  // I2554

        { Reset transparency after GDI operations }

        for y := 0 to FLayerHeight - 1 do   // I4429
        begin
          v := y * FLayerWidth;   // I4429
          if y < FLayerHeight - 5 then   // I4429
          begin
            X1 := 0;
            X2 := FLayerWidth - 1;   // I4429
          end
          else
          begin
            X1 := 5;
            X2 := FLayerWidth - 6;   // I4429
          end;

          for x := X1 to X2 do
            p^[v + x] := (p^[v + x] and $FFFFFF) or FTransparency;  // I2555
        end;

        if FSelectedItemIndex >= 0 then
        begin
          FSelectionRect := FItemRects[FSelectedItemIndex];
          for y := FSelectionRect.Top to FSelectionRect.Bottom - 1 do
          begin
            v := (FLayerHeight - y) * FLayerWidth;   // I4429
            for x := FSelectionRect.Left to FSelectionRect.Right - 1 do
              p^[v + x] := (p^[v + x] and $FFFFFF) or $FF000000;
          end;
        end;
      end;

begin
  if not Assigned(LayeredCanvas) then Exit;  // I2554
  try
    LayeredCanvas.Handle := 0;

    if Assigned(pvBufferBits) and Assigned(pvLayeredBits) then  // I2554
    begin
      GdiFlush;
      DrawCornersAndMakeTransparent;

      dcScreen := GetDC(Handle);
      if not UpdateLayeredWindow(Handle, dcScreen, @ptPos, @szWnd, dcLayered, @ptSrc, 0, @blend, ULW_ALPHA) then
        RaiseLastOSError;
      ReleaseDC(Handle, dcScreen);
    end;
  finally
    FreeAndNil(LayeredCanvas);
    DeleteDC(dcLayered);
    dcLayered := 0;  // I2554
  end;
  //DeleteObject(hLayeredBitmap);
  //hLayeredBItmap := 0;
end;

procedure TfrmKeymanMenu.TntFormShow(Sender: TObject);
begin
  if TLayeredFormUtils.ShouldUseAlpha  // I2555
    then FTransparency := CTransparencyMask
    else FTransparency := CNoTransparencyMask;

  PostMessage(Handle, WM_USER_FormShown, 0, 0); // I2582

  DrawLayer;
end;

procedure TfrmKeymanMenu.WMChar(var Message: TMessage);
var
  i: Integer;
  kmi: TKeymanMenuItem;
begin
  if not Assigned(Fmnu) or not Assigned(Fmnu.Items) then Exit;  // I2884

  for i := 0 to Fmnu.Items.Count - 1 do
  begin
    if Fmnu.Items[i] is TKeymanMenuItem then
    begin
      kmi := Fmnu.Items[i] as TKeymanMenuItem;

      if IsAccel(Message.WParam, kmi.WideCaption) then
      begin
        Hide;
        Fmnu.Items[i].Click;
        Exit;
      end;
    end;
  end;
end;

procedure TfrmKeymanMenu.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Hide;
end;

procedure TfrmKeymanMenu.WMUserFormShown(var Message: TMessage); // I2582
begin
  BringToFront;  // I2582
end;

end.
