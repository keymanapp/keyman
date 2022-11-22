(*
  Name:             keybtn
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
  History:          23 Aug 2006 - mcdurdin - Rework as custom control with new style button painting
                    28 Sep 2006 - mcdurdin - Tweak properties etc to match TOnScreenKeyboard
                    04 Dec 2006 - mcdurdin - Fix display of bitmaps
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    24 Jul 2015 - mcdurdin - I4799 - Preview keys are wrong colour in Developer
*)
unit KeyBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, VisualKeyboardParameters;

type
  TKeyBtnType = (kbtNormal, kbtControl);

  TKeyBtn = class(TCustomControl)
  private
    FIsHover: Boolean;
    FScale: Extended;

    FKeyData: WideString;
    FKeyText: WideString;
    FDataFont: TFont;
    FKeyType: TKeyBtnType;
    FIsSelected: Boolean;
    FKeyGlyph: TBitmap;
    FOnKeyBitmapChanged: TNotifyEvent;
    FStdLeft: Integer;
    FStdRow: Integer;
    FParameters: TKeyBtnParameters;
    FDisplayUnderlyingChar: Boolean;
    FIsDown: Boolean;
    FBackgroundBitmap: TBitmap;
    FDefaultPos: TRect;
    FDrawDisabled: Boolean;

    procedure SetKeyData(Value: WideString);
    procedure SetKeyText(Value: WideString);
    procedure SetDataFont(const Value: TFont);
    procedure SetKeyType(const Value: TKeyBtnType);
    procedure SetSelected(const Value: Boolean);
    procedure SetKeyGlyph(const Value: TBitmap);
    function GetParameters: WideString;
    procedure SetParameters(Value: WideString);
    procedure SetDisplayUnderlyingChar(const Value: Boolean);
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DrawKey;
    procedure Paint; override;
    procedure Resize; override;

    property Parameters: WideString read GetParameters write SetParameters;
    property ParametersRec: TKeyBtnParameters read FParameters write FParameters;

    destructor Destroy; override;
  published
    property DefaultPos: TRect read FDefaultPos write FDefaultPos;
    property Caption;
    property OnClick;
    property KeyText: WideString read FKeyText write SetKeyText;
    property KeyData: WideString read FKeyData write SetKeyData;
    property KeyType: TKeyBtnType read FKeyType write SetKeyType;
    property DataFont: TFont read FDataFont write SetDataFont;
    property StdRow: Integer read FStdRow write FStdRow;
    property StdLeft: Integer read FStdLeft write FStdLeft;
    property Selected: Boolean read FIsSelected write SetSelected;

    property DrawDisabled: Boolean read FDrawDisabled write FDrawDisabled default true;

    property KeyGlyph: TBitmap read FKeyGlyph write SetKeyGlyph;

    property DisplayUnderlyingChar: Boolean read FDisplayUnderlyingChar write SetDisplayUnderlyingChar;

    property OnKeyBitmapChanged: TNotifyEvent read FOnKeyBitmapChanged write FOnKeyBitmapChanged;

    property OnDragOver;
    property OnDragDrop;
    property Enabled;
    property TabOrder;
    property TabStop;
    property Font;
    property ParentFont;
    property Visible;
  end;

procedure Register;

implementation

uses
  CleartypeDrawCharacter,
  OnScreenKeyboard;

{$R keybtn.res}

var
  FDrawChar: TClearTypeDrawCharacter = nil;

procedure Register;
begin
  RegisterComponents('Keyman', [TKeyBtn]);
end;

{ TKeyBtn }

procedure TKeyBtn.SetKeyData(Value: WideString);
begin
  FKeyData := Value;
  Invalidate;
end;

procedure TKeyBtn.SetKeyText(Value: WideString);
begin
  FKeyText := Value;
  Invalidate;
end;

procedure TKeyBtn.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  BitBlt(FBackgroundBitmap.Canvas.Handle, 0, 0, Width, Height, msg.DC, 0, 0, SRCCOPY);
  SetBkMode(msg.DC, TRANSPARENT);
  msg.result := 1;
end;

procedure TKeyBtn.DrawKey;

var
  ARect, R: TRect;

  procedure DrawKeyBitmap(Canvas: TCanvas);
      function CompareRect(r1, r2: TRect): Boolean;
      begin
        Result := (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and (r1.Bottom = r2.Bottom);
      end;

  var
    bIndex, n: Integer;
    RGlyph: TRect;
    Save: THandle;
    MaskDC: THandle;
  begin
    if Enabled then
    begin
      bIndex := 0;
      if FIsDown and FIsHover then bIndex := bIndex or 1;
      if FIsSelected then bIndex := bIndex or 2;
      if FIsHover and not FIsDown then bIndex := bIndex or 4;
      if FKeyType <> kbtNormal then bIndex := bIndex or 8;
    end
    else if DrawDisabled then
      bIndex := 16
    else if FKeyType <> kbtNormal then
      bIndex := 8
    else
      bIndex := 0;

    if FIsDown and FIsHover then n := 1 else n := 0;

    DrawKeyParam(Canvas, OnScreen_KeyBitmap.Parameters.Border[n], R.Left, R.Top, Width, Height, bIndex, OnScreen_KeyBitmap.Bitmap);

    with Canvas, OnScreen_KeyBitmap.Parameters do
    begin
      SetBkMode(Handle, TRANSPARENT);

      Font := Self.Font;
      Font.Name := CapFont;

      {if FLargeCapFont and (FKeyType = kktNormal)
        then Font.Size := Trunc((TextRect[n].Top - TextRect[n].Bottom) * FKeyboard.FScale)
        else}
      Font.Size := Trunc(CapFontSize*FScale);

      if not Enabled and DrawDisabled then    Font.Color := KeyFontColor_Disabled   // I4799
      else if (FKeyType = kbtNormal) and not FIsSelected then Font.Color := KeyFontColor_Base   // I4799
      else if FKeyType = kbtNormal then Font.Color := KeyFontColor_BaseSelected   // I4799
      else Font.Color := KeyFontColor_Cap;   // I4799

      ARect := Rect(R.Left+Trunc(TextRect[n].Left*FScale), R.Top+Trunc(1*FScale),
        R.Left+Trunc(TextRect[n].Right*FScale), R.Top+Height);   // I4799

      if FDisplayUnderlyingChar or (FKeyType <> kbtNormal) then
      begin
        FDrawChar.DisplayQuality := ctCleartype;
        FDrawChar.Color := Font.Color;
        FDrawChar.SetFontDetails(Font.Name, Font.Height);

        FDrawChar.DrawText(Handle, TA_LEFT or TA_TOP, R.Left+Trunc(CapPos[n].X*FScale),
          R.Top+Trunc(CapPos[n].Y*FScale), ARect, FKeyText);
        //ExtTextOutW(Handle, R.Left+Trunc(CapPos[n].X*FKeyboard.FScale), R.Top+Trunc(CapPos[n].Y*FKeyboard.FScale), 0, @ARect, PWideChar(FKeyCap), Length(FKeyCap), nil);
      end;

      if Assigned(FKeyGlyph) and not FKeyGlyph.Empty then
      begin
        // image is anchored to
        RGlyph := Rect(
          R.Left+Trunc(TextRect[n].Right*FScale) - Trunc(FKeyGlyph.Width*FScale),
          R.Top+Trunc(TextRect[n].Bottom*FScale) - Trunc(FKeyGlyph.Height*FScale),
          R.Left+Trunc(TextRect[n].Right*FScale),
          R.Top+Trunc(TextRect[n].Bottom*FScale));

          if RGlyph.Right - RGlyph.Left > (TextRect[n].Right - TextRect[n].Left) * FScale then
          begin
            RGlyph.Top := RGlyph.Bottom - Trunc((RGlyph.Bottom - RGlyph.Top) * ((TextRect[n].Right-TextRect[n].Left)*FScale) / (RGlyph.Right - RGlyph.Left));
            RGlyph.Left := R.Left + Trunc(TextRect[n].Left * FScale);
          end;

          if RGlyph.Bottom - RGlyph.Top > (TextRect[n].Bottom - TextRect[n].Top) * FScale then
          begin
            RGlyph.Left := RGlyph.Right - Trunc((RGlyph.Right - RGlyph.Left) * ((TextRect[n].Bottom-TextRect[n].Top)*FScale) / (RGlyph.Bottom - RGlyph.Top));
            RGlyph.Top := R.Top + Trunc(TextRect[n].Top * FScale);
          end;

          SetStretchBltMode(Handle, HALFTONE);
          SetBrushOrgEx(Handle, 0, 0, nil);
          FKeyGlyph.TransparentMode := tmAuto;
          FKeyGlyph.Transparent := True;
          FKeyGlyph.PixelFormat := pf24Bit;


          Save := 0;
          MaskDC := 0;
          try
            MaskDC := CreateCompatibleDC(0);
            Save := SelectObject(MaskDC, FKeyGlyph.MaskHandle);
            TransparentStretchBlt(Handle, RGlyph.Left, RGlyph.Top, RGlyph.Right - RGlyph.Left,
              RGlyph.Bottom - RGlyph.Top, FKeyGlyph.Canvas.Handle, 0, 0, FKeyGlyph.Width,
              FKeyGlyph.Height, MaskDC, 0, 0);
          finally
            if Save <> 0 then SelectObject(MaskDC, Save);
            if MaskDC <> 0 then DeleteDC(MaskDC);
          end;
          //StretchDraw(RGlyph, FKeyGlyph);
        //Draw(R.Left+CapPos[n].X-FKeyGlyph.Width, R.Top+CapPos[n].Y-FKeyGlyph.Height, FKeyGlyph);
      end
      else
      begin
        Font := FDataFont;
        Font.Height := Trunc((TextRect[n].Top - TextRect[n].Bottom) * FScale);

        if not Enabled and DrawDisabled then      Font.Color := $808080
        else                                      Font.Color := clWindowText;

        FDrawChar.DisplayQuality := ctCleartype;
        FDrawChar.Color := Font.Color;
        FDrawChar.SetFontDetails(Font.Name, Font.Height);

        FDrawChar.DrawText(Handle, TA_RIGHT or TA_BOTTOM, R.Left+Trunc(TextRect[n].Right*FScale),
          R.Top+Trunc(TextRect[n].Bottom*FScale), ARect, FKeyData);

        //GetTextExtentPoint32W(Handle, PWideChar(FKeyValue), Length(FKeyValue), sz);
        //ExtTextOutW(Handle, ETO_CLIPPED, @ARect, PWideChar(FKeyValue), Length(FKeyValue), nil);
      end;
    end;
  end;

begin
  R := Rect(0, 0, Width, Height);
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FBackgroundBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  DrawKeyBitmap(Canvas);
end;

procedure TKeyBtn.CMMouseEnter(var Message: TMessage);
begin
  FIsHover := True;
  DrawKey;
end;

procedure TKeyBtn.CMMouseLeave(var Message: TMessage);
begin
  FIsHover := False;
  DrawKey;
end;

constructor TKeyBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawDisabled := True;
  FBackgroundBitmap := TBitmap.Create;
  ControlStyle := ControlStyle - [csOpaque];
  ParentBackground := True;
  FDataFont := TFont.Create;
  FKeyGlyph := nil; //TBitmap.Create;
end;

procedure TKeyBtn.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle := params.ExStyle or WS_EX_TRANSPARENT;
end;

destructor TKeyBtn.Destroy;
begin
  FDataFont.Free;
  FreeAndNil(FKeyGlyph);
  FreeAndNil(FBackgroundBitmap);
  inherited Destroy;
end;

procedure TKeyBtn.SetDataFont(const Value: TFont);
begin
  FDataFont.Assign(Value);
  Invalidate;
end;

procedure TKeyBtn.SetKeyType(const Value: TKeyBtnType);
begin
  FKeyType := Value;
  Invalidate;
end;

procedure TKeyBtn.SetSelected(const Value: Boolean);
begin
  FIsSelected := Value;
  Invalidate;
end;

procedure TKeyBtn.SetKeyGlyph(const Value: TBitmap);
begin
  if not Assigned(Value) then
    FreeAndNil(FKeyGlyph)
  else
  begin
    if not Assigned(FKeyGlyph) then FKeyGlyph := TBitmap.Create;
    FKeyGlyph.Assign(Value);
  end;
  Invalidate;
end;

function TKeyBtn.GetParameters: WideString;
begin
  Result := KeyBtnParametersToXML(FParameters);
end;

procedure TKeyBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FIsDown := True;
    DrawKey;
  end;
end;

procedure TKeyBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FIsDown := False;
  DrawKey;
end;

procedure TKeyBtn.SetParameters(Value: WideString);
begin
  try
    FParameters := KeyBtnParametersFromXML(Value);
  except
    ;
  end;
end;

procedure TKeyBtn.SetDisplayUnderlyingChar(const Value: Boolean);
begin
  FDisplayUnderlyingChar := Value;
  Invalidate;
end;

procedure TKeyBtn.Paint;
begin
  DrawKey;
end;

procedure TKeyBtn.Resize;
begin
  inherited;
  FBackgroundBitmap.Width := Width;
  FBackgroundBitmap.Height := Height;
  FScale := Height/33;
end;

initialization
  FDrawChar := TCleartypeDrawCharacter.Create;
finalization
  FreeAndNil(FDrawChar);
end.
