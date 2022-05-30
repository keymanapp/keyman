unit HintLabel;

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  THintLabel = class(TLabel)
  private
    FShowHintWindow: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetShowHintWindow(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowHintWindowNow;
    procedure HideHintWindowNow;
  published
    property ShowHintWindow: Boolean read FShowHintWindow write SetShowHintWindow;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [THintLabel]);
end;

{-------------------------------------------------------------------------------
 - Hint window                                                                 -
 ------------------------------------------------------------------------------}

type
  THintWindowLabel = class(THintWindow)
  private
    FFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    property Font: TFont read FFont write SetFont;
  end;

var
  FHintWindow: THintWindowLabel = nil;

function LightenColor(col: TColor; pct: Integer): TColor;
var
  c, r, g, b: Integer;
begin
  c := ColorToRGB(col);

  r := (c and $FF0000) shr 16;
  g := (c and $FF00) shr 8;
  b := (c and $FF);

  r := r * (100+pct) div 100; if r > 255 then r := 255;
  g := g * (100+pct) div 100; if g > 255 then g := 255;
  b := b * (100+pct) div 100; if b > 255 then b := 255;

  c := (r shl 16) + (g shl 8) + b;

  Result := TColor(c);
end;

constructor THintLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(FHintWindow) then FHintWindow := THintWindowLabel.Create(Application);
end;

procedure THintLabel.CMMouseEnter(var Message: TMessage);
begin
  if FShowHintWindow then ShowHintWindowNow;
end;

procedure THintLabel.CMMouseLeave(var Message: TMessage);
begin
  HideHintWindowNow;
end;

procedure THintLabel.SetShowHintWindow(const Value: Boolean);
begin
  FShowHintWindow := Value;
  if not Value then HideHintWindowNow;
end;

procedure THintLabel.ShowHintWindowNow;
var
  r: TRect;
  pt: TPoint;
begin
  HideHintWindowNow;

  with Canvas do
  begin
    Font := Self.Font;
    if TextWidth(Caption) < ClientWidth then Exit; // no need to show the hint window
  end;

  pt := ClientToScreen(Point(-2,-2));

  (FHintWindow as THintWindowLabel).Font := Font;
  FHintWindow.Color := LightenColor(Color, 10); // lighten by 10%

  r := FHintWindow.CalcHintRect(Screen.Width - pt.X, Caption, nil);
  r.Left := r.Left + pt.X;
  r.Right := r.Right + pt.X;
  r.Top := r.Top + pt.Y;
  r.Bottom := r.Bottom + pt.Y;
  FHintWindow.ActivateHint(r, Caption);
end;

procedure THintLabel.HideHintWindowNow;
begin
  FHintWindow.ReleaseHandle;
end;

{ THintWindowLabel }

procedure THintWindowLabel.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left, 1);
  Inc(R.Top, 1);
  Canvas.Font.Color := clBtnText;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;

function THintWindowLabel.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, PChar(AHint), -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 5);
end;


procedure THintWindowLabel.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  br: HBRUSH;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    br := CreateSolidBrush(ColorToRGB(clGray));
    FrameRect(DC, R, br);
    DeleteObject(br);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure THintWindowLabel.SetFont(const Value: TFont);
begin
  FFont := Value;
  Canvas.Font := Value;
end;

end.
