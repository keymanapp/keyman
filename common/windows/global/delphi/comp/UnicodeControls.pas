unit UnicodeControls;

interface

uses Classes, Windows, Graphics, Controls, StdCtrls, ComCtrls, Buttons;

type
  TUnicodeLabel = class(TCustomLabel)
  private
    FWideCaption: WideString;
    procedure SetWideCaption(const Value: WideString);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Color;
    property Constraints;
    property Cursor;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ShowAccelChar;
    property Transparent;
    property Visible;
    property WideCaption: WideString read FWideCaption write SetWideCaption;
    property WordWrap;
  end;

  TUnicodeURLLabel = class(TUnicodeLabel)
  private
    FURL: WideString;
    procedure SetURL(const Value: WideString);
  published
    property URL: WideString read FURL write SetURL;
  end;

  TUnicodePageControl = class(TCustomTabControl)
  end;

  TUnicodeGroupBox = class(TCustomGroupBox)
  end;

  TUnicodeButton = class(TButton)
  end;

implementation

procedure UniDrawText(Handle: THandle; Text: WideString; var Rect: TRect; Flags: DWord);
begin
////  GetTextExtentPoint32W(
end;

{ TUnicodeLabel }

procedure TUnicodeLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: WideString;
begin
  Text := FWideCaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    UniDrawText(Canvas.Handle, Text, Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    UniDrawText(Canvas.Handle, Text, Rect, Flags);
  end
  else
    UniDrawText(Canvas.Handle, Text, Rect, Flags);
end;

procedure TUnicodeLabel.SetWideCaption(const Value: WideString);
begin
  FWideCaption := Value;
  Invalidate;
end;

{ TUnicodeURLLabel }

procedure TUnicodeURLLabel.SetURL(const Value: WideString);
begin
  FURL := Value;
end;

end.

