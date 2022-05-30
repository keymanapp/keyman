{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCtrls.PAS, released May 13, 2000.

The Initial Developer of the Original Code is Petr Vones (petr.v@mujmail.cz)
Portions created by Petr Vones are Copyright (C) 2000 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: Jun 18, 2000
Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ImgList, ActnList,
  JvButton, JVCLVer, JvListBox;

type
  TJvListBox = class(TJvCustomListBox)
  public
    {$IFDEF COMPILER6_UP}
    property Count;
    {$ENDIF COMPILER6_UP}
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;

    property Multiline;
    property SelectedColor;
    property SelectedTextColor;
    property DisabledTextColor;
    property ShowFocusRect;
    property Background;

    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Alignment;
    property HotTrack;
    property HintColor;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
    property OnSelectCancel;
    property OnChange;
    property OnDeleteString;
    property OnAddString;
    property OnVerticalScroll;
    property OnHorizontalScroll;
  end;

  TJvImgBtnLayout = (blImageLeft, blImageRight);

  TJvImgBtnKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose,
    bkAbort, bkRetry, bkIgnore, bkAll);

  TJvCustomImageButton = class;

  TJvImgBtnActionLink = class(TButtonActionLink)
  protected
    FClient: TJvCustomImageButton;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TJvImgBtnDrawEvent = procedure(Sender: TObject; const DrawItemStruct: TDrawItemStruct) of object;
  TJvImgBtnAnimIndexEvent = procedure(Sender: TObject; CurrentAnimateFrame: Byte;
    var ImageIndex: Integer) of object;

  TJvCustomImageButton = class(TJvCustomButton)
  private
    FAlignment: TAlignment;
    FAnimate: Boolean;
    FAnimateFrames: Integer;
    FAnimateInterval: Cardinal;
    FAnimating: Boolean;
    FCanvas: TCanvas;
    FCurrentAnimateFrame: Byte;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FIsFocused: Boolean;
    FKind: TJvImgBtnKind;
    FLayout: TJvImgBtnLayout;
    FOwnerDraw: Boolean;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseInControl: Boolean;
    FOnButtonDraw: TJvImgBtnDrawEvent;
    FOnGetAnimateIndex: TJvImgBtnAnimIndexEvent;
    FImageVisible: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnimate(const Value: Boolean);
    procedure SetAnimateFrames(const Value: Integer);
    procedure SetAnimateInterval(const Value: Cardinal);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetKind(const Value: TJvImgBtnKind);
    procedure SetLayout(const Value: TJvImgBtnLayout);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); dynamic;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetCustomCaption: string; dynamic;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetKindImageIndex: Integer;
    function GetRealCaption: string;override;
    procedure InvalidateImage;
    function IsImageVisible: Boolean;
    procedure Loaded; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure ShowNextFrame;
    procedure StartAnimate;
    procedure StopAnimate;
    procedure RestartAnimate;
    class procedure InitializeDefaultImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DrawButtonImage(ImageBounds: TRect);
    procedure DrawButtonFocusRect(const RectContent: TRect);
    procedure DrawButtonFrame(const DrawItemStruct: TDrawItemStruct; var RectContent: TRect);
    procedure DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
    property Canvas: TCanvas read FCanvas;
    property CurrentAnimateFrame: Byte read FCurrentAnimateFrame;
    property MouseInControl: Boolean read FMouseInControl;
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property AnimateFrames: Integer read FAnimateFrames write SetAnimateFrames default 0;
    property AnimateInterval: Cardinal read FAnimateInterval write SetAnimateInterval default 200;
    property Color default clBtnFace;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default True;
    property Kind: TJvImgBtnKind read FKind write SetKind default bkCustom;
    property Layout: TJvImgBtnLayout read FLayout write SetLayout default blImageLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property OnButtonDraw: TJvImgBtnDrawEvent read FOnButtonDraw write FOnButtonDraw;
    property OnGetAnimateIndex: TJvImgBtnAnimIndexEvent read FOnGetAnimateIndex write FOnGetAnimateIndex;

  end;

  TJvImgBtn = class(TJvCustomImageButton)
  published
    property AboutJVCL;
    property Alignment;
    property Animate;
    property AnimateFrames;
    property AnimateInterval;
    property Color;
    property DropDownMenu;
    property HotTrack;
    property HotTrackFont;
    property HintColor;
    property Images;
    property ImageIndex;
    property ImageVisible;
    property Kind;
    property Layout;
    property Margin;
    property Spacing;
    property WordWrap;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
    property OwnerDraw;
    property OnButtonDraw;
    property OnGetAnimateIndex;
  end;

implementation

uses
  Consts,
  {$IFDEF JVCLThemesEnabled}
  Themes,
  {$ENDIF}
  JvFunctions;

{$R *.res}

resourcestring
  RsLBVirtualCantBeSorted = 'ListBox doesn''t allow sorting in virtual mode';

const
  JvImgBtnModalResults: array [TJvImgBtnKind] of TModalResult =
    (mrNone, mrOk, mrCancel, mrNone, mrYes, mrNo, mrNone,
     mrAbort, mrRetry, mrIgnore, mrAll);

  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  DefaultImgBtnImagesList: TImageList = nil;

//=== TJvImgBtnActionLink ====================================================

procedure TJvImgBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvCustomImageButton;
end;

function TJvImgBtnActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TJvImgBtnActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

//=== TJvCustomImageButton ==============================================================

constructor TJvCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FAlignment := taCenter;
  FAnimateInterval := 200;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
  FImageVisible := True;
  FKind := bkCustom;
  FLayout := blImageLeft;
  FMargin := -1;
  FSpacing := 4;
  Color := clBtnFace;
end;

destructor TJvCustomImageButton.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TJvCustomImageButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
end;

procedure TJvCustomImageButton.CreateWnd;
begin
  inherited CreateWnd;
  if FAnimate then
    StartAnimate;
end;

procedure TJvCustomImageButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if ActionList <> nil then
        Self.SetImages(ActionList.Images);
      Self.SetImageIndex(ImageIndex);
      Invalidate;
    end;
end;

procedure TJvCustomImageButton.CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect);
var
  BlockWidth, ButtonWidth, ButtonHeight, BlockMargin, InternalSpacing: Integer;
begin
  SetRect(RectText, 0, 0, 0, 0);
  //  RectText.Right := ButtonRect.Right - ButtonRect.Left;
  DrawText(Canvas.Handle, PChar(GetRealCaption), -1, RectText, DT_CALCRECT or
    Alignments[FAlignment]);
  if IsImageVisible then
  begin
    with GetImageList do
      SetRect(RectImage, 0, 0, Width - 1, Height - 1);
    InternalSpacing := Spacing;
  end
  else
  begin
    SetRect(RectImage, 0, 0, 0, 0);
    InternalSpacing := 0;
  end;
  BlockWidth := RectImage.Right + InternalSpacing + RectText.Right;
  ButtonWidth := ButtonRect.Right - ButtonRect.Left;
  if Margin = -1 then
    BlockMargin := (ButtonWidth - BlockWidth) div 2
  else
    BlockMargin := Margin;
  case Layout of
    blImageLeft:
      begin
        OffsetRect(RectImage, BlockMargin, 0);
        OffsetRect(RectText, RectImage.Right + InternalSpacing, 0);
      end;
    blImageRight:
      begin
        OffsetRect(RectImage, ButtonWidth - BlockMargin - RectImage.Right, 0);
        OffsetRect(RectText, ButtonWidth - BlockWidth - BlockMargin, 0);
      end;
  end;
  ButtonHeight := ButtonRect.Bottom - ButtonRect.Top;
  OffsetRect(RectImage, ButtonRect.Left, (ButtonHeight - RectImage.Bottom) div 2 + ButtonRect.Top);
  OffsetRect(RectText, ButtonRect.Left, (ButtonHeight - RectText.Bottom) div 2 + ButtonRect.Top);
end;

procedure TJvCustomImageButton.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then
          Form.Close
        else
          inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then
          Application.HelpContext(Control.HelpContext)
        else
          inherited Click;
      end;
  else
    inherited Click;
  end;
end;

procedure TJvCustomImageButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomImageButton.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomImageButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    DoMouseEnter;
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      Repaint;
    {$ENDIF}
  end;
end;

procedure TJvCustomImageButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled and not Dragging then
  begin
    FMouseInControl := False;
    DoMouseLeave;
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      Repaint;
    {$ENDIF}
  end;
end;

procedure TJvCustomImageButton.CNDrawItem(var Msg: TWMDrawItem);
begin
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  try
    FCanvas.Font := Font;
    if FOwnerDraw and Assigned(FOnButtonDraw) then
      FOnButtonDraw(Self, Msg.DrawItemStruct^)
    else
      DrawItem(Msg.DrawItemStruct^);
  finally
    FCanvas.Handle := 0;
  end;
end;

procedure TJvCustomImageButton.CNMeasureItem(var Msg: TWMMeasureItem);
begin
  with Msg.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TJvCustomImageButton.DrawButtonFocusRect(const RectContent: TRect);
begin
  if FIsFocused then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, RectContent);
  end;
end;

procedure TJvCustomImageButton.DrawButtonFrame(const DrawItemStruct: TDrawItemStruct; var RectContent: TRect);
var
  IsDown, IsEnabled, IsDefault: Boolean;
  R: TRect;
  Flags: DWORD;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  Button: TThemedButton;
  {$ENDIF}
begin
  with DrawItemStruct do
  begin
    IsEnabled := itemState and ODS_DISABLED = 0;
    IsDown := (itemState and ODS_SELECTED <> 0) and IsEnabled;
    IsDefault := itemState and ODS_FOCUS <> 0;
  end;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if not IsEnabled then
      Button := tbPushButtonDisabled
    else
    if IsDown then
      Button := tbPushButtonPressed
    else
    if FMouseInControl then
      Button := tbPushButtonHot
    else
    if IsDefault then
      Button := tbPushButtonDefaulted
    else
      Button := tbPushButtonNormal;

    Details := ThemeServices.GetElementDetails(Button);
    // Parent background.
    ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True);
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    // Return content rect
    RectContent := ThemeServices.ContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then
      Flags := Flags or DFCS_PUSHED;
    if not IsEnabled then
      Flags := Flags or DFCS_INACTIVE;

    if FIsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end;

    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(FCanvas.Handle, R, DFC_BUTTON, Flags);
    FCanvas.Brush.Color := Color;
    FCanvas.FillRect(R);

    // Return content rect
    RectContent := ClientRect;
    InflateRect(RectContent, -4, -4);
  end;
end;

procedure TJvCustomImageButton.DrawButtonImage(ImageBounds: TRect);
begin
  with ImageBounds do
    if IsImageVisible then
    begin
      if Assigned(FImages) then
        FImages.Draw(FCanvas, Left, Top, GetImageIndex, Enabled)
      else
        DefaultImgBtnImagesList.Draw(FCanvas, Left, Top, GetKindImageIndex, Enabled);
    end;
end;

procedure TJvCustomImageButton.DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
var
  Flags: DWORD;
  RealCaption: string;
begin
  Flags := DrawTextBiDiModeFlags(DT_VCENTER or Alignments[FAlignment]);
  RealCaption := GetRealCaption;
  with Canvas do
  begin
    Brush.Style := bsClear;
    if not TextEnabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
    end
    else
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
  end;
end;

procedure TJvCustomImageButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  R, RectContent, RectText, RectImage: TRect;
begin
  DrawButtonFrame(DrawItemStruct, RectContent);

  //R := ClientRect;
  //InflateRect(R, -4, -4);
  R := RectContent;
  if (DrawItemStruct.itemState and ODS_SELECTED <> 0) and Enabled then
  begin
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      OffsetRect(R, 1, 0)
    else
    {$ENDIF}
      OffsetRect(R, 1, 1);
  end;

  CalcButtonParts(R, RectText, RectImage);
  DrawButtonText(RectText, Enabled);
  DrawButtonImage(RectImage);

  DrawButtonFocusRect(RectContent);
end;

function TJvCustomImageButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvImgBtnActionLink;
end;

function TJvCustomImageButton.GetCustomCaption: string;
const
  Captions: array [TJvImgBtnKind] of string =
    ('', SOKButton, SCancelButton, SHelpButton, SYesButton, SNoButton,
      SCloseButton, SAbortButton, SRetryButton, SIgnoreButton, SAllButton);
begin
  Result := Captions[FKind];
end;

function TJvCustomImageButton.GetImageIndex: Integer;
begin
  if FAnimating then
  begin
    Result := FImageIndex + FCurrentAnimateFrame - 1;
    if Assigned(FOnGetAnimateIndex) then
      FOnGetAnimateIndex(Self, FCurrentAnimateFrame, Result);
  end
  else
    Result := FImageIndex;
end;

function TJvCustomImageButton.GetImageList: TCustomImageList;
begin
  if Assigned(FImages) then
    Result := FImages
  else
    Result := DefaultImgBtnImagesList;
end;

function TJvCustomImageButton.GetKindImageIndex: Integer;
const
  ImageKindIndexes: array [TJvImgBtnKind] of Integer =
    (-1, 2, 4, 0, 3, 1, 5, 8, 6, 9, 7);
begin
  Result := ImageKindIndexes[FKind];
end;

function TJvCustomImageButton.GetRealCaption: string;
begin
  if (FKind <> bkCustom) and (Caption = '') then
    Result := GetCustomCaption
  else
    Result := inherited GetRealCaption;
end;

procedure TJvCustomImageButton.ImageListChange(Sender: TObject);
begin
  InvalidateImage;
end;

class procedure TJvCustomImageButton.InitializeDefaultImageList;
begin
  if not Assigned(DefaultImgBtnImagesList) then
  begin
    DefaultImgBtnImagesList := TImageList.CreateSize(18, 18);
    DefaultImgBtnImagesList.ResourceLoad(rtBitmap, 'JVIMGBTNDEFAULT', clOlive);
  end;
end;

procedure TJvCustomImageButton.InvalidateImage;
begin
  Invalidate;
end;

function TJvCustomImageButton.IsImageVisible: Boolean;
begin
  Result := FImageVisible and
    ((Assigned(FImages) and (GetImageIndex <> -1)) or
    (not Assigned(FImages) and (FKind <> bkCustom)));
end;

procedure TJvCustomImageButton.Loaded;
begin
  inherited Loaded;
  if FAnimate then
    StartAnimate;
end;

procedure TJvCustomImageButton.RestartAnimate;
begin
  if FAnimating then
  begin
    StopAnimate;
    StartAnimate;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetAnimate(const Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    FAnimate := Value;
    if Value then
      StartAnimate
    else
      StopAnimate;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetAnimateFrames(const Value: Integer);
begin
  if FAnimateFrames <> Value then
  begin
    FAnimateFrames := Value;
    RestartAnimate;
  end;
end;

procedure TJvCustomImageButton.SetAnimateInterval(const Value: Cardinal);
begin
  if FAnimateInterval <> Value then
  begin
    FAnimateInterval := Value;
    RestartAnimate;
  end;
end;

procedure TJvCustomImageButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Refresh;
  end;
end;

procedure TJvCustomImageButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end
  else
    SetImageIndex(-1);
  InvalidateImage;
end;

procedure TJvCustomImageButton.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetKind(const Value: TJvImgBtnKind);
begin
  if FKind <> Value then
  begin
    if Value <> bkCustom then
    begin
      InitializeDefaultImageList;
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];
      if not (csLoading in ComponentState) and (FKind = bkCustom) then
      begin
        Caption := '';
        Images := nil;
      end;
    end;
    ModalResult := JvImgBtnModalResults[Value];
    FKind := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetLayout(const Value: TJvImgBtnLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if (csDesigning in ComponentState) and (FAlignment <> taCenter) then
      case FLayout of
        blImageLeft: FAlignment := taLeftJustify;
        blImageRight: FAlignment := taRightJustify;
      end;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetMargin(const Value: Integer);
begin
  if (FMargin <> Value) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetOwnerDraw(const Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.ShowNextFrame;
begin
  Inc(FCurrentAnimateFrame);
  if FCurrentAnimateFrame > FAnimateFrames then
    FCurrentAnimateFrame := 1;
  InvalidateImage;
end;

procedure TJvCustomImageButton.StartAnimate;
begin
  if ComponentState * [csDesigning, csLoading] = [] then
  begin
    DoubleBuffered := True;
    FCurrentAnimateFrame := 0;
    ShowNextFrame;
    OSCheck(SetTimer(Handle, 1, FAnimateInterval, nil) <> 0);
    FAnimating := True;
  end;
end;

procedure TJvCustomImageButton.StopAnimate;
begin
  if FAnimating then
  begin
    KillTimer(Handle, 1);
    FCurrentAnimateFrame := 0;
    DoubleBuffered := False;
    FAnimating := False;
  end;
end;

procedure TJvCustomImageButton.WMDestroy(var Msg: TWMDestroy);
begin
  StopAnimate;
  inherited;
end;

procedure TJvCustomImageButton.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Msg.Keys, Longint(Msg.Pos));
end;

procedure TJvCustomImageButton.WMTimer(var Msg: TWMTimer);
begin
  if Msg.TimerID = 1 then
  begin
    ShowNextFrame;
    Msg.Result := 1;
  end
  else
    inherited;
end;

initialization

finalization
  FreeAndNil(DefaultImgBtnImagesList);

end.

