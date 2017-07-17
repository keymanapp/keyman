{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3@peter3.com]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvOLBar;

{ Outlook style control. Simpler than TJvLookout)
   Hierarchy:
    TJvCustomOutlookBar
      Pages:TJvOutlookBarPages
        Page:TJvOutlookBarPage
          Buttons:TJvOutlookBarButtons
            Button:TJvOutlookBarButton
}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  Buttons, Graphics, ImgList, Forms, StdCtrls,
  {$IFDEF JVCLThemesEnabled}
  Themes, UxTheme,
  {$ENDIF}
  JvComponent;

const
  CM_CAPTION_EDITING = CM_BASE + 756;
  CM_CAPTION_EDIT_ACCEPT = CM_CAPTION_EDITING + 1;
  CM_CAPTION_EDIT_CANCEL = CM_CAPTION_EDITING + 2;

type
  TJvBarButtonSize = (olbsLarge, olbsSmall);

  TJvOutlookBarButton = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: TCaption;
    FTag: Integer;
    procedure SetCaption(const Value: TCaption);
    procedure SetImageIndex(const Value: Integer);
    procedure Change;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Tag: Integer read FTag write FTag;
  end;

  TJvOutlookBarButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarButton;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarButton);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarButton;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TJvOutlookBarButton;
    property Items[Index: Integer]: TJvOutlookBarButton read GetItem write SetItem; default;
  end;

  TJvOutlookBarPage = class(TCollectionItem)
  private
    FImage: TBitmap;
    FCaption: TCaption;
    FColor: TColor;
    FButtonSize: TJvBarButtonSize;
    FParentButtonSize: Boolean;
    FParentFont: Boolean;
    FParentColor: Boolean;
    FTopButtonIndex: Integer;
    FButtons: TJvOutlookBarButtons;
    FFont: TFont;
    FImageIndex: Integer;
    FAlignment: TAlignment;
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetImage(const Value: TBitmap);
    procedure Change;
    procedure SetParentButtonSize(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetTopButtonIndex(const Value: Integer);
    procedure SetButtons(const Value: TJvOutlookBarButtons);
    procedure SetParentFont(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure DoFontChange(Sender: TObject);
  protected
    function GetDisplayName: string; override;
    { TODO: implement ImageIndex and Alignment }
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EditCaption;
  published
    property Buttons: TJvOutlookBarButtons read FButtons write SetButtons;
    property Caption: TCaption read FCaption write SetCaption;
    property Image: TBitmap read FImage write SetImage;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize;
    property ParentButtonSize: Boolean read FParentButtonSize write SetParentButtonSize default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default False;
    property ParentColor: Boolean read FParentColor write SetParentColor;
    property TopButtonIndex: Integer read FTopButtonIndex write SetTopButtonIndex;
  end;

  TJvOutlookBarPages = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvOutlookBarPage;
    procedure SetItem(Index: Integer; const Value: TJvOutlookBarPage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvOutlookBarPage;
    function Insert(Index: Integer): TJvOutlookBarPage;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvOutlookBarPage read GetItem write SetItem; default;
  end;

  TOutlookBarPageChanging = procedure(Sender: TObject; Index: Integer; var AllowChange: Boolean) of object;
  TOutlookBarPageChange = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarButtonClick = procedure(Sender: TObject; Index: Integer) of object;
  TOutlookBarEditCaption = procedure(Sender: TObject; var NewText: string;
    Index: Integer; var Allow: Boolean) of object;

  TJvCustomOutlookBar = class(TJvCustomControl)
  private
    FTopButton: TSpeedButton;
    FBtmButton: TSpeedButton;
    FPages: TJvOutlookBarPages;
    FLargeChangeLink: TChangeLink;
    FSmallChangeLink: TChangeLink;
    FActivePageIndex: Integer;
    FButtonSize: TJvBarButtonSize;
    FSmallImages: TImageList;
    FLargeImages: TImageList;
    FPageButtonHeight: Integer;
    FBorderStyle: TBorderStyle;
    FNextActivePage: Integer;
    FPressedPageBtn: Integer;
    {$IFDEF JVCLThemesEnabled}
    FHotPageBtn: Integer;
    FThemedBackGround: Boolean;
    {$ENDIF}
    FOnPageChange: TOutlookBarPageChange;
    FOnPageChanging: TOutlookBarPageChanging;
    FButtonRect: TRect;
    FLastButtonIndex: Integer;
    FPressedButtonIndex: Integer;
    FOnButtonClick: TOutlookBarButtonClick;
    FPopUpObject: TObject;
    FEdit: TCustomEdit;
    FOnEditButton: TOutlookBarEditCaption;
    FOnEditPage: TOutlookBarEditCaption;
    procedure SetPages(const Value: TJvOutlookBarPages);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetButtonSize(const Value: TJvBarButtonSize);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetLargeImages(const Value: TImageList);
    procedure SetSmallImages(const Value: TImageList);
    procedure SetPageButtonHeight(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    {$IFDEF JVCLThemesEnabled}
    procedure SetThemedBackground(const Value: Boolean);
    {$ENDIF}
    function DrawTopPages: Integer;
    procedure DrawCurrentPage(PageIndex: Integer);
    procedure DrawPageButton(R: TRect; Pressed: Boolean);
    procedure DrawBottomPages(StartIndex: Integer);
    procedure DrawButtons(Index: Integer);
    procedure DrawArrowButtons(Index: Integer);
    procedure DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
    function DrawBitmap(R: TRect; Bmp: TBitmap): Boolean;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure DoDwnClick(Sender: TObject);
    procedure DoUpClick(Sender: TObject);
    procedure RedrawRect(R: TRect; Erase: Boolean = False);
    procedure CMCaptionEditing(var Msg: TMessage); message CM_CAPTION_EDITING;
    procedure CMCaptionEditAccept(var Msg: TMessage); message CM_CAPTION_EDIT_ACCEPT;
    procedure CMCaptionEditCancel(var Msg: TMessage); message CM_CAPTION_EDIT_CANCEL;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
    procedure DoPageEdit(NewText: string; P: TJvOutlookBarPage);
    function GetActivePage: TJvOutlookBarPage;
    function GetActivePageIndex: Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetButtonHeight(PageIndex: Integer): Integer;
    function GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonTextRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
    function GetPageButtonRect(Index: Integer): TRect;
    function GetPageTextRect(Index: Integer): TRect;
    function GetPageRect(Index: Integer): TRect;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoPageChanging(Index: Integer): Boolean; virtual;
    procedure DoPageChange(Index: Integer); virtual;
    procedure DoButtonClick(Index: Integer); virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
    function GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
  protected
    property PopUpObject: TObject read FPopUpObject write FPopUpObject;
    property Width default 100;
    property Height default 220;
    property TopButton: TSpeedButton read FTopButton;
    property BtmButton: TSpeedButton read FBtmButton;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor default clBtnShadow;
    property Pages: TJvOutlookBarPages read FPages write SetPages;
    property LargeImages: TImageList read FLargeImages write SetLargeImages;
    property SmallImages: TImageList read FSmallImages write SetSmallImages;
    property ButtonSize: TJvBarButtonSize read FButtonSize write SetButtonSize default olbsLarge;
    property PageButtonHeight: Integer read FPageButtonHeight write SetPageButtonHeight default 19;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex default 0;
    {$IFDEF JVCLThemesEnabled}
    property ThemedBackground: Boolean read FThemedBackGround write SetThemedBackground default True;
    {$ENDIF}
    property OnPageChanging: TOutlookBarPageChanging read FOnPageChanging write FOnPageChanging;
    property OnPageChange: TOutlookBarPageChange read FOnPageChange write FOnPageChange;
    property OnButtonClick: TOutlookBarButtonClick read FOnButtonClick write FOnButtonClick;
    property OnEditButton: TOutlookBarEditCaption read FOnEditButton write FOnEditButton;
    property OnEditPage: TOutlookBarEditCaption read FOnEditPage write FOnEditPage;
  public
    property ActivePage: TJvOutlookBarPage read GetActivePage;
  end;

  TJvOutlookBar = class(TJvCustomOutlookBar)
  public
    property PopUpObject;
  published
    property Align;
    property Pages;
    property LargeImages;
    property SmallImages;
    property ButtonSize;
    property PageButtonHeight;
    property ActivePageIndex;
    {$IFDEF JVCLThemesEnabled}
    property ThemedBackground;
    {$ENDIF}
    property OnButtonClick;
    property OnEditButton;
    property OnPageChange;
    property OnPageChanging;
    property OnEditPage;
    property Action;
    property Anchors;
    property BiDiMode;
    property ParentBiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height;
    property HelpContext;
    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    property HelpKeyword;
    property HelpType;
    {$ENDIF COMPILER6_UP}
    // PRY END
    property Hint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
  end;

implementation

uses
  ExtCtrls;

{$R JvOUTLOOKBARRES.RES}

const
  cButtonLeftOffset = 4;
  cButtonTopOffset = 2;

function Max(Val1, Val2: Integer): Integer;
begin
  if Val2 > Val1 then
    Result := Val2
  else
    Result := Val1;
end;

function Min(Val1, Val2: Integer): Integer;
begin
  if Val2 < Val1 then
    Result := Val2
  else
    Result := Val1;
end;

//=== TJvOutlookBarEdit ======================================================

type
  TJvOutlookBarEdit = class(TCustomEdit)
  private
    FCanvas: TControlCanvas;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure EditAccept;
    procedure EditCancel;
    function GetCanvas: TCanvas;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor CreateInternal(AOwner: TComponent; AParent: TWinControl; AObject: TObject);
    destructor Destroy; override;
    procedure ShowEdit(const AText: string; R: TRect);
    property Canvas: TCanvas read GetCanvas;
  end;

constructor TJvOutlookBarEdit.CreateInternal(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  AutoSize := True;
  Visible := False;
  Parent := AParent;
  BorderStyle := bsNone;
  ParentFont := False;
  Tag := Integer(AObject);
end;

destructor TJvOutlookBarEdit.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvOutlookBarEdit.EditAccept;
begin
  Parent.Perform(CM_CAPTION_EDIT_ACCEPT, Integer(Self), Tag);
  Hide;
end;

procedure TJvOutlookBarEdit.EditCancel;
begin
  Parent.Perform(CM_CAPTION_EDIT_CANCEL, Integer(Self), Tag);
  Hide;
end;

function TJvOutlookBarEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvOutlookBarEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        Key := 0;
        EditAccept;
        if Handle = GetCapture then
          ReleaseCapture;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        if Handle = GetCapture then
          ReleaseCapture;
        EditCancel;
//      Hide;
//      Free;
//      Screen.Cursor := crDefault;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvOutlookBarEdit.KeyPress(var Key: Char);
begin
  if Key = #13 then
    Key := #0; // remove beep
  inherited KeyPress(Key);
end;

procedure TJvOutlookBarEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not PtInRect(ClientRect, Point(X, Y)) or ((Button = mbRight) and Visible) then
  begin
    if Handle = GetCapture then
      ReleaseCapture;
    EditCancel;
//    Screen.Cursor := crDefault;
//    FEdit.Hide;
//    FEdit.Free;
//    FEdit := nil;
  end
  else
  begin
    ReleaseCapture;
//    Screen.Cursor := crIBeam;
    SetCapture(Handle);
  end;
end;

procedure TJvOutlookBarEdit.ShowEdit(const AText: string; R: TRect);
begin
  Hide;
  Text := AText;
  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Show;
  SetCapture(Handle);
  SelStart := 0;
  SelLength := Length(Text);
  SetFocus;
end;

procedure TJvOutlookBarEdit.WMNCPaint(var Msg: TMessage);
//var
//  DC: HDC;
//  RC, RW: TRect;
begin
  inherited;
(*
  DC := GetWindowDC(Handle);
  try
    Canvas.Handle := DC;
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);

    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1);

{    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1);

    Canvas.Brush.Color := clBlack;
    Windows.FrameRect(DC,RW,Canvas.Brush.Handle);
    InflateRect(RW,-1,-1); }

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  finally
    ReleaseDC(Handle, DC);
  end;
  *)
end;

//=== TJvOutlookBarButton ====================================================

constructor TJvOutlookBarButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TJvOutlookBarButton.Assign(Source: TPersistent);
begin
  if Source is TJvOutlookBarButton then
  begin
    Caption := TJvOutlookBarButton(Source).Caption;
    ImageIndex := TJvOutlookBarButton(Source).ImageIndex;
    Tag := TJvOutlookBarButton(Source).Tag;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarButton.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarButtons(Collection).GetOwner <> nil) and
    (TCollectionItem(TJvOutlookBarButtons(Collection).GetOwner).Collection <> nil) and
    (TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).GetOwner).Collection).GetOwner) <> nil) then
      TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).GetOwner).Collection).GetOwner).Invalidate;
end;

procedure TJvOutlookBarButton.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(TCollectionItem(TJvOutlookBarButtons(Collection).GetOwner).Collection).GetOwner).Handle,
    CM_CAPTION_EDITING, Integer(Self), 0);
end;

function TJvOutlookBarButton.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarButton.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

//=== TJvOutlookBarButtons ===================================================

constructor TJvOutlookBarButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarButton);
end;

function TJvOutlookBarButtons.Add: TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Add);
  Result.SetCollection(self);
end;

procedure TJvOutlookBarButtons.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarButtons then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarButtons(Source).Count - 1 do
        Add.Assign(TJvOutlookBarButtons(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarButtons.GetItem(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Items[Index]);
end;

function TJvOutlookBarButtons.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

function TJvOutlookBarButtons.Insert(Index: Integer): TJvOutlookBarButton;
begin
  Result := TJvOutlookBarButton(inherited Insert(Index));
end;

procedure TJvOutlookBarButtons.SetItem(Index: Integer;
  const Value: TJvOutlookBarButton);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner <> nil then
    TJvOutlookBarPage(GetOwner).Changed(False);
end;

//=== TJvOutlookBarPage ======================================================

constructor TJvOutlookBarPage.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FButtons := TJvOutlookBarButtons.Create(Self);
  FFont := TFont.Create;
  FFont.OnChange := DoFontChange;
  FParentColor := True;
  FImage := TBitmap.Create;
  FAlignment := taCenter;
  FImageIndex := -1;
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).GetOwner <> nil) then
  begin
    FButtonSize := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).ButtonSize;
    FColor := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).Color;
    Font := TJvCustomOutlookBar(TJvOutlookBarPages(Collection).GetOwner).Font;
  end
  else
  begin
    FButtonSize := olbsLarge;
    FColor := clGray;
  end;
  Font.Color := clWhite;
  FParentButtonSize := True;
end;

destructor TJvOutlookBarPage.Destroy;
begin
  FButtons.Free;
  FImage.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvOutlookBarPage.Assign(Source: TPersistent);
var i:integer;
begin
  if Source is TJvOutlookBarPage then
  begin
    Caption := TJvOutlookBarPage(Source).Caption;
    Image := TJvOutlookBarPage(Source).Image;
    Color := TJvOutlookBarPage(Source).Color;
    ButtonSize := TJvOutlookBarPage(Source).ButtonSize;
    ParentButtonSize := TJvOutlookBarPage(Source).ParentButtonSize;
    ParentColor := TJvOutlookBarPage(Source).ParentColor;
    Buttons.Clear;
    for i := 0 to TJvOutlookBarPage(Source).Buttons.Count - 1 do
      Buttons.Add.Assign(TJvOutlookBarPage(Source).Buttons[i]);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvOutlookBarPage.Change;
begin
  if (Collection <> nil) and (TJvOutlookBarPages(Collection).UpdateCount = 0) then
    TJvOutlookBarPages(Collection).Update(Self);
end;

procedure TJvOutlookBarPage.SetTopButtonIndex(const Value: Integer);
begin
  if (FTopButtonIndex <> Value) and (Value >= 0) and (Value < Buttons.Count) then
  begin
    FTopButtonIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtons(const Value: TJvOutlookBarButtons);
begin
  FButtons.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetButtonSize(const Value: TJvBarButtonSize);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    if not (csReading in TComponent(TJvOutlookBarPages(Collection).GetOwner).ComponentState) then
      FParentButtonSize := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FParentFont := False;
end;

procedure TJvOutlookBarPage.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
  Change;
end;

procedure TJvOutlookBarPage.SetParentButtonSize(const Value: Boolean);
begin
  if FParentButtonSize <> Value then
  begin
    FParentButtonSize := Value;
    if Value then
    begin
      FButtonSize := (TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).ButtonSize;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
    begin
      FColor := (TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).Color;
      Change;
    end;
  end;
end;

procedure TJvOutlookBarPage.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    if Value then
      Font := (TJvOutlookBarPages(Collection).GetOwner as TJvCustomOutlookBar).Font;
    FParentFont := Value;
  end;
end;

procedure TJvOutlookBarPage.EditCaption;
begin
  SendMessage(TCustomControl(TJvOutlookBarPages(Collection).GetOwner).Handle, CM_CAPTION_EDITING, Integer(Self), 1);
end;

function TJvOutlookBarPage.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

procedure TJvOutlookBarPage.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Change;
  end;
end;

procedure TJvOutlookBarPage.DoFontChange(Sender: TObject);
begin
  Change;
  FParentFont := False;
end;

//=== TJvOutlookBarPages =====================================================

constructor TJvOutlookBarPages.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvOutlookBarPage);
end;

function TJvOutlookBarPages.Add: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Add);
end;

procedure TJvOutlookBarPages.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvOutlookBarPages then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvOutlookBarPages(Source).Count - 1 do
        Add.Assign(TJvOutlookBarPages(Source)[I]);
    finally
      EndUpdate
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvOutlookBarPages.GetItem(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Items[Index]);
end;

function TJvOutlookBarPages.Insert(Index: Integer): TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(inherited Insert(Index));
end;

procedure TJvOutlookBarPages.SetItem(Index: Integer;
  const Value: TJvOutlookBarPage);
begin
  inherited Items[Index] := Value;
end;

procedure TJvOutlookBarPages.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner <> nil then
    TJvCustomOutlookBar(GetOwner).Repaint;
end;

{$IFDEF JVCLThemesEnabled}

//=== TJvThemedTopBottomButton =================================================

type
  TJvThemedTopBottomButton = class(TSpeedButton)
  protected
    FIsUpBtn: Boolean;
    procedure Paint; override;
  end;

procedure TJvThemedTopBottomButton.Paint;
var
  PaintRect, ClipRect: TRect;
  Button: TThemedScrollBar;
  Details: TThemedElementDetails;
begin
  if ThemeServices.ThemesEnabled and not Flat then
  begin
    if not Enabled then
      Button := tsArrowBtnUpDisabled
    else
    if FState in [bsDown, bsExclusive] then
      Button := tsArrowBtnUpPressed
    else
    if MouseInControl then
      Button := tsArrowBtnUpHot
    else
      Button := tsArrowBtnUpNormal;

    if not FIsUpBtn then
      Button := TThemedScrollBar(Ord(tsArrowBtnDownNormal) + Ord(Button) - Ord(tsArrowBtnUpNormal));

    Details := ThemeServices.GetElementDetails(Button);

    ClipRect := ClientRect;
    with ClipRect do
      PaintRect := Rect(Left-1, Top, Right, Bottom);
    ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect, @ClipRect);
  end
  else
    inherited Paint;
end;
{$ENDIF}

//=== TJvCustomOutlookBar ====================================================

procedure TJvCustomOutlookBar.DoDwnClick(Sender: TObject);
begin
  with Pages[ActivePageIndex] do
    if TopButtonIndex < Buttons.Count then
      TopButtonIndex := TopButtonIndex + 1;
end;

procedure TJvCustomOutlookBar.DoUpClick(Sender: TObject);
begin
  with Pages[ActivePageIndex] do
    if TopButtonIndex > 0 then
      TopButtonIndex := TopButtonIndex - 1;
end;

constructor TJvCustomOutlookBar.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
  {$IFDEF JVCLThemesEnabled}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  {$ENDIF}
  Bmp := TBitmap.Create;
  try
    {$IFDEF JVCLThemesEnabled}
    FTopButton := TJvThemedTopBottomButton.Create(Self);
    TJvThemedTopBottomButton(FTopButton).FIsUpBtn := True;
    {$ELSE}
    FTopButton := TSpeedButton.Create(Self);
    {$ENDIF}
    with FTopButton do
    begin
      Parent := Self;
      Visible := False;
      Transparent := False;
      Bmp.LoadFromResourceName(hInstance, 'UPARROW');
      Glyph := Bmp;
      OnClick := DoUpClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;

    {$IFDEF JVCLThemesEnabled}
    FBtmButton := TJvThemedTopBottomButton.Create(Self);
    TJvThemedTopBottomButton(FBtmButton).FIsUpBtn := False;
    {$ELSE}
    FBtmButton := TSpeedButton.Create(Self);
    {$ENDIF}
    with FBtmButton do
    begin
      Parent := Self;
      Visible := False;
      Transparent := False;
      Bmp.LoadFromResourceName(hInstance, 'DWNARROW');
      Glyph := Bmp;
      OnClick := DoDwnClick;
      if csDesigning in ComponentState then
        Top := -1000;
    end;
  finally
    Bmp.Free;
  end;

  FPages := TJvOutlookBarPages.Create(Self);
  FLargeChangeLink := TChangeLink.Create;
  FLargeChangeLink.OnChange := DoChangeLinkChange;
  FSmallChangeLink := TChangeLink.Create;
  FSmallChangeLink.OnChange := DoChangeLinkChange;
  FEdit := TJvOutlookBarEdit.CreateInternal(Self, Self, nil);
  FEdit.Top := -1000;
  // set up defaults
  Width := 100;
  Height := 220;
  Color := clBtnShadow;
  BorderStyle := bsSingle;
  ButtonSize := olbsLarge;
  PageButtonHeight := 19;

  FPressedPageBtn := -1;
  FNextActivePage := -1;
  FLastButtonIndex := -1;
  FPressedButtonIndex := -1;
  {$IFDEF JVCLThemesEnabled}
  FHotPageBtn := -1;
  FThemedBackGround := True;
  {$ENDIF}
  ActivePageIndex := 0;
end;

destructor TJvCustomOutlookBar.Destroy;
begin
  FEdit.Free;
  FLargeChangeLink.Free;
  FSmallChangeLink.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure TJvCustomOutlookBar.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TJvCustomOutlookBar.DoChangeLinkChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvCustomOutlookBar.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TJvCustomOutlookBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FLargeImages then
      LargeImages := nil;
    if AComponent = FSmallImages then
      SmallImages := nil;
  end;
end;

procedure TJvCustomOutlookBar.DrawPageButton(R: TRect; Pressed: Boolean);
begin
  if Pressed then
  begin
    if BorderStyle = bsNone then
    begin
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1);
    end
    else
    begin
      Frame3D(Canvas, R, cl3DDkShadow, clBtnHighlight, 1);
      Frame3D(Canvas, R, clBtnShadow, clBtnFace, 1);
    end;
  end
  else
  begin
    if BorderStyle = bsNone then
    begin
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    end
    else
    begin
      Frame3D(Canvas, R, clBtnHighlight, cl3DDkShadow, 1);
      Frame3D(Canvas, R, clBtnFace, clBtnShadow, 1);
    end;
  end;
end;

function TJvCustomOutlookBar.DrawTopPages: Integer;
var
  R: TRect;
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  ToolBar: TThemedToolBar;
  Details: TThemedElementDetails;
  ClipRect: TRect;
  LColor: Cardinal;
  {$ENDIF}
begin
  R := GetPageButtonRect(0);
  for I := 0 to Pages.Count - 1 do
  begin
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if (FPressedPageBtn = I) or (FHotPageBtn = I) then
        ToolBar := ttbButtonPressed
      else
        ToolBar := ttbButtonHot;
      Details := ThemeServices.GetElementDetails(ToolBar);

      if BorderStyle = bsNone then
      begin
        ClipRect := R;
        InflateRect(R, 1, 1);
        ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
        InflateRect(R, -1, -1);
      end
      else
        ThemeServices.DrawElement(Canvas.Handle, Details, R);

      { Determine text color }
      if FPressedPageBtn = I then
        ToolBar := ttbButtonPressed
      else if FHotPageBtn = I then
        ToolBar := ttbButtonHot
      else
        ToolBar := ttbButtonNormal;
      Details := ThemeServices.GetElementDetails(ToolBar);

      with Details do
        GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, LColor);
      Canvas.Font.Color := LColor;
    end
    else
    {$ENDIF}
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
    DrawPageButton(R, FPressedPageBtn = I);
    OffsetRect(R, 0, -1);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    // TODO: add Pages[I].ImageIndex and Pages[I].Alignment to the equation
    DrawText(Canvas.Handle, PChar(Pages[I].Caption), -1, R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    OffsetRect(R, 0, PageButtonHeight + 1);
    if I >= ActivePageIndex then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Pages.Count - 1;
end;

procedure TJvCustomOutlookBar.DrawButtons(Index: Integer);
var
  I, H: Integer;
  R, R2, R3: TRect;
  C: TColor;
  {$IFDEF JVCLThemesEnabled}
  ThemedColor: Cardinal;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0) then
    Exit;
  R2 := GetPageRect(Index);
  R := GetButtonRect(Index, Pages[Index].TopButtonIndex);
  H := GetButtonHeight(Index);
  C := Canvas.Pen.Color;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(ttbButtonNormal);
    with Details do
      GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, ThemedColor);
  end;
  {$ENDIF}
  try
    Canvas.Brush.Style := bsClear;
    for I := Pages[Index].TopButtonIndex to Pages[Index].Buttons.Count - 1 do
    begin
      Canvas.Font := Pages[Index].Font;
//      Canvas.Rectangle(R);  // DEBUG
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.ThemesEnabled then
        Canvas.Font.Color := ThemedColor;
      {$ENDIF}
      case Pages[Index].ButtonSize of
        olbsLarge:
          begin
            if LargeImages <> nil then
              LargeImages.Draw(Canvas, R.Left + ((R.Right - R.Left) - LargeImages.Width) div 2, R.Top + 4,
                Pages[Index].Buttons[I].ImageIndex);
            R3 := GetButtonTextRect(ActivePageIndex, I);
//          Canvas.Rectangle(R3);  // DEBUG
            DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3,
              DT_EXPANDTABS or DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
          end;
        olbsSmall:
          begin
            if SmallImages <> nil then
              SmallImages.Draw(Canvas, R.Left + 2, R.Top + 2, Pages[Index].Buttons[I].ImageIndex);
            R3 := GetButtonTextRect(ActivePageIndex, I);
//          Canvas.Rectangle(R3);  // DEBUG
            DrawText(Canvas.Handle, PChar(Pages[Index].Buttons[I].Caption), -1, R3,
              DT_EXPANDTABS or DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOCLIP or DT_NOPREFIX);
          end;
      end;
      OffsetRect(R, 0, H);
      if R.Top >= R2.Bottom then
        Break;
    end;
  finally
    Canvas.Font := Self.Font;
    Canvas.Pen.Color := C;
  end;
end;

procedure TJvCustomOutlookBar.DrawArrowButtons(Index: Integer);
var
  R: TRect;
  H: Integer;
begin
  if (Index < 0) or (Index >= Pages.Count) or (Pages[Index].Buttons = nil) or
    (Pages[Index].Buttons.Count <= 0) then
  begin
    TopButton.Visible := False;
    BtmButton.Visible := False;
    Exit;
  end;
  R := GetPageRect(Index);
  H := GetButtonHeight(Index);
  TopButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and (Pages[Index].TopButtonIndex > 0);
  BtmButton.Visible := (Pages.Count > 0) and (R.Top < R.Bottom - 20) and
    (R.Bottom - R.Top < (Pages[Index].Buttons.Count - Pages[Index].TopButtonIndex) * H);
  // remove the last - H to show arrow
  // button when the bottom of the last button is beneath the edge
  if TopButton.Visible then
    TopButton.SetBounds(ClientWidth - 20, R.Top + 4, 16, 16)
  else
  if csDesigning in ComponentState then
    TopButton.Top := -1000;
  if BtmButton.Visible then
    BtmButton.SetBounds(ClientWidth - 20, R.Bottom - 20, 16, 16)
  else
  if csDesigning in ComponentState then
    BtmButton.Top := -1000;
end;

function TJvCustomOutlookBar.DrawBitmap(R: TRect; Bmp: TBitmap): Boolean;
begin
  Result := Assigned(Bmp) and not Bmp.Empty;
  if Result then
  begin
    Canvas.Brush.Bitmap := Bmp;
    Canvas.FillRect(R);
    Canvas.Brush.Bitmap := nil;
  end;
end;

procedure TJvCustomOutlookBar.DrawCurrentPage(PageIndex: Integer);
var
  R: TRect;
  AColor: TColor;
begin
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or (Pages[PageIndex].Buttons = nil) then
    Exit;
  R := GetPageRect(PageIndex);
  AColor := Canvas.Brush.Color;
  try
    Canvas.Brush.Color := Pages[PageIndex].Color;
    if not DrawBitmap(R, Pages[PageIndex].Image) then
      {$IFDEF JVCLThemesEnabled}
      if not ThemedBackground or not ThemeServices.ThemesEnabled then
      {$ENDIF}
        Canvas.FillRect(R);
    DrawButtonFrame(ActivePageIndex, FLastButtonIndex, FPressedButtonIndex);
    DrawButtons(PageIndex);
  finally
    Canvas.Brush.Color := AColor;
    Canvas.Brush.Style := bsClear;
    SetBkMode(Canvas.Handle, TRANSPARENT);
  end;
  DrawArrowButtons(PageIndex);
end;

procedure TJvCustomOutlookBar.DrawBottomPages(StartIndex: Integer);
var
  R: TRect;
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  ClipRect: TRect;
  ToolBar: TThemedToolBar;
  LColor: Cardinal;
  {$ENDIF}
begin
  R := GetPageButtonRect(Pages.Count - 1);
  for I := Pages.Count - 1 downto StartIndex do
  begin
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if (FPressedPageBtn = I) or (FHotPageBtn = I) then
        ToolBar := ttbButtonPressed
      else
        ToolBar := ttbButtonHot;
      Details := ThemeServices.GetElementDetails(ToolBar);

      if BorderStyle = bsNone then
      begin
        ClipRect := R;
        InflateRect(R, 1, 1);
        ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
        InflateRect(R, -1, -1);
      end
      else
        ThemeServices.DrawElement(Canvas.Handle, Details, R);

      { Determine text color }
      if FPressedPageBtn = I then
        ToolBar := ttbButtonPressed
      else if FHotPageBtn = I then
        ToolBar := ttbButtonHot
      else
        ToolBar := ttbButtonNormal;
      Details := ThemeServices.GetElementDetails(ToolBar);

      with Details do
        GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_TEXTCOLOR, LColor);
      Canvas.Font.Color := LColor;
    end
    else
    {$ENDIF}
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
    DrawPageButton(R, FPressedPageBtn = I);
    OffsetRect(R, 0, -1);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Pages[I].Caption), -1, R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    OffsetRect(R, 0, -PageButtonHeight + 1);
  end;
end;

function TJvCustomOutlookBar.GetPageButtonAtPos(P: TPoint): TJvOutlookBarPage;
var
  I: Integer;
begin
  // TODO: rewrite more optimal (no loop)
  for I := 0 to Pages.Count - 1 do
  begin
    if PtInRect(GetPageButtonRect(I), P) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TJvCustomOutlookBar.GetPageButtonRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= Pages.Count) then
    Exit;
  Result := Rect(0, 0, ClientWidth, PageButtonHeight);
  if Index <= ActivePageIndex then
    OffsetRect(Result, 0, PageButtonHeight * Index)
  else
    OffsetRect(Result, 0, (ClientHeight - PageButtonHeight * (Pages.Count - Index)));
end;

function TJvCustomOutlookBar.GetPageTextRect(Index: Integer): TRect;
begin
  Result := GetPageButtonRect(Index);
  InflateRect(Result, -2, -2);
end;

function TJvCustomOutlookBar.GetPageRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= Pages.Count) then
    Exit;
  Result := Rect(0, PageButtonHeight * Index + PageButtonHeight, ClientWidth, ClientHeight - (Pages.Count - Index) *
    PageButtonHeight + PageButtonHeight);
end;

function TJvCustomOutlookBar.GetButtonAtPos(P: TPoint): TJvOutlookBarButton;
var
  I, H: Integer;
  R, B: TRect;
begin
  // this always returns the button in the visible part of the active page (if any)
  Result := nil;
  if (ActivePageIndex < 0) or (ActivePageIndex >= Pages.Count) then
    Exit;
  B := GetButtonRect(ActivePageIndex, 0);
  H := GetButtonHeight(ActivePageIndex);
  R := GetPageRect(ActivePageIndex);
  for I := 0 to Pages[ActivePageIndex].Buttons.Count - 1 do
  begin
    if PtInRect(B, P) then
    begin
      Result := Pages[ActivePageIndex].Buttons[I];
      Exit;
    end;
    OffsetRect(B, 0, H);
    if B.Top >= R.Bottom then
      Break;
  end;
end;

function TJvCustomOutlookBar.GetButtonRect(PageIndex, ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0, 0, Max(LargeImages.Width, Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption)) +
          4, H);
        OffsetRect(Result, (ClientWidth - (Result.Right - Result.Left)) div 2, cButtonTopOffset);
      end
      else
        Result := Rect(0, 0, ClientWidth, cButtonTopOffset + H);
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0, 0, SmallImages.Width + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 8,
          H);
        OffsetRect(Result, cButtonLeftOffset, cButtonTopOffset);
      end
      else
        Result := Rect(0, 0, ClientWidth, cButtonTopOffset + H);
  end;
  OffsetRect(Result, 0, (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top);
end;

function TJvCustomOutlookBar.GetButtonFrameRect(PageIndex, ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < 0) or (ButtonIndex >= Pages[PageIndex].Buttons.Count) then
    Exit;
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result := Rect(0, 0, LargeImages.Width + 6, LargeImages.Height + 6);
        OffsetRect(Result, (ClientWidth - (Result.Right - Result.Left)) div 2,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top + 1);
      end
      else
      begin
        Result := Rect(0, 0, ClientWidth, H);
        OffsetRect(Result, 0,
          cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H + GetPageRect(PageIndex).Top + 1);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result := Rect(0, 0, SmallImages.Width + 4, SmallImages.Height + 4);
        OffsetRect(Result, cButtonLeftOffset, cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H +
          GetPageRect(PageIndex).Top);
      end
      else
      begin
        Result := Rect(0, 0, ClientWidth, H);
        OffsetRect(Result, 0, cButtonTopOffset + (ButtonIndex - Pages[PageIndex].TopButtonIndex) * H +
          GetPageRect(PageIndex).Top);
      end;
  end;
end;

function TJvCustomOutlookBar.GetButtonTextRect(PageIndex,
  ButtonIndex: Integer): TRect;
var
  H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if Pages[PageIndex].Buttons.Count <= ButtonIndex then
    Exit;
  Result := GetButtonRect(PageIndex, ButtonIndex);
  H := GetButtonHeight(PageIndex);
  case Pages[PageIndex].ButtonSize of
    olbsLarge:
      if LargeImages <> nil then
      begin
        Result.Top := Result.Bottom + Pages[PageIndex].Font.Height - 2;
        OffsetRect(Result, 0, -4);
      end;
    olbsSmall:
      if SmallImages <> nil then
      begin
        Result.Left := SmallImages.Width + 10;
        Result.Top := Result.Top + (GetButtonHeight(PageIndex) + Pages[PageIndex].Font.Height) div 2;
        Result.Bottom := Result.Top - Pages[PageIndex].Font.Height + 2;
        Result.Right := Result.Left + Canvas.TextWidth(Pages[PageIndex].Buttons[ButtonIndex].Caption) + 4;
        OffsetRect(Result, 0, -(H - (Result.Bottom - Result.Top)) div 4);
      end;
  end;
end;

procedure TJvCustomOutlookBar.Paint;
var
  I: Integer;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  R, ClipRect: TRect;
  {$ENDIF}
begin
  inherited Paint;
  {$IFDEF JVCLThemesEnabled}
  if ThemedBackground and ThemeServices.ThemesEnabled then
  begin
    R := ClientRect;
    ClipRect := R;
    InflateRect(R, 1, 0);
    Details := ThemeServices.GetElementDetails(ttbButtonHot);
    ThemeServices.DrawElement(Canvas.Handle, Details, R, @ClipRect);
  end
  else
  {$ENDIF}
  begin
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  I := DrawTopPages;
  if I >= 0 then
    DrawCurrentPage(I);
  DrawBottomPages(I + 1);
end;

function TJvCustomOutlookBar.DoPageChanging(Index: Integer): Boolean;
begin
  Result := True;
  if (Index > -1) and Assigned(FOnPageChanging) then
    FOnPageChanging(Self, Index, Result);
end;

procedure TJvCustomOutlookBar.DoPageChange(Index: Integer);
begin
  if (Index > -1) and Assigned(FOnPageChange) then
    FOnPageChange(Self, Index);
end;

procedure TJvCustomOutlookBar.DoButtonClick(Index: Integer);
begin
  if (Index > -1) and Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Index);
end;

procedure TJvCustomOutlookBar.SetActivePageIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FPages.Count) then
  begin
    FPressedPageBtn := -1; // reset cache
    // remove old button info
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
    if FActivePageIndex <> Value then
    begin
      if not DoPageChanging(Value) then
        Exit;
      FActivePageIndex := Value;
      DoPageChange(Value);
    end;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomOutlookBar.SetButtonSize(const Value: TJvBarButtonSize);
var
  I: Integer;
begin
  FButtonSize := Value;
  Pages.BeginUpdate;
  try
    for I := 0 to Pages.Count - 1 do
      if Pages[I].ParentButtonSize then
      begin
        Pages[I].ParentButtonSize := False;
        Pages[I].ParentButtonSize := True; // reset flag
      end;
  finally
    Pages.EndUpdate; // calls invalidate
  end;
end;

procedure TJvCustomOutlookBar.SetColor(const Value: TColor);
var
  I: Integer;
begin
  if inherited Color <> Value then
  begin
    inherited Color := Value;
    for I := 0 to Pages.Count - 1 do
      if Pages[I].ParentColor then
      begin
        Pages[I].ParentColor := False;
        Pages[I].ParentColor := True; // reset flag
      end;
  end;
end;

function TJvCustomOutlookBar.GetFont: TFont;
begin
  Result := inherited Font;
end;

procedure TJvCustomOutlookBar.SetFont(const Value: TFont);
var
  I: Integer;
begin
  inherited Font := Value;
  for I := 0 to Pages.Count - 1 do
    if Pages[I].ParentFont then
    begin
      Pages[I].ParentFont := False;
      Pages[I].ParentFont := True; // reset flag
    end;
end;

procedure TJvCustomOutlookBar.SetLargeImages(const Value: TImageList);
begin
  if FLargeImages <> Value then
  begin
    if Assigned(FLargeImages) then
      FLargeImages.UnRegisterChanges(FLargeChangeLink);
    FLargeImages := Value;
    if Assigned(FLargeImages) then
      FLargeImages.RegisterChanges(FLargeChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPageButtonHeight(const Value: Integer);
begin
  if FPageButtonHeight <> Value then
  begin
    FPageButtonHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.SetPages(const Value: TJvOutlookBarPages);
begin
  FPages.Assign(Value); // Assign calls Invalidate
end;

procedure TJvCustomOutlookBar.SetSmallImages(const Value: TImageList);
begin
  if FSmallImages <> Value then
  begin
    if Assigned(FSmallImages) then
      FSmallImages.UnRegisterChanges(FSmallChangeLink);
    FSmallImages := Value;
    if Assigned(FSmallImages) then
      FSmallImages.RegisterChanges(FSmallChangeLink);
    Invalidate;
  end;
end;

procedure TJvCustomOutlookBar.DrawButtonFrame(PageIndex, ButtonIndex, PressedIndex: Integer);
var
  R: TRect;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if (ButtonIndex < 0) or (PageIndex < 0) or (PageIndex >= Pages.Count) or
    (ButtonIndex < Pages[PageIndex].TopButtonIndex) then
    Exit;
  R := GetButtonFrameRect(PageIndex, ButtonIndex);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if PressedIndex = ButtonIndex then
      Details := ThemeServices.GetElementDetails(ttbButtonPressed)
    else
      Details := ThemeServices.GetElementDetails(ttbButtonHot);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  {$ENDIF}
  begin
    if PressedIndex = ButtonIndex then
      Frame3D(Canvas, R, clBlack, clWhite, 1)
    else
      Frame3D(Canvas, R, clWhite, clBlack, 1);
  end;
end;

procedure TJvCustomOutlookBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  P := GetPageButtonAtPos(Point(X, Y));
  if (P <> nil) and (P.Index <> FNextActivePage) then
  begin
    FNextActivePage := P.Index;
    if FNextActivePage <> ActivePageIndex then
    begin // draw button pressed
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FNextActivePage));
    end;
    Exit;
  end
  else
  begin
    if FNextActivePage > -1 then
      RedrawRect(GetPageButtonRect(FNextActivePage));
    FNextActivePage := -1;
    FPressedPageBtn := -1;
  end;
  B := GetButtonAtPos(Point(X, Y));
  if B <> nil then
  begin
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := B.Index;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
  R: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  { TODO -oJv :
1. check whether the mouse is down on a page button and whether the mouse has moved from
    the currently pressed page button }
  P := GetPageButtonAtPos(Point(X, Y));
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if ((P = nil) and (FHotPageBtn >= 0)) or (Assigned(P) and (P.Index <> FHotPageBtn)) then
    begin
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
      if Assigned(P) then
        FHotPageBtn := P.Index
      else
        FHotPageBtn := -1;
      if FHotPageBtn >= 0 then
      begin
        R := GetPageButtonRect(FHotPageBtn);
        RedrawRect(R);
      end;
    end;
  end;
  {$ENDIF}

  if FPressedPageBtn > -1 then
  begin
    if (P = nil) or (P.Index <> FPressedPageBtn) then
    begin
      R := GetPageButtonRect(FPressedPageBtn);
      RedrawRect(R);
      FPressedPageBtn := -1;
    end;
  end
  else
  if (P <> nil) and (P.Index <> ActivePageIndex) then
  begin
    if P.Index = FNextActivePage then
    begin
      FPressedPageBtn := FNextActivePage;
      RedrawRect(GetPageButtonRect(FPressedPageBtn));
      Exit;
    end;
  end;
  // TODO: check for button highlight
  B := GetButtonAtPos(Point(X, Y));
  if B <> nil then
  begin
    if B.Index <> FLastButtonIndex then
    begin
      RedrawRect(FButtonRect, True);
      FButtonRect := GetButtonFrameRect(ActivePageIndex, B.Index);
      RedrawRect(FButtonRect);
      FLastButtonIndex := B.Index;
    end;
  end
  else
  begin
    if FLastButtonIndex > -1 then
      RedrawRect(FButtonRect);
    FLastButtonIndex := -1;
    FButtonRect := Rect(0, 0, 0, 0);
  end;
end;

procedure TJvCustomOutlookBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TJvOutlookBarPage;
  B: TJvOutlookBarButton;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbRight then
    Exit;
  if (FNextActivePage > -1) and (FNextActivePage <> ActivePageIndex) then
  begin
    P := GetPageButtonAtPos(Point(X, Y));
    if (P <> nil) and (P.Index = FNextActivePage) then
      ActivePageIndex := FNextActivePage;
  end;
  FNextActivePage := -1;

  B := GetButtonAtPos(Point(X, Y));
  if B <> nil then
  begin
    if B.Index = FPressedButtonIndex then
      DoButtonClick(FPressedButtonIndex);
    FLastButtonIndex := B.Index;
    FPressedButtonIndex := -1;
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    RedrawRect(FButtonRect);
  end
  else
  begin
    FButtonRect := GetButtonFrameRect(ActivePageIndex, FLastButtonIndex);
    FLastButtonIndex := -1;
    FPressedButtonIndex := -1;
    RedrawRect(FButtonRect);
  end;
end;

procedure TJvCustomOutlookBar.CMMouseEnter(var Msg: TMessage);
begin
  RedrawRect(FButtonRect);
  inherited;
end;

procedure TJvCustomOutlookBar.CMMouseLeave(var Msg: TMessage);
{$IFDEF JVCLThemesEnabled}
var
  R: TRect;
{$ENDIF}
begin
  inherited;
  RedrawRect(FButtonRect);
  FPressedPageBtn := -1;
  FLastButtonIndex := -1;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and (FHotPageBtn >= 0) then
  begin
    R := GetPageButtonRect(FHotPageBtn);
    RedrawRect(R);
    FHotPageBtn := -1;
  end;
  {$ENDIF}
end;

function TJvCustomOutlookBar.GetButtonHeight(PageIndex: Integer): Integer;
const
  cLargeOffset = 8;
  cSmallOffset = 4;
var
  TM: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, TM);
  Result := TM.tmHeight + TM.tmExternalLeading;
  if (PageIndex >= 0) and (PageIndex < Pages.Count) then
  begin
    case Pages[PageIndex].ButtonSize of
      olbsLarge:
        if LargeImages <> nil then
          Result := Max(Result, LargeImages.Height - Pages[PageIndex].Font.Height + cLargeOffset)
        else
          Result := Abs(Pages[PageIndex].Font.Height) + cLargeOffset;
      olbsSmall:
        if SmallImages <> nil then
          Result := Max(SmallImages.Height, -Pages[PageIndex].Font.Height) + cSmallOffset
        else
          Result := Abs(Pages[PageIndex].Font.Height) + cSmallOffset;
    end;
  end;
  Inc(Result, 4);
end;

procedure TJvCustomOutlookBar.WMEraseBkgnd(var Msg: TMessage);
begin
  // don't redraw background: we always fill it anyway
  Msg.Result := Ord(True);
end;

procedure TJvCustomOutlookBar.RedrawRect(R: TRect; Erase: Boolean = False);
begin
  InvalidateRect(Handle, @R, Erase);
end;

procedure TJvCustomOutlookBar.CMCaptionEditing(var Msg: TMessage);
var
  R: TRect;
  B: TJvOutlookBarButton;
  P: TJvOutlookBarPage;
begin
  TJvOutlookBarEdit(FEdit).Tag := Msg.wParam;
//  TJvOutlookBarEdit(FEdit).Font.Name := Pages[ActivePageIndex].Font.Name;
//  TJvOutlookBarEdit(FEdit).Font.Size := Pages[ActivePageIndex].Font.Size;
  case Msg.lParam of
    0: // button
      begin
        B := TJvOutlookBarButton(Msg.wParam);
        R := GetButtonTextRect(ActivePageIndex, B.Index);
        R.Left := Max(R.Left, 0);
        R.Right := Min(R.Right, ClientWidth);
        TJvOutlookBarEdit(FEdit).ShowEdit(B.Caption, R);
      end;
    1: // page
      begin
        P := TJvOutlookBarPage(Msg.wParam);
        R := GetPageTextRect(P.Index);
        TJvOutlookBarEdit(FEdit).ShowEdit(P.Caption, R);
      end;
  end;
end;

procedure TJvCustomOutlookBar.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPersistent;
begin
  P := GetPageButtonAtPos(MousePos);
  if Assigned(P) then
    PopUpObject := P
  else
  begin
    P := GetButtonAtPos(MousePos);
    if Assigned(P) then
      PopUpObject := P;
  end;
  if P = nil then
    PopUpObject := Self;
  inherited DoContextPopup(MousePos, Handled);
end;

procedure TJvCustomOutlookBar.DoButtonEdit(NewText: string; B: TJvOutlookBarButton);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditButton) then
    FOnEditButton(Self, NewText, B.Index, Allow);
  if Allow then
    B.Caption := NewText;
end;

procedure TJvCustomOutlookBar.DoPageEdit(NewText: string; P: TJvOutlookBarPage);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnEditPage) then
    FOnEditPage(Self, NewText, P.Index, Allow);
  if Allow then
    P.Caption := NewText;
end;

procedure TJvCustomOutlookBar.CMCaptionEditAccept(var Msg: TMessage);
begin
  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarButton(LParam))
    else
    if TObject(LParam) is TJvOutlookBarPage then
      DoPageEdit(TJvOutlookBarEdit(WParam).Text, TJvOutlookBarPage(LParam));
  end;
end;

procedure TJvCustomOutlookBar.CMCaptionEditCancel(var Msg: TMessage);
begin
{  with Msg do
  begin
    if TObject(LParam) is TJvOutlookBarButton then
      DoButtonEditCancel(TJvOutlookBarButton(LParam))
    else TObject(LParam) is TJvOutlookBarPage then
      DoPageEditCancel(TJvOutlookBarPage(LParam));
  end;
  }
end;

function TJvCustomOutlookBar.GetActivePage: TJvOutlookBarPage;
begin
  if (ActivePageIndex > -1) and (ActivePageIndex < Pages.Count) then
    Result := Pages[ActivePageIndex]
  else
    Result := nil;
end;

function TJvCustomOutlookBar.GetActivePageIndex: Integer;
begin
  if (FActivePageIndex < 0) or (FActivePageIndex >= FPages.Count) then
    FActivePageIndex := 0;
  Result := FActivePageIndex;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvCustomOutlookBar.SetThemedBackground(const Value: Boolean);
begin
  if Value <> FThemedBackGround then
  begin
    FThemedBackGround := Value;
    if ([csDesigning, csLoading] * ComponentState = []) and ThemeServices.ThemesEnabled then
      Repaint;
  end;
end;
{$ENDIF}

end.

