{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFooter.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando.silva@myrealbox.com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-09-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFooter;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  JvComponent, JvBitBtn, JvTypes;

type
  EJvFooterError = class(EJVCLException);
  TJvFooter = class;

  TJvFooterBtn = class(TJvBitBtn)
  private
    FCanvas: TCanvas;
    FAlignment: TAlignment;
    FButtonIndex: Integer;
    FSpaceInterval: Integer;
    function GetButtonIndex: Integer;
    procedure SetButtonIndex(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetSpaceInterval(const Value: Integer);
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
    property ButtonIndex: Integer read GetButtonIndex write SetButtonIndex;
    property SpaceInterval: Integer read FSpaceInterval write SetSpaceInterval;
  end;

  TJvFooter = class(TJvCustomPanel)
  private
    FBevelStyle: TJvBevelStyle;
    FBevelVisible: Boolean;
    procedure SetBevelStyle(Value: TJvBevelStyle);
    procedure SetBevelVisible(Value: Boolean);
    procedure UpdatePosition;
    procedure GetBtnsValues(const ABtnIndex: Integer;
      const AAlignment: TAlignment; const ADirection: Integer;
      out BtnCount, BtnTotalSpc: Integer);
  protected
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //    property DockManager;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    //property Ctl3D;
    //property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    {$IFDEF COMPILER7_UP}
    property ParentBackground;
    {$ENDIF}
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //property TabOrder;
    //property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    //property OnDockDrop;
    //property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    //property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    //property OnUnDock;
    property BevelStyle: TJvBevelStyle read FBevelStyle write SetBevelStyle default bsLowered;
    property BevelVisible: Boolean read FBevelVisible write SetBevelVisible default False;
  end;

implementation

//=== TJvFooterBtn ===========================================================

const
  DefFootWidth = 350;
  DefFootHeight = 37;
  DefFootSpace = 5;

constructor TJvFooterBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FAlignment := taRightJustify;
  FSpaceInterval := 6;
  Width := 74;
  Height := 23;
end;

destructor TJvFooterBtn.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

function TJvFooterBtn.GetButtonIndex: Integer;
var
  I: Integer;
begin
  Result := FButtonIndex;
  if Parent <> nil then
  begin
    for I := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[I] = Self then
      begin
        Result := I;
        Break;
      end;
  end;
end;

procedure TJvFooterBtn.SetButtonIndex(const Value: Integer);
begin
  if FButtonIndex <> Value then
  begin
    if Parent <> nil then
      TJvFooter(Parent).SetChildOrder(Self, Value);
    FButtonIndex := GetButtonIndex;
    if ComponentState * [csLoading, csDestroying] = [] then
      TJvFooter(Parent).UpdatePosition;
  end;
end;

procedure TJvFooterBtn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if ComponentState * [csLoading, csDestroying] = [] then
      TJvFooter(Parent).UpdatePosition;
  end;
end;

procedure TJvFooterBtn.SetSpaceInterval(const Value: Integer);
begin
  if FSpaceInterval <> Value then
  begin
    FSpaceInterval := Value;
    if ComponentState * [csLoading, csDestroying] = [] then
      TJvFooter(Parent).UpdatePosition;
  end;
end;

procedure TJvFooterBtn.WMMove(var Msg: TWMMove);
begin
  // Avoid running at runtime
  // if (csDesigning in ComponentState) then
  TJvFooter(Parent).UpdatePosition;
end;

procedure TJvFooterBtn.WMSize(var Msg: TWMSize);
begin
  // Does not allow SizeChange
  // Avoid running at runtime
  if csDesigning in ComponentState then
    SetBounds(Left, Top, Width, Height);
end;

procedure TJvFooterBtn.SetParent(AParent: TWinControl);
begin
  if AParent is TJvFooterBtn then // (p3) D6 messing up ?
    AParent := TJvFooterBtn(AParent).Parent;
  if not ((AParent is TJvFooter) or (AParent = nil)) then
    raise
      EJvFooterError.Create('TJvFooterBtn can only be placed on a TJvFooter');
  inherited SetParent(AParent);
end;

//=== TJvFooter ==============================================================

constructor TJvFooter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alBottom;
  ControlStyle := ControlStyle - [csSetCaption];
  Caption := '';
  Width := DefFootWidth;
  Height := DefFootHeight;

  BevelOuter := bvNone;
  BevelWidth := 1;
  Color := clBtnFace;
  //UseDockManager := False;
  FBevelStyle := bsLowered;
  FBevelVisible := False;
end;

destructor TJvFooter.Destroy;
begin
  inherited Destroy;
end;

procedure TJvFooter.GetBtnsValues(const ABtnIndex: Integer;
  const AAlignment: TAlignment; const ADirection: Integer;
  out BtnCount, BtnTotalSpc: Integer);
var
  Idx: Integer;

  // This function returns some total values about the buttons in use
  // BtnCount and BtnTotalSpc return values not considering the current index,
  //   except when searching all values;
  procedure DoTheCount(Idx: Integer);
  begin
    if (Controls[Idx] is TJvFooterBtn) and
      (TJvFooterBtn(Controls[Idx]).Alignment = AAlignment) then
    begin
      Inc(BtnCount);
      Inc(BtnTotalSpc, TJvFooterBtn(Controls[Idx]).SpaceInterval);
    end;
  end;

begin
  BtnCount := 0;
  BtnTotalSpc := 0;
  case ADirection of
    1: // Forward
      for Idx := ABtnIndex + 1 to ControlCount - 1 do
        DoTheCount(Idx);
    0: // All
      for Idx := 0 to ControlCount - 1 do
        DoTheCount(Idx);
    -1: // Backward
      for Idx := ABtnIndex - 1 downto 0 do
        DoTheCount(Idx);
  end;
end;

procedure TJvFooter.UpdatePosition;
var
  Idx: Integer;
  FBtnLeft, FBtnTop, FBtnWidth, FBtnHeight: Integer;
  FBtnCount, FBtnCount_2, FBtnSpace, FBtnSpace_2: Integer;
begin
  for Idx := 0 to ControlCount - 1 do
    if Controls[Idx] is TJvFooterBtn then
    begin
      FBtnTop := (Self.Height - TJvFooterBtn(Controls[Idx]).Height) div 2;
      FBtnWidth := TJvFooterBtn(Controls[Idx]).Width;
      FBtnHeight := TJvFooterBtn(Controls[Idx]).Height;

      case TJvFooterBtn(Controls[Idx]).Alignment of
        taCenter:
          begin
            // Set anchors
            TJvFooterBtn(Controls[Idx]).Anchors := [akBottom];
            // Normal return
            GetBtnsValues(TJvFooterBtn(Controls[Idx]).ButtonIndex,
              TJvFooterBtn(Controls[Idx]).Alignment, -1, FBtnCount_2,
              FBtnSpace_2);
            // Get all buttons
            GetBtnsValues(0, TJvFooterBtn(Controls[Idx]).Alignment, 0,
              FBtnCount, FBtnSpace);

            FBtnLeft := (Width div 2) -
              ((FBtnCount * FBtnWidth) + FBtnSpace) div 2 +
              (FBtnCount_2 * FBtnWidth) + FBtnSpace_2;
          end;
        taLeftJustify:
          begin
            // Set anchors
            TJvFooterBtn(Controls[Idx]).Anchors := [akLeft, akBottom];

            // get the number of backward buttons
            GetBtnsValues(TJvFooterBtn(Controls[Idx]).ButtonIndex,
              TJvFooterBtn(Controls[Idx]).Alignment, -1, FBtnCount, FBtnSpace);

            FBtnLeft := FBtnCount * FBtnWidth;
            if FBtnCount = 0 then
              FBtnLeft := FBtnLeft + DefFootSpace
            else
              FBtnLeft := FBtnLeft + FBtnSpace +
                TJvFooterBtn(Controls[Idx]).SpaceInterval;
          end;
        taRightJustify:
          begin
            // Set anchors
            TJvFooterBtn(Controls[Idx]).Anchors := [akRight, akBottom];
            // get the number of forward buttons
            GetBtnsValues(TJvFooterBtn(Controls[Idx]).ButtonIndex,
              TJvFooterBtn(Controls[Idx]).Alignment, 1, FBtnCount, FBtnSpace);

            FBtnLeft := Width - ((FBtnCount + 1) * FBtnWidth);
            if FBtnCount = 0 then
              FBtnLeft := FBtnLeft - DefFootSpace
            else
              FBtnLeft := FBtnLeft - FBtnSpace -
                TJvFooterBtn(Controls[Idx]).SpaceInterval;
          end;
      else
        FBtnLeft := 0;
      end;

      Controls[Idx].SetBounds(FBtnLeft, FBtnTop, FBtnWidth, FBtnHeight);
    end;
end;

procedure TJvFooter.SetBevelStyle(Value: TJvBevelStyle);
begin
  if Value <> FBevelStyle then
  begin
    FBevelStyle := Value;
    Invalidate;
  end;
end;

procedure TJvFooter.SetBevelVisible(Value: Boolean);
begin
  if Value <> FBevelVisible then
  begin
    FBevelVisible := Value;
    Invalidate;
  end;
end;

procedure TJvFooter.Paint;
var
  Color1, Color2: TColor;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

begin
  inherited Paint;
  // Draw Line in the top of the footer
  if FBevelVisible then
    with Canvas do
    begin
      if csDesigning in ComponentState then
      begin
        Pen.Style := psSolid;
        Pen.Mode := pmCopy;
        Pen.Color := clBlack;
        Brush.Style := bsSolid;
      end;

      Pen.Width := 1;

      if FBevelStyle = bsLowered then
      begin
        Color1 := clBtnShadow;
        Color2 := clBtnHighlight;
      end
      else
      begin
        Color1 := clBtnHighlight;
        Color2 := clBtnShadow;
      end;

      BevelLine(Color1, 0, 0, Width, 0);
      BevelLine(Color2, 0, 1, Width, 1);
    end;
end;

procedure TJvFooter.Loaded;
begin
  TJvFooter(Parent).UpdatePosition;
  inherited Loaded;
end;


end.

