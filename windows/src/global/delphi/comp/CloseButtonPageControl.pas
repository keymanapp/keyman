(*
  Name:             CloseButtonPageControl
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:      
  Create Date:      24 Jul 2015

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit CloseButtonPageControl;

interface

uses
  System.Classes,
  System.Types,
  Winapi.Messages,
  Winapi.UxTheme,
  Winapi.Windows,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Styles,
  Vcl.Themes;

type
  TCloseButtonPageControlCloseTabEvent = procedure(Sender: TObject; Index: Integer) of object;

  TCloseButtonPageControl = class(TPageControl)
  private
    FOnCloseTab: TCloseButtonPageControlCloseTabEvent;
    FCloseButtonsRect: array of TRect;
    FCloseButtonMouseDownIndex: Integer;
    FCloseButtonShowPushed: Boolean;
    FHintControl: THintWindow;
    class constructor Create;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure InitCloseButtons;
    procedure ClearTabHint;
    procedure DoShowTabHint(Index: Integer);
  protected
    procedure DoCloseTab(Index: Integer); virtual;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  published
    property OnCloseTab: TCloseButtonPageControlCloseTabEvent read FOnCloseTab write FOnCloseTab;
  end;

procedure Register;

implementation

uses
  System.Math;

procedure Register;
begin
  RegisterComponents('Keyman', [TCloseButtonPageControl]);
end;

type
  TTabControlStyleHookBtnClose = class(TTabControlStyleHook)
  private
    FHotIndex       : Integer;
    FWidthModified  : Boolean;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    function GetButtonCloseRect(Index: Integer):TRect;
  strict protected
    procedure DrawTab(Canvas: TCanvas; Index: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

constructor TTabControlStyleHookBtnClose.Create(AControl: TWinControl);
begin
  inherited;
  FHotIndex:=-1;
  FWidthModified:=False;
end;

procedure TTabControlStyleHookBtnClose.DrawTab(Canvas: TCanvas; Index: Integer);
var
  Details : TThemedElementDetails;
  ButtonR : TRect;
  FButtonState: TThemedWindow;
begin
  inherited;

  if (FHotIndex>=0) and (Index=FHotIndex) then
   FButtonState := twSmallCloseButtonHot
  else
  if Index = TabIndex then
   FButtonState := twSmallCloseButtonNormal
  else
   FButtonState := twSmallCloseButtonDisabled;

  Details := StyleServices.GetElementDetails(FButtonState);

  ButtonR:= GetButtonCloseRect(Index);
  if ButtonR.Bottom - ButtonR.Top > 0 then
   StyleServices.DrawElement(Canvas.Handle, Details, ButtonR);
end;

procedure TTabControlStyleHookBtnClose.WMLButtonUp(var Message: TWMMouse);
Var
  LPoint : TPoint;
  LIndex : Integer;
begin
  LPoint:=Message.Pos;
  for LIndex := 0 to TabCount-1 do
   if PtInRect(GetButtonCloseRect(LIndex), LPoint) then
   begin
      if Control is TPageControl then
      begin
        TCloseButtonPageControl(Control).DoCloseTab(LIndex);
      end;
      break;
   end;
end;

procedure TTabControlStyleHookBtnClose.WMMouseMove(var Message: TMessage);
Var
  LPoint : TPoint;
  LIndex : Integer;
  LHotIndex : Integer;
begin
  inherited;
  LHotIndex:=-1;
  LPoint:=TWMMouseMove(Message).Pos;
  for LIndex := 0 to TabCount-1 do
   if PtInRect(GetButtonCloseRect(LIndex), LPoint) then
   begin
      LHotIndex:=LIndex;
      break;
   end;

   if (FHotIndex<>LHotIndex) then
   begin
     FHotIndex:=LHotIndex;
     Invalidate;
   end;
end;

function TTabControlStyleHookBtnClose.GetButtonCloseRect(Index: Integer): TRect;
var
  FButtonState: TThemedWindow;
  Details : TThemedElementDetails;
  R, ButtonR : TRect;
begin
  R := TabRect[Index];
  if R.Left < 0 then Exit;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if Index = TabIndex then
      InflateRect(R, 0, 2);
  end
  else
  if Index = TabIndex then
    Dec(R.Left, 2)
  else
    Dec(R.Right, 2);

  Result := R;
  FButtonState := twSmallCloseButtonNormal;

  Details := StyleServices.GetElementDetails(FButtonState);
  if not StyleServices.GetElementContentRect(0, Details, Result, ButtonR) then
    ButtonR := Rect(0, 0, 0, 0);

  Result.Left :=Result.Right - (ButtonR.Width) - 5;
  Result.Width:=ButtonR.Width;
  Result.Height:=ButtonR.Height;
  Result.Top := (R.Height - Result.Height) div 2;
end;

procedure TTabControlStyleHookBtnClose.MouseEnter;
begin
  inherited;
  FHotIndex := -1;
end;

procedure TTabControlStyleHookBtnClose.MouseLeave;
begin
  inherited;
  if FHotIndex >= 0 then
  begin
    FHotIndex := -1;
    Invalidate;
  end;
end;
{ TCloseButtonPageControl }

procedure TCloseButtonPageControl.CMMouseLeave(var Message: TMessage);
begin
  FCloseButtonShowPushed := False;
  ClearTabHint;
  Invalidate;
end;

class constructor TCloseButtonPageControl.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TCloseButtonPageControl, TTabControlStyleHookBtnClose);
end;

procedure TCloseButtonPageControl.DoCloseTab(Index: Integer);
begin
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index)
  else
  begin
    Pages[Index].Parent:=nil;
    Pages[Index].Free;
  end;
end;

{ Close buttons on page control - theme services not in use }

procedure TCloseButtonPageControl.InitCloseButtons;
var
  I: Integer;
begin
//  TabWidth := 0;
//  OwnerDraw := False;
//  OwnerDraw := True;

  //should be done on every change of the page count
  SetLength(FCloseButtonsRect, PageCount);
  FCloseButtonMouseDownIndex := -1;

  for I := 0 to Length(FCloseButtonsRect) - 1 do
    FCloseButtonsRect[I] := Rect(0, 0, 0, 0);
end;

procedure TCloseButtonPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    for I := 0 to Length(FCloseButtonsRect) - 1 do
    begin
      if PtInRect(FCloseButtonsRect[I], Point(X, Y)) then
      begin
        FCloseButtonMouseDownIndex := I;
        FCloseButtonShowPushed := True;
        InvalidateRect(Handle, FCloseButtonsRect[i], False);
      end;
    end;
  end;
end;

procedure TCloseButtonPageControl.DoShowTabHint(Index: Integer);
var
  R: TRect;
begin
  if Pages[Index].Hint <> '' then
  begin
    if not Assigned(FHintControl) then
    begin
      FHintControl := THintWindow.Create(Self);
      FHintControl.Tag := -1;
    end;

    if FHintControl.Tag <> Index then
    begin
      ClearTabHint;
      FHintControl.Tag := Index;
      R := FHintControl.CalcHintRect(840, Pages[Index].Hint, nil);
      with TabRect(Index) do
        with ClientToScreen(Point(Left, Bottom)) do
          OffsetRect(R, X + 2, Y + 2);
      FHintControl.ActivateHint(R, Pages[Index].Hint);
    end;
  end
  else if Assigned(FHintControl) and (FHintControl.Tag <> Index) then
    ClearTabHint;
end;

procedure TCloseButtonPageControl.ClearTabHint;
begin
  if Assigned(FHintControl) then
  begin
    FHintControl.ReleaseHandle;
    FHintControl.Tag := -1;
  end;
end;

procedure TCloseButtonPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Inside: Boolean;
  i: Integer;
begin
  inherited;

  // Tab hints
  i := IndexOfTabAt(X, Y);
  if i >= 0 then
    DoShowTabHint(i)
  else
    ClearTabHint;

  // Close buttons
  if (ssLeft in Shift) and (FCloseButtonMouseDownIndex >= 0) then
  begin
    Inside := PtInRect(FCloseButtonsRect[FCloseButtonMouseDownIndex], Point(X, Y));

    if FCloseButtonShowPushed <> Inside then
    begin
      FCloseButtonShowPushed := Inside;
      InvalidateRect(Handle, FCloseButtonsRect[FCloseButtonMouseDownIndex], False);
    end;
  end;
end;

procedure TCloseButtonPageControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (FCloseButtonMouseDownIndex >= 0) then
  begin
    if PtInRect(FCloseButtonsRect[FCloseButtonMouseDownIndex], Point(X, Y)) then
    begin
      DoCloseTab(FCloseButtonMouseDownIndex);
      InvalidateRect(Handle, FCloseButtonsRect[FCloseButtonMouseDownIndex], False);
      FCloseButtonMouseDownIndex := -1;
    end;
  end;
end;

procedure TCloseButtonPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent is TTabSheet) then
    if AComponent <> Self then
      SetLength(FCloseButtonsRect, 0);
  inherited;
end;

procedure TCloseButtonPageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  TabCaption: TPoint;
  CloseBtnRect: TRect;
  CloseBtnDrawState: Cardinal;
  CloseBtnDrawDetails: TThemedElementDetails;
begin
  if Length(FCloseButtonsRect) = 0 then
    InitCloseButtons;

  if InRange(TabIndex, 0, Length(FCloseButtonsRect) - 1) then
  begin
    CloseBtnSize := 14;
    TabCaption.Y := Rect.Top + 3;

    if Active then
    begin
      CloseBtnRect.Top := Rect.Top + 4;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 6;
      Canvas.Brush.Color := $DAC379; // Keyman Light Blue
    end
    else
    begin
      CloseBtnRect.Top := Rect.Top + 3;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 3;
    end;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    FCloseButtonsRect[TabIndex] := CloseBtnRect;

    Canvas.FillRect(Rect);
    Canvas.TextOut(TabCaption.X, TabCaption.Y, Pages[TabIndex].Caption);

    if not UseThemes then
    begin
      if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      Winapi.Windows.DrawFrameControl(Canvas.Handle,
        FCloseButtonsRect[TabIndex], DFC_CAPTION, CloseBtnDrawState);
    end
    else
    begin
      Dec(FCloseButtonsRect[TabIndex].Left);

      if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
        CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonPushed)
      else
        CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonNormal);

      StyleServices.DrawElement(Canvas.Handle, CloseBtnDrawDetails,
        FCloseButtonsRect[TabIndex]);
    end;
  end;
end;

end.

