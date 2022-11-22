(*
  Name:             PaintPanel
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    10 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Add DrawText, EraseBackground and OnEraseBackground
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    10 Aug 2015 - mcdurdin - I4851 - Consolidate DrawText and ShowCaption in TPaintPanel
*)
unit PaintPanel;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPaintPanel = class(TCustomPanel)
  private
    FOnPaint: TNotifyEvent;
    FEraseBackground: Boolean;
    FOnEraseBackground: TNotifyEvent;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DockManager;

    property Canvas;
  published
    property EraseBackground: Boolean read FEraseBackground write FEraseBackground default True;
    property OnEraseBackground: TNotifyEvent read FOnEraseBackground write FOnEraseBackground;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;   // I4851
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

uses
  System.Types;

procedure Register;
begin
  RegisterComponents('Keyman', [TPaintPanel]);
end;

{ TPaintPanel }

constructor TPaintPanel.Create(AOwner: TComponent);
begin
  FEraseBackground := True;
  inherited Create(AOwner);
end;

procedure TPaintPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VerticalAlignments: array[TVerticalAlignment] of Longint = (DT_TOP, DT_BOTTOM, DT_VCENTER);   // I4796
var
  Rect: TRect;
  Flags: Longint;
begin
  Rect := GetClientRect;   // I4796

  if not Assigned(FOnPaint) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end
  else
    FOnPaint(Self);

  if ShowCaption and (Caption <> '') then   // I4851
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Self.Font;
    Flags := DT_EXPANDTABS or DT_SINGLELINE or
      VerticalAlignments[VerticalAlignment] or Alignments[Alignment];
    Flags := DrawTextBiDiModeFlags(Flags);
    Windows.DrawText(Canvas.Handle, Caption, -1, Rect, Flags);
  end;
end;

procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Assigned(FOnEraseBackground) then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := Message.DC;
      try
        FOnEraseBackground(Self);
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end
  else if FEraseBackground then
    inherited
  else
    Message.Result := 1;
end;

end.
