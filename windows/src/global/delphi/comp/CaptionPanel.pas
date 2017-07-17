unit CaptionPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TCaptionPanel = class(TCustomPanel)
  private
    LButtonDownState: Integer;
    LButtonPos: TSmallPoint;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
    procedure Paint; override;
  public
    property DockManager;
  published
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

procedure Register;
begin
  RegisterComponents('Keyman', [TCaptionPanel]);
end;

{ TCaptionPanel }

procedure TCaptionPanel.Paint;
begin
  Color := clCaptionText;
  DrawCaption(Handle, Canvas.Handle, ClientRect, DC_ACTIVE or DC_SMALLCAP or DC_TEXT or DC_GRADIENT);
end;

procedure TCaptionPanel.WMLButtonDown(var Message: TWMMouse);
begin
  LButtonDownState := 1;
  LButtonPos := Message.Pos;
  SetCapture(Handle);
  //Parent.Perform(WM_NCLBUTTONDOWN, HTCAPTION, MakeLong(Message.XPos, Message.YPos));
end;

procedure TCaptionPanel.WMMouseMove(var Message: TWMMouse);
begin
  if LButtonDownState = 1 then
    if (abs(Message.Pos.x - LButtonPos.x) + abs(Message.Pos.y - LButtonPos.Y) > 5) then
    begin
      GetParentForm(Self).Perform(WM_NCLBUTTONDOWN, HTCAPTION, MakeLong(Word(Message.XPos), Word(Message.YPos)));
      LButtonDownState := 2;
    end;
end;

procedure TCaptionPanel.WMLButtonUp(var Message: TWMMouse);
begin
  if LButtonDownState = 2 then
    GetParentForm(Self).Perform(WM_NCLBUTTONUP, HTCAPTION, MakeLong(Word(Message.XPos), Word(Message.YPos)));
  if LButtonDownState > 0 then ReleaseCapture;
  LButtonDownState := 0;
end;

procedure TCaptionPanel.WMLButtonDblClk(var Message: TWMMouse);
begin
  GetParentForm(Self).Perform(WM_NCLBUTTONDBLCLK, HTCAPTION, MakeLong(Word(Message.XPos), Word(Message.YPos)));
end;

end.
