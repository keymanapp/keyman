unit PanelOverlapped;

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPanelOverlapped = class(TPanel)
  private
    FNextPage: TPanelOverlapped;
    NotifyList: TList;
    { Private declarations }
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure SetNextPage(const Value: TPanelOverlapped);

    procedure RemoveNotify(Owner: TPanelOverlapped);
    procedure NotifyDestroy(Sender: TPanelOverlapped);
    procedure AddNotify(Owner: TPanelOverlapped);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    { Published declarations }
    property NextPage: TPanelOverlapped read FNextPage write SetNextPage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [TPanelOverlapped]);
end;

{ TPanelOverlapped }

procedure TPanelOverlapped.AddNotify(Owner: TPanelOverlapped);
begin
  NotifyList.Add(Owner);  
end;

procedure TPanelOverlapped.RemoveNotify(Owner: TPanelOverlapped);
begin
  NotifyList.Remove(Owner);
end;

procedure TPanelOverlapped.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if (Message.YPos < 8) and (Message.XPos > Width - 8) then Message.Result := 1
  else inherited;
end;

procedure TPanelOverlapped.NotifyDestroy(Sender: TPanelOverlapped);
begin
  if FNextPage = Sender then FNextPage := nil;
end;

procedure TPanelOverlapped.Paint;
var
  r: TRect;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    r := Rect(Width - 8, 0, Width-1, 7);
    Canvas.Brush.Color := clRed;
    Canvas.FrameRect(r);
    Canvas.Brush.Color := clBlue;
    r := Rect(Width - 7, 1, Width-2, 6);
    Canvas.FillRect(r);
  end;
end;

procedure TPanelOverlapped.SetNextPage(const Value: TPanelOverlapped);
begin
  if Assigned(FNextPage) then
    FNextPage.RemoveNotify(Self);
  FNextPage := Value;
  if Assigned(FNextPage) then
    FNextPage.AddNotify(Self);
end;

procedure TPanelOverlapped.WMLButtonDown(var Message: TWMLButtonDown);
var
  i: Integer;
begin
  if not (csDesigning in ComponentState) then inherited
  else
  begin
    if GetAsyncKeyState(VK_SHIFT) < 0 then
      for i := 0 to Parent.ControlCount - 1 do
      begin
        if (Parent.Controls[i] is TPanelOverlapped) and ((Parent.Controls[i] as TPanelOverlapped).NextPage = Self) then
        begin
          Parent.Controls[i].BringToFront;
          Break;
        end;
      end
    else if Assigned(FNextPage) then FNextPage.BringToFront;
  end;
end;

constructor TPanelOverlapped.Create(AOwner: TComponent);
begin
  NotifyList := TList.Create;
  inherited Create(AOwner);
end;

destructor TPanelOverlapped.Destroy;
var
  i: Integer;
begin
  for i := 0 to NotifyList.Count - 1 do
    TPanelOverlapped(NotifyList[i]).NotifyDestroy(Self);
  NotifyList.Free;
  NextPage := nil;
  inherited Destroy;
end;

end.
