unit HeadingCheckListBox;

interface

uses
  System.Types,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst;

type
  THeadingCheckListBox = class(TCheckListBox)
  private
    HeadingRows: TList;
    function GetIsHeading(Index: Integer): Boolean;
    procedure SetIsHeading(Index: Integer; const Value: Boolean);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsHeading[Index: Integer]: Boolean read GetIsHeading write SetIsHeading;
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [THeadingCheckListBox]);
end;

{ THeadingCheckListBox }

procedure THeadingCheckListBox.CNDrawItem(var Message: TWMDrawItem);
begin
  if IsHeading[Message.DrawItemStruct^.itemID] then
    with Message.DrawItemStruct^ do // Remove the check width again!
      if not UseRightToLeftAlignment then
        rcItem.Left := rcItem.Left - GetCheckWidth
      else
        rcItem.Right := rcItem.Right + GetCheckWidth;
  inherited;
end;

constructor THeadingCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HeadingRows := TList.Create;
end;

destructor THeadingCheckListBox.Destroy;
begin
  HeadingRows.Free;
  inherited;
end;

procedure THeadingCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if IsHeading[Index] then
  begin
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Canvas.TextRect(Rect, Rect.Left+2, Rect.Top, Items[Index]);
  end
  else
  begin
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    inherited;
  end;
end;

function THeadingCheckListBox.GetIsHeading(Index: Integer): Boolean;
begin
  Result := HeadingRows.IndexOf(Pointer(Index)) >= 0;
end;

procedure THeadingCheckListBox.SetIsHeading(Index: Integer; const Value: Boolean);
begin
  if Value then
  begin
    if HeadingRows.IndexOf(Pointer(Index)) < 0 then HeadingRows.Add(Pointer(Index));
  end
  else
    if HeadingRows.IndexOf(Pointer(Index)) >= 0 then HeadingRows.Remove(Pointer(Index));
end;

end.
