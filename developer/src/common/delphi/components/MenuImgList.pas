unit MenuImgList;
{
   ActionLinks, a new feature of Delphi 4,  can provide a better way of 
   organising Menus and Toolbars.  The ability to draw bitmaps in MenuItems
   is another nice feature of Delphi 4.  The only problem is that bitmaps of
   disabled Menu Items look ugly.  (Look at DELPHI IDE).
   This little unit allows the use of a separate DisabledImages list for 
   bitmaps of disabled menu items.

   Usage:  Instead of using a standard image list use instead TMenuImgList for
   the normal MenuItem bitmaps.  Link its DisabledImages property to a normal 
   ImageList containing images for disabled items.

   Note:  The DisabledImages list can be shared between toolbars and Menus.
}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, CommCtrl;

type
  TMenuImgList = class(TImageList)
  private
    { Private declarations }
    fDisabledImages : TCustomImageList;
    FDisabledImageChangeLink : TChangeLink;
    procedure SetDisabledImages(const Value: TCustomImageList);
    procedure DisabledImagesChanged(Sender: TObject);
  protected
    { Protected declarations }
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
           Style: Cardinal; Enabled: Boolean); override;
    procedure Notification(AComponent: TComponent;
              Operation: TOperation);  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DisabledImages : TCustomImageList read fDisabledImages write SetDisabledImages;
  end;

procedure Register;

implementation
Uses
  Consts;

constructor TMenuImgList.Create(AOwner: TComponent);
begin
  inherited;
  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImagesChanged;
end;

destructor TMenuImgList.Destroy;
begin
  FDisabledImageChangeLink.Free;
  inherited;
end;

procedure TMenuImgList.DisabledImagesChanged(Sender: TObject);
begin
  Change;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

procedure TMenuImgList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean);
begin
  if HandleAllocated then
  begin
    if (not Enabled) and (DisabledImages <> nil) then
      ImageList_DrawEx(DisabledImages.Handle, Index, Canvas.Handle,
                       X, Y, 0, 0,GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
      inherited;
  end;
end;

procedure TMenuImgList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DisabledImages) and (Operation = opRemove) then DisabledImages := nil;
end;

procedure TMenuImgList.SetDisabledImages(const Value: TCustomImageList);
begin
  if FDisabledImages <> nil then FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);
  FDisabledImages := Value;
  if FDisabledImages <> nil then
  begin
    FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
    FDisabledImages.FreeNotification(Self);
  end;
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TMenuImgList]);
end;


end.
