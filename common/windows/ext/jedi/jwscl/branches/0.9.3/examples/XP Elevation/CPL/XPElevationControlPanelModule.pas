unit XPElevationControlPanelModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, CtlPanel;

type
  TXPElevationAppletModule = class(TAppletModule)
    procedure AppletModuleActivate(Sender: TObject; Data: Integer);
  private
  { private-Deklarationen }
  protected
  { protected-Deklarationen }
  public
  { public-Deklarationen }
  end;

var
  XPElevationAppletModule: TXPElevationAppletModule;

implementation

uses PanelForm;

{$R *.DFM}

procedure TXPElevationAppletModule.AppletModuleActivate(Sender: TObject;
  Data: Integer);
var P : TPanel;
begin
  P := TPanel.Create(nil);
  P.ShowModal;
  P.Free;
end;

end.
