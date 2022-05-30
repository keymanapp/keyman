library XPElevationControlPanel;

uses
  CtlPanel,
  XPElevationControlPanelModule in 'XPElevationControlPanelModule.pas' {XPElevationAppletModule: TAppletModule},
  PanelForm in 'PanelForm.pas' {Panel};

exports CPlApplet;

{$R *.RES}
{$R cpl.RES}

{$E cpl}

begin
  Application.Initialize;
  Application.CreateForm(TXPElevationAppletModule, XPElevationAppletModule);
  Application.Run;
end.