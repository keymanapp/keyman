program browser_renderer_settings_km8;

uses
  Vcl.Forms,
  UfrmMainBrowserRendererSettings in 'UfrmMainBrowserRendererSettings.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Browser Renderer Settings Correction Tool for Keyman Desktop 8';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
