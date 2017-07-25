program i4870;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amakrits');
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
