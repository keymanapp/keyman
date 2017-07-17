program test_i1940_pumpkeys;

uses
  Forms,
  UfrmPumpKeys in 'UfrmPumpKeys.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
