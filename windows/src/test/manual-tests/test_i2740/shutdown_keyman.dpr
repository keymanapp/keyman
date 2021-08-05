program shutdown_keyman;

uses
  Forms,
  shutdown_keyman_test in 'shutdown_keyman_test.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
