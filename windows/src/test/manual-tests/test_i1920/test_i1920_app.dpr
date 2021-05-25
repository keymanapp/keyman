program test_i1920_app;

uses
  Forms,
  test_i1920 in 'test_i1920.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
