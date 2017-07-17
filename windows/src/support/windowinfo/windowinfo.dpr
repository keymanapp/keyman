program windowinfo;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.RES}
{$R version.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
