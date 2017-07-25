program genkmnp;

uses
  Forms,
  genkmn in 'genkmn.pas' {Form1},
  testkbd in 'testkbd.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
