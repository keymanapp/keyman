program prjIEGuidDemo;

uses
  Forms,
  ieguiddemo_u in 'ieguiddemo_u.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
