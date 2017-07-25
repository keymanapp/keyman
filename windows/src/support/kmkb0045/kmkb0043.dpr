program kmkb0043;

uses
  Forms,
  kbdpatchmain in 'kbdpatchmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
