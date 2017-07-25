program test_finddefaultlang_latin;

uses
  Forms,
  finddefaultlang_latin in 'finddefaultlang_latin.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
