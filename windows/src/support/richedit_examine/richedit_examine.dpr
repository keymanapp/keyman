program richedit_examine;

uses
  Forms,
  main in 'main.pas' {Form1},
  RichTextLib_TLB in 'RichTextLib_TLB.pas';

{$R *.RES}
{$R version.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
