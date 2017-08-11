program tds2pdb;

uses
  Forms,
  tds2pdbMain in 'tds2pdbMain.pas' {Form1},
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
