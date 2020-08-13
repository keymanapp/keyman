program StockEditor;

uses
  Forms,
  UfrmMain in 'UfrmMain.pas' {md},
  COMMessageEditor in 'COMMessageEditor.pas' {frmCOMMessages},
  StringGridEditControlled in '..\..\global\delphi\comp\StringGridEditControlled.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  SourceRootPath in '..\devtools\SourceRootPath.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tmd, md);
  Application.Run;
end.
