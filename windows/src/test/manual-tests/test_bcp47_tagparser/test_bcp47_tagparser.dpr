program test_bcp47_tagparser;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  TestBCP47 in 'TestBCP47.pas',
  BCP47SuppressScriptRegistry in '..\..\global\delphi\general\BCP47SuppressScriptRegistry.pas',
  BCP47Tag in '..\..\global\delphi\general\BCP47Tag.pas';

begin
  try
    Run
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
