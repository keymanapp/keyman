program mkver;

uses
  System.SysUtils,
  Main in 'Main.pas',
  TagFunctions in 'TagFunctions.pas',
  Keyman.System.KeymanVersionInfo in '..\..\global\delphi\general\Keyman.System.KeymanVersionInfo.pas';

//  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas';

{$R *.RES}

begin
  try
    Run;
  except
    on E:Exception do
    begin
      writeln('Fatal error: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
