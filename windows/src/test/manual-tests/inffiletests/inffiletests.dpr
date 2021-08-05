program inffiletests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  inffilereader in 'inffilereader.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
