program kmanalyze;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.Analyze.Main in 'Keyman.System.Analyze.Main.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
