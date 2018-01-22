program cvt;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  cv in 'cv.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in 'C:\Projects\keyman\open\windows\src\global\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.KMXFileLanguages in 'C:\Projects\keyman\open\windows\src\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  utilstr in 'C:\Projects\keyman\open\windows\src\global\delphi\general\utilstr.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in 'C:\Projects\keyman\open\windows\src\global\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  utilfiletypes in 'C:\Projects\keyman\open\windows\src\global\delphi\general\utilfiletypes.pas',
  utildir in 'C:\Projects\keyman\open\windows\src\global\delphi\general\utildir.pas',
  OnlineConstants in 'C:\Projects\keyman\open\windows\src\global\delphi\productactivation\OnlineConstants.pas',
  kmxfile in 'C:\Projects\keyman\open\windows\src\global\delphi\general\kmxfile.pas',
  CRC32 in 'C:\Projects\keyman\open\windows\src\global\delphi\general\CRC32.pas',
  KeyNames in 'C:\Projects\keyman\open\windows\src\global\delphi\general\KeyNames.pas',
  KeymanVersion in 'C:\Projects\keyman\open\windows\src\global\delphi\general\KeymanVersion.pas',
  StockFileNames in 'C:\Projects\keyman\open\windows\src\global\delphi\cust\StockFileNames.pas',
  Unicode in 'C:\Projects\keyman\open\windows\src\global\delphi\general\Unicode.pas',
  JsonUtil in 'C:\Projects\keyman\open\windows\src\global\delphi\general\JsonUtil.pas';

begin
  try
    Run;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
