program cvt;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  cv in 'cv.pas',
  Keyman.System.Standards.ISO6393ToBCP47Registry in 'C:\Projects\keyman\open\common\windows\delphi\standards\Keyman.System.Standards.ISO6393ToBCP47Registry.pas',
  Keyman.System.KMXFileLanguages in 'C:\Projects\keyman\open\windows\src\global\delphi\keyboards\Keyman.System.KMXFileLanguages.pas',
  utilstr in 'C:\Projects\keyman\open\common\windows\delphi\general\utilstr.pas',
  Keyman.System.Standards.LCIDToBCP47Registry in 'C:\Projects\keyman\open\common\windows\delphi\standards\Keyman.System.Standards.LCIDToBCP47Registry.pas',
  utilfiletypes in 'C:\Projects\keyman\open\common\windows\delphi\general\utilfiletypes.pas',
  utildir in 'C:\Projects\keyman\open\common\windows\delphi\general\utildir.pas',
  kmxfile in 'C:\Projects\keyman\open\common\windows\delphi\keyboards\kmxfile.pas',
  CRC32 in 'C:\Projects\keyman\open\common\windows\delphi\general\CRC32.pas',
  KeyNames in 'C:\Projects\keyman\open\common\windows\delphi\general\KeyNames.pas',
  KeymanVersion in 'C:\Projects\keyman\open\common\windows\delphi\general\KeymanVersion.pas',
  StockFileNames in 'C:\Projects\keyman\open\windows\src\..\..\common\windows\delphi\general\StockFileNames.pas',
  Unicode in 'C:\Projects\keyman\open\common\windows\delphi\general\Unicode.pas',
  JsonUtil in 'C:\Projects\keyman\open\common\windows\delphi\general\JsonUtil.pas';

begin
  try
    Run;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
