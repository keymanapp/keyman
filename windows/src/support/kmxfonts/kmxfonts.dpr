program kmxfonts;

uses
  findfonts in '..\..\global\delphi\general\findfonts.pas',
  kmxfontsmain in 'kmxfontsmain.pas',
  utilcheckfonts in '..\..\global\delphi\general\utilcheckfonts.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  kmxfileusedchars in '..\..\global\delphi\general\kmxfileusedchars.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  kmxfileutils in '..\..\global\delphi\general\kmxfileutils.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  utilkeyboard in '..\..\global\delphi\general\utilkeyboard.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas';

{$R *.res}

begin
  Run;
end.
