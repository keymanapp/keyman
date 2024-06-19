program kmxfonts;

uses
  findfonts in '..\..\..\..\common\windows\delphi\general\findfonts.pas',
  kmxfontsmain in 'kmxfontsmain.pas',
  utilcheckfonts in '..\..\global\delphi\general\utilcheckfonts.pas',
  kmxfile in '..\..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  kmxfileusedchars in '..\..\global\delphi\general\kmxfileusedchars.pas',
  kmxfileconsts in '..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  kmxfileutils in '..\..\..\..\common\windows\delphi\keyboards\kmxfileutils.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  utilkeyboard in '..\..\..\..\common\windows\delphi\keyboards\utilkeyboard.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  StockFileNames in '..\..\..\..\common\windows\delphi\general\StockFileNames.pas';

{$R *.res}

begin
  Run;
end.
