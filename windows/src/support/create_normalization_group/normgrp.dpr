program normgrp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  normmain in 'normmain.pas',
  kmxfile in '..\..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  kmxfileconsts in '..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  kmxfileusedchars in '..\..\global\delphi\general\kmxfileusedchars.pas',
  kmxfileutils in '..\..\..\..\common\windows\delphi\keyboards\kmxfileutils.pas',
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  utilkeyboard in '..\..\..\..\common\windows\delphi\keyboards\utilkeyboard.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas';

begin
  Run;
end.
