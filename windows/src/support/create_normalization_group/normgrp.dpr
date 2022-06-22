program normgrp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  normmain in 'normmain.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  kmxfileusedchars in '..\..\global\delphi\general\kmxfileusedchars.pas',
  kmxfileutils in '..\..\global\delphi\general\kmxfileutils.pas',
  CRC32 in '..\..\..\..\common\windows\delphi\general\CRC32.pas',
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  utilkeyboard in '..\..\global\delphi\general\utilkeyboard.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas';

begin
  Run;
end.
