program normgrp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  normmain in 'normmain.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  kmxfileusedchars in '..\..\global\delphi\general\kmxfileusedchars.pas',
  kmxfileutils in '..\..\global\delphi\general\kmxfileutils.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  utilkeyboard in '..\..\global\delphi\general\utilkeyboard.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  kmcomapi_TLB in '..\..\engine\kmcomapi\kmcomapi_TLB.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas';

begin
  Run;
end.
