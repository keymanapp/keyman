(*
  Name:             kmxfontsmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jan 2009

  Modified Date:    20 Jan 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jan 2009 - mcdurdin - Initial version
                    20 Jan 2009 - mcdurdin - Display a single line showing no fonts found if that is the case
*)
unit kmxfontsmain;

interface

uses
  utilcheckfonts, findfonts;

type
  TCheckFonts = class
  private
    FCSVFormat: Boolean;
    FCheckFontsThread: TCheckFontsThread;
    procedure StartCheckingFonts(KeyboardFileName: WideString; CSVFormat: Boolean);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure DisplayKeyboardFonts;
  end;

procedure Run;

implementation

uses
  kmxfile,
  kmxfileusedchars,
  SysUtils;
  
procedure TCheckFonts.DisplayKeyboardFonts;
var
  FKeyboard: TCheckFontKeyboard;
  Found: Boolean;
  i: Integer;
begin
  FKeyboard := FCheckFontsThread.Keyboards[0];
  Found := False;
  if FCSVFormat then
  begin
    for I := 0 to FKeyboard.Fonts.Count - 1 do
      if FKeyboard.Fonts[I].Coverage >= 50 then
      begin
        writeln('"'+ExtractFileName(ParamStr(2))+'","'+FKeyboard.Fonts[I].FontName+'",'+IntToStr(FKeyboard.Fonts[I].Coverage));
        Found := True;
      end;

    if not Found then
      writeln('"'+ExtractFileName(ParamStr(2))+'","*",0');
  end
  else
  begin
    writeln('Keyboard '+ParamStr(1));
    for I := 0 to FKeyboard.Fonts.Count - 1 do
      if FKeyboard.Fonts[I].Coverage >= 50 then
      begin
        writeln('  Font '+FKeyboard.Fonts[I].FontName+' ('+IntToStr(FKeyboard.Fonts[I].Coverage)+'% coverage)');
        Found := True;
      end;

    if not Found then
      writeln('  No fonts found');
  end;
end;

procedure TCheckFonts.CheckFontsThreadComplete(Sender: TObject);
begin
  DisplayKeyboardFonts;
  FCheckFontsThread := nil;
end;

procedure TCheckFonts.StartCheckingFonts(KeyboardFileName: WideString; CSVFormat: Boolean);
var
  FCharsUsed: WideString;
begin
  FCSVFormat := CSVFormat;
  if not FileExists(KeyboardFileName) then
  begin
    writeln('File '+KeyboardFileName+' does not exist');
    Exit;
  end;

  FCharsUsed := KMXFile_GetUsedChars(KeyboardFileName);
  
  FCheckFontsThread := TCheckFontsThread.Create;
  FCheckFontsThread.FreeOnTerminate := False;
  FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
  FCheckFontsThread.AddKeyboard(KeyboardFilename, KeyboardFilename, FCharsUsed);
  FCheckFontsThread.Start;
  FCheckFontsThread.WaitFor;
  FCheckFontsThread.Free;
end;

procedure Run;
begin
  if (ParamCount = 0) then
  begin
    writeln('kmxfonts: Find fonts on your system that work with the given keyboard');
    writeln('  Usage: kmxfonts [-c] <filename.kmx>');
    writeln('     -c: CSV Format');
    Exit;
  end;

  with TCheckFonts.Create do
  try
    if ParamStr(1) = '-c' then
      StartCheckingFonts(ParamStr(2), True)
    else StartCheckingFonts(ParamStr(1), False);
  finally
    Free;
  end;
end;

end.
