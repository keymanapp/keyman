(*
  Name:             testfindfonts_main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jun 2008

  Modified Date:    16 Jun 2008
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jun 2008 - mcdurdin - I1353 - Initial version
*)
unit testfindfonts_main;

interface

procedure Run;

implementation

uses
  SysUtils,
  findfonts,
  TntClasses;

procedure Run;
var
  s: WideString;
  i: Integer;
  f: TFindFontList;
begin
  ExitCode := 3;

  if ParamCount < 1 then
  begin
    writeln('Usage: '+ExtractFilePath(ParamStr(0))+' <characters.txt-utf16>');
    ExitCode := 1;
    Exit;
  end;

  if not FileExists(ParamStr(1)) then
  begin
    writeln('File not found');
    ExitCode := 2;
    Exit;
  end;

  with TTntStringList.Create do
  try
    LoadFromFile(ParamStr(1));  {$MESSAGE HINT 'Check LoadFromFile encoding'}
    s := Text;
    if s = '' then Exit;
    if Ord(s[1]) = $FEFF then System.Delete(s,1,1);
  finally
    Free;
  end;

  for i := Length(s) downto 1 do
    if s[i] < #33 then Delete(s,i,1);

  f := TFindFontList.Create;
  try
    FindFontsForChars(s, 50, f);
    for I := 0 to f.Count - 1 do
    begin
      writeln(f[i].FontName + ': '+ IntToStr(f[i].Coverage)+'%');
    end;
  finally
    f.Free;
  end;

  ExitCode := 0;
end;

end.
