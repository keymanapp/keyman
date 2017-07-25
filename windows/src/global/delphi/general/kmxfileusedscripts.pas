(*
  Name:             kmxfileusedscripts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    12 Mar 2010 - mcdurdin - I2218 - Don't show script name for 1 or 2 characters when checking language settings
*)
unit kmxfileusedscripts;

interface

uses
  UnicodeBlocks;

function KMXFile_GetUsedScripts(const FFileName: WideString): TUnicodeBlockDataArray;

implementation

uses
  kmxfileusedchars,
  kmxfileutils,
  Unicode;

function KMXFile_GetUsedScripts(const FFileName: WideString): TUnicodeBlockDataArray;
var
  FChars: WideString;
  I: Integer;
  p: PWideChar;
  ch: Integer;
  FBlockIndex: Integer;

    procedure AddBlock;
    var
      J: Integer;
    begin
      // add another block to the list of used blocks
      for J  := 0 to High(Result) do
        if Result[J].Ch1 = SUnicodeBlocks[FBlockIndex].Ch1 then
        begin
          Inc(Result[J].CharacterCount);
          Exit;
        end;

      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := SUnicodeBlocks[FBlockIndex];
      Result[High(Result)].CharacterCount := 1;
    end;
    
begin
  SetLength(Result,0);

  FChars := KMXFile_GetUsedChars(FFileName);
  p := PWideChar(FChars);

  while (p <> nil) and (p^ <> #0) do
  begin
    ch := GetSuppChar(p);

    FBlockIndex := -1;

    for I := 0 to High(SUnicodeBlocks) do
      if ch <= SUnicodeBlocks[I].Ch2 then
      begin
        FBlockIndex := I;
        Break;
      end;

    if (FBlockIndex >= 0) and (SUnicodeBlocks[FBlockIndex].Ch1 <= ch) then AddBlock;

    p := incxstr(p);
  end;
end;

end.
