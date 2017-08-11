(*
  Name:             main_testxmlencode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      17 Sep 2007

  Modified Date:    17 Sep 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          17 Sep 2007 - mcdurdin - Initial version
*)
unit main_testxmlencode;

interface

procedure Run;

implementation

uses
  utilxml;

function XMLEncode_10(const s: WideString): WideString;
var
  I: Integer;
  t: WideString;
begin
  // This is the old version of XMLEncode which was going wrong.
  Result := s;
  for I := 1 to Length(Result) do
  begin
    case Result[I] of
      '<':  t := '&lt;';
      '>':  t := '&gt;';
      '&':  t := '&amp;';
      '''': t := '&apos;';
      '"':  t := '&quot;';
      else Continue;
    end;
    Delete(Result, I, 1);
    Insert(t, Result, I);
  end;
end;

procedure Run;
const
  text: array[0..4] of WideString = (
   'This is a test',
  '<This is a test>',
  '<<This is a test>>',
  'This is &amp; a test',
  'This < is < a < test');
var
  i: Integer;
begin
  for i := 0 to High(text) do
  begin
    writeln(text[i]); // Original text
    writeln(XMLEncode_10(text[i])); // Faulty output
    writeln(XMLEncode(text[i])); // Fixed version
    writeln; writeln;
  end;
end;

end.
