(*
  Name:             CharacterInfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Mar 2007

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Mar 2007 - mcdurdin - Initial version (split from UframeTextEditor)
                    30 May 2007 - mcdurdin - I673 - Fixed crash when attempting to display character past end of line
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    25 Mar 2011 - mcdurdin - I2843 - Charmap search does not work for supp plane chars stored as chars
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
                    09 Aug 2015 - mcdurdin - I4842 - Character Map and Character Identifier don't match tokens at start of line
                    23 Feb 2016 - mcdurdin - I4981 - Character Map is being updated even when not visible
                    23 Feb 2016 - mcdurdin - I4927 - If 'find char under cursor automatically' is on, OSK editor opens charmap if it is closed
*)
unit CharacterInfo;

interface

function UpdateCharacterMap(Requested: Boolean; const token: WideString; x, tokenx: Integer; plain: Boolean): Boolean;   // I4807
function GetTokenAtCursor(text: WideString; index: Integer; var token_start: Integer; var prev_token: WideString): WideString;

implementation

uses
  KeymanDeveloperOptions,
  SysUtils,
  UfrmCharacterMapDock,
  UfrmCharacterMapNew,
  UfrmCharacterIdentifier,
  UfrmMain,
  Unicode,
  utilstr;

function UpdateCharacterMap(Requested: Boolean; const token: WideString; x, tokenx: Integer; plain: Boolean): Boolean;   // I4807
var
  code_num: Integer;
  FFound: Boolean;
begin
  Result := False;

  if not Requested and (not FKeymanDeveloperOptions.CharMapAutoLookup) then   // I4807
    Exit;

  if not frmCharacterMapDock.Visible then   // I4981   // I4927
    Exit;

  if token = '' then
  begin
    frmCharacterIdentifier.Chars := '';   // I4807
    Exit;
  end
  else if plain then   // I4807
  begin
    code_num := Ord(token[1]);
    if Uni_IsSurrogate1(WideChar(code_num)) and (Length(token) > 1) then code_num := Uni_SurrogateToUTF32(token[1], token[2]);
    FFound := frmCharacterMapNew.FindCharacter(code_num);
    //
  end
  else if (Length(token) > 2) and ((token[1] = 'c') or (token[1] = 'C')) and ((token[2] < #33) or (token[2] = #160)) then
  begin
    // Comment
    if (x - tokenx > 2) and (x - tokenx <= Length(token)) then
    begin
      code_num := Ord(token[x-tokenx]);
      if Uni_IsSurrogate1(WideChar(code_num)) and (x-tokenx < Length(token)) then code_num := Uni_SurrogateToUTF32(token[x-tokenx], token[x-tokenx+1])  // I2843
      else if Uni_IsSurrogate2(WideChar(code_num)) and (x-tokenx > 3) then code_num := Uni_SurrogateToUTF32(token[x-tokenx-1], token[x-tokenx]);

      FFound := frmCharacterMapNew.FindCharacter(code_num);
    end
    else
      Exit;
  end
  else if (token[1] = '''') or (token[1] = '"') then
  begin
    // String
    if (x-tokenx > 1) and (x-tokenx < Length(token)) then
    begin
      code_num := Ord(token[x-tokenx]);
      if Uni_IsSurrogate1(WideChar(code_num)) and (x-tokenx < Length(token) - 1) then code_num := Uni_SurrogateToUTF32(token[x-tokenx], token[x-tokenx+1])  // I2843
      else if Uni_IsSurrogate2(WideChar(code_num)) and (x-tokenx > 2) then code_num := Uni_SurrogateToUTF32(token[x-tokenx-1], token[x-tokenx]);
      
      FFound := frmCharacterMapNew.FindCharacter(code_num);
    end
    else
      Exit;
  end
  else if token[1] = '$' then
  begin
    // Character Name
    FFound := frmCharacterMapNew.FindCharacter(token, code_num);
  end
  else
  begin
    // Ident
    code_num := ExtNumToInt(token);
    if code_num > 0
      then FFound := frmCharacterMapNew.FindCharacter(code_num)
      else Exit;
  end;

  if plain then   // I4807
    frmCharacterIdentifier.Chars := token
  else if code_num > 0 then   // I4807
    frmCharacterIdentifier.Chars := Uni_UTF32CharToUTF16(code_num);

  if not FFound then
  begin
    if code_num = 0
      then frmKeymanDeveloper.barStatus.Panels[2].Text := token + ' not found in character database or current filter'
      else frmKeymanDeveloper.barStatus.Panels[2].Text := FormatUnicode(Uni_UTF32CharToUTF16(code_num)) + ' not found in character database or current filter';
    Exit;
  end;

  frmKeymanDeveloper.barStatus.Panels[2].Text := FormatUnicode(Uni_UTF32CharToUTF16(code_num));
  frmCharacterMapDock.Visible := True;


  Result := True;
end;


function GetTokenAtCursor(text: WideString; index: Integer; var token_start: Integer; var prev_token: WideString): WideString;
var
  ln, n: Integer;
  startn: Integer;
    function is_whitespace(ch: WideChar): Boolean;
    begin
      Result := (ch < #33) or (ch = #160);
    end;

    function quoted_text(quote: WideChar): WideString;
    begin
      while (n <= ln) and (text[n] <> quote) do Inc(n);
      Result := Copy(text, startn, (n-startn)+1);
      if n > ln then Result := Result + quote
      else Inc(n);
    end;

    function is_control(ch: WideChar): Boolean;
    begin
      Result := CharInSet(ch, ['(', '[', ')', ']', '>']);  // I3310
    end;

    function token: WideString;
    var
      quote: WideChar;
    begin
      Result := '';

      // Skip whitespace

      while (n <= ln) and is_whitespace(text[n]) do Inc(n);
      if n > ln then Exit;

      startn := n;

      if ((text[n] = 'C') or (text[n] = 'c')) and (n < ln) and is_whitespace(text[n+1]) then
      begin
        // Special case for comment
        Result := Copy(text, n, Length(text));
        n := Length(text);
        Exit;
      end;

      case text[n] of
        '''', '"':
          begin
            // quoted string
            quote := text[n]; Inc(n);
            Result := quoted_text(quote);
          end;

        '[': Result := quoted_text(']'); // virtual key
        '(': Result := quoted_text(')'); // parameters for a function
        ']', ')', '>':  // other characters
          begin
            Result := text[n];
            Inc(n);
          end;
        else
          begin
            // a space or control delimited token
            while (n <= ln) and not is_whitespace(text[n]) and not is_control(text[n]) do Inc(n);
            Result := Copy(text, startn, n-startn);
          end;
      end;
    end;
var
  s: WideString;
begin
  Result := '';
  n := 1; ln := Length(text);
  s := '';

  while (n <= index) do   // I4842
  begin
    prev_token := s;
    s := token;
    if s = '' then Break;
  end;
  token_start := startn;
  Result := s;
end;

end.
