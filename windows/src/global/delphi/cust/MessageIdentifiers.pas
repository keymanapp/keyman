(*
  Name:             MessageIdentifiers
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    7 Feb 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Changed to not be autogen now
                    12 Dec 2006 - mcdurdin - Add support from reading localization from a particular product
                    07 Feb 2007 - mcdurdin - Trim messages when loading them
*)
unit MessageIdentifiers;

interface

uses
  MessageIdentifierConsts;

function MsgFromId(const msgid: TMessageIdentifier): WideString;
function MsgFromStr(const str: WideString): WideString;
function MsgFromIdFormat(const msgid: TMessageIdentifier; const args: array of const): WideString;

implementation

uses
  kmint,
  Keyman.System.LocaleStrings;

function MsgFromStr(const str: WideString): WideString;
begin
  Result := TLocaleStrings.MsgFromStr(kmcom, str);
end;

function MsgFromId(const msgid: TMessageIdentifier): WideString;
begin
  Result := TLocaleStrings.MsgFromId(kmcom, msgid);
end;

function MsgFromIdFormat(const msgid: TMessageIdentifier; const args: array of const): WideString;
begin
  Result := TLocaleStrings.MsgFromIdFormat(kmcom, msgid, args);
end;

end.
