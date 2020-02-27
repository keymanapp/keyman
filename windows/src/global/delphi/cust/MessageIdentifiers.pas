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
  keymanapi_TLB,
  MessageIdentifierConsts;

function MsgFromId(const msgid: TMessageIdentifier): WideString;
function MsgFromStr(const str: WideString): WideString;
function MsgFromIdFormat(const msgid: TMessageIdentifier; const args: array of const): WideString;

implementation

uses SysUtils, kmint, custinterfaces;


function MsgFromStr(const str: WideString): WideString;
begin
  if kmcom = nil
    then Result := str
    else Result := Trim(kmint.KeymanCustomisation.CustMessages.MessageFromID(str));
end;

function MsgFromId(const msgid: TMessageIdentifier): WideString;
begin
  if kmcom = nil
    then Result := IntToStr(Ord(msgid))
    else Result := Trim(kmint.KeymanCustomisation.CustMessages.MessageFromID(StringFromMsgId(msgid)));
end;

function MsgFromIdFormat(const msgid: TMessageIdentifier; const args: array of const): WideString;
begin
  try
    Result := WideFormat(MsgFromId(msgid), args);
  except
    Result := MsgFromId(msgid) + ' (error displaying message parameters)';
  end;
end;

end.
