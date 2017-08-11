(*
  Name:             OnlineUpdateCheckMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      15 Jan 2007

  Modified Date:    15 Jan 2007
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          15 Jan 2007 - mcdurdin - Initial version
*)
unit OnlineUpdateCheckMessages;

interface

function S_OnlineUpdate_UnableToContact: WideString;
function S_OnlineUpdate_UnableToContact_Error: WideString;
function S_OnlineUpdate_IconTitle: WideString;
function S_OnlineUpdate_IconText: WideString;
function S_OnlineUpdate_IconMenuText: WideString;
function S_OnlineUpdate_IconMenuExit: WideString;

implementation

uses
  MessageIdentifiers,
  MessageIdentifierConsts,
  kmint;

function S_OnlineUpdate_UnableToContact: WideString;
begin
  Result := MsgFromId(SKUpdate_UnableToContact);
end;

function S_OnlineUpdate_UnableToContact_Error: WideString;
begin
  Result := MsgFromId(SKUpdate_UnableToContact_Error);
end;

function S_OnlineUpdate_IconTitle: WideString;
begin
  Result := MsgFromId(SKUpdate_IconTitle);
end;

function S_OnlineUpdate_IconText: WideString;
begin
  Result := MsgFromId(SKUpdate_IconText);
end;

function S_OnlineUpdate_IconMenuText: WideString;
begin
  Result := MsgFromId(SKUpdate_IconMenuText);
end;

function S_OnlineUpdate_IconMenuExit: WideString;
begin
  Result := MsgFromId(SKUpdate_IconMenuExit);
end;

end.
