unit Keyman.System.LocaleStrings;

interface

uses
  keymanapi_TLB,
  MessageIdentifierConsts;

type
  TLocaleStrings = class
    class function MsgFromId(kmcom: IKeyman; const msgid: TMessageIdentifier): string;
    class function MsgFromStr(kmcom: IKeyman; const str: string): string;
    class function MsgFromIdFormat(kmcom: IKeyman; const msgid: TMessageIdentifier; const args: array of const): string;
  end;

implementation

uses
  System.SysUtils,

  custinterfaces;

class function TLocaleStrings.MsgFromStr(kmcom: IKeyman; const str: string): string;
begin
  if kmcom = nil
    then Result := str
    else Result := Trim((kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation.CustMessages.MessageFromID(str));
end;

class function TLocaleStrings.MsgFromId(kmcom: IKeyman; const msgid: TMessageIdentifier): string;
begin
  if kmcom = nil
    then Result := IntToStr(Ord(msgid))
    else Result := Trim((kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation.CustMessages.MessageFromID(StringFromMsgId(msgid)));
end;

class function TLocaleStrings.MsgFromIdFormat(kmcom: IKeyman; const msgid: TMessageIdentifier; const args: array of const): string;
begin
  try
    Result := Format(MsgFromId(kmcom, msgid), args);
  except
    Result := MsgFromId(kmcom, msgid) + ' (error displaying message parameters)';
  end;
end;

end.
