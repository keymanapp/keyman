unit MessageDefaults;

interface

function MsgDefault(msgid: WideString): WideString;

implementation

function MsgDefault(msgid: WideString): WideString;
begin
  Result := '';
end;

end.
