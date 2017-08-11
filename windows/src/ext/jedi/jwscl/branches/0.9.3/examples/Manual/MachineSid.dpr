program MachineSid;

{$APPTYPE CONSOLE}

uses
  sysutils,
  JwaWindows,
  JwsclSid,
  JwsclKnownSid;

var Sid : TJwSecurityId;
begin
  try
    Sid := JwGetMachineSid;
    Writeln('Your local machine SID is: ',Sid.StringSID);
    readln;
  except
    on E : Exception do
      Writeln(E.Message);
  end; 
end.
