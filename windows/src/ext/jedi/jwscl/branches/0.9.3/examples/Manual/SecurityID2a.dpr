program SecurityID2a;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  JwaVista,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclResource,
  JwsclDescriptor,
  JwsclKnownSid;


var SID, SID2 : TJwSecurityID;
    Ident,Id2 : TSidIdentifierAuthority;
    V : Int64;
    O : TWellKnownSidType;
begin
  JwInitWellKnownSIDs();

  SID := TJwSecurityId.CreateWellKnownSid(WinLocalServiceSid);
  SID := TJwSecurityId.Create('S-1-5-32-554');
  writeln(SID.GetText);

end.
