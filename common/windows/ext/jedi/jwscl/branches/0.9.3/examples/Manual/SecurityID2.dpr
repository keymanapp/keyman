program SecurityID2;

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
  JwInitWellKnownSIDsEx([]);

  SID := TJwSecurityId.Create('S-1-0-0');
  O := SID.WellKnownSidType;

  SID := TJwSecurityId.CreateWellKnownSid(WinLocalSystemSid);

end.
