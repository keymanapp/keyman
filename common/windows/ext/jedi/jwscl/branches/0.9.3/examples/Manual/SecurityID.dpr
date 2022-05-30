program SecurityID;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclResource,
  JwsclDescriptor;


var SID, SID2 : TJwSecurityID;
    Ident,Id2 : TSidIdentifierAuthority;
    V : Int64;
begin
  SID := TJwSecurityID.Create('S-1-0x400-1-2-3-4-5-6-7-8-9-10');
  writeln(SID.GetText(true));

  Ident := TJwSecurityID.IntToSidAuth($400);
  SID2 := TJwSecurityID.Create([1,2,3,4,5,6,7,8],Ident);
  writeln(SID2.GetText(true));
end.
