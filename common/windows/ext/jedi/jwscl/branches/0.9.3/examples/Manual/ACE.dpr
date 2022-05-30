program ACE;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclResource,
  JwsclDescriptor,
  JwsclACL,
  JwsclKnownSid;

var ACE1 : TJwDiscretionaryAccessControlEntryAllow;
begin
  ACE1 := TJwDiscretionaryAccessControlEntryAllow.Create(
   nil,[],GENERIC_WRITE,JwAdministratorsSID, false);
end.
