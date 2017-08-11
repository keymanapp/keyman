program ACL;

{$APPTYPE CONSOLE}

uses
  dialogs,
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclToken,
  JwsclResource,
  JwsclDescriptor,
  JwsclACL,
  JwsclSecureObjects,
  JwsclKnownSid;

var DACL : TJwDAccessControlList;
begin
  JwInitWellKnownSIDs;

  DACL := TJwDAccessControlList.Create;
  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], 0, JwNullSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], 1, JwNullSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], 2, JwNullSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[afInheritedAce], 3, JwNullSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[afInheritedAce], 3, JwNullSID, false));

  Writeln(DACL.Text);
  readln;
  DACL.Free;
end.
