program ACL2;

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
  JwsclUtils,
  JwsclSecureObjects,
  JwsclKnownSid;

var DACL : TJwDAccessControlList;
begin
  JwInitWellKnownSIDs;

  DACL := TJwDAccessControlList.Create;
 { DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[afInheritedAce], FILE_READ_DATA, JwGuestsSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_EXECUTE, JwGuestsSID, false));
 }
  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_READ_EA, JwWorldSID, false));

  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_ALL_ACCESS, JwWorldSID, false));


 { DACL.Insert(1,TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_WRITE_ACCESS, JwWorldSID, false));
  }


  Writeln(JwFormatAccessRights(DACL.GetEffectiveRights(JwWorldSID), FileMapping));


  readln;
  DACL.Free;
end.
