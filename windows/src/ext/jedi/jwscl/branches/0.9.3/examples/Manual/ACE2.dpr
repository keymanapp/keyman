program ACE2;

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

var ACE1 : TJwDiscretionaryAccessControlEntryAllow;
    SD : TJwSecurityDescriptor;
    b : Boolean;
    m : Cardinal;
    privs : TJwPrivilegeSet;
    G,A : Cardinal;
begin
  JwInitWellKnownSIDs;
  SD := TJwSecurityDescriptor.Create;

  SD.Owner := JwWorldSID;
  SD.PrimaryGroup := JwNullSID;
  //SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
  //                nil,[],FILE_GENERIC_READ, JwSecurityProcessUserSID, false));

  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], GENERIC_READ, JwSecurityProcessUserSID, false));
  }

  SD.DACL.Revision := ACL_REVISION;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_ALL_ACCESS,JwSecurityProcessUserSID, false));
{  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_GENERIC_EXECUTE,JwSecurityProcessUserSID, false));}
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], GENERIC_ALL {and not (FILE_GENERIC_WRITE or FILE_GENERIC_EXECUTE)},
                  JwSecurityProcessUserSID, false));

 // showmessage(SD.DACL.Text);
  {SD.DACL.Insert(0,TJwDiscretionaryAccessControlEntryAllow.Create(
               nil,[], FILE_WRITE_DATA, JwSecurityProcessUserSID, false));}
 // showmessage(SD.DACL.Text);


  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], 1, JwSecurityProcessUserSID, false));
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], 2, JwGuestsSID, false));
  SD.DACL.Insert(0,TJwDiscretionaryAccessControlEntryAllow.Create(
               nil,[], 0, JwAdministratorsSID, false));
  showmessage(SD.DACL.Text);   }


  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], GENERIC_WRITE,JwSecurityProcessUserSID, false));
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], GENERIC_EXECUTE,JwSecurityProcessUserSID, false));

  SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], GENERIC_READ and not (SYNCHRONIZE or READ_CONTROL), JwSecurityProcessUserSID, false));
  }
  m := TJwSecurityFileMapping.Map(GENERIC_READ);

  b := TJwSecureGeneralObject.AccessCheck(SD,nil, FILE_READ_DATA,
      TJwSecurityFileMapping);
  writeln(b);

  TJwSecureGeneralObject.AccessCheck(SD,nil,MAXIMUM_ALLOWED,TJwSecurityFileFolderMapping, privs,G,B);

  writeln(JwFormatAccessRights(G, FileMapping));
  readln;


end.
