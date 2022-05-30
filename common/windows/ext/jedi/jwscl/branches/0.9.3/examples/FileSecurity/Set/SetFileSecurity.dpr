{This example adds the user (full control) and
Everyone group (read control) to the ACL of a file or folder.
If the user does not have access it tries to become the owner first.
}

program SetFileSecurity;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclSid,
  JwsclToken,
  JwsclACl,
  JwsclTypes,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclKnownSid;

var
  UserToken : TJwSecurityToken;
  SD : TJwSecurityDescriptor;
  FileObject : TJwSecureFileObject;
  Owner : TJwSecurityId;
  DACL : TJwDAccessControlList;
begin
  if not FileExists(ParamStr(1)) then
    exit;

  JwInitWellKnownSIDs;

  UserToken := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  Owner := UserToken.GetTokenOwner;
  try
    FileObject := TJwSecureFileObject.Create(ParamStr(1));
    try

      //Make me owner if we cant access DACL
      if not FileObject.AccessCheck(WRITE_DAC)// and
         {For example purpose only.
          We don't have to check the owner because Accesscheck does it.
         }
        { not FileObject.Owner.EqualSid(Owner)} then
      begin
        //try to become owner
        JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME, pst_Enable);
        FileObject.Owner := Owner;
      end;

      DACL := FileObject.DACL;
      DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, Owner, false));
      DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_READ, JwWorldSID, false));

      FileObject.SetDACL(DACL);
    finally
      FileObject.Free;
    end;

  finally
    Owner.Free;
    UserToken.Free;
  end;
end.
