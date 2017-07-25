{This example adds the user (full control) and
Everyone group (read control) to the ACL of a file or folder.

In constrast to SetFileSecurity.dpr this exampel "SetFileSecurity2"
uses the class TJwSecureGeneralObject which can handle security data
of any handle using its class methods.
Be aware that there are file/folder and registry security classes available,
so use TJwSecureGeneralObject for all other types of objects.
}

program SetFileSecurity2;

{$APPTYPE CONSOLE}

uses
  ExceptionLog,
  SysUtils,
  JwaWindows,
  JwsclSid,
  JwsclToken,
  JwsclACl,
  JwsclTypes,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclComUtils,
  JwsclKnownSid;

var
  UserToken : TJwSecurityToken;
  SD : TJwSecurityDescriptor;
  Owner : TJwSecurityId;
  DACL : TJwDAccessControlList;
begin
  if not FileExists(ParamStr(1)) then
    exit;

  JwInitWellKnownSIDs;

  UserToken := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);

  Owner := UserToken.GetTokenOwner;
  try
    SD := TJwSecureGeneralObject.GetNamedSecurityInfo(ParamStr(1), SE_FILE_OBJECT, [siDaclSecurityInformation]);

//    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, Owner, false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_READ, JwWorldSID, false));

    TJwSecureGeneralObject.SetNamedSecurityInfo(ParamStr(1), SE_FILE_OBJECT, [siDaclSecurityInformation], SD);
  finally
    Owner.Free;
    UserToken.Free;
  end;
end.
