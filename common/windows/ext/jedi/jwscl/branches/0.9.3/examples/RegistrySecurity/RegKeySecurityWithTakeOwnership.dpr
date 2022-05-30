program RegKeySecurityWithTakeOwnership;

{.$APPTYPE CONSOLE}

uses
  SysUtils,
  Registry,
  JwaWindows,
  JwsclToken,
  JwsclSecureObjects,
  JwsclPrivileges,
  JwsclAcl,
  JwsclDescriptor,
  JwsclTypes,
  JwsclConstants,
  JwsclKnownSid,
  JwsclUtils,
  JwsclStrings;

procedure SetRegKeySecurity(KeyRoot : HKEY; KeyName : String);
var
  Privs : IJwPrivilegeScope;
  Key : HKEY;
  KeySec : TJwSecureRegistryKey;
  DACL : TJwDAccessControlList;
begin
  JwInitWellKnownSIDs; //inits JwSecurityProcessUserSID

  if RegOpenKeyEx(KeyRoot, PChar(KeyName), 0, KEY_ALL_ACCESS, Key) = ERROR_ACCESS_DENIED then
  begin
    //not necessary since KeySec.TakeOwnerShip(); does it on its own
    //But just show the power of interfaces
    //The privilege will be restored to inactive state when the procedure exists
    Privs := JwGetPrivilegeScope([SE_TAKE_OWNERSHIP_NAME], pst_Enable);

    //First open key for write owner
    if RegOpenKeyEx(KeyRoot, PChar(KeyName), 0, WRITE_OWNER, Key) <> 0 then
      RaiseLastOSError;

    try
      //take ownership - can fail with exception
      TJwSecureRegistryKey.TakeOwnerShip(Key);

      //we need to reopen the handle for further access
      if RegOpenKeyEx(KeyRoot, PChar(KeyName), 0, WRITE_DAC, Key) <> 0 then
        RaiseLastOSError;

      //because access is granted on handle creation we need to
      //recreate the object

      KeySec := TJwSecureRegistryKey.Create(Key);
      try
        DACL := KeySec.DACL; //returns a cached DACL so we must not free it!

        //add process user with full access
        //and also set inheritance (values don't have their own security.)
        DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [afContainerInheritAce], KEY_ALL_ACCESS, JwSecurityProcessUserSID));
        //set DACL - may fail with exception
        KeySec.SetDACL(DACL);
      finally
        KeySec.Free;
      end;
    finally
      RegCloseKey(Key)
    end;
  end;
end;

begin
  SetRegKeySecurity(HKEY_CURRENT_USER, 'test');
end.
