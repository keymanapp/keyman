program SetKeySecurity;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Registry,
  Dialogs,
  JwaWindows,
  JwsclTypes,
  JwsclExceptions,
  JwsclSid,
  JwsclAcl,
  JwsclKnownSid,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!


var Reg : TRegistry;
    SecReg : TJwSecureRegistryKey;
    DACL : TJwDAccessControlList;
begin
  //create test key
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.CreateKey('JWSCLTest');
  finally
    Reg.Free;
  end;

  //inits JwUsersSID
  JwInitWellKnownSIDs;
  try
    SecReg := TJwSecureRegistryKey.Create('CURRENT_USER\JWSCLTest');
    try
      DACL := SecReg.DACL; //get a cached DACL

      //Add full control to group "Users" to this key and all its children
      DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,
          [afObjectInheritAce,afContainerInheritAce],GENERIC_ALL, JwUsersSID, false));

      //Set DACL
      SecReg.SetDACL(DACL);
    finally
      SecReg.Free;
    end;
  except
    On e : Exception do
      ShowMessage(E.Message);
  end;
end.
