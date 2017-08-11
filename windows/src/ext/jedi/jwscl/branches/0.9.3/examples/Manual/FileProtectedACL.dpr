{This program shows possible access to a file or folder.
Call this program with a parameter.
}
program FileProtectedACL;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  JwaWindows,
  jwsclConstants,
  JwsclMapping,
  JwsclToken,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclExceptions,
  JwsclACL,
  JwsclTypes,
  JwsclKnownSid,
  JwsclStrings;



var F: Textfile;
    Name : String;

    count,
    res : Cardinal;
    FD : TJwSecureFileObject;
    NewDACL,
    NewDACL2 :TJwDAccessControlList;
    Explicits : TJwExplicitAccessArray;
    EA : PEXPLICIT_ACCESS;
    ppACL : PACL;
begin
  JwInitWellKnownSIDs;

  Name := ParamStr(0)+'.test';
  AssignFile(f,Name);
  Rewrite(F);
  CloseFile(F);

  FD := TJwSecureFileObject.Create(Name);
  Writeln(FD.DACL.GetTextMap());

  NewDACL := TJwDAccessControlList.Create;
  NewDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwSecurityProcessUserSID));

  //FD.SetDACL(NewDACL,apProtected);

  Explicits := NewDACL.GetExplicitAccessArray;

  res := SetEntriesInAcl(1, @Explicits[0], nil, ppACL);

  res := GetExplicitEntriesFromAcl(ppACL, count, EA);

  NewDACL2 := TJwDAccessControlList.Create(TJwExplicitAccessArray(EA));
  Writeln(NewDACL2.GetTextMap(TJwSecurityFileFolderMapping));
 { TJwSecureGeneralObject.SetNamedSecurityInfo(TJwPChar(Name), SE_FILE_OBJECT,
        [siProtectedDaclSecurityInformation], nil,
        nil, NewDACL, nil);
                   }

  NewDACL.Free;

  FD.ResetTemp();
  Writeln(FD.DACL.GetTextMap(TJwSecurityFileFolderMapping));
  FD.Free;
  

  DeleteFile(PChar(Name));
  readln;
end.
