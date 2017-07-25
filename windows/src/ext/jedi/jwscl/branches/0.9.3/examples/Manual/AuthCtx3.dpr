program AuthCtx3;

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
  JwsclEnumerations,
  JwsclSecureObjects,
  JwsclKnownSid,
  JwsclAuthCtx;


const
  GUID_1: TGUID = (D1: 1; D2: 2; D3: 1; D4: (1, 2, 3, 4, 5, 6, 7, 8));
  GUID_2: TGUID = (D1: 1; D2: 2; D3: 1; D4: (1, 34, 3, 4, 5, 6, 7, 8));

var
    RMCtx : TJwAuthResourceManager;
    AuthZCtx,
    AuthZCtx2 : TJwAuthContext;

    Reply: TJwAuthZAccessReply;
    AuthZHandle: TAuthZAccessCheckResultHandle;
    Request: TJwAuthZAccessRequest;
    SD : TJwSecurityDescriptor;
    SDArray : TJwSecurityDescriptorArray;
    ObjectTypeArray: TJwObjectTypeArray;
    i : Integer;


    DSids,ASids : TJwSecurityIdList;

    Token  :TJwSecurityToken;
    MySid : TJwSecurityId;
begin
  JwInitWellKnownSIDs;

  MySid := TJwSecurityId.Create('S-1-5-123-3545-342-123');

  RMCtx := TJwAuthResourceManager.Create('',
    [authRM_NoAudit],nil,nil);

  AuthZCtx2 := TJwAuthContext.CreateBySid(
      RMCtx,//const ResourceManager: TJwAuthResourceManager;
      [authZSF_Default],//const Flags : TAuthZSidContextFlags;
      JwGuestsSID,//
      0,//const ExpirationTime: Int64;
      nil//const DynamicGroupArgs: Pointer
      );

  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);

  DSids := TJwSecurityIdList.Create(false);
  ASids := TJwSecurityIdList.Create(false);
  //Sids.Add(JwAdministratorsSID);
 { Sids.Add(JwWorldSID);
  Sids[0].AttributesType := [sidaGroupEnabled];    }
  //Sids[Sids.Add(JwAdministratorsSID)].AttributesType := [sidaGroupEnabled];
  //Sids[Sids.Add(JwGuestsSID)].AttributesType := [sidaGroupEnabled];

  DSids[DSids.Add(MySid)].AttributesType := [sidaGroupUseForDenyOnly];
  //DSids[DSids.Add(MySid)].AttributesType := [sidaGroupEnabled];

 // ASids[ASids.Add(MySid)].AttributesType := [sidaGroupUseForDenyOnly];
 // ASids[ASids.Add(MySid)].AttributesType := [sidaGroupEnabled];

 //ASids[ASids.Add(JwAdministratorsSID)].AttributesType := [sidaGroupEnabled];

  //Sids := Token.TokenGroups;
  //ShowMessage(Sids.GetText(true));

  AuthZCtx := TJwAuthContext.CreateAndAddSids(
    AuthZCtx2, //const AuthContext : TJwAuthContext;
    nil,//ASids,//Sids,//const Sids : TJwSecurityIdList;
    nil//DSids//const RestrictedSids : TJwSecurityIdList
   );

  SD := TJwSecurityDescriptor.Create; //CreateDefaultByToken();
  SD.Owner := JwNullSID;
  SD.PrimaryGroup := JwNullSID;


  SD.DACL.Clear;
  SD.DACL.Revision := 4;
{  SD.DACL.Add(TJwDiscretionaryAccessControlEntryCallbackObjectAllow.Create(
                  nil,[], FILE_ALL_ACCESS, JwAdministratorsSID, false));  }

  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_READ_DATA, JwAdministratorsSID, false));
  }

  i := SD.DACL.Add(TJwDiscretionaryAccessControlEntryCallbackObjectAllow.Create(
                  nil,[], FILE_ALL_ACCESS, JwGuestsSID, false));
  SD.DACL[i].ObjectType := GUID_1;
  SD.DACL[i].Revision := 4;
 { SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_ALL_ACCESS, MySid, false));      }
  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryCallbackDeny.Create(
                  nil,[], FILE_READ_EA, MySid, false));   }

  SDArray := nil;
 { SetLength(SDArray,2);

    SDArray[0] := TJwSecurityDescriptor.CreateDefaultByToken();
    SDArray[0].DACL.Clear;
    SDArray[0].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_READ_ATTRIBUTES, JwAdministratorsSID, false));

    SDArray[1] := TJwSecurityDescriptor.CreateDefaultByToken();
    SDArray[1].DACL.Clear;
    SDArray[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_WRITE_DATA, JwAdministratorsSID, false));

  }



  SetLength(ObjectTypeArray, 4);
  ZeroMemory(@ObjectTypeArray[0], sizeof(ObjectTypeArray[0]));
  ObjectTypeArray[0].Level := ACCESS_OBJECT_GUID;
  ObjectTypeArray[0].ObjectType := @NULL_GUID;

  ZeroMemory(@ObjectTypeArray[1], sizeof(ObjectTypeArray[1]));
  ObjectTypeArray[1].Level := ACCESS_PROPERTY_SET_GUID;
  ObjectTypeArray[1].ObjectType := @GUID_2;

  ZeroMemory(@ObjectTypeArray[2], sizeof(ObjectTypeArray[2]));
  ObjectTypeArray[2].Level := ACCESS_PROPERTY_GUID;
  ObjectTypeArray[2].ObjectType := @GUID_1;

  ZeroMemory(@ObjectTypeArray[3], sizeof(ObjectTypeArray[3]));
  ObjectTypeArray[3].Level := ACCESS_PROPERTY_GUID;
  ObjectTypeArray[3].ObjectType := @GUID_1;
  

  Request := TJwAuthZAccessRequest.Create(
    MAXIMUM_ALLOWED,//FILE_READ_EA,//DesiredAccess: TJwAccessMask;
    JwNullSID, //PrincipalSelfSid: TJwSecurityID;
    ObjectTypeArray,//ObjectTypeArray: TJwObjectTypeArray;
    nil,//Data: Pointer
    shOwned
    );

  AuthZCtx.AccessCheck(
    0,//Flags: Cardinal;
    Request,//const Request: TJwAuthZAccessRequest;
    0,//const AuditInfo: TAuthZAuditInfoHandle;
    SD,//const SecurityDescriptor: TJwSecurityDescriptor;
    SDArray,//const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
    nil,
    Reply,//out Reply: TJwAuthZAccessReply;
    AuthZHandle//out AuthZHandle: TAuthZAccessCheckResultHandle
  );

  //writeln(JwFormatAccessRights(Reply.GrantedAccessMask[0], FileMapping));
   writeln(JwFormatAccessRights(Reply.GrantedAccessMask, Reply.Error, FileFolderMapping));


  Reply.Free;

  //readln;

  AuthZCtx.Free;
  RMCtx.Free;

end.
