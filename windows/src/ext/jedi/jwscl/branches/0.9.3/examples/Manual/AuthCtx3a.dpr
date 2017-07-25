program AuthCtx3a;

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

    Sids : TJwSecurityIdList;
begin
  JwInitWellKnownSIDs;

  RMCtx := TJwAuthResourceManager.Create('',
    [authRM_NoAudit],nil,nil,nil);

  AuthZCtx2 := TJwAuthContext.CreateBySid(
      RMCtx,//const ResourceManager: TJwAuthResourceManager;
      [authZSF_Default],//const Flags : TAuthZSidContextFlags;
      JwAdministratorsSID,//
      0,//const ExpirationTime: Int64;
      nil//const DynamicGroupArgs: Pointer
      );

  Sids := TJwSecurityIdList.Create(false);
  Sids.Add(JwAdministratorsSID);
  Sids.Add(JwWorldSID);

  AuthZCtx := TJwAuthContext.CreateAndAddSids(
    AuthZCtx2, //const AuthContext : TJwAuthContext;
    nil,//nil,//const Sids : TJwSecurityIdList;
    nil//Sids//const RestrictedSids : TJwSecurityIdList
   );

  SD := TJwSecurityDescriptor.Create; //CreateDefaultByToken();
  SD.Owner := JwNullSID;
  SD.PrimaryGroup := JwNullSID;

  SD.DACL.Clear;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_READ_EA, JwAdministratorsSID, false));
  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_READ_DATA, JwAdministratorsSID, false));
  }
  SetLength(SDArray,2);

    SDArray[0] := TJwSecurityDescriptor.CreateDefaultByToken();
    SDArray[0].DACL.Clear;
    SDArray[0].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_READ_ATTRIBUTES, JwAdministratorsSID, false));

    SDArray[1] := TJwSecurityDescriptor.CreateDefaultByToken();
    SDArray[1].DACL.Clear;
    SDArray[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_WRITE_DATA, JwAdministratorsSID, false));



  SetLength(ObjectTypeArray, 1);
  ZeroMemory(@ObjectTypeArray[0], sizeof(ObjectTypeArray[0]));
  ObjectTypeArray[0].Level := ACCESS_PROPERTY_GUID;
  ObjectTypeArray[0].ObjectType := @GUID_1;

  {ZeroMemory(@ObjectTypeArray[1], sizeof(ObjectTypeArray[1]));
  ObjectTypeArray[1].Level := ACCESS_PROPERTY_GUID;
  ObjectTypeArray[1].ObjectType := @GUID_2;     }
  

  Request := TJwAuthZAccessRequest.Create(
    MAXIMUM_ALLOWED,//FILE_READ_EA,//DesiredAccess: TJwAccessMask;
    JwNullSID, //PrincipalSelfSid: TJwSecurityID;
    ObjectTypeArray,//ObjectTypeArray,//ObjectTypeArray: TJwObjectTypeArray;
    nil,//Data: Pointer
    shOwned
    );

  AuthZCtx.AccessCheck(
    1,//Flags: Cardinal;
    Request,//const Request: TJwAuthZAccessRequest;
    0,//const AuditInfo: TAuthZAuditInfoHandle;
    SD,//const SecurityDescriptor: TJwSecurityDescriptor;
    SDArray,//const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
    Reply,//out Reply: TJwAuthZAccessReply;
    AuthZHandle//out AuthZHandle: TAuthZAccessCheckResultHandle
  );

  writeln(JwFormatAccessRights(Reply.GrantedAccessMask[0], FileMapping));

  Reply.Free;

  //readln;

  AuthZCtx.Free;
  RMCtx.Free;

end.
