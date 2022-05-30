program AuthCtx;

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

var //DACL : TJwDAccessControlList;
    RMCtx : TJwAuthResourceManager;
    AuthZCtx : TJwAuthContext;

    Reply: TJwAuthZAccessReply;
    AuthZHandle: TAuthZAccessCheckResultHandle;
    Request: TJwAuthZAccessRequest;
    SD : TJwSecurityDescriptor;
begin
  JwInitWellKnownSIDs;

  RMCtx := TJwAuthResourceManager.Create('',
    [authRM_NoAudit],nil,nil);

  AuthZCtx := TJwAuthContext.CreateBySid(
      RMCtx,//const ResourceManager: TJwAuthResourceManager;
      [authZSF_Default],//const Flags : TAuthZSidContextFlags;
      JwAdministratorsSID,//
      0,//const ExpirationTime: Int64;
      nil//const DynamicGroupArgs: Pointer
      );

  SD := TJwSecurityDescriptor.Create; //CreateDefaultByToken();
  SD.Owner := JwNullSID;
  SD.PrimaryGroup := JwNullSID;

  SD.DACL.Clear;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_ALL_ACCESS, JwAdministratorsSID, false));

  SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_READ_EA, JwAdministratorsSID, false));

  Request := TJwAuthZAccessRequest.Create(
    MAXIMUM_ALLOWED,//FILE_READ_EA,//DesiredAccess: TJwAccessMask;
    JwNullSID, //PrincipalSelfSid: TJwSecurityID;
    nil,//ObjectTypeArray: TJwObjectTypeArray;
    nil,//Data: Pointer
    shOwned
    );

  AuthZCtx.AccessCheck(
    1,//Flags: Cardinal;
    Request,//const Request: TJwAuthZAccessRequest;
    0,//const AuditInfo: TAuthZAuditInfoHandle;
    SD,//const SecurityDescriptor: TJwSecurityDescriptor;
    nil,//const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
    Reply,//out Reply: TJwAuthZAccessReply;
    AuthZHandle//out AuthZHandle: TAuthZAccessCheckResultHandle
  );

  writeln(JwFormatAccessRights(Reply.GrantedAccessMask[0], FileMapping));

  Reply.Free;

  //readln;

  AuthZCtx.Free;
  RMCtx.Free;

end.
