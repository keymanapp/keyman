program AuthCtx4;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
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

    HDR : ACE_HEADER;
begin
  try
  writeln('JwaWindows is lodad in dynamic mode?: ', JWA_CONST_DYNAMICLINK);
  JwInitWellKnownSIDs;



   RMCtx := TJwAuthResourceManager.Create('',
    [authRM_NoAudit],nil,nil);

  AuthZCtx := TJwAuthContext.CreateBySid(
      RMCtx,//const ResourceManager: TJwAuthResourceManager;
      [authZSF_Default],//const Flags : TAuthZSidContextFlags;
      JwAdministratorsSID, //JwSecurityProcessUserSID,
      0,//const ExpirationTime: Int64;
      nil//const DynamicGroupArgs: Pointer
      );

  
  SD := TJwSecurityDescriptor.Create; //CreateDefaultByToken();
  SD.Owner := JwNullSID;
  SD.PrimaryGroup := JwNullSID;

  SD.DACL.Clear;
  SD.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
   // TJwDiscretionaryAccessControlEntryCallbackAllow.Create(
                  nil,[], {FILE_READ_EA} GENERIC_ALL, JwAdministratorsSID, false));
  SD.DACL.Add(
    TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_READ_EA, JwAdministratorsSID, false));
  SD.DACL.Add(
    TJwDiscretionaryAccessControlEntryCallbackDeny.Create(
                  nil,[], FILE_READ_DATA, JwAdministratorsSID, false));

  {SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(
                  nil,[], FILE_READ_DATA, JwAdministratorsSID, false));
  }
  //ShowMessage(SD.DACL.Text);


  Request := TJwAuthZAccessRequest.Create(
    MAXIMUM_ALLOWED,//FILE_READ_EA,//DesiredAccess: TJwAccessMask;
    JwNullSID, //PrincipalSelfSid: TJwSecurityID;
    nil,//ObjectTypeArray,//ObjectTypeArray,//ObjectTypeArray: TJwObjectTypeArray;
    nil,//Data: Pointer
    shOwned
    );

  Reply := nil;
  try
    AuthZCtx.AccessCheck(
      1,//Flags: Cardinal;
      Request,//const Request: TJwAuthZAccessRequest;
      0,//const AuditInfo: TAuthZAuditInfoHandle;
      SD,//const SecurityDescriptor: TJwSecurityDescriptor;
      nil,//SDArray,//const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
      Reply,//out Reply: TJwAuthZAccessReply;
      AuthZHandle//out AuthZHandle: TAuthZAccessCheckResultHandle
    );
  finally
    Request.Free;
  end;

  writeln(JwFormatAccessRights(Reply.GrantedAccessMask[0], FileMapping));

  Reply.Free;

  AuthZCtx.Free;
  RMCtx.Free;

  JwFreeSecurityDescriptorArray(SDArray);
  SD.Free;
  ObjectTypeArray := nil;
  readln;


  except
    on E : Exception  do
    begin
      writeln('0');
      writeln(e.MEssage);
      readln;
      exit;
    end;
  end;
end.
