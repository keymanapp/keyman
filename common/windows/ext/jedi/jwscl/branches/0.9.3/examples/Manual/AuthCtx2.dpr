program AuthCtx2;

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
      JwSecurityProcessUserSID,
      0,//const ExpirationTime: Int64;
      nil//const DynamicGroupArgs: Pointer
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

  //  ShowMessage(SDArray[0].DACL.Text);
  {  SDArray[2] := TJwSecurityDescriptor.CreateDefaultByToken();
    SDArray[2].DACL.Clear;
    SDArray[2].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
                  nil,[], FILE_ALL_ACCESS, JwAdministratorsSID, false)); }



  SetLength(ObjectTypeArray, 7);
{  ZeroMemory(@ObjectTypeArray[0], sizeof(ObjectTypeArray[0]));
  ObjectTypeArray[0].Level := 0;
  ObjectTypeArray[0].ObjectType := @GUID_1;

  ZeroMemory(@ObjectTypeArray[1], sizeof(ObjectTypeArray[1]));
  ObjectTypeArray[1].Level := 0;
  ObjectTypeArray[1].ObjectType := @GUID_2;

  ZeroMemory(@ObjectTypeArray[2], sizeof(ObjectTypeArray[2]));
  ObjectTypeArray[2].Level := 1;
  ObjectTypeArray[2].ObjectType := @GUID_2;    }
                //0, 1, 2, 2, 1, 2, 3}
  i := 0;
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 0;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[1], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 1;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 2;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 3;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 4;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 1;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Inc(i);
  ZeroMemory(@ObjectTypeArray[i], sizeof(TObjectTypeList));
  ObjectTypeArray[i].Level := 2;
  ObjectTypeArray[i].ObjectType := @GUID_1;

  Request := nil;
  Request := TJwAuthZAccessRequest.Create(
    MAXIMUM_ALLOWED,//FILE_READ_EA,//DesiredAccess: TJwAccessMask;
    JwNullSID, //PrincipalSelfSid: TJwSecurityID;
    ObjectTypeArray,//ObjectTypeArray,//ObjectTypeArray,//ObjectTypeArray: TJwObjectTypeArray;
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
      SDArray,//const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
      nil,
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
