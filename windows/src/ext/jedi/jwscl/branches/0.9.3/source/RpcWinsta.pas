{RpcWinsta is only for testing purposes and may not be used for
productive use!
}
unit RpcWinsta;

interface

uses JwaWindows;

type

  TOffsetString = record
    Size : WORD;
    Offset : WORD;
  end;

  PQueryCredentials = ^TQueryCredentials;
  TQueryCredentials = record
    LogonType : DWORD; // 2
    DWORD2 : DWORD;
    DWORD3 : DWORD; // 1 if login was from the console, not terminal session ?
    UserName : TOffsetString;
    Domain : TOffsetString;
    Password : TOffsetString;
  end;

// Buffer must be nil before calling this function, otherwise you'll get an AV
function RpcWinStationQueryLogonCredentials(IcaApiHandle : THandle; SessionId : DWORD;
  var Buffer : PQueryCredentials; var ErrorCode : DWORD) : Boolean; stdcall;

function RpcWinStationQueryInformation(IcaApiHandle : THandle; var functionResult : DWORD;  SessionId : DWORD;
  WinStationInformationClass: Cardinal; pWinStationInformation: PVOID; WinStationInformationLength: DWORD; var pReturnLength: DWORD
) : Boolean; stdcall;

function RpcWinStationSetInformation(IcaApiHandle : THandle; var functionResult : DWORD;  SessionId : DWORD;
  WinStationInformationClass: Cardinal; pWinStationInformation: PVOID; WinStationInformationLength: DWORD) : Boolean; stdcall;

function RpcWinStationShadow(IcaApiHandle : THandle; var FunctionResult : DWORD; SourceSessionId : DWORD;
  serverName : PWideChar; serverNameBytesLength : DWORD; TargetSessionId : DWORD; hotKey : DWORD; hotKeyModifier : DWORD) : Boolean; stdcall;

function RpcWinStationAutoReconnect(IcaApiHandle : THandle; var FunctionResult : DWORD; SessionId : DWORD;
  Unused : DWORD) : Boolean; stdcall;

function RpcWinStationFUSCanRemoteUserDisconnect(IcaApiHandle : THandle; var functionResult : DWORD;
 targetSessionId : DWORD; sourceSessionId : DWORD; sourceProcessId : DWORD;
 domainName : PWideChar; domainNameLength : DWORD; userName : PWideChar; userNameLength : DWORD): Boolean;
  stdcall;

// function can be also used to open a new handle to the local server, if needed, which needs to be closed. 
function WinStationCloseServer(var ServerHandle : THandle) : DWORD;
function WinStationOpenServer(ServerName : WideString) : THandle;
function WinStationGetLocalServerHandle : THandle; // returns a auto open local handle. Do not close it!!!

function AllocateMemoryStd(Size : DWORD) : Pointer; stdcall;
procedure FreeMemoryStd(P : Pointer); stdcall; // If rpc procedure returned any allocated memory, it must be freed using this function



implementation

uses
  SysUtils;

  var
  LCRPC_IfHandle : THandle; // binding handle;

  IcaApi_IfHandle : THandle;// local server handle, it's automatically opened and closed
                            //in initialization/finalization
  Security: _SEC_WINNT_AUTH_IDENTITY_W;

type

  TAllocateFunction = function(size : DWORD) : pointer; stdcall;
  TFreeFunction = procedure (p : Pointer); stdcall;

  function AllocateMemoryStd(Size : DWORD) : Pointer; stdcall;
  begin
    Result := GetMemory(Size);
  end;

  procedure FreeMemoryStd(P : Pointer); stdcall;
  begin
    FreeMemory(P);
  end;

type
  PMIDL_STUB_DESC = ^_MIDL_STUB_DESC;
  _MIDL_STUB_DESC = record
    RpcInterfaceInformation : Pointer;

    pfnAllocate : TAllocateFunction;
    pfnFree : TFreeFunction;

    IMPLICIT_HANDLE_INFO : record
      case Integer of
      1: (pAutoHandle : PHandle);
      2: (pPrimitiveHandle : PHandle);
      3: (pGenericBindingInfo : Pointer); //PGENERIC_BINDING_INFO
    end;

    apfnNdrRundownRoutines : Pointer; // TNDR_RUNDOWN;
//    const GENERIC_BINDING_ROUTINE_PAIR  *   aGenericBindingRoutinePairs;
    aGenericBindingRoutinePairs : pointer;

//    const EXPR_EVAL  *                      apfnExprEval;
    apfnExprEval : pointer;

//    const XMIT_ROUTINE_QUINTUPLE  *         aXmitQuintuple;
    aXmitQuintuple : pointer;

//    const unsigned char  *                  pFormatTypes;
    pFormatTypes : pointer;

    fCheckBounds : DWORD;

    // Ndr library version.
    Version : Cardinal;

//    MALLOC_FREE_STRUCT  *                   pMallocFreeStruct;
    pMallocFreeStruct : Pointer;

    MIDLVersion : DWORD;

//    const COMM_FAULT_OFFSETS  *    CommFaultOffsets;
    CommFaultOffsets : Pointer;

    // New fields for version 3.0+
//    const USER_MARSHAL_ROUTINE_QUADRUPLE  * aUserMarshalQuadruple;
    aUserMarshalQuadruple : Pointer;

    // Notify routines - added for NT5, MIDL 5.0
//    const NDR_NOTIFY_ROUTINE  *             NotifyRoutineTable;
    NotifyRoutineTable : Pointer;

    (*
     * Reserved for future use.
     *)

    mFlags : DWORD;

    // International support routines - added for 64bit post NT5
//    const NDR_CS_ROUTINES *                 CsRoutineTables;
    CsRoutineTables : Pointer;

    Reserved4 : Pointer;
    Reserved5 : Pointer;

    // Fields up to now present in win2000 release.
end;

type
   RPC_VERSION  = record
     MajorVersion : WORD;
     MinorVersion : WORD;
   end;

   RPC_SYNTAX_IDENTIFIER = record
    SyntaxGUID : GUID;
    SyntaxVersion : RPC_VERSION;
   end;

   PRPC_PROTSEQ_ENDPOINT = ^RPC_PROTSEQ_ENDPOINT;
   RPC_PROTSEQ_ENDPOINT = record
    RpcProtocolSequence : Pointer;// TODO
    Endpoint : Pointer; // TODO
   end;

  RPC_SERVER_INTERFACE = record
    Length : DWORD;
    InterfaceId : RPC_SYNTAX_IDENTIFIER;
    TransferSyntax : RPC_SYNTAX_IDENTIFIER;
    DispatchTable : Pointer; //TODO
    RpcProtseqEndpointCount : DWORD;
    RpcProtseqEndpoint : PRPC_PROTSEQ_ENDPOINT;
    DefaultManagerEpv : Pointer;
    InterpreterInfo : Pointer;
    Flags : DWORD;
  end;

  RPC_CLIENT_INTERFACE = record
    Length : DWORD;
    InterfaceId : RPC_SYNTAX_IDENTIFIER;
    TransferSyntax : RPC_SYNTAX_IDENTIFIER;
    DispatchTable : Pointer; // TODO
    RpcProtseqEndpointCount : DWORD;
    RpcProtseqEndpoint : PRPC_PROTSEQ_ENDPOINT;
    Reserved : Pointer;
    InterpreterInfo : Pointer;
    Flags :DWORD;
  end;

const

 InterfaceConst : RPC_SERVER_INTERFACE = (
    Length : $44;
    InterfaceId :
     (
      SyntaxGUID : (D1 : $5CA4A760; D2 : $EBB1; D3: $11CF; D4 : ($86, $11, $00, $A0, $24, $54, $20, $ED));
      SyntaxVersion: (MajorVersion : $1);
     );
    TransferSyntax :
     (
      SyntaxGUID : (D1: $8A885D04; D2 : $1CEB; D3 : $11C9; D4 : ($9F, $E8, $08, $00, $2B, $10, $48, $60));
      SyntaxVersion : (MajorVersion : $2);
     );


  );

 pFormat : array [0..259] of Cardinal = (
   $0C110000,
   $4115C08,
   $0AC300002,
   $4D300000,
   $8110000,
   $125C08,
   $1B0002,
   $54290001,
   $10,
   $115B02,
   $11B0002,
   $290002,
   $0C,
   $115B05,
   $11B0002,
   $290002,
   $14,
   $125B05,
   $1B0002,
   $290001,
   $14,
   $115B02,
   $11B0002,
   $290002,
   $10,
   $115B05,
   $11B0002,
   $290002,
   $18,
   $115B05,
   $12FFD2,
   $11FFDE,
   $1B0002,
   $290001,
   $10,
   $115B02,
   $1B0002,
   $290001,
   $18,
   $115B02,
   $1B0002,
   $290001,
   $20,
   $115B02,
   $1B0002,
   $290001,
   $28,
   $115B02,
   $1B0002,
   $290001,
   $30,
   $115B02,
   $1B0002,
   $290001,
   $0C,
   $115B02,
   $11B0002,
   $290002,
   $1C,
   $115B05,
   $11B0002,
   $290002,
   $24,
   $115B05,
   $11B0002,
   $290002,
   $2C,
   $115B05,
   $1B0002,
   $290001,
   $38,
   $115B02,
   $11B0002,
   $54290002,
   $20,
   $8115B05,
   $0C115C02,
   $14115C02,
   $120002,
   $7150088,
   $5B0B0008,
   $2011C,
   $3A0017,
   $170001,
   $10038,
   $7165B05,
   $5C4B0088,
   $3C5C46,
   $12003C,
   $85BFFE0,
   $0D3004C08,
   $0CF004CFF,
   $0CB004CFF,
   $0C7004CFF,
   $0C3004CFF,
   $0BF004CFF,
   $80606FF,
   $8080808,
   $8080808,
   $8080808,
   $8080808,
   $5B5C0808,
   $1001B,
   $40019,
   $5B020001,
   $0C0316,
   $5C465C4B,
   $0,
   $0FFA40012,
   $85C46,
   $120008,
   $85BFFDC,
   $5B5C0808,
   $0C031B,
   $0C5429,
   $5C4B0001,
   $0C4948,
   $20000,
   $0,
   $0FF780012,
   $80008,
   $0FFB20012,
   $0B9004C5B,
   $125BFF,
   $11FEA2,
   $31A0022,
   $8,
   $2080000,
   $71A5B3F,
   $18,
   $4C0000,
   $4008FFEA,
   $0FF2C004C,
   $7215B5C,
   $290000,
   $10008,
   $0FFFFFFFF,
   $4C0000,
   $5B5CFFDA,
   $5C080814,
   $20011,
   $1001B,
   $240029,
   $5B020000,
   $0FE900012,
   $20011,
   $2011B,
   $80029,
   $5B050001,
   $21411,
   $20012,
   $2011B,
   $145429,
   $5B050001,
   $21411,
   $20012,
   $1001B,
   $0C5429,
   $5B020000,
   $20411,
   $0ED30,
   $20011,
   $1001B,
   $80029,
   $5B020000,
   $20011,
   $2011B,
   $200029,
   $5B050000,
   $21411,
   $760012,
   $2011C,
   $3A5517,
   $55170001,
   $10038,
   $7165B05,
   $5C4B0088,
   $3C5C46,
   $12003C,
   $85BFFE0,
   $6B004C08,
   $67004CFE,
   $63004CFE,
   $5F004CFE,
   $5B004CFE,
   $57004CFE,
   $80606FE,
   $8080808,
   $8080808,
   $8080808,
   $8080808,
   $5B5C0808,
   $0C0316,
   $5C465C4B,
   $0,
   $0FFB00012,
   $85C46,
   $120008,
   $85BFE80,
   $5B5C0808,
   $0C031B,
   $0C5429,
   $5C4B0001,
   $0C4948,
   $20000,
   $0,
   $0FF840012,
   $80008,
   $0FE560012,
   $0B9004C5B,
   $14115BFF,
   $120002,
   $11D0014,
   $5B05002A,
   $340315,
   $0F3004C08,
   $5B083EFF,
   $34031B,
   $145429,
   $4C0001,
   $5B5CFFE8,
   $0,
   $5E002E,
   $0D6008E,
   $16C011E,
   $21A01B4,
   $29E025C,
   $34002F2,
   $3BE0382,
   $43603FA,
   $4C60472,
   $56E04FC,
   $5F205B6,
   $6880640,
   $70006C4,
   $772073C,
   $7F007B4,
   $85C0820,
   $91608DA,
   $9880952,
   $0A0009C4,
   $0A900A48,
   $0B440B02,
   $0BD40B92,
   $0C5E0C22,
   $0CFA0C8E,
   $0D960D60,
   $0E020DCC,
   $0E920E44,
   $0F100ED4,
   $0F8E0F52,
   $10060FCA,
   $106C1036,
   $10F610A2,
   $11981150,
   $121C11E0,
   $12881258,
   $130612CA,
   $0,
   $0);

 RpcWinStationOpenServerFunctionString : array [0..11] of Dword = (
   $4800,
   $0,
   $320010,
   $0,
   $3440059,
   $108,
   $0,
   $42150,
   $1100008,
   $0A0008,
   $0C0070,
   $2);

 RpcWinStationCloseServerExFunctionString : array [0..11] of Dword = (
    $4800,
    $3C0000,
    $0EC30000C,
    $0,
    $590038,
    $1080344,
    $0,
    $1180000,
    $2800000,
    $42150,
    $700008,
    $20008
 );

 RpcWinStationCloseServerFunctionString : array [0..11] of Dword = (
   $4800,
   $10000,
   $4C30000C,
   $0,
   $210024,
   $1080344,
   $0,
   $80000,
   $0E0000,
   $42150,
   $700008,
   $20008
 );

 RpcWinStationQueryLogonCredentialsFunctionString : array [0..14] of DWORD = (
    $4800,
    $370000,
    $4C300014,
    $0,
    $210048,
    $3080545,
    $1,
    $80000,
    $0E0000,
    $40048,
    $20130008,
    $2680008,
    $0C0158,
    $700008,
    $20010
 );

 RpcWinStationQueryInformationFunctionString : array [0..19] of DWORD = (
    $4800,
    $50000,
    $4C300020,
    $0,
    $3D003C,
    $7080847,
    $10001,
    $80000,
    $0E0000,
    $42150,
    $480008,
    $80008,
    $0C0048,
    $1B0008,
    $460010,
    $140048,
    $21500008,
    $80018,
    $1C0070,
    $2
 );

  RpcWinStationSetInformationFunctionString : array [0..17] of DWORD = (
    $4800,
    $60000,
    $4C30001C,
    $0,
    $21003C,
    $7080747,
    $10001,
    $80000,
    $0E0000,
    $42150,
    $480008,
    $80008,
    $0C0048,
    $1B0008,
    $460010,
    $140048,
    $700008,
    $20018
 );

 RpcWinStationShadowFunctionString : array [0..20] of DWORD = (
    $4800,
    $110000,
    $4C300024,
    $0,
    $210047,
    $5080946,
    $10000,
    $80000,
    $0E0000,
    $42150,
    $480008,
    $80008,
    $0C000B,
    $48007A,
    $80010,
    $140048,
    $480008,
    $20018,
    $1C0048,
    $700006,
    $20020
 );

 RpcWinstationAutoReconnectFunctionString : array [0..14] of DWORD = (
    $4800,
    $470000,
    $4C300014,
    $0,
    $210034,
    $1080544,
    $0,
    $80000,
    $0E0000,
    $42150,
    $480008,
    $80008,
    $0C0048,
    $700008,
    $20010
 );

 RpcWinstationFUSCanRemoteUserDisconnectFunctionString : array [0..21] of DWORD = (
  $4800,
  $400000,
  $4C300028,
  $0,
  $21004C,
  $5080A46,
  $20000,
  $80000,
  $0E0000,
  $42150,
  $480008,
  $80008,
  $0C0048,
  $480008,
  $80010,
  $14010B,
  $48006A,
  $80018,
  $1C010B,
  $480298,
  $80020,
  $240070);
//  dw 2

WINSTA_OBJ_UID : PWideChar = '5ca4a760-ebb1-11cf-8611-00a0245420ed';
PROT_SEQ_LOCAL : PWideChar = 'ncalrpc';
END_POINT_LOCAL: PWideChar = 'IcaApi';
PROT_SEQ       : PWideChar = 'ncacn_np';
END_POINT      : PWideChar = '\pipe\Ctx_WinStation_API_service';
OPTIONS        : PWideChar = 'Security=Impersonation Dynamic True';

var
  Stub_descr : _MIDL_STUB_DESC = (
     RpcInterfaceInformation : @InterfaceConst;
     pfnAllocate : AllocateMemoryStd;
     pfnFree : FreeMemoryStd;
     IMPLICIT_HANDLE_INFO : (pAutoHandle : @LCRPC_IfHandle);
     pFormatTypes : @pFormat;
     Version : $50002;
     MIDLVersion : $600015B;
     mFlags : 1;
  );

function NdrClientCall2(stub_descr : PMIDL_STUB_DESC; pFormat : Pointer) : DWORD cdecl; varargs; external 'rpcrt4.dll';
procedure RpcSsDestroyClientContext(var ContextHandle : THandle); stdcall; external 'rpcrt4.dll';

function RpcWinStationOpenServer(Binding : Pointer; var ErrorCode : DWORD; var ServerHandle : DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationOpenServerFunctionString, @Binding) <> 0;
end;

function RpcWinStationCloseServerEx(ServerHandle : PHandle; var ErrorCode : DWORD) : Boolean; stdcall;
begin
   Result := NdrClientCall2(@Stub_descr, @RpcWinStationCloseServerExFunctionString, @ServerHandle) <> 0;
end;

function RpcWinStationCloseServer(ServerHandle : THandle; var ErrorCode : DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationCloseServerFunctionString, @ServerHandle) <> 0;
end;

function RpcWinStationQueryLogonCredentials(IcaApiHandle : THandle; SessionId : DWORD;
  var Buffer : PQueryCredentials; var ErrorCode : DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationQueryLogonCredentialsFunctionString, @IcaApiHandle) <> 0;
end;

function RpcWinStationQueryInformation(IcaApiHandle : THandle; var FunctionResult : DWORD;  SessionId : DWORD;
  WinStationInformationClass: Cardinal; pWinStationInformation: PVOID; WinStationInformationLength: DWORD; var pReturnLength: DWORD
) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationQueryInformationFunctionString, @IcaApiHandle) <> 0;
end;

function RpcWinStationSetInformation(IcaApiHandle : THandle; var FunctionResult : DWORD;  SessionId : DWORD;
  WinStationInformationClass: Cardinal; pWinStationInformation: PVOID; WinStationInformationLength: DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationSetInformationFunctionString, @IcaApiHandle) <> 0;
end;

function RpcWinStationShadow(IcaApiHandle : THandle; var FunctionResult : DWORD; SourceSessionId : DWORD;
  serverName : PWideChar; serverNameBytesLength : DWORD; TargetSessionId : DWORD; HotKey : DWORD; HotKeyModifier : DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationShadowFunctionString, @IcaApiHandle) <> 0;
end;

function RpcWinStationAutoReconnect(IcaApiHandle : THandle; var FunctionResult : DWORD; SessionId : DWORD;
  Unused : DWORD) : Boolean; stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinStationAutoReconnectFunctionString, @IcaApiHandle) <> 0;
end;

function RpcWinStationFUSCanRemoteUserDisconnect(IcaApiHandle : THandle; var functionResult : DWORD;
 targetSessionId : DWORD; sourceSessionId : DWORD; sourceProcessId : DWORD;
 domainName : PWideChar; domainNameLength : DWORD; userName : PWideChar; userNameLength : DWORD): Boolean;
  stdcall;
begin
  Result := NdrClientCall2(@Stub_descr, @RpcWinstationFUSCanRemoteUserDisconnectFunctionString, @IcaApiHandle) <> 0;
end;

function PrepareServerSPN(NetworkAddress : PWideChar) : WideString;
var
  Info : PWKSTA_INFO_100;
begin
  Result := NetworkAddress;
  if NetWkstaGetInfo(NetworkAddress, 100, PByte(Info))= NERR_Success then
    with Info^ do
    try
      if (wki100_computername <> nil) and (wki100_langroup <> nil) then
        Result := WideFormat('%s\%s$', [wki100_langroup, wki100_computername]);
    finally
      NetApiBufferFree(Info);
    end;
end;

function RpcWinStationBind(ObjUuid : PWideChar; Protseq : PWideChar; NetworkAddress : PWideChar;
  EndPoint : PWideChar; Options : PWideChar; var Binding : Pointer) : DWORD;
var
  BindingString : PWideChar;
begin
  Result := RpcStringBindingComposeW(ObjUuid, Protseq, NetworkAddress, EndPoint,
   Options, BindingString);
  if Result <> 0 then
    Exit;
  try
    Result := RpcBindingFromStringBindingW(BindingString, Binding);
  finally
    RpcStringFreeW(BindingString);
  end;
end;

function RpcWinStationBindSecure(ObjUuid : PWideChar; Protseq : PWideChar; NetworkAddress : PWideChar;
  EndPoint : PWideChar; Options : PWideChar; var Binding : Pointer) : DWORD;
var
  SecurityQOS : RPC_SECURITY_QOS;
  ServerSpn : WideString;
  pwDomain: WideString;
  pwPwd: WideString;
  pwUser: WideString;
begin
  Result := RpcWinStationBind(ObjUuid, Protseq, NetworkAddress, EndPoint, Options, Binding);
  if Result <> 0 then
    Exit;

  with SecurityQOS do
  begin
{   Version := RPC_C_SECURITY_QOS_VERSION_1;
    IdentityTracking := RPC_C_QOS_IDENTITY_DYNAMIC;
    Capabilities := RPC_C_QOS_CAPABILITIES_MUTUAL_AUTH;
    ImpersonationType := RPC_C_IMP_LEVEL_IMPERSONATE;}
        SecurityQoS.Version := 1;
        SecurityQos.Capabilities := RPC_C_QOS_IDENTITY_DYNAMIC;
        SecurityQos.IdentityTracking := RPC_C_QOS_CAPABILITIES_MUTUAL_AUTH;
        SecurityQoS.ImpersonationType := RPC_C_IMP_LEVEL_IMPERSONATE;

  end;

  ServerSpn := PrepareServerSPN(NetworkAddress);
  pwUser := 'remko';
  pwPwd := 'W0uter';
  pwDomain := 'PANDORA';
//  pwDomain := '';

  Security.User := PWideChar(pwUser);
  Security.UserLength := 5;

  Security.Domain := PWideChar(pwDomain);
  Security.DomainLength := 7;

  Security.Password := PWideChar(pwPwd);
  Security.PasswordLength := 6;

  Security.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;

  Result := RpcBindingSetAuthInfoExW(Binding, PWideChar(ServerSpn), RPC_C_AUTHN_LEVEL_PKT_PRIVACY,
   RPC_C_AUTHN_DEFAULT, @Security, RPC_C_AUTHN_WINNT, SecurityQOS);

  if Result <> 0 then
     RpcBindingFree(Binding);
end;

function WinStationOpenLocalServer(var ServerHandle : THandle) : DWORD;
var
  Binding : Pointer;
begin
  Result := RpcWinStationBind(WINSTA_OBJ_UID, PROT_SEQ_LOCAL,
    nil, END_POINT_LOCAL, OPTIONS, Binding);
  if Result <> 0 then
    Exit;
  try
    RpcWinStationOpenServer(Binding, Result, ServerHandle);
  finally
    RpcBindingFree(Binding);
  end;
end;

function WinStationOpenServer(ServerName : WideString) : THandle;
var
  Res: DWORD;
  Binding : Pointer;
begin
  Result := 0;
  if ServerName <> '' then
  begin
    Res := RpcWinStationBindSecure(WINSTA_OBJ_UID, PROT_SEQ, PWideChar(ServerName),
      END_POINT, OPTIONS, Binding);
    if Res <> 0 then
      Exit;
    try
      if (RpcWinStationOpenServer(Binding, Res, Result)) then
        Exit;
    finally
      RpcBindingFree(Binding);
    end;
    if (Res = RPC_S_UNKNOWN_AUTHN_SERVICE) or (Res = ERROR_ACCESS_DENIED) then
    begin
      // trying to open non-secure connection to server
      Res := RpcWinStationBind(WINSTA_OBJ_UID, PROT_SEQ, PWideChar(ServerName),
       END_POINT, OPTIONS, Binding);
      if Res <> 0 then
        Exit;
      try
        RpcWinStationOpenServer(Binding, Res, Result);
      finally
        RpcBindingFree(Binding);
      end;
    end;
  end
  else
    WinStationOpenLocalServer(Result);
end;

function WinStationCloseServer(var ServerHandle : THandle) : DWORD;
begin
  Result := ERROR_SUCCESS;
  if (ServerHandle <> 0) then
  begin
    if not RpcWinStationCloseServerEx(@ServerHandle, Result) and (Result = RPC_S_PROCNUM_OUT_OF_RANGE) then
    begin
      RpcWinStationCloseServer(ServerHandle, Result);
      RpcSsDestroyClientContext(ServerHandle);
    end;
  end;
end;

function WinStationGetLocalServerHandle : THandle;
begin
  Result := IcaApi_IfHandle;
end;

procedure InitLocalRpc();
begin
  WinStationOpenLocalServer(IcaApi_IfHandle);
end;

procedure DoneLocalRpc();
begin
  WinStationCloseServer(IcaApi_IfHandle);
end;

initialization
  InitLocalRpc();
finalization
  DoneLocalRpc();
end.
