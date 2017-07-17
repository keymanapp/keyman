unit WinLogonLogoServiceProxy;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  JwaWindows, SensEvents_TLB, ComObj, ActiveX, WinLogonLogoService_TLB, StdVcl;

type
  TSENSLogonProxy = class(TAutoObject, ISensLogon2)
  public
    procedure Logon(const bstrUserName: WideString; dwSessionId: LongWord); overload; safecall;
    procedure Logoff(const bstrUserName: WideString; dwSessionId: LongWord); overload; safecall;
    procedure SessionDisconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure SessionReconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure PostShell(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
   end;

implementation

uses ComServ, dialogs, WinlogonLogoServiceUnit;

{ TCoSENSLogonProxyCoSENSLogonProxy }

{ TSENSLogonProxy }


procedure TSENSLogonProxy.Logoff(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
   SENSTestService.Logoff(bstrUserName,dwSessionId);
end;

procedure TSENSLogonProxy.Logon(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
  SENSTestService.Logon(bstrUserName,dwSessionId);
end;

procedure TSENSLogonProxy.PostShell(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
  SENSTestService.PostShell(bstrUserName,dwSessionId);
end;

procedure TSENSLogonProxy.SessionDisconnect(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
   SENSTestService.SessionDisconnect(bstrUserName,dwSessionId);
end;

procedure TSENSLogonProxy.SessionReconnect(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
  SENSTestService.SessionReconnect(bstrUserName,dwSessionId);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSENSLogonProxy, CLASS_SENSLogonProxy,
    ciSingleInstance, tmApartment);
end.
