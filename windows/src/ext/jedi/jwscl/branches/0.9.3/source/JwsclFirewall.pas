{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides functions and procedures for adding and removing ports and application rules to the windows firewall

Author
Heiko Adams

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU Lesser General Public License (the  "LGPL License"), in which case the
provisions of the LGPL License are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the LGPL License and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL
License.  If you do not delete the provisions above, a recipient may use
your version of this file under either the MPL or the LGPL License.

For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

Note
The Original Code is JwsclFirewall.pas.

The Initial Developer of the Original Code is Heiko Adams


}
unit JwsclFirewall;
{$INCLUDE ..\includes\Jwscl.inc}

interface

uses
    JwaWindows,
    ComObj,
    NetFwTypeLib_TLB, //can be found in \jwapi\trunk\COM
    JwsclConstants,
    JwsclExceptions,
    JwsclStrings;

type
  { TJwsclFirewall implements methods and properties to access, administer, set and
    get the Microsoft Windows Firewall.                                             }
  TJwsclFirewall = class(TObject)
  protected
    FFWMgr: INetFwMgr;
    FProfile: INetFwProfile;

    function GetFirewallState: Boolean; virtual;
    function GetExceptionsAllowed: Boolean; virtual;

    procedure SetFirewallState(Value: Boolean); virtual;
    procedure SetExceptionsAllowed(Value: Boolean); virtual;

    function GetIncomingPingAllowed: Boolean; virtual;
    procedure SetIncomingPingAllowed(Value: Boolean); virtual;

    function GetRemoteAdminAllowed(): Boolean; virtual;
    procedure SetRemoteAdminAllowed(Value: Boolean); virtual;

    function GetRemoteAdminAdress(): TJwString; virtual;
    procedure SetRemoteAdminAdress(Value: TJwString); virtual;
  public
    { Create creates a new TJwsclFirewall instance and connects to the firewall
      manager COM interface.
      
      
      
      
      Exceptions
      EJwsclFirewallProfileInitException :  This exception is raised if the connection
                                            to the firewall COM interface failed. The
                                            lasterror member contains the COM error
                                            \result.
      Wert  :                               Beschreibung                               }
    constructor Create();
    destructor Destroy(); override;

     {<B>AddToWinFirewall</B> Creates a firewallrule for specified program.
     @param ApplicationFilename defines the executable of the program
     @param NameOnExceptionList defines the string that should be used in the firewall's
      rules list
     @param EnableRule defines if the new rule should be active or not
     }
    procedure AddToWinFirewall(const ApplicationFilename, NameOnExceptionList: TJwString;
      const EnableRule: Boolean); virtual;


    {<B>DeleteFromWinFirewall</B> Removes a program's firewall rule.
     @param ApplicationFilename defines the executable of the program
    }
    procedure DeleteFromWinFirewall(const ApplicationFilename: TJwString); virtual;

    {<B>AddTcpPortToFirewall</B> Adds a rule for a single tcp port to the firewall.
     @param ProtocollName gives a description of the opened port (e.g. "FTP-Server") 
     @param ProtocollPort defines the port of the protocol
     @param SubnetOnly defines if the rule affects only the pc's subnet or not
     @param PortRemoteAddresses defines which Remoteadress and port should be allowed
    }
    procedure AddTcpPortToFirewall(ProtocollName: TJwString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: TJwString = '*'); virtual;

    {<B>AddUdpPortToFirewall</B> Adds a rule for a single udp port to the firewall.
     @param ProtocollName gives a description of the opened port (e.g. "FTP-Server")
     @param ProtocollPort defines the port of the protocol
     @param SubnetOnly defines if the rule affects only the pc's subnet or not
     @param PortRemoteAddresses defines which Remoteadress and port should be allowed
    }
    procedure AddUdpPortToFirewall(ProtocollName: TJwString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: TJwString = '*'); virtual;

    {<B>IsAppRulePresent</B> Checks if a firewall rule for the specified application is already present
     @param ApplicationFilename defines the executable of the program
    }
    function IsAppRulePresent(const ApplicationFilename: TJwString): Boolean;

    {<B>Active</B> Sets or gets if the windows firewall is active or not}
    property Active: Boolean read GetFirewallState write SetFirewallState;

    {<B>ExceptionsAllowed</B> Sets or gets if the windows firewall allows exceptions}
    property ExceptionsAllowed: Boolean read GetExceptionsAllowed
      write SetExceptionsAllowed;

    {<B>IncomingPingAllowed</B> Sets or gets if the windows firewall allows Incoming pings}
    property IncomingPingAllowed: Boolean read GetIncomingPingAllowed
      write SetIncomingPingAllowed;

    {<B>RemoteAdminAllowed</B> Sets or gets if the windows firewall allows remote administration}
    property RemoteAdminAllowed: Boolean read GetRemoteAdminAllowed
      write SetRemoteAdminAllowed;

    {<B>ExceptionsAllowed</B> Sets or gets the adress(es) which are allowed for remote administration}
    property RemoteAdminAdress: TJwString read GetRemoteAdminAdress
      write SetRemoteAdminAdress;
  end;

implementation
uses JwsclResource;

constructor TJwsclFirewall.Create();
begin
  inherited;

  try
    FFWMgr := CoNetFwMgr.Create;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclFirewallProfileInitException.CreateFmtEx(e.Message,
        'Create', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;

  try
    FProfile := FFWMgr.LocalPolicy.CurrentProfile;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclFirewallProfileInitException.CreateFmtEx(e.Message,
        'Create', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;

destructor TJwsclFirewall.Destroy();
begin
  inherited;
  FFWMgr := nil;
  FProfile := nil;
end;

procedure TJwsclFirewall.SetFirewallState(Value: Boolean);
begin
  try
    FProfile.FirewallEnabled := Value;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclSetFWStateException.CreateFmtEx(e.Message,
        'SetFirewallState', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


function TJwsclFirewall.GetFirewallState(): Boolean;
begin
  try
    Result := FProfile.FirewallEnabled;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetFWStateException.CreateFmtEx(e.Message,
        'GetFirewallState', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


function TJwsclFirewall.GetExceptionsAllowed(): Boolean;
begin
  try
    Result := (not FProfile.ExceptionsNotAllowed);
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetFWExceptionsAllowedException.CreateFmtEx(e.Message,
        'GetExceptionsAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


procedure TJwsclFirewall.SetExceptionsAllowed(Value: Boolean);
begin
  try
    FProfile.ExceptionsNotAllowed := Value;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclSetFWExceptionsAllowedException.CreateFmtEx(e.Message,
        'SetExceptionsAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


function TJwsclFirewall.GetIncomingPingAllowed: Boolean;
begin
  try
    Result := FProfile.IcmpSettings.AllowInboundEchoRequest;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetIncomingPingAllowedException.CreateFmtEx(e.Message,
        'GetIncomingPingAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


procedure TJwsclFirewall.SetIncomingPingAllowed(Value: Boolean);
begin
  try
    FProfile.IcmpSettings.AllowInboundEchoRequest := Value;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclSetIncomingPingAllowedException.CreateFmtEx(e.Message,
        'SetIncomingPingAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


function TJwsclFirewall.GetRemoteAdminAllowed(): Boolean;
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    Result := RASettings.Enabled;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'GetRemoteAdminAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


procedure TJwsclFirewall.SetRemoteAdminAllowed(Value: Boolean);
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    RASettings.Enabled := Value;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'SetRemoteAdminAllowed', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


function TJwsclFirewall.GetRemoteAdminAdress(): TJwString;
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    Result := RASettings.RemoteAddresses;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclGetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'GetRemoteAdminAdress', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


procedure TJwsclFirewall.SetRemoteAdminAdress(Value: TJwString);
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    RASettings.RemoteAddresses := Value;
  except
    on e: EOleSysError do
    begin
      SetLastError(E.ErrorCode);
      raise EJwsclSetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'SetRemoteAdminAdress', ClassName, RsUNFirewall,
        0, True, []);
    end;
  end;
end;


procedure TJwsclFirewall.AddToWinFirewall(const ApplicationFilename,
  NameOnExceptionList: TJwString; const EnableRule: Boolean);
var
  App: INetFwAuthorizedApplication;
begin                
  if GetFirewallState
    and GetExceptionsAllowed then
  begin
    try
      App := CoNetFwAuthorizedApplication.Create;
	  //Create throws OleException if anything goes wrong

	  App.ProcessImageFileName := ApplicationFilename;
	  App.Name := NameOnExceptionList;
	  App.Scope := NET_FW_SCOPE_ALL;
	  App.IpVersion := NET_FW_IP_VERSION_ANY;
	  App.Enabled := EnableRule;

	  try
	    FProfile.AuthorizedApplications.Add(App);
	  except
	    on e: EOleSysError do
	    begin
	  	  SetLastError(E.ErrorCode);
		  raise EJwsclFirewallAddRuleException.CreateFmtEx(e.Message,
			'AddToWinFirewall', ClassName, RsUNFirewall,
			0, True, []);
		end;
      end;
    finally
      App := nil;
    end;
  end;
end;


procedure TJwsclFirewall.AddTcpPortToFirewall(ProtocollName: TJwString;
  ProtocollPort: Integer; const SubnetOnly: Boolean = False;
  const PortRemoteAddresses: TJwString = '*');
var
  Port: INetFwOpenPort;
begin
  if not GetFirewallState then
  begin
    raise EJwsclFirewallInactiveException.CreateFmtEx(RsFWInactive,
          'AddTcpPortToFirewall', ClassName, RsUNFirewall,
          0, false, []);
  end;

  if not GetExceptionsAllowed then
  begin
    raise EJwsclFirewallNoExceptionsException.CreateFmtEx(RsFWNoExceptionsAllowed,
          'AddTcpPortToFirewall', ClassName, RsUNFirewall,
          0, false, []);
  end;

  try
    Port := CoNetFwOpenPort.Create;

    Port.Name := ProtocollName;
    Port.Protocol := NET_FW_IP_PROTOCOL_TCP;
	  
    Port.Port := ProtocollPort;
    if SubnetOnly then
      Port.Scope := NET_FW_SCOPE_LOCAL_SUBNET
    else
      Port.Scope := NET_FW_SCOPE_ALL;
    Port.RemoteAddresses := PortRemoteAddresses;
    Port.Enabled := True;

    try
      FProfile.GloballyOpenPorts.Add(Port);
    except
      on e: EOleSysError do
      begin
        SetLastError(E.ErrorCode);
        raise EJwsclAddTcpPortToFirewallException.CreateFmtEx(e.Message,
          'AddTcpPortToFirewall', ClassName, RsUNFirewall,
          0, True, []);
      end;
    end;
  finally
    Port := nil;
  end;
end;


procedure TJwsclFirewall.AddUdpPortToFirewall(ProtocollName: TJwString;
  ProtocollPort: Integer; const SubnetOnly: Boolean = False;
  const PortRemoteAddresses: TJwString = '*');
var
  Port: INetFwOpenPort;
begin
  if not GetFirewallState then
  begin
    raise EJwsclFirewallInactiveException.CreateFmtEx(RsFWInactive,
          'AddUdpPortToFirewall', ClassName, RsUNFirewall,
          0, false, []);
  end;

  if not GetExceptionsAllowed then
  begin
    raise EJwsclFirewallNoExceptionsException.CreateFmtEx(RsFWNoExceptionsAllowed,
          'AddUdpPortToFirewall', ClassName, RsUNFirewall,
          0, false, []);
  end;
          
  try
    Port := CoNetFwOpenPort.Create;

    Port.Name := ProtocollName;
    Port.Protocol := NET_FW_IP_PROTOCOL_UDP;
    Port.Port := ProtocollPort;
    if SubnetOnly then
      Port.Scope := NET_FW_SCOPE_LOCAL_SUBNET
    else
      Port.Scope := NET_FW_SCOPE_ALL;
    Port.RemoteAddresses := PortRemoteAddresses;
    Port.Enabled := True;
    

    try
      FProfile.GloballyOpenPorts.Add(Port);
    except
      on e: EOleSysError do
      begin
        SetLastError(E.ErrorCode);
        raise EJwsclAddTcpPortToFirewallException.CreateFmtEx(e.Message,
          'AddUdpPortToFirewall', ClassName, RsUNFirewall,
          0, True, []);
      end;
    end;
  finally
    Port := nil;
  end;
end;


procedure TJwsclFirewall.DeleteFromWinFirewall(const ApplicationFilename: TJwString);
begin
  if GetFirewallState
    and GetExceptionsAllowed then
    try
      FProfile.AuthorizedApplications.Remove(ApplicationFilename);
    except
      on e: EOleSysError do
      begin
        SetLastError(E.ErrorCode);
        raise EJwsclFirewallDelRuleException.CreateFmtEx(e.Message,
          'DeleteFromWinFirewall', ClassName, RsUNFirewall,
          0, True, []);
      end;
    end;
end;


function TJwsclFirewall.IsAppRulePresent(const ApplicationFilename: TJwString) : Boolean;
var
  App: INetFwAuthorizedApplication;
begin
  try
    try
      App := FProfile.AuthorizedApplications.Item(ApplicationFilename);
      Result := (App <> nil)
    except
      Result := False;
    end;
  finally
    App := nil;
  end;
end;

end.

