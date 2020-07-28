(*
  Name:             ProxySettings
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jan 2007

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Initial version
                    20 Jul 2008 - mcdurdin - I1556 - Initial version
                    05 Aug 2008 - mcdurdin - Add Refresh function to proxy settings
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit GlobalProxySettings;

interface

type
  TProxySettings = class
  private
    FPort: Integer;
    FServer: string;
    FPassword: string;
    FUsername: string;
  public
    constructor Create;
    procedure Refresh;
    procedure Save(const Server: string; Port: Integer; const Username, Password: string);
    property Server: string read FServer;
    property Port: Integer read FPort;
    property Username: string read FUsername;
    property Password: string read FPassword;
  end;

function GetProxySettings: TProxySettings;

const
  ProxySettingsStandinPassword = '##PASSWORD##';

implementation

uses
  System.SysUtils,

  DCPcrypt2,
  DCPrc4,
  ErrorControlledRegistry,
  RegistryKeys;

var
  FProxySettings: TProxySettings = nil;

function GetProxySettings: TProxySettings;
begin
  if not Assigned(FProxySettings) then
    FProxySettings := TProxySettings.Create;
  Result := FProxySettings;
end;

{ TProxySettings }

const
  LoginManagerKey = '\uFCEf+$HP/s=QEj/a~\/o9Uy-4+e=CJ-Tac%irowehbcT>j##rb+%e34X+rWYV-';

constructor TProxySettings.Create;
begin
  inherited Create;
  Refresh;
end;

procedure TProxySettings.Refresh;
var
  FOnlineLogin: string;
  n: Integer;
begin
  FServer := '';
  FPort := 0;
  FUsername := '';
  FPassword := '';
  
  inherited Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine_CU) then
    begin
      if ValueExists(SRegValue_ProxyServer) then FServer := ReadString(SRegValue_ProxyServer);
      if ValueExists(SRegValue_ProxyPort) then FPort := ReadInteger(SRegValue_ProxyPort);
      if ValueExists(SRegValue_ProxyLogin) then
      begin
        with TDCP_rc4.Create(nil) do
        try
          Init(LoginManagerKey, Length(LoginManagerKey), nil);
          FOnlineLogin := DecryptString(ReadString(SRegValue_ProxyLogin));
          n := Pos(#13, FOnlineLogin);
          if n > 0 then
          begin
            FUsername := Copy(FOnlineLogin, 1, n-1);
            FPassword := Copy(FOnlineLogin, n+1, Length(FOnlineLogin));
          end;
          Burn;
        finally
          Free;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TProxySettings.Save(const Server: string; Port: Integer;
  const Username, Password: string);
begin
  FServer := Server;
  FPort := Port;
  FUsername := Username;
  FPassword := Password;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanEngine_CU, True) then
    begin
      WriteString(SRegValue_ProxyServer, FServer);
      WriteInteger(SRegValue_ProxyPort, FPort);

      with TDCP_rc4.Create(nil) do
      try
        Init(LoginManagerKey, Length(LoginManagerKey), nil);
        WriteString(SRegValue_ProxyLogin, EncryptString(FUsername + #13 + FPassword));
        Burn;
      finally
        Free;
      end;
    end;
  finally
    Free;
  end;
end;

initialization
finalization
  FreeAndNil(FProxySettings);
end.
