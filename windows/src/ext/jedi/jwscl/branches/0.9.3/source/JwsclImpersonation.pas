{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains structures to support scopy based impersonation.

Author
Christian Wimmer


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

The Original Code is JwsclImpersonation.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclImpersonation;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface
uses ActiveX,
     JwaWindows,
     JwsclTypes,
     JwsclToken,
     JwsclSid,
     JwsclAcl,
     JwsclLsa,
     JwsclResource,
     JwsclExceptions,
     JwsclComUtils;

type {<B>IJwImpersonation</B> defines an interface for TJwImpersonation. }
    IJwImpersonation = Interface(IUnknown)
      function GetToken : TJwSecurityToken;
    end;

     {<B>TJwImpersonation</B> provides methods to impersonate a logged on client.
      Do not use this class instead use JwImpersonateLoggedOnUser, JwImpersonateLoggedOnUser or
      JwImpersonateLoggedOnUser.
	  
	  Remarks
       This class is intended only for use in services and fails without the TCB privilege.
     }
     TJwImpersonation = class(TInterfacedObject, IJwImpersonation)
     private

     protected
       fToken : TJwSecurityToken;
     public
       constructor Create(const LogonSessionLuid : TLuid); overload;
       constructor Create(const LogonSessionId : ULONG); overload;
	   
       {<B>Create</B> creates a new instance and impersonates the user logged onto
       the console session (typically 0 in xp and 1 in vista).

       @param UseWTSCall Set to true to use Windows Terminal Service API to get the user token;
         otherwise compability methods are used (useful in Windows 2000 Workstation).

        Remarks
           This constructor is intended only for use in services.
           If parameter UseWTSCall is false a compatibility function
           TJwSecurityToken.CreateCompatibilityQueryUserToken is called
           that returns the token from the first explorer.exe it finds.
           In this way any user explorer process can be returned - not only the
           console user. To avoid this problem call this constructor
           using true as parameter. Only apply FALSE if the current system
           does not support multiple users (like Win2000 Workstation) 
       }
       constructor Create(const UseWTSCall : Boolean = false); overload;

       function GetToken : TJwSecurityToken;

       destructor Destroy; override;

       property Token : TJwSecurityToken read GetToken;
     end;

{<B>JwImpersonateLoggedOnUser</B> impersonates the current logged on user (on console) and returns an interface pointer to the
token. It's automatically freed and reverted to self if it runs out of scope.

raises
 EJwsclProcessIdNotAvailable:  will be raised it the process does not have
    SE_TCB_NAME privilege and a try to get the explorer handle failed
 EJwsclWinCallFailedException: will be raised if OpenProcess fails
 EJwsclPrivilegeException : will be raised if SE_TCB_NAME is not available
Remarks
  This function is intended only for use in services. It impersonates the user working in the
  active console session and ignores all the other terminal session (like RDP).
  This function works also on Windows 2000 Workstation.
}
function JwImpersonateLoggedOnUser: IJwImpersonation; overload;

{<B>JwImpersonateLoggedOnUser</B> impersonates the current logged on user and returns an interface pointer to the
token. It's automatically freed and reverted to self if it runs out of scope.

@param LogonSessionId defines the user's logon session id. 

Remarks
  This function is intended only for use in services. It impersonates the user working in the 
  given session.
}
function JwImpersonateLoggedOnUser(const LogonSessionId : ULONG) : IJwImpersonation; overload;

{<B>JwImpersonateLoggedOnUser</B> impersonates the current logged on user and returns an interface pointer to the
token. It's automatically freed and reverted to self if it runs out of scope.

@param LogonSessionLuid defines the session luid to be impersonated. 

}
function JwImpersonateLoggedOnUser(const LogonSessionLuid : TLuid) : IJwImpersonation; overload;



implementation
uses SysUtils, JwsclPrivileges;

function JwImpersonateLoggedOnUser: IJwImpersonation;
var Scope : IJwPrivilegeScope;
begin
  Scope := JwGetPrivilegeScope([SE_TCB_NAME]);
  try
    //first try WTS call then look for explorer
    //WTSXXX calls need TCB privilege - so it often simply fails
    result := TJwImpersonation.Create(true);
  except
    on E : EJwsclSecurityException do
      result := TJwImpersonation.Create(false);
  end;  
end;
function JwImpersonateLoggedOnUser(const LogonSessionId : ULONG) : IJwImpersonation;
begin
  result := TJwImpersonation.Create(LogonSessionID);
end;

function JwImpersonateLoggedOnUser(const LogonSessionLuid : TLuid) : IJwImpersonation;
begin
  result := TJwImpersonation.Create(LogonSessionLuid);
end;


{ TJwImpersonation }

constructor TJwImpersonation.Create(const LogonSessionLuid: TLuid);
var SessionData : TJwLsaLogonSessionData;
begin
  SessionData := TJwLsaLogonSession.GetSessionData(LogonSessionLuid);
  Create(SessionData.Session);
end;

constructor TJwImpersonation.Create(const LogonSessionId : ULONG);
begin
  fToken := TJwSecurityToken.CreateWTSQueryUserToken(LogonSessionId);
  fToken.ConvertToImpersonatedToken(SecurityImpersonation,
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
  fToken.ImpersonateLoggedOnUser;
end;

constructor TJwImpersonation.Create(const UseWTSCall : Boolean = false);
begin
  if UseWTSCall then
    Create(WTSGetActiveConsoleSessionId)
  else
  begin
    //
    fToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(
      TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);

    try
      fToken.ConvertToImpersonatedToken(SecurityImpersonation,
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
      fToken.ImpersonateLoggedOnUser;
    except
      on E : EJwsclSecurityException do
      begin
        FreeAndNil(fToken);
        raise;
      end;
    end;
  end;
end;

destructor TJwImpersonation.Destroy;
begin
  if Assigned(fToken) then
    fToken.RevertToSelf;
   
  FreeAndNil(fToken);
  inherited;
end;

function TJwImpersonation.GetToken: TJwSecurityToken;
begin
  result := fToken;
end;

procedure Test;
var t : IJwImpersonation;
    K : TJwSecurityToken;
    user : TJwSecurityId;

begin
  t := JwImpersonateLoggedOnUser;
  K := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  user := K.TokenUser;
  user.StringSID;
  user.Free;

  if t = nil then;
end;

initialization





end.
