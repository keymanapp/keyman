{
Description

This unit provides access to the MS AuthZ technology API.
The main reason is to do AccessCheck without needing a token.
           
Project JEDI Windows Security Code Library (JWSCL)

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

The Original Code is JwsclAuthCtx.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Unsupported features :
* AuthzEnumerateSecurityEventSources
* AuthzFreeAuditEvent
* AuthzInstallSecurityEventSource
* AuthzOpenObjectAudit
* AuthzRegisterSecurityEventSource
* AuthzReportSecurityEvent
* AuthzReportSecurityEventFromParams
* AuthzUninstallSecurityEventSource
* AuthzUnregisterSecurityEventSource


}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAuthCtx;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  SysUtils, 
  jwaWindows, 
  JwsclResource, JwsclUtils,

  JwsclTypes, JwsclExceptions, JwsclMapping, JwsclACL, JwsclToken,
  JwsclEnumerations,
  JwsclVersion, JwsclConstants, JwsclSid, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwAuthContext = class;

  {<B>TJwOnAuthzAccessCheckCallback</B> is called every time TJwAuthContext.AccessCheck finds
   a callback access control entry in an ACL.
   @param AuthzClientContext contains the handle to the context 
   @param Ace contains the access control entry 
   @param Contains an used defined pointer supplied to
     parameter TJwAuthZAccessRequest(Request).fData 
   @param AceApplicable defines whether the ACE is used for AccessCheck (true)
     or not (false) 

   raises
 EJwsclSecurityException:  The callee can raise any exception derived from
    EJwsclSecurityException. The LastError value will be
    used to indicate an error to the caller. The function Accesscheck will
    usually stop executing and return this error. 
  }
  TJwOnAuthzAccessCheckCallback = procedure(
    AuthzClientContext : TAuthZClientContextHandle;
    const Ace : TJwSecurityAccessControlEntry;
    Args : Pointer;
    var AceApplicable : Boolean) of object; stdcall;

  {<B>TJwOnAuthzComputeGroupsCallback</B> is called to add more groups to the authentication
   context. It is used in the creation of TJwAuthResourceManager.

   @param AuthzClientContext contains the handle to the context 
   @param Args is a used defined pointer that is supplied to one
    of the constructors of TJwAuthContext 
   @param SidAttrArray defines a list of Sids to be used
      as additional group Sids. The instance is already created with
      no members. 
      )
   @param Sids contains a list of Sids and its Attributes to be used as
       new groups in the security context. (like TokenGroups in a token).
       The list will initialized but empty and can be filled by the callee.

       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       
        # SE_GROUP_ENABLED - adds a group to the security context.
              It will be treated as if the User has entered a group. 
        # SE_GROUP_USE_FOR_DENY_ONLY - adds a group to the security context,
          but this group is only used for deny check. All positive ACE for
          this group in a DACL are ignored. Only Deny ACEs are recognized
          and can turn off other positive ACEs of other groups. 
        
        
    @param RestrictedSids receives a list of Sids and its Attributes to be used as
       new deny only groups in the security context. (like TokenGroups in a token).
       The list will initialized but empty and can be filled by the callee.
       
       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       
        # SE_GROUP_ENABLED - (probably) the same as parameter SID and
            attribute SE_GROUP_USE_FOR_DENY_ONLY. 
        # SE_GROUP_USE_FOR_DENY_ONLY - Results alway in Access Denied.
            However do not rely on me - maybe somebody with
            internal knowledge can comment it. 
       
     
   raises
 EJwsclSecurityException:  The callee can raise any exception derived from
    EJwsclSecurityException. The LastError value will be
    used to indicate an error to the caller. The function Accesscheck will
    usually stop executing and return this error. 
  }
  TJwOnAuthzComputeGroupsCallback = procedure(
    AuthzClientContext : TAuthZClientContextHandle;
    Args : Pointer;
    const SidAttrArray : TJwSecurityIdList;
    const RestrictedSidAttrArray : TJwSecurityIdList
   ) of object;


  {<B>TJwAuthZAccessRequest</B> provides simple access to a request that is submitted
  to TJwAuthContext.AccessCheck }
  TJwAuthZAccessRequest = class
  protected
    fDesiredAccess    : TJwAccessMask;
    fPrincipalSelfSid : TJwSecurityID;
    fObjectTypeArray : TJwObjectTypeArray;
    fData : Pointer;
    fShared : TJwSharedHandle;
  public
    {<B>Create</B> creates a new request instance.
     @param DesiredAccess defines which access rights are checked. Can be
       MAXIMUM_ALLOWED to get highest possible access. 
     @param PrincipalSelfSid defines a SID that is used to replace a principle self sid
     found in an inherited ACE. A principle self SID (S-1-5-10)
     in a ACE will be replaced by this property SID.
      
     @param ObjectTypeArray defines an array of object properties.
        The Level of the objects must comply to some rules.
      <code lang="Delphi">
      Array[i].Level = a_i
              { a_i +1        | a_i - a_(i-1) = 1 AND a_i < 4
      a_i+1 = { a_i - t       | a_i - t AND t >= 0
              { ERROR_INVALID_PARAMETER | else

      sequence start: a_0 = 0
      </code>
      See also http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx
        
     @param Data defines user data that is supplied to the request in
        the OptionalData member, 
     @param Shared is not used. 
    }
    constructor Create(
      DesiredAccess    : TJwAccessMask;
      PrincipalSelfSid : TJwSecurityID;
      ObjectTypeArray : TJwObjectTypeArray;
      Data : Pointer;
      Shared : TJwSharedHandle);
    destructor Destroy; override;

    {<B>DesiredAccess</B> gets the desired access given to the constructor.}
    property DesiredAccess    : TJwAccessMask read fDesiredAccess;

    {<B>PrincipalSelfSid</B> defines a SID that is used to replace a principle self sid
     found in an inherited ACE. A principle self SID (S-1-5-10)
     in a ACE will be replaced by this property SID.
     }
    property PrincipalSelfSid : TJwSecurityID read fPrincipalSelfSid;

    {<B>ObjectTypeArray</B> gets the object type error defined in the create.}
    property ObjectTypeArray : TJwObjectTypeArray read fObjectTypeArray;

    {<B>Data</B> user defined data that is passed to parameter Args
     of TJwOnAuthzAccessCheckCallback }
    property Data : Pointer read fData write fData;
  end;





  {<B>TJwAuthZAccessReply</B> contains information about a call to TJwAuthContext.AccessCheck}
  TJwAuthZAccessReply = class
  protected
    fGrantedAccessMask : TJwAccessMaskArray;
    fSaclEvaluationResults,
    fError : TJwCardinalArray;
    fErrorByType : TJwReplyErrorEnumArray;
  public
    {<B>Create</B> create a new instance with information returned by TJwAuthContext.AccessCheck}
    constructor Create(const ReplyStruct : TAuthzAccessReply);

    {<B>GrantedAccessMask</B> defines an array of at least one element that defines which
     rights are granted.  }
    property GrantedAccessMask : TJwAccessMaskArray read fGrantedAccessMask;

    {<B>SaclEvaluationResults</B> TBD}
    property SaclEvaluationResults : TJwCardinalArray read fSaclEvaluationResults;

    {<B>Error</B> defines an array of at least one element that contains the error
     result of the access check.
     Values are from
      http://msdn2.microsoft.com/en-us/library/aa376321(VS.85).aspx
     
     # ERROR_SUCCESS  All the access bits, not including MAXIMUM_ALLOWED, are granted and the GrantedAccessMask member is not zero. 
     # ERROR_PRIVILEGE_NOT_HELD DesiredAccess includes ACCESS_SYSTEM_SECURITY and the client does not have SeSecurityPrivilege. 
     # ERROR_ACCESS_DENIED Includes each of the following:
		* The requested bits are not granted.
		* MaximumAllowed bit is on and granted access is zero.
		* DesiredAccess is zero. 
      
     }
    property Error : TJwCardinalArray read fError;

    {<B>ErrorByType</B> defines an array of at least one element that contains
     the error result of the access check. It is the same result as in property
     Error but instead a Delphi enumeration type (TJwReplyErrorEnumArray) is used for better understanding.
     }
    property ErrorByType : TJwReplyErrorEnumArray read fErrorByType;
  end;

  {<B>TJwAuthResourceManager</B> contains structures that mangage resources of a
   security context like callback functions. }
  TJwAuthResourceManager = class
  private
    fHandle : TAuthZResourceManagerHandle;
    fOnAuthzComputeGroupsCallback : TJwOnAuthzComputeGroupsCallback;
    fOnAuthzAccessCheckCallback : TJwOnAuthzAccessCheckCallback;

{$IFDEF DEBUG}
    fOnAuthzComputeGroupsCallbackDebug : TJwOnAuthzComputeGroupsCallback;
    fOnAuthzAccessCheckCallbackDebug : TJwOnAuthzAccessCheckCallback;
{$ENDIF DEBUG}

  protected
    {procedure OnInternalAuthzFreeGroupsCallback(
        const pSidAttrArray : array of SID_AND_ATTRIBUTES
        ); stdcall;}
    {for internal use}
    procedure OnInternalAuthzComputeGroupsCallback(
       AuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
        Args : Pointer;
        const SidAttrArray : TJwSecurityIdList;
        const RestrictedSidAttrArray : TJwSecurityIdList
       );

    {for internal use}
    procedure OnInternalAuthzAccessCheckCallback(
      AuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      const Ace : TJwSecurityAccessControlEntry;
      Args : Pointer;
      var AceApplicable : Boolean);stdcall;


  public
    {<B>Create</B> create a resource manager instance that is used to create
     one or more instances of TJwAuthContext.

     @param Name defines a user defined name of the manager 
     @param Flags defines a set of options how to create the manager.
            If the set is empty the default value is [authRM_NoAudit].

            See TJwAuthZResourceManagerFlags  for more information 
     @param OnAuthzComputeGroupsCallback defines a callback function
       that is called to retrieve more groups for an access check.
       Groups allocated in this method callback should be freed in OnAuthzFreeGroupsCallback.
       However the constructor does not enforce it.
        
     @param OnAuthzAccessCheckCallback is called everytime TJwAuthContext.AccessCheck
        encounters a callback ACE.
        Hint: Be aware that JWSCL does not provide callback access control entries at this time. 
     }
    constructor Create(
      const Name : WideString;
      Flags : TJwAuthZResourceManagerFlags;
      const OnAuthzComputeGroupsCallback : TJwOnAuthzComputeGroupsCallback;

      const OnAuthzAccessCheckCallback : TJwOnAuthzAccessCheckCallback
     );

     destructor Destroy; override;


  public
    {<B>Handle</B> contains the handle to the resource manager.
     Warning: Do not close it!}
    property Handle : TAuthZResourceManagerHandle read fHandle;

    {See Create for more information.}
    property OnAuthzComputeGroupsCallback : TJwOnAuthzComputeGroupsCallback
      read fOnAuthzComputeGroupsCallback;
    {See Create for more information.}
    property OnAuthzAccessCheckCallback : TJwOnAuthzAccessCheckCallback
      read fOnAuthzAccessCheckCallback;
  end;






  PJwCallbackData = ^TJwCallbackData;
  {<B>TJwCallbackData</B> defines a callback structure that is used internally only.}
  TJwCallbackData = record
    Hd : Integer;
    Context : TJwAuthContext;
    RM : TJwAuthResourceManager;
    UserData : Pointer;
  end;

  {<B>TJwAuthContext</B> defines an instance that provides methods for an authentication
   context. This context does not need a token to do an access check.
  }
  TJwAuthContext = class
  protected
    fHandle : TAuthZClientContextHandle;
    fUserSid : TJwSecurityID;
    fGroupSids,
    fRestrictedSids : TJwSecurityIdList;
    fInfoPrivileges : TJwPrivilegeSet;
    fExpirationTime : Int64;
    fContextInfoIdentifier : TLuid;
    fContextInfoSource,
    fContextInfoAll,
    fContextInfoAuthenticationId : Pointer;
    fAuthResourceManager : TJwAuthResourceManager;
    fDynamicGroupArgs : Pointer;

    fCallbackData : PJwCallbackData;
  public
    {<B>GetInformationFromContext</B> returns information about a context.
     @param InfoClass See http://msdn2.microsoft.com/en-us/library/aa376323(VS.85).aspx 
     @return Returns a pointer to the information. Must be freed by Freemem 
     }
    function GetInformationFromContext(
      InfoClass : AUTHZ_CONTEXT_INFORMATION_CLASS) : Pointer;

    procedure InitProperties;
  public
    {Do not use.}
    constructor Create; overload;

    {<B>CreateByContext</B> creates an authentication context from a token.

     @param Flags not used. 
     @param Token A token that is used for initializing the context. The token
        must be opened with TOKEN_QUERY. 
     @param ExpirationTime Time for which the access check must remain valid. 
     @param DynamicGroupArgs is a used defined pointer that is passed to
        a call to TJwAuthResourceManager.OnAuthzComputeGroupsCallback 
     raises
 EJwsclWinCallFailedException:  will be raised if a call to
      AuthzInitializeContextFromAuthzContext failed.
      If GetLastError returns ERROR_INVALID_BLOCK, the reason is because
       a callback data block (TJwCallbackData) is invalid. This can only
       happen if someone changed the internal instance data.
       
      EJwsclNILParameterException: will be raised if parameter ResourceManager or
      Sid is nil 
    }
    constructor CreateByContext(
      const AuthContext : TJwAuthContext;
      Flags : TAuthZSidContextFlags;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;


    {<B>CreateByToken</B> creates an authentication context from a token.

     @param ResourceManager defines a resource manager instance. Must not be nil. 
     @param Flags not used. 
     @param Token A token that is used for initializing the context. The token
        must be opened with TOKEN_QUERY. 
     @param ExpirationTime not used 
     @param DynamicGroupArgs is a used defined pointer that is passed to
        a call to TJwAuthResourceManager.OnAuthzComputeGroupsCallback 
     raises
 EJwsclWinCallFailedException:  will be raised if a call to
      AuthzInitializeContextFromToken failed.
      If GetLastError returns ERROR_INVALID_BLOCK, the reason is because
       a callback data block (TJwCallbackData) is invalid. This can only
       happen if someone changed the internal instance data.
       
      EJwsclNILParameterException: will be raised if parameter ResourceManager or
      Sid is nil 
    }
    constructor CreateByToken(
      const ResourceManager : TJwAuthResourceManager;
      Flags : TAuthZSidContextFlags;
      const Token : TJwSecurityToken;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;


    {<B>CreateBySid</B> creates an authentication context from a Sid.

     @param ResourceManager defines a resource manager instance. Must not be nil. 
     @param Flags defines options how the context is created.
            See flags parameter on
            http://msdn2.microsoft.com/en-us/library/aa376309.aspx for more information 
     @param Sid Defines the user who is used to perform the access check with 
     @param ExpirationTime not used 
     @param DynamicGroupArgs is a used defined pointer that is passed to
        a call to TJwAuthResourceManager.OnAuthzComputeGroupsCallback 
     raises
 EJwsclWinCallFailedException:  will be raised if a call to
      AuthzInitializeContextFromSid failed.
      If GetLastError returns ERROR_INVALID_BLOCK, the reason is because
       a callback data block (TJwCallbackData) is invalid. This can only
       happen if someone changed the internal instance data.
       
      EJwsclNILParameterException: will be raised if parameter ResourceManager or
      Sid is nil 
    }
    constructor CreateBySid(
      const ResourceManager : TJwAuthResourceManager;
      Flags : TAuthZSidContextFlags;
      const Sid : TJwSecurityId;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;



    {<B>CreateAndAddSids</B> creates a new security context using an existing one.
     This function can also add additional positive and negative Sids to
     the context.

     @param ResourceManager defines a resource manager instance. Must not be nil. 
     @param Sids receives a list of Sids and its Attributes to be used as
       new groups in the security context. (like TokenGroups in a token).

       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       
        # SE_GROUP_ENABLED - adds a group to the security context.
              It will be treated as if the User has entered a group. 
        # SE_GROUP_USE_FOR_DENY_ONLY - adds a group to the security context,
          but this group is only used for deny check. All positive ACE for
          this group in a DACL are ignored. Only Deny ACEs are recognized
          and can turn off other positive ACEs of other groups. 
        
        
     @param RestrictedSids receives a list of Sids and its Attributes to be used as
       new deny only groups in the security context. (like TokenGroups in a token).

       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       
        # SE_GROUP_ENABLED - (probably) the same as parameter SID and
            attribute SE_GROUP_USE_FOR_DENY_ONLY. 
        # SE_GROUP_USE_FOR_DENY_ONLY - Results alway in Access Denied.
            However do not rely on me - maybe somebody with
            internal knowledge can comment it. 
       
      
    }
    constructor CreateAndAddSids(
      const AuthContext : TJwAuthContext;
      const Sids : TJwSecurityIdList;
      const RestrictedSids : TJwSecurityIdList);


    {<B>Destroy</B> frees all memory allocated by this context.

    }
    destructor Destroy; override;



    { <b>AccessCheck</b> does an access check of an authentication context. The
      context contains Sids, restricted sids, several security descriptors, auditing
      information and the user herself.
      
      
      Parameters
      Flags :                            from
                                         http\://msdn2.microsoft.com/en\-us/library/aa375788.aspx
                                         A DWORD value that specifies how the security
                                         descriptor is copied. This parameter can be one
                                         of the following values.<p /><p />Value \:
                                         Meaning<p />0 \: If pAuthzHandle is not NULL, a
                                         deep copy of the security descriptor is copied
                                         to the handle referenced by pAuthzHandle.<p />AUTHZ_ACCESS_CHECK_NO_DEEP_COPY_SD
                                         1 \: A deep copy of the security descriptor is
                                         not performed. The caller can use the handle
                                         for AccessCheckCached . The AccessCheck
                                         function sets this handle to a security
                                         descriptor that must remain valid during
                                         subsequent calls to AccessCheckCached.<p />
      Request :                          receives a class that contains information
                                         about the access check procedure. This instance
                                         is automatically freed if no exception was
                                         raised and Request.Shared is shOwned.<p />
      AuditInfo :                        Receives audit information handle. If this
                                         handle is zero the static audit information is
                                         read from resource manager 
      SecurityDescriptor :               defines the primary SD which is used to perfom
                                         access checking. The owner and group must not
                                         be nil; otherwise the call will fail 
      OptionalSecurityDescriptorArray :  defines additional security descriptor which
                                         are used for the access check. They are simply
                                         added at the end of the primary security
                                         descriptor (logical order). The canonical ACE
                                         order is not enforced. Deny ACE in the optional
                                         security descriptors may be useless if a
                                         positive ACE could be found in the primary
                                         security descriptor.<p />
      Reply :                            receives the results of the access check 
      AuthzHandle :                      A handle to the cached results of the access
                                         check.
      Exceptions
      EJwsclWinCallFailedException :  if a call to AuthzAccessCheck failed.
      EJwsclNILParameterException :   will be raised if parameter Request,
                                      SecurityDescriptor or Request.PrincipalSelfSid is
                                      nil                                                                                 }
    procedure AccessCheck(
      Flags : Cardinal;
      const Request : TJwAuthZAccessRequest;
      const AuditInfo : TAuthZAuditInfoHandle;
      const SecurityDescriptor : TJwSecurityDescriptor;
      const OptionalSecurityDescriptorArray : TJwSecurityDescriptorArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out Reply : TJwAuthZAccessReply;
      out AuthZHandle : TAuthZAccessCheckResultHandle
      );

    {<B>AccessCheckCached</B> does a cached access check of an authentication context.
     The context contains Sids, restricted sids,
     several security descriptors, auditing information and the user herself.

     @param AccessCheckHandle receives an handle that was previously retrieved
       by a call to AccessCheck 
     @param Flags from http://msdn2.microsoft.com/en-us/library/aa375788.aspx
        A DWORD value that specifies how the security descriptor is copied.
        This parameter can be one of the following values.
        
          # Value 	Meaning 
          # 0   If pAuthzHandle is not NULL, a deep copy of the
            security descriptor is copied to the handle referenced by pAuthzHandle. 
        # AUTHZ_ACCESS_CHECK_NO_DEEP_COPY_SD 1
          A deep copy of the security descriptor is not performed. The caller
          can use the handle for AccessCheckCached .

          The AccessCheck function sets this handle to a security
          descriptor that must remain valid during subsequent calls
          to AccessCheckCached. 
     
      
     @param Request receives a class that contains information about the
      access check procedure. This instance is automatically freed
      if no exception was raised and Request.Shared is shOwned.
       
     @param AuditInfo Receives audit information handle. If this handle is zero
       the static audit information is read from resource manager 
     @param Reply receives the results of the access check 

     raises
 EJwsclWinCallFailedException:  if a call to AuthzAccessCheck failed. 
      EJwsclNILParameterException: will be raised if parameter Request,
      SecurityDescriptor or Request.PrincipalSelfSid is nil 
    }
    procedure AccessCheckCached(
      const AccessCheckHandle : TAuthZAccessCheckResultHandle;
      Flags : Cardinal;
      const Request : TJwAuthZAccessRequest;
      const AuditInfo : TAuthZAuditInfoHandle;
      out Reply : TJwAuthZAccessReply
      );

  public
    {<B>Handle</B> contains the handle of the context.}
    property Handle : TAuthZClientContextHandle read fHandle;
    {<B>AuthResourceManager</B> points to the resource manager used by this context instance.}
    property AuthResourceManager : TJwAuthResourceManager read fAuthResourceManager;
    {<B>UserSid</B> contains the users Sid.}
    property UserSid : TJwSecurityID read fUserSid;
    {<B>GroupSids</B> contains a list of group sids.}
    property GroupSids : TJwSecurityIdList read fGroupSids;
    {<B>RestrictedSids</B> contains a list of group deny only sids.}
    property RestrictedSids : TJwSecurityIdList read fRestrictedSids;
    {<B>InfoPrivileges</B> contains a list of privileges used for access check.}
    property InfoPrivileges : TJwPrivilegeSet read fInfoPrivileges;
    {<B>ExpirationTime</B> TBD}
    property ExpirationTime : Int64 read fExpirationTime;
    {<B>ContextInfoIdentifier</B> TBD}
    property ContextInfoIdentifier : TLuid read fContextInfoIdentifier;
    {<B>ContextInfoSource</B> is not used and is always nil.}
    property ContextInfoSource : Pointer read fContextInfoSource;
    {<B>ContextInfoAll</B> is not used and is always nil.}
    property ContextInfoAll : Pointer read fContextInfoAll;
    {<B>ContextInfoAuthenticationId</B> is not used and is always nil.}
    property ContextInfoAuthenticationId : Pointer read fContextInfoAuthenticationId;


  end;

  {$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses
  JwsclSecureObjects,
  Math;
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

{$ENDIF SL_INTERFACE_SECTION}



const HEADERMAGIC = 12345678;

procedure OnInternalAuthzFreeGroupsCallback(
  const pSidAttrArray : PSID_AND_ATTRIBUTES); stdcall;
var P : PSidAndAttributesArray;
begin
  P := PSidAndAttributesArray(pSidAttrArray);
  TJwSecurityIdList.Free_PSID_Array(P);
end;
 
function OnInternalAuthzComputeGroupsCallback2(
      hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      Args : Pointer;
      var pSidAttrArray : PSidAndAttributesArray;
      var pSidCount : DWORD;
      var pRestrictedSidAttrArray : PSidAndAttributesArray;
      var pRestrictedSidCount : DWORD
      ) : Boolean; stdcall;

var CallbackData : PJwCallbackData;
    Sid, ResSid : TJwSecurityIdList;
begin
  CallbackData := PJwCallbackData(Args);
  if CallbackData.Hd <> HEADERMAGIC then
  begin
    SetLastError(ERROR_INVALID_BLOCK);
    result := false;
    exit;
  end;

  if Assigned(CallbackData) and
     Assigned(CallbackData.RM) and
     Assigned(CallbackData.RM.OnAuthzComputeGroupsCallback) then
  begin
    SetLastError(0);

    Sid    := TJwSecurityIdList.Create(true);
    ResSid := TJwSecurityIdList.Create(true);

    try
      result := true;
      CallbackData.RM.OnAuthzComputeGroupsCallback(
         hAuthzClientContext, CallbackData.UserData, Sid, ResSid);
    except
      On E : EJwsclSecurityException do
      begin
        Sid.Free;
        ResSid.Free;
        SetLastError(E.LastError);
        result := false;
        exit;
      end;
    end;

    try
      pSidAttrArray           := Sid.Create_PSID_Array;
      pSidCount := Sid.Count;
    finally
      Sid.Free;
    end;

    try
      pRestrictedSidAttrArray := ResSid.Create_PSID_Array;
      pRestrictedSidCount := ResSid.Count;
    finally
      ResSid.Free;
    end;
  end
  else
    result := false;
end;


function OnInternalAuthzAccessCheckCallback2(
  hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
  pAce : PACE_HEADER;
  pArgs : Pointer;
  var pbAceApplicable : Boolean) : Boolean; stdcall;
var CallbackData : PJwCallbackData;
    ACE : TJwSecurityAccessControlEntry;

begin
  result := false;

  CallbackData := PJwCallbackData(pArgs);
  if CallbackData.Hd <> HEADERMAGIC then
  begin
    SetLastError(ERROR_INVALID_BLOCK);
    result := false;
    exit;
  end;

  if Assigned(CallbackData) and
     Assigned(CallbackData.RM) and
     Assigned(CallbackData.RM.OnAuthzAccessCheckCallback) then
  begin
    SetLastError(0);
    try
      ACE := TJwSecurityAccessControlEntry.CreateACE(
        TJwEnumMap.ConvertAceType(pACE^.AceType));
      ACE.Header := pAce^;
    except
      On E1: EJwsclInvalidACEException do
      begin
        ACE := TJwSecurityAccessControlEntry.Create;
        ACE.Header := pAce^;
      end;
      On E2: EJwsclSecurityException do
      begin
        SetLastError(E2.LastError);
        exit;
      end;
    end;

    try
      result := true;
      SetLastError(0);

      CallbackData.RM.OnAuthzAccessCheckCallback(
        hAuthzClientContext, ACE, CallbackData.UserData, pbAceApplicable)
    except
      On E : EJwsclSecurityException do
      begin
        SetLastError(E.LastError);
        result := false;
      end;
    end;
    ACE.Free;
  end
  else
    result := false;
end;


{ TJwAuthContext }

constructor TJwAuthContext.Create;
begin
  inherited;
  fHandle := 0;
end;

constructor TJwAuthContext.CreateByToken(
  const ResourceManager: TJwAuthResourceManager;
  Flags : TAuthZSidContextFlags;
  const Token: TJwSecurityToken;
  const ExpirationTime: Int64;
  const DynamicGroupArgs: Pointer);

var luid : TLuid;
    tempToken: TJwSecurityToken;
begin
  JwRaiseOnNilParameter(ResourceManager, 'ResourceManager', 'CreateByToken',
      ClassName, RsUNAuthZCtx);
  JwRaiseOnNilParameter(Token, 'Token', 'CreateByToken',
      ClassName, RsUNAuthZCtx);
      
  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := ResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;
  
  ZeroMemory(@luid, sizeof(TLuid));

  fDynamicGroupArgs := DynamicGroupArgs;
  fAuthResourceManager := ResourceManager;

  if not Assigned(Token) then
    tempToken := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY)
  else
    tempToken := Token;

  try
    if not AuthzInitializeContextFromToken(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        tempToken.TokenHandle,//__in   HANDLE TokenHandle,
        ResourceManager.Handle, //__in   AUTHZ_RESOURCE_MANAGER_HANDLE AuthzResourceManager,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        DynamicGroupArgs,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateByToken', ClassName,
        RsUNAuthZCtx, 0, True, ['AuthzInitializeContextFromToken']);
  finally
    if not Assigned(Token) then
      tempToken.Free;
  end;

  InitProperties;
end;

constructor TJwAuthContext.CreateBySid(
  const ResourceManager : TJwAuthResourceManager;
  Flags : TAuthZSidContextFlags;
  const Sid : TJwSecurityId;
  const ExpirationTime : Int64;
  const DynamicGroupArgs : Pointer);
var luid : TLuid;

begin
  JwRaiseOnNilParameter(ResourceManager, 'ResourceManager', 'CreateBySid',
      ClassName, RsUNAuthZCtx);
  JwRaiseOnNilParameter(Sid, 'Sid', 'CreateBySid',
      ClassName, RsUNAuthZCtx);

  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := ResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;

  fAuthResourceManager := ResourceManager;
  fDynamicGroupArgs := DynamicGroupArgs;

  ZeroMemory(@luid, sizeof(TLuid));

  try
    if not AuthzInitializeContextFromSid(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        Sid.SID,//__in   PSID UserSid,
        ResourceManager.Handle, //__in   AUTHZ_RESOURCE_MANAGER_HANDLE AuthzResourceManager,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        fCallbackData,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateBySid', ClassName,
        RsUNAuthZCtx, 0, True, ['AuthzInitializeContextFromSid']);
  finally
  end;

  InitProperties;
end;

constructor TJwAuthContext.CreateByContext(
  const AuthContext: TJwAuthContext;
  Flags: TAuthZSidContextFlags;
  const ExpirationTime: Int64;
  const DynamicGroupArgs: Pointer);
var luid : TLuid;
begin
  JwRaiseOnNilParameter(AuthContext, 'AuthContext', 'CreateByContext',
      ClassName, RsUNAuthZCtx);

  fAuthResourceManager := AuthContext.AuthResourceManager;
  fDynamicGroupArgs := DynamicGroupArgs;

  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := fAuthResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;

  ZeroMemory(@luid, sizeof(TLuid));

  if not Assigned(AuthContext) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'CreateByContext',
      ClassName, RsUNAuthZCtx, 0, False, ['AuthContext']);

  try
    if not AuthzInitializeContextFromAuthzContext(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        AuthContext.Handle,//__in   AUTHZ_CLIENT_CONTEXT_HANDLE AuthzHandle,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        DynamicGroupArgs,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateByContext', ClassName,
        RsUNAuthZCtx, 0, True, ['AuthzInitializeContextFromAuthzContext']);
  finally
  end;

  InitProperties;
end;


constructor TJwAuthContext.CreateAndAddSids(
  const AuthContext : TJwAuthContext;
  const Sids : TJwSecurityIdList;
  const RestrictedSids : TJwSecurityIdList);
var luid : TLuid;
    pSid, pRSid : PSidAndAttributesArray;

    c1, c2 : Cardinal;
begin
  if not Assigned(AuthContext) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'CreateByContext',
      ClassName, RsUNAuthZCtx, 0, False, ['AuthContext']);
      
  fAuthResourceManager := AuthContext.AuthResourceManager;
  fDynamicGroupArgs := AuthContext.fDynamicGroupArgs;

  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := fAuthResourceManager;
  fCallbackData.UserData := fDynamicGroupArgs;

  ZeroMemory(@luid, sizeof(TLuid));


  pSid  := nil;
  pRSid := nil;
  c1 := 0;
  c2 := 0;

  if Assigned(Sids) then
  begin
    pSid := Sids.Create_PSID_Array;
    c1 := Sids.Count;
  end;

  if Assigned(RestrictedSids) then
  begin
    pRSid := RestrictedSids.Create_PSID_Array;
    c2 := RestrictedSids.Count;
  end;

  try
     if not AuthzAddSidsToContext(
        AuthContext.Handle,//__in   AUTHZ_CLIENT_CONTEXT_HANDLE OrigClientContext,
        @pSid[0],//__in   PSID_AND_ATTRIBUTES Sids,
        c1,//__in   DWORD SidCount,
        @pRSid[0],//__in   PSID_AND_ATTRIBUTES RestrictedSids,
        c2,//__in   DWORD RestrictedSidCount,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pNewClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateAndAddSids', ClassName,
        RsUNAuthZCtx, 0, True, ['AuthzAddSidsToContext']);
  finally
    if Assigned(Sids) then
      Sids.Free_PSID_Array(pSid);
    if Assigned(RestrictedSids) then
      RestrictedSids.Free_PSID_Array(pRSid);
  end;

  InitProperties;
end;





destructor TJwAuthContext.Destroy;
begin
  AuthzFreeContext(fHandle);
  fHandle := INVALID_HANDLE_VALUE;
  FreeMem(fCallbackData);

  FreeAndNil(fUserSid);

  FreeAndNil(fGroupSids);
  FreeAndNil(fRestrictedSids);
  FreeAndNil(fInfoPrivileges);
end;

function TJwAuthContext.GetInformationFromContext(
  InfoClass: AUTHZ_CONTEXT_INFORMATION_CLASS): Pointer;
var Size,N : Cardinal;
begin
  result := nil;
  AuthzGetInformationFromContext(fHandle,
    InfoClass, 0, @Size, result);

  GetMem(result, Size);
  ZeroMemory(result, Size);

  if not AuthzGetInformationFromContext(fHandle,
    InfoClass, Size, @N, result) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'GetInformationFromContext', ClassName,
        RsUNAuthZCtx, 0, True, ['AuthzGetInformationFromContext']);
end;

procedure TJwAuthContext.InitProperties;
var Sid : PTokenUser;
    Groups : PTokenGroups;
    Privs : PTokenPrivileges;
    I64 : ^Int64;
    Luid : PLuid;
begin
  Sid := GetInformationFromContext(AuthzContextInfoUserSid);
  try
    fUserSid := TJwSecurityId.Create(PSidAndAttributes(@Sid.User));
  finally
    FreeMem(Sid);
  end;

  Groups := GetInformationFromContext(AuthzContextInfoGroupsSids);
  try
    fGroupSids := TJwSecurityIdList.Create(true, Groups);
  finally
    FreeMem(Groups);
  end;

  Groups := GetInformationFromContext(AuthzContextInfoRestrictedSids);
  try
    fRestrictedSids := TJwSecurityIdList.Create(true, Groups);
  finally
    FreeMem(Groups);
  end;

  Privs := GetInformationFromContext(AuthzContextInfoPrivileges);
  try
    fInfoPrivileges := TJwPrivilegeSet.Create(nil, Privs);
  finally
    FreeMem(Privs);
  end;

  I64 := GetInformationFromContext(AuthzContextInfoExpirationTime);
  try
    fExpirationTime := I64^;
  finally
    FreeMem(I64);
  end;

  Luid := GetInformationFromContext(AuthzContextInfoIdentifier);
  try
    fContextInfoIdentifier := Luid^;
  finally
    FreeMem(Luid);
  end;


  fContextInfoSource := nil;
  fContextInfoAll := nil;
  fContextInfoAuthenticationId := nil;
end;


procedure TJwAuthContext.AccessCheck(Flags: Cardinal;
  const Request: TJwAuthZAccessRequest;
  const AuditInfo: TAuthZAuditInfoHandle;
  const SecurityDescriptor: TJwSecurityDescriptor;
  const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out Reply: TJwAuthZAccessReply;
  out AuthZHandle: TAuthZAccessCheckResultHandle);



var pRequest : AUTHZ_ACCESS_REQUEST;
    pSD : PSecurityDescriptor;
    pOSD : array of PSECURITY_DESCRIPTOR;

    pReply : AUTHZ_ACCESS_REPLY;
    i, idx : Integer;
    CallbackData : TJwCallbackData;

    TempSD : TJwSecurityDescriptor;
    bTempSD : Boolean;

begin
  AuthZHandle := 0;

  JwRaiseOnNilParameter(Request, 'Request', 'AccessCheck',
      ClassName, RsUNAuthZCtx);
  JwRaiseOnNilParameter(SecurityDescriptor, 'SecurityDescriptor', 'AccessCheck',
      ClassName, RsUNAuthZCtx);
  JwRaiseOnNilParameter(Request.PrincipalSelfSid, 'Request.PrincipalSelfSid', 'AccessCheck',
      ClassName, RsUNAuthZCtx);



  //check for correct object array
  if Assigned(Request.fObjectTypeArray) then
    if not JwCheckArray(Request.fObjectTypeArray,i) then
      raise EJwsclInvalidObjectArrayException.CreateFmtEx(
          RsInvalidObjectTypeList, 'AuthzAccessCheck', ClassName,
          RsUNAuthZCtx, 0, false, [i]);




  //initialize request structure
  ZeroMemory(@pRequest, sizeof(pRequest));
  //replace generic rights in Desired Access
  pRequest.DesiredAccess    := TJwSecureBaseClass.ConvertAccessMask(GenericMapping, Request.fDesiredAccess);
  pRequest.PrincipalSelfSid := Request.fPrincipalSelfSid.SID;
  if Assigned(Request.fObjectTypeArray) and (Length(Request.fObjectTypeArray) > 0) then
    pRequest.ObjectTypeList   := @Request.fObjectTypeArray[0];

  pRequest.ObjectTypeListLength := Length(Request.fObjectTypeArray);


  CallbackData.Hd := HEADERMAGIC;
  CallbackData.Context  := Self;
  CallbackData.RM := fAuthResourceManager;
  CallbackData.UserData := Request.fData;

  pRequest.OptionalArguments := @CallBackData;

  bTempSD := Assigned(GenericMapping);
  if bTempSD then
  begin
    TempSD := TJwSecurityDescriptor.Create(SecurityDescriptor);
    {AccessCheck does not resolve GENERIC_XXXX rights in ACE
    to the given ones here defined some lines later is quite useless.
      mapping := TGenericMapping(GenericMapping.GetMapping());
    See http://groups.google.de/group/microsoft.public.platformsdk.security/browse_thread/thread/2fb371cbd8ad1246/e9abf141630a0539?lnk=gst&q=accesscheck+generic#e9abf141630a0539

    So we replace the ACEs AccessMask if necessary
    }
    //replace generic rights in dacl
    TJwSecureBaseClass.ReplaceGenericRightsInDACL(GenericMapping, TempSD);
  end
  else
    TempSD := SecurityDescriptor;


  try
    //get security descriptor memory block
    pSD := TempSD.Create_SD();

    Idx := -1;
    try
      SetLength(pOSD, Length(OptionalSecurityDescriptorArray));
      for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
      begin
        try
          pOSD[i] := OptionalSecurityDescriptorArray[i].Create_SD(false);
        except
          On E : EJwsclSecurityException do
          begin
            Idx := i-1;
            raise;
          end;
        end;
      end;
    except
      //we only check for known exceptions
      On E : EJwsclSecurityException do
      begin
        //deallocate memory
        if Assigned(SecurityDescriptor) then
           TJwSecurityDescriptor.Free_SD(pSD);
        for i := 0 to Idx do
          TJwSecurityDescriptor.Free_SD(pOSD[i]);
        raise
      end;
    end;


    ZeroMemory(@pReply, sizeof(pReply));
    //make room for at least one result
    pReply.ResultListLength := pRequest.ObjectTypeListLength;
    if pRequest.ObjectTypeListLength = 0 then
      pReply.ResultListLength := 1;


    GetMem(pReply.GrantedAccessMask,
      (pReply.ResultListLength) * sizeof(ACCESS_MASK));
    ZeroMemory(pReply.GrantedAccessMask, (pReply.ResultListLength) * sizeof(ACCESS_MASK));

    GetMem(pReply.SaclEvaluationResults,
      (pReply.ResultListLength) * sizeof(DWORD));
    ZeroMemory(pReply.SaclEvaluationResults, (pReply.ResultListLength) * sizeof(DWORD));

    GetMem(pReply.Error,
      (pReply.ResultListLength) * sizeof(DWORD));
    ZeroMemory(pReply.Error, (pReply.ResultListLength) * sizeof(DWORD));


    Reply := nil;
    SetLastError(0);
    try
      try
        if not AuthzAccessCheck(
          Flags, //__in      DWORD flags,
          Handle,//__in      AUTHZ_CLIENT_CONTEXT_HANDLE AuthzClientContext,
          @pRequest,//__in      PAUTHZ_ACCESS_REQUEST pRequest,
          AuditInfo,//__in      AUTHZ_AUDIT_INFO_HANDLE AuditInfo,
          pSD,//__in      PSECURITY_DESCRIPTOR pSecurityDescriptor,
          @pOSD[0],//__in_opt  PSECURITY_DESCRIPTOR* OptionalSecurityDescriptorArray,
          Length(OptionalSecurityDescriptorArray),//__in_opt  DWORD OptionalSecurityDescriptorCount,
          @pReply,//__inout   PAUTHZ_ACCESS_REPLY pReply,
          @AuthZHandle //__out     PAUTHZ_ACCESS_CHECK_RESULTS_HANDLE pAuthzHandle
        ) then
          raise EJwsclWinCallFailedException.CreateFmtEx(
            RsWinCallFailed, 'AuthzAccessCheck', ClassName,
            RsUNAuthZCtx, 0, True, ['AuthzAccessCheck']);

        //convert reply to a class
        Reply := TJwAuthZAccessReply.Create(pReply);
      finally
        FreeMem(pReply.GrantedAccessMask);
        FreeMem(pReply.SaclEvaluationResults);
        FreeMem(pReply.Error);
    
        TJwSecurityDescriptor.Free_SD(pSD);


       // FreeMem(CallBackData);

        for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
        begin
          TJwSecurityDescriptor.Free_SD(pOSD[i]);
        end;
        pOSD := nil;
      end;
    except
      on E : EJwsclWinCallFailedException do
      begin
        FreeAndNil(Reply);
        raise;
      end;
    end;
  finally
    if bTempSD then
      TempSD.Free;
  end;
end;

procedure TJwAuthContext.AccessCheckCached(
  const AccessCheckHandle : TAuthZAccessCheckResultHandle;
  Flags: Cardinal;
  const Request: TJwAuthZAccessRequest;
  const AuditInfo: TAuthZAuditInfoHandle; out Reply: TJwAuthZAccessReply);
var pRequest : AUTHZ_ACCESS_REQUEST;
    pReply : AUTHZ_ACCESS_REPLY;
    i : Integer;
begin
  JwRaiseOnNilParameter(Request, 'Request', 'AccessCheck',
      ClassName, RsUNAuthZCtx);
  JwRaiseOnNilParameter(Request.PrincipalSelfSid, 'Request.PrincipalSelfSid', 'AccessCheck',
      ClassName, RsUNAuthZCtx);


  //check for correct object array
  if Assigned(Request.fObjectTypeArray) then
    if not JwCheckArray(Request.fObjectTypeArray,i) then
      raise EJwsclInvalidObjectArrayException.CreateFmtEx(
          RsInvalidObjectTypeList, 'AuthzAccessCheck', ClassName,
          RsUNAuthZCtx, 0, false, [i]);


  //initialize request structure
  ZeroMemory(@pRequest, sizeof(pRequest));
  pRequest.DesiredAccess    := Request.fDesiredAccess;
  pRequest.PrincipalSelfSid := Request.fPrincipalSelfSid.SID;
  if Assigned(Request.fObjectTypeArray) and (Length(Request.fObjectTypeArray) > 0) then
    pRequest.ObjectTypeList   := @Request.fObjectTypeArray[0];

  pRequest.ObjectTypeListLength := Length(Request.fObjectTypeArray);
  pRequest.OptionalArguments := Request.fData;


  ZeroMemory(@pReply, sizeof(pReply));
  //make room for at least one result
  pReply.ResultListLength := pRequest.ObjectTypeListLength;
  if pRequest.ObjectTypeListLength = 0 then
    pReply.ResultListLength := 1;

  //create arrays  
  GetMem(pReply.GrantedAccessMask,
    (pReply.ResultListLength) * sizeof(ACCESS_MASK));
  ZeroMemory(pReply.GrantedAccessMask, (pReply.ResultListLength) * sizeof(ACCESS_MASK));

  GetMem(pReply.SaclEvaluationResults,
    (pReply.ResultListLength) * sizeof(DWORD));
  ZeroMemory(pReply.SaclEvaluationResults, (pReply.ResultListLength) * sizeof(DWORD));

  GetMem(pReply.Error,
    (pReply.ResultListLength) * sizeof(DWORD));
  ZeroMemory(pReply.Error, (pReply.ResultListLength) * sizeof(DWORD));


  Reply := nil;
  SetLastError(0);

  try
    try
      if not AuthzCachedAccessCheck(
        Flags, //__in   DWORD Flags,
        AccessCheckHandle, //__in   AUTHZ_ACCESS_CHECK_RESULTS_HANDLE AuthzHandle,
        @pRequest, //__in   PAUTHZ_ACCESS_REQUEST pRequest,
        AuditInfo, //__in   AUTHZ_AUDIT_EVENT_HANDLE AuditInfo,
        @pReply //__out  PAUTHZ_ACCESS_REPLY pReply
       ) then
         raise EJwsclWinCallFailedException.CreateFmtEx(
            RsWinCallFailed, 'AccessCheckCached', ClassName,
            RsUNAuthZCtx, 0, True, ['AuthzCachedAccessCheck']);

      Reply := TJwAuthZAccessReply.Create(pReply);
    finally
      FreeMem(pReply.GrantedAccessMask);
      FreeMem(pReply.SaclEvaluationResults);
      FreeMem(pReply.Error);
    end;
  except
    on E : EJwsclWinCallFailedException do
    begin
      FreeAndNil(Reply);
      raise;
    end;
  end;
end;

{ TJwAuthResourceManager }

constructor TJwAuthResourceManager.Create(const Name: WideString;
  Flags: TJwAuthZResourceManagerFlags;
  const OnAuthzComputeGroupsCallback: TJwOnAuthzComputeGroupsCallback;
  const OnAuthzAccessCheckCallback: TJwOnAuthzAccessCheckCallback);
var lpwName : PWideChar;
    p1, p2, p3 : TMethod;
begin
  if Length(Name) = 0 then
    lpwName := nil
  else
    lpwName := @Name;

  ZeroMemory(@p1,sizeof(TMethod));
  ZeroMemory(@p2,sizeof(TMethod));
  ZeroMemory(@p3,sizeof(TMethod));

  if (authRM_Default in Flags) and
    (not JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Enabled)) then
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
        RsSecureObjectsPrivilegeSecurityMissing2, 'Create',
        ClassName, RsUNAuthZCtx, 0, False, []);

  if Flags = [] then
    Flags := [authRM_NoAudit];


{$IFDEF DEBUG}
  fOnAuthzComputeGroupsCallback := OnInternalAuthzComputeGroupsCallback;
  fOnAuthzComputeGroupsCallbackDebug := OnAuthzComputeGroupsCallback;
{$ELSE}
  fOnAuthzComputeGroupsCallback := OnAuthzComputeGroupsCallback;
{$ENDIF}


{$IFDEF DEBUG}
  fOnAuthzAccessCheckCallback := OnInternalAuthzAccessCheckCallback;
  fOnAuthzAccessCheckCallbackDebug := OnAuthzAccessCheckCallback;
{$ELSE}
  fOnAuthzAccessCheckCallback := OnAuthzAccessCheckCallback;
{$ENDIF}
  

  if not AuthzInitializeResourceManager(
    TJwEnumMap.ConvertAuthZResourceManager(Flags),//__in   DWORD flags,
    @OnInternalAuthzAccessCheckCallback2,//__in   PFN_AUTHZ_DYNAMIC_ACCESS_CHECK pfnAccessCheck,
    @OnInternalAuthzComputeGroupsCallback2,//__in   PFN_AUTHZ_COMPUTE_DYNAMIC_GROUPS pfnComputeDynamicGroups,
    @OnInternalAuthzFreeGroupsCallback,//__in   PFN_AUTHZ_FREE_DYNAMIC_GROUPS pfnFreeDynamicGroups,
    lpwName,//__in   PCWSTR ResourceManagerName,
    @fHandle//__out  PAUTHZ_RESOURCE_MANAGER_HANDLE pAuthzResourceManager
  ) then
   raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'Create', ClassName,
      RsUNAuthZCtx, 0, True, ['AuthzInitializeResourceManager']);
end;

destructor TJwAuthResourceManager.Destroy;
begin
  AuthzFreeResourceManager(Handle);
  fHandle := INVALID_HANDLE_VALUE;
end;

procedure TJwAuthResourceManager.OnInternalAuthzAccessCheckCallback(
      AuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      const Ace : TJwSecurityAccessControlEntry;
      Args : Pointer;
      var AceApplicable : Boolean);
begin
  //AceApplicable := true;
{$IFDEF DEBUG}
  if Assigned(fOnAuthzAccessCheckCallbackDebug) then
    fOnAuthzAccessCheckCallbackDebug(AuthzClientContext,
      Ace, Args, AceApplicable);
//  AceApplicable := true;
{$ENDIF DEBUG}
end;

procedure TJwAuthResourceManager.OnInternalAuthzComputeGroupsCallback(
        AuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
        Args : Pointer;
        const SidAttrArray : TJwSecurityIdList;
        const RestrictedSidAttrArray : TJwSecurityIdList
       );
begin
{$IFDEF DEBUG}
  if Assigned(fOnAuthzComputeGroupsCallbackDebug) then
    fOnAuthzComputeGroupsCallbackDebug(
      AuthzClientContext, Args, SidAttrArray, RestrictedSidAttrArray);
{$ENDIF DEBUG}
  //SidAttrArray.Add(TJwSecurityId.Create('','Administrator'));
  //RestrictedSidAttrArray.Add(TJwSecurityId.Create('','Administrator'));

end;



{ TJwAuthZAccessReply }

constructor TJwAuthZAccessReply.Create(
  const ReplyStruct: TAuthzAccessReply);
var i : Integer;
begin
  SetLength(fGrantedAccessMask, ReplyStruct.ResultListLength);
  SetLength(fSaclEvaluationResults, ReplyStruct.ResultListLength);
  SetLength(fError, ReplyStruct.ResultListLength);
  SetLength(fErrorByType, ReplyStruct.ResultListLength);

  for i := 0 to ReplyStruct.ResultListLength-1 do
  begin
    fGrantedAccessMask[i] := TJwAccessMaskArray(ReplyStruct.GrantedAccessMask)[i];
    fSaclEvaluationResults[i] := TJwCardinalArray(ReplyStruct.SaclEvaluationResults)[i];
    fError[i] := TJwCardinalArray(ReplyStruct.Error)[i];

    fErrorByType[i] := TJwEnumMap.ConvertReplyErrorEnum(fError[i]);
  end;
end;

{ TJwAuthZAccessRequest }

constructor TJwAuthZAccessRequest.Create(DesiredAccess: TJwAccessMask;
  PrincipalSelfSid: TJwSecurityID; ObjectTypeArray: TJwObjectTypeArray;
  Data: Pointer;Shared : TJwSharedHandle);
begin
  JwRaiseOnNilParameter(PrincipalSelfSid, 'PrincipalSelfSid', 'Create',
      ClassName, RsUNAuthZCtx);

  fDesiredAccess := DesiredAccess;
  fPrincipalSelfSid := TJwSecurityId.Create(PrincipalSelfSid);
  fObjectTypeArray := ObjectTypeArray;
  fData := Data;
  fShared := Shared;
end;

destructor TJwAuthZAccessRequest.Destroy;
begin
  FreeAndNil(fPrincipalSelfSid);
  fObjectTypeArray := nil;
end;

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}