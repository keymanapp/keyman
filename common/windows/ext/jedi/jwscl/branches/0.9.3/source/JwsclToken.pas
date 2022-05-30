{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides access to security token objects

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
The Original Code is JwsclToken.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

See Jwscl.inc for Vista related stuff!

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclToken;

{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Contnrs, Classes,
  JwaWindows,
  JwsclResource, JwsclUtils, 
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl,
  JwsclDescriptor, JwsclEnumerations,
  JwsclVersion, JwsclConstants,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
const
  ExplorerProcessName = 'EXPLORER.EXE';

type
  TJwPrivilegeSet = class;
  TJwPrivilege    = class;
  TJwSecurityTokenStatistics = class;



     {<B>TJwSecurityToken</B> administers a token (impersonated or primary)
      All token information are retrieved dynamically.
      The token handle is closed on instance destroying if Shared is set to false.

      A token is a security card that gives the logged on user the right to do things (like start processes a.s.o).
      Without a token the user would have to prove his/her security state to the system every time.

      The system creates a process token for the process that it can use to get its security constraints.
      A process token is also called primary token. The process can create threads and decrement their security
      state by copying the primary token and remove privileges and/or add restrictions. However a thread
      cannot use a process/primary token. Instead it can only use a impersonated token. So the token must be converted
      to a impersonation token. However the first process token cannot be converted. It must be duplicated and then
      converted. After that the thread can call SetThreadToken to change its security context.

      <B>TJwSecurityToken</B> does not support some of the values defined in the MSDN http://msdn2.microsoft.com/en-us/library/aa379626.aspx

      <B>TJwSecurityToken</B> administers a token (impersonated or not) 
      }

  TJwSecurityToken = class(TInterfacedObject, IJwBase)
  private
       {fPrivelegesList administers all created privileges list by GetTokenPrivileges .
        If a list was not freed by user, it will be freed on the instance ending.
        }
    fPrivelegesList: TObjectList;
    fStackPrivilegesList: TObjectList;
    //internal token handle
    fTokenHandle: TJwTokenHandle;
    //shared status
    fShared:      boolean;

    fClassHash : Integer;

    fAccessMask:  TJwAccessMask;
       {<B>Done</B> is called by Destroy to free all things that were allocated by Create();
        Only objects are destroyed!
       }
    procedure Done; virtual;
     {<B>PopPrivileges</B> restores privileges from a nested call to PushPrivileges.
      TODO: to test
      @return Returns the count of privilege stack elements.
     }
    //function PopPrivileges: cardinal;
    {<B>PushPrivileges</B> saves the current privilege enabled states in a stack.
        They can be restored with a correctly nested call to PopPrivileges.
               TODO: to test
        @return
       }
    //function PushPrivileges: cardinal;

    {<B>RaiseOnInvalidPrimaryToken</B> checks for whether user is SYSTEM and raises exception if not.
     @param MethodName defines the method name of the caller
     raises
 EJwsclInvalidPrimaryToken:  if primary user token is not SYSTEM. }
    procedure RaiseOnInvalidPrimaryToken(MethodName: TJwString);
  public
    {see TokenType }
    function GetTokenType: TOKEN_TYPE; virtual;

        {<B>GetTokenInformationLength</B> returns the needed memory for a token information.
         @param hTokenHandle receives the token handle 
         @param aTokenInformationClass receives token class
              See GetTokenInformationLength GetTokenInformationLength  for class types. 
         @return Returns the length of needed memory buffer. If the call failed zero will be returned ;
         }
    function GetTokenInformationLength(hTokenHandle: TJwTokenHandle;
      aTokenInformationClass: TTokenInformationClass): cardinal; virtual;


        {<B>GetTokenInformation</B> returns a buffer filled with token information.
        @param hTokenHandle receives the token handle 
         @param aTokenInformationClass receives token class.
           Legend : o  defines already implemented structures.
              
              #  TokenDefaultDacl   The buffer receives a TOKEN_DEFAULT_DACL structure containing the default DACL for newly created objects.
              #  o TokenGroups   The buffer receives a TOKEN_GROUPS structure containing the group accounts associated with the token.
              #  TokenGroupsAndPrivileges   The buffer receives a TOKEN_GROUPS_AND_PRIVILEGES structure containing the user SID, the group accounts, the restricted SIDs, and the authentication ID associated with the token.
              #  o TokenImpersonationLevel   The buffer receives a SECURITY_IMPERSONATION_LEVEL value indicating the impersonation level of the token. If the access token is not an impersonation token, the function fails.
              #  o TokenOrigin   The buffer receives a TOKEN_ORIGIN value that contains information about the logon session ID.
              #  o TokenOwner   The buffer receives a TOKEN_OWNER structure containing the default owner SID for newly created objects.
              #  o TokenPrimaryGroup   The buffer receives a TOKEN_PRIMARY_GROUP structure containing the default primary group SID for newly created objects.
              #  o TokenPrivileges   The buffer receives a TOKEN_PRIVILEGES structure containing the token's privileges.
              #  o TokenRestrictedSids   The buffer receives a TOKEN_GROUPS structure containing the list of restricting SIDs in a restricted token.
              #  TokenSandBoxInert   The buffer receives a DWORD value that is nonzero if the token includes the SANDBOX_INERT flag.
              #  o TokenSessionId   The buffer receives a DWORD value that contains the Terminal Services session identifier associated with the token. If the token is associated with the Terminal Server console session, the session identifier is zero. A nonzero session identifier indicates a Terminal Services client session. In a non-Terminal Services environment, the session identifier is zero.
              #  o TokenSource   The buffer receives a TOKEN_SOURCE structure containing the source of the token. TOKEN_QUERY_SOURCE access is needed to retrieve this information.
              #  TokenStatistics   The buffer receives a TOKEN_STATISTICS structure containing various token statistics.
              #  o TokenType   The buffer receives a TOKEN_TYPE value indicating whether the token is a primary or impersonation token.
              #  o TokenUser   The buffer receives a TOKEN_USER structure containing the token's user account.
               
         @param TokenInformation contains the requested information. You must convert the type to the appropiate token type information class. 

        raises
 EJwsclTokenInformationException:  is raised if a call to GetTokenInformation failed.
         EJwsclAccessTypeException: is raised if the given token access rights is not enough to do the necessary work.
          In this case the token instance must be reopened with sufficient rights.
         EJwsclInvalidTokenHandle: is raised if the token handle is invalid. Not opened or closed already.
         EJwsclNotEnoughMemory: is raised if a call to HeapAlloc failed because of not enough space.
         }
    procedure GetTokenInformation(hTokenHandle: TJwTokenHandle;
      TokenInformationClass: TTokenInformationClass;
      out TokenInformation: Pointer); virtual;

    {see property ImpersonationLevel }
    function GetImpersonationLevel: TSecurityImpersonationLevel; virtual;


    function GetTokenUser: TJwSecurityId; virtual;
    function GetUserName : TJwString; virtual;

    procedure GetTokenSource(out SourceName: ShortString;
      out SourceLUID: TLuid);
      overload; virtual;
    function GetTokenSource: TTokenSource; overload; virtual;

    function GetTokenGroups: TJwSecurityIdList; virtual;
    procedure SetTokenGroups(List: TJwSecurityIdList); virtual;

    function GetTokenGroupsEx: PTokenGroups;

    function GetTokenGroupsAttributesInt(Index: integer): TJwSidAttributeSet;
    procedure SetTokenGroupsAttributesInt(Index: integer;
      Attributes: TJwSidAttributeSet);

    function GetTokenGroupsAttributesSid(Sid: TJwSecurityId):
      TJwSidAttributeSet;
    procedure SetTokenGroupsAttributesSid(Sid: TJwSecurityId;
      Attributes: TJwSidAttributeSet);


    function GetTokenRestrictedSids: TJwSecurityIdList; virtual;

    function IsTokenType(index : Integer) : Boolean; virtual;


    function GetTokenDefaultDacl: TJwDAccessControlList;

    {This function is not implemented}
    procedure SetTokenDefaultDacl(const aDefaultDCAL: TJwDAccessControlList);
      virtual; //TOKEN_ADJUST_DEFAULT

    function GetTokenOrigin: TLuid; virtual;
    procedure SetTokenOrigin(const anOrigin: TLuid);
      virtual; //SE_TCB_NAME

    function GetTokenOwner: TJwSecurityId; virtual;
    procedure SetTokenOwner(const anOwner: TJwSecurityId);
      virtual; //TOKEN_ADJUST_DEFAULT

    function GetPrimaryGroup: TJwSecurityId; virtual;
    procedure SetPrimaryGroup(const PrimGroup: TJwSecurityId);
      virtual; //TOKEN_ADJUST_DEFAULT


    function GetTokenSessionId: cardinal; virtual;
    procedure SetTokenSessionId(const SessionID: cardinal);
      virtual; //SE_TCB_NAME


    function GetPrivilegeEnabled(Name: TJwString): boolean; virtual;
    procedure SetPrivilegeEnabled(Name: TJwString; En: boolean); virtual;

    function GetPrivilegeAvailable(Name: TJwString): boolean;

    function GetIntegrityLevel: TJwSecurityIdList; virtual;
    function GetIntegrityLevelType: TJwIntegrityLabelType; virtual;
    procedure SetIntegrityLevelType(const LevelType: TJwIntegrityLabelType);
      virtual;

    function GetLinkedToken: TJwSecurityToken; virtual;

    function GetRunElevation: cardinal; virtual;
    function GetElevationType: TTokenElevationType; virtual;

    function GetVirtualizationAllowed: boolean; virtual;
    function GetVirtualizationEnabled: boolean; virtual;

    function GetMandatoryPolicy: TJwTokenMandatoryPolicies; virtual;

    function RetrieveSpecificAccessRights(const AccessMask : TJwAccessMask) : TJwAccessMask; virtual;
  protected
        {<B>CheckTokenHandle</B> checks the TokenHandle of this instance and raises EJwsclInvalidTokenHandle if the token is invalid; otherwise it does nothing
         @param aSourceProc defines the caller method  
         raises
 EJwsclInvalidTokenHandle:  is raised if the property TokenHandle is invalid. }
    procedure CheckTokenHandle(sSourceProc: TJwString); virtual;

        {<B>CheckTokenAccessType</B> checks if the given token was opened with the desired access mask.
         If the desired access is not included in the token access mask an exception will be raised; otherwise nothing happens.
         @param aDesiredAccessMask contains access mask that must be included in the token access mask to succeed. 
         @param StringMask contains the desired access mask in a human readable format. This string will be display in the exception description. 
         @param SourceProc contains the method name of the caller method. 
         raises
 EJwsclAccessTypeException:  will be raised if a desired access flag could not be found in the access mask of the token 
         }
    procedure CheckTokenAccessType(aDesiredAccessMask: TJwAccessMask;
      StringMask, SourceProc: TJwString);

        {<B>CheckTokenPrivileges</B> checks if the token has all privileges that was given in the array.
        It does not matter whether the privilege is en- or disabled. It simply must exist.
        The privilege names are compared case sensitive. 
        @param Privileges contains all privileges names that the token must held 
        raises
 EJwsclPrivilegeCheckException:  will be raised if one privilege was not found
        }

    procedure CheckTokenPrivileges(Privileges: array of TJwString);

        {<B>IsPrivilegeAvailable</B> checks if one token holds a privilege.
         The privilege names are compared case sensitive.
         @param Priv contains the privilege name to be checked for 
         @return <B>IsPrivilegeAvailable</B> returns true if the privilege could be found in the tokens privileges; otherwise false 
        }
    function IsPrivilegeAvailable(Priv: TJwString): boolean;

    function GetIsRestricted: boolean;

    {<B>GetMaximumAllowed</B> returns the maximum access rights this token can be opened with.
     This function can fail with an exception if the token DACL could not be read
     and the token access rights does not contain TOKEN_READ, TOKEN_QUERY and TOKEN_DUPLICATE.
     If the token DACL can be read the token access rights do not need TOKEN_DUPLICATE.
     This error happens sometimes
    }
    function GetMaximumAllowed: TAccessMask;


  public
    {<B>Create</B> TBD.
     Using this constructor is stronly discouraged!
     It initialises all properties and sets
      TokenHandle to 0 and Shared to true
      }
    constructor Create; overload;


    destructor Destroy; override;

        {
        CreateTokenByProcess creates a new instances and opens a process token.

        To open a token of another session rather than the current session
        the current process token must be the SYSTEM token. Only the access right
        TOKEN_READ can be used with an admin token.

        If parameter aDesiredAccess is MAXIMUM_ALLOWED and the right READ_CONTROL
        is not granted the value of the property AccessMask is zero. Otherwise
        it contains all granted rights for the token.

        If you do not specify TOKEN_DUPLICATE, you will get direct access to
        the target process' token. Therefore you can change the behaviour of
        the process by changing enabled privileges. This does not affect
        an impersonation of the token because for this action is has to be duplicated.

        @param aProcessHandle Receives a process handle which is used to get the process token. The handle can be zero (0) to use the current process handle of the caller 
        @param aDesiredAccess Receives the desired access for this token. The access types can be get from the following list. Access flags must be concatenated with or operator.
              Can be MAXIMUM_ALLOWED to get maximum access. 
        @param Duplicate Defines whether the token of the Processhandle should be spawned into this process.
            If this parameter is true the token handle is opened and duplicated. The new handle may have more
            rights for the current process. This is especially useful if another process is defined in aProcessHandle because
            the handle to this process token may be restricted.

        If you want to use DuplicateToken or creating an impersonated token (by ConvertToImpersonatedToken) you must specific TOKEN_DUPLICATE.

        Access Rights for Access-Token Objects:
                from http://msdn2.microsoft.com/en-us/library/aa374905.aspx:
        
        #  TOKEN_ADJUST_DEFAULT   Required to change the default owner, primary group, or DACL of an access token.
        #  TOKEN_ADJUST_GROUPS   Required to adjust the attributes of the groups in an access token.
        #  TOKEN_ADJUST_PRIVILEGES   Required to enable or disable the privileges in an access token.
        #  TOKEN_ADJUST_SESSIONID   Required to adjust the session ID of an access token. The SE_TCB_NAME privilege is required.
        #  TOKEN_ASSIGN_PRIMARY   Required to attach a primary token to a process. The SE_ASSIGNPRIMARYTOKEN_NAME privilege is also required to accomplish this task.
        #  TOKEN_DUPLICATE   Required to duplicate an access token.
        #  TOKEN_EXECUTE   Combines STANDARD_RIGHTS_EXECUTE and TOKEN_IMPERSONATE.
        #  TOKEN_IMPERSONATE   Required to attach an impersonation access token to a process.
        #  TOKEN_QUERY   Required to query an access token.
        #  TOKEN_QUERY_SOURCE   Required to query the source of an access token.
        #  TOKEN_READ   Combines STANDARD_RIGHTS_READ and TOKEN_QUERY.
        #  TOKEN_WRITE   Combines STANDARD_RIGHTS_WRITE, TOKEN_ADJUST_PRIVILEGES, TOKEN_ADJUST_GROUPS, and TOKEN_ADJUST_DEFAULT.
        #  TOKEN_ALL_ACCESS
        

        Standard Access Rights:
                from http://msdn2.microsoft.com/en-us/library/aa379607.aspx
        
        #  DELETE   The right to delete the object.
        #  READ_CONTROL   The right to read the information in the object's security descriptor, not including the information in the SACL.
        #  SYNCHRONIZE   The right to use the object for synchronization. This enables a thread to wait until the object is in the signaled state. Some object types do not support this access right.
        #  WRITE_DAC   The right to modify the DACL in the object's security descriptor.
        #  WRITE_OWNER   The right to change the owner in the object's security descriptor.
        
        The Windows API also defines the following combinations of the standard access rights constants.
        
        #  Constant   Meaning
        #  STANDARD_RIGHTS_ALL   Combines DELETE, READ_CONTROL, WRITE_DAC, WRITE_OWNER, and SYNCHRONIZE access.
        #  STANDARD_RIGHTS_EXECUTE   Currently defined to equal READ_CONTROL.
        #  STANDARD_RIGHTS_READ   Currently defined to equal READ_CONTROL.
        #  STANDARD_RIGHTS_REQUIRED   Combines DELETE, READ_CONTROL, WRITE_DAC, and WRITE_OWNER access.
        #  STANDARD_RIGHTS_WRITE
        
        )

        raises
 EJwsclOpenProcessTokenException:  If the token could not be opened 
        }
    constructor CreateTokenByProcess(const aProcessHandle: TJwProcessHandle;
      const aDesiredAccess: TJwAccessMask; const Duplicate : Boolean = false); virtual;

    {<B>CreateTokenByProcessId</B> retrieves the token by using a ProcessID.
     The token of the given process will be duplicated into the current process
     so maximum access is granted.

     If the right TOKEN_DUPLICATE is set in parameter DesiredAccess, the token
     is duplicated. With this option, in special situations, the new current
     process can have more access rights on the token than the process specified
     by ProcessID.
     If you do not specify TOKEN_DUPLICATE, you will get direct access to
     the target process' token. Therefore you can change the behaviour of
     the process by changing enabled privileges. This does not affect
        an impersonation of the token because for this action is has to be duplicated.

     <B>CreateTokenByProcessId</B> tries to use debug privilege to open any process.

     @param ProcessID defines a process ID 
     @param DesiredAccess Receives the desired access for this token. The access types can be get from the list written at CreateTokenByProcess.
        Access flags must be concatenated with or operator. Can be MAXIMUM_ALLOWED to get maximum access.

     raises
 EJwsclOpenProcessTokenException:  If the token could not be opened 
      EJwsclWinCallFailedException: will be raised if the process could not be opened
      to retrieve the token 
    }
    constructor CreateTokenByProcessId(const ProcessID: DWORD;
      const DesiredAccess: TJwAccessMask); virtual;

        {CreateTokenByThread creates a new class instance and opens a thread token if available; otherwise it fails.

        @param aThreadHandle Receives a thread handle which is used to get 
				the process token. The handle can be zero (0) to use the current 
				process handle of the caller
        @param aDesiredAccess Receives the desired access for this token. The access types can be get 
			from the following list. Access flags must be concatenated with or operator.
			If you want to use DuplicateToken or creating an impersonated token (by ConvertToImpersonatedToken) you must specific TOKEN_DUPLICATE.
			See CreateTokenByProcess for a list of access rights.
        @param anOpenAsSelf Indicates whether the access check is to be made against the security context of the thread calling the CreateTokenByThread function or against the
                                security context of the process for the calling thread

        raises
 EJwsclNoThreadTokenAvailable:  will be raised if you try to call <B>CreateTokenByThread</B> in a process rather than thread 
         EJwsclOpenThreadTokenException: will be raised if the threak token could not be opened 

        }
    constructor CreateTokenByThread(const aThreadHandle: TJwThreadHandle;
      const aDesiredAccess: TJwAccessMask;
      const anOpenAsSelf: boolean); virtual;

        {<B>CreateTokenEffective</B> opens a token of the current thread or process. If it can't open the thread token it opens the process token instead.
        @param aDesiredAccess Receives the desired access for this token. The access types can be get from the following list. Access flags must be concatenated with or operator.
        If you want to use DuplicateToken or creating an impersonated token (by ConvertToImpersonatedToken) you must specific TOKEN_DUPLICATE.

        @param aDesiredAccess The desired access rights of the new token.
           It can be MAXIMUM_ALLOWED to get the maximum possible access.
           See CreateTokenByProcess  for a list of access rights.
            
       }
    constructor CreateTokenEffective(const aDesiredAccess: TJwAccessMask);
      virtual;

    {<B>CreateDuplicateExistingToken</B> duplicates an existing token. The token will be a primary one.
     You cannot use the class to adapt an existing token, because the access mask of the token is unkown. (AccessCheck not implemented yet)
     The token needs the TOKEN_DUPLICATE access type.

     New: <B>CreateDuplicateExistingToken</B> creates in every case a second handle. Shared will be set to false so the handle is closed
      if instance is freed.

     @param aTokenHandle The token handle to be copied.
     @param aDesiredAccess The desired access rights of the new token.
       It can be MAXIMUM_ALLOWED to get the maximum possible access.

     @param UseDuplicateExistingToken For C++ compability only. If you are using C++ and want to use this constructor instead of Create.
          Set this parameter to true of false. This parameter is ignored!

     raises
       EJwsclSecurityException See ConvertToImpersonatedToken and ConvertToPrimaryToken for possible exceptions.
     }
    constructor CreateDuplicateExistingToken(
      const aTokenHandle: TJwTokenHandle; const aDesiredAccess: TJwAccessMask;
      UseDuplicateExistingToken: boolean = False); overload; virtual;

    {<B>CreateDuplicateExistingToken</B> duplicates an existing token.  The token will be a primary one.
     You cannot use the class to adapt an existing token, because the access mask of the token is unkown. (AccessCheck not implemented yet)
     The token needs the TOKEN_DUPLICATE access type.

     New: <B>CreateDuplicateExistingToken</B> creates in every case a second handle. Shared will be set to false so the handle is closed
      if instance is freed.

     @param Token The token instance to be copied.
     @param DesiredAccess The desired access rights of the new token.
       It can be MAXIMUM_ALLOWED to get the maximum possible access.

     @param UseDuplicateExistingToken For C++ compability only. If you are using C++ and want to use this constructor instead of Create.
          Set this parameter to true of false. This parameter is ignored!

     raises
       EJwsclNILParameterException This exception is raised if if parameter Token is nil.
     }
    constructor CreateDuplicateExistingToken(
      const Token: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
      UseDuplicateExistingToken: boolean = False); overload; virtual;


    {<B>Create</B> creates a token instance using an already existing token handle.
     It can be choosen whether the token instance will close the handle or not.

     @param aTokeHandle defines the handle to the token.
       <B>Create</B> does not do a validity check on the handle!
     @param ShareTokenHandle defines whether the instance ought to close the handle
      or not 
     @param aDesiredAccess defines the access to the token handle.
       MAXIMUM_ALLOWED can be used to get the maximum access allowed. }
    constructor Create(const aTokenHandle: TJwTokenHandle;
      const ShareTokenHandle: TJwSharedHandle;
      const aDesiredAccess: TJwAccessMask);
      overload; virtual;

        {<B>CreateWTSQueryUserToken</B> opens a token of a logged on user.

         This constructor is only present on Windows XP/2003 or higher systems.
         This call fails if the thread does not have system privileges (belong to system).
         Enable SE_TCB_NAME privilege for none system principals.

         This token can be used to get a token from the specified user in a
         terminal session (also Fast User Switching). For example: This token is necessary
         to call CreateProcessAsUser to lunch a process in the given terminal session.

         @param SessionID defines the session which is used to obtain the token.
               If set to INVALID_HANDLE_VALUE, the function does the following steps:
               
                1. Try to open the token of the current console session. Using WtsGetActiveConsoleSessionID to obtain the session ID. 
                2. Try to open the token of the current session using the session ID WTS_CURRENT_SESSION 
                
               If it fails an exception is raised. 

         raises
 EJwsclUnsupportedWindowsVersionException:  is raised if the Windows System does not have WTS function support 
          EJwsclPrivilegeCheckException: is raised if the privilege SE_TCB_NAME is not held. 
          EJwsclWinCallFailedException: if a call to WTSQueryUserToken failed 
         }
    constructor CreateWTSQueryUserToken(SessionID: TJwSessionId = INVALID_HANDLE_VALUE);
      overload; virtual;

    {<B>CreateWTSQueryUserTokenEx</B> opens a token of a logged on user on a local or remote server.

     This constructor can be used in Windows 2000 Terminal Server in contrary
     to CreateWTSQueryUserToken.

     @param Server defines the Terminal Server where this function will
      be processed. Use nil to use current server.
      This parameter receives an instance of a TJwTerminalServer object or derivat.
     @param SessionID defines the session which is used to obtain the token.
         If set to INVALID_HANDLE_VALUE, the function does the following steps :

          1. Try to open the token of the current console session. Using WtsGetActiveConsoleSessionID to obtain the session ID.
          2. Try to open the token of the current session using the session ID WTS_CURRENT_SESSION

         If it fails an exception is raised.

     raises
      EInvalidCast: This exception will be raised if a given instance to parameter Server
        is not a TJwTerminalServer object or a derivat.
      EJwsclTerminalServiceNecessary:  will be raised if the no terminal
        service is running
      EJwsclInvalidPrimaryToken: will be raised if the process token
      is not a SYSTEM user token 
      EJwsclPrivilegeCheckException: will be raised if the privilege
       SE_TCB_NAME is not available 

      EJwsclWinCallFailedException: will be raised if
      a call to WinStationQueryUserToken failed 

     }
    constructor CreateWTSQueryUserTokenEx(const Server: TObject;
      SessionID: cardinal); overload; virtual;


    {<B>CreateCompatibilityQueryUserToken</B> is a compatibility constructor for CreateWTSQueryUserToken which does
     not work in Windows 2000 (only Terminal Server).
     It creates a token of the current logged on user.

     This constructor seeks a process of the user and gets its token.
     It only works in the same (terminal) session of the process.

     @param DesiredAccess defines the desired access to the token
     @param ProcessName defines which process is used to get the token of the user.
       The name must match exactly but can ignore case sensitivity.)

     raises
 EJwsclProcessNotFound:  will be raised if process handle given in parameter
       ProcessName could not be retrieved. 
      EJwsclWinCallFailedException: will be raised if the process handle of the found
       process could not be opened 
      EJwsclSecurityException: Several exceptions can be raised by used methods:
       
      Remarks
           This constructor creates a token from the first explorer.exe it finds.
           In this way any user explorer process can be returned - not only the
           console user. To avoid this problem call this constructor
           using true as parameter. Only apply FALSE if the current system
           does not support multiple users (like Win2000 Workstation)
       }

    constructor CreateCompatibilityQueryUserToken(
      const DesiredAccess: TJwAccessMask;
      const ProcessName: TJwString = ExplorerProcessName);

      (*
       <B>CreateNewToken</B> forges a new token using ZwCreateToken.
       This function can only be called successfully when the
       CREATE_TOKEN privilege is available and the current process
       it the SYSTEM user.
       If the current process is a SYSTEM process but the CREATE_TOKEN
       privilege is missing, retrieve the token from the csrss.exe (session 0)
       impersonate it and call CreateNewToken. In this way, no restart of Windows
       is necessary which would be because you had to add the privilege to
       the SYSTEM account.

     raises
 EJwsclPrivilegeException:  if SE_CREATE_TOKEN_NAME is not available 
         ZwCreateToken(
           TokenHandle: PHANDLE;
           DesiredAccess: ACCESS_MASK;
           ObjectAttributes: POBJECT_ATTRIBUTES;
           Type_: TOKEN_TYPE;
           AuthenticationId: PLUID;
           ExpirationTime: PLARGE_INTEGER;
           User: PTOKEN_USER;
           Groups: PTOKEN_GROUPS;
           Privileges: PTOKEN_PRIVILEGES;
           Owner: PTOKEN_OWNER;
           PrimaryGroup: PTOKEN_PRIMARY_GROUP;
           DefaultDacl: PTOKEN_DEFAULT_DACL;
           Source: PTOKEN_SOURCE):

        *)
    constructor CreateNewToken(const aDesiredAccess: TJwAccessMask;
      const anObjectAttributes: TObjectAttributes;
      const anAuthenticationId: TLUID; const anExpirationTime: int64;
      anUser: TJwSecurityId; aGroups: TJwSecurityIdList;
      aPrivileges: TJwPrivilegeSet; anOwner: TJwSecurityId;
      aPrimaryGroup: TJwSecurityId; aDefaultDACL: TJwDAccessControlList;
      aTokenSource: TTokenSource); virtual;


    {<B>Create_OBJECT_ATTRIBUTES</B> creates and initialises a OBJECT_ATTRIBUTES structure.
     Some members need space on the heap so that
      Free_OBJECT_ATTRIBUTES must be called to free the structure.
     }
    class function Create_OBJECT_ATTRIBUTES(const aRootDirectory: THandle;
      const anObjectName: TJwString; const anAttributes: cardinal;
      const aSecurityDescriptor: TJwSecurityDescriptor;
      const anImpersonationLevel: TSecurityImpersonationLevel;
      const aContextTrackingMode: SECURITY_CONTEXT_TRACKING_MODE;
      const anEffectiveOnly: boolean): TObjectAttributes; virtual;

    {<B>Free_OBJECT_ATTRIBUTES</B> removes memory allocated by the members which were created
     by Create_OBJECT_ATTRIBUTES}
    class procedure Free_OBJECT_ATTRIBUTES(anObjectAttributes:
      TObjectAttributes);
      virtual;


        {<B>CreateRestrictedToken</B> creates a new restricted token of an existing token.
         see http://msdn2.microsoft.com/en-us/library/aa446583.aspx for more information.

         You must set aTokenAccessMask to the token access type of aTokenHandle.

         @param aTokenHandle contains the token handle to be restricted in a new token.
            If this parameter is 0, first the thread token and if not existant
            second the process token will be used as a template, 
         @param aTokenAccessMask contains the access mask of aTokenHandle.
           MAXIMUM_ALLOWED can be used to get the maximum access allowed. 
         @param aFlags contains special flags:
         
          #  DISABLE_MAX_PRIVILEGE
                0x1   Disables all privileges in the new token. If this value is specified, the DeletePrivilegeCount and PrivilegesToDelete parameters are ignored, and the restricted token does not have the SeChangeNotifyPrivilege privilege. 
          #  SANDBOX_INERT
                0x2   Stores this flag in the token. A token may be queried for existence of this flag using GetTokenInformation. 
          #  LUA_TOKEN
                0x4   The new token is a LUA token. 
          #  WRITE_RESTRICTED
                0x8   The new token contains restricting SIDs that are considered only when evaluating write access. 
           
         @param aSidsToDisable contains a list of SIDs that are disabled to the new token. Can be nil. 
         @param aPrivilegesToDelete contains a list of privileges to be removed from the token. Can be nil. 
         @param aRestrictedSids contains a list of SIDs to be restricted. Can be nil. 
         raises
 EJwsclSecurityException:  will be raised if the winapi call failed 

        }
    constructor CreateRestrictedToken(
      PrevTokenHandle : TJwTokenHandle;
      const TokenAccessMask : TJwTokenAccessMask;
      const Flags: cardinal;
      const SidsToDisable: TJwSecurityIdList;
      const PrivilegesToDelete: TJwPrivilegeSet;
      const RestrictedSids: TJwSecurityIdList); overload; virtual;

    {TBD}
    constructor CreateLogonUser(sUsername: TJwString;
    // string that specifies the user name
      sDomain: TJwString;  // string that specifies the domain or server
      sPassword: TJwString;  // string that specifies the password
      dwLogonType,  // specifies the type of logon operation
      dwLogonProvider: cardinal  // specifies the logon provider
      ); overload; virtual;

    //constructor CreateLogonUser(.....
    //constructor CreateLSALogonUser(....
  public
    {<B>ConvertToImpersonatedToken</B> converts the token into an impersonated token. For this purpose
     the token will be converted and the old TokenHandle will be closed. The
     impersonated token will be the new TokenHandle.
     It does nothing if the token is already impersonated.
     The token instance must be opened with TOKEN_DUPLICATE access right.

     Actually you can impersonate a shared token. The impersonated token will be copied into the instance property TokenHandle.
     The old handle will not be closed if Share is set to true. You must save the old value to close it by yourself.

     Because the old handle is discarded you must call these functions again :
       GetTokenPrivileges

     ConvertToImpersonatedToken needs the following access rights:
      # TOKEN_QUERY
      # READ_CONTROL
      # TOKEN_DUPLICATE
     You can use TOKEN_READ instead of TOKEN_QUERY and READ_CONTROL.


     This function does the same as ImpersonateLoggedOnUser if used in this way:
     <code lang="Delphi">
     var Token : TJwSecurityToken;
     begin
       Token := TJwSecurityToken.CreateTokenByProcess(0,
             TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE);
       //Token is the process token
       Token.ConvertToImpersonatedToken(SecurityImpersonation, TOKEN_IMPERSONATE or TOKEN_QUERY);
       //Token is now a new duplicate token (it does not have to do anything with the process token)
       Token.SetThreadToken(0);
       ...do stuff here
       Token.Free;
       Token := TJwSecurityToken.CreateTokenByProcess(0,
             TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE);
     </code>
     As you see the token is freed and recreated in contrast to using "RevertToSelf". This is by design,
     because ConvertToImpersonatedToken converts "Token" to a thread token and loses the process token handle.
     "ReverToSelf" will not help in this case because there is no token to revert to.



    @param impLevel receives the impersonation Level. Use one of these SecurityAnonymous, SecurityIdentification, SecurityImpersonation, SecurityDelegation.  
    @param aDesiredAccess Receives the desired access for this token. The access types can be get from the following list. Access flags must be concatenated with or operator.
    If you want to use DuplicateToken or creating an impersonated token (by ConvertToImpersonatedToken) you must specify TOKEN_DUPLICATE.

    See CreateTokenByProcess CreateTokenByProcess  for a list of access rights. 

    raises
 EJwsclSharedTokenException:  IS NOT USED! .. will be raised if Shared Shared  is set to true. This is because the old token handle will be closed and other referes to it are invalid. 
     EJwsclTokenImpersonationException: will be raised if the call to DuplicateTokenEx failed. 
     EJwsclAccessTypeException: will be raised if the token does not have the access TOKEN_READ and TOKEN_DUPLICATE 

    }
    procedure ConvertToImpersonatedToken(impLevel:
      SECURITY_IMPERSONATION_LEVEL;
      const aDesiredAccess: TJwAccessMask); virtual;

        {<B>ConvertToPrimaryToken</B> converts the token into a primary (or process) token. It does nothing if the token is already a primary token.
         The token instance must be opened with TOKEN_DUPLICATE access right.

         Actually you can impersonate a shared token. The primary token will be copied into the instance property TokenHandle.
         The old handle will not be closed if Share is set to true. You must save the old value to close it by yourself.

         Because the old handle is discarded you must call these functions again :
           GetTokenPrivileges

        ConvertToPrimaryToken needs the following access rights:
          # TOKEN_QUERY
          # READ_CONTROL
          # TOKEN_DUPLICATE
         You can use TOKEN_READ instead of TOKEN_QUERY and READ_CONTROL.

        @param aDesiredAccess Receives the desired access for this token. The access types can be get from the following list. Access flags must be concatenated with or operator.
        If you want to use DuplicateToken or creating an primary token (by ConvertToPrimaryToken) you must specify TOKEN_DUPLICATE.

        See CreateTokenByProcess CreateTokenByProcess  for a list of access rights. 

        @param aDesiredAccess defines the access to the new primary token.
          It can be MAXIMUM_ALLOWED to get the maximum possible access.
          Possible access rights are always equal or lower than the given ones when the handle was opened 
        raises
 EJwsclTokenPrimaryException:  will be raised if the call to DuplicateTokenEx failed. 
         EJwsclAccessTypeException: will be raised if the token does not have the access TOKEN_READ and TOKEN_DUPLICATE 

        }
    procedure ConvertToPrimaryToken(const aDesiredAccess: TJwAccessMask);
      virtual;

        {<B>GetTokenPrivileges</B> creates an instance of TJwPrivilegeSet with all defined privileges of this token.
         The privilege set is a readonly copy.
         You should prefer this function if you want to make more changes.

         Every time you call this function, the resulted instance TJwPrivilegeSet will be saved into an internal list,
          that is cleared if the token instance is freed.
         Be aware that your pointers to these privileges instances are invalid afterwards.
         However you can free the result by yourself. In that case the privileges instance will be removed from the internal list.
        }
    function GetTokenPrivileges: TJwPrivilegeSet;

    function GetTokenPrivilegesEx: PTOKEN_PRIVILEGES;

    {<B>SetIntegrityLevel</B> sets the integrity level of the token using a Sid structure.
     <B>SetIntegrityLevel</B> needs InitWellKnownException from JwsclKnownSid to be called before.

     @param MandatorySid a mandatory Sid. Must not be nil 
     @param Attributes defines attributes for the Sid 
     raises
 EJwsclInitWellKnownException:  will be raised if InitWellKnownException was not called 
      EJwsclNILParameterException: will be raised if parameter MandatorySid is nil 
      EJwsclWinCallFailedException: will be raised if the integrity level could not be changed

    Remarks
      If the compiler directive VISTA (jwscl.inc) is not defined, an exception
      EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
      activated VISTA compiler directive.
    }
    procedure SetIntegrityLevel(const MandatorySid: TJwSecurityId;
      const Attributes: TJwSidAttributeSet = [sidaGroupMandatory]);
      overload; virtual;

    {<B>SetIntegrityLevel</B> sets the integrity level of the token using a Sid structure.
     <B>SetIntegrityLevel</B> needs InitWellKnownException from JwsclKnownSid to be called before.

     @param MandatorySidType a mandatory . Must not be nil
     @param Attributes defines attributes for the Sid
     raises
 EJwsclInitWellKnownException:  will be raised if InitWellKnownException was not called
      EJwsclNILParameterException: will be raised if parameter MandatorySid is nil
      EJwsclWinCallFailedException: will be raised if the integrity level could not be changed

     Remarks
      If the compiler directive VISTA (jwscl.inc) is not defined, an exception
      EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
      activated VISTA compiler directive.
    }
    procedure SetIntegrityLevel(const LabelType: TJwIntegrityLabelType;
      const Attributes: TJwSidAttributeSet = [sidaGroupMandatory]);
      overload; virtual;

    {<B>CreateDuplicateToken</B> duplicates the instance AND token.

     As the token type and impersonation level the current values of the
     instance are used.

     @param Security
      A pointer to a SECURITY_ATTRIBUTES structure that specifies a security descriptor for
      the new token and determines whether child processes can inherit the token.
      If lpTokenAttributes is NULL, the token gets a default security descriptor and
      the handle cannot be inherited. If the security descriptor contains a system access
      control list (SACL), the token gets ACCESS_SYSTEM_SECURITY access right, even if it was not
      requested in dwDesiredAccess.
      (source: http://msdn2.microsoft.com/en-us/library/aa446617.aspx)
      

     @param AccessMask defines the access to the token handle.
       MAXIMUM_ALLOWED can be used to get the maximum access allowed.
    }
    function CreateDuplicateToken(AccessMask: TJwAccessMask;
      Security: PSECURITY_ATTRIBUTES): TJwSecurityToken;

    {<B>CreateRestrictedToken</B> creates a restricted token of the instance.
     This is just a wrapper method of the overloaded method CreateRestrictedToken.}
    function CreateRestrictedToken(
      const TokenAccessMask: TJwTokenHandle;
      const Flags: cardinal;
      const SidsToDisable: TJwSecurityIdList;
      const PrivilegesToDelete: TJwPrivilegeSet;
      const RestrictedSids: TJwSecurityIdList
      ): TJwSecurityToken; overload; virtual;


    {<B>CheckTokenMembership</B> checks whether a given SID is member of the token.
     It returns true if the SID could be found in the list ignoring
     whether the SID is enabled or not; otherwise it returns false.}
    function CheckTokenMembership(aSidToCheck: TJwSecurityId): boolean;

        {<B>IsEqual</B> compares the token instance with a second one.
         This function loads a function from ntdll.dll dynamically. This function is only available on XP or better
         @return <B>IsEqual</B> returns true if both tokens do have the same accesscheck; otherwise false. It returns false if aToken is nil. 
         
         raises
 EJwsclAccessTypeException:  will be raised if the token or aToken does not have access type TOKEN_QUERY 
         }
    function IsEqual(aToken: TJwSecurityToken): boolean;

        {<B>SetThreadToken</B> sets the thread token.
         @param Thread contains the thread handle. If Thread is zero the calling thread will be used. 

        raises
 EJwsclSecurityException:  will be raised if the token could not be attached to the thread
         EJwsclSecurityException: will be raised if a winapi function failed 
        }
    procedure SetThreadToken(const Thread: TJwThreadHandle);

        {
        <B>RemoveThreadToken</B> removes the token from the thread.
        @param Thread contains the thread handle. If Thread is zero the calling thread will be used. 
        raises
 EJwsclSecurityException:  will be raised if a winapi function failed
        }
    class procedure RemoveThreadToken(const Thread: TJwThreadHandle);

        {The <B>ImpersonateLoggedOnUser</B> function lets the calling thread impersonate the security context of a logged-on user. The user is represented by a token handle.
         If the current instance is already a thread token (=impersonated token), the method is just impersonating it. Otherwise if the current instance
         is a primary token, the method is converting it to a thread token and then impersonating it. However the second
         case is worth mentioning because the new thread token is not related to the current token instance. That means any operations on the current instance
         (e.g. set privileges) don't have an effect on the token of the thread. Create a new instance by calling TJwSecurityToken.CreateTokenByThread to get the new thread token or
         call ConvertToImpersonatedToken to get an thread token directly without creating a new instance.
        raises
 EJwsclAccessTypeException:  will be raised if the token is an impersonation token and does not have access type TOKEN_QUERY and TOKEN_IMPERSONATE
         EJwsclAccessTypeException: will be raised if the token is a primary token and does not have access type TOKEN_QUERY and TOKEN_DUPLICATE 
         EJwsclSecurityException: will be raised if a winapi function failed 
        }
    procedure ImpersonateLoggedOnUser;

        {<B>PrivilegeCheck</B> is a simulation of WinAPI PrivilegeCheck (http://msdn2.microsoft.com/en-us/library/aa379304.aspx)
         <B>PrivilegeCheck</B> checks for enabled privleges of the token.
         If RequiresAllPrivs is pcDefault <B>PrivilegeCheck</B> returns true if one privilege provided in aRequiredPrivileges is enabled in the token
          If no privilege from aRequiredPrivileges is enabled in the token the function returns false.
         If RequiresAllPrivs is pcAllPrivsEnabled <B>PrivilegeCheck</B> returns true if all privileges from aRequiredPrivileges are enabled in the token; otherwise false.

         Every privilege that was used for a privilege check will have the property Privilege_Used_For_Access set to true.

         @param aRequiredPrivileges provides a list of priveleges that are compared with the token
         @return see description 
         }
    function PrivilegeCheck(const RequiredPrivileges: TJwPrivilegeSet;
      const RequiresAllPrivs: TJwPrivCheck): boolean; overload;

        {<B>PrivilegeCheckEx</B> works like PrivilegeCheck . However this function uses the winapi call PrivilegeCheck.
         The property Privilege_Used_For_Access in TJwPrivilege is not supported.
         }
    function PrivilegeCheckEx(const RequiredPrivileges: TJwPrivilegeSet;
      const RequiresAllPrivs: TJwPrivCheck): boolean; overload;

        {<B>PrivilegeCheck</B> is a simulation of WinAPI PrivilegeCheck (http://msdn2.microsoft.com/en-us/library/aa379304.aspx)
         <B>PrivilegeCheck</B> checks for enabled privleges of the token.
         If RequiresAllPrivs is pcDefault <B>PrivilegeCheck</B> returns true if one privilege provided in aRequiredPrivileges is enabled in the token
          If no privilege from aRequiredPrivileges is enabled in the token the function returns false.
         If RequiresAllPrivs is pcAllPrivsEnabled <B>PrivilegeCheck</B> returns true if all privileges from aRequiredPrivileges are enabled in the token; otherwise false.

         @param ClientToken is a token that is used to check the privileges 
         @param aRequiredPrivileges provides a list of priveleges that are compared with the token  
         @return see description 
         
        }
    class function PrivilegeCheck(
      const ClientToken: TJwSecurityToken;
      const RequiredPrivileges: TJwPrivilegeSet;
      const RequiresAllPrivs: TJwPrivCheck): boolean;  overload;

    {<B>CopyLUID</B> copies a LUID and returns it}
    class function CopyLUID(const originalLUID: TLUID): TLUID;

        {<B>GetTokenStatistics</B> gets token information in a class called TJwSecurityTokenStatistics .
         The programmer must free the class TJwSecurityTokenStatistics}
    function GetTokenStatistics: TJwSecurityTokenStatistics;

    {<B>GetCurrentUserRegKey</B> opens a registry key HKEY_CURRENT_USER of the current thread token.
     Use it instead of directly access HKEY_CURRENT_USER if you want to
     access the user registry of an impersonated user.

     @param DesiredAccess defines desired access mask to this key.
        MAXIMUM_ALLOWED can be used to get maximum access to the key. 
     @return Returns a handle to the key. Use RegCloseKey to close the key 
     raises
 EJwsclWinCallFailedException:  will be raised if the call
      to RegOpenCurrentUser failed 
    }
    function GetCurrentUserRegKey(const DesiredAccess: TJwAccessMask): HKEY;

    {<B>LoadUserProfile</B> loads the user profile of the current token instance.
     It also uses the roaming profile if possible.

     @param ProfileInfo defines parameter for supplying to winapi LoadUserProfile.
        You must set a flag in parameter ProfileMembers for each member of this record
        you want to set.  
        See MSDN for more information.
        The method returns a registry key handle in member Profile of structure.
        Call UnLoadUserProfile to unload this key if finished.
         
         
     @param ProfileMembers defines which member of ProfileInfo is set by
      the user. All other parameters are set to default values.
     
      # pmFlags xclude this value if you want to let the method
          set PI_NOUI 
      # pmUserName Exclude this value if you want to let the method
          set the correct user name 

      # pmProfilePath Exclude this value if you want to let the method
        get the roaming profile.
     
      

    raises
 EJwsclPrivilegeException
       :  will be raised if the privilege SE_RESTORE_NAME and SE_BACKUP_NAME is not available. 
     EJwsclWinCallFailedException: will be raised if call to LoadUserProfile failed 

    }
    procedure LoadUserProfile(var ProfileInfo : TJwProfileInfo;
        const ProfileMembers : TJwProfileMembers);

    {<B>UnLoadUserProfile</B> unloads a user profile loaded by LoadUserProfile.
     Member ProfileInfo.Profile will be set to INVALID_HANDLE_VALUE.
     @param ProfileInfo define the profile 
     @return Returns success (true) status of the operation. 
    }
    function UnLoadUserProfile(var ProfileInfo : TJwProfileInfo) : Boolean;
  public
    //instance function related to token context

        {<B>PrivilegedServiceAuditAlarm</B> function generates an audit message in the security event log.
         For a detailed information see MSDN : http://msdn2.microsoft.com/en-gb/library/aa379305.aspx

         If you want to enable audit functions the calling process (not thread token!) needs the SeAuditPrivilege privilege.
         Per default only services have this privilege. However it can be enabled in group policy editor : "gpedit.msc" manager (under xp)
           Computer configuration -> Windows settings -> security settings -> local policies -> audit policy
            enable (success/failure) policy : audit privilege
          The parameter AccessGranted is linked with the type of policy - success or failiure.
          (http://www.nemesisblue.info/images%5Cgpedit1.gif)

         The audit event can be seen in the event viewer in security leaf.

         @param ClientToken is the token to be used in audit log.

         raises
 EJwsclPrivilegeNotFoundException:  will be raised if the process token does not have the privilege : SE_AUDIT_NAME 
          EJwsclWinCallFailedException: will be raised if the winapi call to PrivilegedServiceAuditAlarm failed. 
          EJwsclInvalidTokenHandle: will be raised if the parameter ClientToken is nil 
         }
    class procedure PrivilegedServiceAuditAlarm(
      SubsystemName, ServiceName: TJwString; ClientToken: TJwSecurityToken;
      Privileges: TJwPrivilegeSet; AccessGranted: boolean);

    //see equivalent msdn function for more information
    class procedure ImpersonateAnonymousToken(const Thread: TJwThreadHandle);
      virtual;
    //see equivalent msdn function for more information
    class procedure ImpersonateSelf(
      const anImpersonationLevel: SECURITY_IMPERSONATION_LEVEL); virtual;
    //see equivalent msdn function for more information
    class procedure RevertToSelf; virtual;
    //see equivalent msdn function for more information
    class procedure ImpersonateNamedPipeClient(hNamedPipe: THandle); virtual;

        {<B>HasThreadAToken</B> returns whether the current thread has a token or not.
        @return Returns true if the thread has a token; otherwise false.}
    class function HasThreadAToken(): boolean; virtual;
        {<B>GetThreadToken</B> returns the token of the current thread or nil if none exists.
         See CreateTokenByThread for more information.
         @return Returns the thread token or nil if none exists.
          The caller must free the token instance.
         }
    class function GetThreadToken(const aDesiredAccess: TJwAccessMask;
      const anOpenAsSelf: boolean): TJwSecurityToken; virtual;

    {<B>GetTokenUserName</B> returns the username of the token user.}
    function GetTokenUserName : TJwString;

    {<B>GetSecurityDescriptor</B> gets the security descriptor.
     The caller is responsible to free the returned instance.
     See TJwSecureGeneralObject.GetSecurityInfo  for more information
     about exceptions.

     @param SecurityFlags defines which component of the security descriptor
      is retrieved. 
     @return Returns a new security descriptor instance.  
    }
    function GetSecurityDescriptor(
      const SecurityFlags: TJwSecurityInformationFlagSet):
      TJwSecurityDescriptor;
      virtual;

    {<B>SetSecurityDescriptor</B> sets the security descriptor.
     See TJwSecureGeneralObject.SetSecurityInfo  for more information
     about exceptions.
     Warning: Changing the security descriptor's security information can
      lead to security holes. 

     @param SecurityFlags defines which component of the security descriptor
      is changed.  
    }
    procedure SetSecurityDescriptor(
      const SecurityFlags: TJwSecurityInformationFlagSet;
      const SecurityDescriptor: TJwSecurityDescriptor); virtual;

  public
    //overriden basic methods
    function Equals(Obj: TObject): Boolean; {$IFDEF DELPHI2009_UP}override;{$ELSE}virtual;{$ENDIF}
    function GetHashCode: Integer;          {$IFDEF DELPHI2009_UP}override;{$ELSE}virtual;{$ENDIF}
    function ToString: String;              {$IFDEF DELPHI2009_UP}override;{$ELSE}virtual;{$ENDIF}

  public
    {TokenHandle contains a handle to the opened token. It can be zero.}
    property TokenHandle: TJwTokenHandle Read fTokenHandle;

        {Shared is a user defined boolean state that defines whether the token handle is used out of this instance scope.
         If true some methods do not work because they closes the handle which would lead to unpredictable results.}
    property Shared: boolean Read fShared Write fShared;

        {TokenTypes gets the token type. The result can be one of these values :
         TokenPrimary, TokenImpersonation}
    property TokenType: TOKEN_TYPE Read GetTokenType;

    {<B>AccessMask</B>  contains the access flags that was specified when the token was created or opened}
    property AccessMask: TJwAccessMask Read fAccessMask;

        {<B>ImpersonationLevel</B> returns the impersonation level of an impersonated token.
         If the token is a primary token, the result is always DEFAULT_IMPERSONATION_LEVEL}
    property ImpersonationLevel: TSecurityImpersonationLevel
      Read GetImpersonationLevel;

    {<B>IsRestricted</B> returns true if the token was created by CreateRestrictedToken (or by the equivalent winapi function); otherwise false
     The call just checks for deny SIDs in the token groups and if it finds
     any deny SID it returns true.
     Removed privileges are not detectable.
    }
    property IsRestricted: boolean Read GetIsRestricted;

    {<B>IsTokenMemberShip[aSID</B> checks if a user is listed in the tokens user list}
    property IsTokenMemberShip[aSID: TJwSecurityId]: boolean
      Read CheckTokenMembership;

        {<B>TokenUser</B> contains the user that holds the token.
         A read call creates a new TJwSecurityId that must be destroyed!}
    property TokenUser: TJwSecurityId Read GetTokenUser;



    {<B>TokenUserName</B> returns the username stored in the token.
    This value may differ from the API function GetCurrentUserName}
    property TokenUserName : TJwString read GetTokenUserName;

    {<B>UserName</B> returns the logged on user name of the current logon session.
     The return value may differ from TokenUserName because it gets the
     username from the logon session and not from the username stored in the token.
    }
    property UserName : TJwString read GetUserName;

    {<B>TokenGroups</B> contains the groups which the token belongs to.
     The caller is responsible to free the returned security id list.
     Do not use members of TokenGroups directly without using a variable.
     Every call of members directly will result into a new list!

     The token handle must be valid otherwise
      EJwsclInvalidTokenHandle will be raised.

     Get:
      see GetTokenInformation  for more information about exceptions.
     Set:
      EJwsclNILParameterException is raised if the given list is nil.
      EJwsclWinCallFailedException is raised if a call to AdjustTokenGroups failed.

     }
    property TokenGroups: TJwSecurityIdList
      Read GetTokenGroups Write SetTokenGroups;

    {<B>TokenGroupsAttributes[Index</B> sets or gets the token groups attributes.
     Through these attributes a token group can be activated to let
     AccessCheck use it in its checking.
      This property raises EListError if the Index could not be found.
      For further information and exceptions see TokenGroups .
    }
    property TokenGroupsAttributes[Index: integer]: TJwSidAttributeSet
      Read GetTokenGroupsAttributesInt Write SetTokenGroupsAttributesInt;

    {<B>TokenGroupsAttributesBySid[Sid</B> sets or gets the token groups attributes.
     Through these attributes a token group can be activated to let
     AccessCheck use it in its checking.
     This property raises EListError if the Sid could not be found.
     For further information and exceptions see TokenGroups .
    }
    property TokenGroupsAttributesBySid[Sid: TJwSecurityId]:
      TJwSidAttributeSet Read GetTokenGroupsAttributesSid
      Write SetTokenGroupsAttributesSid;

        {<B>TokenRestrictedSids</B> contains all users that have restricted rights on the token.
         The user must free the list}
    property TokenRestrictedSids: TJwSecurityIdList
      Read GetTokenRestrictedSids;

        {<B>TokenDefaultDacl</B> sets or gets the defaul discretionary access control list of the token.
         The value is dynamic returned. It always returns the current token state and is not saved.
         So after a reading call the returned object must be freed!

         }
    property TokenDefaultDacl: TJwDAccessControlList
      Read GetTokenDefaultDacl Write SetTokenDefaultDacl; //TOKEN_ADJUST_DEFAULT

        {<B>TokenOrigin</B> sets or gets the token origin.
         The value can only be set if it has not been already set.
         The process or thread needs the SE_TCB_NAME privilege to set a value.
        }
    property TokenOrigin: TLuid Read GetTokenOrigin Write SetTokenOrigin;
    //SE_TCB_NAME

        {<B>TokenOwner</B> sets or gets the token owner.
         To set the value the token needs TOKEN_ADJUST_DEFAULT privilege.

         Returned Sid must be freed.
         }
    property TokenOwner: TJwSecurityId Read GetTokenOwner Write SetTokenOwner;
    //TOKEN_ADJUST_DEFAULT

        {<B>PrimaryGroup</B> sets or gets the primary group.
         To set the value the token needs TOKEN_ADJUST_DEFAULT privilege}
    property PrimaryGroup: TJwSecurityId
      Read GetPrimaryGroup Write SetPrimaryGroup;
    //TOKEN_ADJUST_DEFAULT

        {<B>TokenSessionId</B> sets or gets the Session ID of the token.
         To set the value the token needs SE_TCB_NAME privilege.

         A write call on a Windows 2000 is ignored!
         A write call on needs the SE_TCB_NAME privilege.

		 To set the SessionID in an existing token you need to create a duplicate first and
		 set the ID of the duplicated token. Use CreateDuplicateExistingToken for this reason.

         See
         http://msdn2.microsoft.com/en-us/library/aa379591.aspx
         for more information.

         }
    property TokenSessionId: cardinal Read GetTokenSessionId
      Write SetTokenSessionId;

        {<B>PrivilegeEnabled[Name</B> sets or gets a privilege of the token.
         If you plan to use this property extensivly  try GetTokenPrivileges instead.

         EJwsclPrivilegeNotFoundException will be raised if you try to set a privilege that is unknown or not available in the token.
           If you try to read a privilege that could not be found in the privilege list the return value will be false.

         }
    property PrivilegeEnabled[Name: TJwString]: boolean
      Read GetPrivilegeEnabled Write SetPrivilegeEnabled;

        {<B>PrivilegeAvailable[Name</B> checks whether a defined privilege is available in the token.
         It returns true if the privilege was found; otherwise false.
        }
    property PrivilegeAvailable[Name: TJwString]: boolean
      Read GetPrivilegeAvailable;

    {<B>RunElevation</B> returns the elavation status of the process on a Windows Vista system.
     If the system is not a supported the exception EJwsclUnsupportedWindowsVersionException will be raised

     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server or newer.

     Remarks
       Only Windows Vista is supported.
       If the token is elevated the return value is 1; otherwise 0.

       If the compiler directive VISTA (jwscl.inc) is not defined, an exception
       EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
       activated VISTA compiler directive.
     }
    property RunElevation: cardinal Read GetRunElevation;

      {<B>ElevationType</B> returns the elavation type of the process on a Windows Vista system.
       If the system is not a supported the exception EJwsclUnsupportedWindowsVersionException will be raised

       Raises
         EJwsclUnsupportedWindowsVersionException This exception will be raised if the
           Windows System is not a Windows Vista/Server or newer.


       Remarks
         Only Windows Vista is supported.

         If the compiler directive VISTA (jwscl.inc) is not defined, an exception
         EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
         activated VISTA compiler directive.
       }
    property ElevationType: TTokenElevationType Read GetElevationType;

    {<B>TokenIntegrityLevel</B> returns the integrity level of the token.

     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

     Remarks
       This function only works in Windows Vista and newer.
       The caller is responsible for freeing the resulting TJwSecurityIdList.

       If the compiler directive VISTA (jwscl.inc) is not defined, an exception
       EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
       activated VISTA compiler directive.
    }
    property TokenIntegrityLevel: TJwSecurityIdList Read GetIntegrityLevel;

    {<B>TokenIntegrityLevelType</B> sets or gets the TokenIntegrityLevel in an easier way.
     This property uses iltLow, iltMedium, iltHigh, iltSystem and iltProtected to
     get or set the integrity level.


     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

     Remarks
       If the token does not have a level the function returns iltNone.
       The value iltNone cannot be set!

       This property can raise exceptions on get and set! See SetIntegrityLevel.

       If the compiler directive VISTA (jwscl.inc) is not defined, an exception
       EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
       activated VISTA compiler directive.
    }
    property TokenIntegrityLevelType: TJwIntegrityLabelType
      Read GetIntegrityLevelType Write SetIntegrityLevelType;


    {<B>LinkedToken</B> returns the linked token of this token.
     In vista every token can have a second token that has more or less
     rights. The UAC uses this token to assign it to a new process with elevated
     rights.
     However this token is useless for non privileged tokens because SetThreadToken
     and other functions which get this token checks whether the user can use this
     token or not.

     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

     Remarks
       This function only works in Windows Vista and newer.
       The caller is responsible for freeing the resulting TJwSecurityToken

       If the compiler directive VISTA (jwscl.inc) is not defined, an exception
       EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
       activated VISTA compiler directive.

       The token must be converted to a primary token first to be usable in
       CreateProcessAsUser. However this duplication needs the TCB privilege
       even if the linked token has less rights.
    }
    property LinkedToken: TJwSecurityToken Read GetLinkedToken;

    {<B>VirtualizationAllowed</B> returns the status of allowance of virtualization of the process on a Windows Vista system.
     If the system is not a supported the exception EJwsclUnsupportedWindowsVersionException will be raised

     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

     Remarks
      Only Windows Vista is supported.

      If the compiler directive VISTA (jwscl.inc) is not defined, an exception
      EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
      activated VISTA compiler directive.
     }
    property VirtualizationAllowed: boolean Read GetVirtualizationAllowed;

    {<B>VirtualizationEnabled</B> returns the status of status of virtualization. It is either on or off and only works on a Windows Vista system.
     If the system is not a supported the exception EJwsclUnsupportedWindowsVersionException will be raised

     Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

     Remarks
      Only Windows Vista is supported.

      If the compiler directive VISTA (jwscl.inc) is not defined, an exception
      EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
      activated VISTA compiler directive.


     }
    property VirtualizationEnabled: boolean Read GetVirtualizationEnabled;

    {<B>MandatoryPolicy</B> returns the mandatory policy of the token.
     This property can have one the following values (from MSDN: http://msdn2.microsoft.com/en-us/library/bb394728.aspx):
      
        # TOKEN_MANDATORY_POLICY_OFF No mandatory integrity policy is enforced for the token. 
        # TOKEN_MANDATORY_POLICY_NO_WRITE_UP A process associated with the token cannot write to objects that have a greater mandatory integrity level. 
        # TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN A process created with the token has an integrity level that is the lesser of the parent-process integrity level and the executable-file integrity level. 
        # TOKEN_MANDATORY_POLICY_VALID_MASK A combination of TOKEN_MANDATORY_POLICY_NO_WRITE_UP and TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN

    Raises
       EJwsclUnsupportedWindowsVersionException This exception will be raised if the
         Windows System is not a Windows Vista/Server 2008 or newer.

    Remarks
      If the compiler directive VISTA (jwscl.inc) is not defined, an exception
      EJwsclVistaFeaturesDisabled is raised. This feature can only be used with
      activated VISTA compiler directive.
    }

    property MandatoryPolicy: TJwTokenMandatoryPolicies
      Read GetMandatoryPolicy;

    {<b>IsPrimaryToken</b> returns true if the current token instance
     is a primary token; otherwise false.}
    property IsPrimaryToken : Boolean index 1 read IsTokenType;
    {<b>IsImpersonationToken</b> returns true if the current token instance
     is a impersonated token; otherwise false. Same as IsThreadToken}
    property IsImpersonationToken : Boolean index 2 read IsTokenType;
    {<b>IsThreadToken</b> returns true if the current token instance
     is a thread token; otherwise false; Same as IsImpersonationToken.}
    property IsThreadToken : Boolean index 2 read IsTokenType;

  end;

     {<B>TJwSecurityTokenStatistics</B> is a class that holds information about a token.
      For a detailed description see msdn : http://msdn2.microsoft.com/en-us/library/aa379632.aspx
      }
  TJwSecurityTokenStatistics = class(TObject)
  protected
    fTokenId:    TLUID;
    fAuthenticationId: LUID;
    fExpirationTime: LARGE_INTEGER;
    fTOKEN_TYPE: TTokenType;
    fSECURITY_IMPERSONATION_LEVEL: TSecurityImpersonationLevel;
    fDynamicCharged: cardinal;
    fDynamicAvailable: cardinal;
    fGroupCount: cardinal;
    fPrivilegeCount: cardinal;
    fModifiedId: TLUID;
  public
       {<B>Create</B> creates a new token statistic class.
        @param stats contains the token statistic structure provided by GetTokenInformation.  }
    constructor Create(stats: TTokenStatistics);

    {<B>GetText</B> returns token statistics as text.}
    function GetText: TJwString; virtual;
       {<B>TokenId</B> contains the luid of the token.
        See also : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property TokenId: TLUID Read fTokenId;

       {<B>AuthenticationId</B> contains the authentication id
        See also : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property AuthenticationId: LUID Read fAuthenticationId;
    {For detailed information on <B>ExpirationTime</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property ExpirationTime: LARGE_INTEGER Read fExpirationTime;
    {For detailed information on <B>TOKEN_TYPE</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property TOKEN_TYPE: TTokenType Read fTOKEN_TYPE;
    {For detailed information on <B>SECURITY_IMPERSONATION_LEVEL</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property SECURITY_IMPERSONATION_LEVEL: TSecurityImpersonationLevel
      Read fSECURITY_IMPERSONATION_LEVEL;
    {For detailed information on <B>DynamicCharged</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property DynamicCharged: cardinal Read fDynamicCharged;
    {For detailed information on <B>DynamicAvailable</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property DynamicAvailable: cardinal Read fDynamicAvailable;
    {For detailed information on <B>GroupCount</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property GroupCount: cardinal Read fGroupCount;
    {For detailed information on <B>PrivilegeCount</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property PrivilegeCount: cardinal Read fPrivilegeCount;
    {For detailed information on <B>ModifiedId</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
    property ModifiedId: TLUID Read fModifiedId;
    {For detailed information on <B>end</B> see : http://msdn2.microsoft.com/en-us/library/aa379632.aspx}
  end;


  {<B>TJwPrivilege</B> contains information about a token privilege}
  TJwPrivilege = class(TObject)
  private
    fAttributes: cardinal;
    fLUID: LUID;


    fName, fDisplayName: TJwString;

    fLanguageID: cardinal;

    fOwner: TJwPrivilegeSet;
    fPrivilege_Used_For_Access: boolean;

    fPrivilege_Enabled_By_Default: boolean;
  protected
       {<B>GetEnabled</B> retrieves the enable status of a privilege.
        raises
 EJwsclAdjustPrivilegeException:  will be raised if :
            
             #  A call to AdjustTokenPrivileges failed, because the privilege does not exist or was refused to change.
             # A second call to AdjustTokenPrivileges failed, because the original state could not be restored. The second call will only be
              made in case of a changed privilege. 
             
         EJwsclNotImplementedException: If the privilete instance does not belong to a token.
         EJwsclAccessTypeException: If the token does not hold TOKEN_QUERY and TOKEN_ADJUST_PRIVILEGES access values.
       }
    function GetEnabled: boolean; virtual;

       {
       <B>SetEnabled</B> sets the enable status of a privilege.
       If the privilege had originally the attribute flag SE_PRIVILEGE_ENABLED_BY_DEFAULT set,
       it is also set.
        raises
 EJwsclAdjustPrivilegeException:  will be raised if call to AdjustTokenPrivileges failed, because the privilege does not exist or was refused to change.
         EJwsclNotImplementedException: If the privilete instance does not belong to a token.
         EJwsclAccessTypeException: If the token does not hold TOKEN_QUERY and TOKEN_ADJUST_PRIVILEGES access values.
       }
    procedure SetEnabled(const en: boolean); virtual;
  public
       {<B>Create</B> creates a new instance with information of a privilege.
        raises
 EJwsclInvalidOwnerException:  will be raised if anOwner is nil. 
        }
    constructor Create(anOwner: TJwPrivilegeSet;
      aLUID_AND_ATTRIBUTES: LUID_AND_ATTRIBUTES);

       {<B>PrivilegeAttributeToText</B> convertes a set of attributes into a human readable string

       @param SE_Attributes receives the privilege attributes to be converted 
       @return The result ist a combination (comma seperated) of these strings:
               (none)
               SE_PRIVILEGE_ENABLED_BY_DEFAULT
               SE_PRIVILEGE_ENABLED
               SE_PRIVILEGE_USED_FOR_ACCESS
               (unknown attributes) 
       }
    class function PrivilegeAttributeToText(PrivilegeAttributes: cardinal;
      HumanReadable: boolean = False): TJwString;
      virtual;

       {<B>LUIDtoText</B> converts a LUID (locally unique ID) into a string.
        Output format:
        <pre>'hi: 0x<hipart>, lo: 0x<lopart> (0x<(hipart shl 4) or (lopart)>)';</pre>

        @param aLUID receives the LUID of the privilege 
       }
    class function LUIDtoText(aLUID: LUID): TJwString; virtual;

       {<B>TextToLUID</B> creates a luid structure out of a privilege name on a system environment.
        @param Name The name parameter defines the privilege name to be converted into a luid.
        @param SystemName The SystemName parameter specifies the system name where to search for. Leave empty to use the local system.
        @return <B>TextToLUID</B> returns a luid with the requested privilege information or the value LUID_INVALID if no exception occured but the luid value not set.

        See http://msdn2.microsoft.com/en-us/library/aa379180.aspx for more information.
       }
    class function TextToLUID(const Name: TJwString;
      const SystemName: TJwString = ''): TLuid;

    {<B>MakeLUID_AND_ATTRIBUTES</B> creates a luid and attributes structure from the given parameters.    }
    class function MakeLUID_AND_ATTRIBUTES(const LowPart: cardinal;
      const HighPart: LONG; Attributes: cardinal): TLuidAndAttributes;
      overload;

    {<B>MakeLUID_AND_ATTRIBUTES</B> creates a luid and attributes strcture from the given parameters.}
    class function MakeLUID_AND_ATTRIBUTES(const Luid: TLuid;
      Attributes: cardinal): TLuidAndAttributes; overload;

    {<B>MakeLUID</B> creates a luid strcture from the given parameter.}
    class function MakeLUID(const LowPart: cardinal;
      const HighPart: LONG): TLuid;

       {<B>RemoveIrrepealable</B> removes a privilege from the token.
        It cannot be readded.
        @param aPrivilege contains the privilege to be removed 
        raises
 EJwsclAdjustPrivilegeException:  if the token could not be removed 
         EJwsclAdjustPrivilegeException: if the token does not held the privilege  
        }
    procedure RemoveIrrepealable; virtual;

       {<B>GetText</B> creates a string that contains a privilege in a human
        readable form :
		 <pre>	
         LUID       : <luid> #13#10
         Name       : <name> #13#10
         DisplayName : <descr> #13#10
         Attributes : <attributes> #13#10#13#10
        </pre>
        }
    function GetText: TJwString; virtual;

       {<B>IsEnabledByDefault</B> returns the whether the state SE_PRIVILEGE_ENABLED_BY_DEFAULT
        is set in the Attributes property (true) or not (false).
       }
    function IsEnabledByDefault: boolean; virtual;
  public
    {The owner token of this privilege set. }
    property Owner: TJwPrivilegeSet Read fOwner;

    //LUID contains the identifier of the privilege
    property LUID: LUID Read fLUID;

       {Attributes contains the status of the privilege.
        It is a bit combination of these values :

        * SE_PRIVILEGE_ENABLED_BY_DEFAULT
        * SE_PRIVILEGE_ENABLED
        * SE_PRIVILEGE_USED_FOR_ACCESS
       }
    property Attributes: cardinal Read fAttributes;

    //<B>Name</B> contains the system name of the privilege (like SeTcbPrivilege)
    property Name: TJwString Read fName;

       {<B>DisplayName</B> contains a description of the privilege provided by the system
        The language can be retrieved in the property LanguageID .
       }
    property DisplayName: TJwString Read fDisplayName;

    //<B>LanguageID</B> provides the language of the display name
    property LanguageID: cardinal Read fLanguageID;

       {<B>Enabled</B> enables or disables a privilege
        EJwsclNotImplementedException will be raised if the privilege is not assigned to a token.
        EJwsclAdjustPrivilegeException will be raised if the privilege attributes could not be set or retrieved.
        See also GetEnabled and SetEnabled for more information.
       }
    property Enabled: boolean Read GetEnabled Write SetEnabled;

       {Privilege_Used_For_Access will be set to true if the privilege was used
        by PrivilegeCheck function. However PrivilegeCheckEx does not support this value.
       }
    property Privilege_Used_For_Access: boolean
      Read fPrivilege_Used_For_Access Write fPrivilege_Used_For_Access;

  end;

     {<B>TJwPrivilegeSet</B> is a set of Privileges (defined by TJwPrivilege)
      There are two types of instances of TJwPrivilegeSet.
      1. TJwPrivilegeSet with an assigned token
      2. TJwPrivilegeSet without an assigned token.
      It is not possible to change from one to the other case.

      1. If the token is assigned, you can enable or disable privileges. However
       you cannot change privileges arbitrarily.
      2. If the privleges set is not assigned to a token, you can add or remove privileges arbitrarily.
       However you cannot enable or disable privileges.

      The description of these function shows what happens in one of these cases.

      }
  TJwPrivilegeSet = class(TObject)
  private
    fControl: cardinal;
    fList:    TObjectList;
    fOwner:   TJwSecurityToken;

    fPPrivilegesList:    TList;
    fPPrivilegesSetList: TList;
  protected

    //see property PrivByIdx 
    function GetPrivByIdx(Index: cardinal): TJwPrivilege;
    //see property PrivByName 
    function GetPrivByName(Index: TJwString): TJwPrivilege;
    //see property PrivByLUID 
    function GetPrivByLUID(Index: LUID): TJwPrivilege;

    function GetCount: cardinal;
  public
       {<B>Create</B> creates a new instance with a list of privileges.
        @param Owner contains the owner token of this privilege set. It must not be nil! 
        @param Privileges contains a set of privileges 
        raises
 EJwsclInvalidOwnerException:  will be raised if anOwner is nil. }
    constructor Create(Owner: TJwSecurityToken;
      Privileges: jwaWindows.TPrivilegeSet); overload;
       {<B>Create</B> creates a new instance with a list of privileges.
        @param Owner contains the owner token of this privilege set. It must not be nil! 
        @param PrivilegesPointer contains a set of privileges 
        raises
 EJwsclInvalidOwnerException:  will be raised if anOwner is nil. }
    constructor Create(Owner: TJwSecurityToken;
      PrivilegesPointer: PTOKEN_PRIVILEGES); overload;

       {<B>Create</B> creates a new instance with a list of privileges from a
         TJwPrivilegeSet instance.

        @param Owner contains the owner token of this privilege set. It must not be nil! 
        @param PrivilegesPointer contains a set of privileges 
        raises
 EJwsclInvalidOwnerException:  will be raised if anOwner is nil. }
    constructor Create(Owner: TJwSecurityToken;
      PrivilegeObject: TJwPrivilegeSet);
      overload;

       {<B>Create</B> creates a new user defined privilege list that
        cannot be used with a token.
        It is used to create a list of privileges that can be assigned a
        token function that needs it.}
    constructor Create; overload;

    destructor Destroy; override;

       {<B>GetText</B> creates a string that contains all privileges in a human
        readable form :
        <pre> 
         LUID       : <luid> #13#10
         Name       : <name> #13#10
         DisplayName : <descr> #13#10
         Attributes : <attributes> #13#10#13#10
        </pre>
        }
    function GetText: TJwString;

       {<B>RemoveIrrepealable</B> removes a privilege from the token.
        It cannot be readded.
        @param aPrivilege contains the privilege to be removed 
        raises
 EJwsclAdjustPrivilegeException:  if the token could not be removed 
         EJwsclNotImplementedException: if the set is not assigned to a token 
        }
    procedure RemoveIrrepealable(Privilege: TJwPrivilege); virtual;

       {<B>DisableAllPrivileges</B> disables all privileges in this token.
        This is done in a faster way than iterate through the privilege list.
        You can undo this by setting Enabled state of a privilege.
        raises
 EJwsclNotImplementedException:  if the set is not assigned to a token 
        }
    procedure DisableAllPrivileges; virtual;

       {<B>Create_PLUID_AND_ATTRIBUTES</B> creates an array of luid and attribute structure from the
         list of added privileges in this instance of TJwPrivilegeSet.
        The number of array elements is count.
        If count is zero the return value is nil.

        The structure must be freed by Free_PLUID_AND_ATTRIBUTES .
        If not freed by the user the structure will be freed on destruction of the TJwPrivilegeSet instance.
        All created structures by <B>Create_PLUID_AND_ATTRIBUTES</B> are freed in this way.
        }
    function Create_PLUID_AND_ATTRIBUTES: PLUID_AND_ATTRIBUTES; virtual;

       {<B>Create_PPRIVILEGE_SET</B> creates a set of privileges with an array of luids and attributes from the
         list of added privileges in this instance of TJwPrivilegeSet.
        The number of array elements is count.
        If count is zero the return value is an emtpy structure but not nil

        @return Contains a set of privileges. 

        The structure must be freed by Free_PPRIVILEGE_SET .
        If not freed by the user the structure will be freed on destruction of the TJwPrivilegeSet instance.
        All created structures by <B>Create_PPRIVILEGE_SET</B> are freed in this way.
        }
    function Create_PPRIVILEGE_SET: jwaWindows.PPRIVILEGE_SET; virtual;

       {<B>Create_PTOKEN_PRIVILEGES</B> creates a set of privileges with an array of luids and attributes from the
         list of added privileges in this instance of TJwPrivilegeSet.
        The number of array elements is count.
        If count is zero the return value is an emtpy structure but not nil

        @return Contains a set of privileges.  

        The structure must be freed by Free_PTOKEN_PRIVILEGES .
        If not freed by the user the structure will be freed on destruction of the TJwPrivilegeSet instance.
        All created structures by <B>Create_PTOKEN_PRIVILEGES</B> are freed in this way.
        }
    function Create_PTOKEN_PRIVILEGES: jwaWindows.PTOKEN_PRIVILEGES;
      virtual;

       {<B>Free_PLUID_AND_ATTRIBUTES</B> frees an allocated luid and attribute structure by Create_PLUID_AND_ATTRIBUTES.
        Postcondition : privs will be nil.

        @param Privileges contains the array pointing to the first element.  
        raises
 EJwsclSecurityException:  will be raised if privs was not created by Create_PLUID_AND_ATTRIBUTES of the same class instance! 
       }
    procedure Free_PLUID_AND_ATTRIBUTES(var Privileges: PLUID_AND_ATTRIBUTES);
      virtual;

       {<B>Free_PPRIVILEGE_SET</B> frees an allocated set of privileges structure by Create_PPRIVILEGE_SET.
        Postcondition : privs will be nil.

        @param Privileges contains the array pointing to the first element.  
        raises
 EJwsclSecurityException:  will be raised if privs was not created by Create_PPRIVILEGE_SET of the same class instance! 
       }
    procedure Free_PPRIVILEGE_SET(var Privileges: jwaWindows.PPRIVILEGE_SET);
      virtual;

       {<B>Free_PTOKEN_PRIVILEGES</B> frees an allocated luid and attribute structure by Create_PLUID_AND_ATTRIBUTES.
        Postcondition : privs will be nil.

        @param Privileges contains the array pointing to the first element.  
        raises
 EJwsclSecurityException:  will be raised if privs was not created by Create_PLUID_AND_ATTRIBUTES of the same class instance! 
       }
    procedure Free_PTOKEN_PRIVILEGES(var Privileges: PTOKEN_PRIVILEGES);
      virtual;


       {<B>DeletePrivilege</B> removes a privilege with the given index.
        If the privilege set is assigned to a token it simply calls RemoveIrrepealable
        If not the privilege will be removed from the list if it exists.

        raises
 EJwsclInvalidIndexPrivilegeException:  will be raised if the index does not exist 
        }
    procedure DeletePrivilege(Index: integer); overload; virtual;

       {<B>DeletePrivilege</B> removes a privilege from the list.
        If the privilege set is assigned to a token it simply calls RemoveIrrepealable
        If not the privilege will be removed from the list if it exists.

        raises
 EJwsclPrivilegeNotFoundException:  will be raised if the privilege does not exist in list. 
        }
    procedure DeletePrivilege(Privilege: TJwPrivilege); overload; virtual;

       {<B>AddPrivilege</B> adds a privilege to the list if the privilege is not assigned to a token; otherwise
        EJwsclNotImplementedException will be raised.
        If the privilege already exists the exception EJwsclSecurityException will be raised.

        @param LuidAttributes defines an luid structure with attributes. The attributes are ignored 

        raises
 EJwsclSecurityException:  will be raised if the privilege already exists 
         EJwsclNotImplementedException: will be raised if the privilege set belongs to a token 
         EJwsclPrivilegeNotFoundException: will be raised if the privilege was not found on the system 
       }
    function AddPrivilege(LuidAttributes: LUID_AND_ATTRIBUTES): integer;
      overload; virtual;

       {<B>AddPrivilege</B> adds a privilege to the list if the privilege is not assigned to a token; otherwise
        EJwsclNotImplementedException will be raised.
        If the privilege already exists the exception EJwsclSecurityException will be raised.

        @param Luid defines a luid to be added 

        raises
 EJwsclSecurityException:  will be raised if the privilege already exists 
         EJwsclNotImplementedException: will be raised if the privilege set belongs to a token 
         EJwsclPrivilegeNotFoundException: will be raised if the privilege was not found on the system 
       }
    function AddPrivilege(Luid: TLuid): integer; overload; virtual;

       {<B>AddPrivilege</B> adds a privilege to the list if the privilege is not assigned to a token; otherwise
        EJwsclNotImplementedException will be raised.
        If the privilege already exists the exception EJwsclSecurityException will be raised.

        @param HighValue contains the high value of the privilege luid to be added 
        @param LowValue contains the low value of the privilege luid to be added 

        raises
 EJwsclSecurityException:  will be raised if the privilege already exists 
         EJwsclNotImplementedException: will be raised if the privilege set belongs to a token 
         EJwsclPrivilegeNotFoundException: will be raised if the privilege was not found on the system 
       }
    function AddPrivilege(HighValue, LowValue: cardinal): integer;
      overload; virtual;

       {<B>AddPrivilege</B> adds a privilege to the list if the privilege is not assigned to a token; otherwise
        EJwsclNotImplementedException will be raised.
        If the privilege already exists the exception EJwsclSecurityException will be raised.

        @param PrivName contains the privilege name to be added 

        raises
 EJwsclSecurityException:  will be raised if the privilege already exists 
         EJwsclNotImplementedException: will be raised if the privilege set belongs to a token 
         EJwsclPrivilegeNotFoundException: will be raised if the privilege was not found on the system 
       }
    function AddPrivilege(PrivName: TJwString): integer; overload; virtual;

    procedure Clear;


  public
       {The owner token of this privilege set.
        If the privilege set belongs to a token, Owner is not nil.
        In this case the privilege in the list are assigned to a token.

        }
    property Owner: TJwSecurityToken Read fOwner;

       {<B>Control</B> is only used if the privilege was created by Create with parameter
         (aPRIVILEGE_SET : PRIVILEGE_SET).
        Specifies a control flag related to the privileges.
        The PRIVILEGE_SET_ALL_NECESSARY control flag is currently defined.
        It indicates that all of the specified privileges must be held by the
        process requesting access. If this flag is not set, the presence
        of any privileges in the user's access token grants the access.

        http://msdn2.microsoft.com/en-us/library/aa379307.aspx
       }
    property Control: cardinal Read fControl write fControl;

    //<B>Count</B> contains the count of privileges
    property Count: cardinal Read GetCount;

       {<B>PrivByIdx[Index</B> returns a privilege by its index of list
        Be aware that an index can change if the set is updated.
        If the index is not between 0 and Count-1 the Exception
          EJwsclInvalidIndexPrivilegeException is raised.
        }
    property PrivByIdx[Index: cardinal]: TJwPrivilege Read GetPrivByIdx;
       {<B>PrivByName[Index</B> returns a privilege by its name.
        The string is compared case sensitive!
        You can use system constants from JwaWinNT (like SE_CREATE_TOKEN_NAME).
        If the given privilege was not found the result is nil.
        }
    property PrivByName[Index: TJwString]: TJwPrivilege Read GetPrivByName;
       {
       <B>PrivByLUID[Index</B> returns a privilege by its LUID (locally unique ID)
        If the given privilege was not found the result is nil.}
    property PrivByLUID[Index: LUID]: TJwPrivilege Read GetPrivByLUID;
  end;


var {<B>JwProcessHeap</B> contains an handle to the process' heap.
     It is used to allocate memory on the heap.
     On unit initialization it is automatically set using GetProcessHeap
      (see http://msdn2.microsoft.com/en-us/library/aa366569.aspx).
     There is no need to change this value.
     Be aware that in future release this variable can become obsolete because the used
      memory functions are adapted or even replaced.
     }
  JwProcessHeap: cardinal = 0;

type
  TJwPrivilegeQueryType = (pqt_Available, pqt_Enabled);
  TJwPrivilegeSetType   = (pst_Enable, pst_EnableIfAvail, pst_Disable);

{<B>JwIsPrivilegeSet</B> checks whether a given privilege is available or enabled in the current process or thread.
@param Index gets the privilege name
@param query defines whether the given privilege should be checked for availability or is enable
@return Returns true if the privilege is available and enabled. If the privilege is not available or disabled the result is false.
}
function JwIsPrivilegeSet(const Index: TJwString;
  const Query: TJwPrivilegeQueryType = pqt_Available): boolean;

{<B>JwEnablePrivilege</B> en- or disables a given privilege.
@param Index gets the privilege name
@param query defines whether the privilege should be enabled or disabled. Define pst_EnableIfAvail if you dont want to raise an exception if
       the privlege does not exist.
@return Returns the previous state of the privilege, true if it was enabled, otherwise false. If the state is not available and query is pst_Disable
        the return value is false.
raises
 EJwsclPrivilegeException:  will be raised if the privilege is not available and query is pst_Enable,
        otherwise the return value is false. If query is pst_EnableIfAvail the return is false, if the privilege could not be enabled.
        In this case no exception is raised.
}
function JwEnablePrivilege(const Index: TJwString;
  const Query: TJwPrivilegeSetType): boolean;

{<B>JwGetPrivilegesText</B> returns a string filled with privilege names (of current token) and their states seperated by #13#10.
SE_XXXXX [enabled]
SE_XXXXX [disabled]
}
function JwGetPrivilegesText: TJwString; overload;

{<B>JwGetPrivilegesText</B> returns a string filled with privilege names (of current token) and their states seperated by #13#10.
SE_XXXXX [enabled]
SE_XXXXX [disabled]
This function returns the status of the given privileges in parameter DisplayPrivileges.
@param DisplayPrivileges defines an array of privilege names that are checked for status and availability. 
}

function JwGetPrivilegesText(
  const DisplayPrivileges: array of TJwString): TJwString;
  overload;

{<B>JwIsMemberOfAdministratorsGroup</B> checks if the user is a member of the Administrators group
and this group is enabled for access checking. Therefore if this group has the flag use-for-deny-only
the returned value is false. This usually happens if the user is an Administrator in Windows Vista with
activated UAC.
 
@return Returns true if the user is a member of the administrators group; otherwise false.

Remarks
This function uses the Windows API function CheckTokenMembership. 

JwIsMemberOfAdministratorsGroup is equal to JwCheckAdministratorAccess.

}
function JwIsMemberOfAdministratorsGroup: boolean;

{<B>JwCheckAdministratorAccess</B> checks if the user is a member of the Administrators group
and this group is enabled for access checking. Therefore if this group has the flag use-for-deny-only
the returned value is false. This usually happens if the user is an Administrator in Windows Vista with
activated UAC.

@return Returns true if the user has administrative access; otherwise false.

Remarks
This function mimics the Windows API function CheckTokenMembership. 

JwCheckAdministratorAccess is equal to JwIsMemberOfAdministratorsGroup.
}
function JwCheckAdministratorAccess: boolean;

{<B>JwGetProcessLogonSession</B> returns the logon session ID of the given process.
By default the current process is used
@param ProcessID defines the process ID to be used to retrieve the session ID.
   By default (value: -1) the current process is used.
@return A integer value that defines the process session ID }
function JwGetProcessLogonSession(ProcessID : TJwProcessId = Cardinal(-1)) : Cardinal;

{<B>JwIsSystem</B> returns true if the current process is running in the system context.
It does not matter whether the process is in fact a service or not.}
function JwIsSystem : Boolean;

{<B>JwIsUACEnabled</B> checks whether the current Windows has UAC enabled.

@return
Returns true if UAC is enabled; otherwise if the Windows version does not
support UAC the return value is false.

Remarks
On Windows Systems that do not support UAC the return value is always false.
 }
function JwIsUACEnabled: Boolean;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses JwsclKnownSid, JwsclMapping, JwsclSecureObjects, JwsclProcess,
     JwsclTerminalServer, JwsclLsa, TypInfo,
      JwsclPrivileges, Math, D5impl;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

function JwIsUACEnabled: Boolean;
  function IsLUA : Boolean;
  var
    Key: HKEY;
    //DataType: DWORD;
    Size : DWORD;
    Data : Pointer;
  begin
    result := false;
    if RegOpenKeyExW(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\Policies\System', 0, KEY_READ, Key) = HRESULT(ERROR_SUCCESS) then
    begin
      Result := RegQueryValueExW(Key, 'EnableLUA', nil, nil, nil, @Size) = HRESULT(ERROR_SUCCESS);
      if Result then
      begin
        GetMem(Data, Size);
        try
          if RegQueryValueExW(Key, 'EnableLUA', nil, nil, Data, @Size) = HRESULT(ERROR_SUCCESS) then
            result := Boolean(Data^);
        finally
          FreeMem(Data);
        end;
      end;
      RegCloseKey(Key);
    end
    else
    RaiseLastOSError;
  end;

begin
  Result := (TJwWindowsVersion.IsWindowsVista(true)
      or TJwWindowsVersion.IsWindows2008(true)) and
        IsLUA;
end;

function JwGetProcessLogonSession(ProcessID : TJwProcessId = Cardinal(-1)) : Cardinal;
var T : TJwSecurityToken;
begin
  if ProcessID = Cardinal(-1) then
    ProcessID := GetCurrentProcessId;

  if not ProcessIdToSessionId(ProcessID, result) then
  begin
    T := TJwSecurityToken.CreateTokenByProcessId(ProcessID, TOKEN_READ or TOKEN_QUERY);
    try
      result := T.TokenSessionId;
    finally
      T.Free;
    end;
  end;
end;



function JwIsSystem : Boolean;
var T : TJwSecurityToken;
    Stat : TJwSecurityTokenStatistics;
begin
  T := TJwSecurityToken.CreateTokenByProcess(0, TOKEN_READ or TOKEN_QUERY);
  try
    Stat := T.GetTokenStatistics;

    //Compare the authentication ID with 0x999d (SYSTEM)
    result := CompareMem(@Stat.fAuthenticationId, @SYSTEM_LUID, sizeof(TLUID));
    Stat.Free;
  finally
    T.Free;
  end;
end;

function JwCheckAdministratorAccess: boolean;
var
  SD: TJwSecurityDescriptor;
begin
  if not Assigned(JwAdministratorsSID) then
    JwInitWellKnownSIDs;

  SD := TJwSecurityDescriptor.Create;
  try
    SD.PrimaryGroup := JwNullSID;
    SD.Owner   := JwAdministratorsSID;
    SD.OwnDACL := True;

    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,
      [], STANDARD_RIGHTS_ALL, JwAdministratorsSID, False));

    Result := TJwSecureGeneralObject.AccessCheck(SD, nil,
      STANDARD_RIGHTS_ALL, TJwSecurityGenericMapping);
  finally
    FreeAndNil(SD);
  end;
end;

function JwIsMemberOfAdministratorsGroup: boolean;
var
  Token: TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or
    TOKEN_DUPLICATE);
  try
    Token.ConvertToImpersonatedToken(SecurityImpersonation, MAXIMUM_ALLOWED);
    Result := Token.CheckTokenMembership(JwAdministratorsSID)
  finally
    FreeAndNil(Token);
  end;
end;



function JwGetPrivilegesText(
  const DisplayPrivileges: array of TJwString): TJwString;
var
  t:    TJwSecurityToken;
  Priv: TJwPrivilege;
  s:    TJwPrivilegeSet;
  i:    integer;
begin
  Result := '';

  t := TJwSecurityToken.CreateTokenEffective(TOKEN_DUPLICATE or
    TOKEN_READ or TOKEN_ADJUST_PRIVILEGES);
  try
    s := t.GetTokenPrivileges;

    for i := low(DisplayPrivileges) to high(DisplayPrivileges) do
    begin
      Priv := s.PrivByName[DisplayPrivileges[i]];
      if Assigned(Priv) then
      begin
        if Priv.Enabled then
          Result := Result + #13#10 + Priv.Name + ' ' + RsTokenEnabledText
        else
          Result := Result + #13#10 + Priv.Name + ' ' + RsTokenDisabledText;
      end
      else
        Result := Result + #13#10 + TJwString(DisplayPrivileges[i]) + ' ' +
          RsTokenUnavailableText;
    end;

    s.Free;
  finally
    t.Free;
  end;
end;

function JwGetPrivilegesText: TJwString;
var
  t: TJwSecurityToken;
  s: TJwPrivilegeSet;
  i: integer;
  //[Hint] b : Boolean;
begin
  Result := '';

  t := TJwSecurityToken.CreateTokenEffective(TOKEN_DUPLICATE or
    TOKEN_READ or TOKEN_ADJUST_PRIVILEGES);
  try
    s := t.GetTokenPrivileges;


    for i := 0 to s.Count - 1 do
    begin
      if (s.GetPrivByIdx(i).Enabled) then
        Result := Result + #13#10 + s.GetPrivByIdx(i).Name +
          ' ' + RsTokenEnabledText
      else
        Result := Result + #13#10 + s.GetPrivByIdx(i).Name +
          ' ' + RsTokenDisabledText;
    end;

    s.Free;
  finally
    t.Free;
  end;
end;



function JwEnablePrivilege(const Index: TJwString;
  const Query: TJwPrivilegeSetType): boolean;
var
  t: TJwSecurityToken;
begin
  if not JwIsPrivilegeSet(Index, pqt_Available) then
  begin
    if (query = pst_Enable) then
      raise EJwsclPrivilegeException.CreateFmtEx(
        RsTokenPrivilegeNotAvailable, 'JwEnablePrivilege',
        RsTokenGlobalClassName, RsUNToken, 0, False, [Index])
    else
      Result := False;

    exit;
  end;

  Result := JwIsPrivilegeSet(Index, pqt_Enabled);

  t := TJwSecurityToken.CreateTokenEffective(TOKEN_DUPLICATE or
    TOKEN_READ or TOKEN_ADJUST_PRIVILEGES);
  try
    t.PrivilegeEnabled[Index] :=
      (query = pst_Enable) or (query = pst_EnableIfAvail);
  finally
    t.Free;
  end;
end;

function JwIsPrivilegeSet(const Index: TJwString;
  const Query: TJwPrivilegeQueryType = pqt_Available): boolean;
var
  t: TJwSecurityToken;
begin
  t := TJwSecurityToken.CreateTokenEffective(TOKEN_DUPLICATE or
    TOKEN_READ or TOKEN_QUERY or TOKEN_WRITE);

  try
    Result := t.IsPrivilegeAvailable(Index);

    if Result and (query = pqt_Enabled) then
      Result := t.PrivilegeEnabled[Index];
  except
    Result := False;
  end;

  t.Free;
end;

{**************** TJwPrivilegeSet ******************}



constructor TJwPrivilegeSet.Create(Owner: TJwSecurityToken;
  PrivilegesPointer: PTOKEN_PRIVILEGES);
var
  i: integer;
begin
  //Todo: check second parameter for nil

  Self.Create;
  fOwner   := Owner;
  fControl := 0;

  for i := 0 to PrivilegesPointer.PrivilegeCount - 1 do
  begin
    fList.Add(TJwPrivilege.Create(Self, PrivilegesPointer.Privileges[i]));
  end;
end;

constructor TJwPrivilegeSet.Create(Owner: TJwSecurityToken;
  PrivilegeObject: TJwPrivilegeSet);

var
  i: integer;
  aPrivileges: jwaWindows.PPRIVILEGE_SET;
begin
  //Todo: check second parameter for invalid data

  Self.Create;
  fOwner := Owner;

  fControl := 0;

  if Assigned(PrivilegeObject) then
  begin
    aPrivileges := PrivilegeObject.Create_PPRIVILEGE_SET;

    try
      for i := 0 to aPrivileges.PrivilegeCount - 1 do
      begin
        fList.Add(TJwPrivilege.Create(Self, aPrivileges.Privilege[i]));
      end;
    finally
      Free_PPRIVILEGE_SET(aPrivileges);
    end;
  end;
end;

constructor TJwPrivilegeSet.Create(Owner: TJwSecurityToken;
  Privileges: jwaWindows.TPrivilegeSet);

var
  i: integer;
begin
  //Todo: check second parameter for nil
  Self.Create;

  fOwner := Owner;

  fControl := Privileges.Control;

  for i := 0 to Privileges.PrivilegeCount - 1 do
  begin
    fList.Add(TJwPrivilege.Create(Self, Privileges.Privilege[i]));
  end;
end;

constructor TJwPrivilegeSet.Create;
begin
  fList  := TObjectList.Create(True);
  fPPrivilegesList := TList.Create;
  fPPrivilegesSetList := TList.Create;
  fOwner := nil;
end;

destructor TJwPrivilegeSet.Destroy;

  procedure ClearPrivilegeList;
  var
    i: integer;
    p: PLUID_AND_ATTRIBUTES;
  begin
    for i := fPPrivilegesList.Count - 1 downto 0 do
    begin
      p := PLUID_AND_ATTRIBUTES(fPPrivilegesList.Items[i]);
      try
        Free_PLUID_AND_ATTRIBUTES(p); //BUBBUG here sometimes
      except
      end;
    end;

    FreeAndNil(fPPrivilegesList);
  end;

  procedure ClearPrivilegeSetList;
  var
    i: integer;
    p: PPRIVILEGE_SET;
  begin
    for i := fPPrivilegesSetList.Count - 1 downto 0 do
    begin
      p := PPRIVILEGE_SET(fPPrivilegesSetList.Items[i]);
      try
        Free_PPRIVILEGE_SET(p);
      except
      end;
    end;

    FreeAndNil(fPPrivilegesSetList);
  end;

begin
  FreeAndNil(fList);
  ClearPrivilegeList;
  ClearPrivilegeSetList;

  if Assigned(Owner) then
    Owner.fPrivelegesList.Remove(Self);
  inherited;
end;


function TJwPrivilegeSet.GetText: TJwString;
var
  i:    integer;
  priv: TJwPrivilege;
begin
  Result := '';
  for i := 0 to Self.Count - 1 do
  begin
    priv   := TJwPrivilege(fList.Items[i]);
    Result := Result + priv.GetText + #13#10#13#10;
  end;
end;

function TJwPrivilegeSet.GetPrivByIdx(Index: cardinal): TJwPrivilege;
begin
  if (Index >= Count) then
    raise EJwsclInvalidIndexPrivilegeException.CreateFmtEx(
      RsTokenInvalidPrivilegeIndex, 'GetPrivByIdx', ClassName,
      RsUNToken, 0, False, [Index, Count]);

  Result := TJwPrivilege(fList.Items[Index]);
end;

function TJwPrivilegeSet.GetPrivByName(Index: TJwString): TJwPrivilege;
var
  i: integer;
  p: TJwPrivilege;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    p := TJwPrivilege(fList.Items[i]);
    if Index = p.Name then
    begin
      Result := p;
      exit;
    end;
  end;
end;



function TJwPrivilegeSet.GetPrivByLUID(Index: LUID): TJwPrivilege;
var
  i: integer;
  p: TJwPrivilege;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    p := TJwPrivilege(fList.Items[i]);
    if (Index.LowPart = p.LUID.LowPart) and (Index.HighPart =
      p.LUID.HighPart) then
    begin
      Result := p;
      exit;
    end;
  end;
end;

function TJwPrivilegeSet.GetCount: cardinal;
begin
  if Assigned(fList) then
    Result := fList.Count
  else
    Result := 0;
end;

procedure TJwPrivilegeSet.RemoveIrrepealable(Privilege: TJwPrivilege);
var
  privs: TOKEN_PRIVILEGES;
  tcbPrevState: boolean;
begin
  if not Assigned(Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenRemovePrivilegeDenied, 'RemoveIrrepealable',
      ClassName, RsUNToken, 0, False, []);

  Owner.CheckTokenHandle('RemoveIrrepealable');
  Owner.CheckTokenAccessType(TOKEN_QUERY + TOKEN_ADJUST_PRIVILEGES,
    'TOKEN_QUERY,TOKEN_ADJUST_PRIVILEGES',
    'TJwPrivilegeSet.RemoveIrrepealable');

  if not Assigned(PrivByName[SE_TCB_NAME]) then
    raise EJwsclAdjustPrivilegeException.CreateFmtEx(
      RsTokenRemovePrivilegeDeniedByPrivilege, 'RemoveIrrepealable',
      ClassName, RsUNToken, 0, False, [Privilege.Name]);

  tcbPrevState := PrivByName[SE_TCB_NAME].Enabled;
  PrivByName[SE_TCB_NAME].Enabled := True;


  privs.PrivilegeCount     := 1;
  privs.Privileges[0].Luid := Privilege.LUID;
  privs.Privileges[0].Attributes := SE_PRIVILEGE_REMOVED;


  SetLastError(0);
  if (not AdjustTokenPrivileges(Owner.TokenHandle, False, @privs,
    0, nil, nil)) then
    raise EJwsclAdjustPrivilegeException.CreateFmtEx(
      RsPrivilegeCallAdjustTokenFailed, 'SetEnabled', ClassName,
      RsUNToken, 0, True, [Privilege.Name]);

  //free memory and remove from privileges list
  fList.Remove(Privilege);

  PrivByName[SE_TCB_NAME].Enabled := tcbPrevState;
end;

procedure TJwPrivilegeSet.DisableAllPrivileges;
var
  privs: PTOKEN_PRIVILEGES;
  i:     integer;
begin
  if not Assigned(Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenRemovePrivilegeDenied, 'RemoveIrrepealable',
      ClassName, RsUNToken, 0, False, []);

  Owner.CheckTokenHandle('RemoveIrrepealable');
  Owner.CheckTokenAccessType(TOKEN_QUERY + TOKEN_ADJUST_PRIVILEGES,
    'TOKEN_QUERY,TOKEN_ADJUST_PRIVILEGES',
    'TJwPrivilegeSet.RemoveIrrepealable');

  if (not AdjustTokenPrivileges(Owner.TokenHandle, True, nil,
    0, nil, nil)) then
    raise EJwsclAdjustPrivilegeException.CreateFmtEx(
      RsWinCallFailed, 'DisableAllPrivileges', ClassName,
      RsUNToken, 0, True, ['AdjustToken']);

  fList.Clear;

  Owner.GetTokenInformation(Owner.TokenHandle, TokenPrivileges,
    Pointer(privs));

  for i := 0 to privs.PrivilegeCount - 1 do
  begin
    fList.Add(TJwPrivilege.Create(Self, privs.Privileges[i]));
  end;

  HeapFree(JwProcessHeap, 0, privs);
end;

function TJwPrivilegeSet.Create_PLUID_AND_ATTRIBUTES: PLUID_AND_ATTRIBUTES;
type
  TArrayLuids = array of LUID_AND_ATTRIBUTES;
var
  i: integer;
  p: ^TArrayLuids;
begin
  Result := nil;

  if Count = 0 then
    exit;

  Result := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY,
    sizeof(TLuidAndAttributes) * Count);

  fPPrivilegesList.Add(Result);

  p := @Result;

  for i := 0 to Count - 1 do
  begin
    p^[i].Luid := PrivByIdx[i].LUID;
    p^[i].Attributes := 0;
  end;
end;

procedure TJwPrivilegeSet.Free_PLUID_AND_ATTRIBUTES(
  var Privileges: PLUID_AND_ATTRIBUTES);
begin
  if Privileges = nil then
    exit;

  if fPPrivilegesList.IndexOf(Privileges) < 0 then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenInvalidPrivilegePointer, 'Free_PLUID_AND_ATTRIBUTES',
      ClassName, RsUNToken, 0, False, []);

  fPPrivilegesList.Remove(Privileges);
  HeapFree(JwProcessHeap, 0, Privileges);

  Privileges := nil;
end;

procedure TJwPrivilegeSet.Free_PTOKEN_PRIVILEGES(
  var Privileges: PTOKEN_PRIVILEGES);
begin
  if Privileges = nil then
    exit;
 { if fPPrivilegesList.IndexOf(Privileges) < 0 then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenInvalidPrivilegePointer,
      'Free_PTOKEN_PRIVILEGES', ClassName, RsUNToken,
      0, False, []);   }

  fPPrivilegesList.Remove(Privileges);
  HeapFree(JwProcessHeap, 0, Privileges);

  Privileges := nil;
end;

function TJwPrivilegeSet.Create_PTOKEN_PRIVILEGES:
jwaWindows.PTOKEN_PRIVILEGES;
var
  size, i: integer;

begin
  size   := sizeof(jwaWindows.PTOKEN_PRIVILEGES) +
    sizeof(TLuidAndAttributes) * Count;
  Result := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY, size);

  Result^.PrivilegeCount := Count;

  fPPrivilegesSetList.Add(Result);

  for i := 0 to Count - 1 do
  begin
    Result^.Privileges[i].Luid := PrivByIdx[i].LUID;
    Result^.Privileges[i].Attributes := PrivByIdx[i].Attributes;
    //SE_PRIVILEGE_ENABLED_BY_DEFAULT or SE_PRIVILEGE_ENABLED;
  end;
end;

function TJwPrivilegeSet.Create_PPRIVILEGE_SET: jwaWindows.PPRIVILEGE_SET;
var
  i: integer;
begin
  Result := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY,
    sizeof(jwaWindows.TPrivilegeSet) - sizeof(TLuidAndAttributes) +
    sizeof(TLuidAndAttributes) * Count);

  Result^.PrivilegeCount := Count;
  Result^.Control := fControl;

  fPPrivilegesSetList.Add(Result);

  for i := 0 to Count - 1 do
  begin
    Result^.Privilege[i].Luid := PrivByIdx[i].LUID;
    Result^.Privilege[i].Attributes := 0;
  end;
end;

procedure TJwPrivilegeSet.Free_PPRIVILEGE_SET(
  var Privileges: jwaWindows.PPRIVILEGE_SET);
begin
  if Privileges = nil then
    exit;


 {HeapFree(JwProcessHeap, 0, Pointer(Privileges));
  fPPrivilegesSetList.Remove(Privileges);}
  
  fPPrivilegesSetList.Remove(Privileges);
  HeapFree(JwProcessHeap, 0, Pointer(Privileges));

  Privileges := nil;
end;


procedure TJwPrivilegeSet.DeletePrivilege(Privilege: TJwPrivilege);
var
  i: integer;
begin
  if not Assigned(Privilege) then
    exit;

  i := fList.IndexOf(Privilege);
  if i >= 0 then
    DeletePrivilege(i)
  else
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
      RsTokenPrivlegeNotInList, 'DeletePrivilege', ClassName,
      RsUNToken, 0, True, []);
end;

procedure TJwPrivilegeSet.DeletePrivilege(Index: integer);
begin
  if Assigned(Owner) then
    RemoveIrrepealable(PrivByIdx[Index])
  else
  begin
    fList.Delete(Index);
  end;
end;

procedure TJwPrivilegeSet.Clear;
var
  i: integer;
begin
  if Assigned(Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenPrivilegeAssignDenied, 'AddPrivilege', ClassName,
      RsUNToken, 0, False, []);

  for i := Count - 1 downto 0 do
  begin
    Self.DeletePrivilege(i);
  end;
end;

function TJwPrivilegeSet.AddPrivilege(PrivName: TJwString): integer;
var
  aLUID: Luid;
begin
  if not
  {$IFDEF UNICODE}LookupPrivilegeValueW{$ELSE}
    LookupPrivilegeValueA
{$ENDIF}
    ((''), TJwPChar(PrivName), aLUID) then
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
      RsTokenCallLookUpPrivilegeValueFailed, 'AddPrivilege',
      ClassName, RsUNToken, 0, True, [PrivName]);
  Result := AddPrivilege(aLUID);
end;

function TJwPrivilegeSet.AddPrivilege(HighValue, LowValue: cardinal): integer;
var
  aLuid: TLuid;
begin
  aLuid.LowPart := HighValue;
  aLuid.HighPart := LowValue;
  Result := AddPrivilege(aLuid);
end;

function TJwPrivilegeSet.AddPrivilege(Luid: TLuid): integer;
var
  la: LUID_AND_ATTRIBUTES;
begin
  la.Luid := Luid;
  la.Attributes := 0;
  Result  := AddPrivilege(la);
end;


function TJwPrivilegeSet.AddPrivilege(LuidAttributes:
  LUID_AND_ATTRIBUTES): integer;
var
  i: integer;
begin
  if Assigned(Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenPrivilegeAssignDenied, 'AddPrivilege', ClassName,
      RsUNToken, 0, False, [])
  else
  begin
    for i := 0 to fList.Count - 1 do
    begin
      if (TJwPrivilege(fList.Items[i]).LUID.LowPart =
        LuidAttributes.Luid.LowPart) and
        (TJwPrivilege(fList.Items[i]).LUID.HighPart =
        LuidAttributes.Luid.HighPart) then
        raise EJwsclSecurityException.CreateFmtEx(
          RsTokenPrivilegeAlreadyInList, 'AddPrivilege', ClassName,
          RsUNToken, 0, False, [TJwPrivilege(fList.Items[i]).LUID.HighPart,
          TJwPrivilege(fList.Items[i]).LUID.LowPart]);
    end;


    Result := fList.Add(TJwPrivilege.Create(self, LuidAttributes));
  end;
end;



{**************** TJwPrivilege ******************}

constructor TJwPrivilege.Create(anOwner: TJwPrivilegeSet;
  aLUID_AND_ATTRIBUTES: LUID_AND_ATTRIBUTES);

  function GetName(aLUID: jwaWindows.LUID): TJwString;
  var
    len:   cardinal;
    sName: TJwPChar;

  begin
    if not Assigned(anOwner) then
      raise EJwsclInvalidOwnerException.CreateFmtEx(
        RsNilParameter, 'Create', ClassName, RsUNToken, 0, False, ['Owner']);

    Result := '';
    len    := 0;
    {$IFDEF UNICODE}LookupPrivilegeNameW{$ELSE}
    LookupPrivilegeNameA
{$ENDIF}
    ('', LUID, '', len);

    sName := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY, (len + 1) *
      sizeof(TJwChar));

    if (sName = nil) then
      raise EJwsclNotEnoughMemory.CreateFmtEx(
        RsTokenNotEnoughMemoryForPrivName, 'Create', ClassName,
        RsUNToken, 0, False, []);

    if
{$IFDEF UNICODE}LookupPrivilegeNameW{$ELSE}
    LookupPrivilegeNameA
{$ENDIF}
      ('', LUID, sName, len) then
      Result := sName;

    HeapFree(JwProcessHeap, 0, sName);
  end;

  function GetDisplayName(const aName: TJwString;
    out aLanguageID: cardinal): TJwString;
  var
    len:   cardinal;
    sName: TJwPChar;

  begin
    Result := '';

    fOwner := anOwner;

    Len := 0;
    {$IFDEF UNICODE}LookupPrivilegeDisplayNameW{$ELSE}
    LookupPrivilegeDisplayNameA
{$ENDIF}
    ('', TJwPChar(aName), '', len, aLanguageID);

    sName := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY, (len + 1) *
      sizeof(TJwChar));

    if
{$IFDEF UNICODE}LookupPrivilegeDisplayNameW{$ELSE}
    LookupPrivilegeDisplayNameA
{$ENDIF}
      (nil, TJwPChar(aName), sName, len, aLanguageID) then
      Result := sName;

    HeapFree(JwProcessHeap, 0, sName);
  end;

begin
  fAttributes := aLUID_AND_ATTRIBUTES.Attributes;
  fLUID := aLUID_AND_ATTRIBUTES.Luid;

  fName := GetName(LUID);
  fPrivilege_Used_For_Access := False;
  fDisplayName := GetDisplayName(Self.Name, fLanguageID);

  fPrivilege_Enabled_By_Default := IsEnabledByDefault;
end;

class function TJwPrivilege.MakeLUID_AND_ATTRIBUTES(const LowPart: cardinal;
  const HighPart: LONG; Attributes: cardinal): TLuidAndAttributes;
begin
  Result.Luid := MakeLUID(LowPart, HighPart);
  Result.Attributes := Attributes;
end;

class function TJwPrivilege.MakeLUID_AND_ATTRIBUTES(const Luid: TLuid;
  Attributes: cardinal): TLuidAndAttributes;
begin
  Result.Luid := Luid;
  Result.Attributes := Attributes;
end;


class function TJwPrivilege.MakeLUID(const LowPart: cardinal;
  const HighPart: LONG): TLuid;
begin
  Result.LowPart  := LowPart;
  Result.HighPart := HighPart;
end;

class function TJwPrivilege.TextToLUID(const Name: TJwString;
  const SystemName: TJwString = ''): TLuid;
begin
  Result := LUID_INVALID;
  if not
{$IFDEF UNICODE}LookupPrivilegeValueW{$ELSE}
    LookupPrivilegeValueA
{$ENDIF}
    (TJwPChar(SystemName), TJwPChar(Name), Result) then

    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'TextToLUID', ClassName, RsUNToken, 0,
      True, ['LookupPrivilegeValue']);

end;

class function TJwPrivilege.LUIDtoText(aLUID: LUID): TJwString;
var
  i: int64;
  s: integer;
begin
  s := sizeof(aLUID.LowPart);
  i := aLUID.HighPart;
  i := i shl s;
  i := i or aLUID.LowPart;


  Result := JwFormatString(RsPrivilegeLuidText,
    [aLUID.HighPart, aLUID.LowPart, i]);
end;

class function TJwPrivilege.PrivilegeAttributeToText(
  PrivilegeAttributes: cardinal; HumanReadable: boolean = False): TJwString;
begin
  Result := '';
  if PrivilegeAttributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT =
    SE_PRIVILEGE_ENABLED_BY_DEFAULT then
  begin
    if HumanReadable then
      Result := ',' + RsPrivilegeEnabledByDefault
    else
      Result := ',SE_PRIVILEGE_ENABLED_BY_DEFAULT';
    PrivilegeAttributes := PrivilegeAttributes and not
      SE_PRIVILEGE_ENABLED_BY_DEFAULT;
  end;
  if PrivilegeAttributes and SE_PRIVILEGE_ENABLED = SE_PRIVILEGE_ENABLED then
  begin
    if HumanReadable then
      Result := ',' + RsPrivilegeEnabled
    else
      Result := Result + ',SE_PRIVILEGE_ENABLED';
    PrivilegeAttributes := PrivilegeAttributes and not SE_PRIVILEGE_ENABLED;
  end;
  if PrivilegeAttributes and SE_PRIVILEGE_USED_FOR_ACCESS =
    SE_PRIVILEGE_USED_FOR_ACCESS then
  begin
    if HumanReadable then
      Result := ',' + RsPrivilegeRemoved
    else
      Result := Result + ',SE_PRIVILEGE_USED_FOR_ACCESS';
    PrivilegeAttributes := PrivilegeAttributes and not
      SE_PRIVILEGE_USED_FOR_ACCESS;
  end;


  if PrivilegeAttributes > 0 then
    Result := Result + ',' + JwFormatString(RsPrivilegeUnknown,
      [PrivilegeAttributes]);
  //Result := Result + ',(unknown attributes)';

  if Length(Result) > 0 then
    System.Delete(Result, 1, 1)
  else
    Result := RsPrivilegeNone;
end;

function TJwPrivilege.GetText: TJwString;
begin
  Result := JwFormatString(RsPrivilegeFormatText,
    [LUIDtoText(LUID), Name, DisplayName,
    PrivilegeAttributeToText(fAttributes)]);

{  Result := 'LUID       : ' + LUIDtoText(LUID) + #13#10 +
    'Name       : ' + Name + #13#10 + 'DisplayName : ' +
    DisplayName + #13#10 + 'Attributes : ' +
    PrivilegeAttributeToText(fAttributes) + #13#10;
  }
end;

function TJwPrivilege.GetEnabled: boolean;
var
  privs, prevState: TOKEN_PRIVILEGES;

  preLen: cardinal;
begin
  if not Assigned(Owner.Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenNotAssignedPrivilege, 'GetEnabled', ClassName,
      RsUNToken, 0, False, []);

  Owner.Owner.CheckTokenHandle('get TJwPrivilege.Enabled');
  Owner.Owner.CheckTokenAccessType(TOKEN_QUERY + TOKEN_ADJUST_PRIVILEGES,
    'TOKEN_QUERY,TOKEN_ADJUST_PRIVILEGES',
    'TJwPrivilege.SetEnabled');


  privs.PrivilegeCount     := 1;
  privs.Privileges[0].Luid := Self.LUID;
  privs.Privileges[0].Attributes := 0; //disable privilege

  (*
  To get privilege attributes, we first have to set them, get the previous state
  and reset it to the previous state
  *)

  Fillchar(prevState, sizeof(prevState), 0);
  preLen := 0;
  if (not AdjustTokenPrivileges(Owner.Owner.TokenHandle, False,
    @privs, sizeof(privs), @prevState, @preLen)) then
  begin
    raise EJwsclAdjustPrivilegeException.CreateFmtEx(
      RsPrivilegeCallAdjustTokenFailed, 'GetEnabled', ClassName,
      RsUNToken, 0, True, [Self.Name]);
  end;

  try
    //if prevState.PrivilegeCount is zero, no privilege was changed, and we do not need to restore it. So omit that:
    if (prevState.PrivilegeCount = 1) then
    begin
      if (not AdjustTokenPrivileges(Owner.Owner.TokenHandle,
        False, @prevState, preLen, nil, nil)) then
      begin
        raise EJwsclAdjustPrivilegeException.CreateFmtEx(
          RsPrivilegeCallAdjustTokenFailed1, 'GetEnabled',
          ClassName, RsUNToken, 0, True, [Self.Name]);
      end;
    end;
  finally
  end;

  fAttributes := prevState.Privileges[0].Attributes;

  Result := (Attributes and SE_PRIVILEGE_ENABLED = SE_PRIVILEGE_ENABLED) or
    (Attributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT =
    SE_PRIVILEGE_ENABLED_BY_DEFAULT);
end;

function TJwPrivilege.IsEnabledByDefault: boolean;
begin
  Result := Attributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT =
    SE_PRIVILEGE_ENABLED_BY_DEFAULT;
end;

procedure TJwPrivilege.SetEnabled(const en: boolean);
var
  privs: TOKEN_PRIVILEGES;
begin
  if not Assigned(Owner.Owner) then
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenNotAssignedPrivilege, 'SetEnabled', ClassName,
      RsUNToken, 0, False, []);

  Owner.Owner.CheckTokenHandle('set TJwPrivilege.Enabled');
  Owner.Owner.CheckTokenAccessType(TOKEN_QUERY + TOKEN_ADJUST_PRIVILEGES,
    'TOKEN_QUERY,TOKEN_ADJUST_PRIVILEGES',
    'TJwPrivilege.SetEnabled');

  privs.PrivilegeCount     := 1;
  privs.Privileges[0].Luid := Self.LUID;

  if en then
    if fPrivilege_Enabled_By_Default then
      //also reset enabled by default attribute if it was originally specified
      privs.Privileges[0].Attributes :=
        SE_PRIVILEGE_ENABLED or SE_PRIVILEGE_ENABLED_BY_DEFAULT
    else
      privs.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
  else
    privs.Privileges[0].Attributes := 0;


  if (not AdjustTokenPrivileges(Owner.Owner.TokenHandle, False,
    @privs, 0, nil, nil)) then
    raise EJwsclAdjustPrivilegeException.CreateFmtEx(
      RsPrivilegeCallAdjustTokenFailed, 'SetEnabled', ClassName,
      RsUNToken, 0, True, [Self.Name]);

  fAttributes := privs.Privileges[0].Attributes;
end;

procedure TJwPrivilege.RemoveIrrepealable;
begin
  Owner.RemoveIrrepealable(Self);
end;



{**************** TJwSecurityToken ******************}

constructor TJwSecurityToken.Create;
begin
  inherited;

  fPrivelegesList      := TObjectList.Create(False);
  fStackPrivilegesList := TObjectList.Create(True);

  //  fSecurityIDList := TJwSecurityIdList.Create(true);

  fTokenHandle := 0;
  fShared      := True;

  fClassHash := 0;
end;

procedure TJwSecurityToken.Done;
//[Hint] var i : Integer;
begin
  fPrivelegesList.Free;
  fPrivelegesList := nil;

  //!!hier noch privs zurückpopen
  fStackPrivilegesList.Free;
  fStackPrivilegesList := nil;
end;

destructor TJwSecurityToken.Destroy;
begin
  inherited;

  //close handle if not shared
  if not Shared then
    CloseHandle(fTokenHandle);

  //invalidate it  
  fTokenHandle := 0;

  Done;
  //  fSecurityIDList.Free;
end;

{function TJwSecurityToken.PopPrivileges: cardinal;
var
  Privs, PrivPop: TJwPrivilegeSet;
  i:    integer;
  Priv: TJwPrivilege;
begin
  Result := 0;

  if (fStackPrivilegesList.Count = 0) then
    exit;
  if not Assigned(fStackPrivilegesList) then
    fStackPrivilegesList := TObjectList.Create;

  Privs   := GetTokenPrivileges;
  PrivPop := fStackPrivilegesList[fStackPrivilegesList.Count - 1] as
    TJwPrivilegeSet;
  for i := 0 to Privs.Count - 1 do
  begin
    try
      Priv := Privs.GetPrivByName(PrivPop.PrivByIdx[i].Name);
    except
      Priv := nil;
    end;
    if Assigned(Priv) then
    begin
      Priv.Enabled := PrivPop.PrivByIdx[i].Enabled;
    end;
  end;
  fStackPrivilegesList.Delete(fStackPrivilegesList.Count - 1);

  Result := fStackPrivilegesList.Count;
end;      }

{function TJwSecurityToken.PushPrivileges: cardinal;
var
  Privs: TJwPrivilegeSet;
begin
  Privs := GetTokenPrivileges;
  fStackPrivilegesList.Add(Privs);

  Result := fStackPrivilegesList.Count;
end;}

procedure TJwSecurityToken.CheckTokenHandle(sSourceProc: TJwString);
begin
  //TODO: check also non null tokens
  if fTokenHandle = 0 then
    raise EJwsclInvalidTokenHandle.CreateFmtEx(
      RsTokenInvalidTokenHandle, sSourceProc, ClassName, RsUNToken,
      0, False, []);
end;

function TJwSecurityToken.GetMaximumAllowed: TAccessMask;
var
  SD: TJwSecurityDescriptor;
  temp, Token: TJwSecurityToken;
begin
 // begin
  try
    {First try a simple solution without TOKEN_DUPLICATE
    This way we get the original access to this token.
    Maybe the token belongs to another process.
     If we duplicate it we would get more power over it because
     it is spawned into this process
    }

    SD := TJwSecureGeneralObject.GetSecurityInfo(
      TokenHandle,//const aHandle: THandle;
      SE_KERNEL_OBJECT,  //const aObjectType: TSeObjectType;
      [siOwnerSecurityInformation, siGroupSecurityInformation,
      siDaclSecurityInformation]//aSecurityInfo: TJwSecurityInformationFlagSet;
      );


  except
    //Make sure that the given token has enough access rights
    //to get the DACL
    //Strangely enough, XP needs TOKEN_DUPLICATE to work correctly

   // ShowMessage('except');
    temp := TJwSecurityToken.CreateDuplicateExistingToken(TokenHandle,
      TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE);

    try
      SD := TJwSecureGeneralObject.GetSecurityInfo(
        temp.TokenHandle,  //const aHandle: THandle;
        SE_KERNEL_OBJECT,  //const aObjectType: TSeObjectType;
        [siOwnerSecurityInformation, siGroupSecurityInformation,
        siDaclSecurityInformation]//aSecurityInfo: TJwSecurityInformationFlagSet;
        );
        //ShowMessage(SD.GetTextMap(TJwSecurityTokenMapping));
    finally
      temp.Free;
    end;
  end;

  //  ShowMEssage(SD.Text);
  try
    //TOKEN_READ = TOKEN_QUERY + READ_CONTROL = read access to dacl
    Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or
      TOKEN_QUERY or TOKEN_DUPLICATE);
    Token.ConvertToImpersonatedToken(SecurityImpersonation,
      TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
    try
      Result := TJwSecureGeneralObject.ConvertMaximumAllowed(SD,
        Token, TJwSecurityTokenMapping);
    finally
      FreeAndNil(Token);
    end;
  finally
    FreeAndNil(SD);
  end;

  //ShowMessage(JwFormatAccessRights(result, TokenMapping));
end;

constructor TJwSecurityToken.CreateTokenByProcess(
  const aProcessHandle: TJwProcessHandle;
  const aDesiredAccess: TJwAccessMask;
  const Duplicate : Boolean = false
  );
var
  hProcess: TJwProcessHandle;
  bResult:  boolean;
begin
  Self.Create;

  hProcess := aProcessHandle;
  if hProcess = 0 then
    hProcess := GetCurrentProcess;

  bResult := OpenProcessToken(hProcess, aDesiredAccess, fTokenHandle);

  if Duplicate and bResult then
  begin
    fAccessMask := TOKEN_ALL_ACCESS; //skip our internal access checks routines 
    ConvertToImpersonatedToken(DEFAULT_IMPERSONATION_LEVEL, aDesiredAccess);
    ConvertToPrimaryToken(aDesiredAccess);
  end;

  if not bResult then
  begin
    raise EJwsclOpenProcessTokenException.CreateFmtEx(
      RsWinCallFailed, 'CreateTokenByProcess', ClassName,
      RsUNToken, 0, True, ['OpenProcessToken']);
  end;

  fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);


  Shared := False;
end;

constructor TJwSecurityToken.CreateTokenByProcessId(const ProcessID: DWORD;
  const DesiredAccess: TJwAccessMask);
var P : IJwPrivilegeScope;
    hProc : DWORD;
begin
   P := JwGetPrivilegeScope([SE_DEBUG_NAME],pst_EnableIfAvail);

  hProc := OpenProcess(PROCESS_QUERY_INFORMATION,
                False, ProcessID);
  if hProc = 0 then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateTokenByProcessId',
        ClassName, RsUNToken, 0, True, ['OpenProcess']);

  try
    //only duplicate the token if the necessary access right is given
    CreateTokenByProcess(hProc, DesiredAccess, (DesiredAccess and TOKEN_DUPLICATE = TOKEN_DUPLICATE));

  //  ShowMessage(GetTokenUserName);
  finally
    CloseHandle(hProc);
  end;
end;

constructor TJwSecurityToken.CreateTokenByThread(
  const aThreadHandle: TJwThreadHandle; const aDesiredAccess: TJwAccessMask;
  const anOpenAsSelf: boolean);
var
  hThread: TJwThreadHandle;
  bResult: boolean;
begin
  Self.Create;


  hThread := aThreadHandle;
  if hThread = 0 then
    hThread := GetCurrentThread;

  bResult := OpenThreadToken(hThread, aDesiredAccess, anOpenAsSelf,
    fTokenHandle);

  if not bResult then
  begin
    Done;

    if GetLastError() = ERROR_NO_TOKEN then //no token available
    begin
      raise EJwsclNoThreadTokenAvailable.CreateFmtEx(
        RsTokenInvalidThreadToken, 'CreateTokenByThread',
        ClassName, RsUNToken, 0, True, []);
    end
    else
    begin
      raise EJwsclOpenThreadTokenException.CreateFmtEx(
        RsTokenUnableToOpenThreadToken, 'CreateTokenByThread',
        ClassName, RsUNToken, 0, True, []);
    end;
  end;

  fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);

  Shared := False;
end;

constructor TJwSecurityToken.CreateTokenEffective(
  const aDesiredAccess: TJwAccessMask);
var
  bResult:      boolean;
  hTokenHandle: TJwTokenHandle;
begin
  //[Hint] bResult := true; //if false the thread token is not available
  (*try
    CreateTokenByThread(0, aDesiredAccess, true);
  except
    on E : EJwsclNoThreadTokenAvailable do  //continue if thread token is not available...
      bResult := false; //thread token is not available
    else
      raise; //...otherwise reraise exception
  end;*)

  bResult := OpenThreadToken(GetCurrentThread, aDesiredAccess,
    True, hTokenHandle);
  if bResult then
  begin
    Self.Create;

    fTokenHandle := hTokenHandle;
    Shared := False;
  end;

  if not bResult then //get process token if thread token is not available
    //open process token
    CreateTokenByProcess(0, aDesiredAccess);

  fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);
end;

constructor TJwSecurityToken.Create(const aTokenHandle: TJwTokenHandle;
  const ShareTokenHandle: TJwSharedHandle;
  const aDesiredAccess: TJwAccessMask);
begin
  Self.Create;
  fTokenHandle := aTokenHandle;
  fAccessMask  := aDesiredAccess;

  fShared := ShareTokenHandle = shShared;


  fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);
end;



constructor TJwSecurityToken.CreateCompatibilityQueryUserToken(
  const DesiredAccess: TJwAccessMask;
  const ProcessName: TJwString = ExplorerProcessName);

  {origin:
   http://www.delphipraxis.net/post721630.html#721630}
  function GetProcessID(Exename: TJwString): DWORD;
  var
    hProcessHandle: THandle;
    pEntry: TProcessEntry32;
  begin
    Result := 0;
    hProcessHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
    if hProcessHandle <> INVALID_HANDLE_VALUE then
    begin
      pEntry.dwSize := SizeOf(ProcessEntry32);
      if Process32First(hProcessHandle, pEntry) then
      begin
        repeat
          if JwCompareString(pEntry.szExeFile, Exename, True) = 0 then
          begin
            Result := pEntry.th32ProcessID;
            break;
          end;
        until Process32Next(hProcessHandle, pEntry) = False;
      end;
      CloseHandle(hProcessHandle);
    end;
  end;

var
  ProcessHandle, ProcessID: cardinal;
  TokenOrig: TJwSecurityToken;

begin
  ProcessID := GetProcessID(TjwPChar(ProcessName));
  if ProcessID = 0 then
    raise EJwsclProcessNotFound.CreateFmtEx(RsProcessNotFound,
      'CreateCompatibilityQueryUserToken', ClassName, RsUNToken,
      0, False, [ProcessName]);


  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, True, ProcessID);
  if ProcessHandle = 0 then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'CreateCompatibilityQueryUserToken',
      ClassName, RsUNToken, 0, True, ['OpenProcess']);

  try
    TokenOrig := TJwSecurityToken.CreateTokenByProcess(ProcessHandle,
      TOKEN_ALL_ACCESS);

    try
      //Copy token handle to a primary one
      Self.CreateDuplicateExistingToken(TokenOrig.TokenHandle, DesiredAccess);
    finally
      FreeAndNil(TokenOrig);
    end;
  finally
    CloseHandle(ProcessHandle);
  end;
end;


type
  TWtsGetActiveConsoleSessionID = function : DWORD; stdcall;
var
  _WtsGetActiveConsoleSessionID : TWtsGetActiveConsoleSessionID = nil;

function InternalWtsGetActiveConsoleSessionID : DWORD;
begin
  if @_WtsGetActiveConsoleSessionID = nil then
    _WtsGetActiveConsoleSessionID := GetProcAddress(
       GetModuleHandle('kernel32.dll'),'WTSGetActiveConsoleSessionId');

  if @_WtsGetActiveConsoleSessionID <> nil then
    result := _WtsGetActiveConsoleSessionID
  else
    result := 0;
end;

constructor TJwSecurityToken.CreateWTSQueryUserTokenEx(
  const Server: TObject; SessionID: cardinal);
var TS : TJwTerminalServer;
    TSRunning : Boolean;
    hServer : THANDLE;
begin
  RaiseOnInvalidPrimaryToken('CreateWTSQueryUserTokenEx');
//                    TJwServerInfo.IsTerminalServer

  hServer := WTS_CURRENT_SERVER_HANDLE;
  if Assigned(Server) then
  begin
    TS := Server as TJwTerminalServer;
    hServer := TS.ServerHandle;

    TSRunning := true;
  end
  else
    TSRunning := TJwWindowsVersion.IsWindows7 or //On Win7 WTS calls are always valid
      TJwWindowsVersion.IsTerminalServiceRunning;

  if (not TSRunning) then
    raise EJwsclTerminalServiceNecessary.CreateFmtEx(
      RsTokenUnsupportedWtsCall, 'Create', ClassName, RsUNToken, 0, False, []);


  if not JwIsPrivilegeSet(SE_TCB_NAME, pqt_Available) then
    raise EJwsclPrivilegeCheckException.CreateFmtEx(
      RsTokenPrivilegeNotHeld, 'Create', ClassName, RsUNToken,
      0, False, [SE_TCB_NAME]);


  Self.Create;
  fShared      := False;
  fTokenHandle := 0;

  SetLastError(0);

  if SessionID = INVALID_HANDLE_VALUE then
  begin
    if not WinStationQueryUserToken(hServer, InternalWtsGetActiveConsoleSessionID,
      fTokenHandle) then
      if not WinStationQueryUserToken(hServer, WTS_CURRENT_SESSION,
        fTokenHandle) then
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsTokenCallWtsQueryUserTokenFailed, 'WinStationQueryUserToken',
          ClassName, RsUNToken, 0, True, [SessionID]);
  end
  else
  begin
    if not WinStationQueryUserToken(hServer, SessionID, fTokenHandle) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsTokenCallWtsQueryUserTokenFailed, 'WinStationQueryUserToken',
        ClassName, RsUNToken, 0, True, [SessionID]);
  end;

  
  try
    fAccessMask := GetMaximumAllowed;
  except
    //if GetMaximumAllowed does not have the READ_CONTROL flag
    //we may fail here, so just set access mask to zero
    //many people don't even read it.
    fAccessMask := 0;
  end;  
end;

constructor TJwSecurityToken.CreateWTSQueryUserToken(SessionID:
  TJwSessionId = INVALID_HANDLE_VALUE);
begin
  RaiseOnInvalidPrimaryToken('CreateWTSQueryUserToken');

  if not (TJwWindowsVersion.IsWindowsXP(True) or
    TJwWindowsVersion.IsWindows2003(True)) then
    raise EJwsclUnsupportedWindowsVersionException.CreateFmtEx(
      RsTokenUnsupportedWtsCall, 'Create', ClassName, RsUNToken, 0, False, []);

  if not JwIsPrivilegeSet(SE_TCB_NAME, pqt_Available) then
    raise EJwsclPrivilegeCheckException.CreateFmtEx(
      RsTokenPrivilegeNotHeld, 'Create', ClassName, RsUNToken,
      0, False, [SE_TCB_NAME]);

  Self.Create;
  fShared      := False;
  fTokenHandle := 0;

  SetLastError(0);

  if SessionID = INVALID_HANDLE_VALUE then
  begin
    if not WTSQueryUserToken(InternalWtsGetActiveConsoleSessionID, fTokenHandle) then
      if not WTSQueryUserToken(WTS_CURRENT_SESSION, fTokenHandle) then
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsTokenCallWtsQueryUserTokenFailed, 'WTSQueryUserToken',
          ClassName, RsUNToken, 0, True, [SessionID]);
  end
  else
  begin
    if not WTSQueryUserToken(SessionID, fTokenHandle) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsTokenCallWtsQueryUserTokenFailed, 'WTSQueryUserToken',
        ClassName, RsUNToken, 0, True, [SessionID]);
  end;

  //should be TOKEN_ALL_ACCESS
  try
    fAccessMask := GetMaximumAllowed
  except
    //if GetMaximumAllowed does not have the READ_CONTROL flag
    //we may fail here, so just set access mask to zero
    //many people don't even read it.
    fAccessMask := 0;
  end;
  //TODO: Warning: in XP may not TOKEN_ALL_ACCESS
  //fAccessMask := TOKEN_ALL_ACCESS;
end;

constructor TJwSecurityToken.CreateDuplicateExistingToken(
  const aTokenHandle: TJwTokenHandle; const aDesiredAccess: TJwAccessMask;
  UseDuplicateExistingToken: boolean = False);
begin
  Self.Create;
  //Shared == true

  fTokenHandle := aTokenHandle;
  fAccessMask  := TOKEN_ALL_ACCESS;

  try
    //first convert the token to a thread token
    //this method will do nothing if the token is already impersonated

    ConvertToImpersonatedToken(DEFAULT_IMPERSONATION_LEVEL, aDesiredAccess);
    //If the token handle differs we have a new token handle that
    //is not shared.
    if fTokenHandle <> aTokenHandle then
      Shared := False;

    //if we are here the token is an impersonated one in every case  
    ConvertToPrimaryToken(aDesiredAccess);

    Shared := False;
  except
    Done; //free objects created by Self.Create;
    raise;
  end;
end;

constructor TJwSecurityToken.CreateDuplicateExistingToken(
  const Token: TJwSecurityToken;
  const DesiredAccess: TJwAccessMask; UseDuplicateExistingToken: boolean);
begin
  JwRaiseOnNilParameter(Token, 'Token', 'CreateDuplicateExistingToken', ClassName, RsUNToken);

  CreateDuplicateExistingToken(Token.TokenHandle, DesiredAccess, UseDuplicateExistingToken);
end;



constructor TJwSecurityToken.CreateRestrictedToken(
  PrevTokenHandle : TJwTokenHandle;
  const TokenAccessMask: TJwTokenAccessMask;
  const Flags: cardinal;
  const SidsToDisable: TJwSecurityIdList;
  const PrivilegesToDelete: TJwPrivilegeSet;
  const RestrictedSids: TJwSecurityIdList);
var
  bRes: boolean;

  pLuids: PLUID_AND_ATTRIBUTES;
  pDisSids, pResSids: PSID_AND_ATTRIBUTES;

  cLuids, cDisSids, cResSids: cardinal;

  aToken: TJwSecurityToken;
begin
  Self.Create;
  fShared := False;

  aToken := nil;

  if PrevTokenHandle = 0 then
  begin
    aToken := TJwSecurityToken.CreateTokenEffective(TokenAccessMask);
    PrevTokenHandle := aToken.TokenHandle;
  end;

  pDisSids := nil;
  pLuids   := nil;
  pResSids := nil;



  cDisSids := 0;
  cLuids   := 0;
  cResSids := 0;

  if Assigned(SidsToDisable) then
  begin
    pDisSids := PSID_AND_ATTRIBUTES(SidsToDisable.Create_PSID_Array);
    cDisSids := SidsToDisable.Count;
  end;

  if Assigned(PrivilegesToDelete) then
  begin
    pLuids := PrivilegesToDelete.Create_PLUID_AND_ATTRIBUTES;
    cLuids := PrivilegesToDelete.Count;
  end;

  if Assigned(RestrictedSids) then
  begin
    pResSids := PSID_AND_ATTRIBUTES(RestrictedSids.Create_PSID_Array);
    cResSids := RestrictedSids.Count;
  end;

  //[Hint] bRes := false;
  try
    bRes := jwaWindows.CreateRestrictedToken(PrevTokenHandle, Flags,
      cDisSids, pDisSids, cLuids, pLuids, cResSids, pResSids, fTokenHandle);
  finally
    if Assigned(pDisSids) then
      SidsToDisable.Free_PSID_Array(PSidAndAttributesArray(pDisSids));

    if Assigned(pLuids) then
      PrivilegesToDelete.Free_PLUID_AND_ATTRIBUTES(pLuids);

    if Assigned(pResSids) then
      RestrictedSids.Free_PSID_Array(PSidAndAttributesArray(pResSids));
  end;

  try
    if TokenAccessMask = MAXIMUM_ALLOWED then
      fAccessMask := GetMaximumAllowed
    else
      fAccessMask := TokenAccessMask;
  except
    //if GetMaximumAllowed does not have the READ_CONTROL flag
    //we may fail here, so just set access mask to zero
    //many people don't even read it.
    fAccessMask := 0;
  end;    

  if Assigned(aToken) then
    aToken.Free;

  if not bRes then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenFailedImpersonateAnonymousToken, 'CreateRestrictedToken',
      ClassName, RsUNToken, 0, True, []);
end;


procedure TJwSecurityToken.ConvertToImpersonatedToken(
  impLevel: SECURITY_IMPERSONATION_LEVEL; const aDesiredAccess: TJwAccessMask);
var
  hNewTokenHandle: TJwTokenHandle;
begin
  {
  TOKEN_READ = TOKEN_QUERY + READ_CONTROL
   We need access to the DACL for GetMaximumAllowed
  }
  //check for valid token handle
  CheckTokenHandle('ConvertToImpersonatedToken');
  CheckTokenAccessType(TOKEN_DUPLICATE + TOKEN_READ,
    'TOKEN_DUPLICATE,TOKEN_READ',
    'ConvertToImpersonatedToken');



  //we do not need to impersonate token if it is already impersonated
  if GetTokenType = TokenImpersonation then
    exit;

  //we are not allowed to close
{  if fShared then
    raise EJwsclSharedTokenException.CreateFmtEx('Cannot convert a SHARED  token',
                        'ConvertToImpersonatedToken','TJwSecurityToken',RsUNToken,0,true,[]);
 }

  //create a copy of the token
  if DuplicateTokenEx(fTokenHandle, aDesiredAccess, nil, impLevel,
    TokenImpersonation, hNewTokenHandle) then
  begin
    //we need to close the handle
    if not fShared then
      CloseHandle(fTokenHandle);
    fTokenHandle := hNewTokenHandle;
    
      
  	fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);  
  end
  else
    raise EJwsclTokenImpersonationException.CreateFmtEx(
      RsTokenCallDuplicateTokenFailed1, 'GetTokenInformation',
      ClassName, RsUNToken, 0, True, []);
end;

procedure TJwSecurityToken.ConvertToPrimaryToken(
  const aDesiredAccess: TJwAccessMask);
var
  hNewTokenHandle: TJwTokenHandle;
begin
  {
  TOKEN_READ = TOKEN_QUERY + READ_CONTROL
   We need access to the DACL for GetMaximumAllowed
  }

  //check for valid token handle
  CheckTokenHandle('ConvertToPrimaryToken');
  CheckTokenAccessType(TOKEN_DUPLICATE + TOKEN_READ,
    'TOKEN_DUPLICATE,TOKEN_READ',
    'ConvertToPrimaryToken');

  //we do not need to impersonate token if it is already impersonated
  if GetTokenType = TokenPrimary then
    exit;

  //we are not allowed to close
{  if fShared then
    raise EJwsclSharedTokenException.CreateFmtEx('Cannot convert a SHARED  token',
                        'ConvertToImpersonatedToken','TJwSecurityToken',RsUNToken,0,true,[]);
 }

  //create a copy of the token
  if DuplicateTokenEx(fTokenHandle, aDesiredAccess, nil,
    DEFAULT_IMPERSONATION_LEVEL, TokenPrimary, hNewTokenHandle) then
  begin
    //we need to close the handle
    if not fShared then
      CloseHandle(fTokenHandle);
    fTokenHandle := hNewTokenHandle;

    fAccessMask := RetrieveSpecificAccessRights(aDesiredAccess);        
  end
  else
    raise EJwsclTokenPrimaryException.CreateFmtEx(
      RsTokenCallDuplicateTokenFailed1, 'GetTokenInformation',
      ClassName, RsUNToken, 0, True, []);
end;


function TJwSecurityToken.GetTokenInformationLength(
  hTokenHandle: TJwTokenHandle;
  aTokenInformationClass: TTokenInformationClass): cardinal;

var
  ptrTokenType: Pointer;
  iError: integer;
  iTC:    cardinal;
  TokenClass: JwaWindows._TOKEN_INFORMATION_CLASS;
begin
  CheckTokenHandle('GetTokenInformationLength');

  Result := 0;
  ptrTokenType := nil;

  iTC := cardinal(aTokenInformationClass);
  TokenClass := JwaWindows._TOKEN_INFORMATION_CLASS(iTC);

  //GetTokenInformation should always return ERROR_INSUFFICIENT_BUFFER
  if not jwaWindows.GetTokenInformation(hTokenHandle, TokenClass,
    ptrTokenType, 0, Result) then
  begin
    iError := GetLastError;
    if (iError <> HRESULT(ERROR_INSUFFICIENT_BUFFER)) and (iError <> 24) then
      Result := 0;
    if iError = 24 then
    begin
      {on XP, this size of the following classtypes
       returns 0.
       We fix that here
      }
      case aTokenInformationClass of
        JwaWindows.TokenSessionId : result := SizeOf(DWORD);
      end;
    end;
  end;
end;

procedure TJwSecurityToken.GetTokenInformation(hTokenHandle: TJwTokenHandle;
  TokenInformationClass: TTokenInformationClass;
  out TokenInformation: Pointer);

  procedure doRaiseError(EClass: EJwsclExceptionClass; msg: TJwString);
  begin
    if EClass = nil then
      EClass := EJwsclTokenInformationException;
    raise EClass.CreateFmtEx(msg, 'GetTokenInformation', ClassName,
      RsUNToken, 0, True, []);
  end;

var
  tokLen: cardinal;
  Result: boolean;
  //[Hint] i,i2,i3 : Integer;
  //[Hint] w : Cardinal;
  //[Hint] k : jwawindows.TTokenInformationClass;
begin
  CheckTokenHandle('GetTokenInformation');

  if TokenInformationClass = TokenSource then
    CheckTokenAccessType(TOKEN_QUERY_SOURCE, 'TOKEN_QUERY_SOURCE',
      'GetTokenInformation')
  else
    CheckTokenAccessType(TOKEN_QUERY, 'TOKEN_QUERY', 'GetTokenInformation');

  tokLen := Self.GetTokenInformationLength(hTokenHandle,
    TokenInformationClass);

  if tokLen <= 0 then
    doRaiseError(nil, RsTokenUnableTokenInformationLength);

  TokenInformation := HeapAlloc(JwProcessHeap, HEAP_ZERO_MEMORY, tokLen);

  if (TokenInformation = nil) then
    doRaiseError(EJwsclNotEnoughMemory,
      RsTokenNotEnoughMemoryTokenSave);

  //result := jwaWindows.GetTokenInformation(hTokenHandle,TokenInformationClass,
  //                       TokenInformation, tokLen,tokLen);
  {i := Integer(TokenInformationClass);
  if i = 0 then;
  i2 := sizeof(JwsclTypes.TTokenInformationClass);  // = 1
  i3 := sizeof(jwawindows.TTokenInformationClass); // = 4
  if i2 = i3 then;
  i3 := sizeof(w);}

  //alignment of 4 bytes is important - JwsclTypes.TTokenInformationClass does not
  Result := jwaWindows.GetTokenInformation(hTokenHandle,
    jwawindows.TTokenInformationClass(TokenInformationClass),
    TokenInformation, tokLen, tokLen);

  if not Result then
  begin
    HeapFree(JwProcessHeap, 0, TokenInformation);
    doRaiseError(nil, RsTokenUnableGetTokenInformation);
  end;
end;

function TJwSecurityToken.GetImpersonationLevel: TSecurityImpersonationLevel;
var
  imp: PSecurityImpersonationLevel;
begin
  CheckTokenHandle('GetImpersonationLevel');

  //a process token does not have impersonation level
  if GetTokenType = TokenPrimary then
  begin
    Result := DEFAULT_IMPERSONATION_LEVEL;
    exit;
  end;


  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}*)
    TokenImpersonationLevel, Pointer(imp));


  Result := imp^;

  HeapFree(JwProcessHeap, 0, imp);
end;

function TJwSecurityToken.GetTokenUser: TJwSecurityId;
var
  pUser: PTOKEN_USER;
begin
  CheckTokenHandle('GetTokenUser');

  pUser := nil;
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}*)
    JwaWindows.TokenUser, Pointer(pUser));


  Result := TJwSecurityId.Create(PSidAndAttributes(@pUser^.User));

  HeapFree(JwProcessHeap, 0, pUser);
end;

function TJwSecurityToken.GetTokenSource: TTokenSource;
var
  pSource: PTOKEN_SOURCE;
begin
  CheckTokenHandle('GetTokenSource');

  GetTokenInformation(fTokenHandle, TokenSource, Pointer(pSource));
  Result := pSource^;
  HeapFree(JwProcessHeap, 0, pSource);
end;



procedure TJwSecurityToken.GetTokenSource(out SourceName: ShortString;
  out SourceLUID: TLuid);
var
  pSource: PTOKEN_SOURCE;
begin
  CheckTokenHandle('GetTokenSource');

  GetTokenInformation(fTokenHandle, TokenSource, Pointer(pSource));

  SourceName := ShortString(pSource^.SourceName);
  SourceLUID := pSource^.SourceIdentifier;

  HeapFree(JwProcessHeap, 0, pSource);
end;

function TJwSecurityToken.GetTokenGroupsAttributesInt(Index: integer):
TJwSidAttributeSet;
var
  Groups: TJwSecurityIdList;
begin
  Groups := TokenGroups;
  try
    Result := Groups.Items[Index].AttributesType;
  finally
    FreeAndNil(Groups);
  end;
end;

procedure TJwSecurityToken.SetTokenGroupsAttributesInt(Index: integer;
  Attributes: TJwSidAttributeSet);
var
  Groups: TJwSecurityIdList;
begin
  Groups := TokenGroups;
  try
    Groups.Items[Index].AttributesType := Attributes;
    TokenGroups := Groups;
  finally
    FreeAndNil(Groups);
  end;
end;

function TJwSecurityToken.GetTokenGroupsAttributesSid(Sid: TJwSecurityId):
TJwSidAttributeSet;
var
  Groups: TJwSecurityIdList;
begin
  Groups := TokenGroups;
  try
    Result := Groups.Items[Groups.FindSid(Sid)].AttributesType;
  finally
    FreeAndNil(Groups);
  end;
end;


procedure TJwSecurityToken.SetTokenGroupsAttributesSid(Sid: TJwSecurityId;
  Attributes: TJwSidAttributeSet);
var
  Groups: TJwSecurityIdList;
begin
  Groups := TokenGroups;
  try
    Groups.Items[Groups.FindSid(Sid)].AttributesType := Attributes;
    TokenGroups := Groups;
  finally
    FreeAndNil(Groups);
  end;
end;


procedure TJwSecurityToken.SetTokenGroups(List: TJwSecurityIdList);
var
  groups: PTOKEN_GROUPS;
  temp:   cardinal;
begin
  CheckTokenHandle('GetTokenGroupsEx');

  if not Assigned(List) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetTokenGroups', ClassName, RsUNToken,
      0, False, ['List']);

  groups := List.Create_PTOKEN_GROUPS;
  temp   := 0;
  try
    if not AdjustTokenGroups(fTokenHandle,//HANDLE TokenHandle,
      False, //BOOL ResetToDefault,
      groups,//PTOKEN_GROUPS NewState,
      0,     //DWORD BufferLength,
      nil,   //PTOKEN_GROUPS PreviousState,
      @temp  //PDWORD ReturnLength
      ) then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed, 'SetTokenGroups',
        //sSourceProc
        ClassName,                                //sSourceClass
        RsUNToken,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                 //bShowLastError
        'AdjustTokenGroups',                  //sWinCall
        ['AdjustTokenGroups']);
    //const Args: array of const


  finally
    List.Free_PTOKEN_GROUPS(groups);
  end;

end;

function TJwSecurityToken.GetTokenGroups: TJwSecurityIdList;
var
  pGroups: PTOKEN_GROUPS;
begin
  CheckTokenHandle('GetTokenGroups');

  GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}*)
    JwaWindows.TokenGroups, Pointer(pGroups));

  Result := TJwSecurityIdList.Create(True, pGroups);

  HeapFree(JwProcessHeap, 0, pGroups);
end;

function TJwSecurityToken.GetTokenGroupsEx: PTokenGroups;
  //var pGroups : PTOKEN_GROUPS;
begin
  CheckTokenHandle('GetTokenGroupsEx');

  GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}*)
    JwaWindows.TokenGroups, Pointer(Result));
end;




function TJwSecurityToken.GetTokenRestrictedSids: TJwSecurityIdList;
var
  pGroups: PTOKEN_GROUPS;
begin
  CheckTokenHandle('GetTokenRestrictedSids');
  GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenRestrictedSids, Pointer(pGroups));
  Result := TJwSecurityIdList.Create(True, pGroups);
  HeapFree(JwProcessHeap, 0, pGroups);
end;



function TJwSecurityToken.GetTokenDefaultDacl: TJwDAccessControlList;
var
  pDACL: PTOKEN_DEFAULT_DACL;
begin
  CheckTokenHandle('GetTokenDefaultDacl');

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenDefaultDacl, Pointer(pDACL));
  {If the value of the TokenInformationClass parameter is TokenDefaultDacl and the
  token has no default DACL, the function sets the variable pointed to by ReturnLength
  to sizeof(TOKEN_DEFAULT_DACL) and sets the DefaultDacl member of the
  TOKEN_DEFAULT_DACL structure to NULL.
  http://msdn2.microsoft.com/en-us/library/aa446671.aspx
  }
  Result := nil;
  if pDACL <> nil then
    try
      Result := TJwDAccessControlList.Create(pDACL^.DefaultDacl);
    finally
      HeapFree(JwProcessHeap, 0, pDACL);
    end;
end;

procedure TJwSecurityToken.SetTokenDefaultDacl(
  const aDefaultDCAL: TJwDAccessControlList);
//TOKEN_ADJUST_DEFAULT
var
  pDACL: TOKEN_DEFAULT_DACL;
begin
  CheckTokenHandle('SetfTokenDefaultDacl');
  CheckTokenAccessType(TOKEN_ADJUST_DEFAULT, 'TOKEN_ADJUST_DEFAULT',
    'SetfTokenDefaultDacl');


  pDACL.DefaultDacl := aDefaultDCAL.Create_PACL;

  if (not SetTokenInformation(fTokenHandle, jwaWindows.TokenDefaultDacl,
    Pointer(@pDACL), sizeof(TOKEN_DEFAULT_DACL))) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'SetTokenDefaultDacl', ClassName, RsUNToken,
      0, True, ['SetTokenInformation']);

  aDefaultDCAL.Free_PACL(pDACL.DefaultDacl);
end;

function TJwSecurityToken.GetTokenOrigin: TLuid;
var
  pOrigin: PTOKEN_ORIGIN;
begin
  CheckTokenHandle('GetTokenOrigin');

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenOrigin, Pointer(pOrigin));
  try
    Result := pOrigin^.OriginatingLogonSession;
  finally
    HeapFree(GetProcessHeap, 0, pOrigin);
  end;
end;

procedure TJwSecurityToken.SetTokenOrigin(const anOrigin: TLuid); //SE_TCB_NAME
var
  pOrigin: TOKEN_ORIGIN;
begin
  CheckTokenHandle('SetTokenOrigin');
  //TODO: CheckTokenPrivileges([SE_TCB_NAME]); do not do this - process must have TCB!!

  try
    //PushPrivileges;
    //PrivilegeEnabled[SE_TCB_NAME] := True;

    pOrigin.OriginatingLogonSession := anOrigin;

    if (not SetTokenInformation(fTokenHandle, jwaWindows.TokenOrigin,
      Pointer(@pOrigin), sizeof(pOrigin))) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'SetTokenOrigin', ClassName, RsUNToken,
        0, True, ['SetTokenInformation']);
  finally
    //PopPrivileges;
  end;

end;

function TJwSecurityToken.GetTokenOwner: TJwSecurityId;
var
  pOwner: PTOKEN_OWNER;
begin
  CheckTokenHandle('GetTokenOwner');

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenOwner, Pointer(pOwner));
  try
    Result := TJwSecurityId.Create(PSidAndAttributes(@pOwner^.Owner));
  finally
    HeapFree(GetProcessHeap, 0, pOwner);
  end;
end;

procedure TJwSecurityToken.SetTokenOwner(const anOwner: TJwSecurityId);
//TOKEN_ADJUST_DEFAULT
var
  pOwner: TOKEN_OWNER;
begin
  CheckTokenHandle('SetTokenOrigin');
  //TODO: CheckTokenPrivileges([SE_TCB_NAME]); do not do this - process must have TCB!!

  pOwner.Owner := anOwner.SID;
  try
    //PushPrivileges;
    //PrivilegeEnabled[SE_TCB_NAME] := True;

    if (not SetTokenInformation(fTokenHandle, jwaWindows.TokenOwner,
      Pointer(@pOwner), sizeof(pOwner))) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'SetTokenInformation', ClassName,
        RsUNToken, 0, True, ['SetTokenInformation']);
  finally
    //PopPrivileges;
  end;
end;

function TJwSecurityToken.GetPrimaryGroup: TJwSecurityId;
var
  pPrimaryGroup: PTOKEN_PRIMARY_GROUP;
begin
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenPrimaryGroup, Pointer(pPrimaryGroup));
  try
    Result := TJwSecurityId.Create(PSidAndAttributes(
      @pPrimaryGroup^.PrimaryGroup));
  finally
    HeapFree(GetProcessHeap, 0, pPrimaryGroup);
  end;
end;

procedure TJwSecurityToken.SetPrimaryGroup(const PrimGroup: TJwSecurityId);
//TOKEN_ADJUST_DEFAULT
var
  pPrimaryGroup: TOKEN_PRIMARY_GROUP;
begin
  CheckTokenHandle('SetTokenOrigin');

  pPrimaryGroup.PrimaryGroup := PrimGroup.SID;

  if (not SetTokenInformation(fTokenHandle, jwaWindows.TokenPrimaryGroup,
    Pointer(@pPrimaryGroup), sizeof(pPrimaryGroup))) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'SetPrimaryGroup', ClassName, RsUNToken,
      0, True, ['SetTokenInformation']);
end;


function TJwSecurityToken.GetTokenSessionId: cardinal;
var
  ID: PCardinal;
begin
  CheckTokenHandle('GetTokenSessionId');

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenSessionId, Pointer(ID));
  try
    Result := ID^;
  finally
    HeapFree(GetProcessHeap, 0, ID);
  end;
end;

procedure TJwSecurityToken.SetTokenSessionId(const SessionID: cardinal);
//SE_TCB_NAME
begin
  if TJwWindowsVersion.IsWindows2000(False) then
    exit;

  CheckTokenHandle('SetTokenSessionId');
  //TODO: CheckTokenPrivileges([SE_TCB_NAME]); do not do this - process must have TCB!!

  try
    //PushPrivileges;

    //PrivilegeEnabled[SE_TCB_NAME] := True;

    if (not SetTokenInformation(fTokenHandle, jwaWindows.TokenSessionId,
      Pointer(@SessionID), sizeof(SessionID))) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'SetTokenSessionId', ClassName,
        RsUNToken, 0, True, ['SetTokenInformation']);
  finally
    //PopPrivileges;
  end;
end;

function TJwSecurityToken.GetPrivilegeAvailable(Name: TJwString): boolean;
var
  privSet: TJwPrivilegeSet;
begin
  CheckTokenHandle('GetPrivilegeAvailable');

  privSet := GetTokenPrivileges;

  try
    Result := Assigned(privSet.PrivByName[Name]);
  finally
    privSet.Free;
  end;
end;

function TJwSecurityToken.GetPrivilegeEnabled(Name: TJwString): boolean;
var
  privSet: TJwPrivilegeSet;
begin
  CheckTokenHandle('GetPrivilegeEnabled');
  privSet := GetTokenPrivileges;

  if Assigned(privSet.PrivByName[Name]) then
    Result := privSet.PrivByName[Name].Enabled
  else
    Result := False;

  privSet.Free;
end;

procedure TJwSecurityToken.SetPrivilegeEnabled(Name: TJwString; En: boolean);
var
  privSet: TJwPrivilegeSet;
  S: TJwString;
begin
  CheckTokenHandle('SetPrivilegeEnabled');
  privSet := GetTokenPrivileges;

  S := TJwString(Name);
  if Assigned(privSet.PrivByName[s]) then
    privSet.PrivByName[s].Enabled := En
  else
  begin
    privSet.Free;
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
      RsTokenPrivilegeNotFound, 'IsPrivilegeEnabled', ClassName,
      RsUNToken, 0, False, [Name]);
  end;

  privSet.Free;
end;

function TJwSecurityToken.GetRunElevation: cardinal;
{$IFDEF VISTA}
var
  privs: PTokenElevation;
{$ENDIF VISTA}
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetRunElevation', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetRunElevation');

  JwCheckVISTACompilerSwitch('GetRunElevation', ClassName, RsUNToken);

{$IFDEF VISTA}
{
If JwaWindows.TokenElevation could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(
    JwaWindows.TokenElevation), Pointer(privs));

  Result := privs^.TokenIsElevated;

  HeapFree(GetProcessHeap, 0, privs);
{$ELSE}
  result := 0;
{$ENDIF VISTA}  
end;


function TJwSecurityToken.GetLinkedToken: TJwSecurityToken;
{$IFDEF VISTA}
var
  Data: PTokenLinkedToken;
{$ENDIF VISTA}  
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetLinkedToken', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetLinkedToken');

  JwCheckVISTACompilerSwitch('GetLinkedToken', ClassName, RsUNToken);

{$IFDEF VISTA}
{
If JwaWindows.XXX could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}


  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(JwaWindows.TokenLinkedToken), Pointer(Data));

  try
    {TODO: Warning:
     I do not really know whether the LinkedToken is safe to be shared or
      must be destroyed.}
    Result := TJwSecurityToken.Create(Data^.LinkedToken, shShared,
      MAXIMUM_ALLOWED);
  finally
    HeapFree(GetProcessHeap, 0, Data);
  end;
{$ELSE}
  result := nil;  
{$ENDIF VISTA}  
end;

function TJwSecurityToken.GetIntegrityLevelType: TJwIntegrityLabelType;
var
  List:   TJwSecurityIdList;
  Labels: TJwIntegrityLabelType;
begin
  for Labels := low(TJwIntegrityLabelType) to high(TJwIntegrityLabelType) do
    if (Labels <> iltNone) and not Assigned(JwIntegrityLabelSID[Labels]) then
      raise EJwsclInitWellKnownException.CreateFmtEx(
        RsInitWellKnownNotCalled, 'GetIntegrityLevelType',
        RsTokenGlobalClassName, RsUNToken, 0, False, []);

  Result := iltNone;

  List := GetIntegrityLevel;
  try
    if Assigned(List) and (List.Count > 0) then
    begin
      for Labels := low(TJwIntegrityLabelType)
        to high(TJwIntegrityLabelType) do
      begin
        if Assigned(JwIntegrityLabelSID[Labels]) and
          JwIntegrityLabelSID[Labels].EqualSid(List[0]) then
        begin
          Result := Labels;
          break;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TJwSecurityToken.SetIntegrityLevelType(
  const LevelType: TJwIntegrityLabelType);
begin
  SetIntegrityLevel(LevelType);
end;

procedure TJwSecurityToken.SetIntegrityLevel(
  const LabelType: TJwIntegrityLabelType;
  const Attributes: TJwSidAttributeSet = [sidaGroupMandatory]);
begin
  if (LabelType <> iltNone) and not
    Assigned(JwIntegrityLabelSID[LabelType]) then
    raise EJwsclInitWellKnownException.CreateFmtEx(
      RsInitWellKnownNotCalled, 'SetIntegrityLevel',
      RsTokenGlobalClassName, RsUNToken, 0, False, []);

  SetIntegrityLevel(JwIntegrityLabelSID[LabelType], Attributes);
end;


procedure TJwSecurityToken.SetIntegrityLevel(const MandatorySid: TJwSecurityId;
  const Attributes: TJwSidAttributeSet = [sidaGroupMandatory]);
{$IFDEF VISTA}
var
  mL: TTokenMandatoryLabel;
{$ENDIF VISTA}  
begin
  if not Assigned(MandatorySid) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetIntegrityLevel', RsTokenGlobalClassName,
      RsUNToken, 0, False, ['MandatorySid']);

  JwCheckVISTACompilerSwitch('SetIntegrityLevel', ClassName, RsUNToken);

{$IFDEF VISTA}
  mL.Label_.Sid := MandatorySid.CreateCopyOfSID;
  mL.Label_.Attributes := TJwEnumMap.ConvertAttributes(Attributes);


{
If JwaWindows.XXX could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}

  try
    if (not SetTokenInformation(fTokenHandle,
      JwaWindows.TTokenInformationClass(JwaWindows.TokenIntegrityLevel),
      Pointer(@mL), sizeof(mL))) then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'SetIntegrityLevel', ClassName,
        RsUNToken, 0, True, ['SetTokenInformation']);
  finally
    MandatorySid.FreeSID(mL.Label_.Sid);
  end;
{$ENDIF VISTA}
end;

function TJwSecurityToken.GetIntegrityLevel: TJwSecurityIdList;
{$IFDEF VISTA}
var
  mL: PTokenMandatoryLabel;
{$ENDIF VISTA}  
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetIntegrityLevel', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetIntegrityLevel');

  JwCheckVISTACompilerSwitch('GetIntegrityLevel', ClassName, RsUNToken);


{$IFDEF VISTA}
{
If JwaWindows.XXX could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}


  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(JwaWindows.TokenIntegrityLevel),
    Pointer(mL));

  Result := nil;
  if Assigned(mL) then
    try
      Result := TJwSecurityIdList.Create(@Ml^.Label_);
    finally
      HeapFree(GetProcessHeap, 0, mL);
    end;
{$ELSE}
  result := nil;
{$ENDIF VISTA}
end;

function TJwSecurityToken.GetElevationType: TTokenElevationType;
{$IFDEF VISTA}
var
  privs: PTokenElevationType;
{$ENDIF VISTA}
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetElevationType', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetElevationType');

  JwCheckVISTACompilerSwitch('GetElevationType', ClassName, RsUNToken);

{$IFDEF VISTA}

{
If JwaWindows.XXX could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}

  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(

    JwaWindows.TokenElevationType), Pointer(privs));

  Result := privs^;

  HeapFree(GetProcessHeap, 0, privs);
{$ELSE}
  result := TokenElevationTypeDefault;
{$ENDIF VISTA}
end;

function TJwSecurityToken.GetVirtualizationAllowed: boolean;
{$IFDEF VISTA}
var
  privs: PCardinal;
{$ENDIF VISTA}
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetVirtualizationAllowed', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetVirtualizationAllowed');

  JwCheckVISTACompilerSwitch('GetVirtualizationAllowed', ClassName, RsUNToken);


{$IFDEF VISTA}
{
If JwaWindows.XXX could not be found, you probably
did not set the WINVISTA compiler directive for JwaWindows.pas
in \includes\jediapilib.inc.
}


  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(
    JwaWindows.TokenVirtualizationAllowed), Pointer(privs));

  Result := privs^ <> 0;

  HeapFree(GetProcessHeap, 0, privs);
{$ELSE}
  result := false;
{$ENDIF VISTA}
end;

function TJwSecurityToken.GetVirtualizationEnabled: boolean;
{$IFDEF VISTA}
var
  privs: PCardinal;
{$ENDIF VISTA}
begin
  TJwWindowsVersion.CheckWindowsVersion(
    cOsVista, True, 'GetVirtualizationEnabled', ClassName, RsUNToken, 0);
  CheckTokenHandle('GetVirtualizationEnabled');

  JwCheckVISTACompilerSwitch('GetVirtualizationEnabled', ClassName, RsUNToken);


{$IFDEF VISTA}
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TTokenInformationClass(

    JwaWindows.TokenVirtualizationEnabled), Pointer(privs));

  Result := privs^ <> 0;
  HeapFree(GetProcessHeap, 0, privs);
{$ELSE}
  result := false;
{$ENDIF VISTA}
end;


function TJwSecurityToken.GetMandatoryPolicy: TJwTokenMandatoryPolicies;
{$IFDEF VISTA}
var
  p: PTokenMandatoryPolicy;
{$ENDIF VISTA}
begin
  JwCheckVISTACompilerSwitch('GetMandatoryPolicy', ClassName, RsUNToken);

{$IFDEF VISTA}
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
    JwaWindows.TokenMandatoryPolicy, Pointer(p));

  Result := TJwEnumMap.ConvertTokenMandatoryPolicyFlags(p^.Policy);

  HeapFree(GetProcessHeap, 0, p);
{$ELSE}
  result := [];
{$ENDIF VISTA}
end;


function TJwSecurityToken.GetTokenPrivileges: TJwPrivilegeSet;
var
  privs: PTOKEN_PRIVILEGES;
begin
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenPrivileges, Pointer(privs));

  Result := TJwPrivilegeSet.Create(Self, privs);
  fPrivelegesList.Add(Result);

  HeapFree(GetProcessHeap, 0, privs);
end;

function TJwSecurityToken.GetTokenPrivilegesEx: PTOKEN_PRIVILEGES;
  //var privs : PTOKEN_PRIVILEGES;
begin
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenPrivileges, Pointer(Result));
end;

function TJwSecurityToken.CreateRestrictedToken(
  const TokenAccessMask: TJwTokenHandle;
  const Flags: cardinal;
  const SidsToDisable: TJwSecurityIdList;
  const PrivilegesToDelete: TJwPrivilegeSet;
  const RestrictedSids: TJwSecurityIdList
  ): TJwSecurityToken;
begin
  Result := TJwSecurityToken.CreateRestrictedToken(Self.TokenHandle,
    TokenAccessMask, Flags, SidsToDisable, PrivilegesToDelete,
    RestrictedSids);
end;


function TJwSecurityToken.CreateDuplicateToken(AccessMask: TJwAccessMask;
  Security: PSECURITY_ATTRIBUTES): TJwSecurityToken;
var
  newTokenHandle: TJwTokenHandle;
begin
  CheckTokenHandle('CreateDuplicateToken');
  CheckTokenAccessType(TOKEN_DUPLICATE + TOKEN_READ,
    'TOKEN_DUPLICATE,TOKEN_READ',
    'CreateDuplicateToken');


  if not jwaWindows.DuplicateTokenEx(TokenHandle, AccessMask,
    LPSECURITY_ATTRIBUTES(Security), GetImpersonationLevel,
    GetTokenType, newTokenHandle) then
    EJwsclDuplicateTokenException.CreateFmtEx(
      RsWinCallFailed,
      'CreateDuplicateToken', ClassName, RsUNToken, 0, True,
      ['DuplicateTokenEx']);

  Result := TJwSecurityToken.Create;

  Result.fShared      := False;
  Result.fTokenHandle := newTokenHandle;

  try
    if AccessMask = MAXIMUM_ALLOWED then
      fAccessMask := GetMaximumAllowed
    else
      fAccessMask := AccessMask;
  except
	//if GetMaximumAllowed does not have the READ_CONTROL flag
	//we may fail here, so just set access mask to zero
	//many people don't even read it.
    fAccessMask := 0;
  end;          
end;



function TJwSecurityToken.CheckTokenMembership(aSidToCheck:
  TJwSecurityId): boolean;
var
  bRes: longbool;
begin
  CheckTokenHandle('CheckTokenMembership');

  bRes := True;
  jwaWindows.CheckTokenMembership(Self.TokenHandle, @aSidToCheck.SID, bRes);
  Result := bRes;
end;

function TJwSecurityToken.IsEqual(aToken: TJwSecurityToken): boolean;
var
  NtCompareTokens: function(FirstTokenHandle, SecondTokenHandle: THandle;
    var Equal: boolean): NTSTATUS; stdcall;
  dRes: NTSTATUS;
begin
  if not Assigned(aToken) then
    Result := False;

  CheckTokenAccessType(TOKEN_QUERY, 'TOKEN_QUERY', 'IsEqual');
  aToken.CheckTokenAccessType(TOKEN_QUERY, 'TOKEN_QUERY', 'IsEqual');

  try
    @NtCompareTokens := TJwLibraryUtilities.LoadLibProc(ntdll, 'NtCompareTokens');
  except
    //on EJwaGetProcAddressError, EJwaLoadLibraryError do
    on Exception do
    begin
      raise EJwsclNotImplementedException.CreateFmtEx(
        RsTokenUnsupportedNTCompareTokens, 'IsEqual', ClassName,
        RsUNToken, 0, False, []);
    end;
  end;

  SetLastError(0);

  Result := False;
  if @NtCompareTokens <> nil then
  begin
    dRes := NtCompareTokens(TokenHandle, aToken.TokenHandle, Result);
    if not NT_SUCCESS(dRes) then
      raise EJwsclSecurityException.CreateFmtEx(
        RsTokenCallNTCompareTokensFailed, 'IsEqual', ClassName,
        RsUNToken, 0, True, [dRes]);
  end
  else
    raise EJwsclNotImplementedException.CreateFmtEx(
      RsTokenUnsupportedNTCompareTokens, 'IsEqual', ClassName,
      RsUNToken, 0, False, []);

end;

class procedure TJwSecurityToken.RemoveThreadToken(
  const Thread: TJwThreadHandle);
var
  pThread: ^TJwThreadHandle;
begin

  if Thread = 0 then
    pThread := nil
  else
    pThread := @Thread;

  if not jwaWindows.SetThreadToken(PHandle(pThread), 0) then
    raise EJwsclSecurityException.CreateFmtEx(RsTokenUnableRemoveToken,
      'SetThreadToken', ClassName, RsUNToken, 0, True, []);
end;


procedure TJwSecurityToken.SetThreadToken(const Thread: TJwThreadHandle);
var
  pThread: ^TJwThreadHandle;
begin
  CheckTokenHandle('SetThreadToken');

  if (TokenType <> TokenImpersonation) then
    raise EJwsclTokenPrimaryException.CreateFmtEx(
      RsTokeOnlyAttachImpersonatedToken, 'SetThreadToken',
      ClassName, RsUNToken, 0, False, []);

  if Thread = 0 then
    pThread := nil
  else
    pThread := @Thread;
  if not jwaWindows.SetThreadToken(PHandle(pThread), TokenHandle) then
    raise EJwsclSecurityException.CreateFmtEx(RsTokenFailedSetToken,
      'SetThreadToken', ClassName, RsUNToken, 0, True, []);
end;

procedure TJwSecurityToken.ImpersonateLoggedOnUser;
begin
  //primary TOKEN_QUERY and TOKEN_DUPLICATE access.
  //If hToken is an impersonation token,
  //it must have TOKEN_QUERY and TOKEN_IMPERSONATE access.

  if TokenType = TokenImpersonation then
    CheckTokenAccessType(TOKEN_QUERY + TOKEN_IMPERSONATE,
      'TOKEN_QUERY,TOKEN_IMPERSONATE', 'ImpersonateLoggedOnUser')
  else
    CheckTokenAccessType(TOKEN_QUERY + TOKEN_DUPLICATE,
      'TOKEN_QUERY,TOKEN_DUPLICATE', 'ImpersonateLoggedOnUser');


  if not jwaWindows.ImpersonateLoggedOnUser(TokenHandle) then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenFailedImpLoggedOnUser, 'ImpersonateLoggedOnUser',
      ClassName, RsUNToken, 0, True, []);
end;

class function TJwSecurityToken.PrivilegeCheck(
  const ClientToken: TJwSecurityToken;
  const RequiredPrivileges: TJwPrivilegeSet;
  const RequiresAllPrivs: TJwPrivCheck): boolean;
var
  i:     integer;
  privs: TJwPrivilegeSet;
  priv:  TJwPrivilege;
begin
  if not Assigned(ClientToken) then
    raise EJwsclInvalidTokenHandle.CreateFmtEx(RsWinCallFailed,
      'PrivilegeCheck', ClassName, RsUNToken, 0, False, ['ClientToken']);

  JwRaiseOnNilParameter(RequiredPrivileges,
    'PrivilegeCheck','PrivilegeCheckEx',ClassName,RsUNToken);

  Result := True;

  privs := ClientToken.GetTokenPrivileges;
  try
    for i := 0 to RequiredPrivileges.Count - 1 do
    begin
      priv := privs.GetPrivByLUID(RequiredPrivileges.PrivByIdx[i].LUID);

      if Assigned(priv) and priv.Enabled then
      begin
        priv.Privilege_Used_For_Access := True;

        if (RequiresAllPrivs = pcDefault) then
        begin
          Result := True;
          privs.Free;
          exit;
        end;
      end
      else
      begin
        if Assigned(priv) then
          priv.Privilege_Used_For_Access := False;
        if (RequiresAllPrivs = pcAllPrivsEnabled) then
        begin
          Result := False;
          privs.Free;
          exit;
        end;
      end;
    end;
  finally
    privs.Free;
  end;
end;

function TJwSecurityToken.PrivilegeCheck(
  const RequiredPrivileges: TJwPrivilegeSet;
  const RequiresAllPrivs: TJwPrivCheck
  ): boolean;
begin
  JwRaiseOnNilParameter(RequiredPrivileges,
    'RequiredPrivileges','PrivilegeCheck',ClassName,RsUNToken);

  //Do not mix up this call to PrivilegeCheckEx
  Result := PrivilegeCheck(Self, RequiredPrivileges, RequiresAllPrivs);
end;

function TJwSecurityToken.PrivilegeCheckEx(
  const RequiredPrivileges: TJwPrivilegeSet;
  const RequiresAllPrivs: TJwPrivCheck): boolean;
var
  pPriv: jwaWindows.PPRIVILEGE_SET;
  bRes:  longbool;
begin
  JwRaiseOnNilParameter(RequiredPrivileges,
    'RequiredPrivileges','PrivilegeCheckEx',ClassName,RsUNToken);

  result := false;

  pPriv := RequiredPrivileges.Create_PPRIVILEGE_SET;

  try
    {Overrule privilege set control}
    if (RequiresAllPrivs = pcAllPrivsEnabled) then
      pPriv.Control := PRIVILEGE_SET_ALL_NECESSARY
    else
      pPriv.Control := pPriv.Control and not PRIVILEGE_SET_ALL_NECESSARY;

    if not jwaWindows.PrivilegeCheck(TokenHandle, pPriv, bRes) then
      raise EJwsclSecurityException.CreateFmtEx(RsWinCallFailed,
        'PrivilegeCheckEx', ClassName, RsUNToken, 0, True, ['PrivilegeCheck']);

    Result := bRes;
  finally
    RequiredPrivileges.Free_PPRIVILEGE_SET(pPriv);
  end;
end;

class procedure TJwSecurityToken.PrivilegedServiceAuditAlarm(
  SubsystemName, ServiceName: TJwString; ClientToken: TJwSecurityToken;
  Privileges: TJwPrivilegeSet; AccessGranted: boolean);

var
  pPriv: jwaWindows.PPRIVILEGE_SET;
  privs: TJwPrivilegeSet;

  primToken:     TJwSecurityToken;
  bOldAuditPriv: boolean;
begin
  bOldAuditPriv := False;
  if not Assigned(ClientToken) then
    raise EJwsclInvalidTokenHandle.CreateFmtEx(RsWinCallFailed,
      'PrivilegedServiceAuditAlarm', ClassName, RsUNToken, 0,
      True, ['ClientToken']);


  {PrivilegedServiceAuditAlarm checks the process token for the needed privilege SE_AUDIT_NAME.
   So we open it here.
   The thread that calls this function does not need that privilege.

   We open the token with minimal access.
  }
  primToken := TJwSecurityToken.CreateTokenByProcess(0,
    TOKEN_READ or TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES or
    TOKEN_AUDIT_SUCCESS_INCLUDE or TOKEN_AUDIT_SUCCESS_EXCLUDE or
    TOKEN_AUDIT_FAILURE_INCLUDE or TOKEN_AUDIT_FAILURE_EXCLUDE);

  {first we try to get status of SE_AUDIT_NAME privilege.
   Maybe the process has not the privilege?

   We save the privilege status for later resetting.
  }
  try
    bOldAuditPriv := primToken.PrivilegeEnabled[SE_AUDIT_NAME];

    //not enable privilege
    primToken.PrivilegeEnabled[SE_AUDIT_NAME] := True;

    //now we set all privileges of the client token, so they will be shown in the audit log message
    privs := ClientToken.GetTokenPrivileges;
    pPriv := privs.Create_PPRIVILEGE_SET;

    if not
{$IFDEF UNICODE}PrivilegedServiceAuditAlarmW{$ELSE}
      PrivilegedServiceAuditAlarmA
{$ENDIF}
      (TJwPChar(SubsystemName), TJwPChar(ServiceName),
      ClientToken.TokenHandle, pPriv^, AccessGranted) then
    begin
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'PrivilegedServiceAuditAlarm',
        ClassName, RsUNToken, 0, True, ['PrivilegeCheck']);
    end;

  finally
    try
      //reset privilege to old status
      primToken.PrivilegeEnabled[SE_AUDIT_NAME] := bOldAuditPriv;
    finally
      privs.Free_PPRIVILEGE_SET(pPriv);
      FreeAndNil(privs);
      //free token
      primToken.Free;
    end;
  end;
end;

class function TJwSecurityToken.CopyLUID(const originalLUID: TLUID): TLUID;
begin
  Result.LowPart  := originalLUID.LowPart;
  Result.HighPart := originalLUID.HighPart;
end;

function TJwSecurityToken.GetTokenStatistics: TJwSecurityTokenStatistics;
var
  stat: PTokenStatistics;
begin
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}*)
    JwaWindows.TokenStatistics, Pointer(stat));

  Result := TJwSecurityTokenStatistics.Create(stat^);

  HeapFree(GetProcessHeap, 0, stat);
end;

//function LsaGetUserName(const UserName, DomainName : PUNICODE_STRING) : NTSTATUS; stdcall; external 'advapi32';

function TJwSecurityToken.GetUserName : TJwString;
var
  //Data : TJwLsaLogonSessionData;
  //UserName : PUNICODE_STRING;


  Buffer : PWideChar;
  Size : DWORD;
begin
  Buffer := nil;


 // UserName := JwCreateUnicodeString('');
 // if NT_ERROR(LsaGetUserName(@UserName, nil)) then
 //   RaiseLastOSError;
   Size := 0;
   GetUserNameW(Buffer, Size);

   GetMem(Buffer, (Size+2) * sizeof(WideChar));
   GetUserNameW(Buffer, Size);
   try
     result := TJwString(Buffer);
   finally
     FreeMem(Buffer);
   end;


  {
  does only work with admin privs

  Data := TJwLsaLogonSession.GetSessionData(TokenOrigin);
  try
    result := Data.UserName;
  finally
    Data.Free;
  end;}
end;

function TJwSecurityToken.GetTokenUserName : TJwString;
var U : TJwSecurityId;
begin
  U := TokenUser;
  try
    result := U.AccountName[U.CachedSystemName];
  finally
    U.Free;
  end;
end;

function TJwSecurityToken.UnLoadUserProfile(var ProfileInfo : TJwProfileInfo) : Boolean;
//var IntProfileInfo : {$IFDEF UNICODE}TProfileInfoW;{$ELSE}TProfileInfoA;{$ENDIF}
begin
  result := false;
  SetLastError(ERROR_INVALID_HANDLE);

  if not JwIsHandleValid(ProfileInfo.Profile) then
    exit;

  result := JwaWindows.UnloadUserProfile(TokenHandle, ProfileInfo.Profile);

  ProfileInfo.Profile := INVALID_HANDLE_VALUE;
end;

procedure TJwSecurityToken.LoadUserProfile(
         var ProfileInfo : TJwProfileInfo;
         const ProfileMembers : TJwProfileMembers);

  function GetRoamingProfile(const UserName : TJwString) : TJwString;
  var DomainControllerInfo : {$IFDEF UNICODE}PDomainControllerInfoW{$ELSE}PDomainControllerInfoA{$ENDIF};
      Res : DWORD;
      intServerName : TJwPChar;

      Data     : LPBYTE;
      UserInfo : PUSER_INFO_3;
  begin
    result := '';
    DomainControllerInfo := nil;
    
    try
      //ask domain controller for its name
      Res := {$IFDEF UNICODE}DsGetDcNameW{$ELSE}DsGetDcNameA{$ENDIF}
        (nil, nil, nil, nil, DS_BACKGROUND_ONLY or
        DS_RETURN_FLAT_NAME, DomainControllerInfo);
    except
      //raises exception on Systems older than Win2000
      Res := 1;
    end;

    if Res = ERROR_SUCCESS then
      intServerName := DomainControllerInfo.DomainControllerName
    else
      intServerName := nil;

    if DomainControllerInfo <> nil then
      NetApiBufferFree(DomainControllerInfo);

    //get user information for roaming profile   
    if NetUserGetInfo(
        PWideChar(WideString(intServerName)),//__in   LPCWSTR servername,
        PWideChar(WideString(UserName)),//__in   LPCWSTR username,
        3, //__in   DWORD level,
        Data//__out  LPBYTE* bufptr
      ) <> NERR_Success then
      exit;

    if Data <> nil then
    begin
      UserInfo := PUSER_INFO_3(Data);
      result := TJwString(UserInfo.usri3_profile);

      NetApiBufferFree(Data);
    end;
  end;

var ProfilePath : TJwString;
    IntProfileInfo : {$IFDEF UNICODE}TProfileInfoW;
    {$ELSE}TProfileInfoA;{$ENDIF}

    PrivScope : IJwPrivilegeScope;

begin
  {
  Enable restore and backup privilege for Loaduserprofile.
  This is not necessary but we check for its availability to inform
  the caller in error case.
  Otherwise Loaduserprofile would just return 5 (Access denied).

  The Interface disables them at the end of this function or in case of an exception
  }
  PrivScope := JwGetPrivilegeScope([SE_RESTORE_NAME, SE_BACKUP_NAME]);

 

  IntProfileInfo.dwSize := Sizeof(IntProfileInfo);

  with IntProfileInfo do
  begin
    if not (pmFlags in ProfileMembers) then
      dwFlags := PI_NOUI
    else
      dwFlags := ProfileInfo.Flags;

    if not (pmUserName in ProfileMembers) then
    begin
      try
        lpUserName := TJwPChar(GetTokenUserName)
      except
        on E : EJwsclWinCallFailedException do
          lpUserName := nil;
      end;
    end
    else
      lpUserName := TJwPChar(ProfileInfo.UserName);

    if not (pmProfilePath in ProfileMembers) then
    begin
      try
        ProfilePath := GetRoamingProfile(lpUserName); //must be saved in a stack var
        lpProfilePath := TJwPChar(ProfilePath);
      except
        lpProfilePath := nil;
      end;
    end
    else
      lpProfilePath := TJwPChar(ProfileInfo.ProfilePath);

    if not (pmDefaultPath in ProfileMembers) then
      lpDefaultPath := nil
    else
      lpDefaultPath := TJwPChar(ProfileInfo.DefaultPath);

    if not (pmServerName in ProfileMembers) then
      lpServerName := nil
    else
      lpServerName := TJwPChar(ProfileInfo.ServerName);

    if not (pmPolicyPath in ProfileMembers) then
      lpPolicyPath := nil
    else
      lpPolicyPath := TJwPChar(ProfileInfo.PolicyPath);

    hProfile := 0;
  end;

  if not {$IFDEF UNICODE}LoadUserProfileW{$ELSE}LoadUserProfileA{$ENDIF}
    (TokenHandle, IntProfileInfo) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'LoadUserProfile',
      ClassName, RsUNToken, 0, True, ['LoadUserProfile']);

  with ProfileInfo do
  begin
    Flags        := IntProfileInfo.dwFlags;
    UserName     := IntProfileInfo.lpUserName;
    ProfilePath  := IntProfileInfo.lpProfilePath;
    DefaultPath  := IntProfileInfo.lpDefaultPath;
    ServerName   := IntProfileInfo.lpServerName;
    PolicyPath   := IntProfileInfo.lpPolicyPath;
    Profile      := IntProfileInfo.hProfile;
  end;
end;

function TJwSecurityToken.GetCurrentUserRegKey(
  const DesiredAccess: TJwAccessMask): HKEY;
var
  Res: HRESULT;
begin
  Res := RegOpenCurrentUser(DesiredAccess, Result);
  if Res <> 0 then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus, 'RegOpenCurrentUser',
      ClassName, RsUNToken, 0, True, ['RegOpenCurrentUser', Res]);
end;




//instance function related to token context
class procedure TJwSecurityToken.ImpersonateAnonymousToken(
  const Thread: TJwThreadHandle);
begin
  if not jwaWindows.ImpersonateAnonymousToken(Thread) then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenFailedImpAnonymousToken, 'ImpersonateAnonymousToken',
      ClassName, RsUNToken, 0, True, []);
end;

class procedure TJwSecurityToken.ImpersonateSelf(
  const anImpersonationLevel: SECURITY_IMPERSONATION_LEVEL);
begin
  if not jwaWindows.ImpersonateSelf(anImpersonationLevel) then
    raise EJwsclSecurityException.CreateFmtEx(RsTokenFailedImpSelf,
      'ImpersonateSelf', ClassName, RsUNToken, 0, True, []);
end;

class procedure TJwSecurityToken.RevertToSelf;
begin
  if not jwaWindows.RevertToSelf then
    raise EJwsclSecurityException.CreateFmtEx(RsTokenFailedRevertSelf,
      'RevertToSelf', ClassName, RsUNToken, 0, True, []);
end;

class procedure TJwSecurityToken.ImpersonateNamedPipeClient(
  hNamedPipe: THandle);
begin
  if not jwaWindows.ImpersonateNamedPipeClient(hNamedPipe) then
    raise EJwsclSecurityException.CreateFmtEx(
      RsTokenFailedImpPipe, 'ImpersonateNamedPipeClient',
      ClassName, RsUNToken, 0, True, []);
end;

class function TJwSecurityToken.HasThreadAToken(): boolean;
var
  Handle : DWORD;
begin
  Handle := INVALID_HANDLE_VALUE;
  result := OpenThreadToken(GetCurrentThread, TOKEN_QUERY or TOKEN_READ, false, Handle);

  if not result then
    result := OpenThreadToken(GetCurrentThread, TOKEN_QUERY or TOKEN_READ, true, Handle);

  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle);
end;

class function TJwSecurityToken.GetThreadToken(
  const aDesiredAccess: TJwAccessMask;
  const anOpenAsSelf: boolean): TJwSecurityToken;
begin
  try
    Result := TJwSecurityToken.CreateTokenByThread(0, aDesiredAccess,
      anOpenAsSelf);
  except
    Result := nil;
  end;
end;



function TJwSecurityToken.GetTokenType: TOKEN_TYPE;
var
  ptrTokenType: PTOKEN_TYPE;
begin
  //Raises an exception if errors occur
  Self.GetTokenInformation(fTokenHandle,
(*{$IFDEF SL_OMIT_SECTIONS}JwsclLibrary.{$ELSE}
    JwsclTypes.
{$ENDIF}     *)
    JwaWindows.TokenType, Pointer(ptrTokenType));

  Result := ptrTokenType^;

  HeapFree(GetProcessHeap, 0, ptrTokenType);
end;

procedure TJwSecurityToken.CheckTokenPrivileges(Privileges: array of TJwString);
var
  privSet: TJwPrivilegeSet;
  i: integer;
begin
  privSet := GetTokenPrivileges;
  try
    for i := Low(Privileges) to High(Privileges) do
    begin
      if not Assigned(privSet.PrivByName[Privileges[i]]) then
      begin
        raise EJwsclPrivilegeCheckException.CreateFmtEx(
          RsTokenPrivilegeNotFound, 'CheckTokenPrivileges',
          ClassName, RsUNToken, 0, False, [Privileges[i]]);
      end;
    end;
  finally
    privSet.Free;
  end;
end;


function TJwSecurityToken.IsPrivilegeAvailable(Priv: TJwString): boolean;
var
  privSet: TJwPrivilegeSet;
begin
  privSet := GetTokenPrivileges;
  try
    Result := Assigned(privSet.PrivByName[Priv]);
  finally
    privSet.Free;
  end;
end;

function TJwSecurityToken.GetIsRestricted: boolean;
  function IsRestrictedToken : Boolean;
  var
    i : Integer;
    Group : TJwSecurityIdList;
  begin
    result := false;
    Group := TokenGroups;
    try
      for i := 0 to Group.Count-1 do
      begin
        result := sidaGroupUseForDenyOnly in Group.Items[i].AttributesType;
        if result then
          break;
      end;
    finally
      Group.Free;
    end;
  end;
begin
  Result := True;
  SetLastError(0);
  if not IsTokenRestricted(TokenHandle) then
  begin
    if GetLastError() <> 0 then
      raise EJwsclInvalidTokenHandle.CreateFmtEx(
        RsWinCallFailed, 'IsRestricted', ClassName, RsUNToken,
        0, True, ['IsTokenRestricted']);

    Result := False;
  end;

  {Safe mechanism
  check for restrited sids in the token groups for ourselves
  since the api call is unreliable.
  }
  if not result then
    result := IsRestrictedToken;
end;



procedure TJwSecurityToken.CheckTokenAccessType(aDesiredAccessMask:
  TJwAccessMask;
  StringMask, SourceProc: TJwString);

  function IntToBin(Value: cardinal): TJwString;
  var
    i: integer;
  begin
    Result := '';
    for i := sizeof(cardinal) * 8 downto 0 do
      if Value and (1 shl i) <> 0 then
        Result := Result + '1'
      else
        Result := Result + '0';
  end;

begin
  if (Self.AccessMask and aDesiredAccessMask) <> aDesiredAccessMask then
    raise EJwsclAccessTypeException.CreateFmtEx(
      RsTokenCheckAccessTypeText, SourceProc, ClassName, RsUNToken,
      0, False, [IntToBin(aDesiredAccessMask), StringMask,
      IntToBin(AccessMask), SourceProc,
      JwFormatAccessRights(aDesiredAccessMask, TokenMapping),
      JwFormatAccessRights(AccessMask, TokenMapping)
      ]);
end;


constructor TJwSecurityTokenStatistics.Create(stats: TTokenStatistics);
begin
  with stats do
  begin
    fTokenId    := TJwSecurityToken.CopyLUID(TokenId);
    fAuthenticationId := TJwSecurityToken.CopyLUID(AuthenticationId);
    fExpirationTime := ExpirationTime;
    fTOKEN_TYPE := TokenType;
    fSECURITY_IMPERSONATION_LEVEL := SECURITY_IMPERSONATION_LEVEL;
    fDynamicCharged := DynamicCharged;
    fDynamicAvailable := DynamicAvailable;
    fGroupCount := GroupCount;
    fPrivilegeCount := PrivilegeCount;
    fModifiedId := TJwSecurityToken.CopyLUID(ModifiedId);
  end;
end;

function TJwSecurityTokenStatistics.GetText: TJwString;

  function FileTime2DateTime(FileTime: FileTime): TDateTime;
  var
    LocalFileTime: TFileTime;
    SystemTime:    TSystemTime;
  begin
    FileTimeToLocalFileTime(FileTime, LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SystemTime);

    Result := SystemTimeToDateTime(SystemTime);
  end;

  function GetExpirationTimeString: TJwString;
  var
    F: FileTIME;
  begin
    if fExpirationTime.QuadPart >= $7FFFFFFFFFFFFFFF then
    begin
      Result := RsInfinite;
      exit;
    end;
    F.dwLowDateTime  := fExpirationTime.LowPart;
    F.dwHighDateTime := fExpirationTime.HighPart;
    try
      Result := DateTimeToStr(FileTime2DateTime(F));
    except
      on E: EConvertError do
        Result := RsInfinite;
    end;
  end;

begin
  Result := '';
  {
  RsTokenStatisticsText = 'TokenID: %0:s\r\AuthenticationId: %1:s\r\nExpirat' +
    'ionTime: %2:s\r\nToken type: %3:d\r\nImpersonation level: 0x%4:x\r\nDynam' +
    'ic charged: 0x%5:x\r\nDynamic available: 0x%6:x\r\nGroup count: %7:d\r\nP' +
    'rivilege count: %8:d\r\nModified ID: %9:s\r\n';
 
  }

  Result := JwFormatString(RsTokenStatisticsText,
    [TJwPrivilege.LUIDtoText(fTokenId), //0
    TJwPrivilege.LUIDtoText(fAuthenticationId), //1
    GetExpirationTimeString, //2
    integer(fTOKEN_TYPE), //3
    integer(fSECURITY_IMPERSONATION_LEVEL), //4
    integer(fDynamicCharged), //5
    integer(fDynamicAvailable), //6
    fGroupCount,     //7
    fPrivilegeCount, //8
    TJwPrivilege.LUIDtoText(fModifiedId) //9
    ]);
  {
  Result := Result + 'TokenID: ' + TJwPrivilege.LUIDtoText(fTokenId) + #13#10;
  Result := Result + 'AuthenticationId: ' + TJwPrivilege.LUIDtoText(
    fAuthenticationId) + #13#10;

  Result := Result + 'ExpirationTime: ' + GetExpirationTimeString + #13#10;
  Result := Result + 'TOKEN_TYPE: 0x' + IntToHex(integer(fTOKEN_TYPE), 2) + #13#10;
  Result := Result + 'SECURITY_IMPERSONATION_LEVEL: 0x' + IntToHex(
    integer(fSECURITY_IMPERSONATION_LEVEL), 2) + #13#10;
  Result := Result + 'DynamicCharged: 0x' + IntToHex(fDynamicCharged, 2) + #13#10;
  Result := Result + 'DynamicAvailable: 0x' + IntToHex(
    fDynamicAvailable, 2) + #13#10;
  Result := Result + 'GroupCount: 0x' + IntToHex(fGroupCount, 2) + #13#10;
  Result := Result + 'PrivilegeCount: 0x' + IntToHex(fPrivilegeCount, 2) + #13#10;
  Result := Result + 'ModifiedId: ' + TJwPrivilege.LUIDtoText(fModifiedId) + #13#10;      }
end;

constructor TJwSecurityToken.CreateLogonUser(sUsername, sDomain,
  sPassword: TJwString; dwLogonType, dwLogonProvider: cardinal);

var
  bResult: boolean;
begin
  Create;

  fShared     := False;
  fAccessMask := TOKEN_ALL_ACCESS;

  bResult :=
{$IFDEF UNICODE}LogonUserW{$ELSE}
    LogonUserA
{$ENDIF}
    (TJwPChar(sUserName), TJwPChar(sDomain), TJwPChar(sPassword),
    dwLogonType, dwLogonProvider, fTokenHandle);

  if (not bResult) then
    raise EJwsclWinCallFailedException.CreateFmtEx(RsWinCallFailed,
      'CreateLogonUser', ClassName, RsUNToken, 0, True, ['LogonUser']);
end;




constructor TJwSecurityToken.CreateNewToken(
  const aDesiredAccess: TJwAccessMask;
  const anObjectAttributes: TObjectAttributes; const anAuthenticationId: TLUID;
  const anExpirationTime: int64; anUser: TJwSecurityId;
  aGroups: TJwSecurityIdList; aPrivileges: TJwPrivilegeSet;
  anOwner, aPrimaryGroup: TJwSecurityId; aDefaultDACL: TJwDAccessControlList;
  aTokenSource: TTokenSource);
var
  ttTokenUser:  TTokenUser;
  pGroups:      PTOKEN_GROUPS;
  pPrivileges:  PTOKEN_PRIVILEGES;
  ttTokenOwner: TTokenOwner;
  ttTokenPrimaryGroup: TTokenPrimaryGroup;
  ttTokenDefaultDACL: TTokenDefaultDacl;

  res: cardinal;
  CurrentToken: TJwSecurityToken;
begin
  fShared := False;
  fPrivelegesList      := nil;
  fStackPrivilegesList := nil;

  // create token privilege must be active in Vista
  // in Vista even SYSTEM does not have this privilege
  // but we could impersonate any other process (like csrss.exe)
  // that has this privilege
  JwEnablePrivilege(SE_CREATE_TOKEN_NAME, pst_Enable);

  CurrentToken := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);
  try
    if not Assigned(anOwner) then
      anOwner := CurrentToken.GetTokenOwner;
    ttTokenOwner.Owner := anOwner.CreateCopyOfSID;

    if not Assigned(aPrimaryGroup) then
      aPrimaryGroup := CurrentToken.GetPrimaryGroup;
    ttTokenPrimaryGroup.PrimaryGroup := aPrimaryGroup.CreateCopyOfSID;


    FillChar(ttTokenUser.User, sizeof(ttTokenUser.User), 0);
    if not Assigned(anUser) then
      anUser := CurrentToken.GetTokenUser;
    ttTokenUser.User.Sid := anUser.CreateCopyOfSID;

    if not Assigned(aGroups) then
    begin
      pGroups := nil;
      aGroups := CurrentToken.GetTokenGroups;
      if Assigned(aGroups) then
      begin
        pGroups := aGroups.Create_PTOKEN_GROUPS;
        aGroups.Free;
      end;
    end
    else
      pGroups := aGroups.Create_PTOKEN_GROUPS;

    if not Assigned(aPrivileges) then
    begin
      pPrivileges := nil;
      aPrivileges := CurrentToken.GetTokenPrivileges;
      if Assigned(aPrivileges) then
      begin
        pPrivileges := aPrivileges.Create_PTOKEN_PRIVILEGES;
        aPrivileges.Free;
      end;
    end
    else
      pPrivileges := aPrivileges.Create_PTOKEN_PRIVILEGES;

    if not Assigned(aDefaultDACL) then
    begin
      ttTokenDefaultDACL.DefaultDacl := nil;
      aDefaultDACL := CurrentToken.GetTokenDefaultDacl;
      if Assigned(aDefaultDACL) then
      begin
        ttTokenDefaultDACL.DefaultDacl := aDefaultDACL.Create_PACL;
        aDefaultDACL.Free;
      end;
    end
    else
      ttTokenDefaultDACL.DefaultDacl := aDefaultDACL.Create_PACL;


    try
      res := ZwCreateToken(@fTokenHandle,//TokenHandle: PHANDLE;
        aDesiredAccess,//DesiredAccess: ACCESS_MASK;
        @anObjectAttributes,//ObjectAttributes: POBJECT_ATTRIBUTES;
        TokenPrimary, //Type_: TOKEN_TYPE;
        @anAuthenticationId,//AuthenticationId: PLUID;
        @anExpirationTime,//ExpirationTime: PLARGE_INTEGER;
        @ttTokenUser, //User: PTOKEN_USER;
        pGroups,      //Groups: PTOKEN_GROUPS;
        pPrivileges,  //Privileges: PTOKEN_PRIVILEGES;
        @ttTokenOwner,//Owner: PTOKEN_OWNER;
        @ttTokenPrimaryGroup,//PrimaryGroup: PTOKEN_PRIMARY_GROUP;
        @ttTokenDefaultDACL,//DefaultDacl: PTOKEN_DEFAULT_DACL;
        @aTokenSource //Source: PTOKEN_SOURCE):
        );

      res := RtlNtStatusToDosError(Res);
      SetLastError(res);

      if res <> 0 then
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed, 'CreateNewToken', ClassName, RsUNToken,
          0, True, ['ZwCreateToken']);
    finally
      TJwSecurityId.FreeSID(ttTokenOwner.Owner);
      TJwSecurityId.FreeSID(ttTokenPrimaryGroup.PrimaryGroup);
      TJwSecurityId.FreeSID(ttTokenUser.User.Sid);
      TJwSecurityIdList.Free_PTOKEN_GROUPS(pGroups);
      try
        aPrivileges.Free_PTOKEN_PRIVILEGES(pPrivileges);
      except
      end;
      TJwDAccessControlList.Free_PACL(ttTokenDefaultDACL.DefaultDacl);

      JwEnablePrivilege(SE_CREATE_TOKEN_NAME, pst_Disable);

    end;

    
    try
      fAccessMask := GetMaximumAllowed;
    except
      //if GetMaximumAllowed does not have the READ_CONTROL flag
      //we may fail here, so just set access mask to zero
      //many people don't even read it.
      fAccessMask := 0;
    end;
    
    

    fPrivelegesList      := TObjectList.Create(False);
    fStackPrivilegesList := TObjectList.Create(True);
  finally
    CurrentToken.Free;
  end;
end;



class function TJwSecurityToken.Create_OBJECT_ATTRIBUTES(
  const aRootDirectory: THandle; const anObjectName: TJwString;
  const anAttributes: cardinal;
  const aSecurityDescriptor: TJwSecurityDescriptor;
  const anImpersonationLevel: TSecurityImpersonationLevel;
  const aContextTrackingMode: SECURITY_CONTEXT_TRACKING_MODE;
  const anEffectiveOnly: boolean): TObjectAttributes;
var
  sqos: PSECURITY_QUALITY_OF_SERVICE;
begin
  FillChar(Result, sizeof(Result), 0);
  Result.Length     := sizeof(Result);
  Result.RootDirectory := aRootDirectory;
  Result.Attributes := anAttributes;
  Result.ObjectName := JwCreateUnicodeString(anObjectName);

  if Assigned(aSecurityDescriptor) then
    Result.SecurityDescriptor := aSecurityDescriptor.Create_SD(False);

  GetMem(sqos, sizeof(SECURITY_QUALITY_OF_SERVICE));
  sqos.Length := sizeof(SECURITY_QUALITY_OF_SERVICE);
  sqos.ImpersonationLevel := anImpersonationLevel;
  sqos.ContextTrackingMode := aContextTrackingMode;
  sqos.EffectiveOnly := anEffectiveOnly;

  Result.SecurityQualityOfService := sqos;
end;

class procedure TJwSecurityToken.Free_OBJECT_ATTRIBUTES(
  anObjectAttributes: TObjectAttributes);
begin
  if (anObjectAttributes.ObjectName <> nil) then
    RtlFreeUnicodeString(anObjectAttributes.ObjectName);

  if (anObjectAttributes.SecurityDescriptor <> nil) then
    TJwSecurityDescriptor.Free_SD(PSECURITY_DESCRIPTOR(
      anObjectAttributes.SecurityDescriptor));

  if (anObjectAttributes.SecurityQualityOfService <> nil) then
    FreeMem(anObjectAttributes.SecurityQualityOfService);

  FillChar(anObjectAttributes, sizeof(anObjectAttributes), 0);
end;

function TJwSecurityToken.GetSecurityDescriptor(
  const SecurityFlags: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
begin
  CheckTokenHandle('GetSecurityDescriptor');
  CheckTokenAccessType(TOKEN_READ, 'TOKEN_READ',
    'GetSecurityDescriptor');

  Result := TJwSecureGeneralObject.GetSecurityInfo(TokenHandle,
    SE_KERNEL_OBJECT, SecurityFlags);
end;

procedure TJwSecurityToken.SetSecurityDescriptor(
  const SecurityFlags: TJwSecurityInformationFlagSet;
  const SecurityDescriptor: TJwSecurityDescriptor);
begin
  CheckTokenHandle('SetSecurityDescriptor');
  CheckTokenAccessType(TOKEN_WRITE, 'TOKEN_WRITE',
    'GetSecurityDescriptor');

  TJwSecureGeneralObject.SetSecurityInfo(TokenHandle,
    SE_KERNEL_OBJECT, SecurityFlags, SecurityDescriptor);
end;

procedure TJwSecurityToken.RaiseOnInvalidPrimaryToken(MethodName: TJwString);
var
  Token: TJwSecurityToken;
  Sid:   TJwSecurityID;
begin
  JwInitWellKnownSIDs; //loads JwLocalSystemSID if not already done

  Token := TJwSecurityToken.CreateTokenByProcess(0, TOKEN_READ or TOKEN_QUERY);
  try
    Sid := Token.GetTokenUser;
    try
      if not Sid.EqualSid(JwLocalSystemSID) then
        raise EJwsclInvalidPrimaryToken.CreateFmtEx(
          RsPrimaryTokenMustBeSystem, MethodName, ClassName,
          RsUNToken, 0, False, []);
    finally
      Sid.Free;
    end;
  finally
    Token.Free;
  end;
end;


function TJwSecurityToken.IsTokenType(index: Integer): Boolean;
begin
  case index of
    {primary}1 : result := TokenType = TokenPrimary;
    {impersonation}2 : result := TokenType = TokenImpersonation;
  else
    result := false;
  end;
end;


function TJwSecurityToken.Equals(Obj: TObject): Boolean;
begin
  result := IsEqual(Obj as TJwSecurityToken);
end;

function TJwSecurityToken.GetHashCode: Integer;
var
  P : Pointer;
begin
  P := JwBeginCreateHash;

   JwIntegerHash(P, TokenHandle);
   JwIntegerHash(P, Integer(TokenType));
   JwIntegerHash(P, Integer(Shared));
   JwIntegerHash(P, Integer(TokenOrigin.LowPart));
   JwStringHash(P, ClassName);
   try
     JwStringHash(P, TokenUserName);
   except
     {
     http://msdn.microsoft.com/en-us/magazine/cc163883.aspx
     If the token is opened with SECURITY_IDENTIFICATION
     this fails.
     }
   end;

   JwStringHash(P, UserName);


  result := JwEndCreateHash(P);
end;

function TJwSecurityToken.ToString: String;
var
  Groups,Restricted : TJwSecurityIdList;
  Privs : TJwPrivilegeSet;
  sUserName : String;
begin
  Groups := TokenGroups;
  Restricted := TokenRestrictedSids;
  Privs := GetTokenPrivileges;

  try
    sUserName := TokenUserName;
  except
   {
   http://msdn.microsoft.com/en-us/magazine/cc163883.aspx
   If the token is opened with SECURITY_IDENTIFICATION
   this fails.
   }
    sUserName := '<Access denied>';
  end;

  try
    result := JwCreateToString(
    [ClassName,'',
     'Hash',GetHashCode,
     'Handle',TokenHandle,
     'AccessMask', JwFormatAccessRightsSimple(AccessMask, TokenMapping),
     'IsPrimaryToken',IsPrimaryToken,
     'IsThreadtoken',IsThreadToken,
     'IsRestricted',IsRestricted,
     'SessionID',TokenSessionId,
     'PrimaryGroup',PrimaryGroup.ToString,
     'TokenUserName',sUserName,
     'UserName',UserName,
     'TokenType', GetEnumName(TypeInfo(TOKEN_TYPE), integer(TokenType)),
     'Shared', Shared,
     'Origin',Format('%d.%d',[TokenOrigin.HighPart, TokenOrigin.LowPart]),
     'Groups', Groups.Count,
     'GroupMembers',Groups.GetText(true), //TODO: instead .ToString
     'Restricted', Restricted.Count,
     'RestrictedMembers',Restricted.GetText(true), //TODO: instead .ToString
     'Privileges',Privs.Count,
     'PrivilegesMembers',Privs.GetText //TODO: instead .ToString
     ]);

  finally
    Groups.Free;
    Restricted.Free;
    Privs.Free;
  end;
end;




function TJwSecurityToken.RetrieveSpecificAccessRights(
  const AccessMask: TJwAccessMask): TJwAccessMask;
begin
  try
    //TODO: GENERICS hier noch!
    if AccessMask = MAXIMUM_ALLOWED then
      result := GetMaximumAllowed
    else
      result := TJwSecurityTokenMapping.GenericMap(AccessMask)
  except
    //if GetMaximumAllowed does not have the READ_CONTROL flag
    //we may fail here, so just set access mask to zero
    //many people don't even read it.
    result := 0;
  end;
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}





initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
  //warning do not add here code lines!!
  JwProcessHeap := GetProcessHeap;
  //add code from here
  //...
{$ENDIF SL_INITIALIZATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
