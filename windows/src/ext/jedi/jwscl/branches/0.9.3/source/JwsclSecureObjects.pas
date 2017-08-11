{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides access to secure windows objects (files, registry and other handles)

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
The Original Code is JwsclSecureObjects.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclSecureObjects;
{$INCLUDE ..\includes\Jwscl.inc}

// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
interface

uses SysUtils, Classes, Registry, Dialogs,
  Contnrs {used for TQueue},
  jwaWindows, JwsclResource,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclUtils,
  JwsclVersion, JwsclConstants,JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  {<b>TJwOnGetNamedSecurityInfo</b> is called by if a security information must be retrieved.
   GetFileInheritanceSource calls this event for the main object and every parent which
   permissions are retrieved.

   @param PathName Defines the full path name to the object which permission must be retrieved.
   @param SeType Defines the type of the object. It is intended for a direct call to WINAPI GetNamedSecurityInfo.
           GetFileInheritanceSource always sets it to SE_FILE_OBJECT.
   @param aSecurityInfo defines the type of security information to be retrieved (Owner, Group, DACL, SACL).
           GetFileInheritanceSource sets it to siDaclSecurityInformation.
   @param OwnedSD Receives whether the returned security descriptor can be freed securely.
           Set to true if the caller (of this event) should not free it; otherwise set to false (default).
   @param Data This parameter contains user defined data.

   @return Returns the security descriptor for this object.
     The caller frees the security descriptor if parameter OwnedSD is false (default).
     If the return value is nil the result depends on the caller. GetFileInheritanceSource aborts
     the current process and returns the inheritance as it was in this moment.

   @raises
     Exception Every exception raised in this event is propagated to the parent of
       the caller of this event. Allocated memory is freed.
  }
  TJwOnGetNamedSecurityInfo =
    function (PathName : TJwString; SeType : TSeObjectType; const aSecurityInfo: TJwSecurityInformationFlagSet;
       var OwnedSD : Boolean; const Data : Pointer) : TJwSecurityDescriptor of object;


     {<B>TJwFnProgressMethod</B> is a callback method that is used by TreeFileObjectSetNamedSecurityInfo.
       It is called on errors or every object which security information is set.

      @param pObjectName contains the object name
      @param cStatus constains the status of the last operation (GetLastError) 
      @param pInvokeSetting defines the current operation and can be changed to set the next step.
             The following values are recognized :

                 # pis_ProgressCancelOperation This value stops the propagation and ends the called function. Warning: This can lead to unpredictable ACLs 
                 # pis_ProgressRetryOperation Retries to set or get the security information
                 # Any other constants is used to ignore the current object and resume on the next one 
                 
                 
      @param E Contains an exception that maybe has more information. This Exception is a type of EJwsclSecurityException. It can be nil. 
      @param Args Contains user defined data provided to the function that calls back.
      @param bSecuritySet Defines whether security information was set (true) or not (false)

      }
  TJwFnProgressMethod = procedure(
    const pObjectName: TJwString;
    // Name of object just processed
    const cStatus: Cardinal;
    // Status of operation on object
    var pInvokeSetting: TJwProgInvokeSetting;
    // When to set
    const E: EJwsclSecurityException;
    //Exception type that was raised or nil if none- only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
    const Args: Pointer;
    // Caller specific data
    const bSecuritySet:
    boolean                 // Whether security was set
    ) of object;
     {<B>TJwFnProgressProcedure</B> is a callback procedure that is used by TreeFileObjectSetNamedSecurityInfo.
       It is called on errors or every object which security information is set.

      @param pObjectName contains the object name 
      @param cStatus constains the status of the last operation (GetLastError) 
      @param pInvokeSetting defines the current operation and can be changed to set the next step.
             The following values are recognized :
                
                 # pis_ProgressCancelOperation This value stops the propagation and ends the called function. Warning: This can lead to unpredictable ACLs 
                 # pis_ProgressRetryOperation Retries to set or get the security information
                 # Any other constants is used to ignore the current object and resume on the next one
                 
                 
      @param E Contains an exception that maybe has more information. This Exception is a type of EJwsclSecurityException. It can be nil. 
      @param Args Contains user defined data provided to the function that calls back.
      @param bSecuritySet Defines whether security information was set (true) or not (false)

      }
  TJwFnProgressProcedure = procedure(
    const pObjectName: TJwString;
    // Name of object just processed
    const cStatus: Cardinal;
    // Status of operation on object
    var pInvokeSetting: TJwProgInvokeSetting;
    // When to set
    const E: EJwsclSecurityException;
    //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
    const Args: Pointer;
    // Caller specific data
    const bSecuritySet:
    boolean                // Whether security was set
    );
  {TBD}
  TJwOnThreadExecute = function(Data: Pointer): integer of object;


     {<B>TJwTagThread</B> is used by
       
        # TreeFileObjectSetNamedSecurityInfo
        

     }
  TJwTagThread = class(TThread)
  protected
    pTag:       Pointer; //data supplied to fOnExecute
    fOnExecute: TJwOnThreadExecute;
    //creates the thread - do not call!
    constructor Create(CreateSuspended: boolean); reintroduce; overload;
    //thread execution - executes fOnExecute
    procedure Execute; override;
  public
    constructor Create; overload; //creates the thread suspeded
  end;

   {<B>TJwSecureBaseClass</B> is a base class and provides abstract methods to get and set
    security infomration on objects (like files, mutexes, ...).

    To use security information with general objects use the class TJwSecureGeneralObject.

    }
  TJwSecureBaseClass = class
  protected
    fHandle:       THandle;
    fAccessMask:   TJwAccessMask;
    fAutoResetACL: boolean;


    {<b>PrepareAccessCheckTokenAssignment</b> is a utility function for
      any AccessCheck method. It makes sure that the given ClientToken
      is a thread token, or - if it is nil - returns the current thread or process
      as a thread token.

      Warning: Don't close hToken! Instead Free parameter Token.

    }
    class procedure PrepareAccessCheckTokenAssignment(
      const ClientToken: TJwSecurityToken;
      out Token : TJwSecurityToken;
      out hToken : TJwTokenHandle);

    {<b>PrepareAccessCheckSecurityDescriptor</b> is a utility function for
      any AccessCheck method. It checks the given parameter SecurityDescriptor
      for validity.

      raises
        EJwsclNILParameterException If parameter SecurityDescriptor is nil.
        EJwsclInvalidGroupSIDException If primary group of the SD is not set.
        EJwsclInvalidOwnerSIDException If owner of the SD is not set.
    }
    class procedure PrepareAccessCheckSecurityDescriptor(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const aClassName : TJwString;
      const Method : TJwString);

    {<b>PrepareAccessCheckGenericACE</b> is a utility function for
      any AccessCheck method. It converts generic access rights in
      the DACL of SecurityDescriptor to specific access rights.

      TempSD: New SD with converted rights. Call Free when no more necessary.
    }
    class procedure PrepareAccessCheckGenericACE(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const GenericMapping : TJwSecurityGenericMappingClass;
      out bTempSD : Boolean;
      out TempSD  : TJwSecurityDescriptor);

     {<B>SetSecurityInfo</B> sets the security information of a object given by a handle.

     <B>SetSecurityInfo</B> cannot change inheritance protection flow. Instead use SetSecurityInfo
      using TJwSecurityDescriptor.

       @param aSecurityInfo receives the security flags that describes which security data to be set.
              It will be automatically extended if the following parameters are not nil.
      }
    class procedure SetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const anOwner: TJwSecurityId;
      const anGroup: TJwSecurityId;
      const aDACL: TJwDAccessControlList;
      const aSACL: TJwSAccessControlList);
      overload; virtual;

    {
    <B>SetNamedSecurityInfo</B> cannot change inheritance protection flow. Instead use SetSecurityInfo
      using TJwSecurityDescriptor.
    }
    class procedure SetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet;
      const anOwner: TJwSecurityId;
      const anGroup: TJwSecurityId;
      const aDACL: TJwDAccessControlList;
      const aSACL: TJwSAccessControlList);
      overload; virtual;

    class procedure GetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      out anOwner: TJwSecurityId;
      out anGroup: TJwSecurityId;
      out aDACL: TJwDAccessControlList;
      out aSACL: TJwSAccessControlList);
      overload; virtual;

    class function GetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet)
      : {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
      TJwSecurityDescriptor; overload; virtual;


    class function GetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet)
      : {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
      TJwSecurityDescriptor; overload; virtual;

    class procedure GetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      out anOwner: TJwSecurityId;
      out anGroup: TJwSecurityId;
      out aDACL: TJwDAccessControlList;
      out aSACL: TJwSAccessControlList);
      overload; virtual;


    class procedure SetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const aSecurityDescriptor:
      TJwSecurityDescriptor);
      overload; virtual;
    class procedure SetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const aSecurityDescriptor:
      TJwSecurityDescriptor);
      overload; virtual;


    function AccessCheck(DesiredAccess: TJwAccessMask = Cardinal(-1);
      const ClientToken: TJwSecurityToken = nil)
      : boolean; overload; virtual; abstract;

    class procedure AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean);
      overload; virtual;

    class function AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass) : Boolean;
      overload; virtual;

    class procedure AccessCheckAndAuditAlarm(
      const SubsystemName: TJwString;
      const HandleID: Pointer;
      const ObjectTypeName,
      ObjectName: TJwString;
      const SecurityDescriptor:
      TJwSecurityDescriptor;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      const ObjectCreation: boolean;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean;
      out pfGenerateOnClose: boolean);
      overload; virtual;

    class procedure AccessCheckByType(
      const SecurityDescriptor : TJwSecurityDescriptor;
      const PrincipalSelfSid : TJwSecurityID;
      const ClientToken : TJwSecurityToken;
      const DesiredAccess : TJwAccessMask;
      var ObjectTypeArray : TJwObjectTypeArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean
    ); virtual;

    class procedure AccessCheckByTypeResultList(
      const SecurityDescriptor : TJwSecurityDescriptor;
      const PrincipalSelfSid : TJwSecurityID;
      const ClientToken : TJwSecurityToken;
      const DesiredAccess : TJwAccessMask;
      var ObjectTypeArray : TJwObjectTypeArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMaskArray;
      out AccessStatus: TJwCardinalArray
    ); virtual;


    {
    The following values are ignored in SD_entries:

        # siprotectedDaclSecurityInformation
        # siUnprotectedDaclSecurityInformation
        # siprotectedSaclSecurityInformation
        # siUnprotectedSaclSecurityInformation
        # siLabelSecurityInformation. The integrity label is a simple ACE in then
          system ACL (SACL). Use SetMandatoryLabel instead

       Instead use TJwSecurityDescriptor.InheritanceDACLProtection or
         TJwSecurityDescriptor.InheritanceSACLProtection to control inheritance.
    }
    procedure SetSecurityDescriptor(const SD: TJwSecurityDescriptor;
      const SD_entries: TJwSecurityInformationFlagSet); virtual;

    function GetSecurityDescriptor(
      const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
      virtual; {WARNING: EJwsclNotImplementedException : abstract in code!!}

    class procedure TreeResetNamedSecurityInfo(pObjectName: TJwString;
      const aObjectType:
      TSeObjectType;//SE_OBJECT_TYPE ObjectType,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL:
      TJwDAccessControlList;
      const SACL:
      TJwSAccessControlList;
      const bKeepExplicit: boolean;
      const fnProgress:
      TJwProgInvokeSetting;
      const FNProgressMethod: TJwFnProgressMethod;
      const FNProgressProcedure:
      TJwFnProgressProcedure;
      const ProgressUserData: Pointer); virtual;




    class function GetInheritanceSource(
      const pObjectName: TJwString;
    //LPTSTR pObjectName,
      const aObjectType: TSeObjectType;
    //SE_OBJECT_TYPE ObjectType,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
      const bContainer: boolean;
    //BOOL Container,
      const GUIDs: TJwGuidArray;
    //GUID** pObjectClassGuids,
      const ACL: TJwSecurityAccessControlList;
    //PACL pAcl,
      const Mapping:
      TJwSecurityGenericMappingClass;
    //PGENERIC_MAPPING pGenericMapping,
      const FN_OBJECT_MGR_FUNCTS: Pointer =
      nil): TJwInheritedFromArray;
    {PINHERITED_FROM pInheritArray} virtual;

      {Name takes the ownership of an object given by a handle. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param Handle Defines a handle to the file or folder which owner is to be changed
       @param aObjectType Defines the type of object
       @param SID defines the new owner. If nil the current user is used.

       raises
 EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const Handle: THandle;
      const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
      overload; virtual;

       {Name takes the ownership of an object given by a handle. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param PathName Defines the file or folder which owner is to be changed
       @param aObjectType Defines the type of object
       @param SID defines the new owner. If nil the current user is used.

       raises
 EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const PathName: TJwString;
      const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
      overload; virtual;

      {<B>GetOwnerShipMembers</B> creates a list of sids that can be used to change the owner of a secured object.
       The first member is always the token owner.
       These Sids can be used to change the owner. Without restore privilege the owner
       can only changed to one of the returned SIDs. However the function ignores restore privileges.

       The token is opened with the TOKEN_READ access right.


       @return Returns a list of sids. The list must be freed by the caller. 
       raises
 EJwsclSecurityException:  An exception can be raised if a call to one of the following items failed:

                 # CreateTokenEffective
                 # GetTokenGroups
                 # GetTokenOwner
                 # TJwSecurityId.Create 
                 
      }

    class function GetOwnerShipMembers: TJwSecurityIdList; virtual;

    class function ConvertMaximumAllowed(
       const SecurityDescriptor: TJwSecurityDescriptor;
       const ClientToken: TJwSecurityToken;
       const GenericMapping:
        TJwSecurityGenericMappingClass
     ) : TAccessMask; virtual;

  public
    // This function converts generic rights to specifc ones
    // It can convert several generic rights.
    class function ConvertAccessMask(const GenericMapping: TJwSecurityGenericMappingClass;
       InputMask : Cardinal) : Cardinal;

    //this procedure replaces GENERIC access masks
    //in the DACL with specific mask using mapping GenericMapping
    class procedure ReplaceGenericRightsInDACL(
      const GenericMapping: TJwSecurityGenericMappingClass;
      const SD: TJwSecurityDescriptor);
  protected
    fDAccessControlList: TJwDAccessControlList;
    fSAccessControlList: TJwSAccessControlList;
    fOwner: TJwSecurityId;
    fGroup: TJwSecurityId;


    function GetDACL: TJwDAccessControlList; virtual; abstract;
    function GetSACL: TJwSAccessControlList; virtual; abstract;

    function GetOwner: TJwSecurityId; virtual; abstract;
    function GetGroup: TJwSecurityId; virtual; abstract;

    function GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry; virtual; abstract;
    procedure SetMandatoryLabel(const MandatoryLabel :
      TJwSystemMandatoryAccessControlEntry); virtual; abstract;

    {@param Protection is not used. It can be used by override methods.  }
    procedure SetDACL(const list: TJwDAccessControlList;
     const Protection : TJwACLProtectionState = apNone); virtual; abstract;
    {@param Protection is not used. It can be used by override methods.  }
    procedure SetSACL(const list: TJwSAccessControlList;
     const Protection : TJwACLProtectionState = apNone); virtual; abstract;

    procedure SetOwner(const ID: TJwSecurityId); virtual; abstract;
    procedure SetGroup(const ID: TJwSecurityId); virtual; abstract;

    function GetTempOwner: TJwSecurityId; virtual; abstract;
    function GetTempGroup: TJwSecurityId; virtual; abstract;

    function GetTempDACL: TJwDAccessControlList; virtual; abstract;
    function GetTempSACL: TJwSAccessControlList; virtual; abstract;

    procedure ResetTemp(resetState: TJwTempResetEnumSet = JwResetEnumSetAll);
      virtual;


    property DACL: TJwDAccessControlList Read GetTempDACL;
    property SACL: TJwSAccessControlList Read GetTempSACL;

    property Owner: TJwSecurityId Read GetTempOwner Write SetOwner;
    property Group: TJwSecurityId Read GetTempGroup Write SetGroup;

    property AccessMask: TJwAccessMask Read fAccessMask Write fAccessMask;
    property Handle: THandle Read fHandle;
    property AutoResetACL: boolean Read fAutoResetACL Write fAutoResetACL;


    constructor Create;
  public
    destructor Destroy; override;

    function HasValidHandle: boolean;
  end;

   {<B>TJwSecureGeneralObject</B> gives access to general security methods to set or get
   security relevant data from objects.

   Do not make instance of this class but use instead the static methods.
   }
  TJwSecureGeneralObject = class(TJwSecureBaseClass)
  public
      {<B>SetSecurityInfo</B> sets security information of a named object
       <B>SetSecurityInfo</B> cannot change inheritance protection flow. Instead use SetSecurityInfo
          using TJwSecurityDescriptor.

       @param anObjectName defines the name of a named object which security information is to be changed
       @param aObjectType defines the named object type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the named object
       @param anOwner if aSecurityInfo contains the flag siOwnerSecurityInformation, this parameter will receive the OwnerSID to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param anGroup if aSecurityInfo contains the flag siGroupSecurityInformation, this parameter will receive the GroupSID to be changed in the destination.
                         If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param aDACL if aSecurityInfo contains the flag siDaclSecurityInformation, this parameter will receive the DACL to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param aSACL if aSecurityInfo contains the flag siSaclSecurityInformation, this parameter will receive the DACL to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.

       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.

       }
    class procedure SetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const anOwner: TJwSecurityId;
      const anGroup: TJwSecurityId;
      const aDACL: TJwDAccessControlList;
      const aSACL: TJwSAccessControlList);
      overload; override;

      {<B>SetNamedSecurityInfo</B> sets security information of a handle.
       @param anObjectName defines the name of a handle which security information is to be changed
       @param aObjectType defines the handle type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the handle
                For files and folders the object inheritance flow can be stopped by using the flag
                  siProtectedDaclSecurityInformation. In this case all inherited ACEs are removed from the DACL leaving the explicit ACEs only.
                The flag siUnprotectedDaclSecurityInformation allows the inheritance flow to the file object.
                Thus this flags reverts the flag siProtectedDaclSecurityInformation.

                Both flags must be used with the combination of siDaclSecurityInformation and a DACL that is not nil. An empty DACL
                is necessary to change this flag.
                The same function is done by RemoveInheritanceFlow and RestoreInheritanceFlow. 


       @param anOwner if aSecurityInfo contains the flag siOwnerSecurityInformation, this parameter will receive the OwnerSID to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param anGroup if aSecurityInfo contains the flag siGroupSecurityInformation, this parameter will receive the GroupSID to be changed in the destination.
                         If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param aDACL if aSecurityInfo contains the flag siDaclSecurityInformation, this parameter will receive the DACL to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.
       @param aSACL if aSecurityInfo contains the flag siSaclSecurityInformation, this parameter will receive the DACL to be changed in the destination.
                        If this parameter is not nil, the flag is automatically added and the parameter is used.

       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.

       }
    class procedure SetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet;
      const anOwner: TJwSecurityId;
      const anGroup: TJwSecurityId;
      const aDACL: TJwDAccessControlList;
      const aSACL: TJwSAccessControlList);
      overload; override;

      {<B>GetSecurityInfo</B> retrieves security information from a handle.
       @param anObjectName defines the name of a handle which security information is to be retrieved.
       @param aObjectType defines the handle type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the handle
       @param anOwner if aSecurityInfo contains the flag siOwnerSecurityInformation, this parameter will contain the OwnerSID instance.
                        The caller musst free this instance.
       @param anGroup if aSecurityInfo contains the flag siGroupSecurityInformation, this parameter will contain the GroupSID instance.
                        The caller musst free this instance.
       @param aDACL if aSecurityInfo contains the flag siDaclSecurityInformation, this parameter will contain the DACL instance.
                        The caller musst free this instance.
       @param aSACL if aSecurityInfo contains the flag siSaclSecurityInformation, this parameter will contain the DACL instance.
                        The caller musst free this instance.

       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.

       }
    class procedure GetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      out anOwner: TJwSecurityId;
      out anGroup: TJwSecurityId;
      out aDACL: TJwDAccessControlList;
      out aSACL: TJwSAccessControlList);
      overload; override;

      {<B>GetSecurityInfo</B> returns the security descriptor from a handle.
       @param anObjectName defines the name of a handle which security information is to be retrieved
       @param aObjectType defines the type of the handle
       @param aSecurityInfo defines which security information shall be retrieved and stored into the security descriptor.
                If the caller has not enough rights the function fails. So not all security information can be retrieved. E.g. the SACL can only
                 retrieved with special privileges.
       @return Returns the security descriptor with the requested information.
       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.

      }
    class function GetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet)
      : {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
      TJwSecurityDescriptor; overload; override;


      {<B>GetNamedSecurityInfo</B> returns the security descriptor from a named object.
       @param anObjectName defines the name of an object which security information is to be retrieved
       @param aObjectType defines the type of the object
       @param aSecurityInfo defines which security information shall be retrieved and stored into the security descriptor.
                If the caller has not enough rights the function fails. So not all security information can be retrieved. E.g. the SACL can only
                 retrieved with special privileges.
       @return Returns the security descriptor with the requested information.
       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.
      }
    class function GetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo:
      TJwSecurityInformationFlagSet)
      : {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
      TJwSecurityDescriptor; overload; override;

      {<B>GetNamedSecurityInfo</B> retrieves security information from a named object.
       @param anObjectName defines the name of an object which security information is to be retrieved
       @param aObjectType defines the object type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the handle
       @param anOwner if aSecurityInfo contains the flag siOwnerSecurityInformation, this parameter will contain the OwnerSID instance.
                        The caller musst free this instance.
       @param anGroup if aSecurityInfo contains the flag siGroupSecurityInformation, this parameter will contain the GroupSID instance.
                        The caller musst free this instance.
       @param aDACL if aSecurityInfo contains the flag siDaclSecurityInformation, this parameter will contain the DACL instance.
                        The caller musst free this instance.
       @param aSACL if aSecurityInfo contains the flag siSaclSecurityInformation, this parameter will contain the DACL instance.
                        The caller musst free this instance.

       raises
 EJwsclWinCallFailedException:  will be raised if a call to the winapi function failed.

       }
    class procedure GetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      out anOwner: TJwSecurityId;
      out anGroup: TJwSecurityId;
      out aDACL: TJwDAccessControlList;
      out aSACL: TJwSAccessControlList);
      overload; override;

      {<B>SetNamedSecurityInfo</B> sets the security information of a named object using a security descriptor.
       Only the parts of the SD defined in aSecurityInfo are used.
       @param anObjectName defines the name of an object which security information is to be changed
       @param aObjectType defines the handle type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the handle
       @param aSecurityDescriptor defines the security descriptor to be used

       raises
 EJwsclInvalidParameterException:  is raised if parameter aSecurityDescriptor is nil.
       }
    class procedure SetNamedSecurityInfo(const anObjectName: TJwString;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const aSecurityDescriptor:
      TJwSecurityDescriptor);
      overload; override;

      {<B>SetSecurityInfo</B> sets the security information of a handle using a security descriptor.
       Only the parts of the SD defined in aSecurityInfo are used.
       <B>SetSecurityInfo</B> cannot change inheritance protection flow. Instead use SetSecurityInfo
        using TJwSecurityDescriptor.

       @param aHandle defines the handle which security information is to be changed
       @param aObjectType defines the handle type
       @param aSecurityInfo defines which security information is retrieved from the SD and used in the handle
       @param aSecurityDescriptor defines the security descriptor to be used

       raises
 EJwsclInvalidParameterException:  is raised if parameter aSecurityDescriptor is nil.
       }
    class procedure SetSecurityInfo(const aHandle: THandle;
      const aObjectType: TSeObjectType;
      aSecurityInfo: TJwSecurityInformationFlagSet;
      const aSecurityDescriptor:
      TJwSecurityDescriptor);
      overload; override;

      {<B>TreeResetNamedSecurityInfo</B> sets the securiy information of a folder and all its children.
        See MSDN for more information http://msdn2.microsoft.com/en-us/library/aa965849.aspx.
        This function needs Windows XP or higher.

        This method can restore the security information for a locked out user without using SeBackupPrivilege privilege.
        It uses a highly privileged system accout to do this.

        Both callback methods can be used at the same time. If both are not nil, they are called in the following order :
         FNProgressMethod, FNProgressProcedure. Exceptions are caught in these calls.

        @param aSecurityInfo defines which security information to be set. (Owner...SACL). If a flag is not set, but the
                parameter Owner...SACL is not nil, it is also set. 
        @param FNProgressMethod defines a object method to be called if a file or folder is changed. Can be nil. See TJwFnProgressMethod  
        @param FNProgressProcedure defines a procedure to be called if a file or folder is changed. Can be nil. See TJwFnProgressProcedure  
        @param ProgressUserData defines user data to be used as parameter ProgressUserData in the callback methods.

        raises
           EJwsclWinCallFailedException:  is raised if the winapi call failed.
           EJwsclUnsupportedWindowsVersionException: This is exception is raised if the Windows system does not support this call.
        }
    class procedure TreeResetNamedSecurityInfo(pObjectName: TJwString;
      const aObjectType:
      TSeObjectType;//SE_OBJECT_TYPE ObjectType,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL:
      TJwDAccessControlList;
      const SACL:
      TJwSAccessControlList;
      const bKeepExplicit: boolean;
      const fnProgress:
      TJwProgInvokeSetting;
      const FNProgressMethod: TJwFnProgressMethod;
      const FNProgressProcedure:
      TJwFnProgressProcedure;
      const ProgressUserData: Pointer); override;



      {<B>GetInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       See MSDN http://msdn2.microsoft.com/en-us/library/aa446640.aspx.

       @return The return value is an array of TJwInheritedFromRecord that maps the winapi struct
        INHERITED_FROM to a Delphi type. The function FreeInheritedFromArray must not be called therefor,
        instead use SetLength(result,0);
        The member SID is not used here.


       This function does not work in all version of Windows XP. If so the
       exception EJwsclInheritanceSourceNotSupportedException is raised.

       }
    class function GetInheritanceSource(
      const pObjectName: TJwString;
    //LPTSTR pObjectName,
      const aObjectType: TSeObjectType;
    //SE_OBJECT_TYPE ObjectType,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
      const bContainer: boolean;
    //BOOL Container,
      const GUIDs: TJwGuidArray;
    //GUID** pObjectClassGuids,
      const ACL: TJwSecurityAccessControlList;
    //PACL pAcl,
      const Mapping:
      TJwSecurityGenericMappingClass;
    //PGENERIC_MAPPING pGenericMapping,
      const FN_OBJECT_MGR_FUNCTS: Pointer =
      nil): TJwInheritedFromArray;
    {PINHERITED_FROM pInheritArray} override;

      { <b>AccessCheck</b> checks the access to a security descriptor of a secure
        object. See AccessCheck in MSDN for more information
        Parameters
        SecurityDescriptor :  Contains the security descriptor that is used to check for
                              access.
        ClientToken :         A token that is used to get the SID and privileges which
                              are used to check against the security descriptor. The
                              parameter can be nil to use the current thread or process
                              token.<p /><p />In contrast to the original AccessCheck API
                              function, this method automatically adjusts the token type
                              to "impersonation" if the given token is a primary one.
                              This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                              (1309). If ClientToken is not nil the method makes a copy
                              of ClientToken and converts it to a thread token. If
                              ClientToken is nil, it retrieves the current thread or
                              primary token (if no thread token is available) and, in the
                              latter case, converts it to a thread token. Furthermore
                              this method does not impersonate any token and thus leaves
                              an already existing thread token intact.<p />
        DesiredAccess :       Defines the desired access to the object.<p /><b>New</b><p />Although
                              the MSDN AccessCheck forbids generic rights (like
                              GENERIC_ALL) in this Parameter. The method AccessCheck will
                              replace all generic rights with specific rights using the
                              mapping defined by parameter GenericMapping. However the
                              original SecurityDescriptor will remain the same. <b>Warning</b>
                              Some generic access rights may overlap. This can lead to
                              access denied.<p /><b>Example</b><p />DACL contains a
                              positive ACE with GENERIC_WRITE and a negative ACE with
                              GENERIC_READ A call to AccessCheck with DesiredAccess set
                              to GENERIC_WRITE and GenericMapping set to
                              TJwSecurityFileMapping (using FileGenericMapping) will fail
                              because GENERIC_WRITE and GENERIC_READ are resolve to
                              FILE_GENERIC_WRITE and FILE_GENERIC_READ which both contain
                              SYNCHRONIZE ($100000) and READ_CONTROL ($2000).<p />
        GenericMapping :      Receives a class type of the class
                              TJwSecurityGenericMapping or one of her derived classes. If
                              the generic class TJwSecurityGenericMapping is used, all
                              generic access rights are mapped to standard access rights
                              (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                              access rights in parameter DesiredAccess that are mapped by
                              the given TJwSecurityGenericMappingClass class; otherwise
                              AccessChec will fail with EJwsclWinCallFailedException.
                              E.g. TJwSecurityGenericMappingClass can be used with
                              DesiredAccess set to STANDARD_RIGHTS_ALL.<p /><p /><b>New</b><p />All
                              access entriey (ACEs) in the security descriptor DACL are
                              scanned for GENERIC access rights (like GENERIC_ALL) and
                              automatically converted to specific rights using parameter
                              GenericMapping. However the original SecurityDescriptor
                              will remain the same. This will not happen, if nil is
                              supplied to this parameter. Make sure there are no generic
                              rights in the ACL or DesiredAccess parameter.<p />
        PrivilegeSet :        Receives the privileges that are used for access check. If
                              none are used, this output will be nil. The caller is
                              responsible for destroying the object!
        GrantedAccess :       Receives an access mask that indicates which rights were
                              granted.
        AccessStatus :        Receives the result of the access check. True if access is
                              granted, otherwise false.
        Exceptions
        EJwsclInvalidParameterException :  will be raised if parameter SecurityDescriptor
                                           is nil;
        EJwsclWinCallFailedException :     will be raised if the call to AccessCheck
                                           failed.
        EJwsclInvalidOwnerSIDException :   will be raised if the owner of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of owner to AccessCheck call.
        EJwsclInvalidGroupSIDException :   will be raised if the group of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of group to AccessCheck call.                  }
    class procedure AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean);
      overload; override;


        { <b>AccessCheck</b> checks the access to a security descriptor of a secure
          object. See AccessCheck in MSDN for more information
          Parameters
          SecurityDescriptor :  Contains the security descriptor that is used to check for
                                access.
          ClientToken :         A token that is used to get the SID and privileges which
                                are used to check against the security descriptor. The
                                parameter can be nil to use the current thread or process
                                token.<p /><p />In contrast to the original AccessCheck API
                                function, this method automatically adjusts the token type
                                to "impersonation" if the given token is a primary one.
                                This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                                (1309). If ClientToken is not nil the method makes a copy
                                of ClientToken and converts it to a thread token. If
                                ClientToken is nil, it retrieves the current thread or
                                primary token (if no thread token is available) and, in the
                                latter case, converts it to a thread token. Furthermore
                                this method does not impersonate any token and thus leaves
                                an already existing thread token intact.<p />
          DesiredAccess :       Defines the desired access to the object.<p /><b>New</b><p />Although
                                the MSDN AccessCheck forbids generic rights (like
                                GENERIC_ALL) in this Parameter. The method AccessCheck will
                                replace all generic rights with specific rights using the
                                mapping defined by parameter GenericMapping. However the
                                original SecurityDescriptor will remain the same. <b>Warning</b>
                                Some generic access rights may overlap. This can lead to
                                access denied.<p /><b>Example</b><p />DACL contains a
                                positive ACE with GENERIC_WRITE and a negative ACE with
                                GENERIC_READ A call to AccessCheck with DesiredAccess set
                                to GENERIC_WRITE and GenericMapping set to
                                TJwSecurityFileMapping (using FileGenericMapping) will fail
                                because GENERIC_WRITE and GENERIC_READ are resolve to
                                FILE_GENERIC_WRITE and FILE_GENERIC_READ which both contain
                                SYNCHRONIZE ($100000) and READ_CONTROL ($2000).<p />
          GenericMapping :      Receives a class type of the class
                                TJwSecurityGenericMapping or one of her derived classes. If
                                the generic class TJwSecurityGenericMapping is used, all
                                generic access rights are mapped to standard access rights
                                (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                                access rights in parameter DesiredAccess that are mapped by
                                the given TJwSecurityGenericMappingClass class; otherwise
                                AccessChec will fail with EJwsclWinCallFailedException.
                                E.g. TJwSecurityGenericMappingClass can be used with
                                DesiredAccess set to STANDARD_RIGHTS_ALL.<p /><p /><b>New</b><p />All
                                access entriey (ACEs) in the security descriptor DACL are
                                scanned for GENERIC access rights (like GENERIC_ALL) and
                                automatically converted to specific rights using parameter
                                GenericMapping. However the original SecurityDescriptor
                                will remain the same. This will not happen, if nil is
                                supplied to this parameter. Make sure there are no generic
                                rights in the ACL or DesiredAccess parameter.<p />
          Exceptions
          EJwsclInvalidParameterException :  will be raised if parameter SecurityDescriptor
                                             is nil;
          EJwsclWinCallFailedException :     will be raised if the call to AccessCheck
                                             failed.
          EJwsclInvalidOwnerSIDException :   will be raised if the owner of the security
                                             descriptor is nil. Use JwNullSID to remove
                                             influence of owner to AccessCheck call.
          EJwsclInvalidGroupSIDException :   will be raised if the group of the security
                                             descriptor is nil. Use JwNullSID to remove
                                             influence of group to AccessCheck call.                  }
    class function AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass) : Boolean;
      overload; override;

    {<B>AccessCheckAndAuditAlarm</B> is not implemented yet}
    class procedure AccessCheckAndAuditAlarm(
      const SubsystemName: TJwString;
      const HandleID: Pointer;
      const ObjectTypeName,
      ObjectName: TJwString;
      const SecurityDescriptor:
      TJwSecurityDescriptor;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      const ObjectCreation: boolean;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean;
      out pfGenerateOnClose: boolean);
      overload; override;



    { <b>AccessCheckByType</b> does an access check of an object with properties.
      Parameters
      SecurityDescriptor :  defines the primary SD which is used to perfom access
                            checking. The owner and group must not be nil; otherwise
                            the call will fail<p />
      PrincipalSelfSid :    defines a SID that is used to replace a principle self sid
                            found in an inherited ACE. A principle self SID (S\-1\-5\-10)
                            in a ACE will be replaced by this property SID. Can be nil
                            if not used.
      ClientToken :         A token that is used to get the SID and privileges which
                            are used to check against the security descriptor. The
                            parameter can be nil to use the current thread or process
                            token.<p />In contrast to the original AccessCheck API
                            function, this method automatically adjusts the token type
                            to "impersonation" if the given token is a primary one.
                            This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                            (1309). If ClientToken is not nil it makes a copy of
                            ClientToken and converts it to a thread token. If
                            ClientToken is nil, it retrieves the current thread or
                            primary token (if no thread token is available) and, in the
                            latter case, converts it to a thread token. Furthermore
                            this method does not impersonate any token and thus leaves
                            an already existing thread token intact.<p />
      DesiredAccess :       define the desired access to the object.<p /><b>New</b><p />Although
                            the MSDN AccessCheck forbids generic rights (like
                            GENERIC_ALL) in this Parameter. The method AccessCheck will
                            replace all generic rights with specific rights using the
                            mapping defined by parameter GenericMapping. However the
                            original SecurityDescriptor will remain the same.<p /><b>Warning</b><p />Some
                            generic access rights may overlap. This can lead to access
                            denied.<p /><b>Example</b><p />DACL contains a positive ACE
                            with GENERIC_WRITE and a negative ACE with GENERIC_READ<p />A
                            call to AccessCheck with DesiredAccess set to GENERIC_WRITE
                            and GenericMapping set to TJwSecurityFileMapping (using
                            FileGenericMapping) will fail because GENERIC_WRITE and
                            GENERIC_READ are resolve to FILE_GENERIC_WRITE and
                            FILE_GENERIC_READ which both contain SYNCHRONIZE ($100000)
                            and READ_CONTROL ($2000).<p />
      ObjectTypeArray :     defines an array of object properties. The Level of the
                            objects must comply to some rules\:
                            <code lang="delphi">
                                                         Array[i].Level = a_i
                                           { a_i +1        | a_i \- a_(i\-1) = 1 AND a_i \< 4
                                   a_i+1 = { a_i \- t       | a_i \- t AND t \>= 0
                                           { ERROR_INVALID_PARAMETER | else
                            
                                   sequence start\: a_0 = 0
                            </code>
                            \See also <link http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx, MSDN>.<p />
      GenericMapping :      Receives a class type of the class
                            TJwSecurityGenericMapping or one of her derived classes. If
                            the generic class TJwSecurityGenericMapping is used, all
                            generic access rights are mapped to standard access rights
                            (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                            access rights in parameter DesiredAccess that are mapped by
                            the given TJwSecurityGenericMappingClass class; otherwise
                            AccessChec will fail with EJwsclWinCallFailedException.
                            E.g. TJwSecurityGenericMappingClass can be used with
                            DesiredAccess set to STANDARD_RIGHTS_ALL. <b>New\:</b> All
                            access entriey (ACEs) in the security descriptor DACL are
                            scanned for GENERIC access rights (like GENERIC_ALL) and
                            automatically converted to specific rights using parameter
                            GenericMapping. However the original SecurityDescriptor
                            will remain the same. This will not happen, if nil is
                            supplied to this parameter. Make sure there are no generic
                            rights in the ACL or DesiredAccess parameter.<p />
      PrivilegeSet :        receives the privileges that are used for access check. If
                            none are used, this output will be nil. The caller is
                            responsible for destroying the object!
      GrantedAccess :       receives an access mask that indicates which rights were
                            granted.
      AccessStatus :        receives the result of the access check. True if access is
                            granted, otherwise false.
      Exceptions
      EJwsclInvalidParameterException :    will be raised if parameter
                                           SecurityDescriptor is nil;
      EJwsclWinCallFailedException :       will be raised if the call to AccessCheck
                                           failed. will be raised if parameter
                                           ObjectTypeArray is nil. if a call to
                                           AccessCheckByType failed.
      EJwsclInvalidObjectArrayException :  will be raised if parameter ObjectTypeArray
                                           contains invalid members. See this parameter
                                           for more information.
      EJwsclInvalidOwnerSIDException :     will be raised if the owner of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of owner to AccessCheck call.
      EJwsclInvalidGroupSIDException :     will be raised if the group of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of group to AccessCheck call.
      EJwsclNILParameterException :        will be raised if parameter Request,
                                           SecurityDescriptor or
                                           Request.PrincipalSelfSid is nil                                             }

    class procedure AccessCheckByType(
      const SecurityDescriptor : TJwSecurityDescriptor;
      const PrincipalSelfSid : TJwSecurityID;
      const ClientToken : TJwSecurityToken;
      const DesiredAccess : TJwAccessMask;
      var ObjectTypeArray : TJwObjectTypeArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean
    );  override;

    { <b>AccessCheckByTypeResultList</b> does an access check of an object with
      properties.
      Parameters
      SecurityDescriptor :  defines the primary SD which is used to perfom access
                            checking. The owner and group must not be nil; otherwise
                            the call will fail.<p />
      PrincipalSelfSid :    defines a SID that is used to replace a principle self sid
                            found in an inherited ACE. A principle self SID (S\-1\-5\-10)
                            in a ACE will be replaced by this property SID. Can be nil
                            if not used.
      ClientToken :         A token that is used to get the SID and privileges which
                            are used to check against the security descriptor. The
                            parameter can be nil to use the current thread or process
                            token.<p /><p />In contrast to the original AccessCheck API
                            function, this method automatically adjusts the token type
                            to "impersonation" if the given token is a primary one.
                            This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                            (1309). If ClientToken is not nil it makes a copy of
                            ClientToken and converts it to a thread token. If
                            ClientToken is nil, it retrieves the current thread or
                            primary token (if no thread token is available) and, in the
                            latter case, converts it to a thread token. Furthermore
                            this method does not impersonate any token and thus leaves
                            an already existing thread token intact.<p />
      DesiredAccess :       Defines the desired access to the object.<p /><b>New</b><p />Although
                            the MSDN AccessCheck forbids generic rights (like
                            GENERIC_ALL) in this Parameter. The method AccessCheck will
                            replace all generic rights with specific rights using the
                            mapping defined by parameter GenericMapping. However the
                            original SecurityDescriptor will remain the same. <b>Warning</b>
                            Some generic access rights may overlap. This can lead to
                            access denied.<p /><b>Example</b><p />DACL contains a
                            positive ACE with GENERIC_WRITE and a negative ACE with
                            GENERIC_READ<p />A call to AccessCheck with DesiredAccess
                            set to GENERIC_WRITE and GenericMapping set to
                            TJwSecurityFileMapping (using FileGenericMapping) will fail
                            because GENERIC_WRITE and GENERIC_READ are resolve to
                            FILE_GENERIC_WRITE and FILE_GENERIC_READ which both contain
                            SYNCHRONIZE ($100000) and READ_CONTROL ($2000).<p />
      ObjectTypeArray :     defines an array of object properties. The Level of the
                            objects must comply to some rules.
                            <code lang="delphi">
                                                                         Array[i].Level = a_i
                                    { a_i +1        | a_i \- a_(i\-1) = 1 AND a_i \< 4
                            a_i+1 = { a_i \- t       | a_i \- t AND t \>= 0
                                    { ERROR_INVALID_PARAMETER | else
                            
                            sequence start\: a_0 = 0
                            </code>
                            \See also <link http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx, MSDN>.<p />
      GenericMapping :      Receives a class type of the class
                            TJwSecurityGenericMapping or one of her derived classes. If
                            the generic class TJwSecurityGenericMapping is used, all
                            generic access rights are mapped to standard access rights
                            (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                            access rights in parameter DesiredAccess that are mapped by
                            the given TJwSecurityGenericMappingClass class; otherwise
                            AccessChec will fail with EJwsclWinCallFailedException.
                            E.g. TJwSecurityGenericMappingClass can be used with
                            DesiredAccess set to STANDARD_RIGHTS_ALL.<p /><b>New</b><p />All
                            access entriey (ACEs) in the security descriptor DACL are
                            scanned for GENERIC access rights (like GENERIC_ALL) and
                            automatically converted to specific rights using parameter
                            GenericMapping. However the original SecurityDescriptor
                            will remain the same. This will not happen, if nil is
                            supplied to this parameter. Make sure there are no generic
                            rights in the ACL or DesiredAccess parameter.<p />
      PrivilegeSet :        receives the privileges that are used for access check. If
                            none are used, this output will be nil. The caller is
                            responsible for destroying the object!
      GrantedAccess :       receives an array of access mask that indicates which
                            rights were granted. The count of elements is the same as
                            length of ObjectTypeArray.
      AccessStatus :        receives an array of return codes which defines why a check
                            failed. The count of elements is the same as length of
                            ObjectTypeArray.
      Exceptions
      EJwsclInvalidParameterException :    will be raised if parameter
                                           SecurityDescriptor is nil.<p />
      EJwsclWinCallFailedException :       will be raised if the call to AccessCheck
                                           failed. will be raised if parameter
                                           ObjectTypeArray is nil. if a call to
                                           AccessCheckByTypeResultList failed.
      EJwsclInvalidObjectArrayException :  will be raised if parameter ObjectTypeArray
                                           contains invalid members. See this parameter
                                           for more information.
      EJwsclInvalidOwnerSIDException :     will be raised if the owner of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of owner to AccessCheck call.
      EJwsclInvalidGroupSIDException :     will be raised if the group of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of group to AccessCheck call.<p />
      EJwsclNILParameterException :        will be raised if parameter Request,
                                           SecurityDescriptor or
                                           Request.PrincipalSelfSid is nil                                             }
    class procedure AccessCheckByTypeResultList(
      const SecurityDescriptor : TJwSecurityDescriptor;
      const PrincipalSelfSid : TJwSecurityID;
      const ClientToken : TJwSecurityToken;
      const DesiredAccess : TJwAccessMask;
      var ObjectTypeArray : TJwObjectTypeArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMaskArray;
      out AccessStatus: TJwCardinalArray
    );  override;

      {Name takes the ownership of an object given by a handle. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param Handle Defines a handle to the file or folder which owner is to be changed
       @param aObjectType Defines the type of object
       @param SID defines the new owner. If nil the current user is used. 

       raises
 EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const Handle: THandle;
      const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
      reintroduce; overload; override;

       {Name takes the ownership of an object given by a handle. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param ObjectName Defines the object which owner is to be changed
       @param aObjectType Defines the type of object
       @param SID defines the new owner. If nil the current user is used. 

       raises
 EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const ObjectName: TJwString;
      const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
      reintroduce; overload; override;


       {<B>GetOwnerShipMembers</B> creates a list of sids that can be used to change the owner of a secured object.
       The first member is always the token owner.
       These Sids can be used to change the owner. Without restore privilege the owner
       can only changed to one of the returned SIDs. However the function ignores restore privileges.

       The token is opened with the TOKEN_READ access right.


       @return Returns a list of sids. The list must be freed by the caller.
       raises
 EJwsclSecurityException:  An exception can be raised if a call to one of the following items failed:
                
                 # CreateTokenEffective 
                 # GetTokenGroups
                 # GetTokenOwner 
                 # TJwSecurityId.Create
                 
      }

    class function GetOwnerShipMembers: TJwSecurityIdList; override;

    { <b>ConvertMaximumAllowed</b> converts the meta access right MAXIMUM_ALLOWED to
      basic access rights that can be granted to the given client token. The access
      rights are read from the given security descriptor. The mapping parameter
      defines a map between generic and specific access rights.
      Parameters
      SecurityDescriptor :  defines a security descriptor which is not nil and has an
                            owner and a group defined; otherwise the call fails. 
      ClientToken :         defines a token which is used to enforce the access rights 
      GenericMapping :      defines a mapping between generic rights and specific
                            rights 
      Returns
      \Returns an combination of access rights which are granted
      Exceptions
      EJwsclSecurityException :  This function does not raise exception. However the
                                 called function AccessCheck can raise an exception.    }
    class function ConvertMaximumAllowed(
       const SecurityDescriptor: TJwSecurityDescriptor;
       const ClientToken: TJwSecurityToken;
       const GenericMapping:
        TJwSecurityGenericMappingClass
     ) : TAccessMask; override;


    property AccessMask: TJwAccessMask Read fAccessMask Write fAccessMask;

    {<B>Handle</B> is not used in this class.}
    property Handle: THandle Read fHandle;

  end;


type
   {<B>TJwSecureFileObject</B> provides access to security relevant information for files and folders.
    The words file and folders are combined into file object. It is used equally for both files and folders.}
  TJwSecureFileObject = class(TJwSecureBaseClass)
  protected
    fDuplicateHandle: boolean;
    fFileName:        TJwString;

    {<B>TreeFileObjectSetNamedSecurityInfo_Execute</B> is used as a thread function by TreeFileObjectSetNamedSecurityInfo}
    class function TreeFileObjectSetNamedSecurityInfo_Execute(
      pData: Pointer): integer;
  public
      {<B>Create</B> creates an file object instance using a filestream (but not the file itself!)
       

       FileStream does not support SACL retrieving or setting. The reason is because
       a TFilestream instance does not open the file with an access mask including ACCESS_SYSTEM_SECURITY
       which is necessary to open a file to retrieve the SACL.
       To do so, use Create with a filename or file handle.
       See also MSDN: http://support.microsoft.com/?scid=kb%3Ben-us%3B240184&x=11&y=11

       The property Handle will be set to the duplicated file handle.

      @param F gets the filestream to be used to get or set the security data.
      @param AccessMask gets the desired access mask to the new file handle. If set to 0 the same access mask of the old handle is used.
      raises
 RsNilParameter:  will be raised if parameter F is nil)
      }
    constructor Create(const F: TFileStream; AccessMask: TJwAccessMask = 0);
      overload;

    {<B>Create</B> creates an file object instance using a filehandle
      The file handle will be duplicated using DuplicateHandle. By default
       the same access mask is used.

      @param FileHandle defines the file handle to be used.
          It will be duplicated if parameter bDuplicateHandle is true. In this case
          AccessMask defines which access will be allowed on to the handle. It always
          can only equal or smaller than the original access mask. If AccessMask is zero (or DUPLICATE_SAME_ACCESS)
          the handle will be duplicated with same access.
          A duplicated file handle is automatically closed if the instace is destroyed. 
      @param AccessMask gets the desired access mask to the new file handle. If set to 0 the same access mask of the old handle is used. 
      @param bDuplicateHandle defines whether the file handle should be duplicated or directly be used
      raises
 EJwsclSecurityObjectException:  will be raised if the file handle could not be copied. 
      }
    constructor Create(const FileHandle: THandle;
      AccessMask: TJwAccessMask = 0; bDuplicateHandle: boolean = False); overload;

    {<B>Create</B> creates a file or folder object instance using a filename (also a folder name).
     The property Handle will not be used.

     <B>Create</B> does not open the file or folder immediately. Instead it uses the name
     to do its operations.

     @param FileName gets the full path to the file or folder. 
     raises
 EJwsclInvalidParameterException:  will be raised if the file or folder
      could not be found  
     }
    constructor Create(const FileName: TJwString); overload;

      {<B>Destroy</B> destroys the instance.
       If a handle was duplicated it is also closed.}
    destructor Destroy; override;

      {<B>GetDACL</B> returns the DACL of the file object.
       @return Returns either a DACL instance or nil if the file object
        does not have a DACL an consequently everyone has full access to it.

        The newly created instance must be destroyed by the caller!
       }
    function GetDACL: TJwDAccessControlList; override;

      {<B>GetSACL</B> returns the SACL of the file object.
       You need to have SE_SECURITY_NAME privilege be enabled otherwise the call fails.

       @return Returns either a SACL instance or nil if the file object
        does not have a SACL.
        The newly created instance must be destroyed by the caller!
       }
    function GetSACL: TJwSAccessControlList; override;

    {<B>GetMandatoryLabel</B> returns the mandatory level of the object. The object is retrieved
     from the SACL.
     Its only supported on Windows Vista and newer.
     @return Returns a mandatory level entry which must be freed. }
    function GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry; override;

    {<B>SetMandatoryLabel</B> sets the mandatory level of the object.
     raises
 EJwsclInvalidObjectException:  will be raised if the handle and name are invalid 
     }
    procedure SetMandatoryLabel(const MandatoryLabel :
       TJwSystemMandatoryAccessControlEntry); override;

    {<B>GetOwner</B> returns the owner of the file object.
      @return The newly created instance must be destroyed by the caller!
    }
    function GetOwner: TJwSecurityId; override;

      {<B>GetGroup</B> returns the group of the file object.
       @return The newly created instance must be destroyed by the caller!
      }
    function GetGroup: TJwSecurityId; override;

      {<B>SetDACL</B> sets the DACL of the file object.
       You need WRITE_DACL rights to set the DACL or the curren token must be
       the owner.

       If you want to replace an existing DACL completly, you have to first
       call SetDACL with a nil list-Parameter. This creates a NULL DACL (with everybody has access to it)
       Afterwards you can set a new DACL to the file.
       So nobody can hijack the file you should also open the file exclusively and do not use the SetNamedXXX methods.

       New: You can also use apProtected to remove inherited ACEs (replace an existing DACL completely).

       The list is copied into the file object.

       @param Protection defines which TJwSecurityInformationFlag Flag is used:
        
          # apNone uses simply siDaclSecurityInformation to set DACL 
          # apProtected uses siProtectedDaclSecurityInformation to set a protected DACL 
          # apUnprotected uses siUnprotectedDaclSecurityInformation to set
              an unprotected DACL and let flow the inheritance stream 
          
       }
    procedure SetDACL(const list: TJwDAccessControlList;
     const Protection : TJwACLProtectionState = apNone); overload; override;
      {<B>SetSACL</B> sets the SACL of the file object.
       You need to have SE_SECURITY_NAME privilege be enabled otherwise the call fails.

       New: You can also use apProtected to remove inherited ACEs (replace an existing SACL completly).

       The list is copied into the file object.

       @param Protection defines which TJwSecurityInformationFlag Flag is used:
        
          # apNone uses simply siSaclSecurityInformation to set SACL 
          # apProtected uses siProtectedSaclSecurityInformation to set a protected SACL 
          # apUnprotected uses siUnprotectedSaclSecurityInformation to set
              an unprotected SACL and let flow the inheritance stream 
          
       }
    procedure SetSACL(const list: TJwSAccessControlList;
     const Protection : TJwACLProtectionState = apNone); overload; override;


      {<B>SetOwner</B> sets the owner of the file object.
       You need WRITE_DACL and WRITE_OWNER rights to set the DACL or
        the privilege SE_TAKE_OWNERSHIP must be enabled.
       To make a successfull call the ID must be the user of the thread calling this function.
        Only a user with enough rights can take ownership to his username.
       To set the owner to a different user as the token owner
        the user must be logged on (using LogonUser or similar), the thread must be
        impersonated to this user and the <B>SetOwner</B> is called in this thread.

       The SID is copied into the file object.
       }
    procedure SetOwner(const ID: TJwSecurityId); overload; override;

      {<B>SetGroup</B> sets the group of the file object.

      The SID is copied into the file object.}
    procedure SetGroup(const ID: TJwSecurityId); overload; override;

      {<B>SetSecurityDescriptor</B> sets the security descriptor of the file.
       It simply calls all SetXXX methods if defined in SD_entries.
       If a entry of the SD cannot be set an exception is raised and the rest is dismissed.
       However all entries that were successfully set before the exception are stored into the file security.

       The following values are ignored in SD_entries:
       
        # siprotectedDaclSecurityInformation 
        # siUnprotectedDaclSecurityInformation 
        # siprotectedSaclSecurityInformation 
        # siUnprotectedSaclSecurityInformation
        # siLabelSecurityInformation : Use SetMandatoryLabel instead
       
       Instead use TJwSecurityDescriptor.InheritanceDACLProtection or
         TJwSecurityDescriptor.InheritanceSACLProtection to control inheritance.

       
       @param SD defines the security descriptor to be set. It must not nil otherwise EJwsclInvalidParameterException is raised.
       @param SD_entries The following security descriptor flags are supported
                  siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation, siSaclSecurityInformation

       @param EJwsclNILParameterException will be raised if parameter SD is nil.

       }
    procedure SetSecurityDescriptor(const SD: TJwSecurityDescriptor;
      const SD_entries: TJwSecurityInformationFlagSet); overload; override;

       {<B>GetSecurityDescriptor</B> returns a security descriptor of the file with specified SD entries given in parameter SD_entries.
        @param SD_entries The following security descriptor flags are supported
                  siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation, siSaclSecurityInformation

      }
    function GetSecurityDescriptor(
      const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
      overload; override;

      {<B>GetTempOwner</B> returns the owner of a file object.
       It does the same like GetOwner but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
       }
    function GetTempOwner: TJwSecurityId; overload; override;

      {<B>GetTempGroup</B> returns the group of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempGroup: TJwSecurityId; overload; override;

      {<B>GetTempDACL</B> returns the DACL of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempDACL: TJwDAccessControlList; overload; override;

      {<B>GetTempSACL</B> returns the SACL of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempSACL: TJwSAccessControlList; overload; override;

      {<B>ResetTemp</B> resets the cache. Subsequent calls to GetTempXXX will not
       use the cache instead it calls the security winapi functions.
       @param resetState gets a set of caches to be cleared.
      }
    procedure ResetTemp(resetState: TJwTempResetEnumSet = JwResetEnumSetAll);
      override;

    //<B>DACL</B> returns a cached version of the file object DACL
    property DACL: TJwDAccessControlList Read GetTempDACL;
    //<B>SACL</B> returns a cached version of the file object SACL
    property SACL: TJwSAccessControlList Read GetTempSACL;

    //<B>Owner</B> return a cached version of the file object owner
    property Owner: TJwSecurityId Read GetTempOwner Write SetOwner;

    //<B>Group</B> return a cached version of the file object group
    property Group: TJwSecurityId Read GetTempGroup Write SetGroup;

      {<B>Handle</B> returns the handle of the currently used file object.
       If the instance was created using a filename the Handle is 0.
       It will be automatically freed.
      }
    property Handle: THandle Read fHandle;

      {<B>AutoResetACL</B> defines whether a call to SetDACL (implicit set property DACL) and to SetSACL (implicit set property SACL)
        removes the old ACL by setting it to nil (true) or leaves it alone (false).
       Set to true if you want to remove all ACEs before restructure new ones.
      }
    property AutoResetACL;

    {<B>AccessMask</B> defines desired access.
     It is used if DesiredAccess parameter of AccessCheck methods is (-1).}
    property AccessMask: TJwAccessMask Read fAccessMask Write fAccessMask;


      {<B>AccessCheck</B> checks whether the user has access to the the file of the instance or not.
       @param DesiredAccess defines which access rights are to be checked. If set to High(Cardinal)
          the access rights given in the constructor are used.

       Exceptions
        EJwsclInvalidParameterException: This exception will be raised if DesiredAccess contains the
            MAXIMUM_ALLOWED access right. If you want to check for granted access rights call
            the other method and read parameter GrantedAccess.
      }
    function AccessCheck(DesiredAccess: TJwAccessMask = Cardinal(-1);
      const ClientToken: TJwSecurityToken = nil)
      : boolean;
      overload; override;

    {<B>AccessCheck</B> checks the access to a security descriptor of a secure object.}
    procedure AccessCheck(
      DesiredAccess: TJwAccessMask;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean;
      const ClientToken: TJwSecurityToken = nil);
      overload; virtual;

    { <b>AccessCheck</b> checks the access to a security descriptor of a secure
        object. See AccessCheck in MSDN for more information
        Parameters
        SecurityDescriptor :  Contains the security descriptor that is used to check for
                              access.
        ClientToken :         A token that is used to get the SID and privileges which
                              are used to check against the security descriptor. The
                              parameter can be nil to use the current thread or process
                              token.<p /><p />In contrast to the original AccessCheck API
                              function, this method automatically adjusts the token type
                              to "impersonation" if the given token is a primary one.
                              This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                              (1309). If ClientToken is not nil the method makes a copy
                              of ClientToken and converts it to a thread token. If
                              ClientToken is nil, it retrieves the current thread or
                              primary token (if no thread token is available) and, in the
                              latter case, converts it to a thread token. Furthermore
                              this method does not impersonate any token and thus leaves
                              an already existing thread token intact.<p />
        DesiredAccess :       Defines the desired access to the object.<p /><b>New</b><p />Although
                              the MSDN AccessCheck forbids generic rights (like
                              GENERIC_ALL) in this Parameter. The method AccessCheck will
                              replace all generic rights with specific rights using the
                              mapping defined by parameter GenericMapping. However the
                              original SecurityDescriptor will remain the same. <b>Warning</b>
                              Some generic access rights may overlap. This can lead to
                              access denied.<p /><b>Example</b><p />DACL contains a
                              positive ACE with GENERIC_WRITE and a negative ACE with
                              GENERIC_READ A call to AccessCheck with DesiredAccess set
                              to GENERIC_WRITE and GenericMapping set to
                              TJwSecurityFileMapping (using FileGenericMapping) will fail
                              because GENERIC_WRITE and GENERIC_READ are resolve to
                              FILE_GENERIC_WRITE and FILE_GENERIC_READ which both contain
                              SYNCHRONIZE ($100000) and READ_CONTROL ($2000).<p />
        GenericMapping :      Receives a class type of the class
                              TJwSecurityGenericMapping or one of her derived classes. If
                              the generic class TJwSecurityGenericMapping is used, all
                              generic access rights are mapped to standard access rights
                              (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                              access rights in parameter DesiredAccess that are mapped by
                              the given TJwSecurityGenericMappingClass class; otherwise
                              AccessChec will fail with EJwsclWinCallFailedException.
                              E.g. TJwSecurityGenericMappingClass can be used with
                              DesiredAccess set to STANDARD_RIGHTS_ALL.<p /><p /><b>New</b><p />All
                              access entriey (ACEs) in the security descriptor DACL are
                              scanned for GENERIC access rights (like GENERIC_ALL) and
                              automatically converted to specific rights using parameter
                              GenericMapping. However the original SecurityDescriptor
                              will remain the same. This will not happen, if nil is
                              supplied to this parameter. Make sure there are no generic
                              rights in the ACL or DesiredAccess parameter.<p />
        PrivilegeSet :        Receives the privileges that are used for access check. If
                              none are used, this output will be nil. The caller is
                              responsible for destroying the object!
        GrantedAccess :       Receives an access mask that indicates which rights were
                              granted.
        AccessStatus :        Receives the result of the access check. True if access is
                              granted, otherwise false.
        Exceptions
        EJwsclInvalidParameterException :  will be raised if parameter SecurityDescriptor
                                           is nil;
        EJwsclWinCallFailedException :     will be raised if the call to AccessCheck
                                           failed.
        EJwsclInvalidOwnerSIDException :   will be raised if the owner of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of owner to AccessCheck call.
        EJwsclInvalidGroupSIDException :   will be raised if the group of the security
                                           descriptor is nil. Use JwNullSID to remove
                                           influence of group to AccessCheck call.                  }
    class procedure AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean);
      overload; virtual;

      {<B>AccessCheckAndAuditAlarm</B>.
       See http://msdn2.microsoft.com/en-us/library/aa374823.aspx
        for more information.

       Not tested yet! }
    class procedure AccessCheckAndAuditAlarm(
      const SubsystemName: TJwString;
      const HandleID: Pointer;
      const ObjectTypeName,
      ObjectName: TJwString;
      const SecurityDescriptor:
      TJwSecurityDescriptor;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      const ObjectCreation: boolean;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean;
      out pfGenerateOnClose: boolean);
      overload; override;

       {<B>TreeResetNamedSecurityInfo</B> sets the securiy information of a folder and all its children.
        See MSDN for more information http://msdn2.microsoft.com/en-us/library/aa965849.aspx.
        This function needs Windows XP or higher.

        This method can restore the security information for a locked out user without using SeBackupPrivilege privilege.
        It uses a highly privileged system accout to do this.

        Both callback methods can be used at the same time. If both are not nil, they are called in the following order :
         FNProgressMethod, FNProgressProcedure. Exceptions are caught in these calls.

        @param aSecurityInfo defines which security information to be set. (Owner...SACL). If a flag is not set, but the
                parameter Owner...SACL is not nil, it is also set. 
        @param FNProgressMethod defines a object method to be called if a file or folder is changed. Can be nil. See TJwFnProgressMethod  
        @param FNProgressProcedure defines a procedure to be called if a file or folder is changed. Can be nil. See TJwFnProgressProcedure
        @param ProgressUserData defines user data to be used as parameter ProgressUserData in the callback methods. 

        raises
           EJwsclWinCallFailedException:  is raised if the winapi call failed.
           EJwsclUnsupportedWindowsVersionException: This is exception is raised if the Windows system does not support this call.
        }
    class procedure TreeResetNamedSecurityInfo(pObjectName: TJwString;
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL:
      TJwDAccessControlList;
      const SACL:
      TJwSAccessControlList;
      const bKeepExplicit: boolean;
      const fnProgress:
      TJwProgInvokeSetting;
      const FNProgressMethod: TJwFnProgressMethod;
      const FNProgressProcedure:
      TJwFnProgressProcedure;
      const ProgressUserData: Pointer); reintroduce; virtual;

       {<B>TreeFileObjectSetNamedSecurityInfo</B> sets or resets the security information of a folder tree. The process can be observed by a callback method or function.
        This method can be run in a seperate thread so that it immediately returns.

        This method cannot restore the security information for a locked out user without using SeBackupPrivilege privilege.
        <B>TreeFileObjectSetNamedSecurityInfo</B> cannot change inheritance protection flow.


        @param pObjectName defines the folder which structure is about to be set or set. It must be an absolute folder path. It is called the root folder. 
        @param aSecurityInfo defines the type of security (DACL, SACL, owner, group) and also inheritance protection to be set.
                The ACEs in DACL and/or SACL is set once in the root folder and inherited to the children based on the ACE flags. 
        @param Action defines in which case the callback method/function is called.
                  The following constants are supported
                   
                    # pis_ProgressInvokeNever  The callback method should not be called 
                    # pis_ProgressInvokeEveryObject The callback method is called for all objects. Including the finished callback. 
                    # pis_ProgressInvokeOnError The callback method is called for all errors. Including the finished callback.
                    
                  The finish callback is called after the security of the last file/folder was changed. The last callback
                  uses the parameter pObjectName and the constant pis_ProgressFinished in parameter pInvokeSetting. 
                 
        @param SetType defines the behaviour of this function.
                
                 # tstSet Simply sets the security setting defined in DACL, SACL, owner or group based on the inheritance flags of the given ACEs. 
                 # tstReset Removes all inheritance protection and ACEs from DACL and SACL of the given tree and sets the security settings like tstSet.
                       The explicit ACEs are preserved if parameter bKeepExplicit is set to true.
                        
                
                 

        @param bKeepExplicit Only applies to tstReset in parameter SetType. It preserves the explicit ACEs of DACL and SACL of root folder and all children from deletion. 
        @param Owner Defines the Owner SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siOwnerSecurityInformation.
                        The instance is copied into a new object. 
        @param Group Defines the Group SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siGroupSecurityInformation.
                        The instance is copied into a new object. 
        @param DACL Defines the DACL SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siDaclSecurityInformation.
                Do not set DACL to nil and include it into aSecurityInfo. A nil DACL cannot be inherited to children. Use instead a GENERIC_ALL with a JwWorldSID.
                The instance is copied into a new object.
                 
        @param SACL Defines the SACL SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siSaclSecurityInformation.
                The instance is copied into a new object. 
        @param FNProgressMethod defines a callback method. It can be nil. This callback function is called before FNProgressProcedure. 
        @param FNProgressProcedure defines a callback procedure. It can be nil. This callback function is called after FNProgressMethod. 
        @param ProgressUserData defines user data that is directed to the callback method 

        @param aThread defines a thread instance that is used to call this function in a seperate thread. To use <B>TreeFileObjectSetNamedSecurityInfo</B> in a thread
                 simply create the thread (aThread := TJwTagThread.Create) and assign it to the parameter aThread.
                 Do not call Create with a parameter.
               
        @param Disable64Redirection defines whether the 64bit redirection of files for 32bit application is deactivated (TRUE) or activated (FALSE).
                        This parameter only works on 64bit windows plattforms. If the plattform is not a 64bit and
                        Disable64Redirection is true, nothing will happen! 


       }
    class procedure TreeFileObjectSetNamedSecurityInfo(
      pObjectName: TJwString;
      const aSecurityInfo:  TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Action: TJwProgInvokeSetting;
      const SetType: TJwTreeSetType;
      const bKeepExplicit: boolean;
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL: TJwDAccessControlList;
      const SACL: TJwSAccessControlList;
      const FNProgressMethod: TJwFnProgressMethod;
      const FNProgressProcedure: TJwFnProgressProcedure;
      const ProgressUserData : Pointer;
      const aThread: TJwTagThread;
      const Disable64Redirection:
      boolean = False); virtual;

      {<B>GetInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       See MSDN http://msdn2.microsoft.com/en-us/library/aa446640.aspx.

       @return The return value is an array of TJwInheritedFromRecord that maps the winapi struct
        INHERITED_FROM to a Delphi type. The function FreeInheritedFromArray must not be called therefor,
        instead use SetLength(result,0);
        The member SID is not used here.


       This function does not work in all version of Windows XP. If so the
       exception EJwsclInheritanceSourceNotSupportedException is raised.

       }
    function GetInheritanceSource(
      const pObjectName: TJwString;
    //LPTSTR pObjectName,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
      const bIsDirectory: boolean;
    //BOOL Container,
      const GUIDs: TJwGuidArray;
    //GUID** pObjectClassGuids,
      const FN_OBJECT_MGR_FUNCTS: Pointer =
      nil): TJwInheritedFromArray;
    {PINHERITED_FROM pInheritArray} reintroduce; virtual;

      {<B>GetFileInheritanceSource</B> retrieves the source of inheritance for the ACEs in the ACL of the given object.
       This method simulates GetInheritanceSource so it can be used in all windows versions with ACL support.

       @param PathName defines an absolute PathName to a file or folder
       @param aSecurityInfo defines the type of security inheritance is to be obtained. The value can be one of siDaclSecurityInformation or siSaclSecurityInformation.
       @param OnGetNamedSecurityInfo defines a callback function which is called everytime security information is retrieved. Set to nil if not used.
       @param Data receives custom information applied to the Data parameter of all OnGetNamedSecurityInfo function calls.

       @return The return value is an array of TJwInheritedFromRecord with the count of ACE in the DACL of the object in PathName
                Each array entry consists of
                 
                  # GenerationGap Levels between the object and the ancestor 
                  # AncestorName Name of the ancestor from which the ACE was inherited 
                  # SID The SID that is inherited. The string has the type <pre><SID account name>@<S-X-X...> </pre>
                 
       raises
 EJwsclInvalidPathException:  will be raised if the instance is created with a handle instead of a file or folder name.
        EJwsclInvalidObjectException: will be raised if the object could not be accessed or found.
        EJwsclInvalidParameterException: will be raised if aSecurityInfo is not [siDaclSecurityInformation] or [siSaclSecurityInformation]
        EJwsclPrivilegeNotFoundException: will be raised if aSecurityInfo is [siSaclSecurityInformation] and the current thread does cannot access audit information, because the privilege could not be activated.

       Remarks
         This functions emulates the GetInheritanceSource function available in
         Windows XP and newer. Since the inheritance functionality of NTFS
         isn't fully known to the author (and also may change) the emulation
         will not be 100% compatible to the original.
         If you run on Windows XP or newer you should consider to use the original
         function. However in this case you can't use a security descriptor cache mechanism
         through the OnGetNamedSecurityInfo event.
       }
    class function GetFileInheritanceSource(const PathName: TJwString;
      const Container : Boolean;
      const aSecurityInfo: TJwSecurityInformationFlagSet =
      [siDaclSecurityInformation];

      const OnGetNamedSecurityInfo : TJwOnGetNamedSecurityInfo = nil;
      const Data : Pointer = nil
      ): TJwInheritedFromArray; overload; virtual;

      {<B>GetFileInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       This method simulates GetInheritanceSource, so that it can be used in all windows versions with ACL support.

       @param aSecurityInfo defines the type of security inheritance is to be obtained. The value can be one of siDaclSecurityInformation or siSaclSecurityInformation.

       @return The return value is an array of TJwInheritedFromRecord with the count of ACE in the DACL of the object in PathName
                Each array entry consists of
                 
                  # GenerationGap Levels between the object and the ancestor 
                  # AncestorName Name of the ancestor from which the ACE was inherited 
                  # SID The SID that is inherited. The string has the type <pre><SID account name>@<S-X-X...> </pre>
                 
       raises
 EJwsclInvalidPathException:  will be raised if the instance is created with a handle instead of a file or folder name.
        EJwsclInvalidObjectException: will be raised if the object could not be accessed or found.
        EJwsclInvalidParameterException: will be raised if aSecurityInfo is not [siDaclSecurityInformation] or [siSaclSecurityInformation]
        EJwsclPrivilegeNotFoundException: will be raised if aSecurityInfo is [siSaclSecurityInformation] and the current thread does cannot access audit information, because the privilege could not be activated.
      }
    function GetFileInheritanceSource(
      const Container : Boolean;
      const aSecurityInfo: TJwSecurityInformationFlagSet =
      [siDaclSecurityInformation]): TJwInheritedFromArray; overload; virtual;


      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file handle;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL 

       See SetNamedSecurityInfo for exceptions.
      }
    class procedure RemoveInheritanceFlow(const Handle: THandle;
      const bCopyInheritedACEs: boolean = True); overload; virtual;


      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file or folder pathname;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL 

       See SetNamedSecurityInfo for exceptions.
      }
    class procedure RemoveInheritanceFlow(const PathName: TJwString;
      const bCopyInheritedACEs: boolean = True); overload; virtual;

      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file or folder pathname;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL 

      raises
 EJwsclInvalidObjectException:  will be raised if the handle and filename is invalid.

      See SetNamedSecurityInfo for exceptions.
      }
    procedure RemoveInheritanceFlow(
      const bCopyInheritedACEs: boolean = True); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
        <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL.If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised. 

       See SetNamedSecurityInfo for exceptions.
       raises
 EJwsclAdaptSecurityInfoException:  will be raised if bTakeOwnerShip is true and the ownership cannot be taken.
      }
    class procedure RestoreInheritanceFlow(const Handle: THandle;
      bTakeOwnerShip: boolean = False); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
        <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL.If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised. 

       See SetNamedSecurityInfo for exceptions.

       @param PathName defines a file or folder path.
       raises
 EJwsclAdaptSecurityInfoException:  will be raised if bTakeOwnerShip is true and the ownership cannot be taken.
      }
    class procedure RestoreInheritanceFlow(const PathName: TJwString;
      bTakeOwnerShip: boolean = False); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
       <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL. If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised. 

       See SetNamedSecurityInfo for exceptions.

       raises
 EJwsclInvalidObjectException:  will be raised if the handle and filename is invalid.
        EJwsclAdaptSecurityInfoException: will be raised if bTakeOwnerShip is true and the ownership cannot be taken.

      }
    procedure RestoreInheritanceFlow(bTakeOwnerShip: boolean = False);
      overload; virtual;

      {<B>SupportACL</B> checks if a given drive or UNC Path supports ACL.
       @param RootPathName contains a drive path or UNC path.
         A drive must consist at least of a char followed by ":".
         The function parses the drive letter automatically from a full path name.
          e.g. <pre>C:\\Windows\\win.com -> C:\\</pre>
         A UNC path contains at least of a servername and a UNC folder.
         Fully qualified UNC path are also stripped :
           <pre>\\\\server\\folder\\subfolder -> \\\\server\\folder\\</pre>

         RootPathName can also be an relative path (without a drive). It will be expanded to an absolute path automatically.

       @return Returns true if ACL is supported; otherwise false.
       raises
         EJwsclWinCallFailedException:  will be raised if GetVolumeInformation returned an error
         EJwsclInvalidParameterException: will be raised if RootPathName contains no drive letter
             or does not contain a valid UNC path.
      }
    class function SupportACL(RootPathName: TJwString): boolean;


      {<b>TakeOwnerShip</b> takes the ownership of a file or folder. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param PathName Defines the file or folder which owner is to be changed
       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const PathName: TJwString;
      SID: TJwSecurityId = nil); overload; virtual;

      {<b>TakeOwnerShip</b> takes the ownership of a file or folder. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param Handle Defines a handle to the file or folder which owner is to be changed
       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const Handle: THandle;
      SID: TJwSecurityId = nil); overload; virtual;

      {<b>TakeOwnerShip</b> takes the ownership of a file or folder used by this instnace. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    procedure TakeOwnerShip(SID: TJwSecurityId = nil); overload; virtual;


  end;



  {<B>TJwSecureRegistryKey</B> provides access to registry key security information}
  TJwSecureRegistryKey = class(TJwSecureBaseClass)
  protected
    fDuplicateHandle: boolean;
    fKeyName:         TJwString;

    fReg: TRegistry;

    
    function GetKey(out bUsesName: boolean): HKEY; virtual;
    class function TreeRegKeySetNamedSecurityInfo_Execute(pData: Pointer)
      : integer; virtual;
  public

    {<B>Create</B> creates an registry object instance using a key handle.
      The handle will be duplicated using DuplicateHandle. By default
       the same access mask is used.

      @param RegKey defines the handle to be used.
          It will be duplicated if parameter bDuplicateHandle is true. In this case
          AccessMask defines which access will be allowed on to the handle. It always
          can only equal or smaller than the original access mask. If AccessMask is zero (or DUPLICATE_SAME_ACCESS)
          the handle will be duplicated with same access.
          A duplicated handle is automatically closed if the instace is destroyed. 
      @param AccessMask gets the desired access mask to the new handle. If set to 0 the same access mask of the old handle is used. 
      @param bDuplicateHandle defines whether the handle should be duplicated or directly be used 
      raises
        EJwsclSecurityObjectException:  will be raised if file handle could not be copied.
      }
    constructor Create(const RegKey: HKEY; AccessMask: TJwAccessMask = 0;
      bDuplicateHandle: boolean = False); overload;

    {<B>Create</B> creates an instance using a VCL TRegistry to obtain key handle information.
     @param RegistryInstance receives a VCL registry instance that is used to get
       key handle information. The handle will not be duplicated so that changes in the registry
       instance will lead to problems when calling methods of TJwSecureRegistryKey.
     @param bUseName is not used
     raises
       EJwsclNILParameterException:  will be raised if parameter RegistryInstance is nil
    }
    constructor Create(const RegistryInstance: TRegistry; bUseName: boolean = False);
      overload;

      {<B>Create</B> creates a registry instance using a path to the registry key.
       The property Handle will not be used.

       @param RegPath This parameter receives the absolute path to the registry key to be accessed.
        Be aware that "HKEY_xxx" constants must be string mapped to "xxx" (without "HKEY_")
        The constructor does not check validity. This check is done when you call any of the
        security methods which actually access the key. In most cases an exception
        with GetLastError "invalid parameter" is raised.

       }
    constructor Create(const RegPath: TJwString); overload;

      {<B>Destroy</B> destroys the instance.
       If a handle was duplicated it is also closed.}
    destructor Destroy; override;

      {<B>GetDACL</B> returns the DACL of the file object.
       @return Returns either a DACL instance or nil if the file object
        does not have a DACL an consequently everyone has full access to it.

        The newly created instance must be destroyed by the caller!
       }
    function GetDACL: TJwDAccessControlList; override;

      {<B>GetSACL</B> returns the SACL of the file object.
       You need to have SE_SECURITY_NAME privilege be enabled otherwise the call fails.

       @return Returns either a SACL instance or nil if the file object
        does not have a SACL.
        The newly created instance must be destroyed by the caller!
       }
    function GetSACL: TJwSAccessControlList; override;

     {<B>GetMandatoryLabel</B> returns the mandatory level of the object. The object is retrieved
     from the SACL.
     Its only supported on Windows Vista and newer.
     @return Returns a mandatory level entry which must be freed. }
    function GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry; override;

    {<B>SetMandatoryLabel</B> sets the mandatory level of the object.
    raises
      EJwsclInvalidObjectException:  will be raised if the handle and name are invalid
    }
    procedure SetMandatoryLabel(const MandatoryLabel :
      TJwSystemMandatoryAccessControlEntry); override;
      {<B>GetOwner</B> returns the owner of the file object.
       @return The newly created instance must be destroyed by the caller!
      }
    function GetOwner: TJwSecurityId; override;

      {<B>GetGroup</B> returns the group of the file object.
       @return The newly created instance must be destroyed by the caller!
      }
    function GetGroup: TJwSecurityId; override;

      {<B>SetDACL</B> sets the DACL of the file object.
       You need WRITE_DACL rights to set the DACL or the curren token must be
       the owner.

       If you want to replace an existing DACL completly, you have to first
       call SetDACL with a nil list-Parameter. This creates a NULL DACL (with everybody has access to it)
       Afterwards you can set a new DACL to the file.
       So nobody can hijack the file you should also open the file exclusively and do not use the SetNamedXXX methods.

       <b>New</b><br/>
         You can also use apProtected to remove inherited ACEs (replace an existing DACL completly).

       The list is copied into the file object.

       @param Protection defines which TJwSecurityInformationFlag Flag is used:

          # apNone uses simply siDaclSecurityInformation to set DACL
          # apProtected uses siProtectedDaclSecurityInformation to set a protected DACL
          # apUnprotected uses siUnprotectedDaclSecurityInformation to set
              an unprotected DACL and let flow the inheritance stream


       }
    procedure SetDACL(const list: TJwDAccessControlList;
     const Protection : TJwACLProtectionState = apNone); overload; override;
      {<B>SetSACL</B> sets the SACL of the file object.
       You need to have SE_SECURITY_NAME privilege be enabled otherwise the call fails.

       <b>New</b><br/>
         You can also use apProtected to remove inherited ACEs (replace an existing SACL completly).

       The list is copied into the file object.

       @param Protection defines which TJwSecurityInformationFlag Flag is used:

          # apNone uses simply siDaclSecurityInformation to set DACL
          # apProtected uses siProtectedDaclSecurityInformation to set a protected DACL
          # apUnprotected uses siUnprotectedDaclSecurityInformation to set
              an unprotected DACL and let flow the inheritance stream


       }

    procedure SetSACL(const list: TJwSAccessControlList;
      const Protection : TJwACLProtectionState = apNone); overload; override;

      {<B>SetOwner</B> sets the owner of the file object.
       You need WRITE_DACL and WRITE_OWNER rights to set the DACL or
        the privilege SE_TAKE_OWNERSHIP must be enabled.
       To make a successfull call the ID must be the user of the thread calling this function.
        Only a user with enough rights can take ownership to his username.
       To set the owner to a different user as the token owner
        the user must be logged on (using LogonUser or similar), the thread must be
        impersonated to this user and the <B>SetOwner</B> is called in this thread.

       The SID is copied into the file object.
       }
    procedure SetOwner(const ID: TJwSecurityId); overload; override;

      {<B>SetGroup</B> sets the group of the file object.

      The SID is copied into the file object.}
    procedure SetGroup(const ID: TJwSecurityId); overload; override;

      {<B>GetTempOwner</B> returns the owner of a file object.
       It does the same like GetOwner but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
       }
    function GetTempOwner: TJwSecurityId; overload; override;

      {<B>GetTempGroup</B> returns the group of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempGroup: TJwSecurityId; overload; override;

      {<B>GetTempDACL</B> returns the DACL of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempDACL: TJwDAccessControlList; overload; override;

      {<B>GetTempSACL</B> returns the SACL of a file object.
       It does the same like GetGroup but the return value is stored
        and used as a cache. Consequently the returned instance must not be freed!
       The first call gets the file object owner. The following calls
       use the data from the first call.
      }
    function GetTempSACL: TJwSAccessControlList; overload; override;


      {<B>ResetTemp</B> resets the cache. Subsequent calls to GetTempXXX will not
       use the cache instead it calls the security winapi functions.
       @param resetState gets a set of caches to be cleared.
      }
    procedure ResetTemp(resetState: TJwTempResetEnumSet = JwResetEnumSetAll);
      override;

    //<B>DACL</B> returns a cached version of the file object DACL
    property DACL: TJwDAccessControlList Read GetTempDACL;
    //<B>SACL</B> returns a cached version of the file object SACL
    property SACL: TJwSAccessControlList Read GetTempSACL;

    //<B>Owner</B> return a cached version of the file object owner
    property Owner: TJwSecurityId Read GetTempOwner Write SetOwner;

    //<B>Group</B> return a cached version of the file object group
    property Group: TJwSecurityId Read GetTempGroup Write SetGroup;

      {<B>Handle</B> returns the handle of the currently used file object.
       If the instance was created using a filename the Handle is 0.
       It will be automatically freed.
      }
    property Handle: THandle Read fHandle;

      {<B>AutoResetACL</B> defines whether a call to SetDACL (implicit set property DACL) and to SetSACL (implicit set property SACL)
        removes the old ACL by setting it to nil (true) or leaves it alone (false).
       Set to true if you want to remove all ACEs before restructure new ones.
      }
    property AutoResetACL;

      {<B>ParseUNC</B> retrieves from a UNC key name the server name, the root key and the sub key (if any).
       If the given KeyName is not a UNC path the return value is false and the output parameter Server is empty.
       The server name can also be the local computer name, "local" or "localhost". The last two names are
       automatically translated into the computer name and returned in parameter Server.

       <pre>UNC: \\server\root\subkey</pre>
       <pre>standard: root\subkey</pre>

       The function does not raise an exception.

       @param KeyName defines the key path to be parsed
       @param Server receives the server name from the key, if parameter KeyName is a UNC. Otherwise the value is empty.
       @param SubKey receives the key path without the root name (e.g. LOCAL_MACHINE). It does not start with a "\" but ends with it.
       @param aRootTuple received the key root in a structure that contains a string, a HKEY and a special enum value.
         The KeyName can contain only one root identifier. The root name can be one of the following :
         
          # MACHINE also HKEY_MACHINE 
          # CURRENT_USER also HKEY_CURRENT_USER 
          # USERS also HKEY_USERS 
          # CONFIG also HKEY_CONFIG 
          # CLASSES_ROOT also HKEY_CLASSES_ROOT 
           
       @return The return value is true if the given KeyName is a UNC path; otherwise false.
      }
    class function ParseUNC(const KeyName: TJwString;
      out Server, SubKey: TJwString; out aRootTuple: TJwRootTuple): boolean;
      virtual;

      {<B>GetSecurityDescriptorEx</B> tries to get the security descriptor of a local or remote registry key.
       To connect to a remote registry use a UNC path (\\server\root\subkey) in parameter KeyName.

       @param KeyName receives the key path which security descriptor is to be retrieved. It can be a UNC path that allows
                to retrieve a security descriptor of a key on a remote machine. For more information see ParseUNC  
       @param aSecurityInfo defines which security information in the security descriptor is to be retrieved.
       @param bUseWOW64 is only used on the local machine (if KeyName is not a remote UNC path)
                and defines the constant SE_REGISTRY_WOW64_32KEY that indicates an object for a registry entry under WOW64.
                Set it to true to use it; otherwise false. 
       @return Returns a new instance of the security descriptor. The caller is responsible for freeing it.

       raises
 EJwsclInvalidParameterException:  will be raised if parameter KeyName is invalid. Validity is only checked for a correct root item. 
        EJwsclWinCallFailedException: will be raised if a call to a winapi function failed. To get the name of the function that failed,
                use the property WinCallName of the exception. It contains the name of the function that failed. 

      }
    class function GetSecurityDescriptorEx(const KeyName: TJwString;
      const aSecurityInfo: TJwSecurityInformationFlagSet;
      bUseWOW64: boolean): TJwSecurityDescriptor; virtual;

      {<B>SetSecurityDescriptorEx</B> tries to set the security descriptor of a local or remote registry key.
       To connect to a remote registry use a UNC path (\\server\root\subkey) in parameter KeyName.

       @param KeyName receives the key path which security descriptor is to be set. It can be a UNC path that allows
                to set a security descriptor of a key on a remote machine. For more information see ParseUNC  
       @param aSecurityInfo defines which security information in the security descriptor is to be changed.
       @param aSecurityDescriptor defines a security descriptor which is used to set the security information of the key. It must not be nil.
       @param bUseWOW64 is only used on the local machine (if KeyName is not a remote UNC path)
                and defines the constant SE_REGISTRY_WOW64_32KEY that indicates an object for a registry entry under WOW64.
                Set it to true to use it; otherwise false. 

       raises
         EJwsclInvalidSecurityDescriptor:  will be raised if the parameter aSecurityDescriptor is nil
         EJwsclInvalidParameterException: will be raised if parameter KeyName is invalid. Validity is only checked for a correct root item.
         EJwsclWinCallFailedException: will be raised if a call to a winapi function failed. To get the name of the function that failed,
                use the property WinCallName of the exception. It contains the name of the function that failed. 

      }
    class procedure SetSecurityDescriptorEx(const KeyName: TJwString;
      const aSecurityInfo: TJwSecurityInformationFlagSet;
      const aSecurityDescriptor: TJwSecurityDescriptor;
      bUseWOW64: boolean); virtual;

      {<B>SetSecurityDescriptor</B> sets the security descriptor of the registry key.
       It simply calls all SetXXX methods if defined in SD_entries.
       If a entry of the SD cannot be set an exception is raised and the rest is dismissed.
       However all entries that were successfully set before the exception are stored into the key security.

       The following values are ignored in SD_entries:
       
        # siprotectedDaclSecurityInformation 
        # siUnprotectedDaclSecurityInformation 
        # siprotectedSaclSecurityInformation 
        # siUnprotectedSaclSecurityInformation 

       Instead use TJwSecurityDescriptor.InheritanceDACLProtection or
         TJwSecurityDescriptor.InheritanceSACLProtection to control inheritance.

       @param SD defines the security descriptor to be set. It must not nil otherwise EJwsclInvalidParameterException is raised.
       @param SD_entries The following security descriptor flags are supported
                  siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation, siSaclSecurityInformation

       @param EJwsclNILParameterException will be raised if parameter SD is nil.

       }
    procedure SetSecurityDescriptor(const SD: TJwSecurityDescriptor;
      const SD_entries: TJwSecurityInformationFlagSet); overload; override;

       {<B>GetSecurityDescriptor</B> returns a security descriptor of the file with specified SD entries given in parameter SD_entries.
        @param SD_entries The following security descriptor flags are supported
                  siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation, siSaclSecurityInformation

      }
    function GetSecurityDescriptor(
      const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
      overload; override;

       {<B>AccessCheck</B> checks whether the user has access to the the file of the instance or not.
       @param DesiredAccess defines which access rights are to be checked. If set to High(Cardinal)
          the access rights given in the constructor are used.

       Exceptions
        EJwsclInvalidParameterException: This exception will be raised if DesiredAccess contains the
            MAXIMUM_ALLOWED access right. If you want to check for granted access rights call
            the other method and read parameter GrantedAccess.
      }
    function AccessCheck(DesiredAccess: TJwAccessMask = Cardinal(-1);
      const ClientToken: TJwSecurityToken = nil)
      : boolean;
      overload; override;

    { <b>AccessCheck</b> checks the access to a security descriptor of a secure
      object. See AccessCheck in MSDN for more information
      Parameters
      SecurityDescriptor :  Contains the security descriptor that is used to check for
                            access.
      ClientToken :         A token that is used to get the SID and privileges which
                            are used to check against the security descriptor. The
                            parameter can be nil to use the current thread or process
                            token.<p /><p />In contrast to the original AccessCheck API
                            function, this method automatically adjusts the token type
                            to "impersonation" if the given token is a primary one.
                            This prevents the error ERROR_NO_IMPERSONATION_TOKEN
                            (1309). If ClientToken is not nil the method makes a copy
                            of ClientToken and converts it to a thread token. If
                            ClientToken is nil, it retrieves the current thread or
                            primary token (if no thread token is available) and, in the
                            latter case, converts it to a thread token. Furthermore
                            this method does not impersonate any token and thus leaves
                            an already existing thread token intact.<p />
      DesiredAccess :       Defines the desired access to the object.<p /><b>New</b><p />Although
                            the MSDN AccessCheck forbids generic rights (like
                            GENERIC_ALL) in this Parameter. The method AccessCheck will
                            replace all generic rights with specific rights using the
                            mapping defined by parameter GenericMapping. However the
                            original SecurityDescriptor will remain the same.<p /><b>Warning</b><p />Some
                            generic access rights may overlap. This can lead to access
                            denied.<p /><b>Example</b><p />DACL contains a positive ACE
                            with GENERIC_WRITE and a negative ACE with GENERIC_READ A
                            call to AccessCheck with DesiredAccess set to GENERIC_WRITE
                            and GenericMapping set to TJwSecurityFileMapping (using
                            FileGenericMapping) will fail because GENERIC_WRITE and
                            GENERIC_READ are resolve to FILE_GENERIC_WRITE and
                            FILE_GENERIC_READ which both contain SYNCHRONIZE ($100000)
                            and READ_CONTROL ($2000).<p />
      GenericMapping :      Receives a class type of the class
                            TJwSecurityGenericMapping or one of her derived classes. If
                            the generic class TJwSecurityGenericMapping is used, all
                            generic access rights are mapped to standard access rights
                            (STANDARD_RIGHTS_READ...STANDARD_RIGHTS_ALL). Use only
                            access rights in parameter DesiredAccess that are mapped by
                            the given TJwSecurityGenericMappingClass class; otherwise
                            AccessChec will fail with EJwsclWinCallFailedException.
                            E.g. TJwSecurityGenericMappingClass can be used with
                            DesiredAccess set to STANDARD_RIGHTS_ALL.<p /><p /><b>New</b><p />All
                            access entriey (ACEs) in the security descriptor DACL are
                            scanned for GENERIC access rights (like GENERIC_ALL) and
                            automatically converted to specific rights using parameter
                            GenericMapping. However the original SecurityDescriptor
                            will remain the same. This will not happen, if nil is
                            supplied to this parameter. Make sure there are no generic
                            rights in the ACL or DesiredAccess parameter.<p />
      PrivilegeSet :        Receives the privileges that are used for access check. If
                            none are used, this output will be nil. The caller is
                            responsible for destroying the object!
      GrantedAccess :       Receives an access mask that indicates which rights were
                            granted.
      AccessStatus :        Receives the result of the access check. True if access is
                            granted, otherwise false.
      Exceptions
      EJwsclInvalidParameterException :  will be raised if parameter SecurityDescriptor
                                         is nil;
      EJwsclWinCallFailedException :     will be raised if the call to AccessCheck
                                         failed.
      EJwsclInvalidOwnerSIDException :   will be raised if the owner of the security
                                         descriptor is nil. Use JwNullSID to remove
                                         influence of owner to AccessCheck call.
      EJwsclInvalidGroupSIDException :   will be raised if the group of the security
                                         descriptor is nil. Use JwNullSID to remove
                                         influence of group to AccessCheck call.                          }
    class procedure AccessCheck(
      const SecurityDescriptor: TJwSecurityDescriptor;
      const ClientToken: TJwSecurityToken;
      const DesiredAccess: TJwAccessMask;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean);
      overload; virtual;

      {<B>AccessCheckAndAuditAlarm</B>.
       See http://msdn2.microsoft.com/en-us/library/aa374823.aspx
        for more information.

       Not tested yet! }
    class procedure AccessCheckAndAuditAlarm(
      const SubsystemName: TJwString;
      const HandleID: Pointer;
      const ObjectTypeName,
      ObjectName: TJwString;
      const SecurityDescriptor:
      TJwSecurityDescriptor;
      const DesiredAccess: TJwAccessMask;
      const GenericMapping:
      TJwSecurityGenericMappingClass;
      const ObjectCreation: boolean;
      out GrantedAccess: TJwAccessMask;
      out AccessStatus: boolean;
      out pfGenerateOnClose: boolean);
      overload; override;


      {<B>TreeResetNamedSecurityInfo</B> sets the securiy information of a folder and all its children.
        See MSDN for more information http://msdn2.microsoft.com/en-us/library/aa965849.aspx.
        This function needs Windows XP or higher.

        This method can restore the security information for a locked out user without using SeBackupPrivilege privilege.
        It uses a highly privileged system accout to do this.

        Both callback methods can be used at the same time. If both are not nil, they are called in the following order :
         FNProgressMethod, FNProgressProcedure. Exceptions are caught in these calls.

        @param aSecurityInfo defines which security information to be set. (Owner...SACL). If a flag is not set, but the
                parameter Owner...SACL is not nil, it is also set. 
        @param FNProgressMethod defines a object method to be called if a file or folder is changed. Can be nil. See TJwFnProgressMethod  
        @param FNProgressProcedure defines a procedure to be called if a file or folder is changed. Can be nil. See TJwFnProgressProcedure  
        @param ProgressUserData defines user data to be used as parameter ProgressUserData in the callback methods. 

        raises
          EJwsclWinCallFailedException:  is raised if the winapi call failed.
          EJwsclUnsupportedWindowsVersionException: This is exception is raised if the Windows system does not support this call.
        }
    class procedure TreeResetNamedSecurityInfo(
      const RootKey: TJwRootRegKey;
      pObjectName: TJwString;
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL:
      TJwDAccessControlList;
      const SACL:
      TJwSAccessControlList;
      const bKeepExplicit: boolean;
      const fnProgress:
      TJwProgInvokeSetting;
      const FNProgressMethod: TJwFnProgressMethod;
      const FNProgressProcedure:
      TJwFnProgressProcedure;
      const ProgressUserData: Pointer); reintroduce; virtual;

       {<B>TreeKeySetNamedSecurityInfo</B> sets or resets the security information of a registry key. The process can be observed by a callback method or function.
        This method can be run in a seperate thread so that it immediately returns.

        This method cannot restore the security information for a locked out user without using SeBackupPrivilege privilege.

        @param pKeyName defines the key which structure is about to be set or set.
                The key path can also be a UNC key name. See ParseUNC  for more information about UNC.
                 
        @param aSecurityInfo defines the type of security (DACL, SACL, owner, group) and also inheritance protection to be set.
                The ACEs in DACL and/or SACL is set once in the root key and inherited to the children based on the ACE flags. 
        @param Action defines in which case the callback method/function is called.
                  The following constants are supported
                   
                    # pis_ProgressInvokeNever  The callback method should not be called 
                    # pis_ProgressInvokeEveryObject The callback method is called for all objects. Including the finished callback. 
                    # pis_ProgressInvokeOnError The callback method is called for all errors. Including the finished callback. 
                    
                  The finish callback is called after the security of the last key was changed. The last callback
                  uses the parameter pObjectName and the constant pis_ProgressFinished in parameter pInvokeSetting. 

        @param SetType defines the behaviour of this function.

                 # tstSet Simply sets the security setting defined in DACL, SACL, owner or group based on the inheritance flags of the given ACEs.
                 # tstReset Removes all inheritance protection and ACEs from DACL and SACL of the given tree and sets the security settings like tstSet.
                       The explicit ACEs are preserved if parameter bKeepExplicit is set to true.




        @param bKeepExplicit Only applies to tstReset in parameter SetType. It preserves the explicit ACEs of DACL and SACL of root key and all children from deletion.
        @param Owner Defines the Owner SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siOwnerSecurityInformation.
                        The instance is copied into a new object.
        @param Group Defines the Group SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siGroupSecurityInformation.
                        The instance is copied into a new object.
        @param DACL Defines the DACL SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siDaclSecurityInformation.
                Do not set DACL to nil and include it into aSecurityInfo. A nil DACL cannot be inherited to children. Use instead a GENERIC_ALL with a JwWorldSID.
                The instance is copied into a new object.

        @param SACL Defines the SACL SID to be set. Can be nil. This parameter is used if aSecurityInfo contains siSaclSecurityInformation.
                The instance is copied into a new object.
        @param FNProgressMethod defines a callback method. It can be nil. This callback function is called before FNProgressProcedure.
        @param FNProgressProcedure defines a callback procedure. It can be nil. This callback function is called after FNProgressMethod.
        @param ProgressUserData defines user data that is directed to the callback method

        @param aThread defines a thread instance that is used to call this function in a seperate thread. To use <B>TreeKeySetNamedSecurityInfo</B> in a thread
                 simply create the thread (aThread := TJwTagThread.Create) and assign it to the parameter aThread.
                 Do not call Create with a parameter.

        @param Disable64Redirection defines whether the 64bit redirection of keys for 32bit application is deactivated (TRUE) or activated (FALSE).
                        This parameter only works on 64bit windows plattforms. If the plattform is not a 64bit and
                        Disable64Redirection is true, nothing will happen!


       }
    class procedure TreeKeySetNamedSecurityInfo(pKeyName: TJwString;
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      const Action:
      TJwProgInvokeSetting;
      const SetType: TJwTreeSetType;
      const bKeepExplicit: boolean;
      const Owner: TJwSecurityId;
      const Group: TJwSecurityId;
      const DACL:
      TJwDAccessControlList;
      const SACL:
      TJwSAccessControlList;
      const FNProgressMethod:
      TJwFnProgressMethod;
      const FNProgressProcedure: TJwFnProgressProcedure;
      const ProgressUserData
      : Pointer;
      const aThread: TJwTagThread;
      const Disable64Redirection:
      boolean = False); virtual;

      {<B>GetInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       See MSDN http://msdn2.microsoft.com/en-us/library/aa446640.aspx.

       @return The return value is an array of TJwInheritedFromRecord that maps the winapi struct
        INHERITED_FROM to a Delphi type. The function FreeInheritedFromArray must not be called therefor,
        instead use SetLength(result,0);
        The member SID is not used here.


       This function does not work in all version of Windows XP. If so the
       exception EJwsclInheritanceSourceNotSupportedException is raised.

       }
    function GetInheritanceSource(
      const pObjectName: TJwString;
    //LPTSTR pObjectName,
      const aSecurityInfo:
      TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
      const bIsDirectory: boolean;
    //BOOL Container,
      const GUIDs: TJwGuidArray;
    //GUID** pObjectClassGuids,
      const FN_OBJECT_MGR_FUNCTS: Pointer =
      nil): TJwInheritedFromArray;
    {PINHERITED_FROM pInheritArray} reintroduce; virtual;

      {<b>BUGBUGBUG - DO NOT USE!</b>

       <B>GetKeyInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       This method simulates GetInheritanceSource, so that it can be used in all windows versions with ACL support.

       <B>GetKeyInheritanceSource</B> supports connection to a remote registry. To use a remote connection set RootKey to rrkString and use
       a fully quallified key path (like '\\\\computer\\USERS', '\\\\computer\\USERS\\S-1-5-18\\Control Panel\\Current').
       A remote connection with another constant like rrkString is not supported.
        <B>GetKeyInheritanceSource</B> uses internally RegConnectRegistry. See http://msdn2.microsoft.com/en-us/library/ms724840.aspx for more information.

       @param RootKey defines the type or root key which is used. It can be on of the following values.
                
                 # rrkString 
                 # rrkLocalMachine 
                 # rrkCurrentUser 
                 # rrkUsers 
                 # rrkCurrentConfig
                 # rrkClassesRoot
                 
            rrkString defines that the root key is located in the parameter KeyName as a string. 

       @param aSecurityInfo defines the type of security inheritance is to be obtained. The value can be one of siDaclSecurityInformation or siSaclSecurityInformation.

       @return The return value is an array of TJwInheritedFromRecord with the count of ACE in the DACL of the object in PathName
                Each array entry consists of
                 
                  # GenerationGap Levels between the object and the ancestor 
                  # AncestorName Name of the ancestor from which the ACE was inherited 
                  # SID The SID that is inherited. The string has the type <pre><SID account name>@<S-X-X...></pre> 
                 
       raises
         EJwsclInvalidPathException:  will be raised if the instance is created with a handle instead of a file or key name.
         EJwsclInvalidObjectException: will be raised if the object could not be accessed or found.
         EJwsclInvalidParameterException: will be raised if aSecurityInfo is not [siDaclSecurityInformation] or [siSaclSecurityInformation]
         EJwsclPrivilegeNotFoundException: will be raised if aSecurityInfo is [siSaclSecurityInformation] and the current thread does cannot access audit information, because the privilege could not be activated.

       }
    class function GetKeyInheritanceSource(const RootKey: TJwRootRegKey;
      const KeyName: TJwString; const bUseWOW64: boolean = False;
      const aSecurityInfo: TJwSecurityInformationFlagSet =
      [siDaclSecurityInformation]): TJwInheritedFromArray; overload; virtual;

      {<B>GetKeyInheritanceSource</B> retrieves the source if inheritance for the ACEs in the ACL of the given object.
       This method simulates GetInheritanceSource, so that it can be used in all windows versions with ACL support.

       @return The return value is an array of TJwInheritedFromRecord with the count of ACE in the DACL of the object in PathName
                Each array entry consists of
                 
                  # GenerationGap Levels between the object and the ancestor 
                  # AncestorName Name of the ancestor from which the ACE was inherited 
                  # SID The SID that is inherited. The string has the type <pre><SID account name>@<S-X-X...> </pre>
                 
       raises
         EJwsclInvalidPathException:  will be raised if the instance is created with a handle instead of a file or folder name.
         EJwsclInvalidObjectException: will be raised if the object could not be accessed or found.
      }
    function GetKeyInheritanceSource(const bUseWOW64: boolean = False;
      const aSecurityInfo: TJwSecurityInformationFlagSet =
      [siDaclSecurityInformation]): TJwInheritedFromArray; overload; virtual;


      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file handle;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL 

       See SetNamedSecurityInfo for exceptions.
      }
    class procedure RemoveInheritanceFlow(const Handle: THandle;
      const bCopyInheritedACEs: boolean = True); overload; virtual;


      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file or folder pathname;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL

       See SetNamedSecurityInfo for exceptions.
      }
    class procedure RemoveInheritanceFlow(const KeyName: TJwString;
      const bCopyInheritedACEs: boolean = True); overload; virtual;

      {<B>RemoveInheritanceFlow</B> removes the inheritance flow of a file or folder pathname;
       @param bCopyInheritedACEs defines whether the inherited ACEs shall be copied (true) or removed (false). If the DACL
                contains no explicit ACEs and the inherited ACEs are removed (false) the remaining DACL is a deny everybody DACL

      raises
        EJwsclInvalidObjectException:  will be raised if the handle and filename is invalid.
      Seealso
        See SetNamedSecurityInfo for more exceptions.
      }
    procedure RemoveInheritanceFlow(
      const bCopyInheritedACEs: boolean = True); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
        <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL.If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised.

       raises
         EJwsclAdaptSecurityInfoException:  will be raised if bTakeOwnerShip is true and the ownership cannot be taken.
       Seealso
         See SetNamedSecurityInfo for more exceptions.
      }
    class procedure RestoreInheritanceFlow(const Handle: THandle;
      bTakeOwnerShip: boolean = False); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
        <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL.If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised.


       @param PathName defines a file or folder path.

       raises
         EJwsclAdaptSecurityInfoException:  will be raised if bTakeOwnerShip is true and the ownership cannot be taken.
       Seealso
        See SetNamedSecurityInfo for more exceptions.
      }
    class procedure RestoreInheritanceFlow(const KeyName: TJwString;
      bTakeOwnerShip: boolean = False); overload; virtual;

      {<B>RestoreInheritanceFlow</B> restores the inheritance flow of the file object.
       ACEs are inherited to the file or folder.
       <B>RestoreInheritanceFlow</B> tries to obtain ownership if bTakeOwnerShip is true and the access is not granted
       without ownership. If the action failed the exception EJwsclAdaptSecurityInfoException is raised.

       @param bTakeOwnerShip Defines whether the function should try to restore inheritance by taking the ownership and
                get so enough rights to restore the DACL. If false and the right to change DACL is denied the exception
                 EJwsclWinCallFailedException is raised.

       raises
         EJwsclInvalidObjectException:  will be raised if the handle and filename is invalid.
         EJwsclAdaptSecurityInfoException: will be raised if bTakeOwnerShip is true and the ownership cannot be taken.
       Seealso
         See SetNamedSecurityInfo for more exceptions.
      }
    procedure RestoreInheritanceFlow(bTakeOwnerShip: boolean = False);
      overload; virtual;

      {Name takes the ownership of a file or folder. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param PathName Defines the file or folder which owner is to be changed
       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exceptions
       }
    class procedure TakeOwnerShip(const KeyName: TJwString;
      SID: TJwSecurityId = nil); overload; virtual;

      {Name takes the ownership of a file or folder. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param Handle Defines a handle to the file or folder which owner is to be changed
       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    class procedure TakeOwnerShip(const Handle: THandle;
      SID: TJwSecurityId = nil); overload; virtual;


      { <b>CheckKeyNameValidity</b> checks if a given KeyName is a correct key path. The
        keyname is correct if it has the following structure: <c>"\\serverrootsubkey"
        (UNC)</c>
        
        or
        
        <c>"rootsubkey" (standard)</c>
        
        
        
        If the keyname is incorrect an exception is raised.
        
        
        Parameters
        KeyName :  defines the keyname to be checked.
        Exceptions
        EJwsclInvalidParameterException :  is raised if the KeyName is empty.
        EJwsclInvalidKeyPath :             is raised if the root key is not present. The
                                           root name can be a string from the RootName
                                           member of JwKeyRootTupleArray                 }
    class procedure CheckKeyNameValidity(const KeyName: TJwString); virtual;

      {Name takes the ownership of a file or folder used by this instnace. If the current has the SE_TAKE_OWNERSHIP_NAME
       available it is made active automatically.
       The owner is set to to current owner or given SID.
       To set the owner to an arbitrary one the caller must enable SE_RESTORE_PRIVILEGE. The function doesnt do it.

       @param SID defines the new owner. If nil the current user is used.

       raises
         EJwsclSecurityException:  See SetNamedSecurityInfo for more exception
       }
    procedure TakeOwnerShip(SID: TJwSecurityId = nil); overload; virtual;

  end;

  {@exclude}
  TJwSecureObjectType = (
    sot_Service,
    sot_Printer,
    sot_LMShare,

    //a kernel object if not specified, instead use sot_KO_XXXX
    sot_Kernel_Object,
    //sot_Kernel_object
    sot_KO_Semaphore,
    sot_KO_Event,
    sot_KO_Mutex,
    sot_KO_WaitableTimer,
    sot_KO_FileMapping,
    sot_KO_Heap,
    sot_KO_Job,
    sot_KO_Socket,
    sot_KO_Thread,
    sot_KO_Timer,
    sot_KO_Token,
    //sot_Window_Object
    sot_Desktop,
    sot_WindowStation,
    sot_DS_Object,
    sot_DS_Object_All,
    sot_PROVIDER_DEFINED_OBJECT,
    sot_WMIGUID_OBJECT,
    sot_REGISTRY_WOW64_32KEY);



 {  TJwSecureObject = class(TJwSecureBaseClass)
   protected
     fHandle : THandle;
     fName   : TJwString;

     fOwner : TJwSecurityId;
     fGroup : TJwSecurityId;
     fDACL  : TJwDAccessControlList;
     fSACL  : TJwSAccessControlList;

     fSecurityDescriptor : TJwSecurityDescriptor;
   public
 (*    constructor Create(Handle : THandle; aSecureObjectType : TJwSecureObjectType); overload;
     constructor Create(ObjectName : TJwString; aSecureObjectType : TJwSecureObjectType); overload; //nur bestimmte typen zulassen

     destructor Destroy;

     property Owner : TJwSecurityId;
     property Group : TJwSecurityId;
     property DACL  : TJwDAccessControlList;
     property SACL  : TJwSAccessControlList;

     property SecurityDescriptor : TJwSecurityDescriptor;

     function ObjectTypeNameToString(const aSecureObjectType : TJwSecureObjectType) : TJwString;



     property Handle       : THandle read fHandle;
     property Name   : THandle read fName;
     property TypeName : TJwString; //read ObjectTypeNameToString
   *)
   end;   }

(*

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureWindowObject = class(TJwSecureBaseClass)
   protected
      //procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecurePrinter = class(TJwSecureBaseClass)
   protected
      //procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureService = class(TJwSecureBaseClass)
   protected
      //procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureLMShare = class(TJwSecureBaseClass)
   protected
      //procedure Dummy; virtual; abstract;
   end;

   //SetNamedSecurityInfo

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureMutex = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureSemaphore = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureEvent = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureWaitableTimer = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureFileMapping = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   //SetSecurityInfo
   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureProcess = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureThread = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureWindowStationObject = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;

   {<B>JwsclEnumerations</B> is not implemented yet}
   TJwSecureDesktopObject = class(TJwSecureBaseClass)
   protected
      procedure Dummy; virtual; abstract;
   end;
*)


 //TSecure = class(TJwSecureBaseClass)
 //end;

//var CacheList : TStringList;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses TypInfo,
     JwsclProcess,
     JwsclPrivileges,
     JwsclEnumerations;

{$ENDIF SL_OMIT_SECTIONS}



{$IFNDEF SL_INTERFACE_SECTION}


const NOERROR = 0;



function ExpandFileName(FileName: TJwString): TJwString;

{$IFDEF DELPHI2009_UP}
begin
  Result := SysUtils.ExpandFileName(FileName);
end;
{$ELSE}

{$IFNDEF UNICODE}
begin
  Result := SysUtils.ExpandFileName(FileName);
end;

{$ELSE}
var
  FName: PWideChar;
  Buffer: PWideChar;
  len : Cardinal;
begin
  result := FileName;

  //just check the necessary space for the filename
  Buffer := nil;
  len := GetFullPathNameW(PWideChar(WideString(FileName)), 0, Buffer, FName);

  if Len = 0 then
    exit;

  Buffer := PWideChar(LocalAlloc(LPTR, (5+len)*sizeof(TJwChar)));

  GetFullPathNameW(PWideChar(WideString(FileName)), len, Buffer, FName);

  result := TJwString(Buffer);
  LocalFree(HLOCAL(Buffer));
end;
{$ENDIF}
{$ENDIF DELPHI2009_UP}

constructor TJwSecureBaseClass.Create;
begin
  inherited;
  fHandle := 0;

  fDAccessControlList := nil;
  fSAccessControlList := nil;
  fOwner := nil;
  fGroup := nil;

end;


destructor TJwSecureBaseClass.Destroy;
begin
  fHandle := 0;


  FreeAndNil(fDAccessControlList);
  FreeAndNil(fSAccessControlList);
  FreeAndNil(fOwner);
  FreeAndNil(fGroup);

  inherited;
end;

function TJwSecureBaseClass.HasValidHandle: boolean;
begin
  Result := (fHandle <> 0) and (fHandle <> Cardinal(-1));
end;




class procedure TJwSecureBaseClass.SetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const anOwner: TJwSecurityId;
  const anGroup: TJwSecurityId;
  const aDACL: TJwDAccessControlList;
  const aSACL: TJwSAccessControlList);
var
  Res: HRESULT;

  OwnerPSid, GroupPSid: PSid;
  pDACL, pSACL: PACL;

begin

  OwnerPSID := nil;
  GroupPSID := nil;
  pDACL := nil;
  pSACL := nil;

  if Assigned(anOwner) then
  begin
    if not (siOwnerSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siOwnerSecurityInformation);

    //TODO: check for WRITE_OWNER access as described in http://msdn2.microsoft.com/en-us/library/aa379579.aspx for parameter psidOwner

    (*Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    if not Token.PrivilegeAvailable[SE_TAKE_OWNERSHIP_NAME] then
    begin
      raise ESetOwnerException.CreateFmtEx('Cannot set .','Free',ClassName,'JwsclKnownSid.pas',0,false,[]);
    end;*)

    OwnerPSID := PSid(anOwner.SID);
  end;

  if Assigned(anGroup) then
  begin
    if not (siGroupSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siGroupSecurityInformation);

    //TODO: check for WRITE_OWNER access as described in http://msdn2.microsoft.com/en-us/library/aa379579.aspx for parameter psidOwner

    GroupPSID := PSid(anGroup.SID);
  end;

  if Assigned(aDACL) then
  begin
    if not (siDaclSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siDaclSecurityInformation);

    pDACL := PACL(aDACL.Create_PACL);
  end;

  if Assigned(aSACL) then
  begin
    if not (siSaclSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siSaclSecurityInformation);

    pSACL := PACL(aSACL.Create_PACL);
  end;



  Res := JwaWindows.SetSecurityInfo(aHandle, aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), jwaWindows_PSID(OwnerPSID),
    jwaWindows_PSID(GroupPSID), jwaWindows_PACL(pDACL),
    jwaWindows_PACL(pSACL));

  if pDACL <> nil then
    TJwSecurityAccessControlList.Free_PACL(jwaWindows.PACL(pDACL));

  if pSACL <> nil then
    TJwSecurityAccessControlList.Free_PACL(jwaWindows.PACL(pSACL));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus, 'SetNamedSecurityInfo',
      ClassName, RsUNSecureObjects, 0, res, ['SetSecurityInfo',res]);
end;




class procedure TJwSecureBaseClass.SetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo:
  TJwSecurityInformationFlagSet;
  const anOwner: TJwSecurityId;
  const anGroup: TJwSecurityId;
  const aDACL: TJwDAccessControlList;
  const aSACL: TJwSAccessControlList);
var
  Res: HRESULT;

  OwnerPSid, GroupPSid: PSid;
  pDACL, pSACL: PACL;

  //[Hint] Token : TJwSecurityToken;
begin

  OwnerPSID := nil;
  GroupPSID := nil;
  pDACL := nil;
  pSACL := nil;

  if Assigned(anOwner) then
  begin
    if not (siOwnerSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siOwnerSecurityInformation);

    //TODO: check for WRITE_OWNER access as described in http://msdn2.microsoft.com/en-us/library/aa379579.aspx for parameter psidOwner

    (*Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    if not Token.PrivilegeAvailable[SE_TAKE_OWNERSHIP_NAME] then
    begin
      raise ESetOwnerException.CreateFmtEx('Cannot set .','Free',ClassName,'JwsclKnownSid.pas',0,false,[]);
    end;*)

    OwnerPSID := PSid(anOwner.SID);
  end;

  if Assigned(anGroup) then
  begin
    if not (siGroupSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siGroupSecurityInformation);

    //TODO: check for WRITE_OWNER access as described in http://msdn2.microsoft.com/en-us/library/aa379579.aspx for parameter psidOwner

    GroupPSID := PSid(anGroup.SID);
  end;

  if Assigned(aDACL) then
  begin
    if not (siDaclSecurityInformation in aSecurityInfo) then
      Include(aSecurityInfo, siDaclSecurityInformation);

    pDACL := PACL(aDACL.Create_PACL);
  end;

  if Assigned(aSACL) then
  begin
    pSACL := PACL(aSACL.Create_PACL);
  end;



  Res :=
{$IFDEF UNICODE}SetNamedSecurityInfoW{$ELSE}
    SetNamedSecurityInfoA
{$ENDIF}
    (TJwPChar(anObjectName), aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), jwaWindows_PSID(OwnerPSID),
    jwaWindows_PSID(GroupPSID), jwaWindows_PACL(pDACL),
    jwaWindows_PACL(pSACL));

  if pDACL <> nil then
    TJwSecurityAccessControlList.Free_PACL(jwaWindows.PACL(pDACL));

  if pSACL <> nil then
    TJwSecurityAccessControlList.Free_PACL(jwaWindows.PACL(pSACL));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus,
      'SetNamedSecurityInfoW', ClassName, RsUNSecureObjects, 0, res,
        ['SetNamedSecurityInfo',res]);
end;




class procedure TJwSecureBaseClass.GetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  out anOwner: TJwSecurityId;
  out anGroup: TJwSecurityId;
  out aDACL: TJwDAccessControlList;
  out aSACL: TJwSAccessControlList);
var
  Res: HRESULT;
  OwnerPSid, GroupPSid: PSid;
  pDACL, pSACL: PACL;

  pSDesc: PSecurityDescriptor;

begin
  pSDesc := nil;

  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  OwnerPSID := nil;
  GroupPSID := nil;
  pDACL := nil;
  pSACL := nil;



  Res := JwaWindows.GetSecurityInfo(aHandle,
    aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo),
    @OwnerPSid,
    @GroupPSid,
    @pDACL, @pSACL,
    jwaWindows_PSecurity_Descriptor(pSDesc));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus,
      'GetSecurityInfo',ClassName, RsUNSecureObjects, 0, res,
      ['GetSecurityInfo',res]);

  if OwnerPSid <> nil then
  begin
    try
      anOwner := TJwSecurityId.Create(jwaWindows.PSid(OwnerPSid));
    finally
    end;
  end;

  if GroupPSID <> nil then
  begin
    try
      anGroup := TJwSecurityId.Create(jwaWindows.PSid(GroupPSID));
    finally
    end;
  end;

  if pDACL <> nil then
  begin
    try
      aDACL := TJwDAccessControlList.Create(jwaWindows.PACL(pDACL));
    finally
    end;
  end;

  if pSACL <> nil then
  begin
    try
      aSACL := TJwSAccessControlList.Create(jwaWindows.PACL(pSACL));
    finally
    end;
  end;

  if pSDesc <> nil then
    LocalFree(HRESULT(pSDesc));

end;

class function TJwSecureBaseClass.GetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet
  )
: {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
TJwSecurityDescriptor;

var
  Res: HRESULT;
  //[Hint] OwnerPSid,
  //[Hint] GroupPSid : PSid;
  //[Hint] pDACL,
  //[Hint] pSACL : PACL;

  pSDesc: jwaWindows.PSecurityDescriptor;

begin
  pSDesc := nil;


  //[Hint] OwnerPSID := nil;
  //[Hint] GroupPSID := nil;
  //[Hint] pDACL     := nil;
  //[Hint] pSACL     := nil;

  Res := JwaWindows.GetSecurityInfo(aHandle,
    aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo),
    nil, nil,
    nil, nil,
    jwaWindows_PSecurity_Descriptor(pSDesc));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus,
      'GetSecurityInfo',
      ClassName, RsUNSecureObjects, 0, res,
       ['GetSecurityInfo',res]);

  try
    Result := TJwSecurityDescriptor.Create(
      jwaWindows.PSecurityDescriptor(pSDesc));
  finally
    LocalFree(HRESULT(pSDesc));
  end;
end;



class procedure TJwSecureBaseClass.GetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  out anOwner: TJwSecurityId;
  out anGroup: TJwSecurityId;
  out aDACL: TJwDAccessControlList;
  out aSACL: TJwSAccessControlList);
var
  Res: HRESULT;
  OwnerPSid, GroupPSid: PSid;
  pDACL, pSACL: PACL;

  pSDesc: PSecurityDescriptor;

begin
  pSDesc := nil;


  OwnerPSID := nil;
  GroupPSID := nil;
  pDACL := nil;
  pSACL := nil;

  Res :=
        {$IFDEF UNICODE}JwaWindows.GetNamedSecurityInfoW{$ELSE}
    JwaWindows.GetNamedSecurityInfoA
{$ENDIF}
    (TJwPChar(anObjectName),
    aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo),
    @OwnerPSid,
    @GroupPSid,
    @pDACL, @pSACL,
    jwaWindows_PSecurity_Descriptor(pSDesc));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus,
      'GetNamedSecurityInfo', ClassName, RsUNSecureObjects, 0, res,
      ['GetNamedSecurityInfo',res]);

  if OwnerPSid <> nil then
  begin
    try
      anOwner := TJwSecurityId.Create(jwaWindows.PSid(OwnerPSid));
    finally
    end;
  end;

  if GroupPSID <> nil then
  begin
    try
      anGroup := TJwSecurityId.Create(jwaWindows.PSid(GroupPSID));
    finally
    end;
  end;

  if pDACL <> nil then
  begin
    try
      aDACL := TJwDAccessControlList.Create(jwaWindows.PACL(pDACL));
    finally
    end;
  end;

  if pSACL <> nil then
  begin
    try
      aSACL := TJwSAccessControlList.Create(jwaWindows.PACL(pSACL));
    finally
    end;
  end;

  if pSDesc <> nil then
    LocalFree(HRESULT(pSDesc));

end;

class function TJwSecureBaseClass.GetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet
  )
: {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
TJwSecurityDescriptor;
var
  Res: HRESULT;
  //[Hint] OwnerPSid,
  //[Hint] GroupPSid : PSid;
  //[Hint] pDACL,
  //[Hint] pSACL : PACL;

  pSDesc: PSecurityDescriptor;
  Privs : IJwPrivilegeScope;
begin
  pSDesc := nil;


  {This is a bug in Vista.
   GetNamedSecurityInfo only retrieves the integrity level information
   if siSaclSecurityInformation is used and privilege SE_SECURITY_NAME is active.
   In the other way around:
    SetNamedSecurity does not need the SE_SECURITY_NAME privilege
  }
  if (aObjectType = SE_SERVICE) and
     (siLabelSecurityInformation in aSecurityInfo) and
     not (siSaclSecurityInformation in aSecurityInfo) and
     ((TJwWindowsVersion.IsWindowsVista(false) or
      (TJwWindowsVersion.IsWindows2008(false)))
     ) then
  begin
    try
      Privs := JwGetPrivilegeScope([SE_SECURITY_NAME]);
      Include(aSecurityInfo, siSaclSecurityInformation);
    except
      on E : EJwsclPrivilegeException do
        raise EJwsclPrivilegeException.CreateFmtEx(
              RsSecureObjectsPrivilegeSecurityMissing + #13#10 + RsPrivilegeLabelBug,
              'GetSecurityInfo',
              ClassName, RsUNSecureObjects, 0, false,
              [SE_SECURITY_NAME]);
    end;
  end;


  //[Hint] OwnerPSID := nil;
  //[Hint] GroupPSID := nil;
  //[Hint] pDACL     := nil;
  //[Hint] pSACL     := nil;

  Res :=
{$IFDEF UNICODE}JwaWindows.GetNamedSecurityInfoW{$ELSE}
    JwaWindows.GetNamedSecurityInfoA
{$ENDIF}
    (TJwPChar(anObjectName),
    aObjectType,
    TJwEnumMap.ConvertSecurityInformation(aSecurityInfo),
    nil, nil,
    nil, nil,
    jwaWindows_PSecurity_Descriptor(pSDesc));

  if (Res <> NOERROR) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailedWithNTStatus,
      'GetSecurityInfo',
      ClassName, RsUNSecureObjects, 0, res,
      ['GetNamedSecurityInfo',res]);

  try
    Result := TJwSecurityDescriptor.Create(
      jwaWindows.PSecurityDescriptor(pSDesc));
  finally
    LocalFree(HRESULT(pSDesc));
  end;
end;



class function TJwSecureBaseClass.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  const GenericMapping: TJwSecurityGenericMappingClass): Boolean;
var PrivilegeSet: TJwPrivilegeSet;
    GrantedAccess: TJwAccessMask;
begin
  AccessCheck(
    SecurityDescriptor,//const SecurityDescriptor: TJwSecurityDescriptor;
    ClientToken,//const ClientToken: TJwSecurityToken;
    DesiredAccess,//const DesiredAccess: TJwAccessMask;
    GenericMapping,//const GenericMapping: TJwSecurityGenericMappingClass;
    PrivilegeSet,//out PrivilegeSet: TJwPrivilegeSet;
    GrantedAccess,//out GrantedAccess: TJwAccessMask;
    result//out AccessStatus: boolean
    );
  if Assigned(PrivilegeSet) then
    FreeAndNil(PrivilegeSet);
end;


class procedure TJwSecureBaseClass.PrepareAccessCheckTokenAssignment(
  const ClientToken: TJwSecurityToken;
  out Token : TJwSecurityToken;
  out hToken : TJwTokenHandle);
begin
  Token := nil;
  hToken := INVALID_HANDLE_VALUE;
  try
    if Assigned(ClientToken) then
    begin
      if ClientToken.IsThreadToken then
      begin
        {
        AccessCheck only allows a thread token!
        }
        hToken := ClientToken.TokenHandle;
      end
      else
      begin
        {
        Duplicate the ClientToken and make it a thread token

        ConvertToImpersonatedToken needs TOKEN_READ to work
        }
        Token := TJwSecurityToken.CreateDuplicateExistingToken(ClientToken, TOKEN_READ or TOKEN_IMPERSONATE or TOKEN_DUPLICATE);

        Token.ConvertToImpersonatedToken(
          jwaWindows.TSecurityImpersonationLevel(SecurityImpersonation),
          MAXIMUM_ALLOWED);
        Assert(Token.IsThreadToken, 'Converted client token is not a thread token.');

        hToken := Token.TokenHandle;
      end;
    end
    else
    begin
      {
      Get the primary or thread token.
      If we get a primary token only we convert it to a thread token
       since AccessCheck wants it that way.

      ConvertToImpersonatedToken needs TOKEN_READ to work
      }
      Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or
          TOKEN_IMPERSONATE or TOKEN_DUPLICATE);
  
      if (Token.IsPrimaryToken) then
      begin
        Token.ConvertToImpersonatedToken(
          jwaWindows.TSecurityImpersonationLevel(SecurityImpersonation),
          MAXIMUM_ALLOWED);
      end;

      Assert(Token.IsThreadToken, 'Converted token is not a thread token.');

      hToken := Token.TokenHandle;
    end;
  except
    on E : EJwsclSecurityException do
    begin
      hToken := INVALID_HANDLE_VALUE;
      FreeAndNil(Token);
      raise;
    end;
  end;
end;


class procedure TJwSecureBaseClass.PrepareAccessCheckSecurityDescriptor(
   const SecurityDescriptor: TJwSecurityDescriptor;
   const aClassName : TJwString;
   const Method : TJwString);
begin
  JwRaiseOnNilParameter(SecurityDescriptor, 'SecurityDescriptor',Method,
      ClassName, RsUNToken);

  if not Assigned(SecurityDescriptor.PrimaryGroup) then
    raise EJwsclInvalidGroupSIDException.CreateFmtEx(
      RsSecureObjectsInvalidGroup,
      Method, aClassName, RsUNSecureObjects, 0, False, []);

  if not Assigned(SecurityDescriptor.Owner) then
    raise EJwsclInvalidOwnerSIDException.CreateFmtEx(
      RsSecureObjectsInvalidOwner,
      Method, aClassName, RsUNSecureObjects, 0, False, []);
end;


class procedure TJwSecureBaseClass.PrepareAccessCheckGenericACE(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out bTempSD : Boolean;
  out TempSD  : TJwSecurityDescriptor);

begin
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
end;

class procedure TJwSecureBaseClass.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken;
  const DesiredAccess: TJwAccessMask;
  const GenericMapping:
  TJwSecurityGenericMappingClass;
  out PrivilegeSet: TJwPrivilegeSet;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);

{
Mostly same code in TJwSecureBaseClass.AccessCheckByType
}


var
  pSDesc: jwaWindows.PSecurityDescriptor;

  hToken: TJwTokenHandle;
  pPrivs: PRIVILEGE_SET;
  pPrivsSize: Cardinal;

  tempDesiredAccess : Cardinal;

  mapping: TGenericMapping;
  lbAccessStatus: Bool;

  Token: TJwSecurityToken;

  TempSD  : TJwSecurityDescriptor;
  bTempSD : Boolean;
begin
  //Test parameter SecurityDescriptor
  PrepareAccessCheckSecurityDescriptor(SecurityDescriptor, ClassName, 'AccessCheck');

  //replace generic rights in Desired Access
  tempDesiredAccess := ConvertAccessMask(GenericMapping, DesiredAccess);


  //convert generic access rights in ACL
  PrepareAccessCheckGenericACE(SecurityDescriptor, GenericMapping, {out}bTempSD, {out}TempSD);
  try
    // ShowMessage(tempSecurityDescriptor.DACL.GetTextMap(TJwSecurityFileFolderMapping));
    pSDesc := tempSD.Create_SD(false);

    try
      //if PrepareAccessCheckTokenAssignment fails with an exception
      //the finally part checks for the privilege size: pPrivsSize
      pPrivsSize := 0;

      PrepareAccessCheckTokenAssignment(ClientToken, {out}Token,{out}hToken);
      {
      We do not impersonate here since AccessCheck does only use the given
      Token handle for access checks.
      }

      pPrivsSize := Sizeof(pPrivs);
      ZeroMemory(@pPrivs, pPrivsSize);


      //mappings
      if Assigned(GenericMapping) then
        mapping := TGenericMapping(GenericMapping.GetMapping())
      else
        ZeroMemory(@mapping, sizeof(mapping));

      lbAccessStatus := AccessStatus;

      if not jwaWindows.AccessCheck(
        PSecurityDescriptor(pSDesc), hToken,
        tempDesiredAccess, mapping, pPrivs,
        pPrivsSize, GrantedAccess, lbAccessStatus) then
      begin
        //MessageBoxA(0,PAnsiCHAR(SysErrorMessage(GetLastError)),'',MB_OK);
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'AccessCheck', ClassName,
          RsUNSecureObjects, 0, True, ['AccessCheck']);
      end;
    finally //clean up
      AccessStatus := lbAccessStatus;
      
      //we did not impersonate so there is nothing to do here
      Token.Free;

      SecurityDescriptor.Free_SD(pSDesc);

      if (pPrivsSize > 0) then
      begin
        PrivilegeSet :=
  {$IFNDEF SL_OMIT_SECTIONS}
          JwsclToken.
  {$ENDIF SL_OMIT_SECTIONS}
          TJwPrivilegeSet.Create(nil, jwaWindows.TPrivilegeSet(pPrivs));
        if (PrivilegeSet.Count = 0) then
          FreeAndNil(PrivilegeSet);
      end
      else
      begin
        PrivilegeSet := nil;
      end;
    end;
  finally
    TempSD.Free;
  end;
end;

// This function converts generic rights to specifc ones
// It can convert several generic rights.
class function TJwSecureBaseClass.ConvertAccessMask(
  const GenericMapping: TJwSecurityGenericMappingClass;
  InputMask : Cardinal) : Cardinal;
begin
  result := InputMask;

  if not Assigned(GenericMapping) then
    exit;

  if InputMask and GENERIC_ALL = GENERIC_ALL then
  begin
    result := GenericMapping.Map(GENERIC_ALL);
    if result = Cardinal(-1) then //if invalid mask found, just rewrite it
      result := GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE;
    //no else, result may contain the other generic rights
  end;

  if InputMask and GENERIC_READ = GENERIC_READ then
  begin
    result := (result and not GENERIC_READ) or GenericMapping.Map(GENERIC_READ);
  end;
  if InputMask and GENERIC_WRITE = GENERIC_WRITE then
  begin
    result := (result and not GENERIC_WRITE) or GenericMapping.Map(GENERIC_WRITE);
  end;
  if InputMask and GENERIC_EXECUTE = GENERIC_EXECUTE then
  begin
    result := (result and not GENERIC_EXECUTE) or GenericMapping.Map(GENERIC_EXECUTE);
  end;
end;

//this procedure replaces GENERIC access masks
//in the DACL with specific mask using mapping GenericMapping
class procedure TJwSecureBaseClass.ReplaceGenericRightsInDACL(
  const GenericMapping: TJwSecurityGenericMappingClass; const SD: TJwSecurityDescriptor);
var i : Integer;
begin
  if Assigned(SD) and Assigned(SD.DACL) then
  for i := 0 to SD.DACL.count-1 do
  begin
    SD.DACL[i].AccessMask := ConvertAccessMask(GenericMapping, SD.DACL[i].AccessMask);
  end;
end;

class procedure TJwSecureBaseClass.AccessCheckByType(
  const SecurityDescriptor : TJwSecurityDescriptor;
  const PrincipalSelfSid : TJwSecurityID;
  const ClientToken : TJwSecurityToken;
  const DesiredAccess : TJwAccessMask;
  var ObjectTypeArray : TJwObjectTypeArray;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out PrivilegeSet: TJwPrivilegeSet;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);

{
Mostly same code in TJwSecureBaseClass.AccessCheck
}

var
  pSDesc: jwaWindows.PSecurityDescriptor;
  pPrincipalSelfSid : PSID;

  i : Integer;

  hToken: TJwTokenHandle;
  pPrivs: PRIVILEGE_SET;
  pPrivsSize: Cardinal;

  tempDesiredAccess : Cardinal;

  mapping: TGenericMapping;
  lbAccessStatus: Bool;

  Token: TJwSecurityToken;

  TempSD: TJwSecurityDescriptor;
  bTempSD : Boolean;

  pObjectTypeArray : POBJECT_TYPE_LIST;
begin
  //Test parameter SecurityDescriptor
  PrepareAccessCheckSecurityDescriptor(SecurityDescriptor, ClassName, 'AccessCheckByType');

  JwRaiseOnNilParameter(ObjectTypeArray, 'ObjectTypeArray', 'AccessCheckByType',
     ClassName, RsUNAuthZCtx);

  //replace generic rights in Desired Access
  tempDesiredAccess := ConvertAccessMask(GenericMapping, DesiredAccess);

  {
  ObjectTypeArray stuff
  }

  //check for correct object array
  if not JwCheckArray(ObjectTypeArray,i) then
     raise EJwsclInvalidObjectArrayException.CreateFmtEx(
          RsInvalidObjectTypeList, 'AccessCheckByTypeResultList', ClassName,
          RsUNAuthZCtx, 0, false, [i]);

  if Assigned(PrincipalSelfSid) then
    pPrincipalSelfSid := PrincipalSelfSid.SID
  else
    pPrincipalSelfSid := nil;

  if not Assigned(ObjectTypeArray) then
  begin
    SetLength(ObjectTypeArray, 2);
    ZeroMemory(@ObjectTypeArray[0], sizeof(ObjectTypeArray[0]));
    ObjectTypeArray[0].ObjectType := @NULL_GUID;
  end;

  pObjectTypeArray := @ObjectTypeArray[0];

  SetLastError(0);

  //convert generic access rights in ACL
  PrepareAccessCheckGenericACE(SecurityDescriptor, GenericMapping, {out}bTempSD, {out}TempSD);
  try
    pSDesc := TempSD.Create_SD(false);

    try
      PrepareAccessCheckTokenAssignment(ClientToken, {out}Token,{out}hToken);
      {
      We do not impersonate here since AccessCheck does only use the given
      Token handle for access checks.
      }

      ZeroMemory(@pPrivs, sizeof(pPrivs));
      pPrivsSize := Sizeof(pPrivs);

      //mappings
      if Assigned(GenericMapping) then
        mapping := TGenericMapping(GenericMapping.GetMapping())
      else
        ZeroMemory(@mapping, sizeof(mapping));

      if not jwaWindows.AccessCheckByType(
          PSecurityDescriptor(pSDesc),//__in         PSECURITY_DESCRIPTOR pSecurityDescriptor,
          pPrincipalSelfSid, //__in_opt     PSID PrincipalSelfSid,
          hToken,//__in         HANDLE ClientToken,
          tempDesiredAccess,//__in         DWORD DesiredAccess,
          pObjectTypeArray,//__inout_opt  POBJECT_TYPE_LIST ObjectTypeList,
          Length(ObjectTypeArray),//__in         DWORD ObjectTypeListLength,
          mapping,//__out        PGENERIC_MAPPING GenericMapping,
          pPrivs,//__out_opt    PPRIVILEGE_SET PrivilegeSet,
          pPrivsSize,//__inout      LPDWORD PrivilegeSetLength,
          GrantedAccess,//__out        LPDWORD ,
          lbAccessStatus//__out        LPDWORD
        ) then

        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'AccessCheckByType', ClassName,
          RsUNSecureObjects, 0, True, ['AccessCheckByType']);

    finally //clean up
      //we did not impersonate so there is nothing to do here
      Token.Free;

      AccessStatus := lbAccessStatus;

      SecurityDescriptor.Free_SD(pSDesc);

      if (pPrivsSize > 0) then
      begin
        PrivilegeSet :=
  {$IFNDEF SL_OMIT_SECTIONS}
          JwsclToken.
  {$ENDIF SL_OMIT_SECTIONS}
          TJwPrivilegeSet.Create(nil, jwaWindows.TPrivilegeSet(pPrivs));
        if (PrivilegeSet.Count = 0) then
          FreeAndNil(PrivilegeSet);
      end
      else
        PrivilegeSet := nil;
    end;
  finally
    TempSD.Free;
  end;
end;

class procedure TJwSecureBaseClass.AccessCheckByTypeResultList(
      const SecurityDescriptor : TJwSecurityDescriptor;
      const PrincipalSelfSid : TJwSecurityID;
      const ClientToken : TJwSecurityToken;
      const DesiredAccess : TJwAccessMask;
      var ObjectTypeArray : TJwObjectTypeArray;
      const GenericMapping: TJwSecurityGenericMappingClass;
      out PrivilegeSet: TJwPrivilegeSet;
      out GrantedAccess: TJwAccessMaskArray;
      out AccessStatus: TJwCardinalArray);
var
  pSDesc: jwaWindows.PSecurityDescriptor;
  pPrincipalSelfSid : PSID;

  i : Integer;

  hToken: TJwTokenHandle;
  pPrivs: PRIVILEGE_SET;
  pPrivsSize: Cardinal;

  tempDesiredAccess : Cardinal;

  mapping: TGenericMapping;
  //lbAccessStatus: Bool;
  Token: TJwSecurityToken;

  TempSD: TJwSecurityDescriptor;
  bTempSD : Boolean;

  pObjectTypeArray : POBJECT_TYPE_LIST;
begin
  //Test parameter SecurityDescriptor
  PrepareAccessCheckSecurityDescriptor(SecurityDescriptor, ClassName, 'AccessCheckByTypeResultList');

  JwRaiseOnNilParameter(ObjectTypeArray, 'ObjectTypeArray', 'AccessCheckByTypeResultList',
     ClassName, RsUNAuthZCtx);


  //replace generic rights in Desired Access
  tempDesiredAccess := ConvertAccessMask(GenericMapping, DesiredAccess);

  {
  ObjectTypeArray stuff
  }

  //check for correct object array
  if not JwCheckArray(ObjectTypeArray,i) then
     raise EJwsclInvalidObjectArrayException.CreateFmtEx(
          RsInvalidObjectTypeList, 'AccessCheckByTypeResultList', ClassName,
          RsUNAuthZCtx, 0, false, [i]);

  if Assigned(PrincipalSelfSid) then
    pPrincipalSelfSid := PrincipalSelfSid.SID
  else
    pPrincipalSelfSid := nil;

  if not Assigned(ObjectTypeArray) then
  begin
    SetLength(ObjectTypeArray, 2);
    ZeroMemory(@ObjectTypeArray[0], sizeof(ObjectTypeArray[0]));
    ObjectTypeArray[0].ObjectType := @NULL_GUID;
  end;

  pObjectTypeArray := @ObjectTypeArray[0];

  SetLength(GrantedAccess, Length(ObjectTypeArray));
  SetLength(AccessStatus, Length(ObjectTypeArray));

  SetLastError(0);

  //convert generic access rights in ACL
  PrepareAccessCheckGenericACE(SecurityDescriptor, GenericMapping, {out}bTempSD, {out}TempSD);
  try
    pSDesc := TempSD.Create_SD(false);

    try
      PrepareAccessCheckTokenAssignment(ClientToken, {out}Token,{out}hToken);
      {
      We do not impersonate here since AccessCheck does only use the given
      Token handle for access checks.
      }

      FillChar(pPrivs, sizeof(pPrivs), 0);
      pPrivsSize := Sizeof(pPrivs);

      //mappings
      if Assigned(GenericMapping) then
        mapping := TGenericMapping(GenericMapping.GetMapping())
      else
        ZeroMemory(@mapping, sizeof(mapping));

      if not jwaWindows.AccessCheckByTypeResultList(
          PSecurityDescriptor(pSDesc),//__in         PSECURITY_DESCRIPTOR pSecurityDescriptor,
          pPrincipalSelfSid, //__in_opt     PSID PrincipalSelfSid,
          hToken,//__in         HANDLE ClientToken,
          tempDesiredAccess,//__in         DWORD DesiredAccess,
          pObjectTypeArray,//__inout_opt  POBJECT_TYPE_LIST ObjectTypeList,
          Length(ObjectTypeArray),//__in         DWORD ObjectTypeListLength,
          mapping,//__out        PGENERIC_MAPPING GenericMapping,
          pPrivs,//__out_opt    PPRIVILEGE_SET PrivilegeSet,
          pPrivsSize,//__inout      LPDWORD PrivilegeSetLength,
          GrantedAccess[0],//__out        LPDWORD GrantedAccessList,
          AccessStatus[0]//__out        LPDWORD AccessStatusList
        ) then

        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'AccessCheckByTypeResultList', ClassName,
          RsUNSecureObjects, 0, True, ['AccessCheckByTypeResultList']);

    finally //clean up
      Token.Free;

      SecurityDescriptor.Free_SD(pSDesc);

      if (pPrivsSize > 0) then
      begin
        PrivilegeSet :=
  {$IFNDEF SL_OMIT_SECTIONS}
          JwsclToken.
  {$ENDIF SL_OMIT_SECTIONS}
          TJwPrivilegeSet.Create(nil, jwaWindows.TPrivilegeSet(pPrivs));
        if (PrivilegeSet.Count = 0) then
          FreeAndNil(PrivilegeSet);
      end
      else
        PrivilegeSet := nil;
    end;
  finally
    TempSD.Free;
  end;
end;


class procedure TJwSecureBaseClass.AccessCheckAndAuditAlarm(
  const SubsystemName: TJwString;
  const HandleID: Pointer;
  const ObjectTypeName,
  ObjectName: TJwString;
  const SecurityDescriptor:
  TJwSecurityDescriptor;
  const DesiredAccess: TJwAccessMask;
  const GenericMapping:
  TJwSecurityGenericMappingClass;
  const ObjectCreation: boolean;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean;
  out pfGenerateOnClose: boolean);
var
  pSDesc: jwaWindows.PSecurityDescriptor;

  mapping: TGenericMapping;
  lbAccessStatus, lbpfGenerateOnClose: longbool;

begin
  if not Assigned(SecurityDescriptor) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsNilParameter,
      'AccessCheck',
      ClassName, RsUNSecureObjects, 0, False, ['SecurityDescriptor']);

  pSDesc := SecurityDescriptor.Create_SD();

  mapping := TGenericMapping(GenericMapping.GetMapping());
  lbAccessStatus := AccessStatus;

  if not
     {$IFDEF UNICODE}AccessCheckAndAuditAlarmW{$ELSE}
    AccessCheckAndAuditAlarmA
{$ENDIF}
    (TJwPChar(SubsystemName), HandleID,
    TJwPChar(ObjectTypeName), TJwPChar(ObjectName),
    PSecurityDescriptor(pSDesc), DesiredAccess,
    mapping, ObjectCreation, GrantedAccess,
    lbAccessStatus, lbpfGenerateOnClose) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'AccessCheck', ClassName,
      RsUNSecureObjects, 0, True, ['AccessCheck']);
  end;


  AccessStatus := lbAccessStatus;
  pfGenerateOnClose := lbpfGenerateOnClose;
end;


procedure CrackSecurityDescriptor(const SD: TJwSecurityDescriptor;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  out anOwner, aGroup: TJwSecurityId; out aDACL: TJwDAccessControlList;
  out aSACL: TJwSAccessControlList);
begin
  anOwner := nil;
  aGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if siOwnerSecurityInformation in aSecurityInfo then
    anOwner := SD.Owner;
  if siGroupSecurityInformation in aSecurityInfo then
    aGroup := SD.PrimaryGroup;
  if siDaclSecurityInformation in aSecurityInfo then
    aDACL := SD.DACL;
  if siSaclSecurityInformation in aSecurityInfo then
    aSACL := SD.SACL;
end;

class procedure TJwSecureBaseClass.SetNamedSecurityInfo(
  const anObjectName: TJwString; const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const aSecurityDescriptor: TJwSecurityDescriptor);
var
  anOwner, aGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  if not Assigned(aSecurityDescriptor) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsNilParameter,
      'SetNamedSecurityInfo', ClassName, RsUNSecureObjects, 0, False,
       ['SecurityDescriptor']);


  CrackSecurityDescriptor(
    aSecurityDescriptor,//const SD : TJwSecurityDescriptor;
    aSecurityInfo,//const aSecurityInfo: TJwSecurityInformationFlagSet;
    anOwner, aGroup,//out anOwner, aGroup : TJwSecurityId;
    aDACL,//out aDACL : TJwDAccessControlList;
    aSACL //out aSACL : TJwSAccessControlList
    );

  SetNamedSecurityInfo(anObjectName, aObjectType,
    aSecurityInfo,
    anOwner,
    aGroup,
    aDACL,
    aSACL);
                         {aSecurityDescriptor.Owner,
                         aSecurityDescriptor.PrimaryGroup,
                         aSecurityDescriptor.DACL,
                         aSecurityDescriptor.SACL);}

end;



class procedure TJwSecureBaseClass.SetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const aSecurityDescriptor: TJwSecurityDescriptor);

var
  anOwner, aGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  if not Assigned(aSecurityDescriptor) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsNilParameter,
      'SetSecurityInfo',
      ClassName, RsUNSecureObjects, 0, False, ['SecurityDescriptor']);

  anOwner := nil;
  aGroup := nil;
  aDACL := nil;
  aSACL := nil;

  CrackSecurityDescriptor(
    aSecurityDescriptor,//const SD : TJwSecurityDescriptor;
    aSecurityInfo,//const aSecurityInfo: TJwSecurityInformationFlagSet;
    anOwner, aGroup,//out anOwner, aGroup : TJwSecurityId;
    aDACL,//out aDACL : TJwDAccessControlList;
    aSACL //out aSACL : TJwSAccessControlList
    );

  SetSecurityInfo(aHandle, aObjectType,
    aSecurityInfo,
    anOwner,
    aGroup,
    aDACL,
    aSACL);
                         {aSecurityDescriptor.Owner,
                         aSecurityDescriptor.PrimaryGroup,
                         aSecurityDescriptor.DACL,
                         aSecurityDescriptor.SACL);}
end;




procedure TJwSecureBaseClass.SetSecurityDescriptor(
  const SD: TJwSecurityDescriptor;
  const SD_entries: TJwSecurityInformationFlagSet);
var SDInfo : TJwSecurityInformationFlagSet;
begin
  if not Assigned(SD) then
    raise EJwsclNILParameterException.CreateFmtEx(
      'Parameter SD must not be nil', 'SetSecurityDescriptor', ClassName,
      RsUNSecureObjects, 0, False, []);

  if siOwnerSecurityInformation in SD_entries then
    SetOwner(SD.Owner);
  if siGroupSecurityInformation in SD_entries then
    SetGroup(SD.PrimaryGroup);


  //protect, unprotect ACL
  SDInfo := SD_entries;

  Exclude(SDInfo, siprotectedDaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedDaclSecurityInformation);
  Exclude(SDInfo, siprotectedSaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedSaclSecurityInformation);

  case SD.InheritanceDACLProtection of
    aclpProtected      : SetDACL(SD.DACL,apProtected);
    aclpForceUnprotect : SetDACL(SD.DACL,apUnProtected);
  else
    SetDACL(SD.DACL);
  end;

  case SD.InheritanceSACLProtection of
    aclpProtected      : SetSACL(SD.SACL,apProtected);
    aclpForceUnprotect : SetSACL(SD.SACL,apUnProtected);
  else
    SetSACL(SD.SACL);
  end;


{  if siLabelSecurityInformation in SD_entries then
    SetIntegrityLabel(SD.}
end;


function TJwSecureBaseClass.GetSecurityDescriptor(
  const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
begin
  {GetSecurityDescriptor must provide all information from a SD
   including Control flag. We cannot do this here.}
  Raise EJwsclNotImplementedException.Create('You must override GetSecurityDescriptor');
end;


type
  PFNProgressRecord = ^TFNProgressRecord;

  TFNProgressRecord = record
    FNProgressMethod: TJwFnProgressMethod;
    FNProgressProcedure: TJwFnProgressProcedure;
    ProgressUserData: Pointer;
    //        E : Exception;
  end;

procedure FNProgress_TreeResetNamedSecurityInfo(
  const pObjectName: PWideChar;
  // Name of object just processed
  const cStatus: Cardinal;
  // Status of operation on object
  var pInvokeSetting: PROG_INVOKE_SETTING; // When to set
  const Args: Pointer;                      // Caller specific data
  const bSecuritySet:
  boolean                 // Whether security was set
  ); stdcall;
var
  FNProgressRecord: PFNProgressRecord absolute Args;
  fnProgress: TJwProgInvokeSetting absolute pInvokeSetting;
  //variable is same as parameter pInvokeSetting
begin
  if not Assigned(FNProgressRecord) then
    exit;

  if Assigned(FNProgressRecord.FNProgressMethod) then
    try
      FNProgressRecord.FNProgressMethod(TJwString(pObjectName),
        cStatus, fnProgress,
        nil,
        FNProgressRecord.ProgressUserData, bSecuritySet);
    except
    end;

  if Assigned(FNProgressRecord.FNProgressProcedure) then
    try
      FNProgressRecord.FNProgressProcedure(TJwString(pObjectName),
        cStatus, fnProgress,
        nil, FNProgressRecord.ProgressUserData,
        bSecuritySet);
    except
    end;
end;


var
  _TreeResetNamedSecurityInfo : Pointer;

function InternalTreeResetNamedSecurityInfo(pObjectName: TJwPchar; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; pOwner, pGroup: PSID; pDacl, pSacl: PACL;
    KeepExplicit: BOOL; fnProgress: FN_PROGRESS; ProgressInvokeSetting: PROG_INVOKE_SETTING;
    Args: PVOID): DWORD;
const Suffix = {$IFDEF UNICODE}'W'{$ELSE}'A'{$ENDIF UNICODE};
begin
  result := 0;
  //raises exception if not found!!
  GetProcedureAddress(_TreeResetNamedSecurityInfo, aclapilib, 'TreeResetNamedSecurityInfo'+Suffix);

  if @_TreeResetNamedSecurityInfo = nil then
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TreeResetNamedSecurityInfo]
  end;
end;


class procedure TJwSecureBaseClass.TreeResetNamedSecurityInfo(
  pObjectName: TJwString;
  const aObjectType: TSeObjectType;//SE_OBJECT_TYPE ObjectType,
  const aSecurityInfo:
  TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const bKeepExplicit: boolean;
  const fnProgress:
  TJwProgInvokeSetting;
  const FNProgressMethod: TJwFnProgressMethod;
  const FNProgressProcedure:
  TJwFnProgressProcedure;
  const ProgressUserData: Pointer);
var
  Result: Cardinal;
  pOwner, pGroup: PSid;
  pDACL, pSACL: PACL;
  FNProgressRecord: PFNProgressRecord;
  SecurityInfo: TJwSecurityInformationFlagSet;
begin
   if not (TJwWindowsVersion.IsWindowsXP(True) or
    TJwWindowsVersion.IsWindows2003(True)) then
    raise EJwsclUnsupportedWindowsVersionException.CreateFmtEx(
      RsTokenUnsupportedWtsCall, 'TreeResetNamedSecurityInfo', ClassName, RsUNToken, 0, False, []);



  FNProgressRecord := nil;
  if Assigned(FNProgressMethod) or Assigned(FNProgressProcedure) then
  begin
    GetMem(FNProgressRecord, sizeof(TFNProgressRecord));
    FNProgressRecord.FNProgressMethod := FNProgressMethod;
    FNProgressRecord.FNProgressProcedure := FNProgressProcedure;
    FNProgressRecord.ProgressUserData := ProgressUserData;
  end;

  SecurityInfo := aSecurityInfo;

  pOwner := nil;
  pGroup := nil;
  pDACL  := nil;
  pSACL  := nil;

  if Assigned(Owner) then
  begin
    pOwner := Owner.SID;
    Include(SecurityInfo, siOwnerSecurityInformation);
  end;

  if Assigned(Group) then
  begin
    pGroup := Group.SID;
    Include(SecurityInfo, siGroupSecurityInformation);
  end;

  if Assigned(DACL) then
  begin
    pDACL := DACL.Create_PACL;
    Include(SecurityInfo, siDaclSecurityInformation);
  end;

  if Assigned(SACL) then
  begin
    pSACL := SACL.Create_PACL;
    Include(SecurityInfo, siSaclSecurityInformation);
  end;


  //[Hint] result := 0;
  try
    Result :=
    InternalTreeResetNamedSecurityInfo(
      TJwPChar(pObjectName),  //LPTSTR pObjectName,
      aObjectType,        //SE_OBJECT_TYPE ObjectType,
      TJwEnumMap.ConvertSecurityInformation(SecurityInfo),
      //SECURITY_INFORMATION SecurityInfo,
      jwaWindows_PSID(pOwner),         //PSID pOwner,
      jwaWindows_PSID(pGroup),         //PSID pGroup,
      jwaWindows_PACL(pDACL),         //PACL pDacl,
      jwaWindows_PACL(pSACL),         //PACL pSacl,
      bKeepExplicit,         //BOOL KeepExplicit,
      @FNProgress_TreeResetNamedSecurityInfo,
      //FN_PROGRESS fnProgress,
      integer(fnProgress),
      //PROG_INVOKE_SETTING ProgressInvokeSetting,
      FNProgressRecord         //PVOID Args
      );

  finally
    if Assigned(FNProgressRecord) then
    begin
      FreeMem(FNProgressRecord);
      //[Hint] FNProgressRecord := nil;
    end;

    if Assigned(pDACL) then
      TJwDAccessControlList.Free_PACL(pDACL);
    if Assigned(pSACL) then
      TJwDAccessControlList.Free_PACL(pSACL);
  end;

  if Result <> ERROR_SUCCESS then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'TreeResetNamedSecurityInfo',
      ClassName, RsUNSecureObjects, 0, False, ['TreeResetNamedSecurityInfo']);
  end;

end;


class function TJwSecureBaseClass.GetInheritanceSource(
  const pObjectName: TJwString; const aObjectType: TSeObjectType;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  const bContainer: boolean; const GUIDs: TJwGuidArray;
  const ACL: TJwSecurityAccessControlList;
  const Mapping: TJwSecurityGenericMappingClass;
  const FN_OBJECT_MGR_FUNCTS: Pointer): TJwInheritedFromArray;
var
  hres:  HRESULT;
  aPACL: jwaWindows.PACL;
  aMapping: TGenericMapping;
  aPGUID: PGUID;
  pfromArray: PInheritedFromW;

  fromArray : TJwInheritedFromArrayEx;
  Count, i: integer;
begin
  if not Assigned(ACL) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'GetInheritanceSource',
      ClassName, RsUNSecureObjects, 0, False, ['ACL']);

  if ACL.Count = 0 then
    raise EJwsclEmptyACLException.CreateFmtEx(
      RsSecureObjectsInvalidEmptyAclParameter, 'GetInheritanceSource',
      ClassName, RsUNSecureObjects, 0, False, []);

  Count := ACL.Count;

  pFromArray := nil;
  aPACL := ACL.Create_PACL();
  try
(*
{$IFDEF FPC}
    if (Mapping.ClassType <> TJwSecurityGenericMapping.ClassType) then
{$ELSE}
    if (not Mapping.ClassNameIs(TJwSecurityGenericMapping.ClassName)) then
{$ENDIF FPC}*)
      aMapping := TGenericMapping(Mapping.GetMapping());
(*    else
      ZeroMemory(@aMapping, sizeof(aMapping));
  *)
    if Length(GUIDs) > 0 then
      aPGUID := @GUIDs
    else
      aPGUID := nil;

    Result := nil;

    pFromArray := PINHERITED_FROMW(LocalAlloc(LPTR, sizeof(pFromArray)*ACL.count+1));
    {The following statement should not interfere with memory manager,
     because it was not created by GetMem!
     At the end of function (or explicitly by fromArray := nil;)
      the array will be freed using FreeMem. Because the array does not
     have correct borders, this fails.
     We use LocalAlloc, so Delphi does nothing.
    }
    fromArray := TJwInheritedFromArrayEx(pFromArray); //array of TInheritedFromW !!

    hres :=
     JwaWindows.GetInheritanceSourceW
      (PWideChar(WideString(pObjectName)),    //LPTSTR pObjectName,
      SE_FILE_OBJECT,//aObjectType,    //SE_OBJECT_TYPE ObjectType,
      //DACL_SECURITY_INFORMATION,
      TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), //SECURITY_INFORMATION SecurityInfo,
      bContainer,//  BOOL Container,
      @aPGUID,  //GUID** pObjectClassGuids,
      Length(GUIDs), //DWORD GuidCount,
      jwaWindows_PACL(aPACL), //PACL pAcl,
      FN_OBJECT_MGR_FUNCTS, //PFN_OBJECT_MGR_FUNCTS pfnArray,
      @aMapping, //PGENERIC_MAPPING pGenericMapping,
      pFromArray//PINHERITED_FROM(result), //PINHERITED_FROM pInheritArray
      );
    if (hres = 120) then //unsupported call
      raise EJwsclInheritanceSourceNotSupportedException.CreateFmtEx(
        RsSecureObjectsUnsupportedInheritanceFunction,
        'GetInheritanceSource', ClassName, RsUNSecureObjects, 0, False, [hres]);


    if hres <> NOERROR then
    begin
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailedWithNTStatus,
        'GetInheritanceSource', ClassName, RsUNSecureObjects, 0, hres,
        ['GetInheritanceSource',hres]);
    end
    else
    begin
      SetLength(Result, Count);


      for i := 0 to ACL.Count - 1 do
      begin
        Result[i].GenerationGap := FromArray[i].GenerationGap;
        Result[i].AncestorName  := FromArray[i].AncestorName;
      end;
    end;


  finally
    if pFromArray <> nil then
      FreeInheritedFromArray(PINHERITED_FROMW(pFromArray), ACL.Count, nil);
    //    LocalFree(HLOCAL(pFromArray));

    ACL.Free_PACL(aPACL);
  end;

end;



procedure TJwSecureBaseClass.ResetTemp(resetState: TJwTempResetEnumSet);
begin
  if (treOwner in resetState) then
    FreeAndNil(fOwner);
  if (treGroup in resetState) then
    FreeAndNil(fGroup);
  if (treDacl in resetState) then
    FreeAndNil(fDAccessControlList);
  if (treSacl in resetState) then
    FreeAndNil(fSAccessControlList);
end;



class procedure TJwSecureBaseClass.TakeOwnerShip(const PathName: TJwString;
  const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
var InternalSid : TJwSecurityId;
begin
  if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
    JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME, pst_Enable);

  InternalSid := SID;

  if InternalSid = nil then
    InternalSid := JwSecurityCurrentThreadUserSID;

  try
    TJwSecureGeneralObject.SetNamedSecurityInfo(PathName,
      aObjectType,
      //const aObjectType : TSeObjectType;
      [siOwnerSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      InternalSid,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      nil,
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME, pst_Disable);

    InternalSid.Free;
  end;
end;

class procedure TJwSecureBaseClass.TakeOwnerShip(const Handle: THandle;
  const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
var InternalSid : TJwSecurityId;  
begin
  if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
    JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME, pst_Enable);

  InternalSid := SID;

  if InternalSid = nil then
    InternalSid := JwSecurityCurrentThreadUserSID;

  try
    TJwSecureGeneralObject.SetSecurityInfo(Handle,
      aObjectType,
      //const aObjectType : TSeObjectType;
      [siOwnerSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      InternalSid,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      nil,
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    if JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME, pst_Disable);

    InternalSid.Free;
  end;
end;




{************ TJwSecureGeneralObject *****************}




class function TJwSecureGeneralObject.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  const GenericMapping: TJwSecurityGenericMappingClass): Boolean;
begin
  result := inherited AccessCheck(SecurityDescriptor, ClientToken,
    DesiredAccess, GenericMapping);
end;

class procedure TJwSecureGeneralObject.SetSecurityInfo(
  const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const anOwner: TJwSecurityId;
  const anGroup: TJwSecurityId;
  const aDACL: TJwDAccessControlList;
  const aSACL: TJwSAccessControlList);
begin
  inherited;
end;


class procedure TJwSecureGeneralObject.SetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const anOwner: TJwSecurityId;
  const anGroup: TJwSecurityId;
  const aDACL: TJwDAccessControlList;
  const aSACL: TJwSAccessControlList);
begin
  inherited;
end;


class procedure TJwSecureGeneralObject.GetSecurityInfo(
  const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  out anOwner: TJwSecurityId;
  out anGroup: TJwSecurityId;
  out aDACL: TJwDAccessControlList;
  out aSACL: TJwSAccessControlList);
begin
  inherited;
end;


class function TJwSecureGeneralObject.GetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet
  )
: {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
TJwSecurityDescriptor;
begin
  Result := inherited GetSecurityInfo(aHandle, aObjectType, aSecurityInfo);
end;



class function TJwSecureGeneralObject.GetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet
  )
: {$IFNDEF SL_OMIT_SECTIONS}JwsclDescriptor.{$ENDIF SL_OMIT_SECTIONS}
TJwSecurityDescriptor;
begin
  Result := inherited GetNamedSecurityInfo(anObjectName,
    aObjectType, aSecurityInfo);
end;


class procedure TJwSecureGeneralObject.GetNamedSecurityInfo(
  const anObjectName: TJwString;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  out anOwner: TJwSecurityId;
  out anGroup: TJwSecurityId;
  out aDACL: TJwDAccessControlList;
  out aSACL: TJwSAccessControlList);
begin
  inherited;
end;

class function TJwSecureGeneralObject.GetInheritanceSource(
  const pObjectName: TJwString;
  //LPTSTR pObjectName,
  const aObjectType: TSeObjectType;
  //SE_OBJECT_TYPE ObjectType,
  const aSecurityInfo:
  TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
  const bContainer: boolean;
  //BOOL Container,
  const GUIDs: TJwGuidArray;
  //GUID** pObjectClassGuids,
  const ACL: TJwSecurityAccessControlList;
  //PACL pAcl,
  const Mapping:
  TJwSecurityGenericMappingClass;
  //PGENERIC_MAPPING pGenericMapping,
  const FN_OBJECT_MGR_FUNCTS: Pointer =
  nil): TJwInheritedFromArray;
  {PINHERITED_FROM pInheritArray}
begin
  Result := inherited GetInheritanceSource(pObjectName,
    aObjectType, aSecurityInfo, bContainer, GUIDs, ACL, Mapping,
    FN_OBJECT_MGR_FUNCTS);
end;

class procedure TJwSecureGeneralObject.TreeResetNamedSecurityInfo(
  pObjectName: TJwString;
  const aObjectType: TSeObjectType;//SE_OBJECT_TYPE ObjectType,
  const aSecurityInfo:
  TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const bKeepExplicit: boolean;
  const fnProgress:
  TJwProgInvokeSetting;
  const FNProgressMethod: TJwFnProgressMethod;
  const FNProgressProcedure:
  TJwFnProgressProcedure;
  const ProgressUserData: Pointer);
begin
  inherited;
end;



class procedure TJwSecureGeneralObject.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);
begin
  inherited;
end;

(*
no none-static methods in TJwSecureGeneralObject!!
function TJwSecureGeneralObject.AccessCheck(
  DesiredAccess: TJwAccessMask = Cardinal(-1);
  const ClientToken: TJwSecurityToken = nil): boolean;
var
  SD: TJwSecurityDescriptor;
  //[Hint] GA : TJwSecurityGenericMapping;
  privSet: TJwPrivilegeSet;
  GrantedAccess: Cardinal;
begin
  SD := GetSecurityDescriptor([siOwnerSecurityInformation,
    siGroupSecurityInformation, siDaclSecurityInformation]);

  if DesiredAccess = Cardinal(-1) then
    DesiredAccess := fAccessMask;

  //!!!  noch tfilestream access mask hier verwenden

  privSet := nil;
  try
    Self.AccessCheck(SD, ClientToken, DesiredAccess, TJwSecurityGenericMapping,
      privSet, GrantedAccess, Result)
  finally
    privSet.Free;
    SD.Free;
  end;
end;        *)

class procedure TJwSecureGeneralObject.AccessCheckAndAuditAlarm(
  const SubsystemName: TJwString;
  const HandleID: Pointer;
  const ObjectTypeName,
  ObjectName: TJwString;
  const SecurityDescriptor:
  TJwSecurityDescriptor;
  const DesiredAccess: TJwAccessMask;
  const GenericMapping:
  TJwSecurityGenericMappingClass;
  const ObjectCreation: boolean;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean;
  out pfGenerateOnClose: boolean);
begin
  inherited;
end;




class procedure TJwSecureGeneralObject.AccessCheckByType(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const PrincipalSelfSid: TJwSecurityID;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  var ObjectTypeArray: TJwObjectTypeArray;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);
begin
  inherited;

end;

class procedure TJwSecureGeneralObject.AccessCheckByTypeResultList(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const PrincipalSelfSid: TJwSecurityID;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  var ObjectTypeArray: TJwObjectTypeArray;
  const GenericMapping: TJwSecurityGenericMappingClass;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMaskArray;
  out AccessStatus: TJwCardinalArray);
begin
  inherited;

end;



class procedure TJwSecureGeneralObject.SetNamedSecurityInfo(
  const anObjectName: TJwString; const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const aSecurityDescriptor: TJwSecurityDescriptor);
begin
  inherited;
end;

class procedure TJwSecureGeneralObject.SetSecurityInfo(const aHandle: THandle;
  const aObjectType: TSeObjectType;
  aSecurityInfo: TJwSecurityInformationFlagSet;
  const aSecurityDescriptor: TJwSecurityDescriptor);
begin
  inherited;
end;

class procedure TJwSecureGeneralObject.TakeOwnerShip(const Handle: THandle;
  const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
begin
  inherited;
end;

class procedure TJwSecureGeneralObject.TakeOwnerShip(const ObjectName: TJwString;
  const aObjectType: TSeObjectType; SID: TJwSecurityId = nil);
begin
  inherited;
end;

class function TJwSecureGeneralObject.ConvertMaximumAllowed(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken;
  const GenericMapping: TJwSecurityGenericMappingClass): TAccessMask;
begin
  result := inherited ConvertMaximumAllowed(SecurityDescriptor, ClientToken,
    GenericMapping);
end;

class function TJwSecureGeneralObject.GetOwnerShipMembers: TJwSecurityIdList;
begin
  result := inherited GetOwnerShipMembers;

end;




{ ******** TJwSecureFileObject ********}

constructor TJwSecureFileObject.Create(const FileHandle: THandle;
  AccessMask: TJwAccessMask = 0; bDuplicateHandle: boolean = False);
var
  d: Cardinal;
begin
  if (FileHandle = 0) or (FileHandle = Cardinal(-1)) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsNilParameter,
      'TJwSecureFileObject.Create', ClassName,
      RsUNSecureObjects, 0, False, ['FileHandle']);

  fAccessMask := AccessMask;
  fHandle := 0;
  fDuplicateHandle := bDuplicateHandle;

  d := 0;
  if AccessMask = 0 then
    d := DUPLICATE_SAME_ACCESS;

  if not bDuplicateHandle then
    fHandle := FileHandle
  else
  if not DuplicateHandle(GetCurrentProcess,
    //HANDLE hSourceProcessHandle,  // handle to process with handle to duplicate
    FileHandle, //HANDLE hSourceHandle,  // handle to duplicate
    GetCurrentProcess,
    //HANDLE hTargetProcessHandle,  // handle to process to duplicate to
    @fHandle, //LPHANDLE lpTargetHandle,  // pointer to duplicate handle
    AccessMask, //DWORD dwDesiredAccess,  // access for duplicate handle
    False, //BOOL bInheritHandle,  // handle inheritance flag
    d //DWORD dwOptions   // optional actions
    ) then
    raise EJwsclSecurityObjectException.CreateFmtEx(
      RsSecureObjectsNoCopyOfObjectHandle, 'TJwSecureFileObject.Create',
      ClassName, RsUNSecureObjects, 0, True, []);


  fFileName := '';
end;

constructor TJwSecureFileObject.Create(const F: TFileStream;
  AccessMask: TJwAccessMask = 0);
begin
  if not Assigned(F) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsNilParameter,
      'TJwSecureFileObject.Create(const F: TFileStream)',
      ClassName, RsUNSecureObjects, 0, False, ['F']);

  Create(THandle(F.Handle), AccessMask, True);

  fDuplicateHandle := False;
  fAccessMask := AccessMask;
end;

constructor TJwSecureFileObject.Create(const FileName: TJwString);
begin
(*
{$IFNDEF DELPHI5}
  if not DirectoryExists(FileName) and not FileExists(FileName) then
{$ELSE}
  if not FileExists(FileName) then
{$ENDIF}
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsFileFolderNotFound, 'TJwSecureFileObject.Create',
      ClassName, RsUNSecureObjects, 0, False, [FileName]);
  *)
  inherited Create;

  fFileName := FileName;
  fDuplicateHandle := False;
  fAccessMask := FILE_ALL_ACCESS;
end;

class procedure TJwSecureFileObject.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);
begin
  inherited AccessCheck(SecurityDescriptor,
    ClientToken,
    DesiredAccess,
    TJwSecurityFileMapping,
    PrivilegeSet,
    GrantedAccess,
    AccessStatus);
end;


function TJwSecureFileObject.AccessCheck(DesiredAccess: TJwAccessMask;
  const ClientToken: TJwSecurityToken): boolean;
var
  SD: TJwSecurityDescriptor;
  GA: Cardinal;
  privSet: TJwPrivilegeSet;
begin
  if DesiredAccess and MAXIMUM_ALLOWED <> 0 then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsInvalidParameterAccessCheck, 'AccessCheck',
      ClassName, RsUNSecureObjects, 0, False, []);
  end;
  
  SD := GetSecurityDescriptor([siOwnerSecurityInformation,
    siGroupSecurityInformation, siDaclSecurityInformation]);

  if DesiredAccess = Cardinal(-1) then
    DesiredAccess := fAccessMask;

  //!!!  noch tfilestream access mask hier verwenden

  privSet := nil;
  try
    AccessCheck(SD, ClientToken, DesiredAccess, privSet, GA, Result);
  finally
    privSet.Free;
    SD.Free;
  end;
end;

procedure TJwSecureFileObject.AccessCheck(
  DesiredAccess: TJwAccessMask;
  out PrivilegeSet: TJwPrivilegeSet;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean;
  const ClientToken: TJwSecurityToken = nil);
var
  SD: TJwSecurityDescriptor;
begin
  SD := GetSecurityDescriptor([siOwnerSecurityInformation,
    siGroupSecurityInformation, siDaclSecurityInformation]);

  if DesiredAccess = Cardinal(-1) then
    DesiredAccess := fAccessMask;

  //!!!  noch tfilestream access mask hier verwenden

  try
    AccessCheck(SD, ClientToken, DesiredAccess,
      PrivilegeSet, GrantedAccess, AccessStatus);
  finally
    SD.Free;
  end;
end;



class procedure TJwSecureFileObject.AccessCheckAndAuditAlarm(
  const SubsystemName: TJwString;
  const HandleID: Pointer;
  const ObjectTypeName,
  ObjectName: TJwString;
  const SecurityDescriptor:
  TJwSecurityDescriptor;
  const DesiredAccess: TJwAccessMask;
  const GenericMapping:
  TJwSecurityGenericMappingClass;
  const ObjectCreation: boolean;
  out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean;
  out pfGenerateOnClose: boolean);
begin
  inherited;
end;



destructor TJwSecureFileObject.Destroy;
begin
  if fDuplicateHandle and HasValidHandle then
    CloseHandle(fHandle);
  fHandle := 0;

  inherited;
end;

function TJwSecureFileObject.GetDACL: TJwDAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  //[Hint] aDACL : TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  //[Hint] aDACL := nil;
  aSACL := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetGroup: TJwSecurityId;
var
  anOwner: TJwSecurityId;
  //[Hint] anGroup : TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  //[Hint] anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetGroup', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetOwner: TJwSecurityId;
var
  //[Hint] anOwner : TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  //[Hint] anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetOwner', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureFileObject.SetMandatoryLabel(const MandatoryLabel :
      TJwSystemMandatoryAccessControlEntry);
var
  SACL : TJwSAccessControlList;
begin
  SACL := TJwSAccessControlList.Create;
  SACL.SetMandatoryLabel(MandatoryLabel,cfCopyInstance);

  try
    if (fFileName <> '') then
    begin
      SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
        [siLabelSecurityInformation],nil,nil,nil,SACL);
    end
    else
    if HasValidHandle then
    begin
      SetSecurityInfo(fHandle, SE_FILE_OBJECT,
        [siLabelSecurityInformation],nil,nil,nil,SACL);
    end
    else
      raise EJwsclInvalidObjectException.CreateFmtEx(
        RsSecureObjectsInvalidRegPathHandle, 'SetMandatoryLabel', ClassName,
        RsUNSecureObjects, 0, False, []);
  finally
    SACL.Free;
  end;
end;

function TJwSecureFileObject.GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry;
var
  SD : TJwSecurityDescriptor;
begin
  Result := nil;

  SD := nil;

  try
    if (fFileName <> '') then
    begin
      SD := GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
        [siLabelSecurityInformation]);

      if Assigned(SD.AuditACL) and SD.AuditACL.HasMandatoryLabel then
      begin
        result := TJwSystemMandatoryAccessControlEntry.Create(SD.AuditACL.MandatoryLabel);
      end;
    end
    else
    if HasValidHandle then
    begin
      SD := GetSecurityInfo(fHandle, SE_FILE_OBJECT,
        [siLabelSecurityInformation]);

      if Assigned(SD.AuditACL) and SD.AuditACL.HasMandatoryLabel then
      begin
        result := TJwSystemMandatoryAccessControlEntry.Create(SD.AuditACL.MandatoryLabel);
      end;
    end
    else
      raise EJwsclInvalidObjectException.CreateFmtEx(
        RsSecureObjectsInvalidRegPathHandle, 'GetMandatoryLabel', ClassName,
        RsUNSecureObjects, 0, False, []);
  finally
    SD.Free;
  end;
end;

function TJwSecureFileObject.GetSACL: TJwSAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  //[Hint] aSACL : TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  //[Hint] aSACL := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetSACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetTempDACL: TJwDAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  //[Hint] aDACL : TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  //[Hint] aDACL := nil;
  aSACL := nil;

  if Assigned(fDAccessControlList) then
  begin
    Result := fDAccessControlList;
    exit;
  end;

  fDAccessControlList.Free;
  fDAccessControlList := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);

    fDAccessControlList := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);

    fDAccessControlList := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetTempGroup: TJwSecurityId;
var
  anOwner: TJwSecurityId;
  //[Hint] anGroup : TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  //[Hint] anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if Assigned(fGroup) then
  begin
    Result := fGroup;
    exit;
  end;

  fGroup.Free;
  fGroup := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

    fGroup := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

    fGroup := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempGroup', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetTempOwner: TJwSecurityId;
var
  //[Hint] anOwner : TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  Result := nil;
  //[Hint] anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if Assigned(fOwner) then
  begin
    Result := fOwner;
    exit;
  end;

  fOwner.Free;
  fOwner := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);

    fOwner := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);

    fOwner := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempOwner', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetTempSACL: TJwSAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  //[Hint] aSACL : TJwSAccessControlList;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  //[Hint] aSACL := nil;

  if Assigned(fSAccessControlList) then
  begin
    Result := fSAccessControlList;
    exit;
  end;

  fSAccessControlList.Free;
  fSAccessControlList := nil;

  if (fFileName <> '') then
  begin
    GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

    fSAccessControlList := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

    fSAccessControlList := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempSACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;




procedure TJwSecureFileObject.ResetTemp(resetState: TJwTempResetEnumSet);
begin
  inherited;
end;


procedure TJwSecureFileObject.SetDACL(const list: TJwDAccessControlList;
  const Protection : TJwACLProtectionState = apNone);
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aSACL: TJwSAccessControlList;
  Flags : TJwSecurityInformationFlagSet;

begin

  anOwner := nil;
  anGroup := nil;

  aSACL := nil;
  Flags := [siDaclSecurityInformation];
  if Protection = apProtected then
    Include(Flags, siProtectedDaclSecurityInformation)
  else
  if Protection = apUnprotected then
    Include(Flags, siUnprotectedDaclSecurityInformation);

  //List can be nil = NULL DACL

  if (fFileName <> '') then
  begin
    //first delete the old DACL
    if AutoResetACL then
      SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
        [siDaclSecurityInformation], anOwner,
        anGroup, nil, aSACL);

    SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      Flags, anOwner,
      anGroup, list, aSACL);
  end
  else
  if HasValidHandle then
  begin
    //first delete the old DACL
    if AutoResetACL then
      SetSecurityInfo(fHandle, SE_FILE_OBJECT,
        [siDaclSecurityInformation], anOwner,
        anGroup, nil, aSACL);

    SetSecurityInfo(fHandle, SE_FILE_OBJECT,
      Flags, anOwner,
      anGroup, list, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureFileObject.SetSecurityDescriptor(
  const SD: TJwSecurityDescriptor;
  const SD_entries: TJwSecurityInformationFlagSet);
var SDInfo : TJwSecurityInformationFlagSet;
begin
  SDInfo := SD_entries;

  //protect, unprotect ACL

  Exclude(SDInfo, siprotectedDaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedDaclSecurityInformation);
  Exclude(SDInfo, siprotectedSaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedSaclSecurityInformation);

  case SD.InheritanceDACLProtection of
    aclpProtected      : Include(SDInfo, siprotectedDaclSecurityInformation);
    aclpForceUnprotect : Include(SDInfo, siUnprotectedDaclSecurityInformation);
  end;

  case SD.InheritanceSACLProtection of
    aclpProtected      : Include(SDInfo, siprotectedSaclSecurityInformation);
    aclpForceUnprotect : Include(SDInfo, siUnprotectedSaclSecurityInformation);
  end;

    


  if (fFileName <> '') then
  begin
    SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      SDInfo, SD);
  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_FILE_OBJECT, SDInfo, SD);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetSecurityDescriptor', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureFileObject.GetSecurityDescriptor(
  const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;

begin
  if (fFileName <> '') then
  begin
    result := GetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT, SD_entries);
  end
  else
  if HasValidHandle then
  begin
    result := GetSecurityInfo(fHandle, SE_FILE_OBJECT, SD_entries);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetSecurityDescriptor', ClassName,
      RsUNSecureObjects, 0, False, []);
end;


procedure TJwSecureFileObject.SetGroup(const ID: TJwSecurityId);
var
  anOwner: TJwSecurityId;
  //[Hint] anGroup : TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  anOwner := nil;
  //[Hint] anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if not Assigned(ID) then
  begin
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetOwner', ClassName,
      RsUNSecureObjects, 0, False, ['ID']);
    exit;
  end;

  if (fFileName <> '') then
  begin
    SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner, ID, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siGroupSecurityInformation], anOwner, ID, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureFileObject.SetOwner(const ID: TJwSecurityId);
var
  //[Hint] anOwner : TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
begin
  //[Hint] anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if not Assigned(ID) then
  begin
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetOwner', ClassName,
      RsUNSecureObjects, 0, False, ['ID']);
    exit;
  end;


  if (fFileName <> '') then
  begin
    SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      [siOwnerSecurityInformation], ID, anGroup, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_FILE_OBJECT,
      [siOwnerSecurityInformation], ID, anGroup, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureFileObject.SetSACL(const list: TJwSAccessControlList;
  const Protection : TJwACLProtectionState = apNone);
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  Flags : TJwSecurityInformationFlagSet;

begin
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  Flags := [siSaclSecurityInformation];
  if Protection = apProtected then
    Include(Flags, siProtectedSaclSecurityInformation)
  else
  if Protection = apUnprotected then
    Include(Flags, siUnprotectedSaclSecurityInformation);

  if (fFileName <> '') then
  begin
    //first delete old SACL
    if AutoResetACL then
      SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
        [siSaclSecurityInformation], anOwner,
        anGroup, aDACL, nil);

    SetNamedSecurityInfo(TJwPChar(fFileName), SE_FILE_OBJECT,
      Flags, anOwner,
      anGroup, aDACL, list);
  end
  else
  if HasValidHandle then
  begin
    //first delete old SACL
    if AutoResetACL then
      SetSecurityInfo(fHandle, SE_FILE_OBJECT,
        [siSaclSecurityInformation], anOwner,
        anGroup, aDACL, nil);

    SetSecurityInfo(fHandle, SE_FILE_OBJECT,
      Flags, anOwner,
      anGroup, aDACL, list);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;


class procedure TJwSecureFileObject.TreeResetNamedSecurityInfo(
  pObjectName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  //  SECURITY_INFORMATION SecurityInfo,
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const bKeepExplicit: boolean;
  const fnProgress:
  TJwProgInvokeSetting;
  const FNProgressMethod: TJwFnProgressMethod;
  const FNProgressProcedure:
  TJwFnProgressProcedure;
  const ProgressUserData: Pointer);
begin
  inherited TreeResetNamedSecurityInfo(
    pObjectName,//pObjectName : TJwString;
    SE_File_Object,
    aSecurityInfo,
    //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
    Owner,//const Owner : TJwSecurityId;
    Group,//const Group : TJwSecurityId;
    DACL, //const DACL : TJwDAccessControlList;
    SACL, //const SACL : TJwSAccessControlList;
    bKeepExplicit,//const bKeepExplicit : Boolean;
    fnProgress,//const fnProgress : TJwProgInvokeSetting;
    FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
    FNProgressProcedure,//const FNProgressProcedure : TJwFnProgressProcedure;
    ProgressUserData//const ProgressUserData : Pointer);
    );

end;

function TJwSecureFileObject.GetInheritanceSource(
  const pObjectName: TJwString;
  //LPTSTR pObjectName,
  const aSecurityInfo:
  TJwSecurityInformationFlagSet; //SECURITY_INFORMATION SecurityInfo,
  const bIsDirectory: boolean;
  //BOOL Container,
  const GUIDs: TJwGuidArray;
  //GUID** pObjectClassGuids,
  const FN_OBJECT_MGR_FUNCTS: Pointer =
  nil): TJwInheritedFromArray;
  {PINHERITED_FROM pInheritArray}
var
  ACL: TJwSecurityAccessControlList;
  pObjectNameTemp: TJwString;
begin
  if siSaclSecurityInformation in aSecurityInfo then
    ACL := GetTempSACL
  else
    ACL := GetTempDACL;

  pObjectNameTemp := pObjectName;
  if Length(pObjectNameTemp) = 0 then
    pObjectNameTemp := fFileName;
  if Length(pObjectNameTemp) = 0 then
    pObjectNameTemp := IntToStr(Handle);


  Result := inherited GetInheritanceSource(pObjectNameTemp,
    SE_FILE_OBJECT, aSecurityInfo, bIsDirectory, GUIDs, ACL,
    TJwSecurityFileMapping, FN_OBJECT_MGR_FUNCTS);
end;


class procedure TJwSecureFileObject.RemoveInheritanceFlow(
  const Handle: THandle; const bCopyInheritedACEs: boolean = True);
var //[Hint] DACL : TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
    SE_FILE_OBJECT, //const aObjectType : TSeObjectType;
    [siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
    );
  try
    if Assigned(SD.DACL) then
    begin
      if bCopyInheritedACEs then
        SD.DACL.ConvertInheritedToExplicit
      else
        SD.DACL.RemoveInherited;


      TJwSecureGeneralObject.SetSecurityInfo(Handle,
        SE_FILE_OBJECT,  //const aObjectType : TSeObjectType;
        [siDaclSecurityInformation,
        siProtectedDaclSecurityInformation],
          //aSecurityInfo : TJwSecurityInformationFlagSet;
        nil,//const anOwner : TJwSecurityId;
        nil,//const anGroup : TJwSecurityId;
        SD.DACL,
        //nil,//const aDACL : TJwDAccessControlList;
        nil//const aSACL : TJwSAccessControlList);
        );
    end;
  finally
    SD.Free;
  end;
end;

class procedure TJwSecureFileObject.RemoveInheritanceFlow(
  const PathName: TJwString; const bCopyInheritedACEs: boolean = True);
var //[Hint] DACL : TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  SD := TJwSecureGeneralObject.GetNamedSecurityInfo(PathName,
    SE_FILE_OBJECT, //const aObjectType : TSeObjectType;
    [siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
    );
  try
    if Assigned(SD.DACL) then
    begin
      if bCopyInheritedACEs then
        SD.DACL.ConvertInheritedToExplicit
      else
        SD.DACL.RemoveInherited;


      TJwSecureGeneralObject.SetNamedSecurityInfo(PathName,
        SE_FILE_OBJECT,   //const aObjectType : TSeObjectType;
        [siDaclSecurityInformation,
        siProtectedDaclSecurityInformation],
        //aSecurityInfo : TJwSecurityInformationFlagSet;
        nil,//const anOwner : TJwSecurityId;
        nil,//const anGroup : TJwSecurityId;
        SD.DACL,
        //nil,//const aDACL : TJwDAccessControlList;
        nil//const aSACL : TJwSAccessControlList);
        );
    end;
  finally
    SD.Free;
  end;
end;

procedure TJwSecureFileObject.RemoveInheritanceFlow(
  const bCopyInheritedACEs: boolean = True);
begin
  if (fFileName <> '') then
  begin
    RemoveInheritanceFlow(fFileName, bCopyInheritedACEs);
  end
  else
  if HasValidHandle then
  begin
    RemoveInheritanceFlow(fHandle, bCopyInheritedACEs);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'RemoveInheritanceFlow',
      ClassName, RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureFileObject.TakeOwnerShip(SID: TJwSecurityId = nil);
begin
  if (fFileName <> '') then
  begin
    TakeOwnerShip(fFileName, SID);
  end
  else
  if HasValidHandle then
  begin
    TakeOwnerShip(fHandle, SID);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'TakeOwnerShip', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

class procedure TJwSecureFileObject.TakeOwnerShip(const PathName: TJwString;
  SID: TJwSecurityId = nil);
begin
  TJwSecureBaseClass.TakeOwnerShip(PathName, SE_FILE_OBJECT, SID);
end;

class procedure TJwSecureFileObject.TakeOwnerShip(const Handle: THandle;
  SID: TJwSecurityId = nil);
begin
  TJwSecureBaseClass.TakeOwnerShip(Handle, SE_FILE_OBJECT, SID);
end;

class procedure TJwSecureFileObject.RestoreInheritanceFlow(
  const Handle: THandle; bTakeOwnerShip: boolean = False);
var
  DACL: TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  try
    SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
      SE_FILE_OBJECT, //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
      );

  except
    on E: EJwsclWinCallFailedException do
    begin
      if not bTakeOwnerShip then
        raise;

      //this only happens if we are not the owner and have not WRITE_DAC
      if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      begin
        raise EJwsclAdaptSecurityInfoException.CreateFmtEx(
          RsSecureObjectsDaclAdaptionFailed,
          'RestoreInheritanceFlow', ClassName, RsUNSecureObjects, 0, True,
          []);
      end;

      TakeOwnerShip(Handle);

      //again
      SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
        SE_FILE_OBJECT, //const aObjectType : TSeObjectType;
        [siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
        );
    end;
  end;


  try
    //[Hint] DACL := nil;
    if not Assigned(SD.DACL) then
    begin
      DACL := TJwDAccessControlList.Create;
      SD.DACL := DACL;
      if not SD.OwnDACL then
        DACL.Free;
    end;

    TJwSecureGeneralObject.SetSecurityInfo(Handle,
      SE_FILE_OBJECT,   //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation,
      siUnprotectedDaclSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      nil,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      SD.DACL,
      //nil,//const aDACL : TJwDAccessControlList;
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    SD.Free;
  end;
end;



class procedure TJwSecureFileObject.RestoreInheritanceFlow(
  const PathName: TJwString; bTakeOwnerShip: boolean = False);
var
  DACL: TJwDAccessControlList;
  SD: TJwSecurityDescriptor;

begin
  SetLastError(0);


  try
    SD := TJwSecureGeneralObject.GetNamedSecurityInfo(PathName,
      SE_FILE_OBJECT,   //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
      );
  except
    on E: EJwsclWinCallFailedException do
    begin
      if not bTakeOwnerShip then
        raise;

      //this only happens if we are not the owner and have not WRITE_DAC
      if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      begin
        raise EJwsclAdaptSecurityInfoException.CreateFmtEx(
          RsSecureObjectsDaclAdaptionFailed,
          'RestoreInheritanceFlow', ClassName, RsUNSecureObjects,
          0, True, []);
      end;

      TakeOwnerShip(PathName);

      //again
      SD := TJwSecureGeneralObject.GetNamedSecurityInfo(PathName,
        SE_FILE_OBJECT,
        //const aObjectType : TSeObjectType;
        [
        siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
        );
    end;
  end;

  try
    //[Hint] DACL := nil;
    if not Assigned(SD.DACL) then
    begin
      DACL := TJwDAccessControlList.Create;
      SD.DACL := DACL;
      if not SD.OwnDACL then
        DACL.Free;
    end;

    TJwSecureGeneralObject.SetNamedSecurityInfo(PathName,
      SE_FILE_OBJECT,
      //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation,
      siUnprotectedDaclSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      nil,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      SD.DACL,
      //nil,//const aDACL : TJwDAccessControlList;
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    SD.Free;
  end;
end;

procedure TJwSecureFileObject.RestoreInheritanceFlow(
  bTakeOwnerShip: boolean = False);
begin
  if (fFileName <> '') then
  begin
    RestoreInheritanceFlow(fFileName, bTakeOwnerShip);
  end
  else
  if HasValidHandle then
  begin
    RestoreInheritanceFlow(fHandle, bTakeOwnerShip);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'RestoreInheritanceFlow',
      ClassName, RsUNSecureObjects, 0, False, []);
end;


function TJwSecureFileObject.GetFileInheritanceSource(
  const Container : Boolean;
  const aSecurityInfo: TJwSecurityInformationFlagSet =
  [siDaclSecurityInformation]): TJwInheritedFromArray;
begin
  if Length(fFileName) = 0 then
    raise EJwsclInvalidPathException.CreateFmtEx(
      RsSecureObjectsInvalidFileOrFolder,
      'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, []);

  Result := GetFileInheritanceSource(fFileName, Container, aSecurityInfo);
end;


type
  TWow64DisableWow64FsRedirection = function(OldValue: Pointer): boolean;
    stdcall;


type
  TWow64RevertWow64FsRedirection = function(OldValue: Pointer): boolean;
    stdcall;


var
  _Wow64FsRedirection_Critical: CRITICAL_SECTION;


procedure Wow64FsRedirection(disable: boolean);
var
  _Wow64RevertWow64FsRedirection:  TWow64RevertWow64FsRedirection;
  _Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
begin
  //EnterCriticalSection(_Wow64FsRedirection_Critical);
  _Wow64RevertWow64FsRedirection  := nil;
  _Wow64DisableWow64FsRedirection := nil;

  try
    if disable then
    begin
      GetProcedureAddress(@_Wow64DisableWow64FsRedirection,
        kernel32, 'Wow64DisableWow64FsRedirection');
      _Wow64DisableWow64FsRedirection(nil);
    end
    else
    begin
      GetProcedureAddress(@_Wow64RevertWow64FsRedirection,
        kernel32, 'Wow64RevertWow64FsRedirection');
      _Wow64RevertWow64FsRedirection(nil);
    end;
  except
    on E1: EJwaGetProcAddressError do
    begin
      //0
    end;
  end;

  //LeaveCriticalSection(_Wow64FsRedirection_Critical);
end;


{******
 class procedure TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo(pObjectName : TJwString;
}



procedure IterateFolder(const Path: TJwString;
  aSecurityInfo:
  TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
  const Action:
  TJwProgInvokeSetting;
  const SetType: TJwTreeSetType;
  const bKeepExplicit: boolean;
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const FNProgressMethod:
  TJwFnProgressMethod;
  const FNProgressProcedure: TJwFnProgressProcedure;
  const ProgressUserData
  : Pointer;
  bIsRoot: boolean;
  out bAbort: boolean);




  procedure FNProgress(const pObjectName: TJwString;
    // Name of object just processed
  const cStatus: Cardinal;
    // Status of operation on object
  var pInvokeSetting: TJwProgInvokeSetting; // When to set
  const E: EJwsclSecurityException;
  const Args: Pointer;
    // Caller specific data
  const bSecuritySet:
    boolean                 // Whether security was set
    );
  begin
    if Assigned(FNProgressMethod) then
      try
        FNProgressMethod(pObjectName, cStatus, pInvokeSetting,
          E, Args, bSecuritySet);
      except
      end;

    if Assigned(FNProgressProcedure) then
      try
        FNProgressProcedure(pObjectName, cStatus, pInvokeSetting,
          E, Args, bSecuritySet);
      except
      end;
  end;


  function CombinePath(Path1, Path2: TJwString): TJwString;
  begin
    if (Length(Path1) > 0) and (Path1[Length(Path1)] <> '\') then
      Path1 := Path1 + '\';
    Result := Path1 + Path2;
  end;

type
  PFindData = ^TFindData;

  TFindData = record
    PathName: TJwString;
  end;

var
  hFind: THandle;
  FindFileData:
{$IFDEF UNICODE}WIN32_FIND_DATAW{$ELSE}
  WIN32_FIND_DATAA
{$ENDIF}
  ;
  stack: TQueue;
  CurrentFileName, CurrentPath: TJwString;

  ptrFindData: PFindData;

  bFirst,
  //[Hint] bCallback,
  bError: boolean;
  pInvokeSetting: TJwProgInvokeSetting;
  Count:  integer;

    {<B>SetSecurity</B> sets the security of a file or folder
     @param CurrentFileName absolute path to file or folder
     @param bRoot defines, if the folder is the first one specified the the user
    }
  procedure SetSecurity(CurrentFileName: TJwString; bRoot: boolean);

  {not necessary}
    procedure CopyWithoutInheritance(
    const SourceACL, DestACL: TJwSecurityAccessControlList);
    var
      i: integer;
      //[Hint] c : Integer;
      s: TJwSecurityAccessControlEntry;
    begin
      for i := SourceACL.Count - 1 downto 0 do
      begin
        if not ((afObjectInheritAce in
          SourceACL.Items[i].Flags) and
          (afContainerInheritAce in SourceACL.Items[i].Flags) and
          (afNoPropagateInheritAce in SourceACL.Items[i].Flags) and
          (afInheritOnlyAce in
          SourceACL.Items[i].Flags)) and
          ([] <> SourceACL.Items[i].Flags) then
        begin
          s := TJwSecurityAccessControlEntry.CreateACE(SourceACL[i].AceType);
          s.Assign(SourceACL[i]);
          DestACL.Add(s);
        end;
      end;
    end;

  var
    oldUser, oldGroup: TJwSecurityId;

    oldDACL: TJwDAccessControlList;
    oldSACL: TJwSAccessControlList;

  begin
    oldUser  := nil;
    oldGroup := nil;
    oldDACL  := nil;
    oldSACL  := nil;

      {the Set type simply adds the given DACL/SACL to the file security.
       The Reset type does the same, but the inheritance protection is removed after the root object was changed
       }
    if bKeepExplicit or (SetType = tstSet) then
    begin
      TJwSecureFileObject.GetNamedSecurityInfo(CurrentFileName, SE_FILE_OBJECT,
        aSecurityInfo,
        oldUser, oldGroup, oldDACL, oldSACL);

      if Assigned(oldDACL) then
      begin
        if bRoot and Assigned(DACL) then
          oldDACL.AddAces(DACL)
        //copy all ACEs of the given ACL to the file/folder ACL
        ;
          (*else
          {not necessary }
            CopyWithoutInheritance(DACL,oldDACL);
            *)
        oldDACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;
      if Assigned(oldSACL) then
      begin
        if bRoot and Assigned(SACL) then
          oldSACL.AddAces(SACL);
        //else
        //  CopyWithoutInheritance(SACL,oldSACL);

        oldSACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;

      try
        TJwSecureFileObject.SetNamedSecurityInfo(
          CurrentFileName, SE_FILE_OBJECT,
          aSecurityInfo, Owner, Group, oldDACL, oldSACL);
      finally
        FreeAndNil(oldUser);
        FreeAndNil(oldGroup);
        FreeAndNil(oldDACL);
        FreeAndNil(oldSACL);
      end;
    end
    else
    begin
      if Assigned(DACL) then
      begin
        oldDACL := TJwDAccessControlList.Create(True);

        if bRoot and Assigned(DACL) then
          oldDACL.Assign(DACL);
        //else
        //  CopyWithoutInheritance(DACL,oldDACL);

        oldDACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;
      if Assigned(SACL) then
      begin
        oldSACL := TJwSAccessControlList.Create(True);

        if bRoot and Assigned(SACL) then
          oldSACL.Assign(SACL);
        //else
        //  CopyWithoutInheritance(SACL,oldSACL);

        oldSACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;

      try
        TJwSecureFileObject.SetNamedSecurityInfo(
          CurrentFileName, SE_FILE_OBJECT,
          aSecurityInfo, Owner, Group, oldDACL, oldSACL);

      finally
        FreeAndNil(oldDACL);
        FreeAndNil(oldSACL);
      end;
    end;
  end;

    {<B>ProcessFile</B> processes the current find file data.
     Sets the security, catches a possible exception and calls the callback function.

     @param FindDataFile the file or folder name (but not path) returned by the FindFirst/FindNext function
     @param CurrentFileName Defines the current path to the file or folder given in FindDataFile. It must also include it at the end of path.
     @param bRoot defines whether this folder is the path given by the first function call. All other calls use FALSE.

     @return Returns true if the callback function defined pis_ProgressCancelOperation as the next operation. In this case bAbort is set to true.
    }
  function ProcessFile(const FindDataFile, CurrentFileName: TJwString;
    bRoot: boolean = False): boolean;
  begin
    Result := False;
    if (FindDataFile <> '.') and (FindDataFile <> '..') then
    begin
      repeat
        pInvokeSetting := Action;
        bError := False;

        try
              {problem - der hauptordner muss noch gesetzt werden
               also das ganze repeat zeugs am besten in eine prozedur auslagern

               DACL einer/s Datei/Ordner setzen

               wenn hauptordner
                0. DACL D/F aus aktuelle Ordner lesen
                1. Lösche alle indirekten ACE aus DACL D/F
                 hier problem, was passiert mit den indirekten? vllt noch boolean parameter
                 der angibt, ob behalten oder nicht?

                1. setze die indirekten (object_inheritance usw) ACEs aus Data.DACL
                2. wenn not Data.bKeepExplicit, alle eplizite ACEs aus DACL löschen
                3. setze explizite ACEs aus Data.DACL
                4. DACL Datei/Folder löschen
                5. DACL D/F setzen
               sonst
                1. DACL D/F aus aktuelle Datei/Ordner lesen
                2. wenn not Data.bKeepExplicit, alle eplizite ACEs aus DACL löschen
                3. lösche alle indirekten ACEs aus DACL
                4. setze explizite ACEs aus Data.DACL
                5. DACL D/F löschen
                6. DACL D/F setzen }
          //remove old DACL
               { if (siDaclSecurityInformation in aSecurityInfo) then
                begin
                  TJwSecureFileObject.SetNamedSecurityInfo(
                    CurrentFileName,SE_FILE_OBJECT,
                          [siDaclSecurityInformation], nil, nil, nil, nil);
                end;
                //remove old SACL
                if (siSaclSecurityInformation in aSecurityInfo) then
                begin
                  TJwSecureFileObject.SetNamedSecurityInfo(
                    CurrentFileName,SE_FILE_OBJECT,
                          [siSaclSecurityInformation], nil, nil, nil, nil);
                end;


                TJwSecureFileObject.SetNamedSecurityInfo(
                  CurrentFileName,SE_FILE_OBJECT,
                          aSecurityInfo, Owner, Group, DACL, SACL);
            }
          SetSecurity(CurrentFileName, bRoot);
        except
          on E: EJwsclSecurityException do
          begin
            bError := True;

            if (Action = pis_ProgressInvokeOnError) then
              FNProgress(CurrentFileName,
                //const pObjectName : TJwString;              // Name of object just processed
                GetLastError(),
                //const cStatus : Cardinal;                    // Status of operation on object
                pInvokeSetting,
                //var pInvokeSetting : TJwProgInvokeSetting; // When to set
                E,     //const E : EJwsclSecurityException;
                ProgressUserData,
                //const Args : Pointer;                      // Caller specific data
                False
                //const bSecuritySet : Boolean                 // Whether security was set
                );
          end;
        end;
        if not bError and (Action = pis_ProgressInvokeEveryObject) then
        begin
          FNProgress(CurrentFileName,
            //const pObjectName : TJwString;              // Name of object just processed
            0,
            //const cStatus : Cardinal;                    // Status of operation on object
            pInvokeSetting,
            //var pInvokeSetting : TJwProgInvokeSetting; // When to set
            nil,     //const E : EJwsclSecurityException;
            ProgressUserData,
            //const Args : Pointer;                      // Caller specific data
            True
            //const bSecuritySet : Boolean                 // Whether security was set
            );
        end;
        //repeat if action is a retry operation
      until (pInvokeSetting <> pis_ProgressRetryOperation);

      if (pInvokeSetting = pis_ProgressCancelOperation) then
      begin
        bAbort := True;
        Result := True;
        exit;
      end;

    end;
  end;

begin
  CurrentPath := Path;
  stack := TQueue.Create;

  bFirst := True;

  bAbort := False;


  FillChar(FindFileData, sizeof(FindFileData), 0);
  //set security to
  bAbort  := ProcessFile(CurrentPath, CurrentPath, bIsRoot{isRootObject});
  bIsRoot := False;

  if Assigned(DACL) then
    DACL.RemoveInherited;
  if Assigned(SACL) then
    SACL.RemoveInherited;

  Exclude(aSecurityInfo, siProtectedDaclSecurityInformation);
  Exclude(aSecurityInfo, siProtectedSaclSecurityInformation);


  //remove protection from all children if reset is defined
  //to let the inheritance flow flow
  if (SetType = tstReset) then
    Include(aSecurityInfo, siUnprotectedDaclSecurityInformation);

  if (SetType = tstReset) then
    Include(aSecurityInfo, siUnprotectedSaclSecurityInformation);


  {stack.Count = 0 and bFirst}
  while ((stack.Count > 0) or bFirst) and not bAbort do
  begin
    Count := stack.Count;
    if Count = 0 then;


    //[Hint] ptrFindData := nil;
    {stack.Count > 0 or bFirst and ptrFindData = nil}
    if (stack.Count > 0) then
    begin
      ptrFindData := PFindData(stack.Pop);
      CurrentPath  := ptrFindData.PathName;
      FreeMem(ptrFindData);
      {stack.Count = stack.Count-1 and ptrFindData <> nil}
    end
    else
    if not bFirst then
      break;

    bFirst := False;


    SetLastError(0);
{$IFDEF UNICODE}
    hFind := FindFirstFileExW(TJwPChar(CurrentPath+'\*.*'),//  LPCTSTR lpFileName,
{$ELSE}
    hFind := FindFirstFileExA(TJwPChar(CurrentPath + '\*.*'),
      //  LPCTSTR lpFileName,
{$ENDIF}
      FindExInfoStandard,//FINDEX_INFO_LEVELS fInfoLevelId,
      @FindFileData,//LPVOID lpFindFileData,
      FindExSearchNameMatch,//FINDEX_SEARCH_OPS fSearchOp,
      nil,//LPVOID lpSearchFilter,
      0//DWORD dwAdditionalFlags
      );
    if hFind <> INVALID_HANDLE_VALUE then
    begin
      CurrentFileName := CombinePath(CurrentPath, FindFileData.cFileName);

      //OutputDebugString(TJwPChar(CurrentFileName+#13));

      bAbort := ProcessFile(TJwString(FindFileData.cFileName), CurrentFileName);

      if (TJwString(FindFileData.cFileName) <> '.') and
        (TJwString(FindFileData.cFileName) <> '..') and
        (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
        FILE_ATTRIBUTE_DIRECTORY) then
      begin
        GetMem(ptrFindData, sizeof(TFindData));
        FillChar(ptrFindData^, sizeof(TFindData), 0);
        ptrFindData.PathName := CurrentFileName;
        stack.Push(ptrFindData);
      end;

      while (GetLastError() <> ERROR_NO_MORE_FILES) do
      begin
        SetLastError(0);



{$IFDEF UNICODE}
        if not FindNextFileW(hFind, FindFileData) then
{$ELSE}
        if not FindNextFileA(hFind, FindFileData) then
{$ENDIF}
        begin
          if GetLastError() <> ERROR_NO_MORE_FILES then
          begin
            //callback function aufrufen bei fehler oder alles
          end;
        end
        else
        begin
          CurrentFileName := CombinePath(CurrentPath, FindFileData.cFileName);
          bAbort := ProcessFile(TJwString(FindFileData.cFileName),
            CurrentFileName);

          if (TJwString(FindFileData.cFileName) <> '.') and
            (TJwString(FindFileData.cFileName) <> '..') and
            (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
            FILE_ATTRIBUTE_DIRECTORY) then
          begin
            GetMem(ptrFindData, sizeof(TFindData));
            FillChar(ptrFindData^, sizeof(TFindData), 0);
            ptrFindData.PathName := CurrentFileName;
            stack.Push(ptrFindData);
          end;
        end;
      end;

      jwaWindows.FindClose(Cardinal(hFind));

      bIsRoot := False;
    end
    else
    if (GetLastError() <> ERROR_NO_MORE_FILES) then
    begin
      pInvokeSetting := pis_ProgressInvokeOnError;
      if (Action = pis_ProgressInvokeOnError) or
        (Action = pis_ProgressInvokeEveryObject) then
      begin
        FNProgress(CurrentPath,
          //const pObjectName : TJwString;              // Name of object just processed
          GetLastError(),
          //const cStatus : Cardinal;                    // Status of operation on object
          pInvokeSetting,
          //var pInvokeSetting : TJwProgInvokeSetting; // When to set
          nil,     //const E : EJwsclSecurityException;
          ProgressUserData,
          //const Args : Pointer;                      // Caller specific data
          False
          //const bSecuritySet : Boolean                 // Whether security was set
          );
      end;

      if (pInvokeSetting = pis_ProgressRetryOperation) then
        IterateFolder(CurrentPath,
          aSecurityInfo,
          //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
          Action,//const Action : TJwProgInvokeSetting;
          SetType,
          bKeepExplicit,

          Owner,//const Owner : TJwSecurityId;
          Group,//const Group : TJwSecurityId;
          DACL, //const DACL : TJwDAccessControlList;
          SACL, //const SACL : TJwSAccessControlList;
          FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
          FNProgressProcedure,
          //const FNProgressProcedure : TJwFnProgressProcedure;
          ProgressUserData,//const ProgressUserData : Pointer;
          bIsRoot,
          bAbort


          )
      else
      if bAbort or (pInvokeSetting = pis_ProgressCancelOperation) then
      begin
        bAbort := True;
        break;
      end;
    end;
  end;//end while

  while (stack.Count > 0) do
  begin
    ptrFindData := PFindData(stack.Pop);

    if ptrFindData <> nil then
      FreeMem(ptrFindData);
  end;


  FreeAndNil(stack);
end;



type
  PThreadData = ^TThreadData;

  TThreadData = record
    pObjectName: TJwString;
    aSecurityInfo: TJwSecurityInformationFlagSet;
    //  SECURITY_INFORMATION SecurityInfo,
    Action: TJwProgInvokeSetting;
    SetType: TJwTreeSetType;
    bKeepExplicit: boolean;
    Owner: TJwSecurityId;
    Group: TJwSecurityId;
    DACL:  TJwDAccessControlList;
    SACL:  TJwSAccessControlList;
    FNProgressMethod: TJwFnProgressMethod;
    FNProgressProcedure: TJwFnProgressProcedure;
    ProgressUserData: Pointer;
    Disable64Redirection: boolean;
    hThread: THandle;
  end;


class function TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo_Execute(
  pData: Pointer): integer;
  //function TreeFileObjectSetNamedSecurityInfo_ThreadFunc(Data : PThreadData) : Cardinal; stdcall;
var
  p: TJwProgInvokeSetting;
  b, bAbort: boolean;
  //[Hint] DACL : TJwDAccessControlList;
  //[Hint] SACL : TJwSAccessControlList;
  //[Hint] i : Integer;
  //[Hint] hThread : THandle;
  Data: PThreadData absolute pData;
begin
  Result := 1;
  if Data = nil then
    exit;

  if Data^.Disable64Redirection then
    Wow64FsRedirection(True);


  //[Hint] hThread := 0;

  {DACL := Data^.DACL;
  SACL := Data^.SACL;

  if Assigned(DACL) then;
  if Assigned(SACL) then;
                              }
  try
    b := Data^.bKeepExplicit;
    if b then;


    IterateFolder(Data^.pObjectName,
      Data^.aSecurityInfo,
      //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      Data^.Action,//const Action : TJwProgInvokeSetting;
      Data^.SetType,
      b,
      Data^.Owner,//const Owner : TJwSecurityId;
      Data^.Group,//const Group : TJwSecurityId;
      Data^.DACL, //const DACL : TJwDAccessControlList;
      Data^.SACL, //const SACL : TJwSAccessControlList;
      Data^.FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
      Data^.FNProgressProcedure,
      //const FNProgressProcedure : TJwFnProgressProcedure;
      Data^.ProgressUserData,//const ProgressUserData : Pointer;
      True, //first call of this function = true
      bAbort
      );
  finally
    if Data^.Disable64Redirection then
      Wow64FsRedirection(False);



    //call callback function, the thread has ended
    if Assigned(Data^.FNProgressMethod) then
      try
        p := pis_ProgressFinished;
        Data^.FNProgressMethod(Data^.pObjectName, 0, p, nil, Data^.ProgressUserData, False);
      except
        //don't let us get out of the concept!
        //so just ignore the users exception
        //After all, we're maybe in a thread!
      end;


    if Assigned(Data^.FNProgressProcedure) then
      try
        p := pis_ProgressFinished;
        Data^.FNProgressProcedure(Data^.pObjectName, 0, p, nil,
          Data^.ProgressUserData, False);
      except
        //don't let us get out of the concept!
        //so just ignore the users exception
        //After all, we're maybe in a thread!
      end;

    Data.Owner.Free;
    Data.Group.Free;
    Data.DACL.Free;
    Data.SACL.Free;

    //[Hint] hThread := Data^.hThread;

    FreeMem(Data);
  end;

  SetLastError(0);



  Result := 0;
end;




class procedure TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo(
  pObjectName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  //  SECURITY_INFORMATION SecurityInfo,
  const Action:
  TJwProgInvokeSetting;
  const SetType: TJwTreeSetType;
  const bKeepExplicit: boolean;
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const FNProgressMethod:
  TJwFnProgressMethod;
  const FNProgressProcedure: TJwFnProgressProcedure;
  const ProgressUserData
  : Pointer;
  const aThread: TJwTagThread;
  const Disable64Redirection:
  boolean = False);

var
  Data: PThreadData;
  aData: TThreadData;
  //[Hint] lpThreadId : Cardinal;
  bAbort: boolean;
  p: TJwProgInvokeSetting;
begin
  //hThreadHandle := INVALID_HANDLE_VALUE;
  Data := nil; //removes warning
  if Assigned(aThread) then
  begin
    GetMem(Data, sizeof(TThreadData));
    FillChar(Data^, sizeof(TThreadData), 0);

    if Data = nil then
      raise EJwsclNILParameterException.CreateFmtEx(
        RsSecureObjectsNotEnoughMemoryForThreadData, 'TreeFileObjectSetNamedSecurityInfo',
        ClassName, RsUNSecureObjects, 0, False, []);

    if Data <> nil then
    begin
      //create copies for thread

      if Assigned(Owner) then
        Data.Owner := TJwSecurityId(Owner);

      if Assigned(Group) then
        Data.Group := TJwSecurityId(Group);

      if Assigned(DACL) then
      begin
        Data.DACL := TJwDAccessControlList.Create();
        Data.DACL.Assign(DACL);
      end;

      if Assigned(SACL) then
      begin
        Data.SACL := TJwSAccessControlList.Create();
        Data.SACL.Assign(SACL);
      end;

      Data.pObjectName := pObjectName;

      Data.Disable64Redirection := Disable64Redirection;
      Data.aSecurityInfo := aSecurityInfo;
      //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      Data.Action  := Action;//const Action : TJwProgInvokeSetting;
      Data.SetType := SetType;
      Data.FNProgressMethod := FNProgressMethod;
      //const FNProgressMethod : TJwFnProgressMethod;
      Data.FNProgressProcedure := FNProgressProcedure;
      //const FNProgressProcedure : TJwFnProgressProcedure;
      Data.ProgressUserData := ProgressUserData;
      //const ProgressUserData : Pointer;
      Data.bKeepExplicit := bKeepExplicit;

       (* hThreadHandle :=
                 CreateThread(
                  nil,//LPSECURITY_ATTRIBUTES lpThreadAttributes,  // pointer to thread security attributes
                  0,//DWORD dwStackSize,  // initial thread stack size, in bytes
                  @TreeFileObjectSetNamedSecurityInfo_ThreadFunc,//LPTHREAD_START_ROUTINE lpStartAddress,  // pointer to thread function
                  Data,//LPVOID lpParameter,  // argument for new thread
                  CREATE_SUSPENDED,//DWORD dwCreationFlags,  // creation flags
                  @lpThreadId //LPDWORD lpThreadId   // pointer to returned thread identifier
                 );
                 *)
      aThread.fOnExecute := TreeFileObjectSetNamedSecurityInfo_Execute;
      aThread.pTag := Data;
      aThread.Resume;



        (*if (hThreadHandle = 0) and (GetLastError() <> 0) then
        begin

          //Free on error
          Data.Owner.Free;
          Data.Group.Free;
          Data.DACL.Free;
          Data.SACL.Free;
          FreeMem(Data);
          raise EJwsclThreadException.CreateFmtEx('Thread could not be started','TreeFileObjectSetNamedSecurityInfo',ClassName,RsUNSecureObjects,0,true,[]);
        end
        else
        begin
          Data^.hThread := hThreadHandle;
          ResumeThread(hThreadHandle);
        end;  *)

    end;
  end
  else
  begin
    if Disable64Redirection then
      Wow64FsRedirection(True);
    try
      //create copies for thread

      FillChar(aData, sizeof(aData), 0);
      if Assigned(Owner) then
        aData.Owner := TJwSecurityId(Owner);

      if Assigned(Group) then
        aData.Group := TJwSecurityId(Group);

      if Assigned(DACL) then
      begin
        aData.DACL := TJwDAccessControlList.Create();
        aData.DACL.Assign(DACL);
      end;

      if Assigned(SACL) then
      begin
        aData.SACL := TJwSAccessControlList.Create();
        aData.SACL.Assign(SACL);
      end;

      IterateFolder(pObjectName,
        aSecurityInfo,
        //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
        Action,//const Action : TJwProgInvokeSetting;
        SetType,
        bKeepExplicit,
        aData.Owner,//const Owner : TJwSecurityId;
        aData.Group,//const Group : TJwSecurityId;
        aData.DACL, //const DACL : TJwDAccessControlList;
        aData.SACL, //const SACL : TJwSAccessControlList;
        FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
        FNProgressProcedure,//const FNProgressProcedure : TJwFnProgressProcedure;
        ProgressUserData,//const ProgressUserData : Pointer;
        True, //first call of this function = true
        bAbort
        );

      if Assigned(FNProgressMethod) then
        try
          p := pis_ProgressFinished;
          FNProgressMethod(pObjectName, 0, p, nil, ProgressUserData, False);
        except
        end;

      if Assigned(FNProgressProcedure) then
        try
          p := pis_ProgressFinished;
          Data^.FNProgressProcedure(pObjectName, 0, p, nil,
            ProgressUserData, False);
        except
        end;

      aData.Owner.Free;
      aData.Group.Free;
      aData.DACL.Free;
      aData.SACL.Free;

    finally
      if Disable64Redirection then
        Wow64FsRedirection(True);
    end;

  end;
end;

class function TJwSecureFileObject.GetFileInheritanceSource(
  const PathName: TJwString;
  const Container : Boolean;
  const aSecurityInfo: TJwSecurityInformationFlagSet =
  [siDaclSecurityInformation];

  const OnGetNamedSecurityInfo : TJwOnGetNamedSecurityInfo = nil;
  const Data : Pointer = nil): TJwInheritedFromArray;

  function GetParent(PathName: TJwString): TJwString;
  var
    len,
    p1,
    i : integer;
  begin
    Result := '';
    len := Length(PathName);

    //prevents stack overflow for the caller
    if Len <= 3 then
    begin
      result := '';
      exit;
    end;

    p1 := 0;
    for i := len-1 downto 1 do //ignore last char - usually a \
    begin
      if PathName[i] = '\' then
      begin
        p1 := i;
        Break;
      end;
    end;

    if p1 = 0 then
      result := PathName
    else
      result := Copy(PathName, 1, p1);
  end;

  procedure RemoveGenericRights(ACL: TJwDAccessControlList);
  var i : Integer;
  begin
    for I := 0 to ACL.Count - 1 do
    begin
      ACL[i].AccessMask := TJwSecurityFileFolderMapping.GenericMap(ACL[i].AccessMask);
    end;
  end;


  function FormatAR(const Access : DWORD) : String;
  begin
    result := 'Human: '+ #13#10 + JwFormatAccessRights(Access, FileFolderMappingEx) + #13#10#13#10;
    result := result + 'Ex:    '#13#10 + JwFormatAccessRights(Access, FileFolderMappingEx) + #13#10;
  //  JwFormatStringEx()
  end;


  //Fill inherited array element with its data
  procedure UpdateInheritedArrayElement(var InhArray : TJwInheritedFromArray;
    const Index, Level : Integer; const PathName : TJwString; const SID : TJwSecurityId);
  var
    DomainName : TJwString;
    SidUse : Cardinal;
  begin
    try
      InhArray[Index].UserName := SID.GetAccountSidString('', DomainName, SidUse);
      InhArray[Index].System := DomainName; //new
    except
      InhArray[Index].UserName := '';
      InhArray[Index].System := SID.CachedSystemName;
    end;
    InhArray[Index].GenerationGap := Level;
    InhArray[Index].AncestorName := PathName;

    InhArray[Index].SIDString := SID.CachedSidString;

    //string format stays for compatibility 
    InhArray[Index].SID := InhArray[Index].UserName + '@' + InhArray[Index].SIDString;
  end;


  procedure UpdateObjectInheritedDACL(PathName: TJwString;
  const PreviousInhACL: TJwDAccessControlList;
  var pInhArray: TJwInheritedFromArray; Level: integer);

  var
    i, ps: integer;
    bError: boolean;
    SD:  TJwSecurityDescriptor;

    bOwnedSD : Boolean;
    //IncFlags : TJwInclusionFlag;


  begin
   bError := false;
    SD := nil;
    try
      bOwnedSD := false;
      if Assigned(OnGetNamedSecurityInfo) then
        SD := OnGetNamedSecurityInfo(PathName, SE_FILE_OBJECT, aSecurityInfo, bOwnedSD, Data)
      else
        SD := GetNamedSecurityInfo(PathName, SE_FILE_OBJECT, aSecurityInfo);
    except
      //bError := true; //file or folder not found
      raise;
    end;

    //bOwnedSD = true -> don't free it

    bError := bError or (not Assigned(SD) or (Assigned(SD) and not Assigned(SD.DACL)));

    if bError then
    begin
      for i := 0 to PreviousInhACL.Count - 1 do
      begin
        //set to invalid
        if pInhArray[i].GenerationGap = 0 then
        begin
          pInhArray[i].GenerationGap := -1;
        end;
      end;

      exit;
    end;

    {We can encounter GENERIC rights in a DACL of a file.
     So we convert them all to specific rights to make
     it possible to find the ACE for FindEqualACE.
    }
//    ShowMessage(SD.DACL.GetTextMap({nil));//}TJwSecurityFileFolderMapping));

    if not SD.DACLGenericRemoved then
    begin
      RemoveGenericRights(SD.DACL);
      SD.DACLGenericRemoved := true;
    end;

//    ShowMessage(PreviousInhACL.GetTextMap({nil));//}TJwSecurityFileFolderMapping));
//    ShowMessage(SD.DACL.GetTextMap({nil));//}TJwSecurityFileFolderMapping));



    //ShowMessage(PreviousInhACL.GetTextMap({nil));//}TJwSecurityFileFolderMapping));
    //ShowMessage(SD.DACL.GetTextMap({nil));//}TJwSecurityFileFolderMapping));


    // try
    for i := 0 to PreviousInhACL.Count - 1 do
    begin
       {SID := PreviousInhACL[i].SID.AccountName[''];
       if Sid = '' then;}

{$IFDEF DEBUG}
//      OutputDebugStringA(PAnsiChar(AnsiString(Format('%s, AM:%d',[PreviousInhACL[i].SID.StringSID, PreviousInhACL[i].AccessMask]))));
{$ENDIF DEBUG}

      {Fixed bug:
        Sometimes Windows creates inherited ACE differently than their parents.
        Their (inherited) access masks can have less bits set than their parents.
        So we check using eactSEAccessMask (smaller equal)
      }
   {   IncFlags := [efInherited];
      if Container then
        Include(ExclusionFlags, efLeaf)
      else
        Include(ExclusionFlags, ef)   }

      ps := SD.DACL.FindEqualACE(PreviousInhACL[i], [eactSameSid,
        eactSameAccessMask, eactSEAccessMask, eactSameType, eactSameFlags, eactGEFlags], -1, [],[efExplicit], true);

      //not inherited found
      //search for explicit only
      if ps < 0 then
        ps := SD.DACL.FindEqualACE(PreviousInhACL[i], [eactSameSid,
          eactSameAccessMask, eactSEAccessMask, eactSameType], -1, [], [efInherited]  ,false);

  //    ShowMessage(PreviousInhACL[i].GetTextMap(TJwSecurityFileFolderMapping));
      //ShowMessage( SD.DACL[i].GetTextMap(TJwSecurityFileFolderMapping));



{$IFDEF DEBUG}
//      OutputDebugStringA(PAnsiChar(AnsiString(Format('For %d found %d',[i,ps]))));
{$ENDIF DEBUG}

      if (ps >= 0) then
      begin
        //* The for loop variable I iterates through PreviousInhACL
        //* The variable ps points to an item in SD.DACL

        if //the previous ACE is inherited
        (afInheritedAce in PreviousInhACL[i].Flags) and
          //the current root ACE is not inherited
          not (afInheritedAce in SD.DACL[ps].Flags) and

          //if object is container, only check container inheritance
        {  ((Container and (afContainerInheritAce in SD.DACL[ps].Flags)) or
           (not Container and (afObjectInheritAce in SD.DACL[ps].Flags))) and}
          //not (afContainerInheritAce in SD.DACL[ps].Flags) and
          //current ACE is explicit
          not (PreviousInhACL[i].Ignore) then
          //root ACE was not already considered
        begin
          UpdateInheritedArrayElement(pInhArray, i, Level, PathName, SD.DACL[ps].SID);

          //This ACE has its inheritance, so don't touch it anymore
          PreviousInhACL[i].Ignore := True; //Ignore this ACE next time
        end
        else
        begin
          //ShowMessageFmt('Could not find the access entry at %d. AccessRights are %s ',[i,#13#10+FormatAR(PreviousInhACL[i].AccessMask)])
  {$IFDEF DEBUG}
  //        OutputDebugStringA(PAnsiChar(AnsiString(Format('Could not find the access entry at %d. AccessRights are %s ',[i,#13#10+FormatAR(PreviousInhACL[i].AccessMask)]))));
  {$ENDIF DEBUG}
        end;

      end;
    end;

    //Stop if this SD is protected.
    if not (sdcDaclProtected in SD.Control) then
    for i := 0 to PreviousInhACL.Count - 1 do
    begin
      if (afInheritedAce in PreviousInhACL[i].Flags) and
        not PreviousInhACL[i].Ignore then
      begin
        PathName := GetParent(PathName);

        {If sPathName is something like C:
         GetParent returns an empty string: there are no more parents.

         We cannot go deeper and stop here.
         BTW:
           It should not be possible that there is an inherited flag set
           on a ACE which is defined for the c: object
        }
        if Length(PathName) >= 3 then
        begin
          UpdateObjectInheritedDACL(PathName, PreviousInhACL,
              pInhArray, Level + 1);
        end
        else
        begin
          {In this case we set the error value.  }
          pInhArray[i].GenerationGap := -1;
        end;
        break;
      end;
    end;
    //finally
    if not bOwnedSD then
      FreeAndNil(SD);
    // end;
  end;


var
  sPathName: TJwString;
  bPrivEn: boolean;
  SD: TJwSecurityDescriptor;
  bOwnedSD : Boolean;
begin
  bPrivEn := False;

  if (aSecurityInfo <> [siDaclSecurityInformation]) and
    (aSecurityInfo <> [siSaclSecurityInformation]) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsParameterSiMissesDacl,
      'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, []);


 { if (aSecurityInfo = [siSaclSecurityInformation]) then
  begin
    if not JwIsPrivilegeSet(SE_SECURITY_NAME) then
      raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
        RsSecureObjectsPrivilegeSecurityMissing, 'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, [PathName]);

    bPrivEn := JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);
  end;
  }
  //SACL is not supported
  if (siSaclSecurityInformation in aSecurityInfo) then
  begin
    raise EJwsclUnimplemented.CreateFmtEx(
        RsUnimplementedFeature, 'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, [RsUnimplementedSACLInheritance]);

  end;

  //we need absolute path
  sPathName := ExpandFileName(PathName);
  //TODO: UNC???

  SetLength(Result, 0);
  try
    bOwnedSD := false;

    if Assigned(OnGetNamedSecurityInfo) then
      SD := OnGetNamedSecurityInfo(PathName, SE_FILE_OBJECT, aSecurityInfo, bOwnedSD, Data)
    else
      SD := GetNamedSecurityInfo(PathName, SE_FILE_OBJECT, aSecurityInfo);

    //bOwnedSD = true -> don't free it

    //do not use invalid SD or nil DACL
    if Assigned(SD) and Assigned(SD.DACL) then
    begin
      //also don't check if the current SD is protected
      //  and thus cannot contain inherited ACEs
      if (sdcDaclProtected in SD.Control) then
      begin
        SetLength(Result, SD.DACL.Count);
      end
      else
      begin
        try
          {We can encounter GENERIC rights in a DACL of a file.
           So we convert them all to specific rights to make
           it possible to find the ACE for FindEqualACE.
          }
          if not SD.DACLGenericRemoved then  //don't do it if already done
          begin
            RemoveGenericRights(SD.DACL);
            SD.DACLGenericRemoved := true;
          end;

          SetLength(Result, SD.DACL.Count);

          sPathName := GetParent(sPathName);

          {sPathName can be c:}
          if Length(sPathName) >= 3 then
            UpdateObjectInheritedDACL(sPathName, SD.DACL, Result, 1);

        finally
          if not bOwnedSD then
            SD.Free;
        end;
      end;
    end
    else
    begin
      //SetLength(Result, 0);
    end;

  finally
    //restore privilege to old value
    if (siSaclSecurityInformation in aSecurityInfo) and not bPrivEn then
      JwEnablePrivilege(SE_SECURITY_NAME, pst_Disable);
  end;

end;

(*

procedure UpdateObjectInheritedDACL(PathName : TJwString; var pInhArray : TJwInheritedFromArray; Pos, Level : Integer);
  function GetParent(PathName : TJwString; bStop : Boolean = false) : TJwString;
  var l,l2,i,i2 : Integer;
      s : TJwString;
  begin
    result := '';

    i := Length(PathName);
    while (i > 0) do
    begin
      if (i < Length(PathName)) and ((PathName[i] = '\') or (PathName[i] = '/') or (PathName[i] = ':')) then
        break;

      result := PathName[i] + result;

      Dec(i);
    end;
    for i2 := 1 to i do
      S := S + PathName[i2];

    if bStop then
      result := GetParent(s)
    else
      result := s;
  end;

var i : Integer;
    aDACL : TJwDAccessControlList;
    c : Integer;
    s : TJwString;
begin
  SD := GetNamedSecurityInfo(PathName, SE_FILE_OBJECT, [siDaclSecurityInformation]);


  try
    aDACL := SD.DACL;

    if Length(pInhArray) = 0 then
      SetLength(pInhArray, aDACL.Count);

      c := Pos;
      if Pos = -1 then
      begin
        Pos := 0;
        c := aDACL.Count -1;
      end;

      for i := Pos to c do
      begin
       //TODO !!!! hier nochmal überdenken mit pos, da parent nicht unbedingt gleiche ACLs hgaben müssen
      // suchen nach sID ?
        if (afInheritedAce in aDACL[i].Flags)
        //or (afContainerInheritAce in aDACL[i].Flags)
          then
        begin
          S := aDACL[i].GetText;
          if s = '' then;
          Inc(Level);
          PathName := GetParent(PathName);
          UpdateObjectInheritedDACL(PathName, pInhArray, i, Level);
        end
        else
        begin
          S := aDACL[i].GetText;
          if s = '' then;

          pInhArray[i].GenerationGap := Level-1;
          pInhArray[i].AncestorName := PathName;
        end;

      end;

  finally
    SD.Free;
    SD := nil;
  end;
end;
*)



class function TJwSecureFileObject.SupportACL(RootPathName: TJwString): boolean;
var
  dw1, dw2: DWORD;
begin
  RootPathName := ExpandFileName(RootPathName);

  RootPathName := ExtractFileDrive(RootPathName);

  if Length(RootPathName) = 0 then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsInvalidRootPathName, 'SupportACL', ClassName, '', 0, False,
      [RootPathName]);


  if RootPathName[Length(RootPathName)] <> '\' then
    RootPathName := RootPathName + '\';


  SetErrorMode(SEM_FAILCRITICALERRORS);

  dw2 := 0;
  dw1 := 0;
  if not
    {$IFDEF UNICODE}GetVolumeInformationW{$ELSE}
    GetVolumeInformationA
{$ENDIF}
    (TJwPChar(RootPathName), //LPCTSTR lpRootPathName,
    nil,//LPTSTR lpVolumeNameBuffer,
    0,//DWORD nVolumeNameSize,
    nil,//LPDWORD lpVolumeSerialNumber,
    dw1,//LPDWORD lpMaximumComponentLength,
    dw2,//LPDWORD lpFileSystemFlags,
    nil,//LPTSTR lpFileSystemNameBuffer,
    0 //DWORD nFileSystemNameSize
    ) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsSecureObjectsFailedReadingVolumeInformation, 'SupportACL',
      ClassName, '', 0, True, [RootPathName]);

  Result := (dw2 and FILE_PERSISTENT_ACLS = FILE_PERSISTENT_ACLS);
end;




{ TJwSecureRegistryKey }



{<B>TJwSecureRegistryKey.GetKey</B> returns the key opened by a create constructor, or
if a name was given, returns zero and sets bUsesName to true.
@param bUsesName Defines whether the instance was created by using a name.
@return Returns the key handle of the opened key.
     If the key is invalid the return value is 0.
}
function TJwSecureRegistryKey.GetKey(out bUsesName: boolean): HKEY;
begin
  bUsesName := False;
  Result := 0;

  if Assigned(fReg) then
    Result := fReg.CurrentKey
  else
  if (Handle <> 0) and HasValidHandle then
    Result := Handle
  else
  if (Handle <> 0) and not HasValidHandle then
    Result := 0
  else
    bUsesName := True;
end;

constructor TJwSecureRegistryKey.Create(const RegPath: TJwString);
begin
  fReg := nil;
  fHandle := 0;
  fKeyName := RegPath;
end;

constructor TJwSecureRegistryKey.Create(const RegistryInstance: TRegistry;
  bUseName: boolean = False);
  //  function RootKeyToString(const Key
begin
  JwRaiseOnNilParameter(RegistryInstance, 'RegistryInstance', 'Create', ClassName, RsUNSecureObjects);

  fReg := nil;
  fHandle := RegistryInstance.CurrentKey;
  //!!!!! F.RootKey
  fKeyName := RegistryInstance.CurrentPath;
  fAccessMask := RegistryInstance.Access;
end;

constructor TJwSecureRegistryKey.Create(const RegKey: HKEY;
  AccessMask: TJwAccessMask; bDuplicateHandle: boolean);
var
  d: TJwAccessMask;
begin
  fReg := nil;
  fKeyName := '';

  d := 0;

  if AccessMask = 0 then
    d := DUPLICATE_SAME_ACCESS;

  if not bDuplicateHandle then
    fHandle := RegKey
  else
  if not DuplicateHandle(GetCurrentProcess,
    //HANDLE hSourceProcessHandle,  // handle to process with handle to duplicate
    RegKey, //HANDLE hSourceHandle,  // handle to duplicate
    GetCurrentProcess,
    //HANDLE hTargetProcessHandle,  // handle to process to duplicate to
    @fHandle, //LPHANDLE lpTargetHandle,  // pointer to duplicate handle
    AccessMask, //DWORD dwDesiredAccess,  // access for duplicate handle
    False, //BOOL bInheritHandle,  // handle inheritance flag
    d //DWORD dwOptions   // optional actions
    ) then
    raise EJwsclSecurityObjectException.CreateFmtEx(
      RsSecureObjectsNoCopyOfObjectHandle, 'TJwSecureRegistryKey.Create',
      ClassName, RsUNSecureObjects, 0, True, []);

end;

destructor TJwSecureRegistryKey.Destroy;
begin
  if fDuplicateHandle and HasValidHandle then
    RegCloseKey(fHandle);
  fHandle := 0;

  inherited;
end;

procedure TJwSecureRegistryKey.SetSecurityDescriptor(
  const SD: TJwSecurityDescriptor;
  const SD_entries: TJwSecurityInformationFlagSet);
var
  bUsesName: boolean;
  SDInfo : TJwSecurityInformationFlagSet;
begin
  GetKey(bUsesName);

  //protect, unprotect ACL

  Exclude(SDInfo, siprotectedDaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedDaclSecurityInformation);
  Exclude(SDInfo, siprotectedSaclSecurityInformation);
  Exclude(SDInfo, siUnprotectedSaclSecurityInformation);

  case SD.InheritanceDACLProtection of
    aclpProtected      : Include(SDInfo, siprotectedDaclSecurityInformation);
    aclpForceUnprotect : Include(SDInfo, siUnprotectedDaclSecurityInformation);
  end;

  case SD.InheritanceSACLProtection of
    aclpProtected      : Include(SDInfo, siprotectedSaclSecurityInformation);
    aclpForceUnprotect : Include(SDInfo, siUnprotectedSaclSecurityInformation);
  end;

  //SE_REGISTRY_WOW64_32KEY is not supported here
  if (bUsesName) then
  begin
    SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY, SDInfo, SD);
  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_REGISTRY_KEY, SDInfo, SD);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetSecurityDescriptor', ClassName,
      RsUNSecureObjects, 0, False, []);
end;


function TJwSecureRegistryKey.GetSecurityDescriptor(
  const SD_entries: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
var
  bUsesName: boolean;
begin
  GetKey(bUsesName);

  //SE_REGISTRY_WOW64_32KEY is not supported here
  if (bUsesName) then
  begin
    result := GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY, SD_entries);
  end
  else
  if HasValidHandle then
  begin
    result := GetSecurityInfo(fHandle, SE_REGISTRY_KEY, SD_entries);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetSecurityDescriptor', ClassName,
      RsUNSecureObjects, 0, False, []);
end;


function TJwSecureRegistryKey.AccessCheck(DesiredAccess: TJwAccessMask =
  Cardinal(-1); const ClientToken:
  TJwSecurityToken = nil): boolean;
var
  SD: TJwSecurityDescriptor;
  privSet: TJwPrivilegeSet;
  GrantedAccess: Cardinal;
begin
  if DesiredAccess and MAXIMUM_ALLOWED <> 0 then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsInvalidParameterAccessCheck, 'AccessCheck',
      ClassName, RsUNSecureObjects, 0, False, []);
  end;

  SD := GetSecurityDescriptor([siOwnerSecurityInformation,
    siGroupSecurityInformation, siDaclSecurityInformation]);

  if DesiredAccess = Cardinal(-1) then
    DesiredAccess := fAccessMask;


  privSet := nil;
  try
    Self.AccessCheck(SD, ClientToken, DesiredAccess, TJwSecurityRegistryMapping,
      privSet, GrantedAccess, Result)
  finally
    privSet.Free;
    SD.Free;
  end;
end;

class procedure TJwSecureRegistryKey.AccessCheck(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);
begin
  inherited AccessCheck(SecurityDescriptor,
    ClientToken,
    DesiredAccess,
    TJwSecurityFileMapping,
    PrivilegeSet,
    GrantedAccess,
    AccessStatus);
end;


class procedure TJwSecureRegistryKey.AccessCheckAndAuditAlarm(
  const SubsystemName: TJwString; const HandleID: Pointer;
  const ObjectTypeName, ObjectName: TJwString;
  const SecurityDescriptor: TJwSecurityDescriptor;
  const DesiredAccess: TJwAccessMask;
  const GenericMapping: TJwSecurityGenericMappingClass;
  const ObjectCreation: boolean; out GrantedAccess: TJwAccessMask;
  out AccessStatus, pfGenerateOnClose: boolean);
begin
  inherited;
end;


function TJwSecureRegistryKey.GetDACL: TJwDAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aSACL: TJwSAccessControlList;
  bUsesName: boolean;

begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aSACL := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetGroup: TJwSecurityId;
var
  anOwner: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
  bUsesName: boolean;
begin
  Result := nil;
  anOwner := nil;
  aDACL := nil;
  aSACL := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetGroup', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetOwner: TJwSecurityId;
var

  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;

  bUsesName: boolean;
begin
  Result := nil;
  //[Hint] anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetOwner', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureRegistryKey.SetMandatoryLabel(const MandatoryLabel :
      TJwSystemMandatoryAccessControlEntry);
var
  SACL : TJwSAccessControlList;
  bUsesName: boolean;
begin
  SACL := TJwSAccessControlList.Create;
  SACL.SetMandatoryLabel(MandatoryLabel,cfCopyInstance);

  GetKey(bUsesName);
  try
    if (bUsesName) then
    begin
      SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
        [siLabelSecurityInformation],nil,nil,nil,SACL);
    end
    else
    if HasValidHandle then
    begin
      SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
        [siLabelSecurityInformation],nil,nil,nil,SACL);
    end
    else
      raise EJwsclInvalidObjectException.CreateFmtEx(
        RsSecureObjectsInvalidRegPathHandle, 'SetMandatoryLabel', ClassName,
        RsUNSecureObjects, 0, False, []);
  finally
    SACL.Free;
  end;
end;

function TJwSecureRegistryKey.GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry;
var
  SD : TJwSecurityDescriptor;
  bUsesName: boolean;
begin
  Result := nil;

  GetKey({out}bUsesName);
  SD := nil;

  try
    if (bUsesName) then
    begin
      SD := GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
        [siLabelSecurityInformation]);

      if Assigned(SD.AuditACL) and SD.AuditACL.HasMandatoryLabel then
      begin
        result := TJwSystemMandatoryAccessControlEntry.Create(SD.AuditACL.MandatoryLabel);
      end;
    end
    else
    if HasValidHandle then
    begin
      SD := GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
        [siLabelSecurityInformation]);

      if Assigned(SD.AuditACL) and SD.AuditACL.HasMandatoryLabel then
      begin
        result := TJwSystemMandatoryAccessControlEntry.Create(SD.AuditACL.MandatoryLabel);
      end;
    end
    else
      raise EJwsclInvalidObjectException.CreateFmtEx(
        RsSecureObjectsInvalidRegPathHandle, 'GetMandatoryLabel', ClassName,
        RsUNSecureObjects, 0, False, []);
  finally
    SD.Free;
  end;
end;


function TJwSecureRegistryKey.GetSACL: TJwSAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  bUsesName: boolean;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetSACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetTempDACL: TJwDAccessControlList;
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aSACL: TJwSAccessControlList;
  bUsesName: boolean;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aSACL := nil;

  if Assigned(fDAccessControlList) then
  begin
    Result := fDAccessControlList;
    exit;
  end;

  fDAccessControlList.Free;
  fDAccessControlList := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);

    fDAccessControlList := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siDaclSecurityInformation], anOwner,
      anGroup, Result, aSACL);

    fDAccessControlList := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetTempGroup: TJwSecurityId;
var
  anOwner: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
  bUsesName: boolean;
begin
  Result := nil;
  anOwner := nil;
  aDACL := nil;
  aSACL := nil;

  if Assigned(fGroup) then
  begin
    Result := fGroup;
    exit;
  end;

  fGroup.Free;
  fGroup := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

    fGroup := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner,
      Result, aDACL, aSACL);

    fGroup := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetTempGroup', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetTempOwner: TJwSecurityId;

var
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;

  bUsesName: boolean;
begin
  Result := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if Assigned(fOwner) then
  begin
    Result := fOwner;
    exit;
  end;

  fOwner.Free;
  fOwner := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);

    fOwner := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], Result,
      anGroup, aDACL, aSACL);

    fOwner := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidRegPathHandle, 'GetTempOwner', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

function TJwSecureRegistryKey.GetTempSACL: TJwSAccessControlList;

var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;

  bUsesName: boolean;
begin
  Result := nil;
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;

  if Assigned(fSAccessControlList) then
  begin
    Result := fSAccessControlList;
    exit;
  end;

  fSAccessControlList.Free;
  fSAccessControlList := nil;

  GetKey(bUsesName);

  if (bUsesName) then
  begin
    GetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

    fSAccessControlList := Result;
  end
  else
  if HasValidHandle then
  begin
    GetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siSaclSecurityInformation], anOwner,
      anGroup, aDACL, Result);

    fSAccessControlList := Result;
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempSACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureRegistryKey.ResetTemp(resetState: TJwTempResetEnumSet);
begin
  inherited;
end;

procedure TJwSecureRegistryKey.SetDACL(const list: TJwDAccessControlList;
     const Protection : TJwACLProtectionState = apNone);
var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aSACL: TJwSAccessControlList;

  bUsesName: boolean;
  Flags : TJwSecurityInformationFlagSet;
begin

  anOwner := nil;
  anGroup := nil;
  aSACL := nil;


  GetKey(bUsesName);
  //List can be nil = NULL DACL

  Flags := [siDaclSecurityInformation];
  if Protection = apProtected then
    Include(Flags, siProtectedDaclSecurityInformation)
  else
  if Protection = apUnprotected then
    Include(Flags, siUnprotectedDaclSecurityInformation);

  if (bUsesName) then
  begin
    //first delete the old DACL
    if AutoResetACL then
      SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
        [siDaclSecurityInformation], anOwner,
        anGroup, nil, aSACL);

    SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      Flags, anOwner,
      anGroup, list, aSACL);
  end
  else
  if HasValidHandle then
  begin
    //first delete the old DACL
    if AutoResetACL then
      SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
        [siDaclSecurityInformation], anOwner,
        anGroup, nil, aSACL);

    SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      Flags, anOwner,
      anGroup, list, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureRegistryKey.SetGroup(const ID: TJwSecurityId);
var
  anOwner: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;

  bUsesName: boolean;
begin
  anOwner := nil;
  aDACL := nil;
  aSACL := nil;

  if not Assigned(ID) then
  begin
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetOwner', ClassName,
      RsUNSecureObjects, 0, False, ['ID']);
    exit;
  end;

  GetKey(bUsesName);
  if (bUsesName) then
  begin
    SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner, ID, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siGroupSecurityInformation], anOwner, ID, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;


procedure TJwSecureRegistryKey.SetOwner(const ID: TJwSecurityId);
var
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  aSACL: TJwSAccessControlList;
  bUsesName: boolean;
begin
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  if not Assigned(ID) then
  begin
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'SetOwner', ClassName,
      RsUNSecureObjects, 0, False, ['ID']);
    exit;
  end;

  GetKey(bUsesName);
  if (bUsesName) then
  begin
    SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], ID, anGroup, aDACL, aSACL);

  end
  else
  if HasValidHandle then
  begin
    SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      [siOwnerSecurityInformation], ID, anGroup, aDACL, aSACL);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

procedure TJwSecureRegistryKey.SetSACL(const list: TJwSAccessControlList;
  const Protection : TJwACLProtectionState = apNone);

var
  anOwner: TJwSecurityId;
  anGroup: TJwSecurityId;
  aDACL: TJwDAccessControlList;
  Flags : TJwSecurityInformationFlagSet;

  bUsesName: boolean;
begin
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;

  Flags := [siSaclSecurityInformation];
  if Protection = apProtected then
    Include(Flags, siProtectedSaclSecurityInformation)
  else
  if Protection = apUnprotected then
    Include(Flags, siUnprotectedSaclSecurityInformation);

  GetKey(bUsesName);
  if (bUsesName) then
  begin
    //first delete old SACL
    if AutoResetACL then
      SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
        [siSaclSecurityInformation], anOwner,
        anGroup, aDACL, nil);

    SetNamedSecurityInfo(TJwPChar(fKeyName), SE_REGISTRY_KEY,
      Flags, anOwner,
      anGroup, aDACL, list);

  end
  else
  if HasValidHandle then
  begin
    //first delete old SACL
    if AutoResetACL then
      SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
        [siSaclSecurityInformation], anOwner,
        anGroup, aDACL, nil);

    SetSecurityInfo(fHandle, SE_REGISTRY_KEY,
      Flags, anOwner,
      anGroup, aDACL, list);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'GetTempDACL', ClassName,
      RsUNSecureObjects, 0, False, []);
end;



constructor TJwTagThread.Create(CreateSuspended: boolean);
begin
  inherited;
end;

constructor TJwTagThread.Create;
begin
  pTag := nil;
  Create(True);
end;

procedure TJwTagThread.Execute;
begin
  ReturnValue := 1;
  if Assigned(fOnExecute) then
    ReturnValue := fOnExecute(pTag);

end;


class procedure TJwSecureRegistryKey.TreeResetNamedSecurityInfo(
  const RootKey: TJwRootRegKey; pObjectName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  const Owner, Group: TJwSecurityId; const DACL: TJwDAccessControlList;
  const SACL: TJwSAccessControlList; const bKeepExplicit: boolean;
  const fnProgress: TJwProgInvokeSetting;
  const FNProgressMethod: TJwFnProgressMethod;
  const FNProgressProcedure: TJwFnProgressProcedure;
  const ProgressUserData: Pointer);
begin
  inherited TreeResetNamedSecurityInfo(
    pObjectName,//pObjectName : TJwString;
    SE_REGISTRY_KEY,
    aSecurityInfo,
    //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
    Owner,//const Owner : TJwSecurityId;
    Group,//const Group : TJwSecurityId;
    DACL, //const DACL : TJwDAccessControlList;
    SACL, //const SACL : TJwSAccessControlList;
    bKeepExplicit,//const bKeepExplicit : Boolean;
    fnProgress,//const fnProgress : TJwProgInvokeSetting;
    FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
    FNProgressProcedure,//const FNProgressProcedure : TJwFnProgressProcedure;
    ProgressUserData//const ProgressUserData : Pointer);
    );
end;

function TJwSecureRegistryKey.GetInheritanceSource(const pObjectName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  const bIsDirectory: boolean; const GUIDs: TJwGuidArray;
  const FN_OBJECT_MGR_FUNCTS: Pointer): TJwInheritedFromArray;
var
  ACL: TJwSecurityAccessControlList;
  pObjectNameTemp: TJwString;
begin
  if siSaclSecurityInformation in aSecurityInfo then
    ACL := GetTempSACL
  else
    ACL := GetTempDACL;

  pObjectNameTemp := pObjectName;
  if Length(pObjectNameTemp) = 0 then
    pObjectNameTemp := fKeyName;
  if Length(pObjectNameTemp) = 0 then
    pObjectNameTemp := IntToStr(Handle);


  Result := inherited GetInheritanceSource(pObjectNameTemp,
    SE_REGISTRY_KEY, aSecurityInfo, bIsDirectory, GUIDs, ACL,
    TJwSecurityFileMapping, FN_OBJECT_MGR_FUNCTS);
end;



class procedure TJwSecureRegistryKey.RemoveInheritanceFlow(
  const Handle: THandle; const bCopyInheritedACEs: boolean = True);
var //[Hint] DACL : TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
    SE_FILE_OBJECT,
    //const aObjectType : TSeObjectType;
    [
    siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
    );
  try
    if Assigned(SD.DACL) then
    begin
      if bCopyInheritedACEs then
        SD.DACL.ConvertInheritedToExplicit
      else
        SD.DACL.RemoveInherited;


      TJwSecureGeneralObject.SetSecurityInfo(Handle,
        SE_FILE_OBJECT,
        //const aObjectType : TSeObjectType;
        [siDaclSecurityInformation,
        siProtectedDaclSecurityInformation],
        //aSecurityInfo : TJwSecurityInformationFlagSet;
        nil,//const anOwner : TJwSecurityId;
        nil,//const anGroup : TJwSecurityId;
        SD.DACL,
        //nil,//const aDACL : TJwDAccessControlList;
        nil//const aSACL : TJwSAccessControlList);
        );
    end;
  finally
    SD.Free;
  end;
end;

class procedure TJwSecureRegistryKey.RemoveInheritanceFlow(
  const KeyName: TJwString; const bCopyInheritedACEs: boolean = True);
var //[Hint] DACL : TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  SD := TJwSecureGeneralObject.GetNamedSecurityInfo(KeyName,
    SE_REGISTRY_KEY,
    //const aObjectType : TSeObjectType;
    [
    siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
    );
  try
    if Assigned(SD.DACL) then
    begin
      if bCopyInheritedACEs then
        SD.DACL.ConvertInheritedToExplicit
      else
        SD.DACL.RemoveInherited;


      TJwSecureGeneralObject.SetNamedSecurityInfo(KeyName,
        SE_REGISTRY_KEY,
        //const aObjectType : TSeObjectType;
        [siDaclSecurityInformation,
        siProtectedDaclSecurityInformation],
        //aSecurityInfo : TJwSecurityInformationFlagSet;
        nil,//const anOwner : TJwSecurityId;
        nil,//const anGroup : TJwSecurityId;
        SD.DACL,
        //nil,//const aDACL : TJwDAccessControlList;
        nil//const aSACL : TJwSAccessControlList);
        );
    end;
  finally
    SD.Free;
  end;
end;

procedure TJwSecureRegistryKey.RemoveInheritanceFlow(
  const bCopyInheritedACEs: boolean = True);
begin
  if (fKeyName <> '') then
  begin
    RemoveInheritanceFlow(fKeyName, bCopyInheritedACEs);
  end
  else
  if HasValidHandle then
  begin
    RemoveInheritanceFlow(fHandle, bCopyInheritedACEs);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'RemoveInheritanceFlow',
      ClassName, RsUNSecureObjects, 0, False, []);
end;

class procedure TJwSecureRegistryKey.RestoreInheritanceFlow(
  const Handle: THandle; bTakeOwnerShip: boolean = False);
var
  DACL: TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  try
    SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
      SE_FILE_OBJECT,
      //const aObjectType : TSeObjectType;
      [
      siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
      );

  except
    on E: EJwsclWinCallFailedException do
    begin
      if not bTakeOwnerShip then
        raise;

      //this only happens if we are not the owner and have not WRITE_DAC
      if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      begin
        raise EJwsclAdaptSecurityInfoException.CreateFmtEx(
          RsSecureObjectsDaclAdaptionFailed
          , 'RestoreInheritanceFlow', ClassName, RsUNSecureObjects, 0,
          True, []);
      end;

      TakeOwnerShip(Handle);

      //again
      SD := TJwSecureGeneralObject.GetSecurityInfo(Handle,
        SE_FILE_OBJECT,
        //const aObjectType : TSeObjectType;
        [
        siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
        );
    end;
  end;


  try
    //[Hint] DACL := nil;
    if not Assigned(SD.DACL) then
    begin
      DACL := TJwDAccessControlList.Create;
      SD.DACL := DACL;
      if not SD.OwnDACL then
        DACL.Free;
    end;

    TJwSecureGeneralObject.SetSecurityInfo(Handle,
      SE_FILE_OBJECT,
      //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation,
      siUnprotectedDaclSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      nil,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      SD.DACL,
      //nil,//const aDACL : TJwDAccessControlList;
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    SD.Free;
  end;
end;



class procedure TJwSecureRegistryKey.RestoreInheritanceFlow(
  const KeyName: TJwString; bTakeOwnerShip: boolean = False);
var
  DACL: TJwDAccessControlList;
  SD: TJwSecurityDescriptor;
begin
  SetLastError(0);


  try
    SD := TJwSecureGeneralObject.GetNamedSecurityInfo(KeyName,
      SE_REGISTRY_KEY,
      //const aObjectType : TSeObjectType;
      [
      siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
      );
  except
    on E: EJwsclWinCallFailedException do
    begin
      if not bTakeOwnerShip then
        raise;

      //this only happens if we are not the owner and have not WRITE_DAC
      if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME) then
      begin
        raise EJwsclAdaptSecurityInfoException.CreateFmtEx(
          RsSecureObjectsDaclAdaptionFailed,
          'RestoreInheritanceFlow', ClassName, RsUNSecureObjects, 0, True,
          []);
      end;

      TakeOwnerShip(KeyName);

      //again
      SD := TJwSecureGeneralObject.GetNamedSecurityInfo(KeyName,
        SE_REGISTRY_KEY,
        //const aObjectType : TSeObjectType;
        [
        siDaclSecurityInformation]//aSecurityInfo : TJwSecurityInformationFlagSet;
        );
    end;
  end;

  try
    //[Hint] DACL := nil;
    if not Assigned(SD.DACL) then
    begin
      DACL := TJwDAccessControlList.Create;
      SD.DACL := DACL;
      if not SD.OwnDACL then
        DACL.Free;
    end;

    TJwSecureGeneralObject.SetNamedSecurityInfo(KeyName,
      SE_REGISTRY_KEY,
      //const aObjectType : TSeObjectType;
      [siDaclSecurityInformation,
      siUnprotectedDaclSecurityInformation],
      //aSecurityInfo : TJwSecurityInformationFlagSet;
      nil,//const anOwner : TJwSecurityId;
      nil,//const anGroup : TJwSecurityId;
      SD.DACL,
      //nil,//const aDACL : TJwDAccessControlList;
      nil//const aSACL : TJwSAccessControlList);
      );
  finally
    SD.Free;
  end;
end;

procedure TJwSecureRegistryKey.RestoreInheritanceFlow(
  bTakeOwnerShip: boolean = False);
begin
  if (fKeyName <> '') then
  begin
    RestoreInheritanceFlow(fKeyName, bTakeOwnerShip);
  end
  else
  if HasValidHandle then
  begin
    RestoreInheritanceFlow(fHandle, bTakeOwnerShip);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'RestoreInheritanceFlow',
      ClassName, RsUNSecureObjects, 0, False, []);
end;


procedure TJwSecureRegistryKey.TakeOwnerShip(SID: TJwSecurityId = nil);
begin
  if (fKeyName <> '') then
  begin
    TakeOwnerShip(fKeyName, SID);
  end
  else
  if HasValidHandle then
  begin
    TakeOwnerShip(fHandle, SID);
  end
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
      RsSecureObjectsInvalidFileNameHandle, 'TakeOwnerShip', ClassName,
      RsUNSecureObjects, 0, False, []);
end;

class procedure TJwSecureRegistryKey.TakeOwnerShip(const KeyName: TJwString;
  SID: TJwSecurityId = nil);
begin
  TJwSecureBaseClass.TakeOwnerShip(KeyName, SE_REGISTRY_KEY, SID);
end;

class procedure TJwSecureRegistryKey.TakeOwnerShip(const Handle: THandle;
  SID: TJwSecurityId = nil);
begin
  TJwSecureBaseClass.TakeOwnerShip(Handle, SE_REGISTRY_KEY, SID);
end;


function ParseRootKeyPath(Str: TJwString): TJwRootTuple;
var
  p: integer;
  i: TJwRootKeyEnum;
  s: TJwString;
begin
  p := pos('\', Str);
  if (p > 0) then
    System.Delete(Str, p, Length(str));

  //HKEY_
  s := Copy(Str, 1, 5);
  if CompareText(s, 'HKEY_') = 0 then
    System.Delete(Str, 1, 5);

  for i := low(JwKeyRootTupleArray) to high(JwKeyRootTupleArray) do
  begin
    if (CompareText(JwKeyRootTupleArray[i].RootName, Str) = 0) then
    begin
      Result := JwKeyRootTupleArray[i];
      exit;
    end;
  end;

  Result := NIL_RootTuple;
end;

function RootKeyToTuple(aRK: TJwRootRegKey): TJwRootTuple;
var //[Hint] p : Integer;
  i: TJwRootKeyEnum;
  //[Hint] s : TJwString;
begin
  for i := low(JwKeyRootTupleArray) to high(JwKeyRootTupleArray) do
  begin
    if (JwKeyRootTupleArray[i].RootKey = aRK) then
    begin
      Result := JwKeyRootTupleArray[i];
      exit;
    end;
  end;

  Result := NIL_RootTuple;
end;

function TJwSecureRegistryKey.GetKeyInheritanceSource(
  const bUseWOW64: boolean = False;
  const aSecurityInfo: TJwSecurityInformationFlagSet =
  [siDaclSecurityInformation]): TJwInheritedFromArray;
var
  bUsesName: boolean;
begin
  GetKey(bUsesName);

  if not bUsesName then
    raise EJwsclInvalidPathException.CreateFmtEx(
      RsSecureObjectsMissingFileOrFolder,
      'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, []);


  Result := GetKeyInheritanceSource(rrkString,
    fKeyName, bUseWOW64, aSecurityInfo);
end;



class function TJwSecureRegistryKey.ParseUNC(const KeyName: TJwString;
  out Server, SubKey: TJwString; out aRootTuple: TJwRootTuple): boolean;
var
  sKeyName: TJwString;
  p: integer;
  cSize: Cardinal;
  pPCName: array[0..512] of TJwChar;
begin
  Result := (Length(KeyName) > 1) and (KeyName[1] = '\') and
    (KeyName[1] = '\');

  Server := '';

  if Result then
  begin
    sKeyName := KeyName;
    System.Delete(sKeyName, 1, 2);//delete "\\"


    p := pos('\', sKeyName); //find next "\"
    if (p = 0) then  //or get full path
      p := Length(sKeyName);

    Server := Copy(sKeyName, 1, p - 1);
    System.Delete(sKeyName, 1, p);
    aRootTuple := ParseRootKeyPath(sKeyName);

    p := pos('\', sKeyName);
    if (p = 0) then
      p := Length(sKeyName);
    System.Delete(sKeyName, 1, p);
    SubKey := sKeyName;

    if (CompareText(Server, 'local') = 0) or
      (CompareText(Server, 'localhost') = 0) then
    begin
      cSize := sizeof(pPCName) - 1;
      FillChar(pPCName, cSize + 1, 0);
{$IFDEF UNICODE}
      if GetComputerNameW(pPCName,cSize)  then
{$ELSE}
      if GetComputerNameA(pPCName, cSize) then
{$ENDIF}
        Server := pPCName;
    end;
  end
  else
  begin
    aRootTuple := ParseRootKeyPath(KeyName);

    sKeyName := KeyName;
    p := pos('\', sKeyName);
    if (p = 0) then
      p := Length(sKeyName);

    System.Delete(sKeyName, 1, p);
    SubKey := sKeyName;

  end;

  if (Length(SubKey) > 0) and (SubKey[Length(SubKey)] <> '\') then
    SubKey := SubKey + '\';
end;

class procedure TJwSecureRegistryKey.CheckKeyNameValidity(
  const KeyName: TJwString);
var
  aRootTuple: TJwRootTuple;
  sMachine, sKeyName: TJwString;
begin
  ParseUNC(KeyName, sMachine, sKeyName, aRootTuple);

  if (KeyName = '') then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsInvalidEmptyKeyPath,
      'CheckKeyNameValidity', ClassName, RsUNSecureObjects, 0, False, [KeyName]);
  end
  else
  if (aRootTuple.RootName = '') then
  begin
    raise EJwsclInvalidKeyPath.CreateFmtEx(
      RsSecureObjectsInvalidKeyPathMissingRootElement,
      'CheckKeyNameValidity', ClassName, RsUNSecureObjects, 0, False, [KeyName]);
  end;
end;


class procedure TJwSecureRegistryKey.SetSecurityDescriptorEx(
  const KeyName: TJwString; const aSecurityInfo: TJwSecurityInformationFlagSet;
  const aSecurityDescriptor: TJwSecurityDescriptor; bUseWOW64: boolean);
var
  hRemoteKey, hSubKey: HKEY;
  //[Hint] bRes : Boolean;
  //[Hint] anOwner, aGroup : TJwSecurityId;
  //[Hint] aDACL : TJwDAccessControlList;
  //[Hint] aSACL : TJwSAccessControlList;

  sMachine, sKeyName: TJwString;
  res:  integer;
  //[Hint] p : Integer;
  aRootTuple: TJwRootTuple;
  pSD:  PSecurityDescriptor;
  SecD: Cardinal;
  //[Hint] m : Pointer;

  bIsUNC, bConnectErrorType: boolean;

begin

  if not Assigned(aSecurityDescriptor) then
    raise EJwsclInvalidSecurityDescriptor.CreateFmtEx(
      RsSecureObjectsNilSdParameter, 'SetSecurityDescriptorEx',
      ClassName, RsUNSecureObjects, 0, False, [KeyName]);

  bIsUNC := ParseUNC(KeyName, sMachine, sKeyName, aRootTuple);

  if (aRootTuple.RootName = '') then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsInvalidEmptyKeyPath, 'SetSecurityDescriptorEx',
      ClassName, RsUNSecureObjects, 0, False, [KeyName]);
  end;

  if bIsUNC then
  begin

    hRemoteKey := 0;
    hSubKey := 0;


    bConnectErrorType := True;
{$IFDEF UNICODE}
    res := RegConnectRegistryW(TJwPChar(sMachine), aRootTuple.Key, hRemoteKey);
{$ELSE}
    res := RegConnectRegistryA(TJwPChar(sMachine), aRootTuple.Key, hRemoteKey);
{$ENDIF}


    if (res = 0) and (sKeyName <> '') then
    begin
      bConnectErrorType := False;
{$IFDEF UNICODE}
      res := RegOpenKeyExW(hRemoteKey, TJwPChar(sKeyName), 0, KEY_READ, hSubKey);
{$ELSE}
      res := RegOpenKeyExA(hRemoteKey, TJwPChar(sKeyName), 0,
        KEY_READ, hSubKey);
{$ENDIF}
    end;

    if Res = 0 then
    begin
      if (hSubKey = 0) then
      begin
        hSubKey := hRemoteKey;
        hRemoteKey := 0;
      end;

      pSD := nil;
      try
        pSD  := nil;
        SecD := 0;
        //get needed buffer size

        if Assigned(aSecurityDescriptor) then
          pSD := aSecurityDescriptor.Create_SD(SecD, True);

        if RegSetKeySecurity(hSubKey,
          TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), pSD) <> 0 then
          raise EJwsclWinCallFailedException.CreateFmtEx(
            RsSecureObjectsCallFailedRegSetKeySecurity,
            'SetSecurityDescriptorEx', ClassName, RsUNSecureObjects, 0, True, []);

      finally
        RegCloseKey(hSubKey);
        if (hRemoteKey <> 0) then
          RegCloseKey(hRemoteKey);
        FreeMem(pSD);
      end;
    end
    else
    begin
      if bConnectErrorType then
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed, 'SetSecurityDescriptorEx',
          ClassName, RsUNSecureObjects, 0, True, ['RegConnectRegistry'])
      else
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsSecureObjectsRegOpenKeyEx,
          'SetSecurityDescriptorEx', ClassName, RsUNSecureObjects, 0, True, []);
    end;

  end
  else
  if bUseWOW64 then

    SetNamedSecurityInfo(KeyName, SE_REGISTRY_WOW64_32KEY, aSecurityInfo,
      aSecurityDescriptor.Owner,
      aSecurityDescriptor.PrimaryGroup, aSecurityDescriptor.DACL,
      aSecurityDescriptor.SACL)
  else
    SetNamedSecurityInfo(KeyName, SE_REGISTRY_KEY, aSecurityInfo,
      aSecurityDescriptor.Owner,
      aSecurityDescriptor.PrimaryGroup, aSecurityDescriptor.DACL,
      aSecurityDescriptor.SACL);
end;


class function TJwSecureRegistryKey.GetSecurityDescriptorEx(
  const KeyName: TJwString; const aSecurityInfo: TJwSecurityInformationFlagSet;
  bUseWOW64: boolean): TJwSecurityDescriptor;
var
  hRemoteKey, hSubKey: HKEY;
  //[Hint] bRes : Boolean;
  //[Hint] anOwner, aGroup : TJwSecurityId;
  //[Hint] aDACL : TJwDAccessControlList;
  //[Hint] aSACL : TJwSAccessControlList;

  sMachine, sKeyName: TJwString;
  res:  integer;
  //[Hint] p : Integer;
  aRootTuple: TJwRootTuple;
  pSD:  PSecurityDescriptor;
  SecD: Cardinal;
  //[Hint] m : Pointer;

  bIsUNC, bConnectErrorType: boolean;
begin
  Result := nil;

  bIsUNC := ParseUNC(KeyName, sMachine, sKeyName, aRootTuple);

  if (aRootTuple.RootName = '') then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsInvalidEmptyKeyPath, 'GetKeyInheritanceSource',
      ClassName, RsUNSecureObjects, 0, False, [KeyName]);

  end;

  if bIsUNC then
  begin

    hRemoteKey := 0;
    hSubKey := 0;


    bConnectErrorType := True;
{$IFDEF UNICODE}
    res := RegConnectRegistryW(TJwPChar(sMachine), aRootTuple.Key, hRemoteKey);
{$ELSE}
    res := RegConnectRegistryA(TJwPChar(sMachine), aRootTuple.Key, hRemoteKey);
{$ENDIF}


    if (res = 0) and (sKeyName <> '') then
    begin
      bConnectErrorType := False;
{$IFDEF UNICODE}
      res := RegOpenKeyExW(hRemoteKey, TJwPChar(sKeyName), 0, KEY_READ, hSubKey);
{$ELSE}
      res := RegOpenKeyExA(hRemoteKey, TJwPChar(sKeyName), 0,
        KEY_READ, hSubKey);
{$ENDIF}
    end;

    if Res = 0 then
    begin
      if (hSubKey = 0) then
      begin
        hSubKey := hRemoteKey;
        hRemoteKey := 0;
      end;

      pSD := nil;
      try
        pSD  := nil;
        SecD := 0;
        //get needed buffer size
        if (RegGetKeySecurity(hSubKey,
          TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), pSD, SecD) <>
          HRESULT(ERROR_INSUFFICIENT_BUFFER)) or (SecD = 0) then
          raise EJwsclWinCallFailedException.CreateFmtWinCall(
            RsSecureObjectsCallFailedRegGetKeySecurity,
            'GetKeyInheritanceSource', ClassName,
            RsUNSecureObjects, 0, True,
            'RegGetKeySecurity', []);

        GetMem(pSD, SecD);


        FillChar(pSD^, SecD, 0);
        pSD.Revision := ACL_REVISION1;
        SecD := SecD;

        if RegGetKeySecurity(hSubKey,
          TJwEnumMap.ConvertSecurityInformation(aSecurityInfo), pSD, SecD) <> 0 then
          raise EJwsclWinCallFailedException.CreateFmtWinCall(
            RsSecureObjectsCallFailedRegGetKeySecurity2,
            'GetKeyInheritanceSource', ClassName, RsUNSecureObjects,
            0, True, 'RegGetKeySecurity', []);

        Result := TJwSecurityDescriptor.Create(pSD);
      finally
        RegCloseKey(hSubKey);
        if (hRemoteKey <> 0) then
          RegCloseKey(hRemoteKey);
        FreeMem(pSD);
      end;
    end
    else
    begin
      if bConnectErrorType then
        raise EJwsclWinCallFailedException.CreateFmtWinCall(
          RsWinCallFailed,
          'GetKeyInheritanceSource',
          ClassName, RsUNSecureObjects, 0, True, 'RegConnectRegistry',
          ['RegConnectRegistry'])
      else
        raise EJwsclWinCallFailedException.CreateFmtWinCall(
          RsSecureObjectsCallFailedRegOpenKeyEx,
          'GetKeyInheritanceSource', ClassName, RsUNSecureObjects,
          0, True, 'RegOpenKeyEx', []);
    end;

  end
  else
  if bUseWOW64 then
    Result := GetNamedSecurityInfo(KeyName, SE_REGISTRY_WOW64_32KEY,
      aSecurityInfo)
  else
    Result := GetNamedSecurityInfo(KeyName, SE_REGISTRY_KEY, aSecurityInfo);
end;

class function TJwSecureRegistryKey.GetKeyInheritanceSource(
  const RootKey: TJwRootRegKey; const KeyName: TJwString;
  const bUseWOW64: boolean = False;
  const aSecurityInfo: TJwSecurityInformationFlagSet =
  [siDaclSecurityInformation]): TJwInheritedFromArray;


  function GetParent(PathName: TJwString; bStop: boolean = False): TJwString;
  var
    i, i2: integer;
    s: TJwString;
  begin
    Result := '';

    i := Length(PathName);
    while (i > 0) do
    begin
      if (i < Length(PathName)) and ((PathName[i] = '\') or
        (PathName[i] = '/') or (PathName[i] = ':')) then
        break;

      Result := PathName[i] + Result;

      Dec(i);
    end;
    for i2 := 1 to i do
      S := S + PathName[i2];

    if bStop then
      Result := GetParent(s)
    else
      Result := s;
  end;




  procedure UpdateObjectInheritedDACL(PathName: TJwString;
    const PreviousInhACL: TJwDAccessControlList;
    const bUseConfigRoot: boolean;
    var pInhArray: TJwInheritedFromArray; Level: integer);

  var
    i, ps: integer;
    SID: TJwString;
    //    Flags1, Flags2 : TJwAceFlags;
    bError: boolean;
    SD:  TJwSecurityDescriptor;
  begin
    bError := False;
    SD := nil;
    try
      SD := GetSecurityDescriptorEx(PathName, aSecurityInfo, bUseWOW64);
    except
      on E: Exception do
        bError := True; //file or folder not found: set array invalid
    end;

    if not bError then
      bError := not Assigned(SD) or (Assigned(SD) and not Assigned(SD.DACL));

    if bError then
    begin
      for i := 0 to PreviousInhACL.Count - 1 do
      begin
        //set to invalid
        if pInhArray[i].GenerationGap = 0 then
        begin
          pInhArray[i].GenerationGap := -1;
        end;
      end;

      exit;
    end;



    // try
    for i := 0 to PreviousInhACL.Count - 1 do
    begin
      //get first equal ACE
      ps := SD.DACL.FindEqualACE(PreviousInhACL[i], [eactSameSid,
        eactSameAccessMask, eactSEAccessMask, eactSameType]);

      if (ps >= 0) then
      begin

        if //(Flags1 = Flags2) and
        (afInheritedAce in PreviousInhACL[i].Flags) and
          //root ACE is inherited
          not (afInheritedAce in SD.DACL[ps].Flags) and
          //current ACE is explicit
          not (PreviousInhACL[i].Ignore) then
          //root ACE was not already considered
        begin

          SID := '';

          if Assigned(SD) then
          try
            SID := SD.DACL[ps].SID.AccountName[''] + '@' +
              SD.DACL[ps].SID.CachedSidString;
          except
            try
              SID := SD.DACL[ps].SID.AccountName[''];
            except
              SID := SD.DACL[ps].SID.CachedSidString;
            end;
          end;

          pInhArray[i].GenerationGap := Level;
          pInhArray[i].AncestorName := PathName;
          pInhArray[i].SID := SID;
          PreviousInhACL[i].Ignore := True; //Ignore this ACE next time
        end;
      end;
    end;

    for i := 0 to PreviousInhACL.Count - 1 do
    begin
      if (afInheritedAce in PreviousInhACL[i].Flags) and
        not PreviousInhACL[i].Ignore then
      begin
        PathName := GetParent(PathName);
        UpdateObjectInheritedDACL(PathName, PreviousInhACL,
          bUseConfigRoot, pInhArray, Level + 1);
        break;
      end;
    end;
    //finally
    FreeAndNil(SD);
    // end;
  end;

  //[Hint] var aHKey : HKEY;
  //[Hint] ret : Cardinal;

var //[Hint] sMachine,
  sKeyName: TJwString;
  //[Hint] bIsFolder : Boolean;
  //[Hint] res, p,
  //[Hint] Level : Integer;
  SD: TJwSecurityDescriptor;
  aRootTuple: TJwRootTuple;

  bPrivEn, bUseConfigRoot: boolean;
begin
  bUseConfigRoot := True;
  //[Hint] Level := 0;

  if (aSecurityInfo <> [siDaclSecurityInformation]) and
    (aSecurityInfo <> [siSaclSecurityInformation]) then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      RsSecureObjectsMissingDaclOrSaclInSiParameter,
      'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, []);




  if (RootKey = rrkString) then
  begin
    sKeyName := KeyName;
  end
  else
  begin
    aRootTuple := RootKeyToTuple(RootKey);
    sKeyName := aRootTuple.RootName;

    if sKeyName = '' then
      raise EJwsclInvalidKeyPath.CreateFmtEx(
        RsSecureObjectsEmptyRootKey,
        'GetFileInheritanceSource', ClassName, RsUNSecureObjects,
        0, False, [KeyName]);

    bUseConfigRoot := aRootTuple.RootKey = rrkCurrentConfig;

    //if (not bUseConfigRoot) then
    begin
      if (Length(KeyName) > 0) and (KeyName[1] <> '\') then
        sKeyName := sKeyName + '\';
      sKeyName := sKeyName + KeyName;
    end;
  end;

  bPrivEn := False;

  if (aSecurityInfo = [siSaclSecurityInformation]) then
  begin
    if not JwIsPrivilegeSet(SE_SECURITY_NAME) then
      raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
        RsSecureObjectsPrivilegeSecurityMissing,
        'GetFileInheritanceSource', ClassName, RsUNSecureObjects, 0, False, [KeyName]);

    bPrivEn := JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);
  end;

  try
    SD := GetSecurityDescriptorEx(sKeyName, aSecurityInfo, bUseWOW64);

    //do not use invalid SD or nil DACL
    if Assigned(SD) and Assigned(SD.DACL) then
    begin
      SetLength(Result, SD.DACL.Count);
      try
        sKeyName := GetParent(sKeyName);

        UpdateObjectInheritedDACL(sKeyName, SD.DACL,
          bUseConfigRoot, Result, 1);
      finally
        SD.Free;
      end;
    end
    else
    begin
      SetLength(Result, 0);
    end;
  finally
    //restore privilege to old value
    if (aSecurityInfo = [siSaclSecurityInformation]) and not bPrivEn then
      JwEnablePrivilege(SE_SECURITY_NAME, pst_Disable);
  end;
end;


var
  _RegQueryReflectionKey: procedure(hBase: HKEY;
        var bIsReflectionDisabled: boolean); stdcall;

function RegQueryReflectionKey(hBase: HKEY): shortint;
begin
  Result := -1;
  try
    {GetProcedureAddress(Pointer(@_RegQueryReflectionKey),
      advapi32, 'RegQueryReflectionKey');}
    _RegQueryReflectionKey := TJwLibraryUtilities.LoadLibProc(advapi32, 'RegQueryReflectionKey');
    SetLastError(0);
    _RegQueryReflectionKey(hBase, boolean(Result));
  except
    on E: EJwaLoadLibraryError do
    begin
    end;
    on E: EJwaGetProcAddressError do
    begin
    end;
  end;
end;

var
  _RegEnDisableReflectionKey: procedure(hBase: HKEY); stdcall;

procedure RegEnableReflectionKey(hBase: HKEY; Enabled: boolean);
begin
  if Enabled then
  begin
    try
      {GetProcedureAddress(Pointer(@_RegEnDisableReflectionKey),
        advapi32, 'RegEnableReflectionKey');}
      _RegEnDisableReflectionKey := TJwLibraryUtilities.LoadLibProc(advapi32, 'RegEnableReflectionKey');
      SetLastError(0);
      _RegEnDisableReflectionKey(hBase);
    except
      on E: EJwaLoadLibraryError do
      begin
      end;
      on E: EJwaGetProcAddressError do
      begin
      end;
    end;
  end
  else
    try
      {GetProcedureAddress(Pointer(@_RegEnDisableReflectionKey),
        advapi32, 'RegDisableReflectionKey');}
      _RegEnDisableReflectionKey := TJwLibraryUtilities.LoadLibProc(advapi32, 'RegDisableReflectionKey');
      SetLastError(0);
      _RegEnDisableReflectionKey(hBase);
    except
      on E: EJwaLoadLibraryError do
      begin
      end;
      on E: EJwaGetProcAddressError do
      begin
      end;
    end;
end;


{TJwSecureRegistryKey}
procedure IterateKey(const Path: TJwString;
    aSecurityInfo:
    TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
    const Action:
    TJwProgInvokeSetting;
    const SetType: TJwTreeSetType;
    const bKeepExplicit: boolean;
    const Owner: TJwSecurityId;
    const Group: TJwSecurityId;
    const DACL:
    TJwDAccessControlList;
    const SACL:
    TJwSAccessControlList;
    const FNProgressMethod:
    TJwFnProgressMethod;
    const FNProgressProcedure: TJwFnProgressProcedure;
    const ProgressUserData
    : Pointer;
    bIsRoot: boolean;
    const Disable64Redirection:
    boolean; out bAbort: boolean);




  procedure FNProgress(const pObjectName: TJwString;
      // Name of object just processed
    const cStatus: Cardinal;
      // Status of operation on object
    var pInvokeSetting: TJwProgInvokeSetting; // When to set
    const E: EJwsclSecurityException;
    const Args: Pointer;
      // Caller specific data
    const bSecuritySet:
      boolean                 // Whether security was set
    );
  begin
    if Assigned(FNProgressMethod) then
      try
        FNProgressMethod(pObjectName, cStatus, pInvokeSetting,
          E, Args, bSecuritySet);
      except
      end;

    if Assigned(FNProgressProcedure) then
      try
        FNProgressProcedure(pObjectName, cStatus, pInvokeSetting,
          E, Args, bSecuritySet);
      except
      end;
  end;


  function EnableReg64Redirection(const KeyPath: TJwString;
    Enabled: boolean): boolean;
  var
    Server, SubKey: TJwString;
    aRootTuple: TJwRootTuple;
    hRemoteKey, aKey: HKEY;
    res: integer;
    bQuery: Short;
  begin
    SetLastError(0);
    Result := False;
    TJwSecureRegistryKey.ParseUNC(KeyPath, Server, SubKey, aRootTuple);

    if (aRootTuple.RootName = '') then
      exit;

    aKey := 0;
    hRemoteKey := 0;

    if (Server <> '') then
    begin
{$IFDEF UNICODE}
      res := RegConnectRegistryW(TJwPChar(Server), aRootTuple.Key, hRemoteKey);
{$ELSE}
      res := RegConnectRegistryA(TJwPChar(Server), aRootTuple.Key,
        hRemoteKey);
{$ENDIF}
      if res <> 0 then
        exit;
    end
    else
      hRemoteKey := aRootTuple.Key;

{$IFDEF UNICODE}
    res := RegOpenKeyExW(hRemoteKey, TJwPChar(SubKey), 0, KEY_READ, aKey);
{$ELSE}
    res := RegOpenKeyExA(hRemoteKey, TJwPChar(SubKey), 0, KEY_READ, aKey);
{$ENDIF}

    if (res = 0) then
    begin
      bQuery := RegQueryReflectionKey(hRemoteKey);
      if (bQuery <> -1) then
      begin
        Result := boolean(bQuery);
        if (Result <> Enabled) then
          RegEnableReflectionKey(hRemoteKey, Enabled);
      end;
    end;

    if (aKey > 0) then
      RegCloseKey(aKey);
    if (hRemoteKey > 0) then
      RegCloseKey(hRemoteKey);
  end;


  function CombinePath(Path1, Path2: TJwString): TJwString;
  begin
    if (Length(Path1) > 0) and (Path1[Length(Path1)] <> '\') then
      Path1 := Path1 + '\';
    Result := Path1 + Path2;
  end;

type
  PFindData = ^TFindData;

  TFindData = record
    SubKey: TJwString;
  end;

type
  TKeyNames = array of TJwString;

var
  stack: TQueue;
  CurrentPath: TJwString;

  ptrFindData: PFindData;

  bFirst,
  bError: boolean;
  pInvokeSetting: TJwProgInvokeSetting;
  Count:  integer;

    {<B>SetSecurity</B> sets the security of a file or folder
     @param CurrentFileName absolute path to file or folder
     @param bRoot defines, if the folder is the first one specified the the user
    }
  procedure SetSecurity(CurrentKeyName: TJwString; bRoot: boolean);
  var
    oldDACL: TJwDAccessControlList;
    oldSACL: TJwSAccessControlList;

    SD: TJwSecurityDescriptor;

  begin
    oldDACL := nil;
    oldSACL := nil;

    {the Set type simply adds the given DACL/SACL to the file security.
     The Reset type does the same, but the inheritance protection is removed after the root object was changed
    }
    if bKeepExplicit or (SetType = tstSet) then
    begin
      SD := TJwSecureRegistryKey.GetSecurityDescriptorEx(
        CurrentKeyName, aSecurityInfo, False);

      try
        oldDACL := SD.DACL;
        oldSACL := SD.SACL;

        //set old DACL
        if Assigned(oldDACL) then
        begin
          if bRoot and Assigned(DACL) then
            oldDACL.AddAces(DACL);
          oldDACL.RemoveInherited;
          //remove inherited ACEs, because these are set automatically by the system
        end;

        if Assigned(oldSACL) then
        begin
          if bRoot and Assigned(SACL) then
            oldSACL.AddAces(SACL);

          oldSACL.RemoveInherited;
          //remove inherited ACEs, because these are set automatically by the system
        end;


        TJwSecureRegistryKey.SetSecurityDescriptorEx(
          CurrentKeyName, aSecurityInfo, SD, False);
      finally
        FreeAndNil(SD);
      end;
    end
    else
    begin
      if Assigned(DACL) then
      begin
        oldDACL := TJwDAccessControlList.Create(True);

        if bRoot and Assigned(DACL) then
          oldDACL.Assign(DACL);

        oldDACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;
      if Assigned(SACL) then
      begin
        oldSACL := TJwSAccessControlList.Create(True);

        if bRoot and Assigned(SACL) then
          oldSACL.Assign(SACL);

        oldSACL.RemoveInherited;
        //remove inherited ACEs, because these are set automatically by the system
      end;
      try
        SD := TJwSecurityDescriptor.Create;
        SD.OwnDACL := False;
        SD.DACL := oldDACL;
        SD.OwnDACL := True;
        SD.SACL := oldSACL;

        TJwSecureRegistryKey.SetSecurityDescriptorEx(
          CurrentKeyName, aSecurityInfo, SD, False);

      finally
        FreeAndNil(oldSACL);
        FreeAndNil(SD);
      end;
    end;
  end;

    {<B>ProcessKey</B> processes the key security.
     Sets the security, catches a possible exception and calls the callback function.

     @param FindDataFile the file or folder name (but not path) returned by the FindFirst/FindNext function
     @param CurrentFileName Defines the current path to the file or folder given in FindDataFile. It must also include it at the end of path.
     @param bRoot defines whether this folder is the path given by the first function call. All other calls use FALSE.

     @return Returns true if the callback function defined pis_ProgressCancelOperation as the next operation. In this case bAbort is set to true.
    }
  function ProcessKey(CurrentKeyName: TJwString;
    bRoot: boolean = False): boolean;
  var
    bOldReg64Redirection: boolean;
  begin
    Result := False;


    repeat
      pInvokeSetting := Action;
      bError := False;

      try
              {problem - der hauptordner muss noch gesetzt werden
               also das ganze repeat zeugs am besten in eine prozedur auslagern

               DACL einer/s Datei/Ordner setzen

               wenn hauptordner
                0. DACL D/F aus aktuelle Ordner lesen
                1. Lösche alle indirekten ACE aus DACL D/F
                 hier problem, was passiert mit den indirekten? vllt noch boolean parameter
                 der angibt, ob behalten oder nicht?

                1. setze die indirekten (object_inheritance usw) ACEs aus Data.DACL
                2. wenn not Data.bKeepExplicit, alle eplizite ACEs aus DACL löschen
                3. setze explizite ACEs aus Data.DACL
                4. DACL Datei/Folder löschen
                5. DACL D/F setzen
               sonst
                1. DACL D/F aus aktuelle Datei/Ordner lesen
                2. wenn not Data.bKeepExplicit, alle eplizite ACEs aus DACL löschen
                3. lösche alle indirekten ACEs aus DACL
                4. setze explizite ACEs aus Data.DACL
                5. DACL D/F löschen
                6. DACL D/F setzen }
        //remove old DACL
               { if (siDaclSecurityInformation in aSecurityInfo) then
                begin
                  TJwSecureFileObject.SetNamedSecurityInfo(
                    CurrentFileName,SE_FILE_OBJECT,
                          [siDaclSecurityInformation], nil, nil, nil, nil);
                end;
                //remove old SACL
                if (siSaclSecurityInformation in aSecurityInfo) then
                begin
                  TJwSecureFileObject.SetNamedSecurityInfo(
                    CurrentFileName,SE_FILE_OBJECT,
                          [siSaclSecurityInformation], nil, nil, nil, nil);
                end;


                TJwSecureFileObject.SetNamedSecurityInfo(
                  CurrentFileName,SE_FILE_OBJECT,
                          aSecurityInfo, Owner, Group, DACL, SACL);
            }
        bOldReg64Redirection := False;
        try
          if Disable64Redirection then
            bOldReg64Redirection :=
              EnableReg64Redirection(CurrentKeyName, not Disable64Redirection);

          //set security
          SetSecurity(CurrentKeyName, bRoot);
        finally
          if Disable64Redirection then
            EnableReg64Redirection(CurrentKeyName,
              bOldReg64Redirection);
        end;
      except
        on E: EJwsclSecurityException do
        begin
          bError := True;

          if (Action = pis_ProgressInvokeOnError) then
            FNProgress(CurrentKeyName,
              //const pObjectName : TJwString;              // Name of object just processed
              GetLastError(),
              //const cStatus : Cardinal;                    // Status of operation on object
              pInvokeSetting,
              //var pInvokeSetting : TJwProgInvokeSetting; // When to set
              E,     //const E : EJwsclSecurityException;
              ProgressUserData,
              //const Args : Pointer;                      // Caller specific data
              False
              //const bSecuritySet : Boolean                 // Whether security was set
              );
        end;
      end;
      if not bError and (Action = pis_ProgressInvokeEveryObject) then
      begin
        FNProgress(CurrentKeyName,
          //const pObjectName : TJwString;              // Name of object just processed
          0,
          //const cStatus : Cardinal;                    // Status of operation on object
          pInvokeSetting,
          //var pInvokeSetting : TJwProgInvokeSetting; // When to set
          nil,     //const E : EJwsclSecurityException;
          ProgressUserData,
          //const Args : Pointer;                      // Caller specific data
          True
          //const bSecuritySet : Boolean                 // Whether security was set
          );
      end;
      //repeat if action is a retry operation
    until (pInvokeSetting <> pis_ProgressRetryOperation);

    if (pInvokeSetting = pis_ProgressCancelOperation) then
    begin
      bAbort := True;
      Result := True;
      exit;
    end;

  end;


  function EnumKeysByHandle(const SubKeys: TJwString;
  const aKey: HKEY): TKeyNames;
  var //[Hint] key : HKEY;
    pName: TJwPChar;
    len:  Cardinal;
    time: TFileTime;
    idx, ret: integer;
  const
    MAX_KEY_LENGTH = 255;
  begin
    SetLastError(0);
    GetMem(pName, MAX_KEY_LENGTH);

    Result := nil;

    try
      idx := 0;
      repeat
        len := MAX_KEY_LENGTH;
        ret :=
{$IFDEF UNICODE}RegEnumKeyExW{$ELSE}
          RegEnumKeyExA
{$ENDIF}
          (aKey, idx, pName, len, nil, nil, nil, @time);
        SetLastError(ret);

        if (ret = 0) then
        begin
          SetLength(Result, idx + 1);
          //SetLength(result[idx], Length(SubKeys+pName));
          Result[idx] := SubKeys + pName;
        end;
        Inc(idx);

      until ret <> 0;
    finally
      FreeMem(pName);
    end;

    if (ret <> HRESULT(ERROR_NO_MORE_ITEMS)) then
    begin
      //raise
    end
    else
      SetLastError(0);

  end;

  function EnumKeys(sKey: TJwString): TKeyNames;
  var
    Server, SubKey: TJwString;
    aRootTuple: TJwRootTuple;
    hRemoteKey, aKey: HKEY;
    res: integer;
  begin
    SetLastError(0);
    Result := nil;
    //split Key into its parts
    TJwSecureRegistryKey.ParseUNC(sKey, Server, SubKey, aRootTuple);

    if (aRootTuple.RootName = '') then
      exit;

    aKey := 0;
    hRemoteKey := 0;
    if (Server <> '') then
    begin
      {$IFDEF UNICODE}
      res := RegConnectRegistryW(TJwPChar(Server), aRootTuple.Key, hRemoteKey);
      {$ELSE}
      res := RegConnectRegistryA(TJwPChar(Server), aRootTuple.Key, hRemoteKey);
      {$ENDIF UNICODE}
      if res <> 0 then
        exit;
    end
    else
      hRemoteKey := aRootTuple.Key;

    {$IFDEF UNICODE}
    {res := }RegOpenKeyExW(hRemoteKey, TJwPChar(SubKey), 0, KEY_READ, aKey);
    {$ELSE}
    {res := }RegOpenKeyExA(hRemoteKey, TJwPChar(SubKey), 0, KEY_READ, aKey);
    {$ENDIF UNICODE}
      {
      TODO: Check for error results!
      So ignore warning!
      }

    if (Length(sKey) > 0) and (sKey[Length(sKey)] <> '\') then
      sKey := sKey + '\';

    if aKey <> 0 then
      Result := EnumKeysByHandle(sKey, aKey);

    if (aKey > 0) then
      RegCloseKey(aKey);

    if (Server <> '') and (hRemoteKey > 0) then
      RegCloseKey(hRemoteKey);
  end;

var
  subKeys: TKeyNames;
  i: integer;

begin
  subKeys := nil;

  CurrentPath := Path;
  if (Length(CurrentPath) > 0) and (CurrentPath[Length(CurrentPath)] <> '\') then
    CurrentPath := CurrentPath + '\';

  {Create a stack on heap for saving
   key recursion
  }
  stack := TQueue.Create;

  //first folder iteration
  bFirst := True;

  //user abortion
  bAbort := False;


  //set security
  bAbort := ProcessKey(CurrentPath, bIsRoot{isRootObject});


  if Assigned(DACL) then
    DACL.RemoveInherited;
  if Assigned(SACL) then
    SACL.RemoveInherited;

  {
  The sub keys are not protected. They get inherited ACEs from
  the parent key. However we have to reset all keys and folder DACL first.
  }
  Exclude(aSecurityInfo, siProtectedDaclSecurityInformation);
  Exclude(aSecurityInfo, siProtectedSaclSecurityInformation);


  //remove protection from all children if reset is defined
  //to let the inheritance flow flow
  if (SetType = tstReset) then
  begin
    Include(aSecurityInfo, siUnprotectedDaclSecurityInformation);
    Include(aSecurityInfo, siUnprotectedSaclSecurityInformation);
  end;


  while ((stack.Count > 0) or bFirst) and not bAbort do
  begin
    bFirst := False;

    Count := stack.Count;
    {
    Get next recursion folder to be processed
    }
    if Count > 0 then
    begin
      ptrFindData := PFindData(stack.Pop);
      CurrentPath  := ptrFindData.SubKey;
      FreeMem(ptrFindData);

      //set security
      bAbort := ProcessKey(CurrentPath, bFirst);
    end;

    if not bAbort then
    begin
      SetLastError(0);

      //get sub keys
      subKeys := EnumKeys(CurrentPath);

      //call user if error occured
      if GetLastError() <> 0 then
      begin
        //ignore, cancel and retry loop
        repeat
          //default
          pInvokeSetting := pis_ProgressInvokeOnError;

          if (Action = pis_ProgressInvokeOnError) or
            (Action = pis_ProgressInvokeEveryObject) then
          begin
            //call user function for
            //next step
            FNProgress(CurrentPath,
              //const pObjectName : TJwString;              // Name of object just processed
              GetLastError(),
              //const cStatus : Cardinal;                    // Status of operation on object
              pInvokeSetting,
              //var pInvokeSetting : TJwProgInvokeSetting; // When to set
              nil,     //const E : EJwsclSecurityException;
              ProgressUserData,
              //const Args : Pointer;                      // Caller specific data
              False
              //const bSecuritySet : Boolean                 // Whether security was set
              );
          end;

          SetLastError(0);
          //retry
          if (pInvokeSetting = pis_ProgressRetryOperation) then
            subKeys := EnumKeys(CurrentPath)
          else
          //abort
          if bAbort or (pInvokeSetting = pis_ProgressCancelOperation) then
          begin
            bAbort := True;
            break;
          end;
        until (GetLastError() = 0) or bAbort;
      end;

      //add sub keys to our stack
      if Assigned(subKeys) and not bAbort then
        for i := low(subKeys) to high(subKeys) do
        begin
          GetMem(ptrFindData, sizeof(TFindData));
          FillChar(ptrFindData^, sizeof(TFindData), 0);
          ptrFindData.SubKey := subKeys[i];
          stack.Push(ptrFindData);
        end;
    end;
  end;

  //free rest of stack members
  while (stack.Count > 0) do
  begin
    ptrFindData := PFindData(stack.Pop);

    if ptrFindData <> nil then
      FreeMem(ptrFindData);
  end;


  FreeAndNil(stack);
end;



class function TJwSecureRegistryKey.TreeRegKeySetNamedSecurityInfo_Execute(
  pData: Pointer): integer;
  //function TreeFileObjectSetNamedSecurityInfo_ThreadFunc(Data : PThreadData) : Cardinal; stdcall;
var
  p: TJwProgInvokeSetting;
  b, bAbort: boolean;
  Data: PThreadData absolute pData;
begin
  Result := 1;
  if Data = nil then
    exit;

  if Data^.Disable64Redirection then
    Wow64FsRedirection(True);

  try
    b := Data^.bKeepExplicit;

    IterateKey(Data^.pObjectName,
      Data^.aSecurityInfo,
      //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      Data^.Action,//const Action : TJwProgInvokeSetting;
      Data^.SetType,
      b,
      Data^.Owner,//const Owner : TJwSecurityId;
      Data^.Group,//const Group : TJwSecurityId;
      Data^.DACL, //const DACL : TJwDAccessControlList;
      Data^.SACL, //const SACL : TJwSAccessControlList;
      Data^.FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
      Data^.FNProgressProcedure,
      //const FNProgressProcedure : TJwFnProgressProcedure;
      Data^.ProgressUserData,//const ProgressUserData : Pointer;
      True, //first call of this function = true
      Data^.Disable64Redirection,
      bAbort
      );
  finally

    //call callback function, the thread has ended
    if Assigned(Data^.FNProgressMethod) then
      try
        p := pis_ProgressFinished;
        Data^.FNProgressMethod(Data^.pObjectName, 0, p, nil, Data^.ProgressUserData, False);
      except
        //don't let us get out of the concept!
        //so just ignore the users exception
        //After all, we're maybe in a thread!
      end;


    if Assigned(Data^.FNProgressProcedure) then
      try
        p := pis_ProgressFinished;
        Data^.FNProgressProcedure(Data^.pObjectName, 0, p, nil,
          Data^.ProgressUserData, False);
      except
        //don't let us get out of the concept!
        //so just ignore the users exception
        //After all, we're maybe in a thread!
      end;

    Data.Owner.Free;
    Data.Group.Free;
    Data.DACL.Free;
    Data.SACL.Free;

    //[Hint] hThread := Data^.hThread;

    FreeMem(Data);
  end;

  SetLastError(0);

  Result := 0;
end;


class procedure TJwSecureRegistryKey.TreeKeySetNamedSecurityInfo(
  pKeyName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  //  SECURITY_INFORMATION SecurityInfo,
  const Action:
  TJwProgInvokeSetting;
  const SetType: TJwTreeSetType;
  const bKeepExplicit: boolean;
  const Owner: TJwSecurityId;
  const Group: TJwSecurityId;
  const DACL:
  TJwDAccessControlList;
  const SACL:
  TJwSAccessControlList;
  const FNProgressMethod:
  TJwFnProgressMethod;
  const FNProgressProcedure: TJwFnProgressProcedure;
  const ProgressUserData
  : Pointer;
  const aThread: TJwTagThread;
  const Disable64Redirection:
  boolean = False);

var
  Data: PThreadData;
  aData: TThreadData;
  //[Hint] lpThreadId : Cardinal;
  bAbort: boolean;
  p: TJwProgInvokeSetting;
begin
  //hThreadHandle := INVALID_HANDLE_VALUE;
  CheckKeyNameValidity(pKeyName);

  if Assigned(aThread) then
  begin
    GetMem(Data, sizeof(TThreadData));
    FillChar(Data^, sizeof(TThreadData), 0);

    if Data <> nil then
    begin
      //create copies for thread

      if Assigned(Owner) then
        Data.Owner := TJwSecurityId(Owner);

      if Assigned(Group) then
        Data.Group := TJwSecurityId(Group);

      if Assigned(DACL) then
      begin
        Data.DACL := TJwDAccessControlList.Create();
        Data.DACL.Assign(DACL);
      end;

      if Assigned(SACL) then
      begin
        Data.SACL := TJwSAccessControlList.Create();
        Data.SACL.Assign(SACL);
      end;

      Data.pObjectName := pKeyName;

      Data.Disable64Redirection := Disable64Redirection;
      Data.aSecurityInfo := aSecurityInfo;
      //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      Data.Action  := Action;//const Action : TJwProgInvokeSetting;
      Data.SetType := SetType;
      Data.FNProgressMethod := FNProgressMethod;
      //const FNProgressMethod : TJwFnProgressMethod;
      Data.FNProgressProcedure := FNProgressProcedure;
      //const FNProgressProcedure : TJwFnProgressProcedure;
      Data.ProgressUserData := ProgressUserData;
      //const ProgressUserData : Pointer;
      Data.bKeepExplicit := bKeepExplicit;

      aThread.fOnExecute := TreeRegKeySetNamedSecurityInfo_Execute;
      aThread.pTag := Data;
      aThread.Resume;
    end;
  end
  else
  begin
    FillChar(aData, sizeof(aData), 0);

    try
      //create copies for thread
      if Assigned(Owner) then
        aData.Owner := TJwSecurityId(Owner);


      if Assigned(Group) then
        aData.Group := TJwSecurityId(Group);

      if Assigned(DACL) then
      begin
        aData.DACL := TJwDAccessControlList.Create();
        aData.DACL.Assign(DACL);
      end;

      if Assigned(SACL) then
      begin
        aData.SACL := TJwSAccessControlList.Create();
        aData.SACL.Assign(SACL);
      end;

      try
        IterateKey(pKeyName,
          aSecurityInfo,
          //const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
          Action,//const Action : TJwProgInvokeSetting;
          SetType,
          bKeepExplicit,
          aData.Owner,//const Owner : TJwSecurityId;
          aData.Group,//const Group : TJwSecurityId;
          aData.DACL, //const DACL : TJwDAccessControlList;
          aData.SACL, //const SACL : TJwSAccessControlList;
          FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
          FNProgressProcedure,//const FNProgressProcedure : TJwFnProgressProcedure;
          ProgressUserData,//const ProgressUserData : Pointer;
          True, //first call of this function = true
          Disable64Redirection,
          bAbort
          );
      finally
        if Assigned(FNProgressMethod) then
          try
            p := pis_ProgressFinished;
            FNProgressMethod(pKeyName, 0, p, nil, ProgressUserData, False);
          except
          end;

        if Assigned(FNProgressProcedure) then
          try
            p := pis_ProgressFinished;
            FNProgressProcedure(pKeyName, 0, p, nil,
              ProgressUserData, False);
          except
          end;
      end;
    finally
      aData.Owner.Free;
      aData.Group.Free;
      aData.DACL.Free;
      aData.SACL.Free;
    end;

  end;
end;


class function TJwSecureBaseClass.GetOwnerShipMembers: TJwSecurityIdList;
var
  token: TJwSecurityToken;
  groups: TJwSecurityIdList;
  i: integer;
begin                              
  token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ);
  try
    Result := TJwSecurityIdList.Create(True);

    groups := token.GetTokenGroups;
    try                            
      Result.add(token.GetTokenOwner);

      for i := 0 to groups.Count - 1 do
      begin
        if groups[i].Attributes and SE_GROUP_OWNER = SE_GROUP_OWNER then
          Result.Add(TJwSecurityId.Create(groups[i]));
      end;
    finally           
      FreeAndNil(groups);
    end;
  finally
    FreeAndNil(Token);
  end;

end;





class function TJwSecureBaseClass.ConvertMaximumAllowed(
  const SecurityDescriptor: TJwSecurityDescriptor;
  const ClientToken: TJwSecurityToken;
  const GenericMapping: TJwSecurityGenericMappingClass): TAccessMask;

var PrivilegeSet: TJwPrivilegeSet;
    AccessStatus: Boolean;
begin
  AccessCheck(SecurityDescriptor,
    ClientToken,
    MAXIMUM_ALLOWED,
    GenericMapping,
    PrivilegeSet,
    result,
    AccessStatus);

  if Assigned(PrivilegeSet) then
    FreeAndNil(PrivilegeSet);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}





initialization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_INITIALIZATION_SECTION}
  InitializeCriticalSection(_Wow64FsRedirection_Critical);
{$ENDIF SL_INITIALIZATION_SECTION}

//  CacheList := TStringList.Create;

{$IFNDEF SL_OMIT_SECTIONS}
finalization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_FINALIZATION_SECTION}
  DeleteCriticalSection(_Wow64FsRedirection_Critical);
{$ENDIF SL_FINALIZATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
