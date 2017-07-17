{ Description
  Project JEDI Windows Security Code Library (JWSCL)
  
  Contains access control classes that are used by the units of JWSCL
  Author
  Christian Wimmer
  License
  The contents of this file are subject to the Mozilla Public License Version 1.1
  (the "License"); you may not use this file except in compliance with the
  \License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
  
  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
  specific language governing rights and limitations under the License.
  
  Alternatively, the contents of this file may be used under the terms of the GNU
  Lesser General Public License (the "LGPL License"), in which case the provisions
  of the LGPL License are applicable instead of those above. If you wish to allow
  use of your version of this file only under the terms of the LGPL License and
  not to allow others to use your version of this file under the MPL, indicate
  your decision by deleting the provisions above and replace them with the notice
  and other provisions required by the LGPL License. If you do not delete the
  provisions above, a recipient may use your version of this file under either the
  MPL or the LGPL License.
  
  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  Note
  The Original Code is JwsclDescriptor.pas.
  
  The Initial Developer of the Original Code is Christian Wimmer. Portions created
  by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.
  
  
  Link List
  \Links Secure object types:
  http://msdn2.microsoft.com/en-us/library/aa379593.aspx
  
  
                                                                                   }
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclDescriptor;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes,
  JwaWindows, JwsclResource, JwsclMapping,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl,
  JwsclVersion, JwsclConstants,  
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
   {<B>TJwHashCodeMethod</B> defines a callback method for calculating a hash value.
     Its used by  TJwSecurityDescriptor.OnHashCodeMethod .
   @param val Contains a pointer to data which is used to calculate the hash
   @param size Contains the size of the data in val. 
   @return Returns the hash value
   }
  TJwHashCodeMethod = function(val: Pointer; size: integer): int64 of object;


type
  TJwSecurityDescriptor = class;

  TJwSecurityDescriptorArray = array of TJwSecurityDescriptor;


  {<B>TJwSecurityDescriptor</B> contains information about a security descriptor.
      It does not hold a pointer to a security descriptor.
       It is created on demand.
      }
  TJwSecurityDescriptor = class
  private

  protected
    fSD: PSecurityDescriptor;

    fOwner, fPrimaryGroup: TJwSecurityId;
    fOwnDACL: boolean;

    fOnHashCodeMethod: TJwHashCodeMethod;

    fOwnOwner, fOwnPrimaryGroup:     boolean;
    fOwnerInherited, fPrimaryGroupInherited: boolean;
    fDACLInherited, fAuditInherited: boolean;

    fDACLGenericRemoved,
    fInheritHandles : Boolean;

    fControl: TJwSecurityDescriptorControlSet;

    fDACL:     TJwDAccessControlList;
    fAuditACL: TJwSAccessControlList;

    fDACLPresent : Boolean;

    fProtectionDACLState,
    fProtectionSACLState  : TJwACLProtection;
    fTag : Integer;



    { function GetOwner : TJwSecurityId;}
    procedure SetOwner(const Value: TJwSecurityId);

    {  function GetPrimaryGroup : TJwSecurityId;            }
    procedure SetPrimaryGroup(aGroup: TJwSecurityId);

    function GetDACL: TJwDAccessControlList;
    procedure SetDACL(anACL: TJwDAccessControlList);

    function GetSACL: TJwSAccessControlList;
    procedure SetSACL(anACL: TJwSAccessControlList);

    //<B>InitializeSD</B> receives a SD and initialises the instance member variables
    procedure InitializeSD(aSecurityDescriptor:
      jwaWindows.PSecurityDescriptor);

    //<B>Done</B> destroys the var members of the instance
    procedure Done; virtual;

    //<B>GetText</B> see property Text for more information
    function GetText: TJwString; virtual;

    function GetRMControl: jwaWindows.TSecurityDescriptorControl; virtual;
    procedure SetRMControl(aRMControl:
      jwaWindows.TSecurityDescriptorControl); virtual;
    procedure SetControl(aControl: TJwSecurityDescriptorControlSet); virtual;

    procedure Init(CreateDACL : Boolean);

    procedure SetProtectedState(Index : Integer; const Protect : TJwACLProtection); virtual;
    function GetProtectedState(Index : Integer) : TJwACLProtection; virtual;
  public
       {<B>Create</B> creates an empty security descriptor.
        The property pSD is initialised and ready to be filled.

        By default the DACL is initialized with no entries so everyone is denied access.
        Set DACL to nil to allow everyone access but do not free it by calling Free.
        DACL := Nil;
        }
    constructor Create; overload;

       {<B>Create</B> creates a new TJwSecurityDescriptor instance by copying from an existing one defined in the lonely parameter.
        For this purpose it uses Assign.
        If aSecurityDescriptor is nil the new instance will be empty.

        @param aSecurityDescriptor Defines an instance to be copied. Can be nil that is equal to a call to Create; .
       }
    constructor Create(aSecurityDescriptor: TJwSecurityDescriptor);
      overload;


    {<B>CreatePrivateObjectSecurity</B> combines a parent and a creator security descriptor into a new security descriptor.
     For detailed information see MSDN http://msdn2.microsoft.com/en-us/library/aa446581.aspx

     @param ObjectType A Pointer to a GUID that defines the type. Set to nil if it does not exist. 
     @param GenericMap Defines the generic map class which maps generic access rights to specific access rights. 
     @param Token defines the token instance which is used to check for access. Can be nil to use
          process or thread token. 

     }
    constructor CreatePrivateObjectSecurity(
      const ParentSecurityDescriptor: TJwSecurityDescriptor;
      const CreatorSecurityDescriptor: TJwSecurityDescriptor;
      const ObjectType : PGUID;
      const IsDirectoryObject : Boolean;
      const AutoInheritFlags : TJwInheritFlagSet;
      const GenericMap : TJwSecurityGenericMappingClass;
      const Token : TObject = nil);


    {<B>Create</B> creates a new security descriptor and copies the defines members
     of a default security descriptor.
     @param SecurityInformationSet defines which members are copied from
       the default security descriptor 
    }
    constructor Create(const SecurityInformationSet : TJwSecurityInformationFlagSet;
      SecurityDescriptor: TJwSecurityDescriptor);
      overload;

    {<B>CreateDefaultByToken</B> creates a default security descriptor. It will contain the same
    elements as if a securable object (like mutex) is created without a SD.
    @param DefaultToken defines a user defined token to be used. It must be of type
            TJwSecurityToken. (because of unit dependings it cannot be the correct type) 
    @param RequestedTokenType defines which token should be used for the new SD.
           If parameter DefaultToken is not nil, RequestedTokenType will be ignored.
          The following values are possible.  
          
            # rttAuto The token of the thread will be used if any; otherwise
                  the process token. 
            # rttTokenPrimary The process token is forced to use.
                    See TJwSecurityToken.CreateTokenByProcess for more information 
            # rttTokenImpersonation The thread token is forced to be used.
                  The token is opened against the process rights.
                    See TJwSecurityToken.CreateTokenByThread for more information 
          
      
    raises
 EJwsclInvalidParameterException:  will be raised if parameter DefaultToken
        is not of type TJwSecurityToken. 
     EJwsclNoThreadTokenAvailable: will be raised if parameter RequestedTokenType
       defines rttTokenImpersonation and no thread token is available 
    }
    constructor CreateDefaultByToken(const DefaultToken : TObject = nil;
        const RequestedTokenType: TJwRequestedTokenType = rttAuto);

       {<B>Create</B> create a new TJwSecurityDescriptor instance from a security descriptor.
        The sd can be a self relative or absolute sd.

        @param aSecurityDescriptor Defines a winapi PSecurityDescriptor
        raises
 EJwsclNILParameterException:  will be raised if parameter aSecurityDescriptor is nil.
       }
    constructor Create(aSecurityDescriptor:
      jwaWindows.PSecurityDescriptor);
      overload;

       {<B>Create</B> creates a new TJwSecurityDescriptor instance from a winapi security string.
        For more information see http://msdn2.microsoft.com/en-us/library/aa379570.aspx

        @param aSDString contains the string to be parsed into a sd.
        raises
 EJwsclWinCallFailedException:  will be raised if the string could not be parsed correctly.
        }

    constructor Create(const aSDString:
 {$IFNDEF SL_OMIT_SECTIONS}JwsclStrings.{$ENDIF SL_OMIT_SECTIONS}TJwString);
      overload;

       {<B>Create</B> creates a new TJwSecurityDescriptor using information from a stream.
        See LoadFromStream  for more information.
       }
    constructor Create(const Stream: TStream); overload;

       {<B>Destroy</B> destroys the instance and its properties if necessary.

       
        #  Owner will be freed if OwnOwner is true
        #  PrimaryGroup will be freed if OwnPrimaryGroup is true
        #  DACL will always be freed. The ACEs will be freed if DACL.ownObjects is true.
        #  AuditACL/SACL will alway be freed. The ACEs will be freed if DACL.ownObjects is true.
        

       }
    destructor Destroy; override;

       {<B>Assign</B> copies all properties from another SD.
        It creates new instances of (if not nil)
        Owner, PrimaryGroup
        DACL and AuditACL will be cleared and filled with the ACL of the SD aObject.
        All the aces in the DACL and SACL structures are copied into new instances.
        The DACL can also be nil if the DACL is a NULL DACL (allows everyone access)

        @param aObject Defines a security descriptor that shall be copied into the existing one.
       }
    procedure Assign(aObject: TJwSecurityDescriptor);

       {<B>Create_SD</B> creates a self relative (continuous block of memory) or an absolute (using pointers to owner, DACL, ...)
         security descriptor (SD).
        The created SD must be freed using Free_SD .

        The function calls a lot of winapi function that can fail. So check for exception. See the exceptions.
        In a case of exception the return value is undefined. However already allocated memory is freed.
        A case for a failure can be an incorrect owner SID, DACLs or out of memory.

        Do not edit the memory block manually or even free sub structures (as owner, dacl...)
        <B>Create_SD</B> uses GetMem for sd memory allocation.

        You can set Control or RMControl to modify the resulting control value of the security descriptor block.
        The values SE_DACL_PRESENT and SE_SACL_PRESENT are always automatically set.
        SE_RELATIVE is set, if parameter bRelative is true.
        SE_SACL_PRESENT will be set if property SACL/AuditACL is not nil.

        @param bRelative The boolean parameter defines whether the sd is a self relative if true or otherwise an absolute SD (false).

        raises
 EJwsclWinCallFailedException:  If a call to a winapi function failed.

       }
    function Create_SD(bRelative: boolean = True): PSecurityDescriptor;
      overload;


       {<B>Create_SD</B> creates a self relative (continuous block of memory) or an absolute (using pointers to owner, DACL, ...)
         security descriptor (SD).

        The function calls a lot of winapi function that can fail. So check for exception. See the exceptions.
        In a case of exception the return value is undefined. However already allocated memory is freed.
        A case for a failure can be an incorrect owner SID, DACLs or out of memory.

        Do not edit the memory block manually or even free sub structures (as owner, dacl...)
        <B>Create_SD</B> uses GetMem for sd memory allocation.

        You can set Control or RMControl to modify the resulting control value of the security descriptor block.
        The values SE_DACL_PRESENT and SE_SACL_PRESENT are always automatically set.
        SE_RELATIVE is set, if parameter bRelative is true.
        SE_SACL_PRESENT will be set if property SACL/AuditACL is not nil.

        @param ipSDSize [out] receives the size of the returned security descriptor block. It is sizeof(TJwSecurityDescriptor)
                if parameter bRelative is false (absolute SD) otherwise it is a non zero value.

        @param bRelative The boolean parameter defines whether the sd is a self relative if true or otherwise an absolute SD (false).

        raises
 EJwsclWinCallFailedException:  If a call to a winapi function failed.
       }
    function Create_SD(out ipSDSize: Cardinal;
      bRelative: boolean {= true}): PSecurityDescriptor; overload;

       {<B>Free_SD</B> frees an security descriptor that was allocated by Create_SD.
        Only use a SD that was created by Create_SD. <B>Free_SD</B> uses FreeMem and
          TJwSecurityAccessControlList.Free_PACL to free memory.
        For unknown reasons some winapi calls fails if the the applied memory was allocated with GetMem.

        <B>Free_SD</B> does not raise a SM exception. However there can be exception because of pointer problems.
        Do not edit the memory block manually or even free sub structures (as owner, dacl...)

        @param SD The SD parameter contains the SD to be freed. It is nil afterwards. If a nil pointer was given the function does nothing.
       }
    class procedure Free_SD(var SD: PSecurityDescriptor);

       {<B>Create_SA</B> creates a Security Attributes structure and initialises it with the security descriptor of this instance.
        The SA structure must be freed by Free_SA. The internal SD structure is automatically freed.

        This method uses GetMem.

        You can set Control or RMControl to modify the resulting control value of the security descriptor block.
        The values SE_DACL_PRESENT and SE_SACL_PRESENT are always automatically set.
        SE_RELATIVE is set, if parameter bRelative is true.
        SE_SACL_PRESENT will be set if property SACL/AuditACL is not nil.

        @param bInheritHandle A Boolean value that specifies whether the returned handle is inherited when a new process is created. If this member is TRUE, the new process inherits the handle.
        @param bSDRelative A Boolean value that defines whether the internal security descriptor is relative (true) or absolute (false).
        @return Returns a pointer to a security attribute.
        raises
 EJwsclWinCallFailedException:  see Create_SD for more information about raised exception.
       }
    function Create_SA(bInheritHandle: boolean = False;
      bSDRelative: boolean = False): PSecurityAttributes;

    {<B>Create_SA2</B> does the same as Create_SA, but uses the property InheritHandles
     to initialize the result.

     See Create_SA for more information.
     }
    function Create_SA2(bSDRelative: boolean = False): PSecurityAttributes;

       {<B>Create_SAEx</B> creates a security attributes structure on the stack. However the internal security descriptor will be created on heap.

        The SA structure must be freed by Free_SAEx. The internal SD structure is automatically freed.

        You can set Control or RMControl to modify the resulting control value of the security descriptor block.
        The values SE_DACL_PRESENT and SE_SACL_PRESENT are always automatically set.
        SE_RELATIVE is set, if parameter bRelative is true.
        SE_SACL_PRESENT will be set if property SACL/AuditACL is not nil.

        @param bInheritHandle A Boolean value that specifies whether the returned handle is inherited when a new process is created. If this member is TRUE, the new process inherits the handle.
        @param bSDRelative A Boolean value that defines whether the internal security descriptor is relative (true) or absolute (false).
        @return Returns security attribute structure.
        raises
 EJwsclWinCallFailedException:  see Create_SD for more information about raised exception.
       }
    function Create_SAEx(bInheritHandle: boolean = False;
      bSDRelative: boolean = False): TSecurityAttributes;

    {<B>Create_SAEx2</B> does the same as Create_SAEx, but uses the property InheritHandles
     to initialize the result.

     See Create_SAEx for more information.
     }
    function Create_SAEx2(bSDRelative: boolean = False): TSecurityAttributes;


       {<B>Free_SA</B> frees a security attribute created by Create_SA.

        This method uses FreeMem, so do not use this function with security attributes structure that was created using LocalAlloc,
        GlobalAlloc or other incompatible functions.


        @param SA [in,out] SA receives the security attribute to be freed. It is set to nil after an successfull deallocation.
        raises
 EJwsclSecurityException:  The exception EJwsclSecurityException is raised if the internal nLength component is not the sizeof TSecurityAttributes.
       }
    class procedure Free_SA(var SA: PSecurityAttributes);

  //  class procedure Free_LPSA(var SA: LPSECURITY_ATTRIBUTES);

       {<B>Free_SAEx</B> frees a security attribute created by Create_SAEx.

        This method uses FreeMem, so do not use this function with security attributes structure that was created using LocalAlloc,
        GlobalAlloc or other incompatible functions.

        @param SA [in,out] SA receives the security attribute to be freed. Its members are set to nil after an successfull deallocation.
        raises
 EJwsclSecurityException:  The exception EJwsclSecurityException is raised if the internal nLength component is not the sizeof TSecurityAttributes.
       }
    class procedure Free_SAEx(var SA: TSecurityAttributes);

 //   class procedure Free_LPSAEx(var SA: LPSECURITY_ATTRIBUTES);

       {<B>SaveToStream</B> writes a relative security descriptor into a stream.
        The method uses a magic header to check for position errors in a stream.


        # Bytes         |  Value 
        # 1..5 (5)      |  SD_MAGIC_HEADER (byte array) 
        # 6..9 (4)      |  SD size (Cardinal) 
        # 10..17 (8)    |  hash value (Cardinal) 
        # 18..18 (1)    |  hash value in use (byte) true if 255 otherwise false.
               == SD_HEADER_SIZE 
        # 19..sd (size) |  security descriptor data 
    


       }
    procedure SaveToStream(const Stream: TStream); virtual;

       {<B>LoadFromStream</B> loads a security descriptor from a stream.

        The stream position must be on the first value of the magic header. (see SaveToStream ).
        If the following values are true the hash value will be checked and an exception EJwsclStreamHashException will
        be raised if the read hash is not equal to the calculated one.
          Assigned(OnHashCodeMethod) and (ReadHash > 0) and (CalculatedHash > 0) and (iCHash <> iRHash)
        A hash comparison will only be done if the hashes are greater than zero.

        raises EJwsclStreamHashException will be raised if the hash ist not valid.
        raises EJwsclInvalidSecurityDescriptor will be raised if the security descriptor read from the stream is not valid (uses IsValidSecurityDescriptor)
               Currently only the version of SD is tested.

       }
    procedure LoadFromStream(const Stream: TStream); virtual;

       {<B>hashCode</B> creates a value out of a buffer with a given size.
        This pseudo hash function is not intended for production uses and
        should be replaced by a custom method using property OnHashCodeMethod.
       }
    class function hashCode(val: Pointer; size: integer): int64;


    function IsEqual(const SD: TJwSecurityDescriptor;
      const Flags: TJwSecurityInformationFlagSet): boolean;

    //see StringSD[SIFlags :TSecurityInformation] for more information
    function GetSecurityDescriptorString(SIFlags: TSecurityInformation): TJwString; overload;
    function GetSecurityDescriptorString(SIFlags: TJwSecurityInformationFlagSet): TJwString; overload;

    {<B>ReplaceDescriptorElements</B> replaces the security descriptor elements given in SecurityInformationSet with
     the ones in SecurityDescriptor.
     @param SecurityInformationSet Contains the members of the SD to be replaced. 
     @param SecurityDescriptor defines the SD which is used to copy the members into
        the instance. 
    }
    procedure ReplaceDescriptorElements(const SecurityInformationSet :
      TJwSecurityInformationFlagSet;
      const SecurityDescriptor : TJwSecurityDescriptor); virtual;

    {<B>SetPrivateObjectSecurity</B> combines a parent and a creator security descriptor into a new security descriptor.
     For detailed information see MSDN http://msdn2.microsoft.com/en-us/library/aa379581.aspx

     If parameter Token is not nil and SecurityInformation contains siSaclSecurityInformation
     you must explicit activate SE_SECURITY_NAME privilege.

     @param GenericMap Defines the generic map class which maps generic access rights to specific access rights. 
     @param Token defines the token instance which is used to check for access. Can be nil to use
          process or thread token. 

     raises
 EJwsclNILParameterException:  will be raised if ModificationDescriptor is nil
      EJwsclInvalidParameterException: will be raisef if the type of parameter Token
      is not a TJwSecurityToken class 
      EJwsclSACLAccessDenied: will be raised if SecurityInformation contains siSaclSecurityInformation,
      Token is nil and the current token does not hold SE_SECURITY_NAME privilege 

     }
    procedure SetPrivateObjectSecurity(
      const SecurityInformation : TJwSecurityInformationFlagSet;
      const ModificationDescriptor : TJwSecurityDescriptor;
      const AutoInheritFlags : TJwInheritFlagSet;
      const GenericMap: TJwSecurityGenericMappingClass;
      const Token: TObject = nil); virtual;

    function GetPrivateObjectSecurity(const SecurityInformation
      : TJwSecurityInformationFlagSet) : TJwSecurityDescriptor; virtual;

    function GetTextMap(const Mapping: TJwSecurityGenericMappingClass =
      nil): TJwString;

    procedure ReplaceOwner(const Sid : TJwSecurityId);
  public
    {<B>Owner</B> sets or gets the owner of the SD.
    If the property OwnOwner is true and the property is set, the old Owner TJwSecurityId instance will be freed and
     the new owner will be copied into a new instance. So there will be two instances of this SID and
      the original instance is not touched and must be freed if necessary.
    If the property OwnOwner is false, the old Owner TJwSecurityId will not be freed and
    the new one will directly point to the new SID.

    The following code can be used to set a newly created instance.
    <code lang="Delphi">
       //first free or disconnect old owner
       //1. If OwnOwner is true, the Owner instance will be freed
       //2. If OwnOwner is false, the property will be set to nil
       Owner := nil;
       OwnOwner := false; //set to false so the next step does not copy the security id in a new instance
       Owner := TJwSecurityID.Create(..); //set new Sid
       OwnOwner := true; //lets free the Sid automatically
    </code>
    This code is equivalent:
    <code lang="Delphi">
       Owner := nil; //free or release old owner
       OwnOwner := false; //set to false so the next step does not copy the security id in a new instance
       Owner := TJwSecurityID.Create(SecurityDescriptor.Owner);
       OwnOwner := true; //lets free the Sid automatically
    </code>

    Use this code to release old owner and copy new owner into a new instance:
    <code lang="Delphi">
       Owner := nil; //free or release old owner
       OwnOwner := true; //next set copies owner
       Owner := SecurityDescriptor.Owner; //create copy of owner and set it
    </code>




    Use this code to use the same instance from another SD instance in both
    security descriptors. In this instance the owner will not be freed.
    You should free this instance first, before freeing the other one because
    if the original instance is freed you cannot access the owner because
    it is invalid but differs from nil.

    <code lang="Delphi">
     Owner := nil; //free or release old owner
     OwnOwner := false; //do not free owner
     Owner := SecurityDescriptor.Owner; //just point to this instance
    </code>
    }
    property Owner: TJwSecurityId Read fOwner Write SetOwner;

       {<B>PrimaryGroup</B> sets or gets the group of the SD.
         If the property OwnPrimaryGroup is true and the property is set, the old Owner TJwSecurityId instance will be freed and
         the new owner will be copied into a new instance. So there will be two instances of this SID and
          the original instance is not touched and must be freed if necessary.
        If the property OwnPrimaryGroup is false, the old Owner TJwSecurityId will not be freed and
        the new one will directly point to the new SID.

        See Owner  for information about how to use this property.
       }
    property PrimaryGroup: TJwSecurityId
      Read fPrimaryGroup Write SetPrimaryGroup;


     {<B>DACL</B> sets or gets the discretionary access control list.
      The read value is the internal used DACL. So do not free it directly. Instead set the write value to nil.
      The write value is copied into a new DACL (using Assign) if the property OwnDACL is false
       otherwise the given DACL instance is used directly (using ":=").

      If the write value is nil the internal list is freed and set to nil.

      The following code releases an old DACL and copies an existing one
      into the SD. At the end there are two DACL instances that will contain
      the same content.
      <code lang="Delphi">
      //first release old DACL.
      //1. if OwnDACL is true, the DACL is freed
      //2. if OwnDACL is false, the DACL is dismissed without freeing it
      DACL := nil;
      //Set OwnDACL to false, so the DACL will be copied into a brand new  instance.
      OwnDACL := false;
      DACL := SecurityDescriptor.DACL;
      //Now let handle the SD freeing it on destruction.
      OwnDACL := true;</code>

      The following code can be used if an old DACL must be released and
      a new one created. The newly created instance will directly become
      the value of the DACL property. It will not be copied but freed on
      destruction of the security descriptor instance.
      <code lang="Delphi">
      //first release old DACL.
      //1. if OwnDACL is true, the DACL is freed
      //2. if OwnDACL is false, the DACL is dismissed without freeing it
      DACL := nil;
      //Set OwnDACL to true, so we get the new instance and use it directly
      OwnDACL := true;
      DACL := TJwDAccessControlList.Create;
      //Now let handle the SD freeing it on destruction.
      OwnDACL := true;</code>
     }
    property DACL: TJwDAccessControlList Read GetDACL Write SetDACL;

    {<B>OwnDACL</B> defines whether the DACL is copied into a new instance (true) and freed at the end
        or points directly to the set DACL.
    }
    property OwnDACL: boolean Read fOwnDACL Write fOwnDACL;

       {<B>AuditACL</B> gets the auditing access control list.
        It returns the internal auditing access control list so do not call Free.
        If the audit ACL is set, it copies the SACL into a new structure, so
         the original list is not touched. 
       }
    property AuditACL: TJwSAccessControlList Read GetSACL Write SetSACL;

       {<B>SACL</B> is the same as the property AuditACL.
        If the audit ACL is set, it copies the SACL into a new structure, so
         the original list is not touched.
        }
    property SACL: TJwSAccessControlList Read GetSACL Write SetSACL;

   {<B>RMControl</B> sets or gets the resource managercontrol values of the sd.
    Do not change them if you do not know what it means.
        For more information see MSDN.
    This value is ignored in current version.
    }
    property RMControl: jwaWindows.TSecurityDescriptorControl
      Read GetRMControl Write SetRMControl;


    {<B>Control</B> defines internal security descriptor controls. Do not
    make write calls to it.}
    property Control: TJwSecurityDescriptorControlSet
      Read fControl Write SetControl;

    {<B>InheritanceDACLProtection</B> defines whether the DACL is protected against inheritance flow or not.
     Use aclpForceUnprotect instead of aclpUnprotected to let flow inheritance.

     }
    property InheritanceDACLProtection : TJwACLProtection index 0 read GetProtectedState write SetProtectedState;

    {<B>InheritanceSACLProtection</B> defines whether the SACL is protected against inheritance flow or not.
     Use aclpForceUnprotect instead of aclpUnprotected to let flow inheritance.
     }
    property InheritanceSACLProtection : TJwACLProtection index 1 read GetProtectedState write SetProtectedState;



     {<B>OwnOwner</B> defines whether the owner SID shall be freed on destruction (true) or not (false)
      If the property OwnOwner is true and the property Owner is set, the old Owner TJwSecurityId instance will be freed and
       the new owner will be copied into a new instance. So there will be two instances of this SID and
        the original instance is not touched and must be freed if necessary.

       If the property OwnOwner is false, the old Owner TJwSecurityId will not be freed and
      the new one will directly point to the new SID.

      See Owner  for information about how to use this property.
      }
    property OwnOwner: boolean Read fOwnOwner Write fOwnOwner;

       {<B>OwnPrimaryGroup</B> defines whether the group SID shall be freed on destruction (true) or not (false)

        If the property OwnPrimaryGroup is true and the property is set, the old Owner TJwSecurityId instance will be freed and
         the new owner will be copied into a new instance. So there will be two instances of this SID and
          the original instance is not touched and must be freed if necessary.
        If the property OwnPrimaryGroup is false, the old Owner TJwSecurityId will not be freed and
        the new one will directly point to the new SID.

        See Owner  for information about how to use this property.
        }
    property OwnPrimaryGroup: boolean
      Read fOwnPrimaryGroup Write fOwnPrimaryGroup;

       {<B>OwnerInherited</B> defines whether the owner sid is inherited (true) or not (false)
        Indicates whether the owner information is derived from a default mechanism.
        If this value is TRUE, it is default information. The function stores this value as
        the SE_OWNER_DEFAULTED flag in the SECURITY_DESCRIPTOR_CONTROL structure.
        If this parameter is zero, the SE_OWNER_DEFAULTED flag is cleared.
        (source: http://msdn2.microsoft.com/en-us/library/aa379585.aspx)

        The Control flag is only updated in a newly created SD allocated by Create_SD.
       }
    property OwnerInherited: boolean
      Read fOwnerInherited Write fOwnerInherited;

       {<B>PrimaryGroupInherited</B> defines whethere the group sid is inherited (true) or not (false)

        Indicates whether the primary group information was derived from a default mechanism.
        If this value is TRUE, it is default information, and the function stores this value as the
        SE_GROUP_DEFAULTED flag in the SECURITY_DESCRIPTOR_CONTROL structure.
        If this parameter is zero, the SE_GROUP_DEFAULTED flag is cleared.
        (source: http://msdn2.microsoft.com/en-us/library/aa379584.aspx);

        The Control flag is only updated in a newly created SD allocated by Create_SD.
        }
    property PrimaryGroupInherited: boolean
      Read fPrimaryGroupInherited Write fPrimaryGroupInherited;


       {
       A flag that indicates the source of the DACL. If this flag is TRUE,
       the DACL has been retrieved by some default mechanism.
       If FALSE, the DACL has been explicitly specified by a user.
       The function stores this value in the SE_DACL_DEFAULTED flag of the SECURITY_DESCRIPTOR_CONTROL structure.
       If this parameter is not specified, the SE_DACL_DEFAULTED flag is cleared.
       (source: http://msdn2.microsoft.com/en-us/library/aa379583.aspx);

       The Control flag is only updated in a newly created SD allocated by Create_SD.
       }
    property DACLInherited: boolean Read fDACLInherited
      Write fDACLInherited;


       {Indicates the source of the SACL. If this flag is TRUE, the SACL has been retrieved by some default mechanism.
       If it is FALSE, the SACL has been explicitly specified by a user. The function stores this value in
       the SE_SACL_DEFAULTED flag of the SECURITY_DESCRIPTOR_CONTROL structure.
       If this parameter is not specified, the SE_SACL_DEFAULTED flag is cleared.

        (source: http://msdn2.microsoft.com/en-us/library/aa379587.aspx)

       The Control flag is only updated in a newly created SD allocated by Create_SD.
       }
    property AuditInherited: boolean
      Read fAuditInherited Write fAuditInherited;

    {This property is useful to determine whether the property DACL should be
    considered if its value is nil. A nil DACL is considered as "allow everybody".
    If DACLPresent is true and DACL is nil and any of the Create_SD and Create_SA
    function is called, the newly created winapi security descriptor will have a
    NULL DACL and so allow everybody access; otherwise the SD will not have a DACL at all.

    This situation is equal to a DACL with an access entry that grants GENERIC_ALL to World SID.

    This property is automatically set to true if a DACL was set to a value different to nil;
    otherwise it won't be set.

    The initial value is true.
    }
    property DACLPresent : Boolean read fDACLPresent write fDACLPresent;



(*       {<B>Text</B> creates a security string descriptor.
        You can set flags to define which information is placed in the newly created string.
        The following flags can be combined with OR:
        
         #  OWNER_SECURITY_INFORMATION
         #  GROUP_SECURITY_INFORMATION
         #  DACL_SECURITY_INFORMATION
         #  SACL_SECURITY_INFORMATION
         #  LABEL_SECURITY_INFORMATION (only available in Vista; otherwise it is ignored)
         #  ALL_SECURITY_INFORMATION - combines all flags above

        }
    property StringSD[SIFlags: TSecurityInformation]: TJwString
      Read GetSecurityDescriptorString;   *)

    {<B>Text</B> returns a text that descripes the security descriptor in a human readable format.}
    property Text: TJwString Read GetText;

       {<B>OnHashCodeMethod</B> sets or gets the stream hash function used by SavetoStream and LoadFromStream
        to generate a hash value. If the property is nil the hash code in the stream is set to
        0 in SaveToStream and an loaded hash value is ignored in LoadFromStream.
       }
    property OnHashCodeMethod: TJwHashCodeMethod
      Read fOnHashCodeMethod Write fOnHashCodeMethod;

    {<B>InheritHandles</B> is custom flag that defines whether handles are inherited (true) or not.
     This property is not used by @ClassName. However some JWSCL methods use
     it instead of the structure SECURITY_ATTRIBUTES. <B>InheritHandles</B> is used instead of
     the member bInheritHandle of SECURITY_ATTRIBUTES. In fact it is mapped internally
     into a SECURITY_ATTRIBUTES structure.
     }
    property InheritHandles : Boolean read fInheritHandles write fInheritHandles;

    {<b>DACLGenericRemoved</b> is used by TJwSecureFileObject.GetFileInheritanceSourc
    to decided whether the DACL's accessmask has been mapped from generic to
    specific rights.
    }
    property DACLGenericRemoved : Boolean read fDACLGenericRemoved write fDACLGenericRemoved;

    property Tag : Integer read fTag write fTag;
  end;

const       {<B>SD_MAGIC_LENGTH</B> is the size of the magic header length in a security descriptor stream.
       See TJwSecurityDescriptor.SaveToStream and TJwSecurityDescriptor.LoadFromStream}
  SD_MAGIC_LENGTH = 5;

  {<B>SD_MAGIC_HEADER</B> is the header string that initiates a security descriptor stream block used
       by TJwSecurityDescriptor.SaveToStream and TJwSecurityDescriptor.LoadFromStream.
      }
  SD_MAGIC_HEADER : array[0..SD_MAGIC_LENGTH-1] of AnsiChar = (#3,#4,'S','D','H');

      {<B>SD_HEADER_SIZE</B> is the size of the header written into a stream by TJwSecurityDescriptor.SaveToStream 
       and TJwSecurityDescriptor.LoadFromStream }
  SD_HEADER_SIZE = 19;

procedure JwFreeSecurityDescriptorArray(var List : TJwSecurityDescriptorArray);

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math,
     JwsclUtils,
     JwsclEnumerations,
     JwsclPrivileges,
     JwsclSecureObjects,
     JwsclToken;


{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

procedure JwFreeSecurityDescriptorArray(var List : TJwSecurityDescriptorArray);
var i : Integer;
begin
  for i := low(List) to high(List) do
  begin
    FreeAndNil(List[i]);
  end;
  List := nil;
end;

procedure TJwSecurityDescriptor.Init(CreateDACL : Boolean);
begin
  fOwner := nil;
  fPrimaryGroup := nil;

  fDACL := nil;
  //per default we deny everyone - empty DACL
  if CreateDACL then
    fDACL := TJwDAccessControlList.Create(True);
  OwnDACL := true; //we're responsible to free it

  fAuditACL := TJwSAccessControlList.Create(True);

  fOwnOwner := False;
  fOwnPrimaryGroup := False;

  DACLPresent := true;

  fOwnerInherited := False;
  fPrimaryGroupInherited := False;

  fDACLInherited  := False;
  fAuditInherited := False;

  OnHashCodeMethod := hashCode;

  fInheritHandles := false;

  fDACLGenericRemoved := false;

  fControl := [];
end;

constructor TJwSecurityDescriptor.Create;
begin
  Init(true);
end;

procedure TJwSecurityDescriptor.ReplaceDescriptorElements(const SecurityInformationSet :
  TJwSecurityInformationFlagSet;
  const SecurityDescriptor : TJwSecurityDescriptor);
begin
  if not Assigned(SecurityDescriptor) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'ReplaceDescriptorElements',
      ClassName, 'JwsclDescriptor.pas', 0, False,
      ['SecurityDescriptor']);


  if siOwnerSecurityInformation in SecurityInformationSet then
  begin
    Owner := nil; //free or release old owner
    OwnOwner := true; //next set copies owner
    Owner := SecurityDescriptor.Owner;
  end;

  if siGroupSecurityInformation in SecurityInformationSet then
  begin
    PrimaryGroup := nil;
    OwnPrimaryGroup := true;
    PrimaryGroup := SecurityDescriptor.PrimaryGroup;
  end;

  if siDaclSecurityInformation  in SecurityInformationSet then
  begin
    {first release old DACL.
     1. if OwnDACL is true, the DACL is freed
     2. if OwnDACL is false, the DACL is dismissed without freeing it}
    DACL := nil;

    {Set OwnDACL to false, so the DACL will be copied into a brand new
     instance.}
    OwnDACL := false;
    DACL := SecurityDescriptor.DACL;

    {Now let handle the SD freeing it on destruction.}
    OwnDACL := true;
  end;

  if siSaclSecurityInformation in SecurityInformationSet then
  begin
    SACL := SecurityDescriptor.SACL;
  end;
end;

procedure TJwSecurityDescriptor.ReplaceOwner(const Sid: TJwSecurityId);
var Own : Boolean;
begin
  Own := OwnOwner;
  try
    //first free or disconnect old owner
    //1. If OwnOwner is true, the Owner instance will be freed
    //2. If OwnOwner is false, the property will be set to nil
    Owner := nil;
    OwnOwner := false; //set to false so the next step does not copy the security id in a new instance
    Owner := Sid;
  finally
    OwnOwner := Own;
  end;
end;

constructor TJwSecurityDescriptor.Create(const SecurityInformationSet :
  TJwSecurityInformationFlagSet; SecurityDescriptor: TJwSecurityDescriptor);
begin
  Self.Create;
  ReplaceDescriptorElements(SecurityInformationSet, SecurityDescriptor);
end;


constructor TJwSecurityDescriptor.CreateDefaultByToken(
  const DefaultToken : TObject = nil;
  const RequestedTokenType: TJwRequestedTokenType = rttAuto);
  
var OwnToken : Boolean;
    Token : TJwSecurityToken;
begin
  Init(false); //init but creates no empty DACL

  OwnToken := not Assigned(DefaultToken);
  if not OwnToken then
  begin
    if not (Token is TJwSecurityToken) then
      raise EJwsclInvalidParameterException.CreateFmtEx(
        RsTokenInvalidClass,
        'Create',
        'CreateDefaultByToken', RsUNDescriptor, 0, False, [DefaultToken.ClassName]);
    Token := DefaultToken as TJwSecurityToken;
  end
  else
  begin
    case RequestedTokenType of
      rttTokenPrimary :
        Token := TJwSecurityToken.CreateTokenByProcess(0,TOKEN_QUERY or TOKEN_READ);
      rttTokenImpersonation :
        Token := TJwSecurityToken.CreateTokenByThread(0,TOKEN_QUERY or TOKEN_READ,false);
    else
      //rttAuto :
      Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);
    end;
  end;

  try
    {Just free old DACL}
    Self.DACL := nil;

    {Token.TokenDefaultDacl returns a new instance that we can use instantly
    so we dont have to copy it again. Just let the class pointer point to the
     new DACL and the SD frees it.}
    Self.OwnDACL := true;
    Self.DACL := Token.TokenDefaultDacl;

    Self.OwnOwner := false; //do not create a new instance
    Self.Owner := Token.TokenUser;
    Self.OwnOwner := true;

    //Token.PrimaryGroup also already creates new instance
    Self.OwnPrimaryGroup := false;
    Self.PrimaryGroup := Token.PrimaryGroup;
    Self.OwnPrimaryGroup := true;

  finally
    if OwnToken then
      FreeAndNil(Token); //free our token
  end;
end;

constructor TJwSecurityDescriptor.CreatePrivateObjectSecurity(
      const ParentSecurityDescriptor: TJwSecurityDescriptor;
      const CreatorSecurityDescriptor: TJwSecurityDescriptor;
      const ObjectType : PGUID;
      const IsDirectoryObject : Boolean;
      const AutoInheritFlags : TJwInheritFlagSet;
      const GenericMap : TJwSecurityGenericMappingClass;
      const Token : TObject = nil);
var ParentSD,
    CreatorSD,
    NewSD : PSecurityDescriptor;
    TokenInstance : TJwSecurityToken;
    TokenHandle : TJwTokenHandle;

    aMapping: JwaWindows.TGenericMapping;
begin
  ParentSD := nil;
  CreatorSD := nil;
  TokenInstance := nil;

  fOwner := nil;
  fPrimaryGroup := nil;
  fDACL := nil;
  fAuditACL := nil;

  if not Assigned(GenericMap) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsMappingInvalid,
      'Create',
      ClassName, RsUNDescriptor, 0, False, []);

  if Assigned(Token) then
  begin
    if not (Token is TJwSecurityToken) then
      raise EJwsclInvalidParameterException.CreateFmtEx(
        RsTokenInvalidClass,
        'Create',
        ClassName, RsUNDescriptor, 0, False, [Token.ClassName]);
    TokenHandle := (Token as TJwSecurityToken).TokenHandle;
  end
  else
  begin
    //open this token with max access
    //because many preconditions must be set for a successful
    //call to CreatePrivateObjectSecurity
    TokenInstance := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
    TokenHandle := TokenInstance.TokenHandle;
  end;

  if Assigned(ParentSecurityDescriptor) then
    ParentSD := ParentSecurityDescriptor.Create_SD(false);

  if Assigned(CreatorSecurityDescriptor) then
    CreatorSD := CreatorSecurityDescriptor.Create_SD(false);

{$IFDEF FPC}
//   if (GenericMap.ClassType <> TJwSecurityGenericMapping.ClassType) then
{$ELSE}
//   if (not GenericMap.ClassNameIs(TJwSecurityGenericMapping.ClassName)) then
{$ENDIF FPC}
     aMapping := TGenericMapping(GenericMap.GetMapping());
{   else
     ZeroMemory(@aMapping, sizeof(aMapping));
 }
  NewSD := nil;
  try
    if not JwaWindows.CreatePrivateObjectSecurityEx(
        ParentSD,//__in_opt      PSECURITY_DESCRIPTOR ParentDescriptor,
        CreatorSD,//__in_opt      PSECURITY_DESCRIPTOR CreatorDescriptor,
        NewSD,//__out         PSECURITY_DESCRIPTOR* NewDescriptor,
        LPGUID(ObjectType),
        IsDirectoryObject,//__in          BOOL IsDirectoryObject,
        TJwEnumMap.ConvertInheritFlags(AutoInheritFlags),
        TokenHandle,//__in_opt      HANDLE Token,
        aMapping//__in          PGENERIC_MAPPING GenericMapping
     ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      RsUNDescriptor,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CreatePrivateObjectSecurityEx',                   //sWinCall
      ['CreatePrivateObjectSecurityEx']);                                  //const Args: array of const

     try
       Self.Create(NewSD);
     finally
       DestroyPrivateObjectSecurity(NewSD);
     end;
  finally
    TJwSecurityDescriptor.Free_SD(ParentSD);
    TJwSecurityDescriptor.Free_SD(CreatorSD);
    FreeAndNil(TokenInstance);
  end;
end;




constructor TJwSecurityDescriptor.Create(aSecurityDescriptor:
  TJwSecurityDescriptor);
begin
  Self.Create;

  if Assigned(aSecurityDescriptor) then
    Self.Assign(aSecurityDescriptor);
end;

constructor TJwSecurityDescriptor.Create(aSecurityDescriptor:
  jwaWindows.PSecurityDescriptor);
begin
  if aSecurityDescriptor = nil then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'Create(aSecurityDescriptor : jwaWindows.PSecurityDescriptor)',
      ClassName, 'JwsclDescriptor.pas', 0, False,
      ['aSecurityDescriptor']);

  Self.Create;

  try
    InitializeSD(aSecurityDescriptor);
  except
    Done;
    raise;
  end;
end;

constructor TJwSecurityDescriptor.Create(const aSDString:
 {$IFNDEF SL_OMIT_SECTIONS}JwsclStrings.{$ENDIF SL_OMIT_SECTIONS}TJwString);
var
  pSD: PSECURITY_DESCRIPTOR;
  iSize: Cardinal;

  return: longbool;
begin
  iSize := 0;
  pSD := nil;


  return :=
  {$IFDEF UNICODE}ConvertStringSecurityDescriptorToSecurityDescriptorW{$ELSE}
    ConvertStringSecurityDescriptorToSecurityDescriptorA
{$ENDIF}
    (TJwPChar(aSDString), // LPCTSTR StringSecurityDescriptor,
    //(Windows.PWChar(ASDString),
    SDDL_REVISION_1, // DWORD StringSDRevision,
    jwaWindows_PSecurity_Descriptor(pSD),
    // PSECURITY_DESCRIPTOR* SecurityDescriptor,
    @iSize       //  PULONG SecurityDescriptorSize
    );

  if not return then
  begin
    //ERROR_INVALID_PARAMETER   A parameter is not valid.
    //ERROR_UNKNOWN_REVISION   The SDDL revision level is not valid.
    //ERROR_NONE_MAPPED
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create(aSDString : TJwString)', ClassName, 'JwsclDescriptor.pas', 0, True,
      ['ConvertStringSecurityDescriptorToSecurityDescriptor']);
  end;


  if pSD <> nil then
  begin
    Self.Create(jwaWindows.PSECURITY_DESCRIPTOR(pSD));
    LocalFree(HLOCAL(pSD));
  end
  else
    Self.Create;
end;




procedure TJwSecurityDescriptor.InitializeSD(aSecurityDescriptor:
  jwaWindows.PSecurityDescriptor);

//we need a pointer to the SD because a self relative SD is a big memory block

var
  c: jwaWindows.TSecurityDescriptorControl;
  //[Hint] absoluteSD : jwaWindows.PSecurityDescriptor;

  rev:  Cardinal;
  aSid: PSID;

  isSacl, isDacl, default: BOOL;

  aSACL, aDACL: PACL;
  //[Hint] aOwner, aGroup : PSID;
  //[Hint] iabsoluteSD,
  //[Hint] itemp,iaSACL,iaDACL, iaOwner, iaGroup : Cardinal;

  b: boolean;
  //[Hint] d, u : String;
begin
  rev := aSecurityDescriptor.Revision;

  b := GetSecurityDescriptorControl(aSecurityDescriptor,
    c, rev);
  if not b then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create', ClassName, RsUNDescriptor, 0, True, ['GetSecurityDescriptorControl']);


  Self.Control := TJwEnumMap.ConvertSecurityControl(c);

  //RMControl := c; Warning: GetRMControl not implemented!!
  {if c and SE_SELF_RELATIVE <> SE_SELF_RELATIVE then
    exit;}

  aSid := nil;


  if not GetSecurityDescriptorOwner(aSecurityDescriptor, aSid, @default) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create', ClassName, RsUNDescriptor, 0, True,
       ['GetSecurityDescriptorOwner']);

  fOwnerInherited := default;

  //free old Owner if we are the owner
  //be careful not to set OwnOwner to true
  Owner := nil;
  OwnOwner := True;
  if aSid <> nil then
  begin
    fOwner := TJwSecurityId.Create(aSid);
    //do not use Owner := ... because that will result in a copy of TJwSecurityId
  end;

  aSid := nil;


  if not GetSecurityDescriptorGroup(aSecurityDescriptor, aSid, @default) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create', ClassName, RsUNDescriptor, 0, True,
      ['GetSecurityDescriptorOwner']);

  fPrimaryGroupInherited := default;

  //free old primary group if we are the owner
  //be careful not to set OwnPrimaryGroup to true
  PrimaryGroup := nil;

  OwnPrimaryGroup := True;
  if aSid <> nil then
  begin
    fPrimaryGroup := TJwSecurityId.Create(aSid);
    //do not use Owner := ... because that will result in a copy of TJwSecurityId
  end;
  aSid := nil;

  isDacl := True;
  default := True;
  aDACL := nil;
  if not GetSecurityDescriptorDacl(aSecurityDescriptor, isDacl,
    aDACL, default) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create', ClassName, RsUNDescriptor, 0, True,
      ['GetSecurityDescriptorDacl']);

  fDACLInherited := default;

  if isDACL then
  begin
    if aDACL = nil then
    begin
      if OwnDACL then
        fDACL.Free;
      fDACL := nil; //NULL DACL -> allow access to everyone
    end
    else
    begin
      if OwnDACL then
        fDACL.Free;
      fDACL := TJwDAccessControlList.Create(aDACL);
    end;
  end
  else
  begin
    if OwnDACL then
      fDACL.Free;
    fDACL := nil;
  end;


  isSacl := True;
  default := True;
  aSACL := nil;
  if not GetSecurityDescriptorSacl(aSecurityDescriptor, isSacl,
    aSACL, default) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'Create', ClassName, RsUNDescriptor, 0, True,
      ['GetSecurityDescriptorDacl']);

  fAuditInherited := default;

  if isSACL then
  begin
    //SACL cannot be owned by another class
    if aSACL = nil then
    begin
      fAuditACL.Clear;
    end
    else
    begin
      fAuditACL.Free;
      fAuditACL := TJwSAccessControlList.Create(aSACL);
    end;
  end
  else
  begin
    if Assigned(fAuditACL) then
      fAuditACL.Clear;
  end;

end;

procedure TJwSecurityDescriptor.Done;
begin
  if OwnDACL then
    fDACL.Free;
  fDACL := nil;

  //AuditACL is never nil
  fAuditACL.Free;
  fAuditACL := nil;

  if fOwnOwner and Assigned(fOwner) and (not fOwner.IsStandardSID) then
    //dont free a standard sid from USM_KnwonSID
    fOwner.Free;
  fOwner := nil;

  if fOwnPrimaryGroup and Assigned(fPrimaryGroup) and
    (not fPrimaryGroup.IsStandardSID) then
    //dont free a standard sid from USM_KnwonSID
    fPrimaryGroup.Free;
  fPrimaryGroup := nil;
end;


destructor TJwSecurityDescriptor.Destroy;
begin
  Done;
  inherited;
end;

procedure TJwSecurityDescriptor.Assign(aObject: TJwSecurityDescriptor);
begin
  if not Assigned(aObject) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'Assign', ClassName, RsUNDescriptor,
      0, False, ['aObject']);

  if fOwnOwner and Assigned(fOwner) then
    fOwner.Free;
  fOwner := nil;

  if fOwnPrimaryGroup and Assigned(fPrimaryGroup) then
    fPrimaryGroup.Free;
  fPrimaryGroup := nil;

  if Assigned(aObject.Owner) then
    fOwner := TJwSecurityId.Create(aObject.Owner);

  if Assigned(aObject.PrimaryGroup) then
    fPrimaryGroup := TJwSecurityId.Create(aObject.PrimaryGroup);

  OwnOwner := True;
  OwnPrimaryGroup := True;

  if (not Assigned(DACL)) then
    fDACL := TJwDAccessControlList.Create;
  //do not use DACL, because it copies the list

  //check for NULL DACL
  if Assigned(aObject.DACL) then
    DACL.Assign(aObject.DACL)  //copies the whole DACL
  else
    DACL := nil; //frees the DACL and sets it nil

  if not Assigned(AuditACL) then
    fAuditACL := TJwSAccessControlList.Create;

  AuditACL.Assign(aObject.AuditACL);

  OwnerInherited := aObject.OwnerInherited;
  PrimaryGroupInherited := aObject.PrimaryGroupInherited;
  DACLInherited  := aObject.DACLInherited;
  AuditInherited := aObject.AuditInherited;

end;


function TJwSecurityDescriptor.GetDACL: TJwDAccessControlList;
begin
  Result := fDACL;
end;

procedure TJwSecurityDescriptor.SetDACL(anACL: TJwDAccessControlList);
begin
  //if we have a nil fdACL class we must create one
  if not Assigned(fDACL) and
     Assigned(anACL) then
  begin
    if OwnDACL then
      fDACL := anACL
    else
    begin
      fDACL := TJwDAccessControlList.Create;
      fDACL.Assign(anACL);
    end;

    DACLPresent := True; //we have a DACL
  end

  else
  //if anACL list was provided we assign it to our own
  if Assigned(fDACL) and
     Assigned(anACL) then
    fDACL.Assign(anACL);

  //if there is no anACL list we free the current one
  //so we get a NULL DACL
  if not Assigned(anACL) then
  begin
    if OwnDACL then
      fDACL.Free;
    fDACL := nil;
  end;
end;

function TJwSecurityDescriptor.GetSACL: TJwSAccessControlList;
begin
  Result := fAuditACL;
end;

procedure TJwSecurityDescriptor.SetSACL(anACL: TJwSAccessControlList);
begin
  //if we have a nil fdACL class we must create one
  if Assigned(anACL) and not Assigned(fAuditACL) then
    fAuditACL := TJwSAccessControlList.Create;

  //if anACL list was provided we assign it to our
  if Assigned(fAuditACL) and Assigned(anACL) then
  begin
    fAuditACL.Assign(anACL);
  end;

  //if there is no anACL list we free the current one
  //so we get a NULL SACL
  if not Assigned(anACL) then
  begin
    fAuditACL.Free;
    fAuditACL := nil;
  end;
end;


function TJwSecurityDescriptor.Create_SD(bRelative: boolean {= true}):
PSecurityDescriptor;
var
  s: Cardinal;
begin
  //out s
  Result := Self.Create_SD(s, bRelative);
end;

function TJwSecurityDescriptor.Create_SD(out ipSDSize: Cardinal;
  bRelative: boolean {= true}): PSecurityDescriptor;
var //[Hint] aOwnerTrustee,
  //[Hint] aGroupTrustee : {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF};
  //[Hint] aPOwnerTrustee,
  //[Hint] aPGroupTrustee : {$IFDEF UNICODE}PTrusteeW{$ELSE}PTrusteeA{$ENDIF};

  //[Hint] aOwnerSID,
  //[Hint] aGroupSID : PSid;

  //[Hint] aAccessACL,
  //[Hint] aAuditACL     : {$IFDEF UNICODE}TExplicitAccessW{$ELSE}TExplicitAccessA{$ENDIF};
  //[Hint] aPAccessACL,
  //[Hint] aPAuditACL     : {$IFDEF UNICODE}PExplicitAccessW{$ELSE}PExplicitAccessA{$ENDIF};

  //[Hint] iSACL,
  //[Hint] iDACL,
  //[Hint] iNewSDSize,
  //[Hint] iError : Cardinal;

  pSD: jwaWindows.PSECURITY_DESCRIPTOR;

  aDACL, aSACL: PACL;

  Control2 : TJwSecurityDescriptorControlSet;

  //aAccessArray : TJwExplicitAccessArray;

  //[Hint] iOwner,
  //[Hint] iGroup: Cardinal;
  //tempCtrl : TJwSecurityDescriptorControlSet;
begin
  ipSDSize := 0;

  if bRelative then
  begin
    //[Hint] result := nil;
    pSD := Create_SD(False);
    //pSD^.Control := pSD^.Control or SE_SELF_RELATIVE; 


  (*  if not MakeAbsoluteSD(pSD,nil,ipSDSize,
             nil,iDACL,
             nil,iSACL,
             nil, iOwner,
             nil, iGroup) then
    begin
      RaiseLastOSError;
      //GetMem(result,ipSDSize);
    end;*)
    {*************************************
     Do not use BuildSecurityDescriptor!
     Testing was a mess and also never worked out.
    }
    if not MakeSelfRelativeSD(pSD, nil, ipSDSize) and
      (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
    begin
      GetMem(Result, ipSDSize);
      //result := PSecurityDescriptor(LocalAlloc(LMEM_FIXED or LMEM_ZEROINIT,sizeof(jwaWindows.TJwSecurityDescriptor)));
      if not MakeSelfRelativeSD(pSD, Result, ipSDSize) then
      begin
        FreeMem(Result);
        //LocalFree(HLOCAL(result));
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed, 'Create_SD;', ClassName,
          RsUNDescriptor, 0, True,
           ['MakeSelfRelativeSD']);
      end;
      result^.Control := result^.Control or SE_SELF_RELATIVE; 


    end
    else
    begin
      Free_SD(pSD);

       raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create_SD;', ClassName, RsUNDescriptor, 0, True,
        ['MakeSelfRelativeSD']);
    end;

    Free_SD(pSD);
  end
  else
  begin
    ipSDSize := sizeof(jwaWindows.TSecurityDescriptor);
//    result := PSecurityDescriptor(LocalAlloc(LMEM_FIXED or LMEM_ZEROINIT,sizeof(jwaWindows.TSecurityDescriptor)));
    GetMem(Result, sizeof(jwaWindows.TSecurityDescriptor));

    //Result.Control := RMControl; Warning: GetRMControl not implemented!! 
    {result <> nil}
    if not InitializeSecurityDescriptor(
      Result, SECURITY_DESCRIPTOR_REVISION) then
    begin
      FreeMem(Result);
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create_SD;', ClassName, RsUNDescriptor, 0, True,
         ['InitializeSecurityDescriptor']);
    end;


    if Assigned(Owner) and (Owner.SID <> nil) then
    begin
      if not SetSecurityDescriptorOwner(Result, Owner.CreateCopyOfSID,
        fOwnerInherited) then
      begin
        FreeMem(Result);
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'Create_SD;', ClassName, RsUNDescriptor, 0, True,
           ['SetSecurityDescriptorOwner']);
      end;
    end;


    if Assigned(PrimaryGroup) and (PrimaryGroup.SID <> nil) then
    begin
      if not SetSecurityDescriptorGroup(Result, PrimaryGroup.CreateCopyOfSID,
        fPrimaryGroupInherited) then
      begin
        FreeMem(Result);
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'Create_SD;', ClassName, RsUNDescriptor, 0, True,
          ['SetSecurityDescriptorGroup']);
      end;

    end;

    aDACL := nil;
    if Assigned(DACL) then
    begin
      aDACL := DACL.Create_PACL;
      if not SetSecurityDescriptorDacl(Result, True, aDACL, fDACLInherited) then
      begin
        {result <> nil
         aDACL <> nil}
        DACL.Free_PACL(aDACL);
        FreeMem(Result);
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'Create_SD;', ClassName, RsUNDescriptor, 0, True,
           ['SetSecurityDescriptorDacl']);
      end;


      //we create an absolute SD, so we must not free it!
      //DACL.Free_PACL(aACL);
    end
    else
    if not SetSecurityDescriptorDacl(Result, DACLPresent, nil, fDACLInherited) then
    begin
      FreeMem(Result);
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create_SD;', ClassName, RsUNDescriptor, 0, True,
        ['SetSecurityDescriptorDacl']);
    end;


    Control2 := TJwEnumMap.ConvertSecurityControl(result^.Control);
    {result <> nil
     aDACL <> nil or aDACL = nil
    }

    aSACL := nil;
    if Assigned(AuditACL) and (AuditACL.Count > 0) then
    begin
      aSACL := AuditACL.Create_PACL;
      if not SetSecurityDescriptorSacl(Result, True, aSACL, fAuditInherited) then
      begin
        {result <> nil
         aDACL <> nil or aDACL = nil
         aSACL <> nil
        }
        AuditACL.Free_PACL(aSACL);
        if aDACL <> nil then //also DACL <> nil
          DACL.Free_PACL(aDACL);

        FreeMem(Result);
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
          'Create_SD;', ClassName, RsUNDescriptor, 0, True,
          ['SetSecurityDescriptorDacl']);
      end;

      //we create an absolute SD, so we must not free it!
      //DACL.Free_PACL(aACL);

      {result <> nil
       aDACL <> nil
      }
    end
    else
    if not SetSecurityDescriptorSacl(Result, False, nil, fAuditInherited) then
    begin
        {result <> nil
         aDACL <> nil or aDACL = nil
         aSACL = nil
        }
      if aDACL <> nil then //also DACL <> nil
        DACL.Free_PACL(aDACL);

      FreeMem(Result);
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create_SD;', ClassName, RsUNDescriptor, 0, True,
        ['SetSecurityDescriptorSacl']);
    end;
  end;


  Result.Control := TJwEnumMap.ConvertSecurityControl(Control);
//  Result.Control := 0;

  Result.Control := Result.Control or SE_DACL_PRESENT;

  //DACL retrieved by default mechanism
  if fDACLInherited then
    Result.Control := Result.Control or SE_DACL_DEFAULTED
  else
    Result.Control := Result.Control and not SE_DACL_DEFAULTED;



  if Assigned(aSACL) then
  begin
    Result.Control := Result.Control or SE_SACL_PRESENT;

    //SACL retrieved by default mechanism
    if fAuditInherited then
      Result.Control := Result.Control or SE_SACL_DEFAULTED
    else
      Result.Control := Result.Control and not SE_SACL_DEFAULTED;
  end;


 // Control2 := TJwEnumMap.ConvertSecurityControl(result^.Control);
  if bRelative then
    Result.Control := Result.Control or SE_SELF_RELATIVE
  else
    Result.Control := Result.Control and not SE_SELF_RELATIVE;

 { if InheritanceDACLProtection = fProtectionDACLState then
    result.Control := Result.Control or SE_DACL_PROTECTED
  else
    result.Control := Result.Control and not SE_DACL_PROTECTED;

  result.Control := Result.Control or SE_DACL_AUTO_INHERITed;
  }
 // Control2 := TJwEnumMap.ConvertSecurityControl(result^.Control);
   


  {result <> nil
  aDACL <> nil or aDACL = nil
  aSACL <> nil or aSACL = nil
  }
end;

class procedure TJwSecurityDescriptor.Free_SD(var SD: PSecurityDescriptor);
begin
  if SD = nil then
    exit;

  if SD.Control and SE_SELF_RELATIVE = SE_SELF_RELATIVE then
  begin
    //LocalFree(HLOCAL(SD));
    FreeMem(SD);
  end
  else
  begin
    if SD.Owner <> nil then
      FreeMem(SD.Owner);

    if SD.Group <> nil then
      FreeMem(SD.Group);

    if SD.SACL <> nil then
      TJwSecurityAccessControlList.Free_PACL(SD.Sacl);

    if SD.Dacl <> nil then
      TJwSecurityAccessControlList.Free_PACL(SD.Dacl);

    FreeMem(SD);
    //LocalFree(HLOCAL(SD));
  end;

  //GlobalFree(HGLOBAL(SD));
  SD := nil;
end;

function TJwSecurityDescriptor.Create_SAEx2(bSDRelative: boolean = False): TSecurityAttributes;
begin
  result := Self.Create_SAEx(Self.InheritHandles, bSDRelative);
end;

function TJwSecurityDescriptor.Create_SAEx(bInheritHandle: boolean = False;
  bSDRelative: boolean = False): TSecurityAttributes;
begin
  Result.nLength := sizeof(TSecurityAttributes);
  Result.bInheritHandle := bInheritHandle;

  try
    Result.lpSecurityDescriptor := Create_SD(bSDRelative);
  except
    on E: Exception do
    begin
      Result.lpSecurityDescriptor := nil;
      raise;
    end;
  end;
end;

function TJwSecurityDescriptor.Create_SA2(bSDRelative: boolean = False): PSecurityAttributes;
begin
  result := Create_SA(Self.InheritHandles, bSDRelative);
end;

function TJwSecurityDescriptor.Create_SA(bInheritHandle: boolean = False;
  bSDRelative: boolean = False): PSecurityAttributes;
begin
  GetMem(Result, sizeof(TSecurityAttributes));
  //result := PSecurityAttributes(LocalAlloc(LPTR, sizeof(TSecurityAttributes)));
  if (Result = nil) then
    exit;


  Result^.nLength := sizeof(TSecurityAttributes);
  Result^.bInheritHandle := bInheritHandle;

  try
    Result^.lpSecurityDescriptor := Create_SD(bSDRelative);
  except
    on E: Exception do
    begin
      FreeMem(Result);
      //[Hint] result := nil;
      raise;
    end;
  end;

end;

class procedure TJwSecurityDescriptor.Free_SAEx(var SA: TSecurityAttributes);
begin
  if SA.nLength <> sizeof(TSecurityAttributes) then
    raise EJwsclSecurityException.CreateFmtEx(
      RsSecurityDescriptorInvalidAttributesSize, 'Free_SA', ClassName,
      RsUNDescriptor, 0, False, []);

  try
    Free_SD(PSecurityDescriptor(SA.lpSecurityDescriptor));
  finally
    SA.nLength := 0;
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := False;
  end;

end;

class procedure TJwSecurityDescriptor.Free_SA(var SA: PSecurityAttributes);
begin
  if (SA = nil) then
    exit;

  if SA.nLength <> sizeof(TSecurityAttributes) then
    raise EJwsclSecurityException.CreateFmtEx(
      RsSecurityDescriptorInvalidAttributesSize, 'Free_SA', ClassName,
      RsUNDescriptor, 0, False, []);

  try
    Free_SD(PSecurityDescriptor(SA.lpSecurityDescriptor));
  finally
    SA.lpSecurityDescriptor := nil;
  end;

  FreeMem(SA);
  SA := nil;
end;



procedure TJwSecurityDescriptor.SetOwner(const Value: TJwSecurityId);
begin
  if Value = fOwner then
    exit;

  if OwnOwner and Assigned(fOwner) and (not fOwner.IsStandardSID) then
    //dont free a standard sid from JwsclKnownSid
    fOwner.Free;

  if OwnOwner and Assigned(Value) then
    //only create copy if we are the owner and value is not nil
    fOwner := TJwSecurityId.Create(Value)
  else
    fOwner := Value;
end;

procedure TJwSecurityDescriptor.SetPrimaryGroup(aGroup: TJwSecurityId);
begin
  if aGroup = fPrimaryGroup then
    exit;
  if OwnPrimaryGroup and Assigned(fPrimaryGroup) and
    (not fPrimaryGroup.IsStandardSID) then
    //dont free a standard sid from USM_KnwonSID
    fPrimaryGroup.Free;

  if OwnPrimaryGroup and Assigned(aGroup) then
    //only create copy if we are the owner and value is not nil
    fPrimaryGroup := TJwSecurityId.Create(aGroup)
  else
    fPrimaryGroup := aGroup;
end;

function TJwSecurityDescriptor.GetSecurityDescriptorString(
  SIFlags: TJwSecurityInformationFlagSet): TJwString;
begin
  result := GetSecurityDescriptorString(
      TJwEnumMap.ConvertSecurityInformation(SIFlags));
end;

function TJwSecurityDescriptor.GetSecurityDescriptorString(
  SIFlags: TSecurityInformation): TJwString;
var
  pSD: PSecurityDescriptor;
  pStrSD: TJwPChar;
  iStrSD: Cardinal;
begin
  pSD := Create_SD(False);

  if SIFlags = 0 then
    SIFlags := ALL_SECURITY_INFORMATION;

  if (SIFlags and LABEL_SECURITY_INFORMATION = LABEL_SECURITY_INFORMATION) and
    not TJwWindowsVersion.IsWindowsVista(True) then
    SIFlags := SIFlags and not LABEL_SECURITY_INFORMATION;
  //remove the flag if vista is not present


  if not IsValidSecurityDescriptor(pSD) then
    raise EJwsclInvalidSecurityDescriptor.CreateFmtEx(
      RsSecurityDescriptorInvalid,
      'StringSid', ClassName, RsUNDescriptor, 0, True, []);


  if not
   {$IFDEF UNICODE}ConvertSecurityDescriptorToStringSecurityDescriptorW{$ELSE}
    ConvertSecurityDescriptorToStringSecurityDescriptorA
{$ENDIF}
    (jwaWindows_PSecurity_Descriptor(pSD),
    //PSECURITY_DESCRIPTOR SecurityDescriptor,
    SDDL_REVISION_1,     //DWORD RequestedStringSDRevision
    SIFlags,        //SECURITY_INFORMATION SecurityInformation,
    pStrSD,        //LPTSTR* StringSecurityDescriptor,
    @iStrSD        //PULONG StringSecurityDescriptorLen
    ) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed,
      'StringSid', ClassName, RsUNDescriptor, 0, True,
       ['ConvertSecurityDescriptorToStringSecurityDescriptor']);

  SetLength(Result, iStrSD);
  Result := pStrSD;

  LocalFree(HLOCAL(pStrSD));

  Free_SD(pSD);
end;

constructor TJwSecurityDescriptor.Create(const Stream: TStream);
begin
  Self.Create;
  LoadFromStream(Stream);
end;


class function TJwSecurityDescriptor.hashCode(val: Pointer;
  size: integer): int64;
var
  i: integer;
  res: extended;
  //[Hint] x: Integer;

  function RoundEx(x: extended): integer;
  begin
    Result := Trunc(x) + Trunc(Frac(x) * 2);
  end;

var
  C: integer;
begin
  res := 0;
  C := integer(val);


  for i := 1 to size do
  begin
    res := res + byte(Pointer(C)^) * Power(1, size - i);
    Inc(c);
  end;

  try
    Result := RoundEx(res);
  except
    Result := 0;
  end;
end;

procedure TJwSecurityDescriptor.SaveToStream(const Stream: TStream);
var
  SD: PSecurityDescriptor;
  iHash: int64;
  iSDSize: Cardinal;

  isHashed: byte;
begin
  SD := Create_SD(iSDSize, True);
  try
    Stream.Write(SD_MAGIC_HEADER, SD_MAGIC_LENGTH);
    Stream.Write(iSDSize, sizeof(iSDSize));

    iHash := 0;
    if Assigned(OnHashCodeMethod) then
    begin
      try
        iHash := OnHashCodeMethod(Pointer(SD), iSDSize);
      except
        iHash := 0;
      end;

      //write hash value
      Stream.Write(iHash, sizeof(iHash));
    end
    else
      Stream.Write(iHash, sizeof(iHash));

    if Assigned(OnHashCodeMethod) then
      isHashed := high(isHashed)
    else
      isHashed := low(isHashed);
    Stream.Write(isHashed, sizeof(byte));

    if (iSDSize > 0) and (SD <> nil) then
      Stream.Write(SD^, iSDSize);
  finally
    Free_SD(SD);
  end;
end;



procedure TJwSecurityDescriptor.LoadFromStream(const Stream: TStream);
var
  SD:  TJwSecurityDescriptor;
  pSD: PSecurityDescriptor;
  magic: array[0..SD_MAGIC_LENGTH - 1] of AnsiChar;
  iRHash, iCHash: int64;
  iSDSize: Cardinal;
  byteHashed: byte;
  isHashed: boolean;
begin
  //[Hint] pSD := nil;
  SD := nil;

  magic := '';
  Stream.Read(magic, SD_MAGIC_LENGTH);

  if magic <> SD_MAGIC_HEADER then
    raise EJwsclStreamInvalidMagicException.CreateFmtEx(
      RsSecurityDescriptorInvalidHeader,
      'LoadFromStream', ClassName, RsUNDescriptor,
      0, False,
      [magic, Stream.Position]);


  iSDSize := 0;
  Stream.Read(iSDSize, sizeof(iSDSize));

  Stream.Read(iRHash, sizeof(iRHash));
  Stream.Read(byteHashed, sizeof(byteHashed));

  //isHashed = true, if isHashed = 255 - all other values : no hash
  isHashed := byteHashed = high(byte);

  if (Stream.Size - Stream.Position < iSDSize) then
    raise EJwsclStreamSizeException.CreateFmtEx(
      RsSecurityDescriptorTooSmallStreamSize,
      'LoadFromStream', ClassName, RsUNDescriptor,
      0, False,
      [iSDSize, Stream.Size - Stream.Position]);


  if iSDSize > 0 then
  begin
    GetMem(pSD, iSDSize);
    try
      Stream.Read(pSD^, iSDSize);

      iCHash := 0;
      if Assigned(OnHashCodeMethod) then
      begin
        try
          iCHash := OnHashCodeMethod(Pointer(pSD), iSDSize);
        except
          iCHash := 0;
        end;
      end;


      if isHashed and Assigned(OnHashCodeMethod) and (iCHash <> iRHash) then
      begin
        //the deallocation is done in the finally branch!! 
        //SD.Free;  do not uncomment!
        //FreeMem(pSD); do not uncomment!
        // SD = nil
        raise EJwsclStreamHashException.CreateFmtEx(
          RsSecurityDescriptorUnequalHash,
          'LoadFromStream', ClassName, RsUNDescriptor,
          0, False, [iRHash, iCHash]);
      end;

      if not IsValidSecurityDescriptor(pSD) then
        raise EJwsclInvalidSecurityDescriptor.CreateFmtEx(
          RsSecurityDescriptorInvalidStreamSD,
          'LoadFromStream', ClassName, RsUNDescriptor,
          0, False, []);

      SD := TJwSecurityDescriptor.Create(pSD);

      Self.Assign(SD);
    finally
      SD.Free;
      FreeMem(pSD);
    end;
  end
  else
  begin
    SD := TJwSecurityDescriptor.Create;
    try
      Self.Assign(SD);
    finally
      SD.Free;
    end;
  end;

end;

function TJwSecurityDescriptor.GetText: TJwString;
begin
  result := GetTextMap(nil);
end;

function TJwSecurityDescriptor.GetTextMap(const Mapping: TJwSecurityGenericMappingClass = nil): TJwString;
var sOwner,
    sGroup,
    sDACL,
    sSACL : TJwString;
const s = 'Owner: %0:s\r\nGroup: %1:s\r\nDACL: %2:s\r\nSACL: %3:s';
begin
  if Assigned(fOwner) then
    sOwner := fOwner.GetText(true);
  if Assigned(fPrimaryGroup) then
    sGroup := fPrimaryGroup.GetText(true);
  if Assigned(fDACL) then
    sDACL := fDACL.GetTextMap(Mapping);
  if Assigned(fAuditACL) then
    sSACL := fAuditACL.GetTextMap(Mapping);

  result := JwFormatString(s,[sOwner,sGroup,sDACL,sSACL]);
end;

function TJwSecurityDescriptor.GetRMControl:
  jwaWindows.TSecurityDescriptorControl;
begin
  result := 0;
  //TODO: Control is not resource manager (RM) Control !!
  //Result := TJwEnumMap.ConvertSecurityControl(fControl);
end;

procedure TJwSecurityDescriptor.SetControl(
  aControl: TJwSecurityDescriptorControlSet);
begin
  fControl := aControl;
end;

procedure TJwSecurityDescriptor.SetRMControl(
  aRMControl: jwaWindows.TSecurityDescriptorControl);
begin
  //TODO: Control is not resource manager (RM) Control !!
  //fControl := TJwEnumMap.ConvertSecurityControl(aRMControl);
end;

function TJwSecurityDescriptor.IsEqual(const SD: TJwSecurityDescriptor;
  const Flags: TJwSecurityInformationFlagSet): boolean;
begin
  Result := False;
  if not Assigned(SD) then
    exit;

  if Control <> SD.Control then
    exit;

  if (siOwnerSecurityInformation in Flags) then
  begin
    Result := (Assigned(Self.Owner) and Assigned(SD.Owner) and
      Self.Owner.EqualSid(SD.Owner));
    if not Result then
      exit;
  end;

  if (siGroupSecurityInformation in Flags) then
  begin
    Result := (Assigned(Self.PrimaryGroup) and
      Assigned(SD.PrimaryGroup) and
      Self.PrimaryGroup.EqualSid(SD.PrimaryGroup));
    if not Result then
      exit;
  end;

  if (siDaclSecurityInformation in Flags) then
  begin
    Result := (Assigned(Self.DACL) and Assigned(SD.DACL) and
      Self.DACL.IsEqual(SD.DACL));
    if not Result then
      exit;
  end;

  if (siSaclSecurityInformation in Flags) then
  begin
    Result := (Assigned(Self.SACL) and Assigned(SD.SACL) and
      Self.SACL.IsEqual(SD.SACL));
    if not Result then
      exit;
  end;


  Result := True;
end;

function TJwSecurityDescriptor.GetPrivateObjectSecurity(const SecurityInformation
      : TJwSecurityInformationFlagSet) : TJwSecurityDescriptor;
var ObjectSD,
    ResultSD : PSecurityDescriptor;

    ReturnLen,
    SDSize : Cardinal;



begin
  result := nil;
  
  ObjectSD := Self.Create_SD(SDSize, true);

  ReturnLen := 0;
  JwaWindows.GetPrivateObjectSecurity(ObjectSD,
    TJwEnumMap.ConvertSecurityInformation(SecurityInformation),nil, 0, ReturnLen);

  ResultSD := PSecurityDescriptor(LocalAlloc(LPTR, ReturnLen));
  SDSize := ReturnLen;

  try
    if not JwaWindows.GetPrivateObjectSecurity(
      ObjectSD,//__in          PSECURITY_DESCRIPTOR ObjectDescriptor,
      TJwEnumMap.ConvertSecurityInformation(SecurityInformation),//__in          SECURITY_INFORMATION SecurityInformation,
      ResultSD,//__out_opt     PSECURITY_DESCRIPTOR ResultantDescriptor,
      SDSize,//__in          DWORD DescriptorLength,
      ReturnLen//__out         PDWORD ReturnLength
      ) then
       raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'GetPrivateObjectSecurity',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNDescriptor,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'GetPrivateObjectSecurity',                   //sWinCall
        ['GetPrivateObjectSecurity']);                                  //const Args: array of const


     result := TJwSecurityDescriptor.Create(ResultSD);

  finally
    Self.Free_SD(ObjectSD);
    LocalFree(HLOCAL(ResultSD));
  end;
end;

procedure TJwSecurityDescriptor.SetPrivateObjectSecurity(
  const SecurityInformation : TJwSecurityInformationFlagSet;
  const ModificationDescriptor : TJwSecurityDescriptor;
  const AutoInheritFlags : TJwInheritFlagSet;
  const GenericMap: TJwSecurityGenericMappingClass; const Token: TObject = nil);
var ModSD,
    PtrSd,
    ObjectSD : PSecurityDescriptor;
    TokenInstance : TJwSecurityToken;
    TokenHandle : TJwTokenHandle;
    SD : TJwSecurityDescriptor;
    SDSize : Cardinal;
   

    PrivScope : IJwPrivilegeScope;
    aMapping: JwaWindows.TGenericMapping;

begin
  ModSD := nil;
  ObjectSD := nil;
  TokenInstance := nil;

  PrivScope := nil;

  if not Assigned(ModificationDescriptor) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'SetPrivateObjectSecurity',
      ClassName, RsUNDescriptor, 0, False, ['ModificationDescriptor']);

  if Assigned(Token) then
  begin
    if not (Token is TJwSecurityToken) then
      raise EJwsclInvalidParameterException.CreateFmtEx(
        RsTokenInvalidClass,
        'SetPrivateObjectSecurity',
        ClassName, RsUNDescriptor, 0, False, [Token.ClassName]);
    TokenHandle := (Token as TJwSecurityToken).TokenHandle;
  end
  else
  begin
    TokenInstance := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
    TokenInstance.ConvertToImpersonatedToken(SecurityImpersonation,MAXIMUM_ALLOWED);
    TokenHandle := TokenInstance.TokenHandle;


    //Enable security priv if SACL must be changed
    if (siSaclSecurityInformation in SecurityInformation) and
      not (ifAvoidPrivilegeCheck in AutoInheritFlags) then
//      (AutoInheritFlags and SEF_AVOID_PRIVILEGE_CHECK <> SEF_AVOID_PRIVILEGE_CHECK)
    begin
      try
        PrivScope := JwGetPrivilegeScope([SE_SECURITY_NAME]);
      except
        on E : EJwsclPrivilegeException do
          raise EJwsclSACLAccessDenied.CreateFmtEx(
            RsPrivateSaclAccessDenied,
            'SetPrivateObjectSecurity',
            ClassName, RsUNDescriptor, 0, False, []);
      end;
    end;
  end;

  ModSD := ModificationDescriptor.Create_SD(true);

  PtrSd := Self.Create_SD(SDSize, true);

  ObjectSD := PSECURITY_DESCRIPTOR(LocalAlloc(LPTR, SDSize));
  CopyMemory(ObjectSD, PtrSd, SDSize);
  Self.Free_SD(PtrSd);

(*
{$IFDEF FPC}
   if (GenericMap.ClassType <> TJwSecurityGenericMapping.ClassType) then
{$ELSE}
   if (not GenericMap.ClassNameIs(TJwSecurityGenericMapping.ClassName)) then
{$ENDIF FPC}*)
   if not Assigned(GenericMap) then
     aMapping := TGenericMapping(TJwSecurityGenericMapping.GetMapping())
   else
     aMapping := TGenericMapping(GenericMap.GetMapping());
(*   else
     ZeroMemory(@aMapping, sizeof(aMapping));
  *)
  try
    if not SetPrivateObjectSecurityEx(
      TJwEnumMap.ConvertSecurityInformation(SecurityInformation),
        //__in          SECURITY_INFORMATION SecurityInformation,
      ModSD,//__in          PSECURITY_DESCRIPTOR ModificationDescriptor,
      ObjectSD,//__in_out      PSECURITY_DESCRIPTOR* ObjectsSecurityDescriptor,
      //TJwEnumMap.ConvertInheritFlags(AutoInheritFlags),
      SEF_DACL_AUTO_INHERIT or SEF_AVOID_OWNER_CHECK or
      SEF_AVOID_PRIVILEGE_CHECK or SEF_DEFAULT_DESCRIPTOR_FOR_OBJECT or
      SEF_DEFAULT_OWNER_FROM_PARENT or SEF_DEFAULT_GROUP_FROM_PARENT or
      $1000, //SEF_AVOID_OWNER_RESTRICTION,

      
      aMapping,//__in          PGENERIC_MAPPING GenericMapping,
      TokenHandle//__in_opt      HANDLE Token
      ) then
       raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'SetPrivateObjectSecurity',                                //sSourceProc
        ClassName,                                //sSourceClass
        RsUNDescriptor,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'SetPrivateObjectSecurity',                   //sWinCall
        ['SetPrivateObjectSecurity']);                                  //const Args: array of const


      SD := TJwSecurityDescriptor.Create(ObjectSD);
      try
        Self.Assign(SD);
      finally
        FreeAndNil(SD);
      end;

  finally
    LocalFree(HLOCAL(ObjectSD));
    TJwSecurityDescriptor.Free_SD(ModSD);
    FreeAndNil(TokenInstance);
  end;
end;

function TJwSecurityDescriptor.GetProtectedState(Index: Integer): TJwACLProtection;
begin
  case Index of
    0 {DACL}:
      begin
        result := fProtectionDACLState;
        if sdcDaclProtected in Control then
          result := aclpProtected;
      end;
    1 {SACL}:
      begin
        result := fProtectionSACLState;
        if sdcSaclProtected in Control then
          result := aclpProtected;
      end;
    else
      raise EJwsclInvalidIndex.CreateFmtEx(
            RsInvalidIndex,
            'GetProtectedState',
            ClassName, RsUNDescriptor, 0, False, [Index]);
  end;
end;

procedure TJwSecurityDescriptor.SetProtectedState(Index: Integer;
  const Protect: TJwACLProtection);
begin
  case Index of
    0 {DACL}:
       begin
         fProtectionDACLState := Protect;
         if Protect = aclpProtected then
           Include(fControl, sdcDaclProtected)
         else
           Exclude(fControl, sdcDaclProtected);
       end;
    1 {SACL}:
       if Protect = aclpProtected then
         Include(fControl, sdcSaclProtected)
       else
         Exclude(fControl, sdcSaclProtected);
  end;
end;



{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}



end.
{$ENDIF SL_OMIT_SECTIONS}(*
    aPOwnerTrustee := nil;
    aPGroupTrustee := nil;
    aPAccessACL    := nil;
    aPAuditACL     := nil;
    aOwnerSID      := nil;
    aGroupSID      := nil;
    iDACL          := 0;
    iSACL          := 0;      


    //create owner trustee
    if Assigned(Owner) then
    begin
      //indeed GetExplicitEntriesFromAcl also returns such a trustee
      FillChar(aOwnerTrustee, sizeof(aOwnerTrustee), 0);

      aOwnerTrustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
      aOwnerTrustee.TrusteeForm := TRUSTEE_IS_SID;
      aOwnerTrustee.TrusteeType := TRUSTEE_IS_UNKNOWN;
      aOwnerTrustee.ptstrName   := PWideChar(Owner.SID);

      aPOwnerTrustee := @aOwnerTrustee;
    end;

    if Assigned(PrimaryGroup) then
    begin
      FillChar(aGroupTrustee, sizeof(aGroupTrustee), 0);

      aGroupTrustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
      aGroupTrustee.TrusteeForm := TRUSTEE_IS_SID;
      aGroupTrustee.TrusteeType := TRUSTEE_IS_GROUP;
      aGroupTrustee.ptstrName   := TJwPChar(PrimaryGroup.SID);

      aPGroupTrustee := @aGroupTrustee;
    end;

    if Assigned(DACL) then
    begin
      aAccessArray := DACL.GetExplicitAccessArray;

      iDACL := DACL.Count;
      if iDACL > 0 then
        aPAccessACL := {$IFDEF UNICODE}PExplicitAccessW{$ELSE}PExplicitAccessA{$ENDIF}(aAccessArray);
    end;

    {
    }
    if Assigned(AuditACL) and (AuditACL.Count > 0) then
    begin
      aAccessArray := AuditACL.GetExplicitAccessArray;

      iSACL := Length(aAccessArray);

      if iSACL > 0 then
        aPAuditACL := {$IFDEF UNICODE}PExplicitAccessW{$ELSE}PExplicitAccessA{$ENDIF}(aAccessArray);

   
    end;
    

    iNewSDSize := 0;
    pSD := nil;
    
   (* iError :=
    {$IFDEF UNICODE}BuildSecurityDescriptorW{$ELSE}BuildSecurityDescriptorA{$ENDIF}
     (aPOwnerTrustee,aPGroupTrustee,
      iDACL,aPAccessACL,
      iSACL,aPAuditACL,
      nil,
      iNewSDSize, pSD);


 
    if iError <> ERROR_SUCCESS then
      raise EJwsclWinCallFailedException.CreateFmtEx('Call to BuildSecurityDescriptor failed. NTError: %d', 'Create_SD',ClassName,RsUNDescriptor, 0,true,[iError]);

    *)//    result := jwaWindows.PSECURITY_DESCRIPTOR(pSD);