{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains access control classes that are used by the units of JWSCL

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

The Original Code is JwsclAcl.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Unsupported structures :
* SYSTEM_ALARM_ACE
* SYSTEM_ALARM_CALLBACK_ACE
* SYSTEM_ALARM_CALLBACK_OBJECT_ACE
* SYSTEM_ALARM_OBJECT_ACE




}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAcl;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  SysUtils, Contnrs, Classes,
  jwaWindows, 
  JwsclResource, JwsclUtils,

  JwsclTypes, JwsclExceptions, JwsclMapping,
  JwsclVersion, JwsclConstants, JwsclSid,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityAccessControlEntry = class;
  TJwAuditAccessControlEntry    = class;
  TJwSystemMandatoryAccessControlEntry = class;

  TJwSecurityAccessControlEntryClass = class of TJwSecurityAccessControlEntry;

  {<B>TJwSecurityAccessControlList</B> provides methods for an access control list.
   Do not make instances of this class. Instead use

    TJwSAccessControlList for audit control lists
    TJwDAccessControlList for Discretionary control lists

  }
  TJwSecurityAccessControlList = class(TObjectList)
  protected
    mDoNoRaiseException: boolean;
    fRevision : Cardinal;
    function GetItem(idx: integer): TJwSecurityAccessControlEntry;
    procedure RemoveOwners;
  protected


    {<B>Create</B> creates an empty access control list.
     @param ownACEs receives whether the items shall be freed on destruction (True) or not. }
    constructor Create(OwnAceEntries: boolean); overload;

    {<B>Create</B> creates a new access control list from an existing access control list.
     All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

     raises
 EJwsclWinCallFailedException:  will be raised if the aAPCL is not a valid access control list 
      EJwsclWinCallFailedException: if an win api function failed 

     }
    constructor Create(AclPointerList: PACL); overload;

    {<B>Create</B> creates a new access control list from an explicit access array.

     raises
 EJwsclNILParameterException:  will be raised if parameter Accesses is nil 
      EJwsclWinCallFailedException: will be raised if the call to SetEntriesInAcl failed. 
     }
    constructor Create(const Accesses : TJwExplicitAccessArray); overload;

    function GetText: TJwString; virtual;
    procedure SetRevision(const Revision : Cardinal); virtual;


  public
    {<B>Create</B> creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

    {<B>Destroy</B> destroys the list and all it items.}
    destructor Destroy; override;

    {<B>GetTextMap</B> returns the ACL content in a new string. Additionally
     the access rights are mapped into string using a defined mapping.}
    function GetTextMap(const Mapping: TJwSecurityGenericMappingClass = nil):
      TJwString; virtual;

    {<B>Create_PACL</B> creates a new access control list for using in winapi functions.
     The created memory block must be freed by Free_PACL.
     The list order in the new ACL will be the same like the list in Items .

     raises
 EJwsclNotEnoughMemory:  will be raised if the new memory block for ACL could not be allocated. 
      EJwsclUnsupportedACE: will be raised if a given ACE in the list is not supported by this function. 
      EJwsclFailedAddACE: will be raised if a AddXXX winapi call for a given ACE in the list failed.  
    }
    function Create_PACL: PACL;
    
    {Deprecated. Do not use.}
    function Create_PACL_Deprecated: PACL;

    {<B>Free_PACL</B> frees an access control list created by Create_PACL.

     @param anACL receives and retrieves the access control list. It will be set to nil afterwards. If anACL is nil nothing happens. 
     }
    class procedure Free_PACL(var AclPointerList: PACL);

    {<B>Assign</B> clears all ACEs in the instance and adds new instances of then ACEs
     from AclInstance. All ACE SIDs are copied.
     If an exception is raised the old ACEs are removed but the newly added ACEs are preserved.
     @param AclInstance receives the list. 
     raises
 EJwsclNILParameterException:  will be raised if AclInstance is nil 
    }
    procedure Assign(AclInstance: TJwSecurityAccessControlList); virtual;

    {<B>Clear</B> removes all ACEs from the list and frees them if ownObject is True.
    }
    procedure Clear; override;

    {<B>GetExplicitAccessArray</B> creates an array of explicit access structure that represents
     the ACEs.

     raises
 EJwsclWinCallFailedException:  will be raised if the call to GetExplicitEntriesFromAcl failed.
    }

    function GetExplicitAccessArray: TJwExplicitAccessArray; virtual;

    {<B>AddACEs</B> adds ACEs from another list to this one.
     The ACEs will be added using Add  so the ACL order will be correct.

     @param AclInstance contains an access control list to be duplicated 
     @param KeepOriginalOrder defines whether the order of elements in AclInstance
      will be kept. In this case the list will be added at the top of the target list. This may
      disturb the canonical order of elements.
      If set to false, the elements are added into the target list using canonical order. 

     raises
 EJwsclNILParameterException:  will be raised if aObject is nil 
      EJwsclInvalidACEException: will be raised if the classtype of this list instance is not the same of aObject 
     }
    procedure AddACEs(AclInstance: TJwSecurityAccessControlList; KeepOriginalOrder : Boolean = true); virtual;

    {<B>Add</B> adds an ACE instance to into the list. The ACE property ListOwner will be set to this list.

     Where the new item is inserted depends on its type:
      
      1.  If the ACE is a direct allow ACE it is added at the bottom of the list
      2.  If the ACE is a inherited allow ACE it is added after the last deny ACE or at the top of list if no deny ACE exists
      3.  If the ACE is a deny ACE it is added at the top of list
     


     The following list shows a full access control list
      
      1.  deny ACE
      2.  allow ACE  3 (direct)
      3.  deny ACE   1 (inherited)
      4.  allow ACE  2 (inherited)
      The numbers shows the order the ACEs were added.

      Audit items are always added to the end of list and cannot be added to discretionary lists.

      Do not call this function without a pointer to the ACE :
        ACLList.Add(TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,False));
      If the Add method raises an Exception, the TJwAuditAccessControlEntry instance is not freed.
      Instead use a pointer
      <code lang="Delphi">
        try
          anACE := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,False)
          try
            ACLList.Add(anACE);
          except
            anACE.Free;
          end;
        except

        end;
     </code>

    @return <B>Add</B> returns the index in the list where the ACE was added
    raises
     EJwsclNILParameterException:  will be raised if aObject is nil
     EJwsclInvalidACEException: will be raised if

               #  TJwDiscretionaryAccessControlEntry was given but the list is not an instance of TJwDAccessControlList
               #  TJwAuditAccessControlEntry was given but the list is not an instance of TJwSAccessControlList

     EJwsclDuplicateListEntryException: will be raised if the given ACE is already in list

    }
    function Add(AccessEntry: TJwSecurityAccessControlEntry): integer;

    {<B>GetEffectiveRights</B> computes the effective access rights of a user and the ACL.
     A better and more secure way to check access rights is to use
     AccessCheck with MAXIMUM_ALLOWED desired access rights.

     Known bug in internal Windows call:
       This function does/may not work with inherited access control elements and
       also can fail on some combination with deny entries!

     @param User defines a trustee that is used to get effective access rights 
     @return <B>GetEffectiveRights</B> returns the rights which are granted to User 
     raises
 EJwsclWinCallFailedException:  will be raised if a call to
       GetEffectiveRightsFromAcl failed 
     }
    function GetEffectiveRights(const User :
       {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF}) : TJwAccessMask; overload;


    {<B>GetEffectiveRights</B> computes the effective access rights of a user and the ACL.
     A better and more secure way to check access rights is to use
     AccessCheck with MAXIMUM_ALLOWED desired access rights.

     Known bug in internal Windows call:
       This function does/may not work with inherited access control elements and
       also can fail on some combination with deny entries!

     @param User defines a SecurityID that is used to get effective access rights.
       This parameter can be nil to use the current user of thread or process instead. 
     @return <B>GetEffectiveRights</B> returns the rights which are granted to User 
     raises
 EJwsclWinCallFailedException:  will be raised if a call to
       GetEffectiveRightsFromAcl failed 
     }
    function GetEffectiveRights(const User : TJwSecurityId) : TJwAccessMask; overload;

    {<B>First</B> returns the first ACE from the list.
     @return The first ACE or if the list is empty <B>First</B> returns nil. 
     }
    function First: TJwSecurityAccessControlEntry;

    {<B>IndexOf</B> returns the list index of a ACE from the list.
     @return Index of list. If the ACE is not in list the return value is -1. 
     }
    function IndexOf(AccessEntry: TJwSecurityAccessControlEntry): integer;

    {<B>Insert</B> inserts a ACE into the list.
     @param Index List index where the ACE shall be inserted before.
            If the Index is not between 0 and Count-1 an exception will be raised. 
     }
    procedure Insert(Index: integer; AccessEntry: TJwSecurityAccessControlEntry);

    {<B>Last</B> returns the last ACE of the list.
     If the list is empty the return value is nil.
     }
    function Last: TJwSecurityAccessControlEntry;

    {<B>FindSID</B> searches for a SID in a access control list.
     @param SID defines the SID to be searched for.
     @param iPos defines the start position in the list. If -1 the search begins from
      the beginning. The search always starts with the next ACE given by index iPos.
     @return Returns the index of the found ACE with the given SID. If the index
       is out of range, invalid or the SID could not be found the return value is -1 .
     }
    function FindSID(const SidInstance: TJwSecurityId;
      const StartIndex: integer = -1): integer;

    {<B>FindEqualACE</B> seeks for a ACE in the ACList.

     @param ACE defines the ACE to be searched for in the list.
     @param EqualACETypeSet defines the criterias that are used to compare the ACE.
       The following criterias are available and can be combined in a set.
          
           # eactSameSid    The SID is used to compare (EqualSID) and must be equal
           # eactSameFlags The Flags are compared and must be equal 
           # eactSameAccessMask The AccessMasks are compared and must be equal 
           # eactSameType The ACE type (deny, allow) are compared and must be equal
           
     @param iPos defines the start position for the search in the ACL list starting from zero (0).

     @return Returns the position of the found ACE in the list starting from 0.
             If the value iPos is out of bounds, or the ACE could not be found the return value is -1
     }
    function FindEqualACE(const AccessEntry: TJwSecurityAccessControlEntry;
      EqualAceTypeSet: TJwEqualAceTypeSet; const StartIndex: integer = -1;
      const Inclusion : TJwInclusionFlags = [ifInherited, ifExplicit, ifContainer, ifLeaf];
      const Exclusion : TJwExclusionFlags = [];
      const Reverse : Boolean = false): integer;

    {<B>ConvertInheritedToExplicit</B> removes the inheritance flag from all ACEs.
      This is useful if a DACL with inherited ACEs must be converted into a DACL with
       only explicit ACEs. This stops the inheritance flow.
      }
    procedure ConvertInheritedToExplicit;

    {<B>RemoveExplicits</B> removes all explicit entries from the list using Remove. See Remove 
     for information about how the entries are removed.}
    procedure RemoveExplicits;

    {<B>RemoveInherited</B> removes all inherited entries from the list using Remove. See Remove 
     for information about how the entries are removed.}
    procedure RemoveInherited;

    {<B>IsCanonical</B> checks whether the ACL is in canonical order.

     The following list shows a access control list in canonical order:
      
      1.  deny ACE   (direct)
      2.  allow ACE  (direct)
      3.  deny ACE   (inherited)
      4.  allow ACE  (inherited)
      
    }
    function IsCanonical: boolean;

    {<B>MergeElements</B> merges duplicate ACE elements.
     The ACE must have same SID, same type and same flags to be merged.
     The duplicates are removed from list.
    }
    procedure MergeElements;
    {<B>IsEqual</B> compares two ACL and returns true if they are equal.
     This method uses FindEqualACE to compare two access control entries.

     @param AccessControlListInstance defines the second ACL. Cannot be nil. 
     @param EqualAceTypeSet defines the ACE comparision done by FindEqualACE.
            By default all ACE members must be equal to return a positive result 
     @return Returns true if both ACL are equal; otherwise False. It also
          returns false if AccessControlListInstance is nil 
    }
    function IsEqual(const AccessControlListInstance: TJwSecurityAccessControlList;
                     const EqualAceTypeSet: TJwEqualAceTypeSet = JwAllEqualAceTypes): boolean;

    {<B>Remove</B> removes a ACE from the list. If the List owns the object
     the ACE will also be freed.
     Do not call the Free method of the object to be removed if property OwnObjects is True otherwise
      an exception will be raised.
     @param aObject contains the ACE to be removed 
     @return <B>Remove</B> returns the index of the ACE in the list before it was removed. If
               the ACE could not be found the return value is -1.  }
    function Remove(const AccessEntry: TJwSecurityAccessControlEntry): integer; overload;

    {<B>Remove</B> removes an object give by index from the list.
     The object will be freed automatically if OwnsObject is true.

    @return Index receives the zero based index of the object to be removed.
             Valid values are 0 to Count -1 .
    @return <B>Remove</B> returns the index of the ACE in the list before it was removed. If
               the ACE could not be found the return value is -1.
    }
    function Remove(const Index: integer): integer; overload;

    {<B>Delete</B> removes an object give by index from the list.
     The object will be freed automatically if OwnsObject is true.
     @return Index receives the zero based index of the object to be removed.
             Valid values are 0 to Count -1 .
     }
    procedure Delete(const Index: integer); reintroduce; virtual; 

    property Items[Index: integer]: TJwSecurityAccessControlEntry Read GetItem;
      default;

    {<B>Text</B> returns a humand readable text that contains information about this ACL.}
    property Text: TJwString Read GetText;

    {<B>Revision</B> gets or sets or gets the revision of the ACL.
     The version is used to set the ACL structure revision in Create_PACL.
     The revision version is always updated to the highest ACE revision version.

     Can be one of the revision levels:
     ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS

     Default value is ACL_REVISION.

     This property raises an exception EJwsclInvalidRevision on setting
      if the Revision is not in range of 1..4
     }
    property Revision : Cardinal read fRevision write SetRevision;
  end;

  {<B>TJwDAccessControlList</B> provides methods for an discretionary access control list.

   For more information on the methods see TJwSecurityAccessControlList.
  }
  TJwDAccessControlList = class(TJwSecurityAccessControlList)
  public
    {<B>Create</B> creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

    {<B>Create</B> creates an empty access control list.
     @param ownACEs receives whether the items shall be freed on destruction (True) or not. }
    constructor Create(ownACEs: boolean); overload;

    {<B>Create</B> creates a new list from an existing access control list.
     All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

     @param aAPCL receives the discretionary access control list to be copied 
     raises
 EJwsclWinCallFailedException:  will be raised if the aAPCL is not a valid access control list 
      EJwsclWinCallFailedException: if an win api function failed 
    }
    constructor Create(AclPointerList: PACL); overload;

    {<B>Create</B> creates a new access control list from an explicit access array.

     raises
 EJwsclNILParameterException:  will be raised if parameter Accesses is nil 
      EJwsclWinCallFailedException: will be raised if the call to SetEntriesInAcl failed. 
     }
    constructor Create(const Accesses : TJwExplicitAccessArray); overload;

    {<B>GetInheritance</B> creates a copy of the ACL that only contains inherited ACEs.
     The ACEs are copied and the list owns them.
     The caller must free the newly created DACL.
    }
    function GetInheritance: TJwDAccessControlList;

    {<B>GetExplicit</B> creates a copy of the ACL that only contains explicit ACEs.
     The ACEs are copied and the list owns them.
     The caller must free the newly created DACL.
    }
    function GetExplicit: TJwDAccessControlList;

    {<B>MakeCanonical</B> rearranges the entries of the list to make the list canonical.

     The following list shows a access control list in canonical order:
      
      1.  deny ACE   (direct)
      2.  allow ACE  (direct)
      3.  deny ACE   (inherited)
      4.  allow ACE  (inherited)
      
    }
    procedure MakeCanonical;
  end;

  {<B>TJwSAccessControlList</B> provides methods for an audit access control list.
   For more information on the methods see TJwSecurityAccessControlList .
  }
  TJwSAccessControlList = class(TJwSecurityAccessControlList)
  protected
    function GetItem(idx: integer): TJwAuditAccessControlEntry;

    {<B>GetMandatoryLabel</B> returns the first mandatory label in the SACL if any
     @return Returns the first mandatory label or nil if none exists 
    }
    function GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry; virtual;
    procedure SetMandatoryLabelEx(
      const NewLabel : TJwSystemMandatoryAccessControlEntry);  virtual;
  public
    {<B>Create</B> creates an empty access control list. The list items will not be owned.}
    constructor Create; overload;

     {<B>Create</B> creates an empty access control list.
      @param ownACEs receives whether the items shall be freed on destruction (True) or not. }
    constructor Create(OwnAceEntries: boolean); overload;

     {<B>Create</B> creates a new list from an existing access control list.
      All entries a copied and the list owns these objects (entry.ListOwner is set to this instance).

      @param aAPCL receives the discretionary access control list to be copied 
      raises
 EJwsclWinCallFailedException:  will be raised if the aAPCL is not a valid access control list 
       EJwsclWinCallFailedException: if an win api function failed 

      }
    constructor Create(AclPointerList: PACL); overload;

     {<B>GetExplicitAccessArray</B> creates an array of explicit access structure that represents
      the ACEs.

      raises
 EJwsclWinCallFailedException:  will be raised if the call to GetExplicitEntriesFromAcl failed.
     }
    function GetExplicitAccessArray: TJwExplicitAccessArray; override;

    {<B>SetMandatoryLabel</B> removes, adds or replaces a mandatory label.
     
     # If a label already exists in the SACL the new label will replace the old one. 
     # If no label exists the label will be added to the list. There is no order
       in a system acl so do not depend on it. 
     # If parameter NewLabel is nil the old label will be removed 
     

     Removing:
     The old label will be erased depending on how OwnObjects. If OwnObjects is
     true the old label instance will be freed otherwise it will just be removed from list.

     @param NewLabel defines the new label instance. It is copied or direclty added
      to the list depending on parameter CopyFlag 
     @param CopyFlag defines how the new label is treated.
      
       # cfCopyInstance creates a new copy of the instance and adds it to the list.
          This is the default behavior if property MandatoryLabel is used.
          You should set OwnObjects to true so the new instance will be released when
          the SACL is freed.
           
       # cfPointAtInstance simply adds the given instance to the list.
         If OwnObjects is true the instance will also be freed. 
        
     }
    procedure SetMandatoryLabel(
      const NewLabel : TJwSystemMandatoryAccessControlEntry;
      const CopyFlag: TJwCopyFlag);  virtual;

    {<B>HasMandatoryLabel</B> checks whether the SACL has a mandatory level.

     @return Returns true if a mandatory level exists otherwise false. }
    function HasMandatoryLabel : boolean; virtual;

    {<B>Items[Index</B> contains the audit ACEs.}
    property Items[Index: integer]: TJwAuditAccessControlEntry Read GetItem;
      default;

    {<B>MandatoryLabel</B> gets or sets the mandatory label of this SACL.
     If the mandatory label is retrieved it returns the mandatory label instance
     directly. Do not free it!
     If no label was found (HasMandatoryLabel = false) the return value is nil.

     The label can be changed whithout regarding access checks.
     Access checks are performed in SetSecurityInfo.
     Only the first label is changed. Any additional labels are ignored.
     The given mandatory label instance will be copied into a new instance
     and added automatically to the list. Do not use the parameter ListOwner
     of the Create constructor.
     Do not write this:
     <code lang="Delphi">
       audit.MandatoryLabel := TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelHigh);
       </code>
     Instead use a variable to prevent a memory leak:
     <code lang="Delphi">
     var ML : TJwSystemMandatoryAccessControlEntry;
       ML := TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelHigh);
       audit1.MandatoryLabel := ML;
       ML.Free;
       audit1.OwnsObject := true;
     </code>
     Also set OwnsObject to true so the copied instance will be freed automatically.

     You can also use SetMandatoryLabel to define how the instance will be treated.
     }
    property MandatoryLabel : TJwSystemMandatoryAccessControlEntry
        read GetMandatoryLabel write SetMandatoryLabelEx;
  end;


  {<B>TJwSecurityAccessControlEntry</B> contains data that describes how an object is accessed.
   The class provides methods to access these properties.

   To create an instance you can use the public constructors.
   However some constructors are protected and only visible in sub classes:
    
        #  TJwDiscretionaryAccessControlEntryAllow.Create(...)
        #  TJwDiscretionaryAccessControlEntryDeny.Create(...)
        #  TJwAuditAccessControlEntry.Create(...)
      

   The ACE can be added to a list (called ACL - access control list).

  }
  TJwSecurityAccessControlEntry = class(TObject)
  protected
    fListOwner:  TJwSecurityAccessControlList;
    fSID:        TJwSecurityId;
    fFlags:      TJwAceFlags;
    fAccessMask: TJwAccessMask;
    fownSID:     boolean;
    fIgnore:     boolean;
    fUserData : Pointer;
    fHeader : TAceHeader;
    fRevision : Cardinal;

    fObjectType,
    fInheritedObjectType : TGuid;

    procedure SetListOwner(ListOwner: TJwSecurityAccessControlList);
    procedure SetFlags(FlagSet: TJwAceFlags);
    procedure SetAccessMask(anAccessMask: TJwAccessMask);
    function GetAccessMask : TJwAccessMask; virtual;
    procedure SetSID(SidInstance: TJwSecurityId);
    procedure CheckReadOnly;
    function GetAceType: TJwAceType; virtual;
    procedure SetOwnSid(const OwnSid : Boolean); Virtual;
    function GetOwnSid : Boolean;
    function GetObjectFlags : Cardinal; virtual;

  public
   {<B>Create</B> creates a new ACE.
    Do not use this constructor.
    Instead use
      
        #  TJwDiscretionaryAccessControlEntryAllow.Create(...)
        #  TJwDiscretionaryAccessControlEntryDeny.Create(...)
        #  TJwAuditAccessControlEntry.Create(...)
      

    @param aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. 
    @param aFlags retrieves the ACE flags as a set 
    @param anAccessMask retrieves the access mask like GENERIC_ALL 
    @param aSID retrieves the ACE to be allowed or denied. It cannot be nil 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 

    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {<B>Create</B> creates a new ACE applying a revision level.
    Do not use this constructor.
    Instead use
      
        #  TJwDiscretionaryAccessControlEntryAllow.Create(...)
        #  TJwDiscretionaryAccessControlEntryDeny.Create(...)
        #  TJwAuditAccessControlEntry.Create(...)
      

    @param ListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. 
    @param Flags retrieves the ACE flags as a set 
    @param AccessMask retrieves the access mask like GENERIC_ALL 
    @param SID retrieves the ACE to be allowed or denied. It can be nil 
    @param Revision Defines the revision level of the ACE. Can be one of the revision levels:
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 

    }
    constructor Create(const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;

    {<B>Create</B> creates a copy of another ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.

    Do not use this constructor.
    Instead use
      TJwDiscretionaryAccessControlEntryAllow.Create(...)
      TJwDiscretionaryAccessControlEntryDeny.Create(...)
      TJwAuditAccessControlEntry.Create(...)
    @param aACE retrieves an existing ACE. It cannot be nil 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const AccessEntry: TJwSecurityAccessControlEntry); overload;

    {<B>Create</B> creates an access allowed structure from an existing one.
    @param AccessEntryPointer contains a pointer to an PAccessDeniedAce structure.
     
       # ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow 
       # ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow 
       # ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow 
       # ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow 

       # ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny 
       # ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny 
       # ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny 
       # ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny 

       # SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry 
       # SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback 
       # SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject 
       # SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject 

       # SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry 
     
     Other types will raise an exception EJwsclInvalidACEException.
     
    @return Returns a derived class of TJwSecurityAccessControlEntry 
    raises
 EJwsclNILParameterException:  if parameter AccessEntryPointer is nil 
     EJwsclInvalidACEException: will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure 
     EJwsclInvalidAceMismatch: will be raised if the header type of parameter AccessEntryPointer
      does not match this ACE instance type 
    }
    constructor Create(const AccessEntryPointer: PAccessAllowedAce); overload;

    {<B>Create</B> creates an access denied structure from an existing one.

    @param AccessEntryPointer contains a pointer to an PAccessDeniedAce structure.
     
       # ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow 
       # ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow 
       # ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow 
       # ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow 

       # ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny 
       # ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny 
       # ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny 
       # ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny 

       # SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry 
       # SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback 
       # SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject 
       # SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject 

       # SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry 
     
     Other types will raise an exception EJwsclInvalidACEException.
     
    @return Returns a derived class of TJwSecurityAccessControlEntry 
    raises
 EJwsclNILParameterException:  if parameter AccessEntryPointer is nil 
     EJwsclInvalidACEException: will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure 
     EJwsclInvalidAceMismatch: will be raised if the header type of parameter AccessEntryPointer
      does not match this ACE instance type 
    }
    constructor Create(const AccessEntryPointer: PAccessDeniedAce); overload;

    {<B>CreateACE</B> creates an access allowed class from an existing structure and returns
     a compatible class instance.

    @param AccessEntryPointer contains a pointer to an AccessAllowedACE structure.
      
       # ACCESS_ALLOWED_ACE returns class type TJwDiscretionaryAccessControlEntryAllow 
       # ACCESS_ALLOWED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackAllow 
       # ACCESS_ALLOWED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectAllow 
       # ACCESS_ALLOWED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectAllow 

       # ACCESS_DENIED_ACE returns class type TJwDiscretionaryAccessControlEntryDeny 
       # ACCESS_DENIED_CALLBACK_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackDeny 
       # ACCESS_DENIED_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryObjectDeny 
       # ACCESS_DENIED_CALLBACK_OBJECT_ACE returns class type TJwDiscretionaryAccessControlEntryCallbackObjectDeny 

       # SYSTEM_AUDIT_ACE returns class type TJwAuditAccessControlEntry 
       # SYSTEM_AUDIT_CALLBACK_ACE returns class type TJwAuditAccessControlEntryCallback 
       # SYSTEM_AUDIT_OBJECT_ACE returns class type TJwAuditAccessControlEntryObject 
       # SYSTEM_AUDIT_CALLBACK_OBJECT_ACE returns class type TJwAuditAccessControlEntryCallbackObject 

       # SYSTEM_MANDATORY_LABEL_ACE returns class type TJwSystemMandatoryAccessControlEntry 
     
     
    @return Returns a derived class of TJwSecurityAccessControlEntry 
    raises
 EJwsclNILParameterException:  if parameter AccessEntryPointer is nil 
     EJwsclInvalidACEException: will be raised if parameter
      AccessEntryPointer does not contain a supported ACE type structure 
    }
    class function CreateACE(const AccessEntryPointer: PAccessAllowedAce):
      TJwSecurityAccessControlEntry; overload;

    {<B>CreateACE</B> creates an access denied structure from an existing one.
    @param AccessEntryPointer contains a pointer to an AccessDeniedACE structure. 
    @return Returns an ACE of type TJwDiscretionaryAccessControlEntryDeny 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    class function CreateACE(const AccessEntryPointer: PAccessDeniedAce):
      TJwSecurityAccessControlEntry; overload;

  
    {<B>CreateACE</B> creates an with 0 initialized ACE and returns the corresponding class.
    @param anACEType receives an TJwAceType  type. 
    @return The return value is one of the listed class types

     
      # TjwAceType member - returned class type 

      # actAudit - TJwAuditAccessControlEntry 
      # actAuditCallback - TJwAuditAccessControlEntryCallback 
      # actAuditObject - TJwAuditAccessControlEntryObject 
      # actAuditCallbackObject - TJwAuditAccessControlEntryCallbackObject 

      # actAllow - TJwDiscretionaryAccessControlEntryAllow 
      # actAllowCallback - TJwDiscretionaryAccessControlEntryCallbackAllow 
      # actAllowObject - TJwDiscretionaryAccessControlEntryObjectAllow 
      # actAllowCallbackObject - TJwDiscretionaryAccessControlEntryCallbackObjectAllow 
      # actDeny - TJwDiscretionaryAccessControlEntryDeny 
      # actDenyCallback - TJwDiscretionaryAccessControlEntryCallbackDeny 
      # actDenyObject - TJwDiscretionaryAccessControlEntryObjectDeny 
      # actDenyCallbackObject - TJwDiscretionaryAccessControlEntryCallbackObjectDeny 
      
      # actMandatory - TJwSystemMandatoryAccessControlEntry 
     
     
     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 
    }
    class function CreateACE(anAceType: TJwAceType):
      TJwSecurityAccessControlEntry;
      overload;


    {<B>Create</B> initializes a ACE. It is used internally}
    constructor Create; overload;

  public
    {<B>Destroy</B> destroys an ACE.
     raises
 EJwsclReadOnlyPropertyException:  will be raised if the ACE is in a list (ListOwner is not nil) 
     }
    destructor Destroy; override;

    {<B>Free</B> frees the memory of an ACE if it does not belong to a list (ListOwner = nil); otherwise nothing happens.
     Instead use Remove of TJwSecurityAccessControlList .

     raises
 EJwsclInvalidACEException:  will be raised if the ACE is not in the list of ListOwner  
    }
    procedure Free;
  protected

    {<B>Create_AllowACE</B> creates a memory block filled with an ACE structure.
     The structure is points to PAccessAllowedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     
      # actAudit 
      # actAuditCallback 
      # actMandatory 
      # actAllow 
      # actAllowCallback 
      # actDeny 
      # actDenyCallback 
     
     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 
    }
    function Create_AllowACE: PAccessAllowedAce; overload;


    {<B>Create_DenyACE</B> creates a memory block filled with an ACE structure.
     The structure is points to PAccessDeniedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     
      # actAudit 
      # actAuditCallback 
      # actMandatory 
      # actAllow 
      # actAllowCallback 
      # actDeny 
      # actDenyCallback 
     

     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 
     }
    function Create_DenyACE: PAccessDeniedAce; overload;


    {<B>CreateDynamicACE</B> creates an ACE WinAPI structure depending on the Sid type (property AceType) of
     the instance.

     The following types are supported:
     
      # TjwAceType member - returned pointer to structure 
      # actAudit - SYSTEM_AUDIT_ACE 
      # actAuditCallback - SYSTEM_AUDIT_CALLBACK_ACE 
      # actAuditObject - SYSTEM_AUDIT_OBJECT_ACE 
      # actAuditCallbackObject - SYSTEM_AUDIT_CALLBACK_OBJECT_ACE 
      # actMandatory - SYSTEM_MANDATORY_LABEL_ACE 
      # actAllow - ACCESS_ALLOWED_ACE 
      # actAllowCallback - ACCESS_ALLOWED_CALLBACK_ACE 
      # actAllowObject - ACCESS_ALLOWED_OBJECT_ACE 
      # actAllowCallbackObject - ACCESS_ALLOWED_CALLBACK_OBJECT_ACE 
      # actDeny - ACCESS_DENIED_ACE 
      # actDenyCallback - ACCESS_DENIED_CALLBACK_ACE 
      # actDenyObject - ACCESS_DENIED_OBJECT_ACE 
      # actDenyCallbackObject - ACCESS_DENIED_CALLBACK_OBJECT_ACE 
     
     @param Size returns the memory size of the newly allocated block in bytes.
     @return The return value is a pointer to a ACE Winapi structure. The supported
       types can be read from the description above.
       The memory block must be freed with GlobalFree. 
     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 
    }
    function CreateDynamicACE(out Size : Cardinal) : Pointer;


    {<B>GetDynamicTypeSize</B> returns the size of an ace structure without the additional
     space for the Sid memory.

     The following types are supported:
     
      # actAudit 
      # actAuditCallback 
      # actAuditObject 
      # actAuditCallbackObject 
      # actMandatory 
      # actAllow 
      # actAllowCallback 
      # actAllowObject 
      # actAllowCallbackObject 
      # actDeny 
      # actDenyCallback 
      # actDenyObject 
      # actDenyCallbackObject 
     
     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 

    }
    function GetDynamicTypeSize : Cardinal;

    {<B>Free_PACE</B> frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_AllowACE .

     @param aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens. 

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessAllowedAce); overload;

    {<B>Free_PACE</B> frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_DenyACE .

     @param aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens. 

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessDeniedAce); overload;

    {<B>GetExplicitAccess</B> creates an Explicit Access structure.

     If the ACE is an audit ace the grfAccessMode value is set to SET_AUDIT_FAILURE even if the flag AuditFailure is not set.
      You have to create two ExplicitAccess structures and change them to use them.

     raises
 EJwsclInvalidSIDException:  will be raised if the property SID is nil or invalid. 
     }
    function GetExplicitAccess: TJwExplicitAccess;

  public
    {<B>GetClassAceType</B> returns an ACE class type depending on the give ACE type.
     To create a class instance CreateACE  can be used.

     @param AceType defines the wished ace type.
          Supported types are
          
            # actAudit 
            # actAuditCallback 
            # actAuditObject 
            # actAuditCallbackObject 
            # actMandatory 
            # actAllow 
            # actAllowCallback 
            # actAllowObject 
            # actAllowCallbackObject 
            # actDeny 
            # actDenyCallback 
            # actDenyObject 
            # actDenyCallbackObject 
           
     raises
 EJwsclInvalidACEException:  will be raised if the given ACE type
        is not supported. 
    }
    class function GetClassAceType(const AceType: TJwAceType)
      : TJwSecurityAccessControlEntryClass;

    {<B>Assign</B> copies all properties from another ACE.
     This method does not add this instance to any list or change ListOwner.

     raises
 EJwsclNILParameterException:  will be raised if one of these
        objects are nil:
        
          1. AccessEntry 
          2. AccessEntry.SID 
        
      

    }
    procedure Assign(AccessEntry: TJwSecurityAccessControlEntry); virtual;

    {<B>GetText</B> returns information about the SID instance using human readable
     description. This function shows bit states for the AccessMask.
     Use GetTextMap  to define readable access rights mapping.}
    function GetText: TJwString;
  public
    {<B>GetTextMap</B> returns information about the SID instance using human readable
     description. This function can convert the AccessMask bits into strings
     using a mapping class.
     @param Mapping defines a class thats provides the mapping implementation.
       If this parameter is nil the AccessMask will be shown as decimal and
       as hex number.
         
    }
    function GetTextMap(const Mapping: TJwSecurityGenericMappingClass =
      nil): TJwString;

    {<B>ListOwner</B> contains the owner of this ACE. It cannot be set.
     If ListOwner is nil it can be set once. If it set to a
      TJwSecurityAccessControlList instance the ACE will automatically added
      to the list.

     To change the value a copy must be created of the instance.  }
    property ListOwner: TJwSecurityAccessControlList
      Read fListOwner Write SetListOwner;

    {<B>Flags</B> contains the flags of the ACE}
    property Flags: TJwAceFlags Read fFlags Write SetFlags;

    {<B>AccessMask</B> contains the access mask of the ACE.}
    property AccessMask: TJwAccessMask Read GetAccessMask Write SetAccessMask;

    {<B>SID</B> contains the security identifier. It can be nil.
     }
    property SID: TJwSecurityId Read fSid Write SetSID;

    {<B>AceType</B> returns the type of the access control entry.
     It can return the following values :
     
       #  TJwDiscretionaryAccessControlEntry        - actAllow
       #  TJwDiscretionaryAccessControlEntryAllow   - actAllow
       #  TJwDiscretionaryAccessControlEntryDeny    - actDeny
       #  TJwAuditAccessControlEntry                - actAudit
       
       }
    property AceType: TJwAceType Read GetAceType;

    {<B>OwnSID</B> defines whether the TJwSecurityId SID will be freed (True) or not (False).
     If property SID is a well known SID from unit JwsclKnownSid this property will be ignored)
    }
    property OwnSID: boolean Read GetOwnSID Write SetownSID;

    property Header : TAceHeader read fHeader write fHeader;

    {<B>Revision</B> gets or sets or gets the revision of the ACE
     Can be one of the revision levels:
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS
    }
    property Revision : Cardinal read fRevision write fRevision;

    {not used}
    property Ignore: boolean Read fIgnore Write fIgnore;

    {<B>UserData</B> defines user data that can be used to attach used defined data}
    property UserData : Pointer read fUserData write fUserData;

    {<B>ObjectFlags</B> returns the object flags of the ACE.
     This property depends on property ObjectType and InheritedObjectType.

     It returns a combination of these flags if parameter...
      
       # ACE_OBJECT_TYPE_PRESENT ...ObjectType is not a NULL GUID 
       # ACE_INHERITED_OBJECT_TYPE_PRESENT ...InheritedObjectType is not a NULL GUID 
      
    }
    property ObjectFlags : Cardinal read GetObjectFlags;

    {<B>ObjectType</B> contains the object or property specific guid.
     It is only of use if the instance of this ACE is an object of one of these types
      
        # TJwAuditAccessControlEntryObject 
        # TJwAuditAccessControlEntryCallbackObject 
        # TJwDiscretionaryAccessControlEntryObjectAllow 
        # TJwDiscretionaryAccessControlEntryCallbackObjectAllow 
        # TJwDiscretionaryAccessControlEntryObjectDeny 
        # TJwDiscretionaryAccessControlEntryCallbackObjectDeny 
      

      See also http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx
     }
    property ObjectType : TGuid read fObjectType write fObjectType;

     {<B>InheritedObjectType</B> contains the object or property specific guid.
     It is only of use if the instance of this ACE is an object of one of these types
      
        # TJwAuditAccessControlEntryObject 
        # TJwAuditAccessControlEntryCallbackObject 
        # TJwDiscretionaryAccessControlEntryObjectAllow 
        # TJwDiscretionaryAccessControlEntryCallbackObjectAllow 
        # TJwDiscretionaryAccessControlEntryObjectDeny 
        # TJwDiscretionaryAccessControlEntryCallbackObjectDeny 
      

      See also http://msdn2.microsoft.com/en-us/library/aa374917(VS.85).aspx
     }
    property InheritedObjectType : TGuid read fInheritedObjectType write fInheritedObjectType;
  end;

  {<B>TJwDiscretionaryAccessControlEntry</B> defines a discretionary access control entry.
   Use TJwDiscretionaryAccessControlEntryAllow  or TJwDiscretionaryAccessControlEntryDeny  for creating
    ACEs.
   }
  TJwDiscretionaryAccessControlEntry = class(TJwSecurityAccessControlEntry);

  {<B>TJwDiscretionaryAccessControlEntryAllow</B> is a class that defines a positve/allow access control entry.}
  TJwDiscretionaryAccessControlEntryAllow =
    class(TJwDiscretionaryAccessControlEntry)
  public
    { <b>Create</b> creates a new positive ACE.
      
      
      Parameters
      aListOwner :    retrieves the list owner (including nil). If it is set to a list
                      (not nil) the ACE is added to the list automatically. 
      aFlags :        retrieves the ACE flags as a set 
      anAccessMask :  retrieves the access mask like GENERIC_ALL. If you want to set
                      \file or folder security use FILE_ALL_ACCESS or similar instead
                      of GENERIC_XXX. Some flags are discarded when written to disk and
                      would differ after read from disk.<p />
      aSID :          retrieves the SID to be allowed or denied. It can be nil 
      ownSID :        defines whether the SID given in parameter aSID should be freed
                      automatically. If this SID is a well known SID from unit
                      JwsclKnownSid this parameter is ignored
      
      Remarks
      The given SID in parameter aSID<b> must not</b> be already assigned to another
      access control instance if both - this and the other instance - have the boolean
      parameter ownSID (or its property OwnSid) set to true. In this case the first
      ACE instance may destroy the used SID instance and which has an effect on the
      second ACE instance that runs with an invalid SID instance.
      
      If you want to copy (even with changed values) an instance, use the copy
      constructor instead. If you also want to set the aListOwner parameter, simply
      add the new instance to the target list.                                          }

    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    { <b>Create</b> creates a new positive ACE and applies an ACE revision level.
      Parameters
      aListOwner :    retrieves the list owner (including nil). If it is set to a list
                      (not nil) the ACE is added to the list automatically.
      aFlags :        retrieves the ACE flags as a set
      anAccessMask :  retrieves the access mask like GENERIC_ALL. If you want to set
                      \file or folder security use FILE_ALL_ACCESS or similar instead
                      of GENERIC_XXX. Some flags are discarded when written to disk and
                      would differ after read from disk.<p />
      Revision :      Defines the revision level of the ACE. Can be one of the revision
                      levels. ACL_REVISION, ACL_REVISION1, ACL_REVISION2,
                      ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS
      aSID :          retrieves the SID to be allowed or denied. It can be nil
      ownSID :        defines whether the SID given in parameter aSID should be freed
                      automatically. If this SID is a well known SID from unit
                      JwsclKnownSid this parameter is ignored
      Remarks
      The given SID in parameter aSID<b> must not</b> be already assigned to another
      access control instance if both - this and the other instance - have the boolean
      parameter ownSID (or its property OwnSid) set to true. In this case the first
      ACE instance may destroy the used SID instance and which has an effect on the
      second ACE instance that runs with an invalid SID instance.
      
      If you want to copy (even with changed values) an instance, use the copy
      constructor instead. If you also want to set the aListOwner parameter, simply
      add the new instance to the target list.                                          }
    constructor Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;


    {<B>Create</B> creates a positive copy of another positive ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.
     It is possible to provide a negative ACE (by type conversion). In this case
      the ACE is converted to a positive one.

    @param aACE retrieves an existing positive ACE. It cannot be nil 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const AccessEntry: TJwDiscretionaryAccessControlEntryAllow); overload;


    {<B>Create</B> creates an access allowed structure from an existing one.
    @param accessACE contains a pointer to an AccessAllowedACE structure. 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const AccessEntryPointer: PAccessAllowedAce); overload;

    {<B>Create_AllowACE</B> creates a memory block filled with an ACE structure.
     The structure is points to PAccessAllowedAce structure;
     It must be freed by Free_PACE.

     The following types (value of property AceType) are supported:
     
      # actAudit 
      # actAuditCallback 
      # actMandatory 
      # actAllow 
      # actAllowCallback 
      # actDeny 
      # actDenyCallback 
     
     raises
 EJwsclInvalidACEException:  will be raised if the ACE type of
      the instance is not supported 
     }
    function Create_AllowACE: PAccessAllowedAce; overload;

    {<B>Free_PACE</B> frees a PAccessAllowedAce access control list.
     It can free ACE memory created by Create_AllowACE .

     @param aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens. 

     }
    procedure Free_PACE(var AccessEntryPointer: PAccessAllowedAce); overload;
  end;

  {<B>TJwDiscretionaryAccessControlEntryCallbackAllow</B> defines a callback allow access control element.
   Every time a function that scans an ACL with a callback element generates
   a callback event which decides whether this ACE can be used in the
   process.
   Callback elements are used in unit JwsclAuthCtx method TJwAuthContext.AccessCheck.
  }
  TJwDiscretionaryAccessControlEntryCallbackAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;

  {<B>TJwDiscretionaryAccessControlEntryObjectAllow</B> defines a allow access control element with object properties.
   Object ACEs uses the following properties

    * ObjectFlags
    * ObjectType
    * InheritedObjectType

  }
  TJwDiscretionaryAccessControlEntryObjectAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;

  { <b>TJwDiscretionaryAccessControlEntryCallbackObjectAllow</b> defines a callback
    allow access control element with object properties. Every time a function that
    scans an ACL with a callback element generates a callback event which decides
    whether this ACE can be used in the process. Callback elements are used in unit JwsclAuthCtx.pas
    method TJwAuthContext.AccessCheck.
    
    Object ACEs uses the following properties
    
      * ObjectFlags
      * ObjectType
      * InheritedObjectType
    
    For some Windows internal reasons this type of ACE is ignored in
    TJwAuthContext.AccessCheck .
    
    
                                                                                                     }
  TJwDiscretionaryAccessControlEntryCallbackObjectAllow =
    class(TJwDiscretionaryAccessControlEntryAllow)
  end;


  {<B>TJwDiscretionaryAccessControlEntryDeny</B> is a class that defines a negative/deny access control entry.}
  TJwDiscretionaryAccessControlEntryDeny =
    class(TJwDiscretionaryAccessControlEntry)
  public
    {<B>Create</B> creates a new negative ACE.

    @param aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. 
    @param aFlags retrieves the ACE flags as a set 
    @param anAccessMask retrieves the access mask like GENERIC_ALL 
    @param aSID retrieves the SID to be allowed or denied. It can be nil 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 

    Remarks
      The given SID in parameter aSID<b> must not</b> be already assigned to another
      access control instance if both - this and the other instance - have the boolean
      parameter ownSID (or its property OwnSid) set to true. In this case the first
      ACE instance may destroy the used SID instance and which has an effect on the
      second ACE instance that runs with an invalid SID instance.
      
      If you want to copy (even with changed values) an instance, use the copy
      constructor instead. If you also want to set the aListOwner parameter, simply
      add the new instance to the target list.                                    
    }

    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {<B>Create</B> creates a new negative ACE and applies an ACE revision level.

    @param aListOwner retrieves the list owner  (including nil). If it is set to a list (not nil) the ACE is added to the list automatically. 
    @param aFlags retrieves the ACE flags as a set 
    @param anAccessMask retrieves the access mask like GENERIC_ALL.
           If you want to set file or folder security use FILE_ALL_ACCESS or similar instead of GENERIC_XXX.
           Some flags are discarded when written to disk and would differ after read from disk.
               
    @param Revision Defines the revision level of the ACE. Can be one of the revision levels.
          ACL_REVISION, ACL_REVISION1, ACL_REVISION2, ACL_REVISION3, ACL_REVISION4 or ACL_REVISION_DS 
    @param aSID retrieves the SID to be allowed or denied. It can be nil 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 

    Remarks
      The given SID in parameter aSID<b> must not</b> be already assigned to another
      access control instance if both - this and the other instance - have the boolean
      parameter ownSID (or its property OwnSid) set to true. In this case the first
      ACE instance may destroy the used SID instance and which has an effect on the
      second ACE instance that runs with an invalid SID instance.
      
      If you want to copy (even with changed values) an instance, use the copy
      constructor instead. If you also want to set the aListOwner parameter, simply
      add the new instance to the target list.                                    

    }
    constructor Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True); overload;


    {<B>Create</B> creates a negative copy of another negative ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.
     It is possible to provide a positive ACE (by type conversion). In this case
      the ACE is converted to a negative one.

    @param aACE retrieves an existing positive ACE. It cannot be nil 
    raises
     EJwsclNILParameterException:  if aACE is nil
    }
    constructor Create(AccessEntry: TJwDiscretionaryAccessControlEntryDeny); overload;

    {<B>Create</B> creates an access denied structure from an existing one.
    @param accessACE contains a pointer to an AccessDeniedACE structure. 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const AccessEntryPointer: PAccessDeniedAce); overload;

    {<B>Create_DenyACE</B> creates a memory block filled with an ACE structure.
     The structure is points to PAccessDeniedAce structure;
     It must be freed by Free_PACE.
     }
    function Create_DenyACE: PAccessDeniedAce; overload;

    {<B>Free_PACE</B> frees a PAccessDeniedAce access control list.
     It can free ACE memory created by Create_DenyACE .

     @param aPACE a PAccessAllowedAce or PAccessDeniedAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens. 
     }
    procedure Free_PACE(var AccessEntryPointer: PAccessDeniedAce); overload;
  end;


  {<B>TJwDiscretionaryAccessControlEntryCallbackDeny</B> defines a callback deny access control element.
   Every time a function that scans an ACL with a callback element generates
   a callback event which decides whether this ACE can be used in the
   process.
   Callback elements are used in unit JwsclAuthCtx method TJwAuthContext.AccessCheck.
  }
  TJwDiscretionaryAccessControlEntryCallbackDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;

  {<B>TJwDiscretionaryAccessControlEntryObjectDeny</B> defines a deny access control element with object properties.
   Object ACEs uses the following properties

    * ObjectFlags
    * ObjectType
    * InheritedObjectType
   )
  }
  TJwDiscretionaryAccessControlEntryObjectDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;


  {<B>TJwDiscretionaryAccessControlEntryCallbackObjectDeny</B> defines a callback deny access control element with object properties.
   Every time a function that scans an ACL with a callback element generates
   a callback event which decides whether this ACE can be used in the
   process.
   Callback elements are used in unit JwsclAuthCtx method TJwAuthContext.AccessCheck.

   Object ACEs uses the following properties

    * ObjectFlags
    * ObjectType
    * InheritedObjectType
   )

   For some Windows internal reasons this type of ACE is ignored in
   TJwAuthContext.AccessCheck .
  }
  TJwDiscretionaryAccessControlEntryCallbackObjectDeny =
    class(TJwDiscretionaryAccessControlEntryDeny)
  end;


  {<B>TJwAuditAccessControlEntry</B> provides function for a system control entry.
   The flags property is ignored.
  }
  TJwAuditAccessControlEntry = class(TJwSecurityAccessControlEntry)
  private
  protected
    fAuditSuccess, fAuditFailure: boolean;
    fFlagsIgnored: TJwAceFlags;

    constructor Create; overload;
    function GetAuditSuccess: boolean; virtual;
    function GetAuditFailure: boolean; virtual;

    procedure SetAuditFailure(const Value: boolean); virtual;
    procedure SetAuditSuccess(const Value: boolean); virtual;

    function GetAccessMask : TJwAccessMask; override;
  public
    {<B>Create</B> creates a new audit ACE.  (SACL).
     AuditSuccess, AuditFailure are set to False and can be changed afterwards.

    @param aListOwner retrieves the list owner (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.  
    @param aFlags retrieves the ACE flags as a set 
    @param anAccessMask retrieves the access mask like GENERIC_ALL 
    @param aSID retrieves the SID to be allowed or denied. It can be nil 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 


    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      const aFlags: TJwAceFlags;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean = True); overload;

    {<B>Create</B> creates a new audit ACE.  (SACL).

    @param aListOwner retrieves the list owner (including nil). If it is set to a list (not nil) the ACE is added to the list automatically.  
    @param aFlags retrieves the ACE flags as a set 
    @param anAccessMask retrieves the access mask like GENERIC_ALL 
    @param anAuditSuccess receives the state of success audit flag. If true the ACE will
        audit successfull access. 
    @param aAuditFailure receives the state of failure audit flag. If true the ACE will
        audit failed access. 
    @param aSID retrieves the SID to be allowed or denied. It can be nil 
    @param ownSID defines whether the SID given in parameter aSID should be freed automatically.
            If this SID is a well known SID from unit JwsclKnownSid this parameter is ignored 


    }
    constructor Create(const aListOwner: TJwSecurityAccessControlList;
      aAuditSuccess, aAuditFailure: boolean;
      const anAccessMask: TJwAccessMask;
      const aSID: TJwSecurityId;
      ownSID: boolean); overload;

    {<B>Create</B> creates a copy of another audit ACE (SACL).
     All properties are copied except ListOwner. The ACE is not added to any list.
     Internally the audit object is converted to a positive ACE.

    @param aACE retrieves an existing positive ACE. It cannot be nil 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(AccessEntry: TJwAuditAccessControlEntry); overload;

    {<B>Create</B> creates an audit access structure from an existing one.
     The properties AuditSuccess and AuditFailure are retrieved from the accessACE.Flags value.

    @param accessACE contains a pointer to an PSystemAuditAce structure. 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const AccessEntryPointer: PSystemAuditAce); overload;

    {<B>Create_AuditACE</B> creates a memory block filled with an ACE audit structure.
     The properties AuditSuccess and AuditFailure are set from the accessACE.Flags value.

     The structure is points to PSystemAuditAce structure;
     It must be freed by Free_PACE.
     }
    function Create_AuditACE: PSystemAuditAce; overload;

    {<B>Free_PACE</B> frees a PSystemAuditAce access control list.
     It can free ACE memory created by Create_AuditACE .

     @param aPACE a PSystemAuditAce to be freed. It will be set to nil afterwards.
            If aPACE is nil nothing happens. 
     }
    procedure Free_PACE(var AccessEntryPointer: PSystemAuditAce); overload;

    {<B>Assign</B> copies all properties from another audit ACE.
     The instance @classname must not be already added to a list (ListOwner must be nil).
      However aObject can be in a list.
     <B>Assign</B> creates copies of all properties. It also makes a copy of the SID and sets
      ownSID to True so the SID will be freed on destruction.
     ListOwner will be set to nil. You have to add the ACE manually.
    }
    procedure Assign(AccessEntry: TJwAuditAccessControlEntry); reintroduce; virtual;
  public
    {<B>AuditSuccess</B> defines whether an positive access is audited (True) or not (False).}
    property AuditSuccess: boolean Read GetAuditSuccess Write SetAuditSuccess;

    {<B>AuditFailure</B> defines whether an negative access is audited (True) or not (False).}
    property AuditFailure: boolean Read GetAuditFailure Write SetAuditFailure;

    //<B>end</B> is ignored and cannot be changed - the value is empty
    //  property Flags: TJwAceFlags read fFlagsIgnored;
  end;

  {<B>TJwAuditAccessControlEntryCallback</B> defines a callback audit access control element.
   Every time a function that scans an ACL with a callback element generates
   a callback event which decides whether this ACE can be used in the
   process.
   Callback elements are used in unit JwsclAuthCtx method TJwAuthContext.AccessCheck.
  }
  TJwAuditAccessControlEntryCallback =
    class(TJwAuditAccessControlEntry)
  end;



  {<B>TJwAuditAccessControlEntryObject</B> defines a audit access control element with object properties.
   Object ACEs uses the following properties

    * ObjectFlags
    * ObjectType
    * InheritedObjectType
   )
  }
  TJwAuditAccessControlEntryObject =
    class(TJwAuditAccessControlEntry)
  end;

  {<B>TJwAuditAccessControlEntryCallbackObject</B> defines a callback audit access control element with object properties.
   Every time a function that scans an ACL with a callback element generates
   a callback event which decides whether this ACE can be used in the
   process.
   Callback elements are used in unit JwsclAuthCtx method TJwAuthContext.AccessCheck.

   Object ACEs uses the following properties
    * ObjectFlags
    * ObjectType
    * InheritedObjectType

   For some Windows internal reasons this type of ACE is ignored in
   TJwAuthContext.AccessCheck .
  }
  TJwAuditAccessControlEntryCallbackObject =
    class(TJwAuditAccessControlEntry)
  end;


  {<B>TJwSystemMandatoryAccessControlEntry</B> defines a mandatory label ACE in a SACL.
   }
  TJwSystemMandatoryAccessControlEntry = class(TJwSecurityAccessControlEntry)
  public
    constructor Create(const MandatoryLevel : TMandatoryLevel;
      const ListOwner: TJwSAccessControlList = nil);overload;

    {<B>Create</B> creates a mandatory label structure from an existing one.
    @param accessACE contains a pointer to an MandatoryLabel structure. 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const MandatoryLabel: PSystemMandatoryLabelAce); overload;


    {<B>Create</B> creates a copy of another ACE.
     All properties are copied except ListOwner. The ACE is not added to any list.

    @param aACE retrieves an existing ACE. It cannot be nil 
    raises
 EJwsclNILParameterException:  if aACE is nil 
    }
    constructor Create(const MandatoryLabel: TJwSystemMandatoryAccessControlEntry); overload;

    {<B>GetMandatoryLevelType</B> returns the mandatory level type.}
    function GetMandatoryLevelType : TMandatoryLevel; virtual;

    {<B>GetMandatoryPolicy</B> returns the policy of the mandatory label.
     This policy defines how access is defined to lower or higher objects.
     This value is an interpreted value from property AccessMask.}
    function GetMandatoryPolicy : TJwMandatoryPolicyFlagSet; virtual;

    {<B>SetMandatoryPolicy</B> sets the mandatory policy.
     This function simply changes the property AccessMask.
    }
    procedure SetMandatoryPolicy(const Level : TJwMandatoryPolicyFlagSet); virtual;


    {<B>Compare</B> compare the mandatory ACE with another one.

     @param MandatoryLabel defines a label to be compared 
     @return The return value is smaller than zero if the current label is less
      privileged than the given label. If the return value is greater than zero
      the given label has a higher level than the given one.
      The returned value is the difference of
       "Self.GetMandatoryLevelType - MandatoryLabel.GetMandatoryLevelType"

      If the return value is zero both labels are equal. 
     raises
 EJwsclNILParameterException:  The exception is raised if parameter
       MandatoryLabel is nil 
     }
    function Compare(const MandatoryLabel : TJwSystemMandatoryAccessControlEntry) : Integer;

    {<B>MandatoryPolicy</B> sets or gets the mandatory policy.
     See http://msdn2.microsoft.com/en-us/library/aa965848.aspx}
    property MandatoryPolicy : TJwMandatoryPolicyFlagSet read GetMandatoryPolicy
      write SetMandatoryPolicy;
  end;

  { <b>JwFormatAccessRights</b> creates a formatted string that
    splits up an access mask into its rights constants. The
    constants can be named if parameter RightsMapping is used.
    
    Each line contains a checkbox that defines whether the access
    mask contains this specifc right or not. Furthermore it
    contains the name of the right. Lines may look like this:
    <code>
    '[X] <Name of access rights>'#13#10  for a access right that is set
    '[ ] <Name of access rights>'#13#10  for a access right that is not set
    </code>
    
    Parameters
    GrantedAccess :  defines an array of access masks. Each array
                     element will get its own column (zero based).
    RightsMapping :  defines a map structure that maps between
                     constants and strings (e.g.
                     JwsclConstants.FileMapping ) 
    Returns
    \Returns the access mask with its rights states. Each line is
    separated by a line break (#13#10)                             }
  function JwFormatAccessRights(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString; overload;


  { <b>JwFormatAccessRights</b> creates a formatted string that
    splits up several access massk into its rights constants. The
    constants can be named if parameter RightsMapping is used.
    
    Each line contains one or more checkboxes that define whether
    the access masks contain a specifc right or not. Furthermore
    it contains the name of the right. A line may look like this:
    <code>
     [X] .. [ ] <Name of access rights>  for a access right that is set
     [ ] .. [ ] <Name of access rights>  for a access right that is not set
    </code>
    
    Parameters
    GrantedAccess :  defines an array of access masks. Each array
                     element will get its own column (zero based).
    AccessStatus :   defines an array of the error status of the
                     right. If status is neither zero nor 5
                     (access denied) instead of an emtyp or
                     checked checkbox ([ ] or [X]), the status
                     will be shown in the checkbox (e.g. [2]). The
                     array index of GrantedAccess will be used to
                     get the error status index from AccessStatus.
                     Thus the length of array AccessStatus and
                     GrantedStatus must be the same. 
    RightsMapping :  defines a map structure that maps between
                     constants and strings (e.g.
                     JwsclConstants.FileMapping ) 
    Returns
    \Returns the access mask with its rights states. Each line is
    separated by a line break (#13#10)                             }

  function JwFormatAccessRights(
    const GrantedAccess : TJwAccessMaskArray;
    const AccessStatus : TJwCardinalArray;
    RightsMapping : Array of TJwRightsMapping) : TJwString; overload;

  {<b>JwFormatAccessRightsSimple</b> does the same as JwFormatAccessRights
   but returns just a comma separated list of specific  access rights names.}
  function JwFormatAccessRightsSimple(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString;


  {<B>IsStandardRight</B> returns true if the given right has a standard (16-23) bit set;
   otherwise false.}
  function IsStandardRight(const Right : Cardinal) : Boolean;

  {<B>IsGenericRight</B> returns true if the given right has a generic (31-28) bit set;
   otherwise false.}
  function IsGenericRight(const Right : Cardinal) : Boolean;

  {<B>IsReservedRight</B> returns true if the given right has a reserved (27, 25) bit set;
   otherwise false.}
  function IsReservedRight(const Right : Cardinal) : Boolean;

  {<B>IsMaximumRight</B> returns true if the given right has the maximum allowed (26) bit set;
   otherwise false.}
  function IsMaximumRight(const Right : Cardinal) : Boolean;

  {<B>IsSpecificRight</B> returns true if the given right has an object specific (0..15) bit set;
   otherwise false.}
  function IsSpecificRight(const Right : Cardinal) : Boolean;

  {<B>IsSACLRight</B> returns true if the given right has the system (24) bit set;
   otherwise false.}
  function IsSACLRight(const Right : Cardinal) : Boolean;

  {<B>JwRightType</B> returns the type of a given right. The function checks the bits
   of parameter Right from higher bits to lower bits and returns the type when it
   finds a set bit.
   The order it returns a type is the following :
     Generic, Reserved, Maximum, SACL, Standard, Specific,

   @return Returns the type of right as TJwRightType. It returns rtNone
    if parameter Right is zero.  
  }
  function JwRightType(const Right : Cardinal) : TJwRightType;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math, JwsclEnumerations;



{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

procedure RaiseOnInvalidRevision(const Revision: Cardinal; const MethodName, ClassName : TJwString);
begin
 if (Revision < MIN_ACL_REVISION) or (Revision > MAX_ACL_REVISION) then
   raise EJwsclInvalidRevision.CreateFmtEx(RsInvalidRevision,
      MethodName, ClassName, RsUNAcl, 0, false, [Revision]);
end;


function JwFormatAccessRights(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString;
var i : Integer;
begin
  for i := low(RightsMapping) to high(RightsMapping) do
  begin
    if Access and RightsMapping[i].Right =
      RightsMapping[i].Right then
      Result := Result + '[X] '
    else
      Result := Result + '[ ] ';

    //names may vary depeding on resource string contents
    Result := Result + RightsMapping[i].Name;

    case JwRightType(RightsMapping[i].Right ) of
      rtGeneric  : result := result + ' ' + RsRightGeneric;
      rtReserved : result := result + ' ' + RsRightReserved;
      rtMaximum  : result := result + ' ' + RsRightMaximumAllowed;
      rtSystem   : result := result + ' ' + RsRightSacl;
      rtStandard : result := result + ' ' + RsRightStandard;
      rtSpecific : result := result + ' ' + RsRightSpecific;
    end;

    result := result + #13#10;
  end;
end;

function JwFormatAccessRightsSimple(const Access : Cardinal;
     RightsMapping : Array of TJwRightsMapping) : TJwString;
var i : Integer;
begin
  for i := low(RightsMapping) to high(RightsMapping) do
  begin
    if Access and RightsMapping[i].Right =
      RightsMapping[i].Right then
    begin
      //names may vary depeding on resource string contents
      Result := Result + RightsMapping[i].Name+', ';
    end;
  end;
  System.Delete(Result, Length(Result)-1, 2);
end;

function IsStandardRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and $FF0000) <> 0;
end;

function IsGenericRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and $F0000000) <> 0;
end;

function IsReservedRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and $A000000) <> 0;
end;

function IsMaximumRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and MAXIMUM_ALLOWED) <> 0;
end;

function IsSpecificRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and $FFFF) <> 0;
end;

function IsSACLRight(const Right : Cardinal) : Boolean;
begin
  result := (Right and ACCESS_SYSTEM_SECURITY) <> 0;
end;

function JwRightType(const Right : Cardinal) : TJwRightType;
begin
  result := rtNone;
  if Right = 0 then
    exit;

  result := rtUnknown;

  if IsGenericRight(Right) then
    result := rtGeneric
  else
  if IsReservedRight(Right) then
    result := rtReserved
  else
  if IsMaximumRight(Right) then
    result := rtMaximum
  else
  if IsSACLRight(Right) then
    result := rtSystem
  else
  if IsStandardRight(Right) then
    result := rtStandard
  else
  if IsSpecificRight(Right) then
    result := rtSpecific;
end;


function JwFormatAccessRights(
    const GrantedAccess : TJwAccessMaskArray;
    const AccessStatus : TJwCardinalArray;
    RightsMapping : Array of TJwRightsMapping) : TJwString;
var i,i2 : Integer;
begin
  //still undefined
  //JwRaiseOnInvalidParameter(Length(GrantedAccess) <> Length(AccessStatus),
  //   'GrantedAccess or AccessStatus', 'JwFormatAccessRights', '', RsUNAcl);

  //result := '- generic'#13#10;
  result := '';
  for i := low(RightsMapping) to high(RightsMapping) do
  begin
    for i2 := 0 to High(GrantedAccess) do
    begin
      if GrantedAccess[i2] and RightsMapping[i].Right =
        RightsMapping[i].Right then
        result := result + '[X]'
      else
      if (AccessStatus[i2] <> ERROR_SUCCESS) and
         (AccessStatus[i2] <> 5)
        then
        result := result + '['+IntToStr(AccessStatus[i2])+']'
      else
        result := result + '[ ]';
    end;

    //names may vary depeding on resource string contents
    result := result + ' ' + RightsMapping[i].Name;

    case JwRightType(RightsMapping[i].Right ) of
      rtGeneric  : result := result + ' ' + RsRightGeneric;
      rtReserved : result := result + ' ' + RsRightReserved;
      rtMaximum  : result := result + ' ' + RsRightMaximumAllowed;
      rtSystem   : result := result + ' ' + RsRightSacl;
      rtStandard : result := result + ' ' + RsRightStandard;
      rtSpecific : result := result + ' ' + RsRightSpecific;
    end;

    result := result + #13#10;
  end;
end;


constructor TJwDAccessControlList.Create;
begin
  inherited;
end;

constructor TJwDAccessControlList.Create(ownACEs: boolean);
begin
  inherited Create(ownACEs);
end;

constructor TJwDAccessControlList.Create(AclPointerList: PACL);
begin
  inherited Create(AclPointerList);
end;

constructor TJwDAccessControlList.Create(const Accesses : TJwExplicitAccessArray);
begin
  inherited;
end;

function TJwDAccessControlList.GetInheritance: TJwDAccessControlList;
begin
  Result := TJwDAccessControlList.Create;
  Result.Assign(Self);
  Result.RemoveExplicits;
end;

function TJwDAccessControlList.GetExplicit: TJwDAccessControlList;
begin
  Result := TJwDAccessControlList.Create;
  Result.Assign(Self);
  Result.RemoveInherited;
end;




constructor TJwSecurityAccessControlList.Create(OwnAceEntries: boolean);
begin
  inherited Create(OwnAceEntries);
  fRevision := ACL_REVISION;
end;

constructor TJwSecurityAccessControlList.Create;
begin
  inherited;
  fRevision := ACL_REVISION;
end;


constructor TJwSAccessControlList.Create;
begin
  inherited;
end;

constructor TJwSAccessControlList.Create(OwnAceEntries: boolean);
begin
  inherited Create(OwnAceEntries);
end;

constructor TJwSAccessControlList.Create(AclPointerList: PACL);
begin
  inherited Create(AclPointerList);
end;

function TJwSAccessControlList.GetItem(idx: integer): TJwAuditAccessControlEntry;
begin
  Result := TJwAuditAccessControlEntry(inherited GetItem(idx));
end;

function TJwSAccessControlList.GetExplicitAccessArray: TJwExplicitAccessArray;
begin
  Result := inherited GetExplicitAccessArray;
end;

function TJwSAccessControlList.GetMandatoryLabel : TJwSystemMandatoryAccessControlEntry;
var i : Integer;
begin
  result := nil;
  for i := 0 to Count-1 do
  begin
    if (Items[i].AceType = actMandatory) and
       (TJwSecurityAccessControlEntry(Items[i]) is TJwSystemMandatoryAccessControlEntry) then
    begin
      result := TJwSystemMandatoryAccessControlEntry(Items[i]);
      break;
    end;
  end;
end;

procedure TJwSAccessControlList.SetMandatoryLabel(
      const NewLabel : TJwSystemMandatoryAccessControlEntry;
      const CopyFlag: TJwCopyFlag);
{this const declaration prevents changing the type TJwCopyFlag
without notice of this implementation. The compiler will
show an error and source code must be adapted.
This code will be removed by the optimizer
}
const CopyFlagCheck : array[TJwCopyFlag] of byte = (0,1);
begin
  if (CopyFlag = cfCopyInstance) then
    SetMandatoryLabelEx(NewLabel)
  else
  begin
    SetMandatoryLabelEx(nil); //remove old label
    if Assigned(NewLabel) then
      Add(NewLabel);
  end;
end;

procedure TJwSAccessControlList.SetMandatoryLabelEx(
  const NewLabel : TJwSystemMandatoryAccessControlEntry);
var i : Integer;
begin
  for i := 0 to Count-1 do
  begin
    if (Items[i].AceType = actMandatory) and
       (TJwSecurityAccessControlEntry(Items[i]) is TJwSystemMandatoryAccessControlEntry) then
    begin
      Remove(Items[i]);
      break;
    end;
  end;

  if Assigned(NewLabel) then 
    Add(TJwSystemMandatoryAccessControlEntry.Create(
        TJwSecurityAccessControlEntry(NewLabel)));
end;

function TJwSAccessControlList.HasMandatoryLabel : boolean;
begin
  result := GetMandatoryLabel <> nil;
end;



constructor TJwSecurityAccessControlList.Create(AclPointerList: PACL);

var
  s: ACL_SIZE_INFORMATION;
  aPACE: PACE;
  i: integer;

begin
  Self.Create(True);

  if (AclPointerList = nil) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
      RsACLClassNilACLPointer,
      'Create(ownACEs: Boolean; aAPCL: PACL)', ClassName, RsUNAcl, 0, False, []);

  if not IsValidAcl(AclPointerList) then
    raise EJwsclWinCallFailedException.CreateFmtEx(RsACLClassInvalidACL,
      'Create(ownACEs: Boolean; aAPCL: PACL)', ClassName, RsUNAcl, 0, True, []);


  {if not GetAclInformation(aAPCL, @s, sizeof(s),AclSizeInformation) then
    raise EJwsclWinCallFailedException.CreateFmtEx('Call to GetAclInformation failed.', 'Create(ownACEs: Boolean; aAPCL: PACL)',ClassName,'JwsclAcl.pas', 0,True,[]);}

  fRevision := AclPointerList.AclRevision; //fastest way
  s.AceCount := AclPointerList.AceCount;

  for i := 0 to s.AceCount - 1 do
  begin
    aPACE := nil;

    if GetAce(AclPointerList, i, Pointer(aPACE)) then
    begin
      //keep order of original ACL
      Self.Insert(i, TJwSecurityAccessControlEntry.CreateACE(
        PAccessAllowedAce(aPACE)));
    end
    else
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsACLClassGetAceFailed,
        'Create(ownACEs: Boolean; aAPCL: PACL)',
        ClassName, RsUNAcl, 0, True, [i]);
  end;
end;



constructor TJwSecurityAccessControlList.Create(const Accesses : TJwExplicitAccessArray);
var ppACL : PACL;
begin
  JwRaiseOnNilParameter(
    Accesses, 'Accessess', 'Create', ClassName, RsUNAcl);

  if SetEntriesInAcl(1, @Accesses[0], nil, ppACL) <> ERROR_SUCCESS then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed,
        'Create',
        ClassName, RsUNAcl, 0, True, []);

  if ppACL = nil then
    Create
  else
    try
      Create(ppACL);
    finally
      LocalFree(HLOCAL(ppACL));
    end;
end;


procedure TJwSecurityAccessControlList.RemoveOwners;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].fListOwner = Self then
      Items[i].fListOwner := nil;
  end;
end;

procedure TJwSecurityAccessControlList.Clear;
begin
  RemoveOwners;
  inherited;
end;

destructor TJwSecurityAccessControlList.Destroy;
begin
  RemoveOwners;
  inherited;

end;

procedure TJwSecurityAccessControlList.Assign(AclInstance:
  TJwSecurityAccessControlList);
var
  i: integer;
  ACE: TJwSecurityAccessControlEntry;
begin
  if not Assigned(AclInstance) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Assign', ClassName, RsUNAcl,
      0, False, ['AclInstance']);

  Clear;
  fRevision := AclInstance.fRevision;

  for i := 0 to AclInstance.Count - 1 do
  begin
    ACE := TJwSecurityAccessControlEntry.CreateACE(AclInstance.Items[i].AceType);
    ACE.Assign(AclInstance.Items[i]);

    try
      Insert(i,ACE); //keep order of original ACL
    except
      //free the ACE it will not be published
      ACE.Free;
      raise;
    end;
  end;
end;


procedure TJwSecurityAccessControlList.AddACEs(
  AclInstance: TJwSecurityAccessControlList; KeepOriginalOrder : Boolean = true);
var
  i: integer;
  E: TJwSecurityAccessControlEntry;
begin
  if not Assigned(AclInstance) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Assign', ClassName, RsUNAcl,
      0, False, ['AclInstance']);

  if (Self.ClassType <> AclInstance.ClassType) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassClassMismatch,
      'AddACEs', ClassName, RsUNAcl, 0, False, [Self.ClassName,
      AclInstance.ClassName]);

  for i := 0 to AclInstance.Count - 1 do
  begin
    E := TJwSecurityAccessControlEntry.CreateACE(AclInstance[i].AceType);
    E.Assign(AclInstance[i]);

    //keep order of original ACL
    if KeepOriginalOrder then
      Self.Insert(i, E)
    else
      Self.Add(E);
  end;
end;


function TJwSecurityAccessControlList.Create_PACL_Deprecated: PACL;
var
  c, i: integer;
  //[Hint] aPSID: PSID;

  aAudit:  TJwAuditAccessControlEntry;
  Mandatory : TJwSystemMandatoryAccessControlEntry;
  bResult: boolean;

  iSize: Cardinal;
begin
  for i := 0 to Count - 1 do
  begin
    if not Assigned(Items[i].SID) or
      (Assigned(Items[i].SID) and (Items[i].SID.SID = nil)) then
      raise EJwsclInvalidSIDException.CreateFmtEx(
        RsACLClassNilSid,
        'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
  end;

  c := max(1, Count);

  //determining the size comes from http://msdn2.microsoft.com/en-US/library/aa378853.aspx
  iSize := sizeof(TACL) + (sizeof(ACCESS_ALLOWED_ACE) -
    sizeof(Cardinal)) * c;

  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
      try
        Inc(iSize, Items[i].SID.SIDLength); //can throw exception!
      except
        on E: EJwsclSecurityException do
        begin
          raise EJwsclInvalidSIDException.CreateFmtEx(
            RsACLClassNilSid, 'Create_PACL', ClassName, RsUNAcl,
            0, True, [i]);
        end;
      end;
  end;

  Result := PACL(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT, iSize));

  if Result = nil then
    raise EJwsclNotEnoughMemory.CreateFmtEx(
      RsACLClassNewAclNotEnoughMemory,
      'Create_PACL', ClassName, RsUNAcl, 0, True, []);

  // InitializeAcl(Result,GlobalSize(Cardinal(Result)),ACL_REVISION);
  InitializeAcl(Result, iSize, ACL_REVISION);

  //Add...ex functions only Windows 2000 or higher
  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
    begin
      if Items[i] is TJwDiscretionaryAccessControlEntryAllow then
        bResult := AddAccessAllowedAceEx(Result, ACL_REVISION,
          TJwEnumMap.ConvertAceFlags(
          Items[i].Flags), Items[i].AccessMask, Items[i].SID.SID)
      else
      if Items[i] is TJwDiscretionaryAccessControlEntryDeny then
        bResult := AddAccessDeniedAceEx(Result, ACL_REVISION,
          TJwEnumMap.ConvertAceFlags(
          Items[i].Flags), Items[i].AccessMask, Items[i].SID.SID)
      else
      if Items[i] is TJwAuditAccessControlEntry then
      begin
        aAudit  := (Items[i] as TJwAuditAccessControlEntry);
        bResult := AddAuditAccessAce(Result, ACL_REVISION,
          Items[i].AccessMask, Items[i].SID.SID,
          aAudit.AuditSuccess,
          aAudit.AuditFailure);
      end
      else
{$IFDEF VISTA}
      if Items[i] is TJwSystemMandatoryAccessControlEntry then
      begin
        Mandatory := (Items[i] as TJwSystemMandatoryAccessControlEntry);
        bResult := AddMandatoryAce(
              Result,//PACL pAcl,
              ACL_REVISION,//DWORD dwAceRevision,
              TJwEnumMap.ConvertAceFlags(Items[i].Flags),//DWORD AceFlags,
              Mandatory.AccessMask,//DWORD MandatoryPolicy,
              Items[i].SID.SID//PSID pLabelSid
            );
      end
      else
{$ENDIF}      
      begin //class is not supported
        GlobalFree(HRESULT(Result));

        raise EJwsclUnsupportedACE.CreateFmtEx(
          RsACLClassUnknownAccessAce,
          'Create_PACL', ClassName, RsUNAcl, 0, True, [Items[i].ClassName,i]);
      end;

      if not bResult then
      begin
        GlobalFree(HRESULT(Result));

        raise EJwsclFailedAddACE.CreateFmtEx(
          RsACLClassAddXAccessAceFailed,
          'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
      end;
    end;

  end;
end;


function TJwSecurityAccessControlList.Create_PACL: PACL;


  function AddAceToList(ACL : PACL;
    ACE : TJwSecurityAccessControlEntry;
    const Size : Cardinal) : Boolean;

  var ppACE : PACE;
      AceSize : Cardinal;
//      p : Pointer;
//      s : AnsiString;
  begin
    //result := RtlValidAcl(Acl);
    //result := RtlFirstFreeAce( Acl, p);
    {Warning:
      AddAce cannot add object ACEs properly so we use the special functions here
    }
    case ACE.AceType of
      actAuditObject :
       result := AddAuditAccessObjectAce(ACL, ACE.Revision,
          ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid,
          (ACE as TJwAuditAccessControlEntry).AuditSuccess,
          (ACE as TJwAuditAccessControlEntry).AuditFailure);


     actAllowObject :
        result := AddAccessAllowedObjectAce(ACL, ACE.Revision,
            ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid);

     actDenyObject  :
        result := AddAccessDeniedObjectAce(ACL,ACE.Revision,
            ACE.ObjectFlags, ACE.AccessMask, @ACE.ObjectType, @ACE.InheritedObjectType, ACE.SID.Sid);
    else
      ppACE := ACE.CreateDynamicACE(AceSize);


      result := AddAce(
          ACL,//__inout  PACL pAcl,
          ACE.Revision,//__in     DWORD dwAceRevision,
          MAXDWORD,//__in     DWORD dwStartingAceIndex,
          ppACE,//__in     LPVOID pAceList,
          AceSize//__in     DWORD nAceListLength
        );

      GlobalFree(HGLOBAL(ppACE));
//      s := ACe.ClassName;
    end;
  end;


var
  i: integer;
  bResult: boolean;
  iSize: Cardinal;
begin
  RaiseOnInvalidRevision(fRevision, 'Create_PACL', ClassName);

  for i := 0 to Count - 1 do
  begin
    if not Assigned(Items[i].SID) or
      (Assigned(Items[i].SID) and (Items[i].SID.SID = nil)) then
      raise EJwsclInvalidSIDException.CreateFmtEx(
        RsACLClassNilSid,
        'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
  end;

  iSize := sizeof(TACL); //header size for ACL
    //determining the size comes from http://msdn2.microsoft.com/en-US/library/aa378853.aspx


  for i := 0 to Count - 1 do
  begin
    //update ACL revision to highest ACE revision 
    if Items[i].Revision > Self.Revision then
      Self.Revision := Items[i].Revision;

    Inc(iSize, Items[i].GetDynamicTypeSize); //get size of ACE structure

    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
      try
        Inc(iSize, Items[i].SID.SIDLength); //can throw exception!
      except
        on E: EJwsclSecurityException do
        begin
          raise EJwsclInvalidSIDException.CreateFmtEx(
            RsACLClassNilSid, 'Create_PACL', ClassName, RsUNAcl,
            0, True, [i]);
        end;
      end;
  end;
  Result := PACL(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT, iSize));

  if Result = nil then
    raise EJwsclNotEnoughMemory.CreateFmtEx(
      RsACLClassNewAclNotEnoughMemory,
      'Create_PACL', ClassName, RsUNAcl, 0, True, []);



  if not InitializeAcl(Result, iSize, fRevision) then
  begin
    GlobalFree(HGLOBAL(Result));

    raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
           'Create_PACL', ClassName, RsUNAcl, 0, true,
           ['InitializeAcl']);
  end;

  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].SID) and (Items[i].SID.SID <> nil) then
    begin
      {//make sure AddAceToList does the same as this one:
      	bResult := AddAccessAllowedAceEx(result, Items[i].Revision, TJwEnumMap.ConvertAceFlags(
          Items[i].Flags), Items[i].AccessMask, Items[i].SID.Sid);}
      bResult := AddAceToList(Result, Items[i], iSize);

      if not bResult then
      begin
        GlobalFree(HGLOBAL(Result));

        raise EJwsclFailedAddACE.CreateFmtEx(
          RsACLClassAddXAccessAceFailed,
          'Create_PACL', ClassName, RsUNAcl, 0, True, [i]);
      end;
    end;

  end;
end;

class procedure TJwSecurityAccessControlList.Free_PACL(var AclPointerList: PACL);
begin
  if AclPointerList = nil then
    Exit;

  GlobalFree(HGLOBAL(AclPointerList));

  AclPointerList := nil;
end;


function TJwSecurityAccessControlList.GetItem(idx: integer):
TJwSecurityAccessControlEntry;
begin
  Result := inherited Get(idx);
end;

function TJwSecurityAccessControlList.GetEffectiveRights(const User :
       {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF}) : TJwAccessMask;
var ACL : PACL;
begin
  result := 0;

  ACL := Self.Create_PACL;
  try
    if {$IFDEF UNICODE}GetEffectiveRightsFromAclW{$ELSE}GetEffectiveRightsFromAclA{$ENDIF}
      (ACL,//__in   PACL pacl,
      @User,//__in   PTRUSTEE pTrustee,
      result//__out  PACCESS_MASK pAccessRights
      ) <> ERROR_SUCCESS then
    begin
       raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed,
           'GetEffectiveRights', ClassName, RsUNAcl, 0, true,
           ['GetEffectiveRightsFromAcl']);
    end;
  finally
    Free_PACL(ACL);
  end;
end;

function TJwSecurityAccessControlList.GetEffectiveRights
  (const User : TJwSecurityId) : TJwAccessMask;
var Trust : {$IFDEF UNICODE}TTrusteeW{$ELSE}TTrusteeA{$ENDIF};
begin
  ZeroMemory(@Trust, sizeof(Trust));

  if not Assigned(User) then
    {$IFDEF UNICODE}BuildTrusteeWithNameW{$ELSE}BuildTrusteeWithNameA{$ENDIF}
     (@Trust, 'CURRENT_USER')
  else
  {$IFDEF UNICODE}BuildTrusteeWithSidW{$ELSE}BuildTrusteeWithSidA{$ENDIF}
   (@Trust, User.Sid);

  result := GetEffectiveRights(Trust);
end;

function TJwSecurityAccessControlList.Add(AccessEntry:
  TJwSecurityAccessControlEntry): integer;

procedure UpdateACLRevision;
begin
  //update ACL revision to the newer ACE revision
  //ACE structure does not change during revisions
  //so it is safe
  if AccessEntry.Revision > Self.fRevision then
    Self.fRevision := AccessEntry.Revision;
end;

var
  i:  integer;
  bInserted: boolean;
  b1,
  b3: boolean;
begin
  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Add', ClassName, RsUNAcl, 0, False, ['AObject']);

  i := IndexOf(AccessEntry);
  if (i >= 0) or (Assigned(AccessEntry.ListOwner)) then

    raise EJwsclDuplicateListEntryException.CreateFmtEx(
      RsACLClassAceAlreadyInList, 'Add', ClassName, RsUNAcl,
      0, False, []);

  if ((Self is TJwDAccessControlList) and not
    (AccessEntry is TJwDiscretionaryAccessControlEntry)) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAceInDacl,
      'Add', ClassName, RsUNAcl, 0, False, [AccessEntry.ClassName]);


  b1 := (Self is TJwSAccessControlList);



  //  b2 := (Self is TJwSecurityAccessControlList);
  //[Hint] b2 := True;

  b3 := (AccessEntry is TJwAuditAccessControlEntry) or
        (AccessEntry is TJwSystemMandatoryAccessControlEntry);
  {If this list is a SACL the entry must be an audit ace or
   a mandatory ace.}
  if ((b1) and (not b3)) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvaldAceInSacl,
      'Add', ClassName, RsUNAcl, 0, False, []);


  if (Self is TJwSAccessControlList) then
  begin
    AccessEntry.fListOwner := Self;
    Result := inherited Add(AccessEntry);
    //do not call our Self Add -> otherwise we get a recursion

    UpdateACLRevision;
    Exit;
  end;

  Result := -1;

  if (Self is TJwDAccessControlList) and
    ((AccessEntry is TJwDiscretionaryAccessControlEntryDeny) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackDeny) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackObjectDeny)
     )
    then
  begin
    if not (afInheritedAce in AccessEntry.Flags) then
    begin
      //a safe way to add a deny ACE is to add it directly at the beginning of list
      inherited Insert(0, AccessEntry);
      //do not call our Self Insert -> otherwise we get a recursion
      AccessEntry.fListOwner := Self;
      Result := 0;

      UpdateACLRevision;
      Exit;
    end;

    bInserted := False;
    for i := 0 to Count - 1 do
    begin
      if (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i, AccessEntry);
        //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        bInserted := True;
        AccessEntry.fListOwner := Self;

        UpdateACLRevision;
        break;
      end;
    end;

    if not bInserted then
    begin
      AccessEntry.fListOwner := Self;

      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion

      UpdateACLRevision;
    end;

    Exit;
  end;

  if (Self is TJwDAccessControlList) and
    ((AccessEntry is TJwDiscretionaryAccessControlEntryAllow) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackAllow) or
     (AccessEntry is TJwDiscretionaryAccessControlEntryCallbackObjectAllow)
     )
    then
  begin
    if (afInheritedAce in AccessEntry.Flags) then
    begin
      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion
      AccessEntry.fListOwner := Self;

      UpdateACLRevision;
      Exit;
    end;

    bInserted := False;
    for i := 0 to Count - 1 do
    begin
      if (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i, AccessEntry);
        //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        //[Hint] bInserted := True;
        AccessEntry.fListOwner := Self;

        UpdateACLRevision;
        Exit;
      end;
    end;

    if not bInserted then
    begin
      AccessEntry.fListOwner := Self;
      Result := inherited Add(AccessEntry);
      //do not call our Self Add -> otherwise we get a recursion

      UpdateACLRevision;
    end;

    Exit;
  end;


  if (Result = -1) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAce,
      'Add', ClassName, RsUNAcl, 0, False, []);

 (*
  if (Self is TJwDAccessControlList) and (AObject is TJwDiscretionaryAccessControlEntryDeny) then
  begin
    //a safe way to add a deny ACE is to add it directly at the beginning of list
    inherited Insert(0,AObject); //do not call our Self Insert -> otherwise we get a recursion
    Result := 0;
  end
  else
  if (Self is TJwDAccessControlList) and (AObject is TJwDiscretionaryAccessControlEntryAllow) then
  begin
    bInserted := False;
    for i := 0 to Count -1 do
    begin
      if (Items[i] is TJwDiscretionaryAccessControlEntryAllow) and
        (afInheritedAce in Items[i].Flags) then
      begin
        inherited Insert(i,aObject); //do not call our Self Insert -> otherwise we get a recursion
        Result := i;
        bInserted := True;
      end;
    end;
    if not bInserted then
      Result := inherited Add(aObject);//do not call our Self Add -> otherwise we get a recursion
  end
  else
    Result := inherited Add(AObject); //do not call our Self Add -> otherwise we get a recursion
   *)

end;

function TJwSecurityAccessControlList.GetText: TJwString;
begin
  Result := GetTextMap(TJwSecurityGenericMapping);
end;

procedure TJwSecurityAccessControlList.SetRevision(const Revision : Cardinal);
begin
  RaiseOnInvalidRevision(Revision, 'SetRevision', ClassName);
  fRevision := Revision;
end;


function TJwSecurityAccessControlList.GetTextMap(
  const Mapping: TJwSecurityGenericMappingClass): TJwString;
var
  i: integer;
  anACE: TJwSecurityAccessControlEntry;
begin
  Result := '';

  if Self is TJwDAccessControlList then
    Result := RsACLClassDaclName;

  if Self is TJwSAccessControlList then
    Result := RsACLClassSaclName;

  Result := JwFormatString(RsACLClassAceCount,[Count]);


  for i := 0 to Count - 1 do
  begin
    anACE := Items[i];

    Result := Result + #13#10 + '#' + IntToStr(i) +
      anACE.GetTextMap(Mapping) + #13#10;
  end;
end;



procedure TJwSecurityAccessControlList.ConvertInheritedToExplicit;
var
  i: integer;
  x: TJwAceFlags;
begin
  for i := 0 to Count - 1 do
  begin
    x := Items[i].Flags;
    Exclude(x, afInheritedAce);
    Items[i].Flags := x;
  end;
end;

procedure TJwSecurityAccessControlList.RemoveExplicits;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if not (afInheritedAce in Items[i].Flags) then
      Remove(i);
  end;
end;

procedure TJwSecurityAccessControlList.RemoveInherited;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if afInheritedAce in Items[i].Flags then
      Remove(i);
  end;
end;


function TJwSecurityAccessControlList.First: TJwSecurityAccessControlEntry;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TJwSecurityAccessControlEntry(inherited First);
end;

function TJwSecurityAccessControlList.IndexOf(AccessEntry:
  TJwSecurityAccessControlEntry): integer;
begin
  Result := inherited IndexOf(AccessEntry);
end;

procedure TJwSecurityAccessControlList.Insert(Index: integer;
  AccessEntry: TJwSecurityAccessControlEntry);
begin
  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Insert', ClassName, RsUNAcl, 0, False, ['AccessEntry']);

  if (IndexOf(AccessEntry) >= 0) or (Assigned(AccessEntry.ListOwner)) then
    raise EJwsclDuplicateListEntryException.CreateFmtEx(
      RsACLClassAceAlreadyInList, 'Insert', ClassName, RsUNAcl,
      0, False, []);

  if ((Self is TJwDAccessControlList) and not
    (AccessEntry is TJwDiscretionaryAccessControlEntry)
    ) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvalidAceInDacl,
      'Add', ClassName, RsUNAcl, 0, False, [AccessEntry.ClassName]);

  if ((Self is TJwSAccessControlList) and not
    (
     (AccessEntry is TJwAuditAccessControlEntry) or
     (AccessEntry is TJwAuditAccessControlEntryObject) or
     (AccessEntry is TJwAuditAccessControlEntryCallback) or
     (AccessEntry is TJwAuditAccessControlEntryCallbackObject) or
     (AccessEntry is TJwSystemMandatoryAccessControlEntry)
    )) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassInvaldAceInSacl,
      'Add', ClassName, RsUNAcl, 0, False, []);


  inherited Insert(Index, AccessEntry);

  AccessEntry.fListOwner := Self;

  //update ACL revision to the newer ACE revision
  //ACE structure does not change during revisions
  //so it is safe
  if AccessEntry.Revision > Self.fRevision then
    Self.fRevision := AccessEntry.Revision;

end;


function TJwSecurityAccessControlList.FindEqualACE(
  const AccessEntry: TJwSecurityAccessControlEntry;
  EqualAceTypeSet: TJwEqualAceTypeSet;
  const StartIndex: integer = -1;
  //new
  const Inclusion : TJwInclusionFlags = [ifInherited, ifExplicit, ifContainer, ifLeaf];
  const Exclusion : TJwExclusionFlags = [];
  const Reverse : Boolean = false): integer;
var
  i: integer;
  ACEi: TJwSecurityAccessControlEntry;
  B: boolean;
begin
  Result := -1;

  if Reverse then
    i := Count -1
  else
    i := StartIndex + 1;

  while (Reverse and (i > StartIndex) or
        (not Reverse and (i < Count))) do
  begin
    try
      ACEi := GetItem(i);
    except
      raise;
    end;

    B := True;

    if (eactSameSid in EqualAceTypeSet) then
      B := B and ACEi.SID.EqualSid(AccessEntry.SID);

    if (eactSameFlags in EqualAceTypeSet) then
    begin
      if (eactGEFlags in EqualAceTypeSet) then
        B := B and (ACEi.Flags >= AccessEntry.Flags)
      else if (eactSEFlags in EqualAceTypeSet) then
        B := B and (ACEi.Flags <= AccessEntry.Flags)
      else
        B := B and (ACEi.Flags = AccessEntry.Flags);
    end;

    if (eactSameAccessMask in EqualAceTypeSet) then
    begin
      {Instead of 100% equality we just check whether
       AccessEntry.AccessMask is part of ACEi.AccessMask.

       SE:                        TRUE   FALSE
       ACEi.AccessMask        = 100101   010101
       AccessEntry.AccessMask = 000101   110111
                                ---------------
                                000101   010101
       So if the and operation returns the same flags as AccessEntry.AccessMask contains,
       we have a true result.

      }

      //and the other way around
      if (eactSEAccessMask in EqualAceTypeSet) then
      begin
        //B := B and (ACEi.AccessMask and AccessEntry.AccessMask = AccessEntry.AccessMask) // Untermenge Element Obermenge
        //           Obermenge           Untermenge
        B := B and ((ACEi.AccessMask and AccessEntry.AccessMask) = ACEi.AccessMask); // Obermenge Element Untermenge
(*
{$IFDEF JWSCL_DEBUG_INFO}
        OutputDebugStringA(PAnsiChar(AnsiString(Format('Compating: Acei:AE: %s:%s = %s',
          [#13#10+JwAccesMaskToBits(ACEi.AccessMask),#13#10+JwAccesMaskToBits(AccessEntry.AccessMask), BoolToStr(B,true)]))));
{$ENDIF JWSCL_DEBUG_INFO}*)
      end
      else
        B := B and (ACEi.AccessMask = AccessEntry.AccessMask);
    end;

    if (eactSameType in EqualAceTypeSet) then
      B := B and (ACEi.AceType = AccessEntry.AceType);

    if B and (ifContainer in Inclusion) then
      B := B and (afContainerInheritAce in ACEi.Flags);

    if B and (ifLeaf in Inclusion) then
      B := B and (afObjectInheritAce in ACEi.Flags);

    if B then
    begin
      Result := i;
      Exit;
    end;

    if Reverse then
      Dec(i)
    else
      Inc(i);
  end;
end;

function TJwSecurityAccessControlList.FindSID(const SidInstance: TJwSecurityId;
  const StartIndex: integer = -1): integer;
var
  i: integer;
  ACE: TJwSecurityAccessControlEntry;
begin
  Result := -1;
  for i := StartIndex + 1 to Count - 1 do
  begin
    try
      ACE := GetItem(i);
      if Assigned(ACE) and ACE.SID.EqualSid(SidInstance) then
      begin
        Result := i;
        Exit;
      end;

    except
      on E: EListError do
        Exit; //exit with return -1
    end;
  end;
end;

function TJwSecurityAccessControlList.Last: TJwSecurityAccessControlEntry;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TJwSecurityAccessControlEntry(inherited Last);
end;

procedure TJwSecurityAccessControlList.Delete(const Index: integer);
begin
  Remove(Index);
end;

function TJwSecurityAccessControlList.Remove(const Index: integer): integer;
var
  P: TJwSecurityAccessControlEntry;
begin
  P := GetItem(Index);

  Result := Remove(P);
end;

function TJwSecurityAccessControlList.Remove(const AccessEntry:
  TJwSecurityAccessControlEntry): integer;
var
  idx: integer;
  b: boolean;
begin
  if not Assigned(AccessEntry) then
    raise EJwsclNILParameterException.CreateFmtEx(RsACLClassNilParameter,
      'Insert', ClassName, RsUNAcl, 0, False, ['AObject']);

  idx := IndexOf(AccessEntry);
  b := AccessEntry.ListOwner = Self;
  if (idx < 0) or not b then
  begin
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsACLClassAceNotInList, 'Remove', ClassName, RsUNAcl,
      0, False, []);
    Exit;
  end;

  //set owner to nil so it can be removed safely without an exception  
  AccessEntry.fListOwner := nil;

  Result := inherited Remove(AccessEntry);
end;


function TJwSecurityAccessControlList.GetExplicitAccessArray:
TJwExplicitAccessArray;
var
  i: integer;
  iErr, iCount: Cardinal;
  pExpAccess: PEXPLICIT_ACCESS;
  pSACL: PACL;
  arr: TJwExplicitAccessArray;

begin
  Result := nil;
  arr := nil;

  pSACL := Create_PACL;

  try
    pExpAccess := nil;
    iCount := 0;
    iErr := GetExplicitEntriesFromAcl(jwaWindows_PACL(pSACL),
      iCount, pExpAccess);
    if iErr <> ERROR_SUCCESS then
      raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailedWithNTStatus,
         'GetExplicitAccessArray', ClassName, RsUNAcl, 0, False,
         ['GetExplicitEntriesFromAcl',iErr]);

    if iCount > 0 then
    begin
      arr := TJwExplicitAccessArray(pExpAccess);

      SetLength(Result, iCount);
      for i := 0 to iCount - 1 do
      begin
        Result[i] := arr[i];
      end;
    end;

  finally
    Free_PACL(pSACL);
  end;

 { if pExpAccess <> nil then
    LocalFree(HLOCAL(pExpAccess));
  }

end;


constructor TJwSecurityAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList;
  const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask;
  const aSID: TJwSecurityId;
  ownSID: boolean = True);
begin
  JwRaiseOnNilParameter(aSid,'aSID','Create',ClassName,RsUNACL);
  
  fListOwner := nil;

  fFlags := aFlags;
  fAccessMask := anAccessMask;
  fSID := aSID;
  fRevision := ACL_REVISION;

  if Assigned(aSID) and (aSID.IsStandardSID) then
    ownSID := False;

  fownSID := ownSID;
  fIgnore := False;

  if Assigned(aListOwner) then
    aListOwner.Add(Self);

  fListOwner := aListOwner;

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;

constructor TJwSecurityAccessControlEntry.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  Self.Create(ListOwner, Flags, AccessMask, SID, ownSID);
  fRevision := Revision;
end;

constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntry: TJwSecurityAccessControlEntry);
begin
  inherited Create;
  Assign(AccessEntry);
  ownSid := True;

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;


procedure TJwSecurityAccessControlEntry.Assign(AccessEntry:
  TJwSecurityAccessControlEntry);
var
  S: TJwSecurityId;
begin
  JwRaiseOnNilParameter(AccessEntry,'AccessEntry','Assign',ClassName,RsUNACL);
  JwRaiseOnNilParameter(AccessEntry.SID,'AccessEntry.SID','Assign',ClassName,RsUNACL);

  //free SID automatically
  OwnSID := True;

  //make a copy of SID for our entry
  if Assigned(AccessEntry.SID) then
  begin
    if (AccessEntry.SID.IsStandardSID) then
    begin
      //do not duplicate a well known sid from unit JwsclKnownSid
      S := AccessEntry.SID;
      ownSID := False;
    end
    else
      S := TJwSecurityId.Create(AccessEntry.SID)
  end
  else
    S := nil;

  fSID := S;

  begin
    (*
    do not change fListOwner
    because this ace instance could already be added
    *)
    fFlags := AccessEntry.Flags;
    fAccessMask := AccessEntry.AccessMask;
    fHeader := AccessEntry.Header;
    fRevision := AccessEntry.fRevision;
    fObjectType := AccessEntry.fObjectType;
    fInheritedObjectType := AccessEntry.fInheritedObjectType;
  end;
end;


constructor TJwSecurityAccessControlEntry.Create;
begin
  fListOwner := nil;
  fFlags := [];
  fAccessMask := 0;
  fSID := nil;
  fownSID := False;
  fUserData := nil;
  fRevision := ACL_REVISION;
  ZeroMemory(@fHeader, sizeof(fHeader));

  ObjectType    := NULL_GUID;
  InheritedObjectType := NULL_GUID;
end;


destructor TJwSecurityAccessControlEntry.Destroy;
begin
  if Assigned(ListOwner) then
  begin
    raise EJwsclReadOnlyPropertyException.CreateFmtEx(
      RsACLClassAceRemovingDenied,
      'Destroy', ClassName, RsUNAcl, 0, False, []);
  end;

  if fownSID and Assigned(fSID)
    and not fSID.IsStandardSID then
    FreeAndNil(fSID);

  fUserData := nil;


  inherited;
end;

procedure TJwSecurityAccessControlEntry.Free;
var
  B: boolean;
begin
  B := False;
  if Assigned(ListOwner) then
  begin
    if ListOwner.OwnsObjects then
      Exit;
    try
      ListOwner.Remove(Self);
    except
      //not correct list
      on E: EJwsclInvalidACEException do
      begin
        ListOwner.OwnsObjects := B;
        raise;
      end;
    end;

  end;

  inherited;
end;

class function TJwSecurityAccessControlEntry.CreateACE(anAceType: TJwAceType):
TJwSecurityAccessControlEntry;
begin
  //Result := nil;
  case anACEType of
    actAllow         : Result := TJwDiscretionaryAccessControlEntryAllow.Create;
    actAllowCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackAllow.Create;
    actAllowObject :
      Result := TJwDiscretionaryAccessControlEntryObjectAllow.Create;
    actAllowCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectAllow.Create;

    actDeny          : Result  := TJwDiscretionaryAccessControlEntryDeny.Create;
    actDenyCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackDeny.Create;
    actDenyObject :
      Result := TJwDiscretionaryAccessControlEntryObjectDeny.Create;
    actDenyCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectDeny.Create;

    actAudit : Result := TJwAuditAccessControlEntry.Create;
    actAuditCallback  :
      Result := TJwAuditAccessControlEntryCallback.Create;
    actAuditObject  :
      Result := TJwAuditAccessControlEntryObject.Create;
    actAuditCallbackObject  :
      Result := TJwAuditAccessControlEntryCallbackObject.Create;

    actMandatory     : Result := TJwSystemMandatoryAccessControlEntry.Create;
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'CreateACE', ClassName, RsUNAcl,
      0, False, []);
  end;
end;

class function TJwSecurityAccessControlEntry.
  GetClassAceType(const AceType: TJwAceType) : TJwSecurityAccessControlEntryClass;
begin
  case AceType of
    actAllow :
      result := TJwDiscretionaryAccessControlEntryAllow;
    actAllowCallback:
      Result := TJwDiscretionaryAccessControlEntryCallbackAllow;
    actAllowObject:
      Result := TJwDiscretionaryAccessControlEntryObjectAllow;
    actAllowCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectAllow;


    actDeny :
      Result := TJwDiscretionaryAccessControlEntryDeny;
    actDenyCallback :
      Result := TJwDiscretionaryAccessControlEntryCallbackDeny;
    actDenyObject:
      Result := TJwDiscretionaryAccessControlEntryObjectDeny;
    actDenyCallbackObject :
      Result := TJwDiscretionaryAccessControlEntryCallbackObjectDeny;

    actAudit :
      Result := TJwAuditAccessControlEntry;
    actAuditCallback :
      Result := TJwAuditAccessControlEntryCallback;
    actAuditObject:
      Result := TJwAuditAccessControlEntryObject;
    actAuditCallbackObject :
      Result := TJwAuditAccessControlEntryCallbackObject;


    actMandatory :
      Result := TJwSystemMandatoryAccessControlEntry;
  else
   raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'GetClassAceType', ClassName, RsUNAcl,
      0, False, []);
  end;
end;


constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntryPointer: PAccessAllowedAce);

begin
  if not Assigned(AccessEntryPointer) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'Create', ClassName, RsUNAcl,
      0, False, ['accessACE']);

  if (Self.ClassType <>
    GetClassAceType(TJwEnumMap.ConvertAceType(AccessEntryPointer.Header.AceType))) then
   raise EJwsclInvalidAceMismatch.CreateFmtEx(
      RsACEMismatch, 'Create', ClassName, RsUNAcl,
      0, False, []);

  Self.Create;
  fFlags := TJwEnumMap.ConvertAceFlags(AccessEntryPointer.Header.AceFlags);
  fAccessMask := AccessEntryPointer.Mask;
  fRevision := ACL_REVISION;
  fHeader := AccessEntryPointer.Header;


  OwnSID := True;
  fSID := TJwSecurityId.Create(PSID(@AccessEntryPointer.SidStart));

  //get other data
  case AccessEntryPointer.Header.AceType of
    SYSTEM_AUDIT_OBJECT_ACE_TYPE,
    SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE,
    ACCESS_ALLOWED_OBJECT_ACE_TYPE,
    ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE,
    ACCESS_DENIED_CALLBACK_ACE_TYPE,
    ACCESS_DENIED_OBJECT_ACE_TYPE,
    ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE :
      begin
        fObjectType :=
          PACCESS_ALLOWED_OBJECT_ACE(AccessEntryPointer).ObjectType;
        fInheritedObjectType :=
          PACCESS_ALLOWED_OBJECT_ACE(AccessEntryPointer).InheritedObjectType;
      end;
  end;

  fUserData := nil;
end;

constructor TJwSecurityAccessControlEntry.Create(
  const AccessEntryPointer: PAccessDeniedAce);
begin
  Self.Create(PAccessAllowedAce(AccessEntryPointer));
end;

class function TJwSecurityAccessControlEntry.CreateACE(
const  AccessEntryPointer: PAccessAllowedAce): TJwSecurityAccessControlEntry;
begin
  if not Assigned(AccessEntryPointer) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsACLClassNilParameter, 'CreateACE', ClassName, RsUNAcl,
      0, False, ['accessACE']);

  {if AccessEntryPointer.Header.AceType = ACCESS_ALLOWED_ACE_TYPE then
    Result := TJwDiscretionaryAccessControlEntryAllow.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_ALLOWED_CALLBACK_ACE_TYPE then
    result := TJwDiscretionaryAccessControlEntryCallbackAllow.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_DENIED_ACE_TYPE then
    Result := TJwDiscretionaryAccessControlEntryDeny.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = ACCESS_DENIED_CALLBACK_ACE_TYPE then
    result := TJwDiscretionaryAccessControlEntryCallbackDeny.Create(AccessEntryPointer)
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_AUDIT_ACE_TYPE then
    Result := TJwAuditAccessControlEntry.Create(PSystemAuditAce(AccessEntryPointer))
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_AUDIT_CALLBACK_ACE_TYPE then
    Result := TJwAuditAccessControlEntryCallback.Create(PSystemAuditAce(AccessEntryPointer))
  else
  if AccessEntryPointer.Header.AceType = SYSTEM_MANDATORY_LABEL_ACE_TYPE then
    Result := TJwSystemMandatoryAccessControlEntry.Create(
        (AccessEntryPointer))
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'CreateACE', ClassName, RsUNAcl,
      0, False, []);   }


  result := GetClassAceType(TJwEnumMap.ConvertAceType(AccessEntryPointer.Header.AceType))
       .Create(AccessEntryPointer);

end;

class function TJwSecurityAccessControlEntry.CreateACE(
const  AccessEntryPointer: PAccessDeniedAce): TJwSecurityAccessControlEntry;
begin
  Result := CreateACE(PAccessAllowedAce(AccessEntryPointer));
end;

function TJwSecurityAccessControlEntry.GetDynamicTypeSize : Cardinal;
begin
  case GetAceType of
    actAudit         : result := sizeof(SYSTEM_AUDIT_ACE);
    actAuditCallback : result := sizeof(SYSTEM_AUDIT_CALLBACK_ACE);
    actAuditObject   : result := sizeof(SYSTEM_AUDIT_OBJECT_ACE);
    actAuditCallbackObject : result := sizeof(SYSTEM_AUDIT_CALLBACK_OBJECT_ACE);

    actMandatory : result := sizeof(SYSTEM_MANDATORY_LABEL_ACE);

    actAllow : result := sizeof(ACCESS_ALLOWED_ACE);
    actAllowCallback : result := sizeof(ACCESS_ALLOWED_CALLBACK_ACE);
    actAllowObject : result := sizeof(ACCESS_ALLOWED_OBJECT_ACE);
    actAllowCallbackObject : result := sizeof(ACCESS_ALLOWED_CALLBACK_OBJECT_ACE);

    actDeny : result := sizeof(ACCESS_DENIED_ACE);
    actDenyCallback : result := sizeof(ACCESS_DENIED_CALLBACK_ACE);
    actDenyObject : result := sizeof(ACCESS_DENIED_OBJECT_ACE);
    actDenyCallbackObject : result := sizeof(ACCESS_DENIED_CALLBACK_OBJECT_ACE);
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsInvalidAceType,
      'GetDynamicTypeSize', ClassName, RsUNAcl, 0, False, []);
  end;

  {The correct size of a ACE header does not include the
   SidStart (DWORD) member of the ACE type. The SidStart member is only a placeholder
   for a sid structure that is placed behind the ACE header. 
  }
  Dec(Result, sizeof(DWORD));
end;                                

function TJwSecurityAccessControlEntry.CreateDynamicACE(out Size : Cardinal) : Pointer;

 procedure SetSidStart(var SidStart : Cardinal);
 var aPSID: PSID;
 begin
   if Assigned(SID) then
   begin
     aPSID := SID.CreateCopyOfSID;
     if aPSID <> nil then
     begin
       CopyMemory(@SidStart, aPSID, SID.SIDLength);
       SID.FreeSID(aPSID);
     end;
   end;
 end;

 function CompareGUID(const G1, G2: TGUID): boolean;
 begin
   Result := CompareMem(@G1, @G2, Sizeof(TGUID));
 end;


var p1 : PACCESS_ALLOWED_CALLBACK_ACE;
    //p2 : PSYSTEM_MANDATORY_LABEL_ACE;
    p3 : PACCESS_DENIED_OBJECT_ACE;
    //p4 : PACCESS_ALLOWED_CALLBACK_OBJECT_ACE;

    AceType : TJwAceType;
{$IFDEF JWSCL_DEBUG_INFO}
{
type TB = array[0..27] of byte;
var
    Data : ^TB;
}
{$ENDIF JWSCL_DEBUG_INFO}

begin
  Size := GetDynamicTypeSize;


  if Assigned(SID) and (SID.SID <> nil) then
    Inc(Size, SID.SIDLength);

  Result := Pointer(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT, Size));


{$IFDEF JWSCL_DEBUG_INFO}
//  Data := result;  //get the contents in this way
{$ENDIF JWSCL_DEBUG_INFO}

  PACCESS_ALLOWED_ACE(result).Header.AceType
    := TJwEnumMap.ConvertAceType(Self.AceType);

  PACCESS_ALLOWED_ACE(result).Header.AceFlags
    := TJwEnumMap.ConvertAceFlags(Self.Flags);

  PACCESS_ALLOWED_ACE(result).Header.AceSize
    := Size;



  AceType := GetAceType;
  case AceType of
    actMandatory,

    actAudit,
    actAuditCallback,

    actAllow,
    actAllowCallback,

    actDeny,
    actDenyCallback:
    begin
      p1 := result;

      p1.Mask := AccessMask;
      SetSidStart(p1.SidStart);
    end;

    actAuditObject,
    actAuditCallbackObject,

    actAllowObject,
    actAllowCallbackObject,
    actDenyObject,
    actDenyCallbackObject:
    begin
      p3 := result;

      p3.Mask := AccessMask;

      p3.Flags := 0;
      if CompareGUID(NULL_GUID, Self.ObjectType) then
        p3.Flags := p3.Flags or ACE_OBJECT_TYPE_PRESENT;
      if CompareGUID(NULL_GUID, Self.InheritedObjectType) then
        p3.Flags := p3.Flags or ACE_INHERITED_OBJECT_TYPE_PRESENT;

      p3.ObjectType := Self.ObjectType;
      p3.InheritedObjectType := Self.InheritedObjectType;

      SetSidStart(p3.SidStart);
    end;
  end;
end;

function TJwSecurityAccessControlEntry.Create_AllowACE: PAccessAllowedAce;
var
  //aPSID: PSID;
  Size : Cardinal;
begin
  //only allow return structures that are compatible to result type
  if
  not
  (
  (Self is TJwDiscretionaryAccessControlEntryCallbackAllow) or
  (Self is TJwDiscretionaryAccessControlEntryAllow) or
  (Self is TJwDiscretionaryAccessControlEntryCallbackDeny) or
  (Self is TJwDiscretionaryAccessControlEntryDeny) or
  (Self is TJwAuditAccessControlEntryCallback) or
  (Self is TJwAuditAccessControlEntry) or
  (Self is TJwSystemMandatoryAccessControlEntry)
  ) then
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'Create_XXXXXACE', ClassName, RsUNAcl,
      0, False, []);

  result := CreateDynamicACE(Size);
end;



function TJwSecurityAccessControlEntry.Create_DenyACE: PAccessDeniedAce;
begin
  Result := PAccessDeniedAce(Create_AllowACE);
end;

procedure TJwSecurityAccessControlEntry.Free_PACE(var AccessEntryPointer: PAccessAllowedAce);
begin
  Free_PACE(PAccessDeniedAce(AccessEntryPointer));
end;

procedure TJwSecurityAccessControlEntry.Free_PACE(var AccessEntryPointer: PAccessDeniedAce);
begin
  if AccessEntryPointer <> nil then
    GlobalFree(HGLOBAL(AccessEntryPointer));
  AccessEntryPointer := nil;
end;

procedure TJwSecurityAccessControlEntry.CheckReadOnly;
begin
  if Assigned(fListOwner) then
    raise EJwsclReadOnlyPropertyException.CreateFmtEx(RsACLClassPropertyReadOnly,
      'SetListOwner', ClassName, RsUNAcl, 0, False, []);
end;

procedure TJwSecurityAccessControlEntry.SetListOwner(ListOwner:
  TJwSecurityAccessControlList);
begin
  CheckReadOnly;

  if Assigned(ListOwner) then
  begin
    ListOwner.Add(Self);
  end;

  fListOwner := ListOwner;
end;

procedure TJwSecurityAccessControlEntry.SetFlags(FlagSet: TJwAceFlags);
begin
  fFlags := FlagSet;
end;

function TJwSecurityAccessControlEntry.GetAccessMask : TJwAccessMask;
begin
  result := fAccessMask;
end;

procedure TJwSecurityAccessControlEntry.SetAccessMask(
  anAccessMask: TJwAccessMask);
begin

  fAccessMask := anAccessMask;
end;

procedure TJwSecurityAccessControlEntry.SetSID(SidInstance: TJwSecurityId);
begin
  fSID := SidInstance;
end;




{ TJwDiscretionaryAccessControlEntryDeny }

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  inherited Create(ListOwner, Flags, AccessMask, SID, Revision, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  const AccessEntryPointer: PAccessDeniedAce);
begin
  inherited Create(AccessEntryPointer);
end;

constructor TJwDiscretionaryAccessControlEntryDeny.Create(
  AccessEntry: TJwDiscretionaryAccessControlEntryDeny);
begin
  inherited Create(AccessEntry);
end;

function TJwDiscretionaryAccessControlEntryDeny.Create_DenyACE:
PAccessDeniedAce;
begin
  Result := inherited Create_DenyACE;
end;

procedure TJwDiscretionaryAccessControlEntryDeny.Free_PACE(
  var AccessEntryPointer: PAccessDeniedAce);
begin
  inherited Free_PACE(AccessEntryPointer);
end;


{ TJwDiscretionaryAccessControlEntryAllow }

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
      const ListOwner: TJwSecurityAccessControlList;
      const Flags: TJwAceFlags;
      const AccessMask: TJwAccessMask;
      const SID: TJwSecurityId;
      const Revision : Cardinal;
      ownSID: boolean = True);
begin
  Self.Create(ListOwner, Flags, AccessMask, SID, ownSID);
  fRevision := Revision;
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const AccessEntryPointer: PAccessAllowedAce);
begin
  inherited Create(AccessEntryPointer);
end;

constructor TJwDiscretionaryAccessControlEntryAllow.Create(
  const AccessEntry: TJwDiscretionaryAccessControlEntryAllow);
begin
  inherited Create(AccessEntry);
end;

function TJwDiscretionaryAccessControlEntryAllow.Create_AllowACE:
PAccessAllowedAce;
begin
  Result := inherited Create_AllowACE;
end;

procedure TJwDiscretionaryAccessControlEntryAllow.Free_PACE(
  var AccessEntryPointer: PAccessAllowedAce);
begin
  inherited Free_PACE(AccessEntryPointer);
end;

{ TJwAuditAccessControlEntry }

constructor TJwAuditAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList; const aFlags: TJwAceFlags;
  const anAccessMask: TJwAccessMask; const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, aFlags, anAccessMask, aSID, ownSID);
  AuditSuccess := (afSuccessfulAccessAceFlag in aFlags);
  AuditFailure := (afFailedAccessAceFlag in aFlags);
end;

constructor TJwAuditAccessControlEntry.Create(
  const aListOwner: TJwSecurityAccessControlList;
  aAuditSuccess, aAuditFailure: boolean; const anAccessMask: TJwAccessMask;
  const aSID: TJwSecurityId; ownSID: boolean);
begin
  inherited Create(aListOwner, [], anAccessMask, aSID, ownSID);
  AuditSuccess := aAuditSuccess;
  AuditFailure := aAuditFailure;
end;

constructor TJwAuditAccessControlEntry.Create(AccessEntry: TJwAuditAccessControlEntry);
begin
  inherited Create(AccessEntry);

  AuditSuccess := AccessEntry.AuditSuccess;
  AuditFailure := AccessEntry.AuditFailure;
end;

constructor TJwAuditAccessControlEntry.Create(
  const AccessEntryPointer: PSystemAuditAce);
begin
  inherited Create(PAccessAllowedAce(AccessEntryPointer));

  AuditSuccess := afSuccessfulAccessAceFlag in Flags;
  //Exclude(fFlags,afSuccessfulAccessAceFlag);

  AuditFailure := afFailedAccessAceFlag in Flags;
  //Exclude(fFlags,afFailedAccessAceFlag);
end;

constructor TJwAuditAccessControlEntry.Create;
begin
  inherited;

  fFlagsIgnored := [];
end;

function TJwAuditAccessControlEntry.Create_AuditACE: PSystemAuditAce;
var
  s1, s2, s3: TJwString;
begin
  Result := PSystemAuditAce(inherited Create_AllowACE);

  s1 := GetText;
  if s1 = '' then;

  //Result.Header.AceFlags := Result.Header.AceFlags and not SUCCESSFUL_ACCESS_ACE_FLAG;
  //Result.Header.AceFlags := Result.Header.AceFlags and not FAILED_ACCESS_ACE_FLAG;

  s2 := GetText;
  if s2 = '' then;

  if AuditSuccess then
    Result.Header.AceFlags :=
      Result.Header.AceFlags or SUCCESSFUL_ACCESS_ACE_FLAG;
  if AuditFailure then
    Result.Header.AceFlags :=
      Result.Header.AceFlags or FAILED_ACCESS_ACE_FLAG;

  s3 := GetText;
  if s3 = '' then;
end;

procedure TJwAuditAccessControlEntry.Free_PACE(var AccessEntryPointer: PSystemAuditAce);
begin

  inherited Free_PACE(PAccessAllowedAce(AccessEntryPointer));
end;


function TJwAuditAccessControlEntry.GetAccessMask : TJwAccessMask;
begin
  result := 0;
  if AuditSuccess then
    result := result or SUCCESSFUL_ACCESS_ACE_FLAG;
  if AuditFailure then
    result := result or FAILED_ACCESS_ACE_FLAG;
end;

procedure TJwAuditAccessControlEntry.SetAuditSuccess(const Value: boolean);
begin
  if Value then
    Include(fFlags, afSuccessfulAccessAceFlag)
  else
    Exclude(fFlags, afSuccessfulAccessAceFlag);

  AuditFailure := afFailedAccessAceFlag in Flags;
  //Exclude(fFlags,afFailedAccessAceFlag);
end;


procedure TJwAuditAccessControlEntry.SetAuditFailure(const Value: boolean);
begin
  if Value then
    Include(fFlags, afFailedAccessAceFlag)
  else
    Exclude(fFlags, afFailedAccessAceFlag);
end;

function TJwAuditAccessControlEntry.GetAuditSuccess: boolean;
begin
  Result := afSuccessfulAccessAceFlag in Flags;
end;

function TJwAuditAccessControlEntry.GetAuditFailure: boolean;
begin
  Result := afFailedAccessAceFlag in Flags;
end;

procedure TJwAuditAccessControlEntry.Assign(AccessEntry: TJwAuditAccessControlEntry);
begin
  inherited Assign(AccessEntry);
  AuditSuccess := AccessEntry.AuditSuccess;
  AuditFailure := AccessEntry.AuditFailure;
end;

function TJwSecurityAccessControlEntry.GetObjectFlags : Cardinal;
 function CompareGUID(const G1, G2: TGUID): boolean;
 begin
   Result := CompareMem(@G1, @G2, Sizeof(TGUID));
 end;
begin
  result := 0;
  if CompareGUID(NULL_GUID, Self.ObjectType) then
    result := result or ACE_OBJECT_TYPE_PRESENT;
  if CompareGUID(NULL_GUID, Self.InheritedObjectType) then
    result := result or ACE_INHERITED_OBJECT_TYPE_PRESENT;
end;

function TJwSecurityAccessControlEntry.GetOwnSid : Boolean;
begin
  if Assigned(SID) and (SID.IsStandardSID) then
    fOwnSid := False;

  result := fOwnSid;
end;

procedure TJwSecurityAccessControlEntry.SetOwnSid(const OwnSid: Boolean);
begin
  if Assigned(SID) and (SID.IsStandardSID) then
    fOwnSid := False
  else
    fOwnSid := OwnSid;
end;


function TJwSecurityAccessControlEntry.GetAceType: TJwAceType;
begin
  {Order must be correct
    CallbackX
    CallbackObjectX
    X
  }

  if Self is TJwDiscretionaryAccessControlEntryCallbackAllow then
    Result := actAllowCallback
  else
  if Self is TJwDiscretionaryAccessControlEntryObjectAllow then
    Result := actAllowObject
  else
  if Self is TJwDiscretionaryAccessControlEntryCallbackObjectAllow then
    Result := actAllowCallbackObject
  else
  if Self is TJwDiscretionaryAccessControlEntryAllow then
    Result := actAllow
  else

  if Self is TJwDiscretionaryAccessControlEntryCallbackDeny then
    Result := actDenyCallback
  else
  if Self is TJwDiscretionaryAccessControlEntryObjectDeny then
    Result := actDenyObject
  else
  if Self is TJwDiscretionaryAccessControlEntryCallbackObjectDeny then
    Result := actDenyCallbackObject
  else
  if Self is TJwDiscretionaryAccessControlEntryDeny then
    Result := actDeny
  else

  if Self is TJwAuditAccessControlEntryCallback then
    Result := actAuditCallback
  else
  if Self is TJwAuditAccessControlEntryCallback  then
    Result := actAuditCallbackObject
  else
  if Self is TJwAuditAccessControlEntryObject  then
    Result := actAuditObject
  else
  if Self is TJwAuditAccessControlEntry then
    Result := actAudit
  else


  if Self is TJwSystemMandatoryAccessControlEntry then
    Result := actMandatory
  else
    raise EJwsclInvalidACEException.CreateFmtEx(
      RsUnsupportedACE, 'GetAceType', ClassName, RsUNAcl,
      0, False, []);
end;




function TJwSecurityAccessControlEntry.GetExplicitAccess: TJwExplicitAccess;
begin
  if not Assigned(SID) or (ASsigned(SID) and
    (not IsValidSid(Sid.SID))) then
    raise EJwsclInvalidSIDException.CreateFmtEx(
      RsACLClassInvalidAceSid,
      'GetExplicitAccess', ClassName, RsUNAcl,
      0, False, []);

  Result.grfAccessPermissions := AccessMask;
  Result.grfInheritance := TJwEnumMap.ConvertAceFlags(Flags);
  //  Result.grfAccessMode := TAccessMode(Integer(ConvertAceFlagsToCardinal(Flags)));

  case GetAceType of
    actAllowCallback,
    actAllowObject,
    actAllowCallbackObject,
    actAllow: Result.grfAccessMode := GRANT_ACCESS;

    actDenyCallback,
    actDenyObject,
    actDenyCallbackObject,
    actDeny: Result.grfAccessMode  := DENY_ACCESS;


    actMandatory : Result.grfAccessMode := TAccessMode(AccessMask);

    actAudit,
    actAuditCallback,
    actAuditObject,
    actAuditCallbackObject:
    begin
      Result.grfAccessMode  := SET_ACCESS;
      Result.grfInheritance := 0;
      if TJwAuditAccessControlEntry(Self).AuditSuccess then
        Result.grfInheritance :=
          Result.grfInheritance or SUCCESSFUL_ACCESS_ACE_FLAG;
      if TJwAuditAccessControlEntry(Self).AuditFailure then
        Result.grfInheritance :=
          Result.grfInheritance or FAILED_ACCESS_ACE_FLAG;
    end;
  end;


  FillChar(Result.Trustee, sizeof(Result.Trustee), 0);
  Result.Trustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
  Result.Trustee.TrusteeForm := TRUSTEE_IS_SID;
  Result.Trustee.TrusteeType := TRUSTEE_IS_UNKNOWN;
  Result.Trustee.ptstrName := TJwPChar(SID.SID);
end;

function TJwSecurityAccessControlEntry.GetText: TJwString;
begin
  Result := GetTextMap(TJwSecurityGenericMapping);
end;



function TJwSecurityAccessControlEntry.GetTextMap(
  const Mapping: TJwSecurityGenericMappingClass): TJwString;
var
  i: TJwAceFlag;
  SidText,
  FlagString,
  sBits,
  sMap : TJwString;

begin
  FlagString := '';
  for i := low(TJwAceFlag) to high(TJwAceFlag) do
  begin
    if i in Flags then
      FlagString := FlagString + TJwAceFlagStrings[i]+',';
  end;
  System.Delete(FlagString, Length(FlagString), 1);



  if Assigned(Mapping) then
  begin
    //map generics to specific rights and then convert
    //all remaining specific rights to string
    sMap := Mapping.MapAccessMaskToString(Mapping.GenericMap(AccessMask))
  end
  else
    sMap := RsMapNoMapGiven + ' ' + IntToStr(Accessmask) +', 0x' + IntToHex(AccessMask,4);

  sBits := '['+JwAccessMaskToBits(Accessmask) + '] (' + IntToStr(Accessmask) +', 0x' + IntToHex(AccessMask,4)+')';

  if not Assigned(SID) then
    SidText := RsBracketNil
  else
    SidText := Sid.GetText(True);

  result := JwFormatString(
    RsACLClassGetTextMap,
     [ClassName,
     TAceTypeString[AceType],
     FlagString,
     sMap,
     SidText,
     sBits
     ]);
end;

procedure TJwSecurityAccessControlList.MergeElements;

var //ACE, ACE2 : TJwSecurityAccessControlEntry;
    ACE : TJwSecurityAccessControlEntry;
    i, i2 : Integer;
    EqualAceTypeSet: TJwEqualAceTypeSet;
begin
  {if Self is TJwDAccessControlList then
    EqualAceTypeSet := [eactSameSid, eactSameType,eactSameFlags]
  else
  if Self is TJwSAccessControlList then
    EqualAceTypeSet := [eactSameSid, eactSameType, eactSameFlags]
  else }
    EqualAceTypeSet := [eactSameSid, eactSameType, eactSameFlags];


  i := 0;
  while i < Count do
  begin
    ACE := GetItem(i);

    repeat
      i2 := FindEqualACE(ACE, EqualAceTypeSet, i);

      if (i2 >= 0) then
      begin
        ACE.AccessMask := ACE.AccessMask or GetItem(i2).AccessMask;

        Remove(i2);
        i := 0;
      end;
    until i2 = -1;
    Inc(i);
  end;
end;

function TJwSecurityAccessControlList.IsEqual(
  const AccessControlListInstance: TJwSecurityAccessControlList;
  const EqualAceTypeSet: TJwEqualAceTypeSet = JwAllEqualAceTypes): boolean;
var
  i, iPos: integer;
  tempACL1, tempACL2: TJwSecurityAccessControlList;
begin
  Result := False;

  if not Assigned(AccessControlListInstance) then
    Exit;

  if Self.ClassType <> AccessControlListInstance.ClassType then
    exit;

  tempACL1 := TJwSecurityAccessControlList(Self.ClassType.Create);
  tempACL1.Assign(Self);
  tempACL1.MergeElements;

  if tempACL1 is TJwDAccessControlList then
   (tempACL1 as TJwDAccessControlList).MakeCanonical;

  tempACL2 :=  TJwSecurityAccessControlList(Self.ClassType.Create);
  tempACL2.Assign(AccessControlListInstance);
  tempACL2.MergeElements;

  if tempACL1 is TJwDAccessControlList then
    (tempACL2 as TJwDAccessControlList).MakeCanonical;

  if tempACL1.Count <> tempACL2.Count then
  begin
    tempACL1.Free;
    tempACL2.Free;
    Exit;
  end;

  for i := 0 to tempACL1.Count - 1 do
  begin
    iPos := tempACL1.FindEqualACE(tempACL2.Items[i], EqualAceTypeSet, i - 1);
    if iPos <> i then
    begin
      tempACL1.Free;
      tempACL2.Free;
      Exit;
    end;
  end;

  Result := True;

  tempACL1.Free;
  tempACL2.Free;
end;

function TJwSecurityAccessControlList.IsCanonical: boolean;
var //[Hint] hasDirectNegative,
  hasDirectPositive, hasInheritedNegative,
  hasInheritedPositive: boolean;
  i: integer;
begin
  Result := False;

  //[Hint] hasDirectNegative     := False;
  hasDirectPositive := False;
  hasInheritedNegative := False;
  hasInheritedPositive := False;

  if Count = 0 then
  begin
    Result := True;
    Exit;
  end;

  if Items[0].AceType = actAudit then
  begin
    Result := True;
    Exit;
  end;


  for i := 0 to Count - 1 do
  begin

    if afInheritedAce in Items[i].Flags then
    begin
      if Items[i].AceType = actAllow then
      begin
        //inherited allow
        hasInheritedPositive := True;
      end
      else
      begin
        //inherited deny
        if hasInheritedPositive then
          Exit;

        hasInheritedNegative := True;
      end;
    end
    else
    if Items[i].AceType = actAllow then
    begin
      //direct allow
      if hasInheritedNegative or hasInheritedPositive then
        Exit;

      hasDirectPositive := True;
    end
    else
    begin
      //direct deny
      if hasInheritedNegative or hasInheritedPositive or
        hasDirectPositive then
        Exit;

      //[Hint] hasDirectNegative := True;
    end;
  end;

  Result := True;
end;

procedure TJwDAccessControlList.MakeCanonical;
var
  ACL: TJwSecurityAccessControlList;

  i: integer;

  ACE : TJwSecurityAccessControlEntry;
  //OldList : TJwSecurityAccessControlList;
  OldOwns : Boolean;
begin
  ACL := TJwDAccessControlList.Create;

  OldOwns := OwnsObjects;
  OwnsObjects := false;
  for i := Count - 1 downto 0 do
  begin
    ACE := Items[i];
    //detach from old list
    Remove(i);

    ACL.Add(ACE); //rearrange ACE in ACL
  end;
  OwnsObjects := OldOwns;

  Assign(ACL);
  ACL.Free;
end;


{ TJwSystemMandatoryAccessControlEntry }

constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLabel: PSystemMandatoryLabelAce);
begin
  inherited Create(TJwSecurityAccessControlEntry(MandatoryLabel));
end;
constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLabel: TJwSystemMandatoryAccessControlEntry);
begin
  inherited Create(TJwSecurityAccessControlEntry(MandatoryLabel));
end;

constructor TJwSystemMandatoryAccessControlEntry.Create(
  const MandatoryLevel : TMandatoryLevel;
  const ListOwner: TJwSAccessControlList = nil);
var Sid : TJwSecurityId;
    SidStr : TJwString;
begin
  case MandatoryLevel of
    MandatoryLevelUntrusted : SidStr := LowIL;
    MandatoryLevelLow       : SidStr := LowIL;
    MandatoryLevelMedium    : SidStr := MediumIL;
    MandatoryLevelHigh      : SidStr := HighIL;
    MandatoryLevelSystem    : SidStr := SystemIL;
    MandatoryLevelSecureProcess : SidStr := ProtectedProcessIL;
  else
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'Create', ClassName, RsUNAcl,
      0, False, []);
  end;
  Sid := TJwSecurityId.Create(SidStr);

  inherited Create(ListOwner,[],0,Sid, true);
end;

function TJwSystemMandatoryAccessControlEntry.Compare(
  const MandatoryLabel: TJwSystemMandatoryAccessControlEntry): Integer;
begin
  if not Assigned(MandatoryLabel) then
     raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter,
      'Compare', ClassName, RsUNAcl,
      0, False, ['MandatoryLabel']);
      
  result := Integer(Self.GetMandatoryLevelType) -
              Integer(MandatoryLabel.GetMandatoryLevelType);
end;

function TJwSystemMandatoryAccessControlEntry.GetMandatoryPolicy : TJwMandatoryPolicyFlagSet;
begin
  result := TJwEnumMap.ConvertMandatoryPolicyFlags(AccessMask);
end;

procedure TJwSystemMandatoryAccessControlEntry.
  SetMandatoryPolicy(const Level : TJwMandatoryPolicyFlagSet);
begin
  AccessMask := TJwEnumMap.ConvertMandatoryPolicyFlags(Level);
end;


function TJwSystemMandatoryAccessControlEntry.GetMandatoryLevelType: TMandatoryLevel;
begin
  if Sid.SubAuthorityCount = 0 then
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'GetMandatoryLevelType', ClassName, RsUNAcl,
      0, False, []);

  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_UNTRUSTED_RID then
    result := MandatoryLevelUntrusted
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_LOW_RID then
    result := MandatoryLevelLow
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_MEDIUM_RID then
    result := MandatoryLevelMedium
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_HIGH_RID then
    result := MandatoryLevelHigh
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_SYSTEM_RID then
    result := MandatoryLevelSystem
  else
  if Sid.SubAuthorityArray[0] = SECURITY_MANDATORY_PROTECTED_PROCESS_RID then
   result := MandatoryLevelSecureProcess
  else
    raise EJwsclInvalidMandatoryLevelException.CreateFmtEx(
      RsInvalidMandatoryLevelType,
      'GetMandatoryLevelType', ClassName, RsUNAcl,
      0, False, []);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


end.
{$ENDIF SL_OMIT_SECTIONS}