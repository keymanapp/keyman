{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit is not implemented yet
Warning, this source code is under development and is not even in beta state!
Do not use it!


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

The Original Code is JwsclSecurePrivateObjects.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclSecurePrivateObjects;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
//do not move header comment from above unit declaration!

interface


uses
  JwaWindows, SysUtils, D5Impl,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclMapping, JwsclSid,
  JwsclSecureObjects, JwsclResource,
  JwsclVersion, JwsclConstants, JwsclDescriptor, JwsclToken,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  IJwPrivateSecurityInformation = interface;

  TJwPrivateSecurityInformationArray = Array of IJwPrivateSecurityInformation;

  IJwPrivateSecurityInformation = interface {$IFDEF DELPHI6_UP}(IInterface){$ENDIF}
    ['{979EC50C-1111-4239-8FD0-75A8C43C7441}']

    {<B>GetObjectInformation</B> retrieves information about the private objec
     @param ObjectInformationSet contains information which
     information must be returned. 
     @return Defines a structure which contains information about the private
       object 
    }
    function GetObjectInformation(const ObjectInformationSet :
            TJwSecurityObjectInformationFlagSet): TJwSecurityObjectInformation;

    {<B>GetParent</B> is called to retrieve the parent security descriptor of
     the current private object. This is used by GetPrivateInheritanceSource and
     other inheritance methods)
     @param Parent received a pointer to the parent private object. Return nil
      if no parent exists - e.g. it is on top of a tree structure. 
     @return Return S_OK if the parameter Parent is valid or the current object
      has no parent.
      Return E_NOTIMPL if the private object does not support tree structures. 
     }
    function GetParent(out Parent : IJwPrivateSecurityInformation) : HRESULT;

    function GetChildren(out Children : TJwPrivateSecurityInformationArray) : HRESULT;

    {<B>GetUseAccessCheck</B> is called if the security descriptor (by TJwSecurePrivateObject) is
     set or get and SetSecurityDescriptor and SetSecurityDescriptor wants
     to know whether the process should be checked vor validity.
     @param AccessCheckType defines whether the access check shall be performed
      for a get or set operation.
     @return If the function returns true an access check will prevent unauthorized
     change or retrieving of security information.
     If the function returns false no access check will be done and the user
     should do access check. 

     }
    function GetUseAccessCheck(const AccessCheckType : TJwGetAccessCheckType) : Boolean;

    {<B>GetSecurity</B> is called if the security information is retrieved.
     If an error occurs you can raise an exception which is redirected to
     the caller of the method of TJwSecurePrivateObject.

     Before this function is called the method GetUseAccessCheck is called to
     determine whether the security information should be checked for
     access.

     @param SecurityInformation This parameter contains the security descriptor
      parts which must be copied into the new descriptor. Only the given
      descriptor parts should be retrieved. No more or less.
       
     @param SecurityDescriptor Contains a security descriptor that must be adapted.
      The descriptor is already created and contains empty parts. Do not Free it!
      This method should only change the requested security parts
      in SecurityDescriptor defined by SecurityInformation!

    }

    procedure GetSecurity(const SecurityInformation :
            TJwSecurityInformationFlagSet; var SecurityDescriptor :
            TJwSecurityDescriptor);

    {<B>SetSecurity</B> is called if the security information is changed.
     If an error occurs you can raise an exception which is redirected to
     the caller of the method of TJwSecurePrivateObject.

     Before this function is called the method GetUseAccessCheck is called to
     determine whether the security information should be checked for
     access.

     @param SecurityInformation This parameter contains the security descriptor
      parts which must be copied into the private descriptor. Only the given
      descriptor parts should be set. No more or less. 
     @param SecurityDescriptor Contains a security descriptor that only contains
      parts which are defined in SecurityInformation. Do not free the
       the descriptor!
     )
    }

    procedure SetSecurity(const SecurityInformation :
            TJwSecurityInformationFlagSet; const SecurityDescriptor :
            TJwSecurityDescriptor);

    {<B>MapGenericMask</B> is called if a generic access right was found on any access mask.
     You have to provide a generic map class that defines how generic access
     rights are mapped to your private access rights.

     @param GenericMap receives a classname (not instance) which describes how
       to map generic access rights 
     @return 
              # Return S_OK if the call succeeded.
              # Return E_NOTIMPL to use TJwSecurityGenericMapping as standard map. 
              # Any other result will raise EJwsclInvalidObjectException 
             
    }
    function MapGenericMask(out GenericMap : TJwSecurityGenericMappingClass) : HRESULT;
  end;

  TJwInterfacedPrivateSecurityInformation = class(TInterfacedObject, IJwPrivateSecurityInformation)
  protected
    fSecurityDescriptor : TJwSecurityDescriptor;
    fCheckOwnerGroup : Boolean;

    function GetObjectInformation(const ObjectInformationSet :
            TJwSecurityObjectInformationFlagSet): TJwSecurityObjectInformation; virtual; abstract;
    function GetParent(out Parent : IJwPrivateSecurityInformation) : HRESULT; virtual;
    function GetChildren(out Children : TJwPrivateSecurityInformationArray) : HRESULT; virtual;


    function GetUseAccessCheck(const AccessCheckType : TJwGetAccessCheckType)
      : Boolean; virtual;


    {See IJwPrivateSecurityInformation.GetSecurity  for more information.
     Remarks:
      This method only changes the requested security parts
      in SecurityDescriptor defined by SecurityInformation!
    }
    procedure GetSecurity(const SecurityInformation :
            TJwSecurityInformationFlagSet; var SecurityDescriptor :
            TJwSecurityDescriptor); virtual;

    {This method implements IJwPrivateSecurityInformation.GetSecurity 

     <B>SetSecurity</B> changes the internal security descriptor (fSecurityDescriptor ).
     It adapts it to the given security information in the parameters.

     Internal calls of <B>SetSecurity</B> will fail if the private security descriptor
     does not have a valid Owner and PrimaryGroup. This is because
     every private object must have its own owner and group.
     However this implementation tries to foresee this situation and
     automatically set the owner and group if necessary.
     The owner and group is retrieved from the parent (or its parents) if any.
     If there are no parents or the owner or group could not be retrieved
     the exception EJwsclInvalidObjectException is raised.

     @param SecurityInformation This parameter contains the security descriptor
      parts which must be copied into the private descriptor. Only the given
      descriptor parts should be set. No more or less. 
     @param SecurityDescriptor Contains a security descriptor that only contains
      parts which are defined in SecurityInformation. Do not free the
       the descriptor! 
     raises
 EJwsclInvalidObjectException:  will be raised if owner or group
      of the internal security descriptor (fSecurityDescriptor ) are nil. 
    }
    procedure SetSecurity(const SecurityInformation :
            TJwSecurityInformationFlagSet; const SecurityDescriptor :
            TJwSecurityDescriptor); virtual;

    function MapGenericMask(out GenericMap : TJwSecurityGenericMappingClass)
      : HRESULT; virtual; abstract;

  protected
    function GetInheritFlags(const FlagsType : TJwGetInheritFlagsType) : TJwInheritFlagSet; virtual; Abstract;
    function GetIsDirectoryObject : Boolean; virtual; Abstract;

  public
    {<B>Create</B> creates a new secure object.
     Warning: Your object must always have an owner and group. Otherwise
       many methods will fail.

     To create a secure object that inherits the security descriptor. See CreateInherited.

     @param DefaultSecurityDescriptor defines a default security descriptor that will
       be used by this object. If this parameter is nil the security information of the current token
       will be used.
       Read warning in description! 
     }
    constructor Create(const DefaultSecurityDescriptor : TJwSecurityDescriptor = nil); overload;

    {<B>CreateInherited</B> creates a secure object that inherits all security information from its parent.

     @param EJwsclInvalidParentDescriptor will be raised if the secure object does not have
        a parent or does not support inheritance. It simply calls GetParent to determine it. 
    }
    constructor CreateInherited; overload;

    destructor Destroy; Override;
  end;

  TJwSecurePrivateObject = class (TObject)
  public
     class procedure CheckMapGenericMask(
       const SecurityObject: IJwPrivateSecurityInformation;
       out Mapping : TJwSecurityGenericMappingClass);
  public
     {<B>GetSecurityDescriptor</B> returns security information of the given object.
      <B>GetSecurityDescriptor</B> does an access check to determine whether the caller
      is allowed to get the requested information. The requested secure object
      defines whether an automatic access check is done or the secure object
      does it itself (it calls GetUseAccessCheck). Access check is done
      including inherited ACEs from parent (if DACL is not protected).


      @param SecurityObject Defines a secure object which security information is requested. Must not be nil
      @param SecurityInformationSet Defines which security information is requested.
              If it contains siSaclSecurityInformation the caller must have enabled the privilege SE_SECURITY_NAME
              if automatic access check is activated. 
      @return Returns the requested security information in a security descriptor. 
      raises
 EJwsclAccessDenied:  will be raised if a requested security information is denied in the requested
            secure object or one of its parents. This behavior can be changed by the secure object individually.
            See the documentation of the secure object how access checks are done. 
       Exception: Any exception that is raised in a method of implementation of IJwPrivateSecurityInformation will be
            redirected to the caller of <B>GetSecurityDescriptor</B>. There is no guarantee that 100% of allocated memory is freed correctly. However
            the design is made to accomplish this goal. 
       EJwsclInvalidOwnerSIDException: will be raised if owner or group of the secure object is nil.
              Set the owner to JwNullSID if not exists. In this case the access will be denied if the DACL
              does not allow it. 
     }

     class function GetSecurityDescriptor(const SecurityObject :
       IJwPrivateSecurityInformation; SecurityInformationSet:
       TJwSecurityInformationFlagSet) : TJwSecurityDescriptor; virtual;

     class procedure SetSecurityDescriptor(const SecurityObject :
       IJwPrivateSecurityInformation; const SecurityInformationSet:
       TJwSecurityInformationFlagSet;
       const SecurityDescriptor : TJwSecurityDescriptor); virtual;
     class function GetPrivateInheritanceSource(const SecurityObject :
       IJwPrivateSecurityInformation;
       const aSecurityInfo: TJwSecurityInformationFlagSet =
       [siDaclSecurityInformation]): TJwInheritedFromArray; virtual;
     class procedure RemoveInheritanceFlow(const SecurityObject :
       IJwPrivateSecurityInformation; const Handle: THandle;
       const bCopyInheritedACEs: boolean = True); virtual;
     class procedure RestoreInheritanceFlow(const SecurityObject :
       IJwPrivateSecurityInformation; const Handle: THandle;
       bTakeOwnerShip: boolean = False); overload; virtual;
     class procedure TakeOwnerShip(const SecurityObject :
       IJwPrivateSecurityInformation; const SID: TJwSecurityId = nil); virtual;
     class procedure AccessCheck(const SecurityObject :
       IJwPrivateSecurityInformation;
       const ClientToken: TJwSecurityToken;
       const DesiredAccess: TJwAccessMask;
       out PrivilegeSet: TJwPrivilegeSet;
       out GrantedAccess: TJwAccessMask;
       out AccessStatus: boolean);
       overload; virtual;
     class function AccessCheck(const SecurityObject :
       IJwPrivateSecurityInformation;
       const DesiredAccess: TJwAccessMask = Cardinal(-1);
       const ClientToken: TJwSecurityToken = nil)
       : boolean;
       overload; virtual;

     class procedure TreeFileObjectSetNamedSecurityInfo(
       const SecurityObject : IJwPrivateSecurityInformation;
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
       const Disable64Redirection: boolean = False); virtual;
  end;







{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses JwsclEnumerations, JwsclKnownSid;


{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}



{ TJwSecurePrivateObject }

class function TJwSecurePrivateObject.GetSecurityDescriptor(const SecurityObject :
       IJwPrivateSecurityInformation; SecurityInformationSet:
       TJwSecurityInformationFlagSet): TJwSecurityDescriptor;

var UseAccessCheck : Boolean;
    DesiredAccess: TJwAccessMask;
begin
  UseAccessCheck := SecurityObject.GetUseAccessCheck(gactGetSecurity);

  //result := TJwSecurityDescriptor.CreateDefaultByToken(nil);
  result := TJwSecurityDescriptor.Create;
  try
    SecurityObject.GetSecurity(SecurityInformationSet, result);
  finally
  end;


  if UseAccessCheck then
  begin
    DesiredAccess := READ_CONTROL;
    if siSaclSecurityInformation in SecurityInformationSet then
      DesiredAccess := DesiredAccess or ACCESS_SYSTEM_SECURITY;

    if not AccessCheck(
      SecurityObject,//const SecurityObject: IJwPrivateSecurityInformation;
      DesiredAccess, //const DesiredAccess: TJwAccessMask;
      nil//const ClientToken: TJwSecurityToken): boolean;
      ) then
    begin
      FreeAndNil(result);
      raise EJwsclAccessDenied.CreateFmtEx(
        RsAccessDenied,
        'GetSecurityDescriptor', ClassName, RsUNSecurePrivateObjects,
        0, False, []);
    end; 
  end;
end;


class procedure TJwSecurePrivateObject.SetSecurityDescriptor(
  const SecurityObject: IJwPrivateSecurityInformation;
  const SecurityInformationSet: TJwSecurityInformationFlagSet;
  const SecurityDescriptor: TJwSecurityDescriptor);
var UseAccessCheck : Boolean;
    DesiredAccess: TJwAccessMask;
begin
  UseAccessCheck := SecurityObject.GetUseAccessCheck(gactSetSecurity);

  if UseAccessCheck then
  begin
    DesiredAccess := 0;
    if siOwnerSecurityInformation in SecurityInformationSet then
      DesiredAccess := DesiredAccess or WRITE_OWNER;

    if siGroupSecurityInformation in SecurityInformationSet then
      DesiredAccess := DesiredAccess or WRITE_OWNER;

    if siDaclSecurityInformation in SecurityInformationSet then
      DesiredAccess := DesiredAccess or WRITE_DAC;

    if siSaclSecurityInformation in SecurityInformationSet then
      DesiredAccess := DesiredAccess or ACCESS_SYSTEM_SECURITY;

    if not AccessCheck(
      SecurityObject,//const SecurityObject: IJwPrivateSecurityInformation;
      DesiredAccess, //const DesiredAccess: TJwAccessMask;
      nil//const ClientToken: TJwSecurityToken): boolean;
      ) then
    begin
      raise EJwsclAccessDenied.CreateFmtEx(
        RsAccessDenied,
        'SetSecurityDescriptor', ClassName, RsUNSecurePrivateObjects,
        0, False, []);
    end;

  end;

  try
    SecurityObject.SetSecurity(SecurityInformationSet, SecurityDescriptor);
  finally
  end;
end;

class function TJwSecurePrivateObject.AccessCheck(
  const SecurityObject: IJwPrivateSecurityInformation;
  const DesiredAccess: TJwAccessMask;
  const ClientToken: TJwSecurityToken): boolean;
var PrivilegeSet: TJwPrivilegeSet;
    GrantedAccess: TJwAccessMask;
begin
  PrivilegeSet := nil;
  try
    AccessCheck(
      SecurityObject,//const SecurityObject: IJwPrivateSecurityInformation;
      ClientToken,//const ClientToken: TJwSecurityToken;
      DesiredAccess,//const DesiredAccess: TJwAccessMask;
      PrivilegeSet,//out PrivilegeSet: TJwPrivilegeSet;
      GrantedAccess,//out GrantedAccess: TJwAccessMask;
      result//out AccessStatus: boolean
     );
  finally
    FreeAndNil(PrivilegeSet);
  end;
end;

class procedure TJwSecurePrivateObject.CheckMapGenericMask(
  const SecurityObject: IJwPrivateSecurityInformation;
  out Mapping : TJwSecurityGenericMappingClass);

var
    hr : HRESULT;
begin
  hr := SecurityObject.MapGenericMask(Mapping);
  case hr  of
    S_OK :;
    E_NOTIMPL : Mapping := TJwSecurityGenericMapping;
  else
    raise EJwsclInvalidObjectException.CreateFmtEx(
        RsInvalidResultValue,
        'CheckMapGenericMask', ClassName, RsUNSecurePrivateObjects,
        0, False, [hr]);
  end
end;


class procedure TJwSecurePrivateObject.AccessCheck(
  const SecurityObject: IJwPrivateSecurityInformation;
  const ClientToken: TJwSecurityToken; const DesiredAccess: TJwAccessMask;
  out PrivilegeSet: TJwPrivilegeSet; out GrantedAccess: TJwAccessMask;
  out AccessStatus: boolean);
var SD : TJwSecurityDescriptor;
    Mapping : TJwSecurityGenericMappingClass;
begin
  SD := TJwSecurityDescriptor.Create;


  try
    SecurityObject.GetSecurity(
      [siOwnerSecurityInformation,siGroupSecurityInformation,siDaclSecurityInformation,
      siSaclSecurityInformation],
      SD);
    //ShowMEssage(SD.Text);

    TJwSecurePrivateObject.CheckMapGenericMask(SecurityObject, Mapping);

    TJwSecureGeneralObject.AccessCheck(
      SD,//const SecurityDescriptor: TJwSecurityDescriptor;
      ClientToken,//const ClientToken: TJwSecurityToken;
      DesiredAccess,//const DesiredAccess: TJwAccessMask;
      Mapping,//const GenericMapping: TJwSecurityGenericMappingClass;
      PrivilegeSet,//out PrivilegeSet: TJwPrivilegeSet;
      GrantedAccess,//out GrantedAccess: TJwAccessMask;
      AccessStatus//out AccessStatus: boolean
      );
  finally
    FreeAndNil(SD);
  end;

end;

class function TJwSecurePrivateObject.GetPrivateInheritanceSource(
  const SecurityObject: IJwPrivateSecurityInformation;
  const aSecurityInfo: TJwSecurityInformationFlagSet): TJwInheritedFromArray;
begin

end;


class procedure TJwSecurePrivateObject.RemoveInheritanceFlow(
  const SecurityObject: IJwPrivateSecurityInformation;
  const Handle: THandle; const bCopyInheritedACEs: boolean);
begin

end;

class procedure TJwSecurePrivateObject.RestoreInheritanceFlow(
  const SecurityObject: IJwPrivateSecurityInformation;
  const Handle: THandle; bTakeOwnerShip: boolean);
begin

end;




class procedure TJwSecurePrivateObject.TakeOwnerShip(
  const SecurityObject: IJwPrivateSecurityInformation;
  const SID: TJwSecurityId);
begin

end;

class procedure TJwSecurePrivateObject.TreeFileObjectSetNamedSecurityInfo(
  const SecurityObject: IJwPrivateSecurityInformation;
  pObjectName: TJwString;
  const aSecurityInfo: TJwSecurityInformationFlagSet;
  const Action: TJwProgInvokeSetting; const SetType: TJwTreeSetType;
  const bKeepExplicit: boolean; const Owner, Group: TJwSecurityId;
  const DACL: TJwDAccessControlList; const SACL: TJwSAccessControlList;
  const FNProgressMethod: TJwFnProgressMethod;
  const FNProgressProcedure: TJwFnProgressProcedure;
  const ProgressUserData: Pointer; const aThread: TJwTagThread;
  const Disable64Redirection: boolean);
begin

end;

{ TJwInterfacedPrivateSecurityInformation }

constructor TJwInterfacedPrivateSecurityInformation.Create(
  const DefaultSecurityDescriptor: TJwSecurityDescriptor);
//var Token
begin
  inherited Create;
  _AddRef;

  fCheckOwnerGroup := false;

  if not Assigned(DefaultSecurityDescriptor) then
    fSecurityDescriptor := TJwSecurityDescriptor.CreateDefaultByToken(nil)
  else
    fSecurityDescriptor := TJwSecurityDescriptor.Create(DefaultSecurityDescriptor);
end;

constructor TJwInterfacedPrivateSecurityInformation.CreateInherited;
//var Parent : IJwPrivateSecurityInformation;
begin
(* BUGBUG
  Parent := nil;
  if (GetParent(Parent) <> S_OK) or (not Assigned(Parent)) then
    raise EJwsclInvalidParentDescriptor.CreateFmtEx(
        RsPrivateInvalidParentDescriptor,
        'CreateInherited', ClassName, RsUNSecurePrivateObjects,
        0, False, []);*)

  inherited Create;
  _AddRef;

  fSecurityDescriptor := TJwSecurityDescriptor.Create;
  fSecurityDescriptor.Control := fSecurityDescriptor.Control +
      [sdcDaclAutoInherited,sdcSaclAutoInherited];
  fSecurityDescriptor.DACLInherited := true;
end;

destructor TJwInterfacedPrivateSecurityInformation.Destroy;
begin
  FreeAndNil(fSecurityDescriptor);
  inherited;
end;

function TJwInterfacedPrivateSecurityInformation.GetParent(
  out Parent: IJwPrivateSecurityInformation): HRESULT;
begin
  result := E_NOTIMPL;
end;

function TJwInterfacedPrivateSecurityInformation.GetChildren(
  out Children : TJwPrivateSecurityInformationArray) : HRESULT;
begin
  result := E_NOTIMPL;
end;

procedure TJwInterfacedPrivateSecurityInformation.GetSecurity(
  const SecurityInformation: TJwSecurityInformationFlagSet;
  var SecurityDescriptor: TJwSecurityDescriptor);

  function CompareGUID(const G1, G2: TGUID): boolean;
  begin
    Result := CompareMem(@G1, @G2, Sizeof(TGUID));
  end;

  function HasPreParent(const Parent : IJwPrivateSecurityInformation) : Boolean;
  var P : IJwPrivateSecurityInformation;
  begin
    result := Parent.GetParent(P) = S_OK;
    result := result and Assigned(P);
  end;

var Mapping : TJwSecurityGenericMappingClass;
    ParentSD,
    NewSD : TJwSecurityDescriptor;
    Parent : IJwPrivateSecurityInformation;
    Info : TJwSecurityObjectInformation;
    ObjectType : PGUID;
begin
  TJwSecurePrivateObject.CheckMapGenericMask(
       Self,
       Mapping);

  if (GetParent(Parent) = S_OK) and Assigned(Parent) then
  begin
    Info := GetObjectInformation([soifObjectType]);

    if CompareGUID(Info.ObjectType, NULL_GUID) then
      ObjectType := nil
    else
      ObjectType := @Info.ObjectType;

    ParentSD := TJwSecurityDescriptor.Create;
    try
      Parent.GetSecurity(SecurityInformation, ParentSD);

      {OutputDebugStringW('ParentSD:'#13#10);
      OutputDebugStringW(PWideChar(ParentSD.Text+#13#10));  }

      NewSD := TJwSecurityDescriptor.CreatePrivateObjectSecurity(
        ParentSD,//const ParentSecurityDescriptor: TJwSecurityDescriptor;
        fSecurityDescriptor,//const CreatorSecurityDescriptor: TJwSecurityDescriptor;
        ObjectType,//const ObjectType : PGUID;
        GetIsDirectoryObject,//const IsDirectoryObject : Boolean;
        GetInheritFlags(giftCreatePrivate),//const AutoInheritFlags : Cardinal;
        Mapping//const GenericMap : TJwSecurityGenericMappingClass;
        //const Token : TObject = nil
      );
    finally
      FreeAndNil(ParentSD);
    end;

    try
      if Assigned(SecurityDescriptor) then
      begin
        if not (siOwnerSecurityInformation in SecurityInformation) then
        begin
          NewSD.OwnOwner := true;
          NewSD.Owner := nil;
        end;
        if not (siGroupSecurityInformation in SecurityInformation) then
        begin
          NewSD.OwnPrimaryGroup := true;
          NewSD.PrimaryGroup := nil;
        end;
      end;

      //just replace elements that are requested!
      SecurityDescriptor.ReplaceDescriptorElements(SecurityInformation, NewSD);
    finally
      FreeAndNil(NewSD);
    end;

    //ShowMessage(SecurityDescriptor.Text);
    {OutputDebugStringW('SD+ParentSD:'#13#10);
    OutputDebugStringW(PWideChar(SecurityDescriptor.Text+#13#10));  }
  end
  else
  begin
    SecurityDescriptor.Free;

    {Simply return the requested security parts}
    SecurityDescriptor := fSecurityDescriptor.GetPrivateObjectSecurity(SecurityInformation);

    {OutputDebugStringW('Origin SD:'#10#13);
    OutputDebugStringW(PWideChar(SecurityDescriptor.Text+#13#10));}

    {dont use this - it does not work as you would expect!
     SecurityDescriptor.SetPrivateObjectSecurity(SecurityInformation,
      fSecurityDescriptor,
       [ifAvoidOwnerRestriction, ifAvoidPrivilegeCheck, ifAvoidOwnerCheck,ifDefaultOwnerFromParent,ifDefaultGroupFromParent] +
       GetInheritFlags(giftSetPrivate),Mapping);}
  end;


end;

function TJwInterfacedPrivateSecurityInformation.GetUseAccessCheck(
  const AccessCheckType: TJwGetAccessCheckType): Boolean;
begin
  result := true;
end;

procedure TJwInterfacedPrivateSecurityInformation.SetSecurity(
  const SecurityInformation: TJwSecurityInformationFlagSet;
  const SecurityDescriptor: TJwSecurityDescriptor);
var Mapping : TJwSecurityGenericMappingClass;
    ObjectSecurityDescriptor : TJwSecurityDescriptor;
    Flags : TJwSecurityInformationFlagSet;
begin
  TJwSecurePrivateObject.CheckMapGenericMask(
       Self,
       Mapping);

  ObjectSecurityDescriptor := TJwSecurityDescriptor.Create(fSecurityDescriptor);

  {SetPrivateObjectSecurity will fail if ObjectSecurityDescriptor
  does not have a valid Owner and PrimaryGroup. This is because
  every private object has its own owner and group.
  We do this here.

  If the caller does not want to set owner and group,
  we simply get it from our own security descriptor.
  A call to GetSecurity returns the owner and group of the private object
  or if nil, it returns the parent owner or group.
  }
  Flags := [];
  if not Assigned(ObjectSecurityDescriptor.Owner) then
    Include(Flags,siOwnerSecurityInformation);
  if not Assigned(ObjectSecurityDescriptor.PrimaryGroup) then
    Include(Flags,siGroupSecurityInformation);


  try
    if Flags <> [] then
      Self.GetSecurity(Flags, ObjectSecurityDescriptor);

    {Whether this object nor its parent do have a valid owner?!}
    if (not Assigned(ObjectSecurityDescriptor.Owner) or
      not Assigned(ObjectSecurityDescriptor.PrimaryGroup)) then
       raise EJwsclInvalidObjectException.CreateFmtEx(
        RsPrivateInvalidOwnerOrGroup,
        'SetSecurity', ClassName, RsUNSecurePrivateObjects,
        0, False, []);

    ObjectSecurityDescriptor.SetPrivateObjectSecurity(SecurityInformation,
        SecurityDescriptor,[ifAvoidOwnerCheck,ifAvoidPrivilegeCheck],Mapping);

    fSecurityDescriptor.ReplaceDescriptorElements(SecurityInformation,
      ObjectSecurityDescriptor);
  finally
    ObjectSecurityDescriptor.Free;
  end;
end;



{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
