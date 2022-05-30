{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit is for testing purposes only!!. TBD

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
The Original Code is JwsclSecureUserObjects.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclSecureUserObjects;
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
{$INCLUDE ..\includes\Jwscl.inc}

interface

uses
  SysUtils,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclMapping, JwsclSid,
  JwsclSecureObjects, JwsclResource, JwsclSecurePrivateObjects,
  JwsclVersion, JwsclConstants,  JwsclDescriptor, JwsclToken,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!



type

  TJwUserSecurityInformation = class(TInterfacedObject, IJwPrivateSecurityInformation)
  public
    constructor Create(const Parent : IJwPrivateSecurityInformation;
        const GenericMask : TJwSecurityGenericMappingClass;
        const AccessCheckType : TJwGetAccessCheckTypeSet;
        const Level      : TCardinalE = -1;
        const ServerName : TJwString  = '';
        const ObjectName : TJwString  = '';
        const ObjectGuid : PGuid      = nil;
        const ObjectType : PGuid      = nil);
  protected
    fSecurityDescriptor : TJwSecurityDescriptor;
    fCheckOwnerGroup : Boolean;
    fParent : IJwPrivateSecurityInformation;
    fGenericMask : TJwSecurityGenericMappingClass;

    fLevel: TCardinalE;
    fObjectGuid: TGuid;
    fObjectName: TJwString;
    fObjectType: TGuid;
    fServerName: TJwString;
    fAccessCheckType : TJwGetAccessCheckTypeSet;

  public
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
  public
    property Level: TCardinalE read fLevel;
    property ObjectGuid: TGuid read fObjectGuid;
    property ObjectName: TJwString read fObjectName;
    property ObjectType: TGuid read fObjectType;
    property ServerName: TJwString read fServerName;

    property Parent : IJwPrivateSecurityInformation read fParent;
    property GenericMask : TJwSecurityGenericMappingClass read fGenericMask;
    property AccessCheckType : TJwGetAccessCheckTypeSet read fAccessCheckType;
  end;


implementation

{ TJwUserSecurityInformation }

constructor TJwUserSecurityInformation.Create;
  function InitGuid(p : PGUID) : TGuid;
  begin
    if p <> nil then
      result := p^
    else
      result := NULL_GUID;
  end;
begin
  inherited Create;
  fParent := Parent;
  fGenericMask := GenericMask;

  fLevel      := Level;
  fObjectGuid := InitGuid(ObjectGuid);
  fObjectName := ObjectName;
  fObjectType := InitGuid(ObjectType);
  fServerName := ServerName;
end;

function TJwUserSecurityInformation.GetChildren(
  out Children: TJwPrivateSecurityInformationArray): HRESULT;
begin
  Children := nil;
  result := 5;
end;

function TJwUserSecurityInformation.GetObjectInformation(
  const ObjectInformationSet: TJwSecurityObjectInformationFlagSet): TJwSecurityObjectInformation;

 function CompareGUID(const G1, G2: TGUID): boolean;
  begin
    Result := CompareMem(@G1, @G2, Sizeof(TGUID));
  end;
begin
  result.Level := Level;
  result.ObjectGuid := ObjectGuid;
  result.ObjectName := ObjectName;
  result.ObjectType := ObjectType;
  result.ServerName := ServerName;
end;

function TJwUserSecurityInformation.GetParent(
  out Parent: IJwPrivateSecurityInformation): HRESULT;
begin
  Parent := fParent;
  result := 5;
end;

procedure TJwUserSecurityInformation.GetSecurity(
  const SecurityInformation: TJwSecurityInformationFlagSet;
  var SecurityDescriptor: TJwSecurityDescriptor);
begin
  inherited;

end;

function TJwUserSecurityInformation.GetUseAccessCheck(
  const AccessCheckType: TJwGetAccessCheckType): Boolean;
begin
  result := AccessCheckType in fAccessCheckType;
end;

function TJwUserSecurityInformation.MapGenericMask(
  out GenericMap: TJwSecurityGenericMappingClass): HRESULT;
begin
  GenericMap := fGenericMask;
  result := 5;
end;

procedure TJwUserSecurityInformation.SetSecurity(
  const SecurityInformation: TJwSecurityInformationFlagSet;
  const SecurityDescriptor: TJwSecurityDescriptor);
begin
  inherited;

end;

end.
