{
Description
Project JEDI Windows Security Code Library (JWSCL)

Implements a very simple Security Descriptor 
This unit contains ansi- and unicode string types that are used by the units of JWSCL.
You can define UNICODE to use unicode strings. Otherwise ansicode will be used.


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

The Original Code is JwsclSimpleDescriptor.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


}
unit JwsclSimpleDescriptor;

interface
uses
  SysUtils,
  JwaWindows,
  JwsclDescriptor,
  JwsclSid,
  JwsclTypes,
  JwsclAcl,
  JwsclStrings;


type
  {<B>TJwSimpleDescriptor</B> implements a simplier version of a security descriptor.
  It only supports :
    * discretionary access control list
    * owner
    * allow access control entries
    * deny access control entries
    * access rights
    * static security attributes (managed by class)
  The class does not support:
    * inheritance
    * group
    * access control entry flags
  }
  TJwSimpleDescriptor = class
  protected
    fSD : TJwSecurityDescriptor;
    fSA : PSecurityAttributes;
    function GetSA : LPSECURITY_ATTRIBUTES;

    function GetOwner : TJwSecurityId;

    procedure CheckDACL;
  public
    {<B>Allow</B> adds a new allow access control entry to the DACL
     using an username and an access mask.
     @param UserName defines a string of a username. Domain and computername is not supported.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Allow(const UserName : TJwString; const Access : TJwAccessMask = GENERIC_ALL); overload;

    {<B>Allow</B> adds a new allow access control entry to the DACL
     using an username and an access mask.
     @param SID defines a pointer to a binary representation of a security identifier. Must not be nil.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Allow(const SID : PSID; const Access : TJwAccessMask = GENERIC_ALL); overload;
    {<B>Allow</B> adds a new allow access control entry to the DACL
     using an username and an access mask.
     @param Sid defines a user through a TJwSecurityID instance. Must not be nil.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Allow(const SID : TJwSecurityID; const Access : TJwAccessMask = GENERIC_ALL); overload;

    {<B>Deny</B> adds a new deny access control entry to the DACL in the correct order.
     using an username and an access mask.
     @param UserName defines a string of a username. Domain and computername is not supported.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Deny(const UserName : TJwString; const Access : TJwAccessMask = GENERIC_ALL); overload;

    {<B>Deny</B> adds a new deny access control entry to the DACL in the correct order.
     using an username and an access mask.
     @param SID defines a pointer to a binary representation of a security identifier. Must not be nil.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Deny(const SID : PSID; const Access : TJwAccessMask = GENERIC_ALL); overload;

    {<B>Deny</B> adds a new deny access control entry to the DACL in the correct order.
     using an username and an access mask.
     @param Sid defines a user through a TJwSecurityID instance. Must not be nil.
     @param Access defines the access mask that defines the user's access to the object.
    }
    procedure Deny(const SID : TJwSecurityID; const Access : TJwAccessMask = GENERIC_ALL); overload;

    {<B>SetOwner</B> replaces the (existing) owner with the given one as a username on the local machine.
    }
    procedure SetOwner(const UserName : TJwString); overload;

    {<B>SetOwner</B> replaces the (existing) owner using a binary representation
    of a security identifier.
    }
    procedure SetOwner(const SID : PSID); overload;

    {<B>SetOwner</B> replaces the (existing) owner with a TJwSecurityId instance.
    }
    procedure SetOwner(const SID : TJwSecurityId); overload;

    {<B>Create</B> creates a new TJwSimpleDescriptor instance and initializes
    an empty security descriptor. It contains:
     * an empty DACL which denies all access
     * an empty owner - property Owner is nil.
    }
    constructor Create;
    destructor Destroy; override;

    {<B>SecurityAttributes</B> returns a security attribute structure that must not
     be freed. Each call of this property generates a new
     pointer to a data structure. The previous structure is freed and thus made invalid.}
    property SecurityAttributes : LPSECURITY_ATTRIBUTES read GetSA;

    {<B>Owner</B> returns the internal representation of the owner of the security descriptor.
     The property returns nil if the owner was not set previously.}
    property Owner : TJwSecurityId read GetOwner;

    {Returns a the internal presentation of a JWSCL security descriptor class.
    It can be used with other JWSCL methods. Do not free it!}
    property SecurityDescriptor : TJwSecurityDescriptor read fSD;
  end;

implementation
{ TJwSimpleDACL }

procedure TJwSimpleDescriptor.Allow(const UserName: TJwString; const Access : TJwAccessMask = GENERIC_ALL);
var Sid : TJwSecurityID;
begin
  Sid := TJwSecurityId.Create('', UserName);
  try
    Allow(Sid, Access);
  finally
    Sid.Free;
  end;
end;

procedure TJwSimpleDescriptor.Allow(const SID: PSID; const Access : TJwAccessMask = GENERIC_ALL);
var SidObj : TJwSecurityID;
begin
  SidObj := TJwSecurityId.Create(Sid);
  try
    Allow(SidObj, Access);
  finally
    SidObj.Free;
  end;
end;

procedure TJwSimpleDescriptor.Allow(const SID: TJwSecurityID; const Access : TJwAccessMask = GENERIC_ALL);
begin
  CheckDACL;
  fSD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], Access, Sid, false))
end;

procedure TJwSimpleDescriptor.CheckDACL;
begin
  if not Assigned(fSD.DACL) then
  begin
    //first release old DACL.
    //1. if OwnDACL is true, the DACL is freed
    //2. if OwnDACL is false, the DACL is dismissed without freeing it
    fSD.DACL := nil;
    //Set OwnDACL to true, so we get the new instance and use it directly
    fSD.OwnDACL := true;
    fSD.DACL := TJwDAccessControlList.Create;
    //Now let handle the SD freeing it on destruction.
  end;
end;

constructor TJwSimpleDescriptor.Create;
begin
  fSD := TJwSecurityDescriptor.Create;
  fSD.OwnPrimaryGroup := false;
  fSD.PrimaryGroup := TJwSecurityId.Create('S-1-0-0'); //Null SID
  fSD.OwnPrimaryGroup := true;

  fSD.OwnOwner := True;
  fSD.OwnDACL := True;
end;

procedure TJwSimpleDescriptor.Deny(const UserName: TJwString; const Access : TJwAccessMask = GENERIC_ALL);
var Sid : TJwSecurityID;
begin
  Sid := TJwSecurityId.Create('', UserName);
  try
    Deny(Sid, Access);
  finally
    Sid.Free;
  end;
end;

procedure TJwSimpleDescriptor.Deny(const SID: PSID; const Access : TJwAccessMask = GENERIC_ALL);
var SidObj : TJwSecurityID;
begin
  SidObj := TJwSecurityId.Create(Sid);
  try
    Deny(SidObj, Access);
  finally
    SidObj.Free;
  end;
end;

procedure TJwSimpleDescriptor.Deny(const SID: TJwSecurityID; const Access : TJwAccessMask = GENERIC_ALL);
begin
  CheckDACL;
  fSD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil, [], Access, Sid, false))
end;

destructor TJwSimpleDescriptor.Destroy;
begin
  FreeAndNil(fSD);
  TJwSecurityDescriptor.Free_SA(fSA);
  inherited;
end;

function TJwSimpleDescriptor.GetOwner: TJwSecurityId;
begin
  result := fSD.Owner;
end;

function TJwSimpleDescriptor.GetSA: LPSECURITY_ATTRIBUTES;
begin
  TJwSecurityDescriptor.Free_SA(fSA);
  fSA := fSD.Create_SA();
  result := LPSECURITY_ATTRIBUTES(fSA);
end;

procedure TJwSimpleDescriptor.SetOwner(const UserName: TJwString);
begin
  fSD.ReplaceOwner(TJwSecurityId.Create('', UserName));
end;

procedure TJwSimpleDescriptor.SetOwner(const SID: TJwSecurityId);
begin
  if not Assigned(Sid) then
    fSD.Owner := nil
  else
    fSD.ReplaceOwner(TJwSecurityId.Create(SID));
end;

procedure TJwSimpleDescriptor.SetOwner(const SID: PSID);
begin
  if not Assigned(Sid) then
    fSD.Owner := nil
  else
    fSD.ReplaceOwner(TJwSecurityId.Create(SID));
end;


end.
