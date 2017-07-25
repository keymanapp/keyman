{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit provides simple privilege administration. See JwGetPrivilegeScope how to use it.

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
The Original Code is JwsclPrivileges.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclPrivileges;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
//do not move header comment from above unit declaration!

interface


uses
  SysUtils, Classes,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclToken,
  JwsclVersion, JwsclConstants,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}


type
  {<B>IJwPrivilegeScope</B> provides an interface to a scope based
   privilege administration.
   See JwGetPrivilegeScope how to use this.
  }
  IJwPrivilegeScope = interface {$IFDEF DELPHI6_UP}(IInterface){$ENDIF}
    ['{257AC2B6-7B56-4C34-80AC-B4233BA71313}']
    function GetEnabledPrivileges : TJwStringArray;
    property EnabledPrivileges : TJwStringArray read GetEnabledPrivileges;
  end;


  {<B>TJwPrivilegeScope</B> provides an interface to a scope based
   privilege administration.}
  TJwPrivilegeScope = class (TInterfacedObject, IJwPrivilegeScope)
  protected
    fEnabledPrivileges : TJwStringArray;
    function GetEnabledPrivileges : TJwStringArray;
  public
    {<B>Create</B> creates a new @classname instance and activates
     all privileges specified in parameter Privileges.
     These privileges will be disabled on destruction of this instance.
     @param defines a list of privileges to be enabled.
     @param SetType defines whether the privileges should be enabled or disabled.
      Define pst_EnableIfAvail if you dont want to raise an exception if the
      privlege does not exist. 
     raises
 EJwsclPrivilegeException:  will be raised if the privilege is not available and query is pst_Enable,
        otherwise the return value is false. If query is pst_EnableIfAvail the return is false, if the privilege could not be enabled.
        In this case no exception is raised.
        If an exception is raised all privileges enabled to this point are
        disabled 
    }
    constructor Create(const Privileges : array of TJwString; const SetType : TJwPrivilegeSetType);
    destructor Destroy; override;

    {<B>EnabledPrivileges</B> returns an array of privileges that were given to to constructor.}
    property EnabledPrivileges : TJwStringArray read GetEnabledPrivileges;
  end;



  {<B>JwGetPrivilegeScope</B> creates a new privilege scope instance and activates
     all privileges specified in parameter Privileges.
     These privileges will be disabled on destruction of this instance.
     The instance will be automatically be destroyed if the instance pointer
     runs out of scope.
     <code lang="Delphi">
      var P : IJwPrivilegeScope;
      begin
        try
          P := JwGetPrivilegeScope([SE_SHUTDOWN_NAME, SE_TCB_NAME, SE_SECURITY_NAME]);
        except
          on E : EJwsclPrivilegeException do
           //do things on error and exit
        end;
        //do things on success that needs privileges enabled
      end; //here the privileges are automatically disabled
     </code>
     There is no need for an exception handling if parameter SetType is set to
     pst_EnableIfAvail.

     @param defines a list of privileges to be enabled.
     @param SetType defines whether the privileges should be enabled or disabled.
      Define pst_EnableIfAvail if you dont want to raise an exception if the
      privlege does not exist. 
     raises
 EJwsclPrivilegeException:  will be raised if the privilege is not available and query is pst_Enable,
        otherwise the return value is false. If query is pst_EnableIfAvail the return is false, if the privilege could not be enabled.
        In this case no exception is raised.
        If an exception is raised all privileges enabled to this point are
        disabled 
    
  }
  function JwGetPrivilegeScope(const Privileges : array of TJwString;
    const SetType : TJwPrivilegeSetType = pst_Enable) : IJwPrivilegeScope;

{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}
{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
function JwGetPrivilegeScope(const Privileges : array of TJwString;
  const SetType : TJwPrivilegeSetType = pst_Enable) : IJwPrivilegeScope;
begin
  result := TJwPrivilegeScope.Create(Privileges, SetType);
end;



{ TJwPrivilegeScope }

constructor TJwPrivilegeScope.Create(const Privileges: array of TJwString;
  const SetType : TJwPrivilegeSetType);
var i : Integer;
begin
  SetLength(fEnabledPrivileges, Length(Privileges));
  for i := low(Privileges) to high(Privileges) do
  begin
    fEnabledPrivileges[i]  := Privileges[i];
    JwEnablePrivilege(fEnabledPrivileges[i], SetType);
  end;
end;


destructor TJwPrivilegeScope.Destroy;
var i : Integer;
begin
  for i := low(fEnabledPrivileges) to high(fEnabledPrivileges) do
  begin
    if Length(fEnabledPrivileges[i]) <> 0 then
    try
      JwEnablePrivilege(fEnabledPrivileges[i], pst_Disable);
    except
      on E : EJwsclPrivilegeException do;
       //do nothing, we want that all privileges are disabled
    end;
  end;
  inherited;
end;
function TJwPrivilegeScope.GetEnabledPrivileges: TJwStringArray;
begin
  result := fEnabledPrivileges;
end;


procedure Test;
var P : IJwPrivilegeScope;
begin
  P := JwGetPrivilegeScope([SE_SHUTDOWN_NAME, SE_TCB_NAME, SE_SECURITY_NAME]);
  if P = nil then;
end;


initialization
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
