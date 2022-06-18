(*
  Name:             ErrorControlledRegistry
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      12 Oct 2007

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          12 Oct 2007 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors, include WideString support
                    04 May 2012 - mcdurdin - I3308 - V9.0 - Start to move towards Delphi namespaces
                    18 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    26 Jun 2012 - mcdurdin - I3383 - KM9 - ErrorControlledRegistry is obsolete
*)
// Add error management
{ *********************************************************************** }
{                                                                         }
{ Delphi Runtime Library                                                  }
{                                                                         }
{ Copyright (c) 1995-2004 Borland Software Corporation                    }
{                                                                         }
{ *********************************************************************** }

unit ErrorControlledRegistry;  // I3309

{$R-,T-,H+,X+}

interface

uses
  System.Win.Registry;

type
  TRegistryErrorControlled = class(TRegistry)
  public
    procedure RaiseLastRegistryError(const msg: string = '');
  end;

implementation

uses
  System.SysUtils;

{ TRegistryErrorControlled }

procedure TRegistryErrorControlled.RaiseLastRegistryError(const msg: string);
begin
  if msg = ''
    then raise ERegistryException.Create(LastErrorMsg + ' ('+IntToHex(LastError,8)+')')
    else raise ERegistryException.Create(msg + #13#10 + LastErrorMsg + ' ('+IntToHex(LastError,8)+')');
end;

end.


