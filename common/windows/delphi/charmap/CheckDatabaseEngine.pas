(*
  Name:             CheckDatabaseEngine
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Moved to new location
*)
unit CheckDatabaseEngine;

interface

uses SysUtils, ADODB_TLB;

//var
//  dbEngine: TADOConnection = nil;

function CanUseDatabases: Boolean;
procedure InitDatabaseEngine;
procedure FreeDatabaseEngine;

implementation

function CanUseDatabases: Boolean;
begin
  Result := True;
//  Result := Assigned(dbEngine);
end;

procedure InitDatabaseEngine;
begin
{  try
    dbEngine := ADOConnection.Create(nil);
  except
    dbEngine := nil;
  end;}
end;

procedure FreeDatabaseEngine;
begin
{  dbEngine.Free;
  dbEngine := nil;}
end;

end.

