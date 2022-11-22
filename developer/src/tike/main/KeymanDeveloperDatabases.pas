(*
  Name:             KeymanDeveloperDatabases
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
*)
unit KeymanDeveloperDatabases;

interface

uses SysUtils, ADODB;

var
  dbEngine: TADOConnection = nil;

function CanUseDatabases: Boolean;
procedure InitDatabaseEngine;
procedure FreeDatabaseEngine;

implementation

function CanUseDatabases: Boolean;
begin
  Result := Assigned(dbEngine);
end;

procedure InitDatabaseEngine;
begin
  try
    dbEngine := TADOConnection.Create(nil);
  except
    dbEngine := nil;
  end;
end;

procedure FreeDatabaseEngine;
begin
  dbEngine.Free;
  dbEngine := nil;
end;

end.

