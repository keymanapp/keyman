(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Test UUIDGen
*)
unit main;

interface

procedure Run;

implementation

uses
  crypt_guid,
  SysUtils;

procedure Run;
var
  g: TGuid;
const
  nsguid: TGuid = '{35AE7E02-08A8-401a-8C28-CFD1AC44485D}';
  buf = 'Tavultesoft UUID Test';
begin
  UUIDCreateSha1FromName(g, nsguid, PByte(PChar(buf)), Length(buf));
  writeln(GuidToString(g));
end;

end.
