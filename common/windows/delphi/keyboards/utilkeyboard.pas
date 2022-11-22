(*
  Name:             utilkeyboard
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
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
*)
unit utilkeyboard;

interface

uses
  Windows;

function MAKELCID(wLanguageID, wSortID: WORD): DWORD;

implementation

uses
  SysUtils;

function MAKELCID(wLanguageID, wSortID: WORD): DWORD;
begin
    Result := (DWORD(wSortID) shl 16) or DWORD(wLanguageID);
end;


end.
