(*
  Name:             MSOfficeLanguages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    27 Mar 2008
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1220 - Initial version
*)
unit MSOfficeLanguages;

interface

type
  TIntegerArray = array of Integer;

function GetOfficeLanguages(const OfficeVersion: Integer): TIntegerArray;

implementation

// Office 2000 = 9
// Office XP = 10
// Office 2003 = 11
// Office 2007 = 12

const
  OfficeLanguage_9: array[0..2] of Integer =
    (1,2,3);

function GetOfficeLanguages(const OfficeVersion: Integer): TIntegerArray;
begin

end;

end.
