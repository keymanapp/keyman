(*
  Name:             WideStringClass
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
  History:          27 Mar 2008 - mcdurdin - Initial version
*)
unit WideStringClass;

interface

type
  TWideString = class
    s:  string;
    constructor Create(const fs: WideString);
  end;
  
implementation

{ TWideString }

constructor TWideString.Create(const fs: WideString);
begin
  s := fs;
end;

end.
