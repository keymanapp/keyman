(*
  Name:             CharMapInsertMode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Moved to new location
*)
unit CharMapInsertMode;

interface

type
  TCharMapInsertMode = (
    cmimCode,       // U+0065 or d96          (will adjust quotes and spacing as necessary)
    cmimCharacter,  // 'a'                    (will adjust quotes as necessary)
    cmimName,       // $LATIN_SMALL_LETTER_A  (will adjust quotes and spacing as necessary)
    cmimText,       // a                      (i.e. no quotes)
    cmimDefault,    // (whatever is selected in the character map)
    cmimCustom);    // call a callback in the control

implementation

end.
