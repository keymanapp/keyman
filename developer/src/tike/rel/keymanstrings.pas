(*
  Name:             keymanstrings
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      26 Jun 2012

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit keymanstrings;

interface

const
  SKMustIncludeDebug = 'You must include debug information to start the debugger.  Check ''Include debug information'' in the Keyboard menu.';
  SKErrorsInCompile = 'The keyboard did not compile successfully. Unable to start debugger.';
  SKKeyboardKMXDoesNotExist = 'The keyboard .kmx output file does not exist. The keyboard must target Windows or macOS for interactive debugging.';

implementation

end.
