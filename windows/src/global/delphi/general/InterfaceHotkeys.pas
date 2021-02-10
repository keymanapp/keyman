(*
  Name:             InterfaceHotkeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    9 Apr 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    09 Apr 2010 - mcdurdin - I2296 - OSK menu not showing hotkeys for new items
*)
unit InterfaceHotkeys;

interface

uses
  keymanapi_TLB;

const
  CInterfaceHotkeys: array[0..7] of KeymanHotkeyTarget = (
    khKeymanOff,
    khKeyboardMenu,
    khVisualKeyboard,
    khKeymanConfiguration,
    khFontHelper,
    khCharacterMap,
    khTextEditor,
    khLanguageSwitch);

implementation

end.
