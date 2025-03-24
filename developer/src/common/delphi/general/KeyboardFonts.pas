(*
  Name:             KeyboardFonts
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      25 Sep 2014

  Modified Date:    25 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 Sep 2014 - mcdurdin - I4409 - V9.0 - Wrong font selected in keyboard debugger touch layout
                    
*)
unit KeyboardFonts;

interface

type
  TKeyboardFont = (kfontCode, kfontChar, kfontOSK, kfontTouchLayoutPhone, kfontTouchLayoutTablet, kfontTouchLayoutDesktop,
    kfontDisplayMap);

  TKeyboardFontArray = array[TKeyboardFont] of string;   // I4409

  TKeyboardFontInfo = record
    Name: string;
    Size: string;
    //Filename: string;
    Enabled: Boolean;
  end;

const
  KeyboardFontId: array[TKeyboardFont] of string = (
    'Code', 'Char', 'Osk', 'TouchLayoutPhone', 'TouchLayoutTablet', 'TouchLayoutDesktop', 'DisplayMap'
  );
implementation

end.
