(*
  Name:             CharacterRanges
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
  History:          14 Sep 2006 - mcdurdin - Initial version
*)
unit CharacterRanges;

interface

type
  TCharacterRange = record
    ParentRange: Integer;
    IsANSI: Boolean;
    n1, n2: Integer;
    nm: string;
  end;

const
  CR_ANSIRange = 0;
  CR_UnicodeRange = 1;

  CharacterRange: array[0..7] of TCharacterRange = (
    (IsANSI: True; nm: 'ANSI'),         // 0
    (nm: 'Unicode'),                    // 1
    (nm: 'Unicode - Latin'),            // 2
    (nm: 'Unicode - Punctuation'),      // 3
    (nm: 'Unicode - Miscellaneous'),    // 4
    (nm: 'Unicode - Scripts'),          // 5
    (nm: 'Unicode - CJK'),              // 6
    (nm: 'Unicode - Planes'));          // 7

  CharacterSubRange: array[0..81] of TCharacterRange = (
    //(ParentRange: 1; n1: $0000; ns: $FFFF; nm: 'Unicode'),
    (ParentRange: 2; n1: $0000; n2: $007F; nm: 'Basic Latin'),
    (ParentRange: 2; n1: $0080; n2: $00FF; nm: 'Latin-1 Supplement'),
    (ParentRange: 2; n1: $0100; n2: $017F; nm: 'Latin Extended-A'),
    (ParentRange: 2; n1: $0180; n2: $024F; nm: 'Latin Extended-B'),
    (ParentRange: 2; n1: $0250; n2: $02AF; nm: 'IPA Extensions'),
    (ParentRange: 4; n1: $02B0; n2: $02FF; nm: 'Spacing Modifier Letters'),
    (ParentRange: 4; n1: $0300; n2: $036F; nm: 'Combining Diacritical Marks'),
    (ParentRange: 5; n1: $0370; n2: $03FF; nm: 'Greek'),
    (ParentRange: 5; n1: $0400; n2: $04FF; nm: 'Cyrillic'),
    (ParentRange: 5; n1: $0530; n2: $058F; nm: 'Armenian'),
    (ParentRange: 5; n1: $0590; n2: $05FF; nm: 'Hebrew'),
    (ParentRange: 5; n1: $0600; n2: $06FF; nm: 'Arabic'),
    (ParentRange: 5; n1: $0900; n2: $097F; nm: 'Devanagari'),
    (ParentRange: 5; n1: $0980; n2: $09FF; nm: 'Bengali'),
    (ParentRange: 5; n1: $0A00; n2: $0A7F; nm: 'Gurmukhi'),
    (ParentRange: 5; n1: $0A80; n2: $0AFF; nm: 'Gujarati'),
    (ParentRange: 5; n1: $0B00; n2: $0B7F; nm: 'Oriya'),
    (ParentRange: 5; n1: $0B80; n2: $0BFF; nm: 'Tamil'),
    (ParentRange: 5; n1: $0C00; n2: $0C7F; nm: 'Telegu'),
    (ParentRange: 5; n1: $0C80; n2: $0CFF; nm: 'Kannada'),
    (ParentRange: 5; n1: $0D00; n2: $0D7F; nm: 'Malayalam'),
    (ParentRange: 5; n1: $0E00; n2: $0E7F; nm: 'Thai'),
    (ParentRange: 5; n1: $0E80; n2: $0EFF; nm: 'Lao'),
    (ParentRange: 5; n1: $0F00; n2: $0FBF; nm: 'Tibetan'),
    (ParentRange: 5; n1: $1080; n2: $10FF; nm: 'Georgian'),
    (ParentRange: 5; n1: $1100; n2: $11FF; nm: 'Hangul Jamo'),
    (ParentRange: 2; n1: $1E00; n2: $1EFF; nm: 'Latin Extended Additional'),
    (ParentRange: 5; n1: $1F00; n2: $1FFF; nm: 'Greek Extended'),
    (ParentRange: 3; n1: $2000; n2: $206F; nm: 'General Punctuation'),
    (ParentRange: 4; n1: $2070; n2: $209F; nm: 'Superscripts and Subscripts'),
    (ParentRange: 4; n1: $20A0; n2: $20CF; nm: 'Currency Symbols'),
    (ParentRange: 4; n1: $20D0; n2: $20FF; nm: 'Combining Marks for Symbols'),
    (ParentRange: 4; n1: $2100; n2: $214F; nm: 'Letterlike Symbols'),
    (ParentRange: 4; n1: $2150; n2: $218F; nm: 'Number Forms'),
    (ParentRange: 4; n1: $2190; n2: $21FF; nm: 'Arrows'),
    (ParentRange: 4; n1: $2200; n2: $22FF; nm: 'Mathematical Operators'),
    (ParentRange: 4; n1: $2300; n2: $23FF; nm: 'Miscellaneous Technical'),
    (ParentRange: 4; n1: $2400; n2: $243F; nm: 'Control Pictures'),
    (ParentRange: 4; n1: $2440; n2: $245F; nm: 'Optical Character Recognition'),
    (ParentRange: 4; n1: $2460; n2: $24FF; nm: 'Enclosed Alphanumerics'),
    (ParentRange: 4; n1: $2500; n2: $257F; nm: 'Box Drawing'),
    (ParentRange: 4; n1: $2580; n2: $259F; nm: 'Block Elements'),
    (ParentRange: 4; n1: $25A0; n2: $25FF; nm: 'Geometric Shapes'),
    (ParentRange: 4; n1: $2600; n2: $26FF; nm: 'Miscellaneous Symbols'),
    (ParentRange: 4; n1: $2700; n2: $27BF; nm: 'Dingbats'),
    (ParentRange: 6; n1: $3000; n2: $303F; nm: 'CJK Symbols and Punctuation'),
    (ParentRange: 6; n1: $3040; n2: $309F; nm: 'Hiragana'),
    (ParentRange: 6; n1: $30A0; n2: $30FF; nm: 'Katakana'),
    (ParentRange: 6; n1: $3100; n2: $312F; nm: 'Bopomofo'),
    (ParentRange: 6; n1: $3130; n2: $318F; nm: 'Hangul Compatibility Jamo'),
    (ParentRange: 6; n1: $3190; n2: $319F; nm: 'Kanbun'),
    (ParentRange: 6; n1: $3200; n2: $32FF; nm: 'Enclosed CJK Letters and Months'),
    (ParentRange: 6; n1: $3300; n2: $33FF; nm: 'CJK Compatibility'),
    (ParentRange: 6; n1: $4E00; n2: $9FFF; nm: 'CJK Unified Ideographs'),
    (ParentRange: 6; n1: $AC00; n2: $D7A3; nm: 'Hangul Syllables'),
    (ParentRange: 4; n1: $D800; n2: $DFFF; nm: 'Surrogates - NYI'),
    (ParentRange: 4; n1: $E000; n2: $F8FF; nm: 'Private Use Area'),
    (ParentRange: 6; n1: $F900; n2: $FAFF; nm: 'CJK Compatibility Ideographs'),
    (ParentRange: 4; n1: $FB00; n2: $FB4F; nm: 'Alphabetic Presentation Forms'),
    (ParentRange: 5; n1: $FB50; n2: $FDFF; nm: 'Arabic Presentation Forms'),
    (ParentRange: 4; n1: $FE20; n2: $FE2F; nm: 'Combining Half Marks'),
    (ParentRange: 6; n1: $FE30; n2: $FE4F; nm: 'CJK Compatibility Forms'),
    (ParentRange: 4; n1: $FE50; n2: $FE6F; nm: 'Small Form Variants'),
    (ParentRange: 5; n1: $FE70; n2: $FEFE; nm: 'Arabic Presentation Forms-B'),
    (ParentRange: 4; n1: $FF00; n2: $FFEE; nm: 'Halfwidth and Fullwidth Forms'),

    (ParentRange: 7; n1:   $0000; n2:   $FFFF; nm: 'Plane 0'),
    (ParentRange: 7; n1:  $10000; n2:  $1FFFF; nm: 'Plane 1'),
    (ParentRange: 7; n1:  $20000; n2:  $2FFFF; nm: 'Plane 2'),
    (ParentRange: 7; n1:  $30000; n2:  $3FFFF; nm: 'Plane 3'),
    (ParentRange: 7; n1:  $40000; n2:  $4FFFF; nm: 'Plane 4'),
    (ParentRange: 7; n1:  $50000; n2:  $5FFFF; nm: 'Plane 5'),
    (ParentRange: 7; n1:  $60000; n2:  $6FFFF; nm: 'Plane 6'),
    (ParentRange: 7; n1:  $70000; n2:  $7FFFF; nm: 'Plane 7'),
    (ParentRange: 7; n1:  $80000; n2:  $8FFFF; nm: 'Plane 8'),
    (ParentRange: 7; n1:  $90000; n2:  $9FFFF; nm: 'Plane 9'),
    (ParentRange: 7; n1:  $A0000; n2:  $AFFFF; nm: 'Plane 10'),
    (ParentRange: 7; n1:  $B0000; n2:  $BFFFF; nm: 'Plane 11'),
    (ParentRange: 7; n1:  $C0000; n2:  $CFFFF; nm: 'Plane 12'),
    (ParentRange: 7; n1:  $D0000; n2:  $DFFFF; nm: 'Plane 13'),
    (ParentRange: 7; n1:  $E0000; n2:  $EFFFF; nm: 'Plane 14'),
    (ParentRange: 7; n1:  $F0000; n2:  $FFFFF; nm: 'Plane 15'),
    (ParentRange: 7; n1: $100000; n2: $10FFFF; nm: 'Plane 16'));

implementation

end.
 
