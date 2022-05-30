(*
  Name:             UnicodeBlocks
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    18 Feb 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    12 Mar 2010 - mcdurdin - I2219 - Add Unicode 5.2 blocks
                    18 Feb 2011 - mcdurdin - I2706 - Add Unicode 6.0 blocks
*)
unit UnicodeBlocks;

interface

type
  TUnicodeBlockType = (ubtComplex, ubtRTL, ubtFarEast, ubtSupplementary);
  TUnicodeBlockTypes = set of TUnicodeBlockType;

  TUnicodeBlockData = record
    Ch1, Ch2: Integer;
    Name, CleanName: string;
    Types: TUnicodeBlockTypes;
    UniscribeVersion: string;
    CharacterCount: Integer;
{    IsComplex: Boolean;
    IsRTL: Boolean;
    IsFarEast: Boolean;
    IsSupp: Boolean;}
  end;

type
  TUnicodeBlockDataArray = array of TUnicodeBlockData;

const
  SUnicodeBlocks: array[0..208] of TUnicodeBlockData =
  (
(Ch1: $0000; Ch2: $007F; Name: 'Basic Latin';                 CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $0080; Ch2: $00FF; Name: 'Latin-1 Supplement';          CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $0100; Ch2: $017F; Name: 'Latin Extended-A';            CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $0180; Ch2: $024F; Name: 'Latin Extended-B';            CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $0250; Ch2: $02AF; Name: 'IPA Extensions';              CleanName: 'IPA'; Types: []; UniscribeVersion: ''),
(Ch1: $02B0; Ch2: $02FF; Name: 'Spacing Modifier Letters';    CleanName: 'Special'; Types: []; UniscribeVersion: ''),
(Ch1: $0300; Ch2: $036F; Name: 'Combining Diacritical Marks'; CleanName: 'Diacritics'; Types: []; UniscribeVersion: ''),
(Ch1: $0370; Ch2: $03FF; Name: 'Greek and Coptic';            Types: []; UniscribeVersion: ''),
(Ch1: $0400; Ch2: $04FF; Name: 'Cyrillic';                    Types: []; UniscribeVersion: ''),
(Ch1: $0500; Ch2: $052F; Name: 'Cyrillic Supplement';         CleanName: 'Cyrillic'; Types: []; UniscribeVersion: ''),
(Ch1: $0530; Ch2: $058F; Name: 'Armenian';                    Types: []; UniscribeVersion: ''),
(Ch1: $0590; Ch2: $05FF; Name: 'Hebrew';                      Types: [ ubtRTL  ]; UniscribeVersion: '1.405.2416.1'),
(Ch1: $0600; Ch2: $06FF; Name: 'Arabic';                      Types: [ ubtRTL  ]; UniscribeVersion: '1.325.2195.6692'),
(Ch1: $0700; Ch2: $074F; Name: 'Syriac';                      Types: [ ubtRTL  ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $0750; Ch2: $077F; Name: 'Arabic Supplement';           CleanName: 'Arabic';   Types: [ ubtRTL  ]; UniscribeVersion: '1.325.2195.6692'),
(Ch1: $0780; Ch2: $07BF; Name: 'Thaana';                      Types: [ ubtRTL  ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $07C0; Ch2: $07FF; Name: 'NKo';                         Types: [ ubtRTL  ]; UniscribeVersion: ''),
(Ch1: $0800; Ch2: $083F; Name: 'Samaritan';                   Types: [ubtRTL];    UniscribeVersion: ''), // I2219
(Ch1: $0840; Ch2: $085F; Name: 'Mandaic';                     Types: [ubtRTL];    UniscribeVersion: ''), // I2706
(Ch1: $0900; Ch2: $097F; Name: 'Devanagari';                  Types: [ubtComplex   ]; UniscribeVersion: '1.325.2180.1'),
(Ch1: $0980; Ch2: $09FF; Name: 'Bengali';                     Types: [ubtComplex   ]; UniscribeVersion: '1.420.2600.2180'),
(Ch1: $0A00; Ch2: $0A7F; Name: 'Gurmukhi'; Types: [ubtComplex   ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $0A80; Ch2: $0AFF; Name: 'Gujarati'; Types: [ubtComplex   ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $0B00; Ch2: $0B7F; Name: 'Oriya'; Types: [ubtComplex   ]; UniscribeVersion: '1.626.5756.0'),
(Ch1: $0B80; Ch2: $0BFF; Name: 'Tamil'; Types: [ubtComplex   ]; UniscribeVersion: '1.325.2180.1'),
(Ch1: $0C00; Ch2: $0C7F; Name: 'Telugu'; Types: [ubtComplex   ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $0C80; Ch2: $0CFF; Name: 'Kannada'; Types: [ubtComplex   ]; UniscribeVersion: '1.407.2600.0'),
(Ch1: $0D00; Ch2: $0D7F; Name: 'Malayalam'; Types: [ubtComplex   ]; UniscribeVersion: '1.420.2600.2180'),
(Ch1: $0D80; Ch2: $0DFF; Name: 'Sinhala'; Types: [ubtComplex   ]; UniscribeVersion: '1.601.5022.8'),
(Ch1: $0E00; Ch2: $0E7F; Name: 'Thai'; Types: [ubtComplex   ]; UniscribeVersion: '1.325.2180.1'),
(Ch1: $0E80; Ch2: $0EFF; Name: 'Lao'; Types: [ubtComplex   ]; UniscribeVersion: '1.325.2180.1'),
(Ch1: $0F00; Ch2: $0FFF; Name: 'Tibetan'; Types: [ubtComplex   ]; UniscribeVersion: '1.453.3665.0'),
(Ch1: $1000; Ch2: $109F; Name: 'Myanmar'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $10A0; Ch2: $10FF; Name: 'Georgian'; Types: []; UniscribeVersion: ''),
(Ch1: $1100; Ch2: $11FF; Name: 'Hangul Jamo'; CleanName: 'Korean'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $1200; Ch2: $137F; Name: 'Ethiopic'; Types: []; UniscribeVersion: ''),
(Ch1: $1380; Ch2: $139F; Name: 'Ethiopic Supplement'; CleanName: 'Ethiopic'; Types: []; UniscribeVersion: ''),
(Ch1: $13A0; Ch2: $13FF; Name: 'Cherokee'; Types: []; UniscribeVersion: ''),
(Ch1: $1400; Ch2: $167F; Name: 'Unified Canadian Aboriginal Syllabics'; CleanName: 'Syllabics'; Types: []; UniscribeVersion: ''),
(Ch1: $1680; Ch2: $169F; Name: 'Ogham'; Types: []; UniscribeVersion: ''),
(Ch1: $16A0; Ch2: $16FF; Name: 'Runic'; Types: []; UniscribeVersion: ''),
(Ch1: $1700; Ch2: $171F; Name: 'Tagalog'; Types: []; UniscribeVersion: ''),
(Ch1: $1720; Ch2: $173F; Name: 'Hanunoo'; Types: []; UniscribeVersion: ''),
(Ch1: $1740; Ch2: $175F; Name: 'Buhid'; Types: []; UniscribeVersion: ''),
(Ch1: $1760; Ch2: $177F; Name: 'Tagbanwa'; Types: []; UniscribeVersion: ''),
(Ch1: $1780; Ch2: $17FF; Name: 'Khmer'; Types: [ubtComplex   ]; UniscribeVersion: '1.471.4063.0'),
(Ch1: $1800; Ch2: $18AF; Name: 'Mongolian'; Types: [ubtComplex   ]; UniscribeVersion: '1.606.5065.1'),
(Ch1: $18B0; Ch2: $18FF; Name: 'Unified Canadian Aboriginal Syllabics Extended'; CleanName: 'Syllabics'; Types: []; UniscribeVersion: ''), // I2219
(Ch1: $1900; Ch2: $194F; Name: 'Limbu'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $1950; Ch2: $197F; Name: 'Tai Le'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $1980; Ch2: $19DF; Name: 'New Tai Lue'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $19E0; Ch2: $19FF; Name: 'Khmer Symbols'; CleanName: 'Khmer'; Types: [ubtComplex   ]; UniscribeVersion: '1.471.4063.0'),
(Ch1: $1A00; Ch2: $1A1F; Name: 'Buginese'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $1A20; Ch2: $1AAF; Name: 'Tai Tham'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $1B00; Ch2: $1B7F; Name: 'Balinese'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $1B80; Ch2: $1BBF; Name: 'Sundanese'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $1BC0; Ch2: $1BFF; Name: 'Batak';  Types: [ubtComplex   ]; UniscribeVersion: ''), // I2706
(Ch1: $1C00; Ch2: $1C4F; Name: 'Lepcha'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $1C50; Ch2: $1C7F; Name: 'Ol Chiki'; Types: [   ]; UniscribeVersion: ''), // I2219
(Ch1: $1CD0; Ch2: $1CFF; Name: 'Vedic Extensions'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $1D00; Ch2: $1D7F; Name: 'Phonetic Extensions'; CleanName: 'IPA'; Types: []; UniscribeVersion: ''),
(Ch1: $1D80; Ch2: $1DBF; Name: 'Phonetic Extensions Supplement'; CleanName: 'IPA'; Types: []; UniscribeVersion: ''),
(Ch1: $1DC0; Ch2: $1DFF; Name: 'Combining Diacritical Marks Supplement'; CleanName: 'Diacritics'; Types: []; UniscribeVersion: ''),
(Ch1: $1E00; Ch2: $1EFF; Name: 'Latin Extended Additional'; CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $1F00; Ch2: $1FFF; Name: 'Greek Extended'; CleanName: 'Greek'; Types: []; UniscribeVersion: ''),
(Ch1: $2000; Ch2: $206F; Name: 'General Punctuation'; CleanName: 'Punctuation'; Types: []; UniscribeVersion: ''),
(Ch1: $2070; Ch2: $209F; Name: 'Superscripts and Subscripts'; CleanName: 'Special'; Types: []; UniscribeVersion: ''),
(Ch1: $20A0; Ch2: $20CF; Name: 'Currency Symbols'; Types: []; UniscribeVersion: ''),
(Ch1: $20D0; Ch2: $20FF; Name: 'Combining Diacritical Marks for Symbols'; CleanName: 'Diacritics'; Types: []; UniscribeVersion: ''),
(Ch1: $2100; Ch2: $214F; Name: 'Letterlike Symbols'; Types: []; UniscribeVersion: ''),
(Ch1: $2150; Ch2: $218F; Name: 'Number Forms'; Types: []; UniscribeVersion: ''),
(Ch1: $2190; Ch2: $21FF; Name: 'Arrows'; Types: []; UniscribeVersion: ''),
(Ch1: $2200; Ch2: $22FF; Name: 'Mathematical Operators'; Types: []; UniscribeVersion: ''),
(Ch1: $2300; Ch2: $23FF; Name: 'Miscellaneous Technical'; Types: []; UniscribeVersion: ''),
(Ch1: $2400; Ch2: $243F; Name: 'Control Pictures'; Types: []; UniscribeVersion: ''),
(Ch1: $2440; Ch2: $245F; Name: 'Optical Character Recognition'; Types: []; UniscribeVersion: ''),
(Ch1: $2460; Ch2: $24FF; Name: 'Enclosed Alphanumerics'; Types: []; UniscribeVersion: ''),
(Ch1: $2500; Ch2: $257F; Name: 'Box Drawing'; Types: []; UniscribeVersion: ''),
(Ch1: $2580; Ch2: $259F; Name: 'Block Elements'; Types: []; UniscribeVersion: ''),
(Ch1: $25A0; Ch2: $25FF; Name: 'Geometric Shapes'; Types: []; UniscribeVersion: ''),
(Ch1: $2600; Ch2: $26FF; Name: 'Miscellaneous Symbols'; Types: []; UniscribeVersion: ''),
(Ch1: $2700; Ch2: $27BF; Name: 'Dingbats'; Types: []; UniscribeVersion: ''),
(Ch1: $27C0; Ch2: $27EF; Name: 'Miscellaneous Mathematical Symbols-A'; Types: []; UniscribeVersion: ''),
(Ch1: $27F0; Ch2: $27FF; Name: 'Supplemental Arrows-A'; Types: []; UniscribeVersion: ''),
(Ch1: $2800; Ch2: $28FF; Name: 'Braille Patterns'; Types: []; UniscribeVersion: ''),
(Ch1: $2900; Ch2: $297F; Name: 'Supplemental Arrows-B'; Types: []; UniscribeVersion: ''),
(Ch1: $2980; Ch2: $29FF; Name: 'Miscellaneous Mathematical Symbols-B'; Types: []; UniscribeVersion: ''),
(Ch1: $2A00; Ch2: $2AFF; Name: 'Supplemental Mathematical Operators'; Types: []; UniscribeVersion: ''),
(Ch1: $2B00; Ch2: $2BFF; Name: 'Miscellaneous Symbols and Arrows'; Types: []; UniscribeVersion: ''),
(Ch1: $2C00; Ch2: $2C5F; Name: 'Glagolitic'; Types: []; UniscribeVersion: ''),
(Ch1: $2C60; Ch2: $2C7F; Name: 'Latin Extended-C'; CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $2C80; Ch2: $2CFF; Name: 'Coptic'; Types: []; UniscribeVersion: ''),
(Ch1: $2D00; Ch2: $2D2F; Name: 'Georgian Supplement'; CleanName: 'Georgian'; Types: []; UniscribeVersion: ''),
(Ch1: $2D30; Ch2: $2D7F; Name: 'Tifinagh'; Types: []; UniscribeVersion: ''),
(Ch1: $2D80; Ch2: $2DDF; Name: 'Ethiopic Extended'; CleanName: 'Ethiopic'; Types: []; UniscribeVersion: ''),
(Ch1: $2DE0; Ch2: $2DFF; Name: 'Cyrillic Extended-A'; CleanName: 'Cyrillic'; Types: []; UniscribeVersion: ''), // I2219
(Ch1: $2E00; Ch2: $2E7F; Name: 'Supplemental Punctuation'; Types: []; UniscribeVersion: ''),
(Ch1: $2E80; Ch2: $2EFF; Name: 'CJK Radicals Supplement'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $2F00; Ch2: $2FDF; Name: 'Kangxi Radicals'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $2FF0; Ch2: $2FFF; Name: 'Ideographic Description Characters'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3000; Ch2: $303F; Name: 'CJK Symbols and Punctuation'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3040; Ch2: $309F; Name: 'Hiragana'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $30A0; Ch2: $30FF; Name: 'Katakana'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3100; Ch2: $312F; Name: 'Bopomofo'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3130; Ch2: $318F; Name: 'Hangul Compatibility Jamo'; CleanName: 'Korean'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3190; Ch2: $319F; Name: 'Kanbun'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $31A0; Ch2: $31BF; Name: 'Bopomofo Extended'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $31C0; Ch2: $31EF; Name: 'CJK Strokes'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $31F0; Ch2: $31FF; Name: 'Katakana Phonetic Extensions'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3200; Ch2: $32FF; Name: 'Enclosed CJK Letters and Months'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3300; Ch2: $33FF; Name: 'CJK Compatibility'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $3400; Ch2: $4DBF; Name: 'CJK Unified Ideographs Extension A'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $4DC0; Ch2: $4DFF; Name: 'Yijing Hexagram Symbols'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $4E00; Ch2: $9FFF; Name: 'CJK Unified Ideographs'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $A000; Ch2: $A48F; Name: 'Yi Syllables'; CleanName: 'Yi'; Types: []; UniscribeVersion: ''),
(Ch1: $A490; Ch2: $A4CF; Name: 'Yi Radicals'; CleanName: 'Yi'; Types: []; UniscribeVersion: ''),
(Ch1: $A4D0; Ch2: $A4FF; Name: 'Lisu'; Types: []; UniscribeVersion: ''),  // I2219
(Ch1: $A500; Ch2: $A63F; Name: 'Vai'; Types: []; UniscribeVersion: ''),  // I2219
(Ch1: $A640; Ch2: $A69F; Name: 'Cyrillic Extended-B'; CleanName: 'Cyrillic'; Types: []; UniscribeVersion: ''),  // I2219
(Ch1: $A6A0; Ch2: $A6FF; Name: 'Bamum'; Types: []; UniscribeVersion: ''),  // I2219
(Ch1: $A700; Ch2: $A71F; Name: 'Modifier Tone Letters'; CleanName: 'Diacritics'; Types: []; UniscribeVersion: ''),
(Ch1: $A720; Ch2: $A7FF; Name: 'Latin Extended-D'; CleanName: 'Latin'; Types: []; UniscribeVersion: ''),
(Ch1: $A800; Ch2: $A82F; Name: 'Syloti Nagri'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $A830; Ch2: $A83F; Name: 'Common Indic Number Forms'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $A840; Ch2: $A87F; Name: 'Phags-pa'; Types: [ubtComplex   ]; UniscribeVersion: ''),
(Ch1: $A880; Ch2: $A8DF; Name: 'Saurashtra'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $A8E0; Ch2: $A8FF; Name: 'Devanagari Extended'; CleanName: 'Devanagari'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $A900; Ch2: $A92F; Name: 'Kayah Li'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $A930; Ch2: $A95F; Name: 'Rejang'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $A960; Ch2: $A97F; Name: 'Hangul Jamo Extended-A'; CleanName: 'Korean'; Types: [ubtFarEast   ]; UniscribeVersion: ''), // I2219
(Ch1: $A980; Ch2: $A0DF; Name: 'Javanese'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $AA00; Ch2: $AA5F; Name: 'Cham'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $AA60; Ch2: $AA7F; Name: 'Myanmar Extended-A'; CleanName: 'Myanmar'; Types: [ubtComplex   ]; UniscribeVersion: ''),// I2219
(Ch1: $AA80; Ch2: $AADF; Name: 'Tai Viet'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $AB00; Ch2: $AB2F; Name: 'Ethiopic Extended-A'; CleanName: 'Ethiopic'; Types: []; UniscribeVersion: ''), // I2706
(Ch1: $ABC0; Ch2: $ABFF; Name: 'Meetei Mayek'; Types: [ubtComplex   ]; UniscribeVersion: ''), // I2219
(Ch1: $AC00; Ch2: $D7AF; Name: 'Hangul Syllables'; CleanName: 'Korean'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $D7B0; Ch2: $D7FF; Name: 'Hangul Jamo Extended-B'; CleanName: 'Korean'; Types: [  ubtFarEast ]; UniscribeVersion: ''), // I2219
(Ch1: $D800; Ch2: $DB7F; Name: 'High Surrogates'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $DB80; Ch2: $DBFF; Name: 'High Private Use Surrogates'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $DC00; Ch2: $DFFF; Name: 'Low Surrogates'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $E000; Ch2: $F8FF; Name: 'Private Use Area'; Types: []; UniscribeVersion: ''),
(Ch1: $F900; Ch2: $FAFF; Name: 'CJK Compatibility Ideographs'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $FB00; Ch2: $FB4F; Name: 'Alphabetic Presentation Forms'; Types: []; UniscribeVersion: ''),
(Ch1: $FB50; Ch2: $FDFF; Name: 'Arabic Presentation Forms-A'; CleanName: 'Arabic'; Types: [ ubtRTL  ]; UniscribeVersion: '1.325.2195.6692'),
(Ch1: $FE00; Ch2: $FE0F; Name: 'Variation Selectors'; Types: []; UniscribeVersion: ''),
(Ch1: $FE10; Ch2: $FE1F; Name: 'Vertical Forms'; Types: []; UniscribeVersion: ''),
(Ch1: $FE20; Ch2: $FE2F; Name: 'Combining Half Marks'; Types: []; UniscribeVersion: ''),
(Ch1: $FE30; Ch2: $FE4F; Name: 'CJK Compatibility Forms'; CleanName: 'CJK'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $FE50; Ch2: $FE6F; Name: 'Small Form Variants'; Types: []; UniscribeVersion: ''),
(Ch1: $FE70; Ch2: $FEFF; Name: 'Arabic Presentation Forms-B'; CleanName: 'Arabic'; Types: [ ubtRTL  ]; UniscribeVersion: '1.325.2195.6692'),
(Ch1: $FF00; Ch2: $FFEF; Name: 'Halfwidth and Fullwidth Forms'; Types: [  ubtFarEast ]; UniscribeVersion: ''),
(Ch1: $FFF0; Ch2: $FFFF; Name: 'Specials'; Types: []; UniscribeVersion: ''),
(Ch1: $10000; Ch2: $1007F; Name: 'Linear B Syllabary'; CleanName: 'Linear B'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10080; Ch2: $100FF; Name: 'Linear B Ideograms'; CleanName: 'Linear B'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10100; Ch2: $1013F; Name: 'Aegean Numbers'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10140; Ch2: $1018F; Name: 'Ancient Greek Numbers'; CleanName: 'Greek'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10190; Ch2: $101CF; Name: 'Ancient Symbols'; CleanName: 'Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $101D0; Ch2: $101FF; Name: 'Phaistos Disk'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10280; Ch2: $1029F; Name: 'Lycian'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $102A0; Ch2: $102DF; Name: 'Carian'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10300; Ch2: $1032F; Name: 'Old Italic'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10330; Ch2: $1034F; Name: 'Gothic'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10380; Ch2: $1039F; Name: 'Ugaritic'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $103A0; Ch2: $103DF; Name: 'Old Persian'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10400; Ch2: $1044F; Name: 'Deseret'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10450; Ch2: $1047F; Name: 'Shavian'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10480; Ch2: $104AF; Name: 'Osmanya'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10800; Ch2: $1083F; Name: 'Cypriot Syllabary'; CleanName: 'Cypriot'; Types: [ ubtRTL,  ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10840; Ch2: $1085F; Name: 'Imperial Aramaic'; Types: [ ubtRTL,  ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10900; Ch2: $1091F; Name: 'Phoenician'; Types: [ ubtRTL,  ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10920; Ch2: $1093F; Name: 'Lydian'; Types: [ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10A00; Ch2: $10A5F; Name: 'Kharoshthi'; Types: [ubtComplex,   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $10A60; Ch2: $10A7F; Name: 'Old South Arabian'; Types: [ubtRTL,   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10B00; Ch2: $10B3F; Name: 'Avestan'; Types: [ubtComplex, ubtRTL, ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10B40; Ch2: $10B5F; Name: 'Inscriptional Parthian'; Types: [ubtComplex, ubtRTL,  ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10B60; Ch2: $10B7F; Name: 'Inscriptional Pahlavi'; Types: [ubtComplex,  ubtRTL,   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10C00; Ch2: $10C4F; Name: 'Old Turkic'; Types: [ubtComplex, ubtRTL,  ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $10E60; Ch2: $10E7F; Name: 'Rumi Numeral Symbols'; Types: [ubtComplex,   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $11000; Ch2: $1107F; Name: 'Brahmi'; Types: [ubtComplex,   ubtSupplementary]; UniscribeVersion: ''), // I2706
(Ch1: $11080; Ch2: $110CF; Name: 'Kaithi'; Types: [ubtComplex,   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $12000; Ch2: $123FF; Name: 'Cuneiform'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $12400; Ch2: $1247F; Name: 'Cuneiform Numbers and Punctuation'; CleanName: 'Cuneiform'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $13000; Ch2: $1342F; Name: 'Egyptian Hieroglyphics'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $16800; Ch2: $16A3F; Name: 'Bamum Supplement'; Types: [ubtSupplementary]; UniscribeVersion: ''), // I2706
(Ch1: $1B000; Ch2: $1B0FF; Name: 'Kana Supplement'; Types: [ubtSupplementary]; UniscribeVersion: ''), // I2706
(Ch1: $1D000; Ch2: $1D0FF; Name: 'Byzantine Musical Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1D100; Ch2: $1D1FF; Name: 'Musical Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1D200; Ch2: $1D24F; Name: 'Ancient Greek Musical Notation'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1D300; Ch2: $1D35F; Name: 'Tai Xuan Jing Symbols'; Types: [  ubtFarEast, ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1D360; Ch2: $1D37F; Name: 'Counting Rod Numerals'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1D400; Ch2: $1D7FF; Name: 'Mathematical Alphanumeric Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $1F000; Ch2: $1F02F; Name: 'Mahjong Tiles'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $1F030; Ch2: $1F09F; Name: 'Domino Tiles'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2219
(Ch1: $1F0A0; Ch2: $1F0FF; Name: 'Playing Cards'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $1F100; Ch2: $1F1FF; Name: 'Enclosed Alphanumeric Supplement'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $1F200; Ch2: $1F2FF; Name: 'Enclosed Ideographic Supplement'; Types: [   ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $1F300; Ch2: $1F5FF; Name: 'Miscellaneous Symbols And Pictographs'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $1F600; Ch2: $1F64F; Name: 'Emoticons'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $1F680; Ch2: $1F6FF; Name: 'Transport And Map Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $1F700; Ch2: $1F77F; Name: 'Alchemical Symbols'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $20000; Ch2: $2A6DF; Name: 'CJK Unified Ideographs Extension B'; CleanName: 'CJK'; Types: [  ubtFarEast, ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $2A700; Ch2: $2B73F; Name: 'CJK Unified Ideographs Extension C'; CleanName: 'CJK'; Types: [  ubtFarEast, ubtSupplementary]; UniscribeVersion: ''), // I2219
(Ch1: $2B740; Ch2: $2B81F; Name: 'CJK Unified Ideographs Extension D'; Types: [   ubtSupplementary]; UniscribeVersion: ''),  // I2706
(Ch1: $2F800; Ch2: $2FA1F; Name: 'CJK Compatibility Ideographs Supplement'; CleanName: 'CJK'; Types: [  ubtFarEast, ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $E0000; Ch2: $E007F; Name: 'Tags'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $E0100; Ch2: $E01EF; Name: 'Variation Selectors Supplement'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $F0000; Ch2: $FFFFF; Name: 'Supplementary Private Use Area-A'; Types: [   ubtSupplementary]; UniscribeVersion: ''),
(Ch1: $100000; Ch2: $10FFFF; Name: 'Supplementary Private Use Area-B'; Types: [   ubtSupplementary]; UniscribeVersion: '')
);

implementation

end.
