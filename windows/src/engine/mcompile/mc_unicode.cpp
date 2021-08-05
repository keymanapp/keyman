#include "pch.h"

const WCHAR cp1252[256] = {
  /*0x00*/  0x0000,  //Null
  /*0x01*/  0x0001,  //Start Of Heading
  /*0x02*/  0x0002,  //Start Of Text
  /*0x03*/  0x0003,  //End Of Text
  /*0x04*/  0x0004,  //End Of Transmission
  /*0x05*/  0x0005,  //Enquiry
  /*0x06*/  0x0006,  //Acknowledge
  /*0x07*/  0x0007,  //Bell
  /*0x08*/  0x0008,  //Backspace
  /*0x09*/  0x0009,  //Horizontal Tabulation
  /*0x0a*/  0x000a,  //Line Feed
  /*0x0b*/  0x000b,  //Vertical Tabulation
  /*0x0c*/  0x000c,  //Form Feed
  /*0x0d*/  0x000d,  //Carriage Return
  /*0x0e*/  0x000e,  //Shift Out
  /*0x0f*/  0x000f,  //Shift In
  /*0x10*/  0x0010,  //Data Link Escape
  /*0x11*/  0x0011,  //Device Control One
  /*0x12*/  0x0012,  //Device Control Two
  /*0x13*/  0x0013,  //Device Control Three
  /*0x14*/  0x0014,  //Device Control Four
  /*0x15*/  0x0015,  //Negative Acknowledge
  /*0x16*/  0x0016,  //Synchronous Idle
  /*0x17*/  0x0017,  //End Of Transmission Block
  /*0x18*/  0x0018,  //Cancel
  /*0x19*/  0x0019,  //End Of Medium
  /*0x1a*/  0x001a,  //Substitute
  /*0x1b*/  0x001b,  //Escape
  /*0x1c*/  0x001c,  //File Separator
  /*0x1d*/  0x001d,  //Group Separator
  /*0x1e*/  0x001e,  //Record Separator
  /*0x1f*/  0x001f,  //Unit Separator
  /*0x20*/  0x0020,  //Space
  /*0x21*/  0x0021,  //Exclamation Mark
  /*0x22*/  0x0022,  //Quotation Mark
  /*0x23*/  0x0023,  //Number Sign
  /*0x24*/  0x0024,  //Dollar Sign
  /*0x25*/  0x0025,  //Percent Sign
  /*0x26*/  0x0026,  //Ampersand
  /*0x27*/  0x0027,  //Apostrophe
  /*0x28*/  0x0028,  //Left Parenthesis
  /*0x29*/  0x0029,  //Right Parenthesis
  /*0x2a*/  0x002a,  //Asterisk
  /*0x2b*/  0x002b,  //Plus Sign
  /*0x2c*/  0x002c,  //Comma
  /*0x2d*/  0x002d,  //Hyphen-Minus
  /*0x2e*/  0x002e,  //Full Stop
  /*0x2f*/  0x002f,  //Solidus
  /*0x30*/  0x0030,  //Digit Zero
  /*0x31*/  0x0031,  //Digit One
  /*0x32*/  0x0032,  //Digit Two
  /*0x33*/  0x0033,  //Digit Three
  /*0x34*/  0x0034,  //Digit Four
  /*0x35*/  0x0035,  //Digit Five
  /*0x36*/  0x0036,  //Digit Six
  /*0x37*/  0x0037,  //Digit Seven
  /*0x38*/  0x0038,  //Digit Eight
  /*0x39*/  0x0039,  //Digit Nine
  /*0x3a*/  0x003a,  //Colon
  /*0x3b*/  0x003b,  //Semicolon
  /*0x3c*/  0x003c,  //Less-Than Sign
  /*0x3d*/  0x003d,  //Equals Sign
  /*0x3e*/  0x003e,  //Greater-Than Sign
  /*0x3f*/  0x003f,  //Question Mark
  /*0x40*/  0x0040,  //Commercial At
  /*0x41*/  0x0041,  //Latin Capital Letter A
  /*0x42*/  0x0042,  //Latin Capital Letter B
  /*0x43*/  0x0043,  //Latin Capital Letter C
  /*0x44*/  0x0044,  //Latin Capital Letter D
  /*0x45*/  0x0045,  //Latin Capital Letter E
  /*0x46*/  0x0046,  //Latin Capital Letter F
  /*0x47*/  0x0047,  //Latin Capital Letter G
  /*0x48*/  0x0048,  //Latin Capital Letter H
  /*0x49*/  0x0049,  //Latin Capital Letter I
  /*0x4a*/  0x004a,  //Latin Capital Letter J
  /*0x4b*/  0x004b,  //Latin Capital Letter K
  /*0x4c*/  0x004c,  //Latin Capital Letter L
  /*0x4d*/  0x004d,  //Latin Capital Letter M
  /*0x4e*/  0x004e,  //Latin Capital Letter N
  /*0x4f*/  0x004f,  //Latin Capital Letter O
  /*0x50*/  0x0050,  //Latin Capital Letter P
  /*0x51*/  0x0051,  //Latin Capital Letter Q
  /*0x52*/  0x0052,  //Latin Capital Letter R
  /*0x53*/  0x0053,  //Latin Capital Letter S
  /*0x54*/  0x0054,  //Latin Capital Letter T
  /*0x55*/  0x0055,  //Latin Capital Letter U
  /*0x56*/  0x0056,  //Latin Capital Letter V
  /*0x57*/  0x0057,  //Latin Capital Letter W
  /*0x58*/  0x0058,  //Latin Capital Letter X
  /*0x59*/  0x0059,  //Latin Capital Letter Y
  /*0x5a*/  0x005a,  //Latin Capital Letter Z
  /*0x5b*/  0x005b,  //Left Square Bracket
  /*0x5c*/  0x005c,  //Reverse Solidus
  /*0x5d*/  0x005d,  //Right Square Bracket
  /*0x5e*/  0x005e,  //Circumflex Accent
  /*0x5f*/  0x005f,  //Low Line
  /*0x60*/  0x0060,  //Grave Accent
  /*0x61*/  0x0061,  //Latin Small Letter A
  /*0x62*/  0x0062,  //Latin Small Letter B
  /*0x63*/  0x0063,  //Latin Small Letter C
  /*0x64*/  0x0064,  //Latin Small Letter D
  /*0x65*/  0x0065,  //Latin Small Letter E
  /*0x66*/  0x0066,  //Latin Small Letter F
  /*0x67*/  0x0067,  //Latin Small Letter G
  /*0x68*/  0x0068,  //Latin Small Letter H
  /*0x69*/  0x0069,  //Latin Small Letter I
  /*0x6a*/  0x006a,  //Latin Small Letter J
  /*0x6b*/  0x006b,  //Latin Small Letter K
  /*0x6c*/  0x006c,  //Latin Small Letter L
  /*0x6d*/  0x006d,  //Latin Small Letter M
  /*0x6e*/  0x006e,  //Latin Small Letter N
  /*0x6f*/  0x006f,  //Latin Small Letter O
  /*0x70*/  0x0070,  //Latin Small Letter P
  /*0x71*/  0x0071,  //Latin Small Letter Q
  /*0x72*/  0x0072,  //Latin Small Letter R
  /*0x73*/  0x0073,  //Latin Small Letter S
  /*0x74*/  0x0074,  //Latin Small Letter T
  /*0x75*/  0x0075,  //Latin Small Letter U
  /*0x76*/  0x0076,  //Latin Small Letter V
  /*0x77*/  0x0077,  //Latin Small Letter W
  /*0x78*/  0x0078,  //Latin Small Letter X
  /*0x79*/  0x0079,  //Latin Small Letter Y
  /*0x7a*/  0x007a,  //Latin Small Letter Z
  /*0x7b*/  0x007b,  //Left Curly Bracket
  /*0x7c*/  0x007c,  //Vertical Line
  /*0x7d*/  0x007d,  //Right Curly Bracket
  /*0x7e*/  0x007e,  //Tilde
  /*0x7f*/  0x007f,  //Delete
  /*0x80*/  0x20ac,  //Euro Sign
  /*0x81*/  0x0081,
  /*0x82*/  0x201a,  //Single Low-9 Quotation Mark
  /*0x83*/  0x0192,  //Latin Small Letter F With Hook
  /*0x84*/  0x201e,  //Double Low-9 Quotation Mark
  /*0x85*/  0x2026,  //Horizontal Ellipsis
  /*0x86*/  0x2020,  //Dagger
  /*0x87*/  0x2021,  //Double Dagger
  /*0x88*/  0x02c6,  //Modifier Letter Circumflex Accent
  /*0x89*/  0x2030,  //Per Mille Sign
  /*0x8a*/  0x0160,  //Latin Capital Letter S With Caron
  /*0x8b*/  0x2039,  //Single Left-Pointing Angle Quotation Mark
  /*0x8c*/  0x0152,  //Latin Capital Ligature Oe
  /*0x8d*/  0x008d,
  /*0x8e*/  0x017d,  //Latin Capital Letter Z With Caron
  /*0x8f*/  0x008f,
  /*0x90*/  0x0090,
  /*0x91*/  0x2018,  //Left Single Quotation Mark
  /*0x92*/  0x2019,  //Right Single Quotation Mark
  /*0x93*/  0x201c,  //Left Double Quotation Mark
  /*0x94*/  0x201d,  //Right Double Quotation Mark
  /*0x95*/  0x2022,  //Bullet
  /*0x96*/  0x2013,  //En Dash
  /*0x97*/  0x2014,  //Em Dash
  /*0x98*/  0x02dc,  //Small Tilde
  /*0x99*/  0x2122,  //Trade Mark Sign
  /*0x9a*/  0x0161,  //Latin Small Letter S With Caron
  /*0x9b*/  0x203a,  //Single Right-Pointing Angle Quotation Mark
  /*0x9c*/  0x0153,  //Latin Small Ligature Oe
  /*0x9d*/  0x009d,
  /*0x9e*/  0x017e,  //Latin Small Letter Z With Caron
  /*0x9f*/  0x0178,  //Latin Capital Letter Y With Diaeresis
  /*0xa0*/  0x00a0,  //No-Break Space
  /*0xa1*/  0x00a1,  //Inverted Exclamation Mark
  /*0xa2*/  0x00a2,  //Cent Sign
  /*0xa3*/  0x00a3,  //Pound Sign
  /*0xa4*/  0x00a4,  //Currency Sign
  /*0xa5*/  0x00a5,  //Yen Sign
  /*0xa6*/  0x00a6,  //Broken Bar
  /*0xa7*/  0x00a7,  //Section Sign
  /*0xa8*/  0x00a8,  //Diaeresis
  /*0xa9*/  0x00a9,  //Copyright Sign
  /*0xaa*/  0x00aa,  //Feminine Ordinal Indicator
  /*0xab*/  0x00ab,  //Left-Pointing Double Angle Quotation Mark
  /*0xac*/  0x00ac,  //Not Sign
  /*0xad*/  0x00ad,  //Soft Hyphen
  /*0xae*/  0x00ae,  //Registered Sign
  /*0xaf*/  0x00af,  //Macron
  /*0xb0*/  0x00b0,  //Degree Sign
  /*0xb1*/  0x00b1,  //Plus-Minus Sign
  /*0xb2*/  0x00b2,  //Superscript Two
  /*0xb3*/  0x00b3,  //Superscript Three
  /*0xb4*/  0x00b4,  //Acute Accent
  /*0xb5*/  0x00b5,  //Micro Sign
  /*0xb6*/  0x00b6,  //Pilcrow Sign
  /*0xb7*/  0x00b7,  //Middle Dot
  /*0xb8*/  0x00b8,  //Cedilla
  /*0xb9*/  0x00b9,  //Superscript One
  /*0xba*/  0x00ba,  //Masculine Ordinal Indicator
  /*0xbb*/  0x00bb,  //Right-Pointing Double Angle Quotation Mark
  /*0xbc*/  0x00bc,  //Vulgar Fraction One Quarter
  /*0xbd*/  0x00bd,  //Vulgar Fraction One Half
  /*0xbe*/  0x00be,  //Vulgar Fraction Three Quarters
  /*0xbf*/  0x00bf,  //Inverted Question Mark
  /*0xc0*/  0x00c0,  //Latin Capital Letter A With Grave
  /*0xc1*/  0x00c1,  //Latin Capital Letter A With Acute
  /*0xc2*/  0x00c2,  //Latin Capital Letter A With Circumflex
  /*0xc3*/  0x00c3,  //Latin Capital Letter A With Tilde
  /*0xc4*/  0x00c4,  //Latin Capital Letter A With Diaeresis
  /*0xc5*/  0x00c5,  //Latin Capital Letter A With Ring Above
  /*0xc6*/  0x00c6,  //Latin Capital Ligature Ae
  /*0xc7*/  0x00c7,  //Latin Capital Letter C With Cedilla
  /*0xc8*/  0x00c8,  //Latin Capital Letter E With Grave
  /*0xc9*/  0x00c9,  //Latin Capital Letter E With Acute
  /*0xca*/  0x00ca,  //Latin Capital Letter E With Circumflex
  /*0xcb*/  0x00cb,  //Latin Capital Letter E With Diaeresis
  /*0xcc*/  0x00cc,  //Latin Capital Letter I With Grave
  /*0xcd*/  0x00cd,  //Latin Capital Letter I With Acute
  /*0xce*/  0x00ce,  //Latin Capital Letter I With Circumflex
  /*0xcf*/  0x00cf,  //Latin Capital Letter I With Diaeresis
  /*0xd0*/  0x00d0,  //Latin Capital Letter Eth
  /*0xd1*/  0x00d1,  //Latin Capital Letter N With Tilde
  /*0xd2*/  0x00d2,  //Latin Capital Letter O With Grave
  /*0xd3*/  0x00d3,  //Latin Capital Letter O With Acute
  /*0xd4*/  0x00d4,  //Latin Capital Letter O With Circumflex
  /*0xd5*/  0x00d5,  //Latin Capital Letter O With Tilde
  /*0xd6*/  0x00d6,  //Latin Capital Letter O With Diaeresis
  /*0xd7*/  0x00d7,  //Multiplication Sign
  /*0xd8*/  0x00d8,  //Latin Capital Letter O With Stroke
  /*0xd9*/  0x00d9,  //Latin Capital Letter U With Grave
  /*0xda*/  0x00da,  //Latin Capital Letter U With Acute
  /*0xdb*/  0x00db,  //Latin Capital Letter U With Circumflex
  /*0xdc*/  0x00dc,  //Latin Capital Letter U With Diaeresis
  /*0xdd*/  0x00dd,  //Latin Capital Letter Y With Acute
  /*0xde*/  0x00de,  //Latin Capital Letter Thorn
  /*0xdf*/  0x00df,  //Latin Small Letter Sharp S
  /*0xe0*/  0x00e0,  //Latin Small Letter A With Grave
  /*0xe1*/  0x00e1,  //Latin Small Letter A With Acute
  /*0xe2*/  0x00e2,  //Latin Small Letter A With Circumflex
  /*0xe3*/  0x00e3,  //Latin Small Letter A With Tilde
  /*0xe4*/  0x00e4,  //Latin Small Letter A With Diaeresis
  /*0xe5*/  0x00e5,  //Latin Small Letter A With Ring Above
  /*0xe6*/  0x00e6,  //Latin Small Ligature Ae
  /*0xe7*/  0x00e7,  //Latin Small Letter C With Cedilla
  /*0xe8*/  0x00e8,  //Latin Small Letter E With Grave
  /*0xe9*/  0x00e9,  //Latin Small Letter E With Acute
  /*0xea*/  0x00ea,  //Latin Small Letter E With Circumflex
  /*0xeb*/  0x00eb,  //Latin Small Letter E With Diaeresis
  /*0xec*/  0x00ec,  //Latin Small Letter I With Grave
  /*0xed*/  0x00ed,  //Latin Small Letter I With Acute
  /*0xee*/  0x00ee,  //Latin Small Letter I With Circumflex
  /*0xef*/  0x00ef,  //Latin Small Letter I With Diaeresis
  /*0xf0*/  0x00f0,  //Latin Small Letter Eth
  /*0xf1*/  0x00f1,  //Latin Small Letter N With Tilde
  /*0xf2*/  0x00f2,  //Latin Small Letter O With Grave
  /*0xf3*/  0x00f3,  //Latin Small Letter O With Acute
  /*0xf4*/  0x00f4,  //Latin Small Letter O With Circumflex
  /*0xf5*/  0x00f5,  //Latin Small Letter O With Tilde
  /*0xf6*/  0x00f6,  //Latin Small Letter O With Diaeresis
  /*0xf7*/  0x00f7,  //Division Sign
  /*0xf8*/  0x00f8,  //Latin Small Letter O With Stroke
  /*0xf9*/  0x00f9,  //Latin Small Letter U With Grave
  /*0xfa*/  0x00fa,  //Latin Small Letter U With Acute
  /*0xfb*/  0x00fb,  //Latin Small Letter U With Circumflex
  /*0xfc*/  0x00fc,  //Latin Small Letter U With Diaeresis
  /*0xfd*/  0x00fd,  //Latin Small Letter Y With Acute
  /*0xfe*/  0x00fe,  //Latin Small Letter Thorn
  /*0xff*/  0x00ff };//Latin Small Letter Y With Diaeresis

void InplaceUnicode(PWSTR p) {
  while(p && *p) {
    if(*p != UC_SENTINEL) {
      if(*p <= 0xFF) {
        *p = cp1252[*p];
      }
    }

    p = incxstr(p);
  }
}

BOOL ConvertKeyboardToUnicode(LPKEYBOARD kbd) {
  LPGROUP gp;
  LPSTORE sp;
  LPKEY kp;
  DWORD i, j;

  if(kbd->StartGroup[BEGIN_UNICODE] < kbd->cxGroupArray) {
    // Keyboard is already Unicode
    return TRUE;
  }

  for(sp = kbd->dpStoreArray, i = 0; i < kbd->cxStoreArray; i++, sp++) {
    InplaceUnicode(sp->dpName);
    InplaceUnicode(sp->dpString);
  }

  for(gp = kbd->dpGroupArray, i = 0; i < kbd->cxGroupArray; i++, gp++) {
    for(kp = gp->dpKeyArray, j = 0; j < gp->cxKeyArray; j++, kp++) {
      InplaceUnicode(kp->dpContext);
      InplaceUnicode(kp->dpOutput);
    }

    InplaceUnicode(gp->dpName);
    InplaceUnicode(gp->dpMatch);
    InplaceUnicode(gp->dpNoMatch);
  }

  kbd->StartGroup[BEGIN_UNICODE] = kbd->StartGroup[BEGIN_ANSI];
  kbd->StartGroup[BEGIN_ANSI] = 0xFFFFFFFF;

  return TRUE;
}
