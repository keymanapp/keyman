import { KMX } from "@keymanapp/common-types";

export enum TRequiredKey {
  K_LOPT='K_LOPT', K_BKSP='K_BKSP', K_ENTER='K_ENTER'
}; // I4447

export const
  CRequiredKeys: TRequiredKey[] = [TRequiredKey.K_LOPT, TRequiredKey.K_BKSP, TRequiredKey.K_ENTER]; // I4447

// See also builder.js: specialCharacters; web/source/osk/oskKey.ts: specialCharacters
export const
  CSpecialText10: string = '*Shift*\0*Enter*\0*Tab*\0*BkSp*\0*Menu*\0*Hide*\0*Alt*\0*Ctrl*\0*Caps*\0' +
    '*ABC*\0*abc*\0*123*\0*Symbol*\0*Currency*\0*Shifted*\0*AltGr*\0*TabLeft*\0',
  // these names were added in Keyman 14
  CSpecialText14: string = '*LTREnter*\0*LTRBkSp*\0*RTLEnter*\0*RTLBkSp*\0*ShiftLock*\0*ShiftedLock*\0*ZWNJ*\0*ZWNJiOS*\0*ZWNJAndroid*\0',
  CSpecialText14ZWNJ: string = '*ZWNJ*\0*ZWNJiOS*\0*ZWNJAndroid*',
  CSpecialText14Map: string[][] = [
    ['*LTREnter*', '*Enter*'],
    ['*LTRBkSp*', '*BkSp*'],
    ['*RTLEnter*', '*Enter*'],
    ['*RTLBkSp*', '*BkSp*'],
    ['*ShiftLock*', '*Shift*'],
    ['*ShiftedLock*', '*Shifted*'],
    ['*ZWNJ*', '<|>'],
    ['*ZWNJiOS*', '<|>'],
    ['*ZWNJAndroid*', '<|>'],
  ],
  // these names were added in Keyman 17
  CSpecialText17: string = '*Sp*\0*NBSp*\0*NarNBSp*\0*EnQ*\0*EmQ*\0*EnSp*\0*EmSp*\0*PunctSp*\0' +
    '*ThSp*\0*HSp*\0*ZWSp*\0*ZWJ*\0*WJ*\0*CGJ*\0*LTRM*\0*RTLM*\0*SH*\0*HTab*\0',
  CSpecialText17ZWNJ: string = '*ZWNJGeneric*',
  CSpecialText17Map: string[][] = [
    ['*ZWNJGeneric*', '<|>']
  ];

  // Map for checking minimum versions and Special Text
  export const CSpecialText = new Map<number, string>([
    [KMX.KMXFile.VERSION_100, CSpecialText10],
    // [KMX.KMXFile.VERSION_110, CSpecialText10], - this file version does not exist
    // [KMX.KMXFile.VERSION_120, CSpecialText10], - this file version does not exist
    // [KMX.KMXFile.VERSION_130, CSpecialText10], - this file version does not exist
    [KMX.KMXFile.VERSION_140, CSpecialText14 + CSpecialText10],
    [KMX.KMXFile.VERSION_150, CSpecialText14 + CSpecialText10],
    [KMX.KMXFile.VERSION_160, CSpecialText14 + CSpecialText10],
    [KMX.KMXFile.VERSION_170, CSpecialText17 + CSpecialText14 + CSpecialText10]
  ]);
  export const CSpecialTextMinVer = KMX.KMXFile.VERSION_100;
  export const CSpecialTextMaxVer = KMX.KMXFile.VERSION_170;

// These correspond to TSS_ values in kmx.ts
export const
KMXCodeNames: string[] = [
  '',
  'any', 'index', 'context', 'nul', 'use', 'return', 'beep', 'deadkey',
  '',
  'extended', '', 'switch', 'key', 'clearcontext', 'call',
  '', 'contextex', 'notany',
  'set', 'if', 'save', 'reset',  // I3429
  'if(&system)', 'set(&system)'];  // I3430    // I3437


export const
  USEnglishUnshift: string = ' `'        + '1234567890' + '-' +  '='  + 'qwertyuiop' + '['  + ']'  + '\\'  + 'asdfghjkl' + ';'  + '\'' + 'zxcvbnm' + ','  + '.'  + '/',
  USEnglishShift: string   = '\u00FF' + '~'  + '!@#$%^&*()' + '_' +  '+'  + 'QWERTYUIOP' + '{'  + '}'  + '|'   + 'ASDFGHJKL' + ':'  + '"'  + 'ZXCVBNM' + '<'  + '>'  + '?',
  USEnglishValues: string  = '\u0020' + '\u00c0' + '1234567890' + '\u00bd' + '\u00bb' + 'QWERTYUIOP' + '\u00db' + '\u00dd' + '\u00dc'  + 'ASDFGHJKL' + '\u00ba' + '\u00de' + 'ZXCVBNM' + '\u00bc' + '\u00be' + '\u00bf',
  UnreachableKeyCodes: number[] = [  // I4141
    0x00,    // &H0
    0x01, //VK_LBUTTON,   // &H1
    0x02, //VK_RBUTTON,   // &H2
    0x03, //VK_CANCEL,      // &H3
    0x04, //VK_MBUTTON,   // &H4
    0x05,    // &H5
    0x06,    // &H6
    0x07,    // &H7
    0x0A,    // &HA
    0x0B,    // &HB
    0x0E,    // &HE
    0x0F,    // &HF
    0x10, //VK_SHIFT,    // &H10
    0x11, //VK_CONTROL,   // &H11
    0x12, //VK_MENU,    // &H12
    0x13, //VK_PAUSE,    // &H13
    0x14, //VK_CAPITAL,    // &H14
    0x1B, //VK_ESCAPE,    // &H1B

    0x90, //VK_NUMLOCK,   // &H90
    0x91  //VK_SCROLL   // &H91
  ];

/**
 * The character set that we allow for identifiers for JavaScript
 */
export const SValidIdentifierCharSet = /[A-Za-z0-9_]/;
