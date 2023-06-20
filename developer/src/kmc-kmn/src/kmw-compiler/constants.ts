export enum TRequiredKey {
  K_LOPT, K_BKSP, K_ENTER
}; // I4447

export const
  CRequiredKeys: TRequiredKey[] = [TRequiredKey.K_LOPT, TRequiredKey.K_BKSP, TRequiredKey.K_ENTER]; // I4447

// See also builder.js: specialCharacters; web/source/osk/oskKey.ts: specialCharacters
export const
  CSpecialText10: string = '*Shift*\0*Enter*\0*Tab*\0*BkSp*\0*Menu*\0*Hide*\0*Alt*\0*Ctrl*\0*Caps*\0' +
    '*ABC*\0*abc*\0*123*\0*Symbol*\0*Currency*\0*Shifted*\0*AltGr*\0*TabLeft*',
  // these names were added in Keyman 14
  CSpecialText14: string = '*LTREnter*\0*LTRBkSp*\0*RTLEnter*\0*RTLBkSp*\0*ShiftLock*\0*ShiftedLock*\0*ZWNJ*\0*ZWNJiOS*\0*ZWNJAndroid*', CSpecialText14ZWNJ: string = '*ZWNJ*\0*ZWNJiOS*\0*ZWNJAndroid*', CSpecialText14Map: string[][] = [
    ['*LTREnter*', '*Enter*'],
    ['*LTRBkSp*', '*BkSp*'],
    ['*RTLEnter*', '*Enter*'],
    ['*RTLBkSp*', '*BkSp*'],
    ['*ShiftLock*', '*Shift*'],
    ['*ShiftedLock*', '*Shifted*'],
    ['*ZWNJ*', '<|>'],
    ['*ZWNJiOS*', '<|>'],
    ['*ZWNJAndroid*', '<|>']
  ];


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
  USEnglishShift: string   = 0xFF + '~'  + '!@#$%^&*()' + '_' +  '+'  + 'QWERTYUIOP' + '{'  + '}'  + '|'   + 'ASDFGHJKL' + ':'  + '"'  + 'ZXCVBNM' + '<'  + '>'  + '?',
  USEnglishValues: string  = 0x20 + 0xc0 + '1234567890' + 0xbd + 0xbb + 'QWERTYUIOP' + 0xdb + 0xdd + 0xdc  + 'ASDFGHJKL' + 0xba + 0xde + 'ZXCVBNM' + 0xbc + 0xbe + 0xbf,
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