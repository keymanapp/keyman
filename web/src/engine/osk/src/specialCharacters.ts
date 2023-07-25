// Defines the PUA code mapping for the various 'special' modifier/control keys on keyboards.
// `specialCharacters` must be kept in sync with the same variable in builder.js. See also CompileKeymanWeb.pas: CSpecialText10
let specialCharacters = {
  '*Shift*':    8,
  '*Enter*':    5,
  '*Tab*':      6,
  '*BkSp*':     4,
  '*Menu*':     11,
  '*Hide*':     10,
  '*Alt*':      25,
  '*Ctrl*':     1,
  '*Caps*':     3,
  '*ABC*':      16,
  '*abc*':      17,
  '*123*':      19,
  '*Symbol*':   21,
  '*Currency*': 20,
  '*Shifted*':  9,
  '*AltGr*':    2,
  '*TabLeft*':  7,
  '*LAlt*':     0x56,
  '*RAlt*':     0x57,
  '*LCtrl*':    0x58,
  '*RCtrl*':    0x59,
  '*LAltCtrl*':       0x60,
  '*RAltCtrl*':       0x61,
  '*LAltCtrlShift*':  0x62,
  '*RAltCtrlShift*':  0x63,
  '*AltShift*':       0x64,
  '*CtrlShift*':      0x65,
  '*AltCtrlShift*':   0x66,
  '*LAltShift*':      0x67,
  '*RAltShift*':      0x68,
  '*LCtrlShift*':     0x69,
  '*RCtrlShift*':     0x70,
  // Added in Keyman 14.0.
  '*LTREnter*':       0x05, // Default alias of '*Enter*'.
  '*LTRBkSp*':        0x04, // Default alias of '*BkSp*'.
  '*RTLEnter*':       0x71,
  '*RTLBkSp*':        0x72,
  '*ShiftLock*':      0x73,
  '*ShiftedLock*':    0x74,
  '*ZWNJ*':           0x75, // If this one is specified, auto-detection will kick in.
  '*ZWNJiOS*':        0x75, // The iOS version will be used by default, but the
  '*ZWNJAndroid*':    0x76, // Android platform has its own default glyph.
};

export default specialCharacters;