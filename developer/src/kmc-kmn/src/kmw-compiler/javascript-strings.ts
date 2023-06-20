import { TSentinelRecord, GetSuppChar, ExpandSentinel, incxstr, xstrlen, xstrlen_printing } from "./util.js";
import { KMX } from "@keymanapp/common-types";

import { callbacks, FMnemonic, FTabStop, IsKeyboardVersion10OrLater, IsKeyboardVersion14OrLater, nl, options } from "./compiler-globals.js";
import { KmwCompilerMessages } from "./messages.js";
import { FFix183_LadderLength, FormatModifierAsBitflags, RuleIsExcludedByPlatform } from "./write-compiled-keyboard.js";
import { KMXCodeNames, UnreachableKeyCodes, USEnglishShift, USEnglishUnshift, USEnglishValues } from "./constants.js";
import { KMWVKeyNames } from "./keymanweb-key-codes.js";

const SValidIdentifierCharSet = /[A-Za-z0-9_]/;

// TODO: lifecycle
let FUnreachableKeys: KMX.KEY[] = [];
let FCallFunctions: string[] = [];

export function JavaScript_Name(i: number, pwszName: string, KeepNameForPersistentStorage: boolean = false): string {   // I3659
  let FChanged: boolean = false;
  let p = pwszName;

  if((pwszName == null || pwszName == undefined || pwszName == '') || (!options.saveDebug && !KeepNameForPersistentStorage)) {   // I3659   // I3681
    return i.toString(10); // for uniqueness
  }
  else {
    let result = KeepNameForPersistentStorage // I3659
      ? ''   // Potential for overlap in theory but in practice we only use this for named option stores so can never overlap
      : '_'; // Ensures we cannot overlap numbered instances
    while(p.length) {
      let ch = p.charAt(0);
      if(SValidIdentifierCharSet.test(ch)) {  // I3681
        result += ch;
      } else {
        result += '_';
        FChanged = true;
      }
      p = p.substring(1);
    }
    if(!KeepNameForPersistentStorage) {
      // Ensure each transformed name is still unique
      result += '_' + i.toString(10);
      if(FChanged) {
        result += '/*'+pwszName.replace(/\*\//g, '*-/')+'*/';
      }
    } else if(FChanged) {
      // For named option stores, we are only supporting the valid identifier
      // character set, which is a breaking change in 14.0.
      callbacks.reportMessage(KmwCompilerMessages.Warn_OptionStoreNameInvalid({name:pwszName}));
    }
    return result;
  }
}

export function JavaScript_Store(fk: KMX.KEYBOARD, line: number, pwsz: string): string {
  let ch: number, rec: TSentinelRecord, result: string;
  const wcsentinel: string = String.fromCharCode(KMX.KMXFile.UC_SENTINEL);
  let n = pwsz.indexOf(wcsentinel);

  // Start:  plain text store.  Always use for < 10.0, conditionally for >= 10.0.
  if(n < 0 || !IsKeyboardVersion10OrLater()) {
    result = '"';
    while(pwsz.length) {
      if(pwsz.charCodeAt(0) == KMX.KMXFile.UC_SENTINEL) {
        result += '.'; // UC_SENTINEL values are not supported in stores for KMW < 10.0.
      } else {
        ch = GetSuppChar(pwsz, 0);
        if(ch == '"'.charCodeAt(0) || ch == '\\'.charCodeAt(0)) {
          result += '\\';
        }
        result += JavaScript_String(ch);  // I2242
      }

      // TODO: problems with supp chars
      pwsz = pwsz.substring(1);
    }
    result += '"';
  }
  else {
    result = '[';
    let x = 0;
    while(x < pwsz.length) {
      if(result != '[') {
        result += ',';
      }
      rec = ExpandSentinel(fk, pwsz, x);
      if(rec.IsSentinel) {
        if(rec.Code == KMX.KMXFile.CODE_DEADKEY) {
          result += `{t:'d',d:${rec.DeadKey.DeadKey}}`;
        }
        else if(rec.Code == KMX.KMXFile.CODE_BEEP) {
          result += `{t:'b'}`;
        }
        else { //if rec.Code = CODE_EXTENDED then
          // At some point, we may wish to filter which codes are safe to stub out like this
          // versus which ones should be an error.  The commented-out-code shows the way to
          // handle such cases.
          result += `''`;
        }
//        else
//        begin
//          //ReportError(line, CERR_SomewhereIGotItWrong, 'Internal Error: unexpected sentinel character in store definition');
//        end;
      }
      else {
        ch = GetSuppChar(pwsz, x);
        result += '"';
        // TODO:  Refactor the section below into JavaScript_String, as it's
        // quite common in our code base.
        if(ch == '"'.charCodeAt(0) || ch == '\\'.charCodeAt(0)) {
          result += '\\';
        }
        result += JavaScript_String(ch) + '"';  // I2242
      }

      x = incxstr(pwsz, x);
    }
    result += ']';
  }
  return result;
}

export function JavaScript_String(ch: number): string {  // I2242
  if(ch < 32) {
    switch(ch) {
      case 9:  return '\\t';
      case 10: return '\\n';
      case 13: return '\\r';
    }
    return '\\x' + zeroPadHex(ch, 2);
  }
  else {
    // Note: unpaired surrogates will be maintained
    return String.fromCodePoint(ch);
  }
}

function JavaScript_Rule(FTabStops: string, FElse: string, fk: KMX.KEYBOARD, fgp: KMX.GROUP, fkp: KMX.KEY): string {
  let predicate: string = '1', linecomment: string = '', FIndent: string;
  let result = '';

  if(fkp.Line > 0 && options.saveDebug) {   // I4384
    linecomment = '   // Line '+fkp.Line.toString();   // I4373
  }

  if(xstrlen(fkp.dpContext) > 0) {
    predicate = JavaScript_ContextMatch(fk, fkp, fkp.dpContext);
  }

  FIndent = FTabStops+FTabStop;
  result = `${FTabStops}${FElse}if(${predicate}){${nl}`;

  if(fgp.fUsingKeys) {
    result += `${FIndent}r=m=1;${linecomment}${JavaScript_OutputString(fk, FIndent, fkp, fkp.dpOutput, fgp)}`;    // I1959   // I3681
  }
  else {
    result += `${FIndent}m=1;${linecomment}${JavaScript_OutputString(fk, FIndent, fkp, fkp.dpOutput, fgp)}`;    // I1959   // I3681
  }

  result += `${nl}${FTabStops}}${nl}`;
  return result;
}


export function JavaScript_Rules(keyboard: KMX.KEYBOARD, fMnemonic: boolean, fgp: KMX.GROUP): string {
  let IsEqualKey = function(k1: KMX.KEY, k2: KMX.KEY): boolean {
    return (
      (JavaScript_Key(k1, FMnemonic) == JavaScript_Key(k2, FMnemonic)) &&
      (JavaScript_Shift(k1, FMnemonic) == JavaScript_Shift(k2, FMnemonic))
    );
  }

  let result = '';
  let HasRules = false;

  let processed_rule = Array(fgp.keys.length);
  for(let j = 0; j < fgp.keys.length; j++) {
    processed_rule[j] = false;
  }

  let j = 0;
  let Counter = 0;
  while(j < fgp.keys.length) {   // I1964
    let fkp = fgp.keys[j];
    if(!processed_rule[j] && !RuleIsExcludedByPlatform(keyboard, fkp)) {
      // Break down by key code
      // We know the rules are sorted by context length and then key code.
      // First pass, break the grouping down by key code.

      if(fgp.fUsingKeys) {
        result +=
          `${FTabStop+FTabStop}${HasRules?'else ':''}`+
          `if(k.KKM(e,${JavaScript_ShiftAsString(fkp, fMnemonic)},${JavaScript_KeyAsString(fkp, fMnemonic)})) {${nl}`;

        HasRules = true;
        Counter++;

        let LocalHasRules = false;
        let fkp2 = fgp.keys[j];
        let j2 = j;
        let LocalCounter = 0;
        while (j < fgp.keys.length) {
          fkp = fgp.keys[j];
          if (!processed_rule[j] && !RuleIsExcludedByPlatform(keyboard, fkp) && IsEqualKey(fkp, fkp2)) {
            processed_rule[j] = true;
            result += JavaScript_Rule(FTabStop + FTabStop + FTabStop, LocalHasRules ? 'else ' : '', keyboard, fgp, fkp);
            LocalCounter++;

            if (FFix183_LadderLength != 0 && (LocalCounter % FFix183_LadderLength) == 0) {
              // Break if/else ladders
              result += `${FTabStop+FTabStop+FTabStop}if(m) {}${nl}`;
            }
            LocalHasRules = true;
          }

          j++;
        }

        result += FTabStop + FTabStop + '}' + nl;
        j = j2 + 1;
      }
      else {
        // TODO: context character level switches instead of full context comparisons
        result += JavaScript_Rule(FTabStop + FTabStop + FTabStop, HasRules ? 'else ' : '', keyboard, fgp, fkp);
        HasRules = true;
        Counter++;
        j++;
      }

      if (FFix183_LadderLength != 0 && (Counter % FFix183_LadderLength) == 0) {
        // Break if/else ladders
        // We need to only match if no previous line is matched (i.e. m is false)
        result += `${FTabStop+FTabStop+FTabStop}if(m) {}${nl}`;
      }
    }
    else {
      j++;
    }
  }
  return result;
}

export function JavaScript_Shift(fkp: KMX.KEY, FMnemonic: boolean): number {
  if (FMnemonic) {
    if (fkp.ShiftFlags & KMX.KMXFile.VIRTUALCHARKEY) {
      callbacks.reportMessage(KmwCompilerMessages.Error_VirtualCharacterKeysNotSupportedInKeymanWeb());
      return 0;
    }

    if (fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY && fkp.Key <= 255) {
      // We prohibit K_ keys for mnemonic layouts. We don't block T_ and U_ keys.
      // TODO: this doesn't resolve the issue of, e.g. SHIFT+K_SPACE
      // https://github.com/keymanapp/keyman/issues/265
      callbacks.reportMessage(KmwCompilerMessages.Error_VirtualKeysNotValidForMnemonicLayouts());
      return 0;
    }
  }

  if (fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY) {
    if(IsKeyboardVersion10OrLater()) {
      // Full chiral modifier and state key support starts with KeymanWeb 10.0
      return fkp.ShiftFlags;
    }

    // Non-chiral support only and no support for state keys
    if (fkp.ShiftFlags & (KMX.KMXFile.LCTRLFLAG | KMX.KMXFile.RCTRLFLAG | KMX.KMXFile.LALTFLAG | KMX.KMXFile.RALTFLAG)) {   // I4118
      callbacks.reportMessage(KmwCompilerMessages.Warn_ExtendedShiftFlagsNotSupportedInKeymanWeb({flags: 'LALT, RALT, LCTRL, RCTRL'}));
    }

    if (fkp.ShiftFlags & (
      KMX.KMXFile.CAPITALFLAG | KMX.KMXFile.NOTCAPITALFLAG | KMX.KMXFile.NUMLOCKFLAG | KMX.KMXFile.NOTNUMLOCKFLAG |
      KMX.KMXFile.SCROLLFLAG | KMX.KMXFile.NOTSCROLLFLAG)) {   // I4118
      callbacks.reportMessage(KmwCompilerMessages.Warn_ExtendedShiftFlagsNotSupportedInKeymanWeb({flags: 'CAPS and NCAPS'}));
    }

    return KMX.KMXFile.ISVIRTUALKEY | (fkp.ShiftFlags & (KMX.KMXFile.K_SHIFTFLAG | KMX.KMXFile.K_CTRLFLAG | KMX.KMXFile.K_ALTFLAG));
  }

  return USEnglishShift.includes(String.fromCharCode(fkp.Key)) ? KMX.KMXFile.ISVIRTUALKEY | KMX.KMXFile.K_SHIFTFLAG : KMX.KMXFile.ISVIRTUALKEY;
}

/**
 * Returns a Javascript representation of a key modifier state, either as a constant (debug mode)
 * or as an integer.
 *
 * @param fkp         Pointer to key record
 * @param FMnemonic   True if the keyboard is a mnemonic layout
 *
 * @return string representation of the key modifier state, e.g.
 *         'modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 * /' or
 *         '16656'
 */
export function JavaScript_ShiftAsString(fkp: KMX.KEY, FMnemonic: boolean): string {
  if(!options.saveDebug) {
    return JavaScript_Shift(fkp, FMnemonic).toString();
  }
  return ' '+FormatModifierAsBitflags(JavaScript_Shift(fkp, FMnemonic));
}

export const VKeyNames = [ // from vkeys.h
// Key Codes
"K_?00",				// &H0
"K_LBUTTON",			// &H1
"K_RBUTTON",			// &H2
"K_CANCEL",		   	// &H3
"K_MBUTTON",			// &H4
"K_?05",				// &H5
"K_?06",				// &H6
"K_?07",				// &H7
"K_BKSP",	    		// &H8
"K_TAB",	    		// &H9
"K_?0A",				// &HA
"K_?0B",				// &HB
"K_KP5",		    	// &HC
"K_ENTER",				// &HD
"K_?0E",				// &HE
"K_?0F",				// &HF
"K_SHIFT",				// &H10
"K_CONTROL",			// &H11
"K_ALT",				// &H12
"K_PAUSE",				// &H13
"K_CAPS",				// &H14
"K_KANJI?15",			// &H15
"K_KANJI?16",			// &H16
"K_KANJI?17",			// &H17
"K_KANJI?18",			// &H18
"K_KANJI?19",			// &H19
"K_?1A",				// &H1A
"K_ESC",				// &H1B
"K_KANJI?1C",			// &H1C
"K_KANJI?1D",			// &H1D
"K_KANJI?1E",			// &H1E
"K_KANJI?1F",			// &H1F
"K_SPACE",				// &H20
"K_PGUP",				// &H21
"K_PGDN",				// &H22
"K_END",				// &H23
"K_HOME",				// &H24
"K_LEFT",				// &H25
"K_UP",				// &H26
"K_RIGHT",				// &H27
"K_DOWN",				// &H28
"K_SEL",				// &H29
"K_PRINT",				// &H2A
"K_EXEC",				// &H2B
"K_PRTSCN",			// &H2C
"K_INS",				// &H2D
"K_DEL",				// &H2E
"K_HELP",				// &H2F
"K_0",					// &H30
"K_1",					// &H31
"K_2",					// &H32
"K_3",					// &H33
"K_4",					// &H34
"K_5",					// &H35
"K_6",					// &H36
"K_7",					// &H37
"K_8",					// &H38
"K_9",					// &H39
"K_?3A",				// &H3A
"K_?3B",				// &H3B
"K_?3C",				// &H3C
"K_?3D",				// &H3D
"K_?3E",				// &H3E
"K_?3F",				// &H3F
"K_?40",				// &H40

"K_A",					// &H41
"K_B",					// &H42
"K_C",					// &H43
"K_D",					// &H44
"K_E",					// &H45
"K_F",					// &H46
"K_G",					// &H47
"K_H",					// &H48
"K_I",					// &H49
"K_J",					// &H4A
"K_K",					// &H4B
"K_L",					// &H4C
"K_M",					// &H4D
"K_N",					// &H4E
"K_O",					// &H4F
"K_P",					// &H50
"K_Q",					// &H51
"K_R",					// &H52
"K_S",					// &H53
"K_T",					// &H54
"K_U",					// &H55
"K_V",					// &H56
"K_W",					// &H57
"K_X",					// &H58
"K_Y",					// &H59
"K_Z",					// &H5A
"K_?5B",				// &H5B
"K_?5C",				// &H5C
"K_?5D",				// &H5D
"K_?5E",				// &H5E
"K_?5F",				// &H5F
"K_NP0",				// &H60
"K_NP1",				// &H61
"K_NP2",				// &H62
"K_NP3",				// &H63
"K_NP4",				// &H64
"K_NP5",				// &H65
"K_NP6",				// &H66
"K_NP7",				// &H67
"K_NP8",				// &H68
"K_NP9",				// &H69
"K_NPSTAR",			// &H6A
"K_NPPLUS",			// &H6B
"K_SEPARATOR",			// &H6C
"K_NPMINUS",			// &H6D
"K_NPDOT",				// &H6E
"K_NPSLASH",			// &H6F
"K_F1",				// &H70
"K_F2",				// &H71
"K_F3",				// &H72
"K_F4",				// &H73
"K_F5",				// &H74
"K_F6",				// &H75
"K_F7",				// &H76
"K_F8",				// &H77
"K_F9",				// &H78
"K_F10",				// &H79
"K_F11",				// &H7A
"K_F12",				// &H7B
"K_F13",				// &H7C
"K_F14",				// &H7D
"K_F15",				// &H7E
"K_F16",				// &H7F
"K_F17",				// &H80
"K_F18",				// &H81
"K_F19",				// &H82
"K_F20",				// &H83
"K_F21",				// &H84
"K_F22",				// &H85
"K_F23",				// &H86
"K_F24",				// &H87

"K_?88",				// &H88
"K_?89",				// &H89
"K_?8A",				// &H8A
"K_?8B",				// &H8B
"K_?8C",				// &H8C
"K_?8D",				// &H8D
"K_?8E",				// &H8E
"K_?8F",				// &H8F

"K_NUMLOCK",			// &H90
"K_SCROLL",			// &H91

"K_?92",				// &H92
"K_?93",				// &H93
"K_?94",				// &H94
"K_?95",				// &H95
"K_?96",				// &H96
"K_?97",				// &H97
"K_?98",				// &H98
"K_?99",				// &H99
"K_?9A",				// &H9A
"K_?9B",				// &H9B
"K_?9C",				// &H9C
"K_?9D",				// &H9D
"K_?9E",				// &H9E
"K_?9F",				// &H9F
"K_?A0",				// &HA0
"K_?A1",				// &HA1
"K_?A2",				// &HA2
"K_?A3",				// &HA3
"K_?A4",				// &HA4
"K_?A5",				// &HA5
"K_?A6",				// &HA6
"K_?A7",				// &HA7
"K_?A8",				// &HA8
"K_?A9",				// &HA9
"K_?AA",				// &HAA
"K_?AB",				// &HAB
"K_?AC",				// &HAC
"K_?AD",				// &HAD
"K_?AE",				// &HAE
"K_?AF",				// &HAF
"K_?B0",				// &HB0
"K_?B1",				// &HB1
"K_?B2",				// &HB2
"K_?B3",				// &HB3
"K_?B4",				// &HB4
"K_?B5",				// &HB5
"K_?B6",				// &HB6
"K_?B7",				// &HB7
"K_?B8",				// &HB8
"K_?B9",				// &HB9

"K_COLON",				// &HBA
"K_EQUAL",				// &HBB
"K_COMMA",				// &HBC
"K_HYPHEN",			// &HBD
"K_PERIOD",			// &HBE
"K_SLASH",				// &HBF
"K_BKQUOTE",			// &HC0

"K_?C1",				// &HC1
"K_?C2",				// &HC2
"K_?C3",				// &HC3
"K_?C4",				// &HC4
"K_?C5",				// &HC5
"K_?C6",				// &HC6
"K_?C7",				// &HC7
"K_?C8",				// &HC8
"K_?C9",				// &HC9
"K_?CA",				// &HCA
"K_?CB",				// &HCB
"K_?CC",				// &HCC
"K_?CD",				// &HCD
"K_?CE",				// &HCE
"K_?CF",				// &HCF
"K_?D0",				// &HD0
"K_?D1",				// &HD1
"K_?D2",				// &HD2
"K_?D3",				// &HD3
"K_?D4",				// &HD4
"K_?D5",				// &HD5
"K_?D6",				// &HD6
"K_?D7",				// &HD7
"K_?D8",				// &HD8
"K_?D9",				// &HD9
"K_?DA",				// &HDA

"K_LBRKT",				// &HDB
"K_BKSLASH",			// &HDC
"K_RBRKT",				// &HDD
"K_QUOTE",				// &HDE
"K_oDF",				// &HDF
"K_oE0",				// &HE0
"K_oE1",				// &HE1
"K_oE2",				// &HE2
"K_oE3",				// &HE3
"K_oE4",				// &HE4

"K_?E5",				// &HE5

"K_oE6",				// &HE6

"K_?E7",				// &HE7
"K_?E8",				// &HE8

"K_oE9",				// &HE9
"K_oEA",				// &HEA
"K_oEB",				// &HEB
"K_oEC",				// &HEC
"K_oED",				// &HED
"K_oEE",				// &HEE
"K_oEF",				// &HEF
"K_oF0",				// &HF0
"K_oF1",				// &HF1
"K_oF2",				// &HF2
"K_oF3",				// &HF3
"K_oF4",				// &HF4
"K_oF5",				// &HF5

"K_?F6",				// &HF6
"K_?F7",				// &HF7
"K_?F8",				// &HF8
"K_?F9",				// &HF9
"K_?FA",				// &HFA
"K_?FB",				// &HFB
"K_?FC",				// &HFC
"K_?FD",				// &HFD
"K_?FE",				// &HFE
"K_?FF"				// &HFF
];

const enum
  TKeymanWebTouchStandardKey {
    K_LOPT = 50001,
    K_ROPT = 50002,
    K_NUMERALS = 50003,
    K_SYMBOLS = 50004,
    K_CURRENCIES = 50005,
    K_UPPER = 50006,
    K_LOWER = 50007,
    K_ALPHA = 50008,
    K_SHIFTED = 50009,
    K_ALTGR = 50010,
    K_TABBACK = 50011,
    K_TABFWD = 50012
  };

function FormatKeyForErrorMessage(fkp: KMX.KEY, FMnemonic: boolean): string {
  function FormatShift(ShiftFlags: number): string {
    const
      mask: string[] = [
        'LCTRL',             // 0X0001
        'RCTRL',             // 0X0002
        'LALT',              // 0X0004
        'RALT',              // 0X0008

        'SHIFT',             // 0X0010
        'CTRL',              // 0X0020
        'ALT',               // 0X0040

        '???',               // Reserved

        'CAPS',              // 0X0100
        'NCAPS',             // 0X0200

        'NUMLOCK',           // 0X0400
        'NNUMLOCK',          // 0X0800

        'SCROLLLOCK',        // 0X1000
        'NSCROLLLOCK'        // 0X2000
      ];

    let result = '';
    for(let i = 0; i < mask.length; i++) {
      if(ShiftFlags & (1 << i)) {
        result += mask[i] + ' ';
      }
    }
    return result;
  }

  let result: string;
  if(!FMnemonic) {
    if (fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY) {
      if(fkp.Key < 256) {
        result = `[${FormatShift(fkp.ShiftFlags)}${VKeyNames[fkp.Key]}]`;
      }
      else {
        result = `[${FormatShift(fkp.ShiftFlags)}K_${fkp.Key.toString(16).toUpperCase()}]`;
      }
    }
    else {
      result = `'${String.fromCharCode(fkp.Key)}'`;
    }
  }
  else {
    if (fkp.ShiftFlags & KMX.KMXFile.VIRTUALCHARKEY) {
      result = `[${FormatShift(fkp.ShiftFlags)}'${String.fromCharCode(fkp.Key)}']`;
    }
    else {
      result = `'${String.fromCharCode(fkp.Key)}'`;
    }
  }
  return result;
}

export function JavaScript_Key(fkp: KMX.KEY, FMnemonic: boolean): number {
  let Result: number;
  if(!FMnemonic) {
    if(fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY) {
      Result = fkp.Key;
    }
    else {
      // Convert the character to a virtual key
      let n = USEnglishShift.indexOf(String.fromCharCode(fkp.Key));
      if(n < 0) {
        n = USEnglishUnshift.indexOf(String.fromCharCode(fkp.Key));
      }
      if(n < 0) {
        Result = 0;
      }
      else {
        Result = USEnglishValues.charCodeAt(n);
      }
    }
  }
  else {
    Result = fkp.Key;
  }

  // Check that key is not unreachable (e.g. K_SHIFT, touch-specific special keys 50,000+)

  if(UnreachableKeyCodes.indexOf(Result) >= 0) {
    Result = 0;
  }

  if (Result == 0 || Result >= TKeymanWebTouchStandardKey.K_LOPT) {   // I4141
    if(!FUnreachableKeys.includes(fkp)) {
      callbacks.reportMessage(KmwCompilerMessages.Hint_UnreachableKeyCode({key: FormatKeyForErrorMessage(fkp,FMnemonic)}));
      FUnreachableKeys.push(fkp);
    }
  }
  return Result;
}

///
/// Returns a Javascript representation of a key value, either as a constant (debug mode)
/// or as an integer.
///
/// @param fkp         Pointer to key record
/// @param FMnemonic   True if the keyboard is a mnemonic layout
///
/// @return string representation of the key value, e.g. 'keyCodes.K_A /* 0x41 */' or '65'
///
export function JavaScript_KeyAsString(fkp: KMX.KEY, FMnemonic: boolean): string {
  if(options.saveDebug) {
    return ' '+FormatKeyAsString(JavaScript_Key(fkp, FMnemonic));
  } else {
    return JavaScript_Key(fkp, FMnemonic).toString();
  }
}

export function JavaScript_ContextMatch(fk: KMX.KEYBOARD, fkp: KMX.KEY, context: string): string {
  if(IsKeyboardVersion10OrLater()) {
    return JavaScript_FullContextValue(fk, fkp, context);
  }
  else {
    return JavaScript_CompositeContextValue(fk, fkp, context);
  }
}

function JavaScript_ContextLength(Context: string): number {
  return xstrlen_printing(Context);
}

function GetCodeName(code: number): string {
  if (code >= 0 && code < KMXCodeNames.length && KMXCodeNames[code] != '') {
    return KMXCodeNames[code];
  }
  return code.toString();
}

function CheckStoreForInvalidFunctions(fk: KMX.KEYBOARD, key: KMX.KEY, store: KMX.STORE) {  // I1520
  let n: number, rec: TSentinelRecord;
  const wcsentinel = String.fromCharCode(0xFFFF);

  n = store.dpString.indexOf(wcsentinel);

  // Disable the check with versions >= 10.0, since we now support deadkeys in stores.
  if (n >= 0 && !IsKeyboardVersion10OrLater) {
    rec = ExpandSentinel(fk, store.dpString, n);
    callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebStore({code:GetCodeName(rec.Code), store:store.dpName}));
  }
}


// Used when targeting versions prior to 10.0, before the introduction of FullContextMatch/KFCM.
function JavaScript_CompositeContextValue(fk: KMX.KEYBOARD, fkp: KMX.KEY, pwsz: string): string {
// var
  // StartQuotes, Len, Cur: Integer;
  // InQuotes: Boolean;
  // rec: TSentinelRecord;

  let Result = '';

  let InQuotes = false;
  let Len = JavaScript_ContextLength(pwsz);
  let StartQuotes = -1;
  let x = 0, Cur = 0;

  while(x < pwsz.length) {
    let rec = ExpandSentinel(fk, pwsz, x);
    if(rec.IsSentinel) {
      if(InQuotes) {
        Result += `",${Cur-StartQuotes})`;
        InQuotes = false;
      }
      if(Result != '') {
        Result += '&&';
      }

      switch(rec.Code) {
      case KMX.KMXFile.CODE_ANY:
        CheckStoreForInvalidFunctions(fk, fkp, rec.Any.Store);  // I1520
        Result += `k.KA(${Cur},k.KC(${Len-Cur},1,t),this.s${JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.dpName)})`;
        break;
      case KMX.KMXFile.CODE_DEADKEY:
        Result += `k.KDM(${Len-Cur},t,${rec.DeadKey.DeadKey})`;
        Cur--; // don't increment on deadkeys -- correlates with AdjustIndex function   // I3910
        break;
      case KMX.KMXFile.CODE_NUL:    // I2243
        Result += `k.KN(${Len-Cur},t)`;
        Cur--; // don't increment on nul -- correlates with AdjustIndex function   // I3910
        break;
      case KMX.KMXFile.CODE_IFOPT:    // I3429
        Result += `this.s${JavaScript_Name(rec.IfOpt.StoreIndex1, rec.IfOpt.Store1.dpName)}`+
          `${rec.IfOpt.IsNot == 0 ? '!==':'==='}`+
          `this.s${JavaScript_Name(rec.IfOpt.StoreIndex2,rec.IfOpt.Store2.dpName)}`;  // I3429   // I3659   // I3681
        Cur--; // don't increment on ifopt -- correlates with AdjustIndex function   // I3910
        break;
      case KMX.KMXFile.CODE_IFSYSTEMSTORE:     // I3430
        Result += `${rec.IfSystemStore.IsNot == 0 ? '!' : ''}`+
          `k.KIFS(${rec.IfSystemStore.dwSystemID},`+
          `this.s${JavaScript_Name(rec.IfSystemStore.StoreIndex,rec.IfSystemStore.Store.dpName)},t)`;
        Cur--; // don't increment on ifsystemstore -- correlates with AdjustIndex function   // I3910
        break;
      case KMX.KMXFile.CODE_CONTEXTEX:   // I3980
        Result += `k.KCCM(${Len-Cur},${Len-rec.ContextEx.Index+1},t)`;
        break;
      case KMX.KMXFile.CODE_NOTANY:   // I3981
        CheckStoreForInvalidFunctions(fk, fkp, rec.Any.Store);  // I1520
        Result += `k.KC(${Len-Cur},1,t)!=""&&!k.KA(${Cur},k.KC(${Len-Cur},1,t),`+
          `this.s${JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.dpName)})`;
        break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({code: GetCodeName(rec.Code)}));
        Result += '/*.*/ 0 ';
      }
    }
    else {
      if(!InQuotes) {
        if(Result != '') {
          Result += '&&';
        }
        Result += `k.KCM(${Len-Cur},t,"`;
        StartQuotes = Cur;
        InQuotes = true;
      }
      if(rec.ChrVal == '"'.charCodeAt(0) || rec.ChrVal == '\\'.charCodeAt(0)) {
        Result += '\\';
      }
      Result += JavaScript_String(rec.ChrVal);  // I2242
    }

    Cur++;
    x = incxstr(pwsz, x);
  }

  if(InQuotes) {
    Result += `",${Cur - StartQuotes})`;
  }
  return Result;
}

// Used when targeting versions >= 10.0, after the introduction of FullContextMatch/KFCM.
function JavaScript_FullContextValue(fk: KMX.KEYBOARD, fkp: KMX.KEY, pwsz: string): string {
/*var
  Len: Integer;
  rec: TSentinelRecord;
  FullContext, Suffix: string;
begin*/
  let Result = '';
  let FullContext = '';
  let Suffix = '';
  let Len = xstrlen(pwsz);
  let x = 0;

  while(x < pwsz.length) {
    if(FullContext != '') {
      FullContext += ',';
    }

    let rec = ExpandSentinel(fk, pwsz, x);
    if(rec.IsSentinel) {
      switch(rec.Code) {
      case KMX.KMXFile.CODE_ANY:
        CheckStoreForInvalidFunctions(fk, fkp, rec.Any.Store);  // I1520
        FullContext += `{t:'a',a:this.s${JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.dpName)}}`;
        break;
      case KMX.KMXFile.CODE_DEADKEY:
        FullContext += `{t:'d',d:${rec.DeadKey.DeadKey}}`;
        break;
      case KMX.KMXFile.CODE_NUL:    // I2243
        FullContext += `{t:'n'}`;
        break;
      case KMX.KMXFile.CODE_IFOPT:    // I3429
        Len--;
        if(Suffix != '') {
          Suffix += '&&';
        }
        if(FullContext == ',') {
          FullContext = '';
        }
        Suffix += `this.s${JavaScript_Name(rec.IfOpt.StoreIndex1, rec.IfOpt.Store1.dpName)}`+
          `${rec.IfOpt.IsNot == 0 ? '!==' : '==='}this.s${JavaScript_Name(rec.IfOpt.StoreIndex2,rec.IfOpt.Store2.dpName)}`;  // I3429   // I3659   // I3681
        break;
      case KMX.KMXFile.CODE_IFSYSTEMSTORE:     // I3430
        Len--;
        if(Suffix != '') {
          Suffix += '&&';
        }
        if(FullContext == ',') {
          FullContext = '';
        }
        Suffix += `${rec.IfSystemStore.IsNot == 0 ? '!' : ''}k.KIFS(${rec.IfSystemStore.dwSystemID},`+
          `this.s${JavaScript_Name(rec.IfSystemStore.StoreIndex,rec.IfSystemStore.Store.dpName)},t)`;  // I3430   // I3659   // I3681
        break;
      case KMX.KMXFile.CODE_NOTANY:   // I3981
        CheckStoreForInvalidFunctions(fk, fkp, rec.Any.Store);  // I1520
        FullContext += `{t:'a',a:this.s${JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.dpName)},n:1}`;
        break;
      case KMX.KMXFile.CODE_CONTEXTEX:
        FullContext += `{t:'c',c:${rec.ContextEx.Index}}`;   // I4611
        break;
      case KMX.KMXFile.CODE_INDEX:
        FullContext += `{t:'i',i:this.s${JavaScript_Name(rec.Index.StoreIndex, rec.Index.Store.dpName)},`+
          `o:${rec.Index.Index}}`;   // I4611
        break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({code: GetCodeName(rec.Code)}));
        Result += '/*.*/ 0 ';
      }
    }
    else
    { // Simple context character.
      FullContext += `'`;
      if(rec.ChrVal == '"'.charCodeAt(0) || rec.ChrVal == '\\'.charCodeAt(0) || rec.ChrVal == '\''.charCodeAt(0)) {
        FullContext += '\\';
      }
      FullContext += JavaScript_String(rec.ChrVal) + `'`;  // I2242
    }

    x = incxstr(pwsz, x);
  }

  if(FullContext != '') {
    Result = `k.KFCM(${Len},t,[${FullContext}])`;
  }

  if (Result != '' && Suffix != '') {
    Result += '&&' + Suffix;
  }
  else if(Suffix != '') {
    Result = Suffix;
  }
  return Result;
}

function isGroupReadOnly(fk: KMX.KEYBOARD, fgp: KMX.GROUP) {
  // TODO: export group + store metadata in debug store, use that
  return false;
}

function CallFunctionName(s: string): string {
  let n: number;
  n = s.indexOf(':');
  return s.substring(n+1); // not found gives -1, substring(0) is ok :grin:
}

export function JavaScript_OutputString(fk: KMX.KEYBOARD, FTabStops: string, fkp: KMX.KEY, pwszOutput: string, fgp: KMX.GROUP): string {
  let InQuotes = false;
  let len = 0;
  const nlt = nl + FTabStops;   // I3681

/*var
  i, n, len: Integer;
  InQuotes: Boolean;
  rec: TSentinelRecord;
  pwszcontext,pwsz: PWideChar;
  Index: Integer;   // I3910
  nlt: string;*/

  const AdjustIndex = function(pwszContext: string, Index: number): number {   // I3910
    let Result = Index;
    let x = 0;
    for(let I = 1; I < Index; I++) {
      let recContext = ExpandSentinel(fk, pwszContext, x);

      if(IsKeyboardVersion10OrLater()) {
        if(recContext.IsSentinel && [KMX.KMXFile.CODE_NUL, KMX.KMXFile.CODE_IFOPT, KMX.KMXFile.CODE_IFSYSTEMSTORE].includes(recContext.Code)) {
          Result--;
        }
      }
      else {
        if(recContext.IsSentinel && [KMX.KMXFile.CODE_DEADKEY, KMX.KMXFile.CODE_NUL, KMX.KMXFile.CODE_IFOPT, KMX.KMXFile.CODE_IFSYSTEMSTORE].includes(recContext.Code)) {
          Result--;
        }
      }
      x = incxstr(pwszContext, x);
    }
    return Result + 1; // 1-based
  }

  const ContextChar = function(ContextIndex: number, pwszContext: string, xContext: number): string {   // I4611
    let Index: number;
    let Result = '';
    let recContext = ExpandSentinel(fk, pwszContext, xContext);
    if(recContext.IsSentinel) {
      if(InQuotes) {   // I4611
        Result += '");';
        InQuotes =false;
      }

      switch(recContext.Code) {
      case KMX.KMXFile.CODE_ANY:
        Index = AdjustIndex(fkp.dpContext, ContextIndex);   // I3910   // I4611
        Result += nlt + `k.KIO(${len},this.s${JavaScript_Name(recContext.Any.StoreIndex, recContext.Any.Store.dpName)},${Index},t);`;   // I4611
        break;
      case KMX.KMXFile.CODE_DEADKEY:
        Result += nlt + `k.KDO(${len},t,${recContext.DeadKey.DeadKey});`;   // I4611
        break;
      case KMX.KMXFile.CODE_NOTANY:
        // #917: Minimum version required is 14.0: the KCXO function was only added for 14.0
        // Note that this is checked in compiler.cpp as well, so this error can probably never occur
        if(!IsKeyboardVersion14OrLater()) {
          callbacks.reportMessage(KmwCompilerMessages.Error_NotAnyRequiresVersion14());
        }
        Result += nlt + `k.KCXO(${len},t,${AdjustIndex(fkp.dpContext, xstrlen(fkp.dpContext))},${AdjustIndex(fkp.dpContext, ContextIndex)});`;
        break;
      case KMX.KMXFile.CODE_IFOPT:
      case KMX.KMXFile.CODE_IFSYSTEMSTORE:
      case KMX.KMXFile.CODE_NUL:
          // These have no output for a context emit
          break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({code: GetCodeName(recContext.Code)}));
        Result += nlt + '/*.*/ ';   // I4611
      }
    }
    else {
      if(!InQuotes) {
        Result += nlt + `k.KO(${len},t,"`;   // I4611
        InQuotes = true;
      }

      if(recContext.ChrVal == '"'.charCodeAt(0) || recContext.ChrVal == '\\'.charCodeAt(0)) {
        Result += '\\';
      }
      Result  += JavaScript_String(recContext.ChrVal);  // I2242
    }
    return Result;
  }

  let Result = '';
  InQuotes = false;

  let pwsz = pwszOutput;

  if(fkp != null) {
    if(IsKeyboardVersion10OrLater()) {
      // KMW >= 10.0 use the full, sentinel-based length for context deletions.
      len = xstrlen(fkp.dpContext);
      let n = len;

      let x = 0;
      for(let i = 0; i < n; i++) {
        let rec = ExpandSentinel(fk, fkp.dpContext, x);
        if(rec.IsSentinel && [KMX.KMXFile.CODE_NUL, KMX.KMXFile.CODE_IFOPT, KMX.KMXFile.CODE_IFSYSTEMSTORE].includes(rec.Code)) {
          len--;
        }
        x = incxstr(fkp.dpContext, x);
      }
    }
    else {
      // KMW < 10.0 exclude all sentinel-based characters, including deadkeys, from direct context deletion.
      // Deadkeys have alternative special handling.
      len = xstrlen_printing(fkp.dpContext);
    }
  }
  else {
    len = -1;
  }

  let x = 0;
  if(IsKeyboardVersion10OrLater() && pwsz.length > 0) {
    if(!isGroupReadOnly(fk, fgp)) {
      Result += nlt+`k.KDC(${len},t);`;   // I3681
    }
    len = -1;
  }

  while(x < pwsz.length) {
    let rec = ExpandSentinel(fk, pwsz, x);
    if(rec.IsSentinel) {
      if(InQuotes) {
        if(!isGroupReadOnly(fk, fgp)) {
          Result += '");';
        }
        InQuotes = false;
      }

      switch(rec.Code) {
      case KMX.KMXFile.CODE_CONTEXT:
        if (x > 0 || len == -1) {
          let xContext = 0;
          let n = 0;
          while(xContext < fkp.dpContext.length) {   // I4611
            if(!isGroupReadOnly(fk, fgp)) {
              Result += ContextChar(n, fkp.dpContext, xContext);
            }
            n++;
            xContext = incxstr(fkp.dpContext, xContext);
          }
          //Result := Result + Format('k.KO(%d,t,k.KC(%d,%d,t));', [len, xstrlen_printing(fkp.dpContext), xstrlen_printing(fkp.dpContext)]);
        }
        // else, we don't need to output anything - just don't delete the context
        len = -1;
        break;
      case KMX.KMXFile.CODE_CONTEXTEX:
        let xContext = 0;
        for(let i = 0; i < rec.ContextEx.Index; i++) {
          xContext = incxstr(fkp.dpContext, xContext);
        }

        if(!isGroupReadOnly(fk, fgp)) {
          Result += ContextChar(rec.ContextEx.Index, fkp.dpContext, xContext);   // I4611
        }
        len = -1;
        break;
      case KMX.KMXFile.CODE_BEEP:
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
          Result += nlt+'k.KB(t);';   // I3681
        }
        len = -1;
        break;
      case KMX.KMXFile.CODE_NUL:
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }
        len = -1;
        break;
      case KMX.KMXFile.CODE_INDEX:
        CheckStoreForInvalidFunctions(fk, fkp, rec.Index.Store); // I1520

        // This code was wrong.  We need to ignore CODE_NUL, CODE_DEADKEY in LHS context index counter.
        // This is why the compiler goes wrong -- and why the previous fix was inconsistent.
        // The I783 test did not test either of these cases.  It seems some of the keyboards were
        // compiled in-between the original fix and I783 re-fix, and then happened to work due to
        // their simplicity.

        let Index = AdjustIndex(fkp.dpContext, rec.Index.Index);   // I3910

        if(!isGroupReadOnly(fk, fgp)) {
          Result += nlt+`k.KIO(${len},this.s${JavaScript_Name(rec.Index.StoreIndex, rec.Index.Store.dpName)},${Index},t);`;
            // I783 - was: rec.Index.Index [2007-06-04]
            // I783 again.  Returned to rec.Index.Index.  Was previously: [2008-08-15]
            //              xstrlen(fkp.dpContext) + 1 - rec.Index.Index]);
            //      this was wrong.  Can't find any reason why this change was made
            //      which suggests it was in response to another bug and poorly traced (bad Marc)
            //      and not properly tested (bad, bad Marc).  Anyway, now tested with test_i783
        }
        len = -1;
        break;
      case KMX.KMXFile.CODE_DEADKEY:
        if(!isGroupReadOnly(fk, fgp)) {
          Result += nlt+`k.KDO(${len},t,${rec.DeadKey.DeadKey});`;   // I3681
        }
        len = -1;
        break;
      case KMX.KMXFile.CODE_USE:
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }
        Result += nlt+`r=this.g${JavaScript_Name(rec.Use.GroupIndex, rec.Use.Group.dpName)}(t,e);`;    // I1959   // I3681
        Result += nlt+'m=2;';  // #5440 - match desktop behavior
        len = -1;
        break;
      case KMX.KMXFile.CODE_CALL:
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }
        let n = FCallFunctions.indexOf(CallFunctionName(rec.Call.Store.dpString));
        if(n == -1) {
          n = FCallFunctions.push(CallFunctionName(rec.Call.Store.dpString));
        }
        Result += nlt+`r=this.c${n}(t,e);`;    // I1959   // I3681
        Result += nlt+'m=2;';  // #5440 - match desktop behavior
        len = -1;
        break;
      case KMX.KMXFile.CODE_SETOPT:    // I3429
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }
        Result += nlt+`this.s${JavaScript_Name(rec.SetOpt.StoreIndex1,rec.SetOpt.Store1.dpName)}=`+
          `this.s${JavaScript_Name(rec.SetOpt.StoreIndex2,rec.SetOpt.Store2.dpName)};`;
        len = -1;
        break;
      case KMX.KMXFile.CODE_RESETOPT:  // I3429
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }

        Result += nlt+`this.s${JavaScript_Name(rec.ResetOpt.StoreIndex,rec.ResetOpt.Store.dpName)}=`+
          `k.KLOAD(this.KI,"${JavaScript_Name(rec.ResetOpt.StoreIndex,rec.ResetOpt.Store.dpName,true)}",`+
          `${JavaScript_Store(fk, fkp.Line, rec.ResetOpt.Store.dpString)});`;  // I3429   // I3681   // I3659
        len = -1;
        break;
      case KMX.KMXFile.CODE_SAVEOPT:  // I3429
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }

        Result += nlt+`k.KSAVE("${JavaScript_Name(rec.SaveOpt.StoreIndex,rec.SaveOpt.Store.dpName,true)}",`+
          `this.s${JavaScript_Name(rec.SaveOpt.StoreIndex,rec.SaveOpt.Store.dpName)});`; // I3690  // I3429   // I3659   // I3681
        len = -1;
        break;
      case KMX.KMXFile.CODE_SETSYSTEMSTORE:  // I3437
        if(!isGroupReadOnly(fk, fgp)) {
          if(len > 0) {
            Result += nlt+`k.KO(${len},t,"");`;   // I3681
          }
        }

        Result += nlt+`k.KSETS(${rec.SetSystemStore.dwSystemID},`+
          `this.s${JavaScript_Name(rec.SetSystemStore.StoreIndex, rec.SetSystemStore.Store.dpName)},t);`;   // I3681
        len = -1;
        break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebOutput({code: GetCodeName(rec.Code)}));
        Result += '';
      }
    }
    else {
      if(!InQuotes) {
        if(!isGroupReadOnly(fk, fgp)) {
          Result += nlt+`k.KO(${len},t,"`;   // I3681
        }
        InQuotes = true; len = -1;
      }

      if(!isGroupReadOnly(fk, fgp)) {
        if(rec.ChrVal == '"'.charCodeAt(0) || rec.ChrVal == '\\'.charCodeAt(0)) {
          Result += '\\';
        }
        Result += JavaScript_String(rec.ChrVal);  // I2242
      }
    }

    x = incxstr(pwsz, x);
  }

  if(InQuotes) {
    if(!isGroupReadOnly(fk, fgp)) {
      Result += '");';
    }
  }
  return Result;
}

export function zeroPadHex(n: number, len: number): string {
  let result = n.toString(16).toUpperCase();
  if(result.length < len) {
    return '0'.repeat(len - result.length) + result;
  }
  return result;
}

/**
 * Converts a key value into a constant
 *
 * @param key A virtual key code
 *
 * @return string of JavaScript code, e.g. 'keyCodes.K_A /* 0x41 * /'
 */
function FormatKeyAsString(key: number): string {
  if(IsKeyboardVersion10OrLater()) {
    // Depends on flags defined in KeymanWeb 10.0
    if (key <= 255 && KMWVKeyNames[key] != '') {
      return 'keyCodes.'+KMWVKeyNames[key]+ ' /* 0x' + zeroPadHex(key, 2) + ' */';
    }
    return '0x' + zeroPadHex(key, 2);
  }
  return '0x' + zeroPadHex(key, 2);
}
