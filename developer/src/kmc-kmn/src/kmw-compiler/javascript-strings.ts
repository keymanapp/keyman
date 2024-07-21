import { TSentinelRecord, GetSuppChar, ExpandSentinel, incxstr, xstrlen, xstrlen_printing } from "./util.js";
import { KMX } from "@keymanapp/common-types";

import { callbacks, FCallFunctions, FFix183_LadderLength, FMnemonic, FTabStop, FUnreachableKeys,
         kmxResult, nl, options, isKeyboardVersion10OrLater, isKeyboardVersion14OrLater,
         verifyAndSetMinimumRequiredKeymanVersion10 } from "./compiler-globals.js";
import { KmwCompilerMessages } from "./kmw-compiler-messages.js";
import { FormatModifierAsBitflags, RuleIsExcludedByPlatform } from "./kmw-compiler.js";
import { KMXCodeNames, SValidIdentifierCharSet, UnreachableKeyCodes, USEnglishShift,
         USEnglishUnshift, USEnglishValues } from "./constants.js";
import { KMWVKeyNames, TKeymanWebTouchStandardKey, VKeyNames } from "./keymanweb-key-codes.js";

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
  if(n < 0 || !isKeyboardVersion10OrLater()) {
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
      const x = incxstr(pwsz, 0);
      pwsz = pwsz.substring(x);
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
      callbacks.reportMessage(KmwCompilerMessages.Error_VirtualCharacterKeysNotSupportedInKeymanWeb({line:fkp.Line}));
      return 0;
    }

    if (fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY && fkp.Key <= 255) {
      // We prohibit K_ keys for mnemonic layouts. We don't block T_ and U_ keys.
      // TODO: this doesn't resolve the issue of, e.g. SHIFT+K_SPACE
      // https://github.com/keymanapp/keyman/issues/265
      callbacks.reportMessage(KmwCompilerMessages.Error_VirtualKeysNotValidForMnemonicLayouts({line:fkp.Line}));
      return 0;
    }
  }

  if (fkp.ShiftFlags & KMX.KMXFile.ISVIRTUALKEY) {
    if(isKeyboardVersion10OrLater()) {
      // don't attempt to upgrade to v10 at this point, only if we are already v10+
      // Full chiral modifier and state key support starts with KeymanWeb 10.0
      return fkp.ShiftFlags;
    }

    // Non-chiral support only and no support for state keys
    if (fkp.ShiftFlags & (KMX.KMXFile.LCTRLFLAG | KMX.KMXFile.RCTRLFLAG | KMX.KMXFile.LALTFLAG | KMX.KMXFile.RALTFLAG)) {   // I4118
      if(verifyAndSetMinimumRequiredKeymanVersion10()) {
        // upgrade to v10 if possible
        return fkp.ShiftFlags;
      }
      callbacks.reportMessage(KmwCompilerMessages.Warn_ExtendedShiftFlagsNotSupportedInKeymanWeb({line:fkp.Line, flags: 'LALT, RALT, LCTRL, RCTRL'}));
    }

    if (fkp.ShiftFlags & (
      KMX.KMXFile.CAPITALFLAG | KMX.KMXFile.NOTCAPITALFLAG | KMX.KMXFile.NUMLOCKFLAG | KMX.KMXFile.NOTNUMLOCKFLAG |
      KMX.KMXFile.SCROLLFLAG | KMX.KMXFile.NOTSCROLLFLAG)) {   // I4118
        if(verifyAndSetMinimumRequiredKeymanVersion10()) {
          // upgrade to v10 if possible
          return fkp.ShiftFlags;
        }
        callbacks.reportMessage(KmwCompilerMessages.Warn_ExtendedShiftFlagsNotSupportedInKeymanWeb({line:fkp.Line, flags: 'CAPS and NCAPS'}));
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
      callbacks.reportMessage(KmwCompilerMessages.Hint_UnreachableKeyCode({line:fkp.Line, key: FormatKeyForErrorMessage(fkp,FMnemonic)}));
      FUnreachableKeys.push(fkp);
    }
  }
  return Result;
}

/**
 * Returns a Javascript representation of a key value, either as a constant (debug mode)
 * or as an integer.
 *
 * @param fkp         Pointer to key record
 * @param FMnemonic   True if the keyboard is a mnemonic layout
 *
 * @return string representation of the key value, e.g. 'keyCodes.K_A /* 0x41 * /' or '65'
 */
export function JavaScript_KeyAsString(fkp: KMX.KEY, FMnemonic: boolean): string {
  if(options.saveDebug) {
    return ' '+FormatKeyAsString(JavaScript_Key(fkp, FMnemonic));
  } else {
    return JavaScript_Key(fkp, FMnemonic).toString();
  }
}

export function JavaScript_ContextMatch(fk: KMX.KEYBOARD, fkp: KMX.KEY, context: string): string {
  if(isKeyboardVersion10OrLater()) {
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
  if (n >= 0 && !isKeyboardVersion10OrLater()) {
    rec = ExpandSentinel(fk, store.dpString, n);
    callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebStore({code:GetCodeName(rec.Code), store:store.dpName}));
  }
}


// Used when targeting versions prior to 10.0, before the introduction of FullContextMatch/KFCM.
function JavaScript_CompositeContextValue(fk: KMX.KEYBOARD, fkp: KMX.KEY, pwsz: string): string {
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
        Result += `k.KCCM(${Len-Cur},${Len-rec.ContextEx.Index},t)`;
        break;
      case KMX.KMXFile.CODE_NOTANY:   // I3981
        CheckStoreForInvalidFunctions(fk, fkp, rec.Any.Store);  // I1520
        Result += `k.KC(${Len-Cur},1,t)!=""&&!k.KA(${Cur},k.KC(${Len-Cur},1,t),`+
          `this.s${JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.dpName)})`;
        break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({line: fkp.Line, code: GetCodeName(rec.Code)}));
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
        FullContext += `{t:'c',c:${rec.ContextEx.Index+1}}`;   // I4611
        break;
      case KMX.KMXFile.CODE_INDEX:
        FullContext += `{t:'i',i:this.s${JavaScript_Name(rec.Index.StoreIndex, rec.Index.Store.dpName)},`+
          `o:${rec.Index.Index}}`;   // I4611
        break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({line: fkp.Line, code: GetCodeName(rec.Code)}));
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
  const index = fk.groups.indexOf(fgp);
  return kmxResult.extra.groups[index].isReadOnly;
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

  const AdjustIndex = function(pwszContext: string, Index: number): number {   // I3910
    let Result = Index;
    let x = 0;
    for(let I = 1; I < Index; I++) {
      let recContext = ExpandSentinel(fk, pwszContext, x);

      if(isKeyboardVersion10OrLater()) {
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
    return Result;
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
        Index = AdjustIndex(fkp.dpContext, ContextIndex) + 1;   // I3910   // I4611
        Result += nlt + `k.KIO(${len},this.s${JavaScript_Name(recContext.Any.StoreIndex, recContext.Any.Store.dpName)},${Index},t);`;   // I4611
        break;
      case KMX.KMXFile.CODE_DEADKEY:
        Result += nlt + `k.KDO(${len},t,${recContext.DeadKey.DeadKey});`;   // I4611
        break;
      case KMX.KMXFile.CODE_NOTANY:
        // #917: Minimum version required is 14.0: the KCXO function was only added for 14.0
        if(!isKeyboardVersion14OrLater()) {
          // Note that this is checked in compiler.cpp as well, so this error can probably never occur
          throw new Error('Unexpected: notany() encountered with invalid version');
        }
        Result += nlt + `k.KCXO(${len},t,${AdjustIndex(fkp.dpContext, xstrlen(fkp.dpContext))},${AdjustIndex(fkp.dpContext, ContextIndex)+1});`;
        break;
      case KMX.KMXFile.CODE_IFOPT:
      case KMX.KMXFile.CODE_IFSYSTEMSTORE:
      case KMX.KMXFile.CODE_NUL:
          // These have no output for a context emit
          break;
      default:
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebContext({line: fkp.Line, code: GetCodeName(recContext.Code)}));
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
    if(isKeyboardVersion10OrLater()) {
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
  if(isKeyboardVersion10OrLater() && pwsz.length > 0) {
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
          n = FCallFunctions.push(CallFunctionName(rec.Call.Store.dpString)) - 1;
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
        callbacks.reportMessage(KmwCompilerMessages.Error_NotSupportedInKeymanWebOutput({line: fkp?.Line ?? 0, code: GetCodeName(rec.Code)}));
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
  // TODO: automatic version upgrade
  if(isKeyboardVersion10OrLater()) {
    // Depends on flags defined in KeymanWeb 10.0
    if (key <= 255 && KMWVKeyNames[key] != '') {
      return 'keyCodes.'+KMWVKeyNames[key]+ ' /* 0x' + zeroPadHex(key, 2) + ' */';
    }
    return '0x' + zeroPadHex(key, 2);
  }
  return '0x' + zeroPadHex(key, 2);
}
