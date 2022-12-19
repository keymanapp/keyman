import { GROUP, KEY, KMXFile } from "../../../../../common/web/types/src/kmx/kmx.js";

import { FMnemonic, FTabStop, IsKeyboardVersion10OrLater, nl, options } from "./compiler-globals.js";
import { CERR_VirtualCharacterKeysNotSupportedInKeymanWeb, CERR_VirtualKeysNotValidForMnemonicLayouts, CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb, CWARN_OptionStoreNameInvalid, ReportError } from "./messages.js";

//const SValidIdentifierCharSet = '123';

export function JavaScript_Name(i: number, pwszName: string, KeepNameForPersistentStorage: boolean = false): string {   // I3659
  let FChanged: boolean = false;
  let p = pwszName;

  if((pwszName == null || pwszName == undefined || pwszName == '') || (!isFDebug && !KeepNameForPersistentStorage)) {   // I3659   // I3681
    return i.toString(10); // for uniqueness
  }
  else {
    let result = KeepNameForPersistentStorage // I3659
      ? ''   // Potential for overlap in theory but in practice we only use this for named option stores so can never overlap
      : '_'; // Ensures we cannot overlap numbered instances
    while(p.length) {
      let ch = p.charAt(0);
      if(SValidIdentifierCharSet.includes(ch)) {  // I3681
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
      ReportError(0, CWARN_OptionStoreNameInvalid,
        Format('The option store %s should be named with characters in the range A-Z, a-z, 0-9 and _ only.',
        [pwszName]));
    }
    return result;
  }
}

export function JavaScript_Store(line: number, pwsz: string): string {
  let ch: number, rec: TSentinelRecord, result: string;
  const wcsentinel: string = '\uFFFF';
  let n = pwsz.indexOf(wcsentinel);

  // Start:  plain text store.  Always use for < 10.0, conditionally for >= 10.0.
  if(n == 0 || !IsKeyboardVersion10OrLater) {
    result = '"';
    while(pwsz.length) {
      if(pwsz.charCodeAt(0) == KMXFile.UC_SENTINEL) {
        result += '.'; // UC_SENTINEL values are not supported in stores for KMW < 10.0.
      } else {
        ch = GetSuppChar(pwsz);
        if(ch == '"' || ch == '\\') {
          result += '\\';
        }
        result += JavaScript_String(ch);  // I2242
      }

      pwsz = pwsz.substring(1);
    }
    result += '"';
  }
  else {
    result = '[';
    while(pwsz.length) {
      if(result != '[') {
        result += ',';
      }
      rec = ExpandSentinel(pwsz);
      if(rec.IsSentinel) {
        if(rec.Code == KMXFile.CODE_DEADKEY) {
          result += `{t:'d',d:${rec.Deadkey.DeadKey}}`;
        }
        else if(rec.Code == KMXFile.CODE_BEEP) {
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
        ch = GetSuppChar(pwsz);
        result += '"';
        // TODO:  Refactor the section below into JavaScript_String, as it's
        // quite common in our code base.
        if(ch == '"'.charCodeAt(0) || ch == '\\'.charCodeAt(0)) {
          result += '\\';
        }
        result += JavaScript_String(ch) + '"';  // I2242
      }

      pwsz = pwsz.substring(1);
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
    if(ch < 9) {
      return '\\x0'+ch.toString(16);
    } else {
      return '\\x'+ch.toString(16);
    }
  }
  else {
    return String.fromCodePoint(ch);
  }
}

export function JavaScript_Rules(fgp: GROUP): string {
  let IsEqualKey = function(k1: KEY, k2: KEY): boolean {
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
    if(!processed_rule[j] && !RuleIsExcludedByPlatform(fkp)) {
      let fkp = fgp.keys[j];
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
          if (!processed_rule[j] && !RuleIsExcludedByPlatform(fkp) && IsEqualKey(fkp, fkp2)) {
            processed_rule[j] = true;
            result += JavaScript_Rule(FTabStop + FTabStop + FTabStop, LocalHasRules ? 'else ' : '', fgp, fkp);
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
        result += JavaScript_Rule(FTabStop + FTabStop + FTabStop, HasRules ? 'else ' : '', fgp, fkp);
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

export function JavaScript_Shift(fkp: KEY, FMnemonic: boolean): number {
  if (FMnemonic) {
    if (fkp.ShiftFlags & KMXFile.VIRTUALCHARKEY) {
      ReportError(fkp.Line, CERR_VirtualCharacterKeysNotSupportedInKeymanWeb, 'Virtual character keys not currently supported in KeymanWeb');  // I1971   // I4061
      return 0;
    }

    if (fkp.ShiftFlags & KMXFile.ISVIRTUALKEY && fkp.Key <= 255) {
      // We prohibit K_ keys for mnemonic layouts. We don't block T_ and U_ keys.
      // TODO: this doesn't resolve the issue of, e.g. SHIFT+K_SPACE
      // https://github.com/keymanapp/keyman/issues/265
      ReportError(fkp.Line, CERR_VirtualKeysNotValidForMnemonicLayouts, 'Virtual keys are not valid for mnemonic layouts');  // I1971   // I4061
      return 0;
    }
  }

  if (fkp.ShiftFlags & KMXFile.ISVIRTUALKEY) {
    if(IsKeyboardVersion10OrLater()) {
      // Full chiral modifier and state key support starts with KeymanWeb 10.0
      return fkp.ShiftFlags;
    }

    // Non-chiral support only and no support for state keys
    if (fkp.ShiftFlags & (KMXFile.LCTRLFLAG | KMXFile.RCTRLFLAG | KMXFile.LALTFLAG | KMXFile.RALTFLAG)) {   // I4118
      ReportError(fkp.Line, CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb, 'Extended shift flags LALT, RALT, LCTRL, RCTRL are not supported in KeymanWeb');
    }

    if (fkp.ShiftFlags & (
      KMXFile.CAPITALFLAG | KMXFile.NOTCAPITALFLAG | KMXFile.NUMLOCKFLAG | KMXFile.NOTNUMLOCKFLAG |
      KMXFile.SCROLLFLAG | KMXFile.NOTSCROLLFLAG)) {   // I4118
      ReportError(fkp.Line, CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb, 'Extended shift flags CAPS and NCAPS are not supported in KeymanWeb');
    }

    return KMXFile.ISVIRTUALKEY | (fkp.ShiftFlags & (KMXFile.K_SHIFTFLAG | KMXFile.K_CTRLFLAG | KMXFile.K_ALTFLAG));
  }

  return USEnglishShift.includes(fkp.Key) ? KMXFile.ISVIRTUALKEY | KMXFile.K_SHIFTFLAG : KMXFile.ISVIRTUALKEY;
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
export function JavaScript_ShiftAsString(fkp: KEY, FMnemonic: boolean): string {
  if(!options.debug) {
    return JavaScript_Shift(fkp, FMnemonic).toString();
  }
  return ' '+FormatModifierAsBitflags(JavaScript_Shift(fkp, FMnemonic));
}



function TCompileKeymanWeb.JavaScript_Key(fkp: PFILE_KEY; FMnemonic: Boolean): Integer;
var
  n: Integer;
  i: Integer;
begin
  if not FMnemonic then
  begin
    if (fkp.ShiftFlags and KMX_ISVIRTUALKEY) = KMX_ISVIRTUALKEY then
    begin
      Result := Ord(fkp.Key);
    end
    else
    begin
      // Convert the character to a virtual key
      n := Pos(fkp.Key, USEnglishShift);
      if n = 0 then n := Pos(fkp.Key, USEnglishUnshift);
      if n = 0
        then Result := 0
        else Result := Ord(USEnglishValues[n]);
    end;
  end
  else
  begin
    Result := Ord(fkp.Key);
  end;

  // Check that key is not unreachable (e.g. K_SHIFT, touch-specific special keys 50,000+)

  for i := 0 to High(UnreachableKeyCodes) do   // I4141
    if Result = UnreachableKeyCodes[i] then
      Result := 0;

  if (Result = 0) or (Result >= Ord(Low(TKeymanWebTouchStandardKey))) then   // I4141
  begin
    if not FUnreachableKeys.Contains(fkp) then
    begin
      ReportError(fkp.Line, CHINT_UnreachableKeyCode,
        'The rule will never be matched for key '+
        FormatKeyForErrorMessage(fkp,FMnemonic)+' because its key code is never fired.');
      FUnreachableKeys.Add(fkp);
    end;
  end;
end;

///
/// Returns a Javascript representation of a key value, either as a constant (debug mode)
/// or as an integer.
///
/// @param fkp         Pointer to key record
/// @param FMnemonic   True if the keyboard is a mnemonic layout
///
/// @return string representation of the key value, e.g. 'keyCodes.K_A /* 0x41 */' or '65'
///
function JavaScript_KeyAsString(fkp: KEY, FMnemonic: boolean): string {
  if(options.debug) {
    return ' '+FormatKeyAsString(JavaScript_Key(fkp, FMnemonic));
  } else {
    return JavaScript_Key(fkp, FMnemonic).toString();
  }
}

