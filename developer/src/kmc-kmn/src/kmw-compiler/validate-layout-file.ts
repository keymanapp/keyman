import { KMX, TouchLayout } from "@keymanapp/common-types";
import { TouchLayoutFileReader, TouchLayoutFileWriter } from "@keymanapp/developer-utils";
import { callbacks, minimumKeymanVersion, verifyAndSetMinimumRequiredKeymanVersion15,
         isKeyboardVersion14OrLater, isKeyboardVersion17OrLater,
         verifyAndSetMinimumRequiredKeymanVersion14,
         verifyAndSetMinimumRequiredKeymanVersion17} from "./compiler-globals.js";
import { JavaScript_Key } from "./javascript-strings.js";
import { TRequiredKey, CRequiredKeys, CSpecialText, CSpecialText14Map, CSpecialText17Map,
         CSpecialTextMinVer, CSpecialTextMaxVer } from "./constants.js";
import { KeymanWebTouchStandardKeyNames, KMWAdditionalKeyNames, VKeyNames } from "./keymanweb-key-codes.js";
import { KmwCompilerMessages } from "./kmw-compiler-messages.js";
import * as Osk from '../compiler/osk.js';

export interface KeyAddress {
  rowIndex: number;
  keyIndex: number;
  subKeyIndex?: number;
  direction?: string;
  multitapIndex?: number
};

interface VLFOutput {
  output: string;
  result: boolean;
};

function IsValidUnicodeValue(ch: number): boolean {   // I4198
  return ((ch >= 0x0020) && (ch <= 0x007F)) ||
         ((ch >= 0x00A0) && (ch <= 0x10FFFF));
}

enum TKeyIdType { Key_Invalid, Key_Constant, Key_Touch, Key_Unicode, Key_Unicode_Multi };   // I4142

function GetKeyIdUnicodeType(value: string): TKeyIdType {
  let values = value.split('_');
  for(let v of values) {
    if(!IsValidUnicodeValue(parseInt(v,16))) {
      return TKeyIdType.Key_Invalid;
    }
  }
  if(values.length > 1) {
    return TKeyIdType.Key_Unicode_Multi;
  }
  return TKeyIdType.Key_Unicode;
}

function KeyIdType(FId: string): TKeyIdType {   // I4142
  FId = FId.toUpperCase();
  switch(FId.charAt(0)) {
  case 'T':
    return TKeyIdType.Key_Touch;
  case 'U':
    if(FId.startsWith('U_')) {
      return GetKeyIdUnicodeType(FId.substring(2));
    }
  default:
    // Note: can't use indexOf because some VKeyNames are mixed case, e.g. K_oE2
    if(VKeyNames.find(key => key.toUpperCase() == FId)) {
      return TKeyIdType.Key_Constant;
    }

    if(KeymanWebTouchStandardKeyNames.indexOf(FId) >= 0) {
      return TKeyIdType.Key_Constant;
    }

    if(KMWAdditionalKeyNames.indexOf(FId) >= 0) {
      return TKeyIdType.Key_Constant;
    }
  }
  return TKeyIdType.Key_Invalid;
}


function CheckKey(
  platformId: string,
  FPlatform: TouchLayout.TouchLayoutPlatform,
  layer: TouchLayout.TouchLayoutLayer,
  FId: string,
  FText: string,
  FNextLayer: string,
  FKeyType: TouchLayout.TouchLayoutKeySp,
  FRequiredKeys: TRequiredKey[],
  FDictionary: string[],
  address: KeyAddress
): boolean {   // I4119
  //
  // Coerce missing ID and Text to empty strings for additional tests
  //

  FId = FId ?? '';
  FText = FText ?? '';

  //
  // Check that each touch layer has K_LOPT, [K_ROPT,] K_BKSP, K_ENTER
  //

  for(let key of CRequiredKeys) {
    if(TRequiredKey[key].toLowerCase() == FId.toLowerCase()) {
      if(FRequiredKeys.indexOf(key) < 0) {
        FRequiredKeys.push(key);
      }
      break;
    }
  }

  //
  // Check that each layer referenced exists
  //

  if(typeof FNextLayer == 'string' && FNextLayer.length > 0) {
    if(FPlatform.layer.find(l => l.id.toLowerCase() == FNextLayer.toLowerCase()) == undefined) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutMissingLayer({
        keyId: FId,
        layerId: layer.id,
        nextLayer: FNextLayer,
        platformName: platformId,
        address
      }));
    }
  }

  //
  // Check that the key has a valid id   // I4142
  //

  if(FId.trim() == '') {
    if(!([TouchLayout.TouchLayoutKeySp.blank, TouchLayout.TouchLayoutKeySp.spacer].includes(FKeyType))) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutUnidentifiedKey({layerId: layer.id, address}));
    }
    return true;
  }

  let FValid = KeyIdType(FId);

  if(FValid == TKeyIdType.Key_Invalid) {
    callbacks.reportMessage(KmwCompilerMessages.Error_TouchLayoutInvalidIdentifier({keyId: FId, platformName: platformId, layerId: layer.id, address}));
    return false;
  }
  else if (FValid == TKeyIdType.Key_Unicode_Multi && !verifyAndSetMinimumRequiredKeymanVersion15()) {
    callbacks.reportMessage(KmwCompilerMessages.Error_TouchLayoutIdentifierRequires15({keyId: FId, platformName: platformId, layerId: layer.id, address}));
    return false;
  }

  //
  // Check that each custom key code has at least *a* rule associated with it
  //

  if (FValid == TKeyIdType.Key_Touch && FNextLayer == '' && [TouchLayout.TouchLayoutKeySp.normal, TouchLayout.TouchLayoutKeySp.deadkey].includes(FKeyType)) {
    // Search for the key in the key dictionary - ignore K_LOPT, K_ROPT...
    if(FDictionary.indexOf(FId) < 0) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutCustomKeyNotDefined({keyId: FId, platformName: platformId, layerId: layer.id, address}));
    }
  }

  //
  // Check that if the key has a *special* label, it is available in the target version
  //
  if(FText.startsWith('*') && FText.endsWith('*') && FText.length > 2) {
    // Keyman versions before 14 do not support '*special*' labels on non-special keys.
    // ZWNJ use, however, is safe because it will be transformed in function
    // TransformSpecialKeys14 to '<|>',  which does not require the custom OSK font.
    const mapVersion = Math.max(Math.min(minimumKeymanVersion(), CSpecialTextMaxVer), CSpecialTextMinVer);
    const specialText = CSpecialText.get(mapVersion);
    if(specialText.includes(FText) &&
        !verifyAndSetMinimumRequiredKeymanVersion14() &&
        !([TouchLayout.TouchLayoutKeySp.special, TouchLayout.TouchLayoutKeySp.specialActive].includes(FKeyType))) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutSpecialLabelOnNormalKey({
        keyId: FId,
        platformName: platformId,
        layerId: layer.id,
        label: FText,
        address
      }));
    }
  }

  return true;
}

function CheckDictionaryKeyValidity(fk: KMX.KEYBOARD, FDictionary: string[]) {   // I4142

  // TODO: O(eeek) performance here

  for(let i = 0; i < FDictionary.length; i++) {
    if(FDictionary[i] == '') {
      continue;
    }

    if([TKeyIdType.Key_Invalid, TKeyIdType.Key_Constant].includes(KeyIdType(FDictionary[i]))) {
      for(let fgp of fk.groups) {
        if(fgp.fUsingKeys) {
          for(let fkp of fgp.keys) {
            if(JavaScript_Key(fkp, fk.isMnemonic) == i+256) {
              callbacks.reportMessage(KmwCompilerMessages.Error_InvalidKeyCode({keyId: FDictionary[i]}));
            }
          }
        }
      }
    }
  }
}

function TransformSpecialKeys14(FDebug: boolean, sLayoutFile: string): string {
  // Rewrite Special key labels that are only supported in Keyman 14+
  // This code is a little ugly but effective.
  if(!isKeyboardVersion14OrLater()) {
    for(let i = 0; i < CSpecialText14Map.length; i++) {
      // Assumes the JSON output format will not change
      if(FDebug) {
        sLayoutFile = sLayoutFile.replaceAll('"text": "'+CSpecialText14Map[i][0]+'"', '"text": this._v>13 ? "'+CSpecialText14Map[i][0]+'" : "'+CSpecialText14Map[i][1]+'"');
      } else {
        sLayoutFile = sLayoutFile.replaceAll('"text":"'+CSpecialText14Map[i][0]+'"', '"text":this._v>13?"'+CSpecialText14Map[i][0]+'":"'+CSpecialText14Map[i][1]+'"');
      }
    }
  }
  return sLayoutFile;
}

function TransformSpecialKeys17(FDebug: boolean, sLayoutFile: string): string {
  // Rewrite Special key labels that are only supported in Keyman 17+
  // This code is a little ugly but effective.
  if(!isKeyboardVersion17OrLater()) {
    for(let i = 0; i < CSpecialText17Map.length; i++) {
      // Assumes the JSON output format will not change
      if(FDebug) {
        sLayoutFile = sLayoutFile.replaceAll('"text": "'+CSpecialText17Map[i][0]+'"', '"text": this._v>16 ? "'+CSpecialText17Map[i][0]+'" : "'+CSpecialText17Map[i][1]+'"');
      } else {
        sLayoutFile = sLayoutFile.replaceAll('"text":"'+CSpecialText17Map[i][0]+'"', '"text":this._v>16?"'+CSpecialText17Map[i][0]+'":"'+CSpecialText17Map[i][1]+'"');
      }
    }
  }
  return sLayoutFile;
}

export function ValidateLayoutFile(fk: KMX.KEYBOARD, FDebug: boolean, sLayoutFile: string, sVKDictionary: string, displayMap: Osk.PuaMap): VLFOutput {   // I4060   // I4139
  let FDictionary: string[] = sVKDictionary.split(/\s+/);

  CheckDictionaryKeyValidity(fk, FDictionary);   // I4142

  let reader = new TouchLayoutFileReader();
  let data: TouchLayout.TouchLayoutFile;
  try {
    if(!callbacks.fs.existsSync(sLayoutFile)) {
      callbacks.reportMessage(KmwCompilerMessages.Error_TouchLayoutFileDoesNotExist({filename: sLayoutFile}));
      return null;
    }
    data = reader.read(callbacks.loadFile(sLayoutFile));
    if(!data) {
      throw new Error('Unknown error reading touch layout file');
    }
  } catch(e) {
    callbacks.reportMessage(KmwCompilerMessages.Error_InvalidTouchLayoutFileFormat({msg: (e??'Unspecified error').toString()}));
    return null;
  }

  let hasWarnedOfGestureUseDownlevel = false;
  const warnGesturesIfNeeded = function(keyId: string) {
    if(!hasWarnedOfGestureUseDownlevel && !verifyAndSetMinimumRequiredKeymanVersion17()) {
      hasWarnedOfGestureUseDownlevel = true;
      callbacks.reportMessage(KmwCompilerMessages.Hint_TouchLayoutUsesUnsupportedGesturesDownlevel({keyId}));
    }
  }

  let result: boolean = true;
  let FTouchLayoutFont = '';   // I4872
  let pid: keyof TouchLayout.TouchLayoutFile;
  for(pid in data) {
    let platform = data[pid];

    // Test that the font matches on all platforms   // I4872

    if(FTouchLayoutFont == '') {
      FTouchLayoutFont = platform.font?.toLowerCase() ?? '';
    }
    else if((platform.font?.toLowerCase() ?? '') != FTouchLayoutFont) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutFontShouldBeSameForAllPlatforms());
      // TODO: why support multiple font values if it has to be the same across all platforms?!
    }

    // Test that all required keys are present
    for(let layer of platform.layer) {
      let FRequiredKeys: TRequiredKey[] = [];
      let rowIndex = 0;
      for(let row of layer.row) {
        rowIndex++;
        let keyIndex = 0;
        for(let key of row.key) {
          keyIndex++;
          result = CheckKey(pid, platform, layer, key.id, key.text, key.nextlayer, key.sp, FRequiredKeys, FDictionary, {rowIndex, keyIndex}) && result;   // I4119
          if(key.sk) {
            let subKeyIndex = 0;
            for(let subkey of key.sk) {
              subKeyIndex++;
              result = CheckKey(pid, platform, layer, subkey.id, subkey.text, subkey.nextlayer, subkey.sp, FRequiredKeys, FDictionary,
                {rowIndex, keyIndex, subKeyIndex}) && result;
            }
          }
          let direction: keyof TouchLayout.TouchLayoutFlick;
          if(key.flick) {
            for(direction in key.flick) {
              warnGesturesIfNeeded(key.id);
              result = CheckKey(pid, platform, layer, key.flick[direction].id, key.flick[direction].text,
                key.flick[direction].nextlayer, key.flick[direction].sp, FRequiredKeys, FDictionary, {rowIndex, keyIndex, direction}) && result;
            }
          }

          if(key.multitap) {
            let multitapIndex = 0;
            for(let subkey of key.multitap) {
              multitapIndex++;
              warnGesturesIfNeeded(key.id);
              result = CheckKey(pid, platform, layer, subkey.id, subkey.text, subkey.nextlayer, subkey.sp, FRequiredKeys, FDictionary,
                {rowIndex, keyIndex, multitapIndex}) && result;
            }
          }
        }
      }

      if(FRequiredKeys.length != CRequiredKeys.length) {
        callbacks.reportMessage(KmwCompilerMessages.Warn_TouchLayoutMissingRequiredKeys({
          layerId: layer.id,
          platformName: pid,
          missingKeys: CRequiredKeys.filter(x => !FRequiredKeys.includes(x)).join(', ')
        }));
      }
    }
  }

  // Transform the layout keys with displayMap
  if(displayMap) {
    Osk.remapTouchLayout(data, displayMap);
  }

  // If not debugging, then this strips out formatting for a big saving in file size
  // This also normalises any values such as Pad or Width which should be strings
  let writer = new TouchLayoutFileWriter({formatted: FDebug});

  sLayoutFile = writer.compile(data);

  sLayoutFile = TransformSpecialKeys14(FDebug, sLayoutFile);

  sLayoutFile = TransformSpecialKeys17(FDebug, sLayoutFile);

  return {
    output: sLayoutFile,
    result
  }
}
