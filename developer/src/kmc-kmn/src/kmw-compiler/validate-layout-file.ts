import { KMX, Osk, TouchLayout, TouchLayoutFileReader, TouchLayoutFileWriter } from "@keymanapp/common-types";
import { callbacks, IsKeyboardVersion14OrLater, IsKeyboardVersion15OrLater } from "./compiler-globals.js";
import { JavaScript_Key, VKeyNames } from "./javascript-strings.js";
import { TRequiredKey, CRequiredKeys, CSpecialText10, CSpecialText14, CSpecialText14ZWNJ, CSpecialText14Map } from "./constants.js";


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
  }
  return TKeyIdType.Key_Invalid;
}

// TODO lifecycle

function CheckKey(FPlatform: TouchLayout.TouchLayoutPlatform,
  FId: string, FText: string, FNextLayer: string, FKeyType: TouchLayout.TouchLayoutKeySp,
  FRequiredKeys: TRequiredKey[], FDictionary: string[]) {   // I4119

  //
  // Check that each touch layer has K_LOPT, [K_ROPT,] K_BKSP, K_ENTER
  //

  for(let key of CRequiredKeys) {
    if(TRequiredKey[key].toLowerCase() == FId.toLowerCase()) {
      FRequiredKeys.push(key);
      break;
    }
  }

  //
  // Check that each layer referenced exists
  //

  if(typeof FNextLayer == 'string' && FNextLayer.length > 0) {
    if(FPlatform.layer.find(l => l.id.toLowerCase() == FNextLayer.toLowerCase()) == undefined) {
      // TODO: callbacks.reportMessage() ReportError(0, CWARN_TouchLayoutMissingLayer, 'Key "'+FId+'" on platform "'+FPlatform.Name+'", layer "'+FLayer.Id+'", platform "'+FPlatform.Name+'", references a missing layer "'+FNextLayer+'".');
    }
  }

  //
  // Check that the key has a valid id   // I4142
  //

  if(FId.trim() == '') {
    if(!(FKeyType in [TouchLayout.TouchLayoutKeySp.blank, TouchLayout.TouchLayoutKeySp.spacer]) && FNextLayer == '') {
      // TODO: ReportError(0, CWARN_TouchLayoutUnidentifiedKey, 'A key on layer "'+FLayer.Id+'" has no identifier.');
    }
    return;
  }

  let FValid = KeyIdType(FId);

  if(FValid == TKeyIdType.Key_Invalid) {
    // TODO: ReportError(0, CERR_TouchLayoutInvalidIdentifier, 'Key "'+FId+'" on "'+FPlatform.Name+'", layer "'+FLayer.Id+'" has an invalid identifier.');
  }
  else if (FValid == TKeyIdType.Key_Unicode_Multi && !IsKeyboardVersion15OrLater()) {
    // TODO: ReportError(0, CERR_TouchLayoutInvalidIdentifier, 'Key "'+FId+'" on "'+FPlatform.Name+'", layer "'+FLayer.Id+'" has a multi-part identifier which requires version 15.0 or newer.');
  }

  //
  // Check that each custom key code has at least *a* rule associated with it
  //

  if (FValid == TKeyIdType.Key_Touch && FNextLayer == '' && FKeyType in [TouchLayout.TouchLayoutKeySp.normal, TouchLayout.TouchLayoutKeySp.deadkey]) {
    // Search for the key in the key dictionary - ignore K_LOPT, K_ROPT...
    if(FDictionary.indexOf(FId) < 0) {
      // TODO: ReportError(0, CWARN_TouchLayoutCustomKeyNotDefined, 'Key "'+FId+'" on layer "'+FLayer.Id+'", platform "'+FPlatform.Name+'", is a custom key but has no corresponding rule in the source.');
    }
  }

  //
  // Check that if the key has a *special* label, it is available in the target version
  //
  if(FText.startsWith('*') && FText.endsWith('*') && FText.length > 2) {
    // Keyman versions before 14 do not support '*special*' labels on non-special keys.
    // ZWNJ use, however, is safe because it will be transformed in function
    // TransformSpecialKeys14 to '<|>',  which does not require the custom OSK font.
    if((CSpecialText10.includes(FText) || CSpecialText14.includes(FText)) &&
        !CSpecialText14ZWNJ.includes(FText) &&
        !IsKeyboardVersion14OrLater() &&
        !(FKeyType in [TouchLayout.TouchLayoutKeySp.special, TouchLayout.TouchLayoutKeySp.specialActive])) {
      // TODO: ReportError(0, CWARN_TouchLayoutSpecialLabelOnNormalKey,
      //   Format('Key "%s" on layout "%s", platform "%s" does not have the key type "Special" or "Special (active)" but has the label "%s". This feature is only supported in Keyman 14 or later', [
      //     FId, FLayer.Id, FPlatform.Name, FText
      //   ]));
    }
  }
}

function CheckDictionaryKeyValidity(fk: KMX.KEYBOARD, FDictionary: string[]) {   // I4142

  // TODO: O(eeek) performance here

  for(let i = 0; i < FDictionary.length; i++) {
    if(FDictionary[i] == '') {
      continue;
    }

    if(KeyIdType(FDictionary[i]) in [TKeyIdType.Key_Invalid, TKeyIdType.Key_Constant]) {
      for(let fgp of fk.groups) {
        if(fgp.fUsingKeys) {
          for(let fkp of fgp.keys) {
            if(JavaScript_Key(fkp, fk.isMnemonic) == i+256) {
              // TODO: ReportError(fkp.Line, CERR_InvalidKeyCode, 'Invalid key identifier "'+FDictionary[i]+'"');
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
  if(!IsKeyboardVersion14OrLater()) {
    for(let i = 0; i < CSpecialText14Map.length; i++) {
      // Assumes the JSON output format will not change
      if(FDebug) {
        sLayoutFile = sLayoutFile.replace('"text": "'+CSpecialText14Map[i][0]+'"', '"text": this._v>13 ? "'+CSpecialText14Map[i][0]+'" : "'+CSpecialText14Map[i][1]+'"');
      } else {
        sLayoutFile = sLayoutFile.replace('"text":"'+CSpecialText14Map[i][0]+'"', '"text":this._v>13?"'+CSpecialText14Map[i][0]+'":"'+CSpecialText14Map[i][1]+'"');
      }
    }
  }
  return sLayoutFile;
}

export function ValidateLayoutFile(fk: KMX.KEYBOARD, FDebug: boolean, sLayoutFile: string, sVKDictionary: string, displayMap: Osk.PuaMap): VLFOutput {   // I4060   // I4139

/*
var
  FPlatform: TTouchLayoutPlatform;
  FLayer: TTouchLayoutLayer;
  FRow: TTouchLayoutRow;
  FKey: TTouchLayoutKey;
  FSubKey: TTouchLayoutSubKey;
  FRequiredKeys: set of TRequiredKey;
  FDictionary: TStringList;
  FDirection: TTouchLayoutFlickDirection;

*/

  let FDictionary: string[] = sVKDictionary.split(/\s+/);

  CheckDictionaryKeyValidity(fk, FDictionary);   // I4142

  let reader = new TouchLayoutFileReader();
  let data = reader.read(callbacks.loadFile(sLayoutFile));
  if(!data) {
    // TODO: ReportError(0, CERR_InvalidTouchLayoutFile, sMsg);
    return {output:null, result: false};
  }

  let FTouchLayoutFont = '';   // I4872
  let pid: keyof TouchLayout.TouchLayoutFile;
  for(pid in data) {
    let platform = data[pid];

    // Test that the font matches on all platforms   // I4872

    if(FTouchLayoutFont == '') {
      FTouchLayoutFont = platform.font;
    }
    else if(platform.font.toLowerCase() != FTouchLayoutFont) {
      // TODO: ReportError(0, CWARN_TouchLayoutFontShouldBeSameForAllPlatforms, 'The touch layout font should be the same for all platforms.');
      // TODO: why support multiple font values if it has to be the same across all platforms?!
    }

    // Test that all required keys are present
    for(let layer of platform.layer) {
      let FRequiredKeys: TRequiredKey[] = [];
      for(let row of layer.row) {
        for(let key of row.key) {
          CheckKey(platform, key.id, key.text, key.nextlayer, key.sp, FRequiredKeys, FDictionary);   // I4119
          if(key.sk) {
            for(let subkey of key.sk) {
              CheckKey(platform, subkey.id, subkey.text, subkey.nextlayer, subkey.sp, FRequiredKeys, FDictionary);
            }
          }
          let direction: keyof TouchLayout.TouchLayoutFlick;
          if(key.flick) {
            for(direction in key.flick) {
              CheckKey(platform, key.flick[direction].id, key.flick[direction].text,
                key.flick[direction].nextlayer, key.flick[direction].sp, FRequiredKeys, FDictionary);
            }
          }

          if(key.multitap) {
            for(let subkey of key.multitap) {
              CheckKey(platform, subkey.id, subkey.text, subkey.nextlayer, subkey.sp, FRequiredKeys, FDictionary);
            }
          }
        }
      }

      if(FRequiredKeys.length != CRequiredKeys.length) {
        // TODO: ReportError(0, CWARN_TouchLayoutMissingRequiredKeys, 'Layer "'+FLayer.Id+'" on platform "'+FPlatform.Name+'" is missing the required key(s) '+RequiredKeysToString(CRequiredKeys-FRequiredKeys)+'.');
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

  return {
    output: sLayoutFile,
    result: true
  }
}