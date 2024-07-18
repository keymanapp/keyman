/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

import { Version, deepCopy } from "@keymanapp/web-utils";
import { TouchLayout } from "@keymanapp/common-types";

import LayoutFormFactorBase = TouchLayout.TouchLayoutPlatform;
import LayoutLayerBase = TouchLayout.TouchLayoutLayer;
export type LayoutRow = TouchLayout.TouchLayoutRow;
export type LayoutKey = TouchLayout.TouchLayoutKey;
export type LayoutSubKey = TouchLayout.TouchLayoutSubKey;

import ButtonClasses = TouchLayout.TouchLayoutKeySp;

export { ButtonClasses };

import Codes from "../text/codes.js";
import type Keyboard from "./keyboard.js";

export interface EncodedVisualKeyboard {
  /** Represents CSS font styling to use for VisualKeyboard text */
  F: string;
  /** Should there be a 102nd key? */
  K102?: boolean,
  /**
   * Keyboard Layer Specification: an object-based map of layer name to the keycaps for its
   * 65 keys.  The 65 keys are ordered from left to right, then top to bottom.
   *
   * The key ID corresponding to each index of the array is specified within `Codes.dfltCodes`.
   * Entries corresponding to `K_*` in `Codes.dfltCodes` are reserved for future use.
   */
  KLS?: {[layerName: string]: string[]},
  /**
   * @deprecated
   * The older form for data in KLS - defines keycaps for 'default' keys, then 'shift' keys,
   * in a single concatenated array.
   */
  BK?: string[];
}

// The following types provide type definitions for the full JSON format we use for visual keyboard definitions.
export type ButtonClass       =  0 | 1 | 2 | 3 | 4 | /*5 | 6 | 7 |*/ 8 | 9 | 10;

export interface LayoutLayer extends LayoutLayerBase {
  // Post-processing elements.
  shiftKey?: LayoutKey,
  capsKey?: LayoutKey,
  numKey?: LayoutKey,
  scrollKey?: LayoutKey,
  aligned?: boolean,
  nextlayer?: string
};
export interface LayoutFormFactor extends LayoutFormFactorBase {
  // To facilitate those post-processing elements.
  layer: LayoutLayer[]
};

export type LayoutSpec = {
  "desktop"?: LayoutFormFactor,
  "phone"?: LayoutFormFactor,
  "tablet"?: LayoutFormFactor
}

const KEY_102_WIDTH = 200;

// This class manages default layout construction for consumption by OSKs without a specified layout.
export class Layouts {
  static readonly dfltCodes: ReadonlyArray<string> = [
    "K_BKQUOTE","K_1","K_2","K_3","K_4","K_5","K_6","K_7","K_8","K_9","K_0",
    "K_HYPHEN","K_EQUAL","K_*","K_*","K_*","K_Q","K_W","K_E","K_R","K_T",
    "K_Y","K_U","K_I","K_O","K_P","K_LBRKT","K_RBRKT","K_BKSLASH","K_*",
    "K_*","K_*","K_A","K_S","K_D","K_F","K_G","K_H","K_J","K_K","K_L",
    "K_COLON","K_QUOTE","K_*","K_*","K_*","K_*","K_*","K_oE2",
    "K_Z","K_X","K_C","K_V","K_B","K_N","K_M","K_COMMA","K_PERIOD",
    "K_SLASH","K_*","K_*","K_*","K_*","K_*","K_SPACE"
  ];

  static readonly dfltText='`1234567890-=\xA7~~qwertyuiop[]\\~~~asdfghjkl;\'~~~~~?zxcvbnm,./~~~~~ '
    +'~!@#$%^&*()_+\xA7~~QWERTYUIOP{}\\~~~ASDFGHJKL:"~~~~~?ZXCVBNM<>?~~~~~ ';

  // The function baked into keyboards by the current Web compiler creates an
  // array of single-char strings for BK. Refer to
  // developer/src/kmc-kmn/src/kmw-compiler/visual-keyboard-compiler.ts.
  static readonly DEFAULT_RAW_SPEC = {'F':'Tahoma', 'BK': Layouts.dfltText.split('')} as const;

  static modifierSpecials = {
    'leftalt': '*LAlt*',
    'rightalt': '*RAlt*',
    'alt': '*Alt*',
    'leftctrl': '*LCtrl*',
    'rightctrl': '*RCtrl*',
    'ctrl': '*Ctrl*',
    'ctrl-alt': '*AltGr*',
    'leftctrl-leftalt': '*LAltCtrl*',
    'rightctrl-rightalt': '*RAltCtrl*',
    'leftctrl-leftalt-shift': '*LAltCtrlShift*',
    'rightctrl-rightalt-shift': '*RAltCtrlShift*',
    'shift': '*Shift*',
    'shift-alt': '*AltShift*',
    'shift-ctrl': '*CtrlShift*',
    'shift-ctrl-alt': '*AltCtrlShift*',
    'leftalt-shift': '*LAltShift*',
    'rightalt-shift': '*RAltShift*',
    'leftctrl-shift': '*LCtrlShift*',
    'rightctrl-shift': '*RCtrlShift*'
  } as const;

  /**
  * Build a default layout for keyboards with no explicit layout
  *
  * @param   {Object}  PVK         raw specifications
  * @param   {Keyboard} keyboard   keyboard object (as loaded)
  * @param   {string} formFactor   (really utils.FormFactor)
  * @return  {LayoutFormFactor}
  */
  static buildDefaultLayout(PVK: EncodedVisualKeyboard, keyboard: Keyboard, formFactor: string): LayoutFormFactor {
    // Build a layout using the default for the device
    let layoutType = formFactor as keyof TouchLayout.TouchLayoutFile;

    if(typeof Layouts.dfltLayout[layoutType] != 'object') {
      layoutType = 'desktop';
    }

    let kbdBitmask = Codes.modifierBitmasks['NON_CHIRAL'];
    // An unfortunate dependency there.  Should probably also set a version within web-core for use.
    let kbdDevVersion = Version.CURRENT;
    if(keyboard) {
      kbdBitmask = keyboard.modifierBitmask;
      kbdDevVersion = keyboard.compilerVersion;
    }

    if(!PVK) {
      PVK = this.DEFAULT_RAW_SPEC;
    }

    // Clone the default layout object for this device
    var layout: LayoutFormFactor = deepCopy(Layouts.dfltLayout[layoutType]);

    var n,layers=layout['layer'], keyLabels: EncodedVisualKeyboard['KLS'] = PVK['KLS'], key102=PVK['K102'];
    var i, j, k, rows: LayoutRow[], key: LayoutKey, keys: LayoutKey[];
    var chiral: boolean = (kbdBitmask & Codes.modifierBitmasks.IS_CHIRAL) != 0;

    if(PVK['F']) {
      // The KeymanWeb compiler generates a string of the format `[italic ][bold ] 1em "<font>"`
      // We will ignore the bold, italic and font size spec
      let legacyFontSpec = /^(?:(?:italic|bold) )* *[0-9.eE-]+(?:[a-z]+) "(.+)"$/.exec(PVK['F']);
      if(legacyFontSpec) {
        layout.font = legacyFontSpec[1];
      }
    }

    var kmw10Plus = !(typeof keyLabels == 'undefined' || !keyLabels);
    if(!kmw10Plus) {
      // Save the processed key label information to the keyboard's general data.
      // Makes things more efficient elsewhere and for reloading after keyboard swaps.
      keyLabels = PVK['KLS'] = Layouts.processLegacyDefinitions(PVK['BK']);
    }

    // *** Step 1:  instantiate the layer objects. ***

    // Get the list of valid layers, enforcing that the 'default' layer must be the first one processed.
    var validIdList = Object.getOwnPropertyNames(keyLabels), invalidIdList: string[] = [];
    validIdList.splice(validIdList.indexOf('default'), 1);
    validIdList = [ 'default' ].concat(validIdList);

    // Automatic AltGr emulation if the 'leftctrl-leftalt' layer is otherwise undefined.
    if(keyboard && keyboard.emulatesAltGr) {
      // We insert only the layers that need to be emulated.
      if((validIdList.indexOf('leftctrl-leftalt') == -1) && validIdList.indexOf('rightalt') != -1) {
        validIdList.push('leftctrl-leftalt');
        keyLabels['leftctrl-leftalt'] = keyLabels['rightalt'];
      }

      if((validIdList.indexOf('leftctrl-leftalt-shift') == -1) && validIdList.indexOf('rightalt-shift') != -1) {
        validIdList.push('leftctrl-leftalt-shift');
        keyLabels['leftctrl-leftalt-shift'] = keyLabels['rightalt-shift'];
      }
    }

    // If there is no predefined layout, even touch layouts will follow the desktop's
    // setting for the displayUnderlying flag.  As the desktop layout uses a different
    // format for its layout spec, that's found at the field referenced below.
    layout["displayUnderlying"] = keyboard ? !!keyboard.scriptObject['KDU'] : false;

    // For desktop devices, we must create all layers, even if invalid.
    if(formFactor == 'desktop') {
      invalidIdList = Layouts.generateLayerIds(chiral);

      // Filter out all ids considered valid.  (We also don't want duplicates in the following list...)
      for(n=0; n<invalidIdList.length; n++) {
        if(validIdList.indexOf(invalidIdList[n]) != -1) {
          invalidIdList.splice(n--, 1);
        }
      }
    }

    // This ensures all 'valid' layers are at the front of the layer array and managed by the main loop below.
    // 'invalid' layers aren't handled by the loop and thus remain blank after it.
    var idList = validIdList.concat(invalidIdList);

    if(kmw10Plus && formFactor != 'desktop') { // KLS exists, so we know the exact layer set.
      // Find the SHIFT key...
      var shiftKey: LayoutKey = null;

      rows = layers[0]['row'];
      for(var r=0; r < rows.length; r++) {
        keys = rows[r]['key'];
        for(var c=0; c < keys.length; c++) {
          key = keys[c];
          if(key['id'] == 'K_SHIFT') {
            shiftKey = key;
          }
        }
      }

      if(shiftKey) {
        // Erase the legacy shifted subkey array.
        shiftKey['sk'] = [];

        for(let layerID in keyLabels) {
          if(layerID == 'default' || layerID == 'shift') {
            // These two are accessible from the layer without subkeys.
            continue;
          }

          // Create a new subkey for the specified layer so that it will be accessible via OSK.
          var specialChar = Layouts.modifierSpecials[(layerID as keyof typeof Layouts.modifierSpecials)];
          let subkey: LayoutSubKey = {
            id: `K_${specialChar}`,
            text: specialChar,
            sp: 1,
            nextlayer: layerID
          }
          shiftKey['sk'].push(subkey);
        }
      } else {
        // Seriously, this should never happen.  It's here for the debugging log only.
        console.warn("Error in default layout - cannot find default Shift key!");
      }
    }

    for(n=0; n<idList.length; n++) {
      // Populate non-default (shifted) keygroups
      if(n > 0) {
        layers[n]=deepCopy(layers[0]);
      }
      layers[n]['id']=idList[n];
      layers[n]['nextlayer']=idList[n]; // This would only be different for a dynamic keyboard

      // Extraced into a helper method to improve readability.
      Layouts.formatDefaultLayer(layers[n], chiral, formFactor, !!key102);
    }

    // *** Step 2: Layer objects now exist; time to fill them with the appropriate key labels and key styles ***
    for(n=0; n<layers.length; n++) {
      var layer=layers[n], kx, shiftKey: LayoutKey = null;
      var capsKey: LayoutKey = null, numKey: LayoutKey = null, scrollKey: LayoutKey = null;  // null if not in the OSK layout.
      var layerSpec = keyLabels[layer['id']];
      var isShift = layer['id'] == 'shift' ? 1 : 0;
      var isDefault = layer['id'] == 'default' || isShift ? 1 : 0;

      rows=layer['row'];
      for(i=0; i<rows.length; i++) {
        keys=rows[i]['key'];
        for(j=0; j<keys.length; j++) {
          key=keys[j];
          kx=Layouts.dfltCodes.indexOf(key['id']);

          // Only create keys for defined layers.  ('default' and 'shift' are always defined.)
          if(layerSpec || isDefault) {
            // Get keycap text from visual keyboard array, if defined in keyboard
            if(layerSpec) {
              if(kx >= 0 && kx < layerSpec.length) key['text']=layerSpec[kx];
            }

            // Legacy (pre 12.0) behavior:  fall back to US English keycap text as default for the base two layers
            // if a key cap is not otherwise defined. (Any intentional 'ghost' keys must be explicitly defined.)
            if(isDefault && kbdDevVersion.precedes(Version.NO_DEFAULT_KEYCAPS)) {
              if(key['id'] != 'K_SPACE' && kx+65 * isShift < Layouts.dfltText.length && key['text'] !== null) {
                key['text'] = key['text'] || Layouts.dfltText[kx+65*isShift];
              }
            }
          }

          // Leave any unmarked key caps as null strings
          if(key['text'] !== null) {
            key['text'] = key['text'] || '';
          }

          // Detect important tracking keys.
          switch(key['id']) {
            case "K_SHIFT":
              shiftKey=key;
              break;
            case "K_CAPS":
              capsKey=key;
              break;
            case "K_NUMLOCK":
              numKey=key;
              break;
            case "K_SCROLL":
              scrollKey=key;
              break;
          }

          // Remove pop-up shift keys referencing invalid layers (Build 349)
          if(key['sk'] != null) {
            for(k=0; k<key['sk'].length; k++) {
              if(validIdList.indexOf(key['sk'][k]['nextlayer']) == -1) {
                key['sk'].splice(k--, 1);
              }
            }

            if(key['sk'].length == 0) {
              key['sk']=null;
            }
          }
        }
      }

      // We're done with the layer keys initialization pass.  Time to do post-analysis layer-level init where necessary.
      layer.shiftKey=shiftKey;
      layer.capsKey=capsKey;
      layer.numKey=numKey;
      layer.scrollKey=scrollKey;

      const layerId = layers[n].id as keyof typeof Layouts.modifierSpecials;

      // Set modifier key appearance and behaviour for non-desktop devices using the default layout
      if(formFactor != 'desktop') {
        if(n > 0 && shiftKey != null) {
          shiftKey['sp']=ButtonClasses.specialActive;
          shiftKey['sk']=null;
          shiftKey['text'] = Layouts.modifierSpecials[layerId] ?? "*Shift*";
        }
      }
    }

    return layout;
  }

      /**
   * Function     getLayerId
   * Scope        Private
   * @param       {number}      m     shift modifier code
   * @return      {string}            layer string from shift modifier code (desktop keyboards)
   * Description  Get name of layer from code, where the modifer order is determined by ascending bit-flag value.
   */
  static getLayerId(m: number): string {
    let modifierCodes = Codes.modifierCodes;

    var s='';
    if(m == 0) {
      return 'default';
    } else {
      if(m & modifierCodes['LCTRL']) {
        s = (s.length > 0 ? s + '-' : '') + 'leftctrl';
      }
      if(m & modifierCodes['RCTRL']) {
        s = (s.length > 0 ? s + '-' : '') + 'rightctrl';
      }
      if(m & modifierCodes['LALT']) {
        s = (s.length > 0 ? s + '-' : '') + 'leftalt';
      }
      if(m & modifierCodes['RALT']) {
        s = (s.length > 0 ? s + '-' : '') + 'rightalt';
      }
      if(m & modifierCodes['SHIFT']) {
        s = (s.length > 0 ? s + '-' : '') + 'shift';
      }
      if(m & modifierCodes['CTRL']) {
        s = (s.length > 0 ? s + '-' : '') + 'ctrl';
      }
      if(m & modifierCodes['ALT']) {
        s = (s.length > 0 ? s + '-' : '') + 'alt';
      }
      return s;
    }
  }

  /**
   * Generates a list of potential layer ids for the specified chirality mode.
   *
   * @param   {boolean}   chiral    // Does the keyboard use chiral modifiers or not?
   */
  static generateLayerIds(chiral: boolean): string[] {
    var layerCnt, offset;

    if(chiral) {
      layerCnt=32;
      offset=0x01;
    } else {
      layerCnt=8;
      offset=0x10;
    }

    var layerIds = [];

    for(var i=0; i < layerCnt; i++) {
      layerIds.push(Layouts.getLayerId(i * offset));
    }

    return layerIds;
  }

  /**
   * Sets a formatting property for the modifier keys when constructing a default layout for a keyboard.
   *
   * @param   {Object}    layer   // One layer specification
   * @param   {boolean}   chiral  // Whether or not the keyboard uses chiral modifier information.
   * @param   {string}    formFactor  // The form factor of the device the layout is being constructed for.
   * @param   {boolean}   key102      // Whether or not the extended key 102 should be hidden.
   */
  static formatDefaultLayer(layer: LayoutLayer, chiral: boolean, formFactor: string, key102: boolean) {
    var layerId = layer['id'];

    // Correct appearance of state-dependent modifier keys according to group
    for(var i=0; i<layer['row'].length; i++) {
      var row=layer['row'][i];
      var keys=row['key'];
      for(var j=0; j<keys.length; j++) {
        var key=keys[j];
        switch(key['id']) {
          case 'K_SHIFT':
          case 'K_LSHIFT':
          case 'K_RSHIFT':
            if(layerId.indexOf('shift') != -1) {
              key['sp'] = ButtonClasses.specialActive;
            }
            if(formFactor != 'desktop') {
              if(layerId != 'default') {
                key['nextlayer']='default';
              } else {
                key['nextlayer']='shift';
              }
            }
            break;
          case 'K_LCTRL':
          case 'K_LCONTROL':
            if(chiral) {
              if(layerId.indexOf('leftctrl') != -1) {
                key['sp'] = ButtonClasses.specialActive;
              }
              break;
            }
          case 'K_RCTRL':
          case 'K_RCONTROL':
            if(chiral) {
              if(layerId.indexOf('rightctrl') != -1) {
                key['sp'] = ButtonClasses.specialActive;
              }
              break;
            }
          case 'K_CONTROL':
            if(layerId.indexOf('ctrl') != -1) {
              if(!chiral || (layerId.indexOf('leftctrl') != -1 && layerId.indexOf('rightctrl') != -1)) {
                key['sp'] = ButtonClasses.specialActive;
              }
            }
            break;
          case 'K_LALT':
            if(chiral) {
              if(layerId.indexOf('leftalt') != -1) {
                key['sp'] = ButtonClasses.specialActive;
              }
              break;
            }
          case 'K_RALT':
            if(chiral) {
              if(layerId.indexOf('rightalt') != -1) {
                key['sp'] = ButtonClasses.specialActive;
              }
              break;
            }
          case 'K_ALT':
            if(layerId.indexOf('alt') != -1) {
              if(!chiral || (layerId.indexOf('leftalt') != -1 && layerId.indexOf('rightalt') != -1)) {
                key['sp'] = ButtonClasses.specialActive;
              }
            }
            break;
          case 'K_oE2':
            if(typeof key102 == 'undefined' || !key102) {
              if(formFactor == 'desktop') {
                keys.splice(j--, 1);
                keys[0]['width']=KEY_102_WIDTH;
              } else {
                keys[j]['sp']=ButtonClasses.spacer;
              }
            }
            break;
        }
      }
    }
  }

  /**
   * Converts the legacy BK property from pre 10.0 into the KLS keyboard layer spec format,
   * sparsifying it as possible to pre-emptively check invalid layers.
   *
   * @param   {Array}   BK      keyboard object (as loaded)
   * @return  {Object}
   */
  static processLegacyDefinitions(BK: string[]): EncodedVisualKeyboard['KLS'] {
    //['default','shift','ctrl','shiftctrl','alt','shiftalt','ctrlalt','shiftctrlalt'];
    var idList=Layouts.generateLayerIds(false); // Non-chiral.

    var KLS: EncodedVisualKeyboard['KLS'] = {};

    // The old default:  eight auto-managed layers...
    for(var n=0; n<idList.length; n++) {
      var id = idList[n], arr = [], valid = false;

      // ... with keycode mappings in blocks of 65.
      for(var k=0; k < 65; k++) {
        var index = k + 65 * n;
        arr.push(BK[index]);

        // The entry for K_SPACE's keycode tends to hold ' ' instead of '', which causes
        // the whole layer to be treated as 'valid' if not included in the conditional.
        if(index < BK.length && BK[index] != '' && k != Layouts.dfltCodes.indexOf('K_SPACE')) {
          valid = true;
        }
      }

      if(valid) {
        KLS[id] = arr;
      }
    }

    // There must always be at least a plain 'default' layer.  Array(65).fill('') would be preferable but isn't supported on IE,
    // but buildDefaultLayer will set the defaults for these layers if no entry exists for them in the array due to length.
    if(typeof KLS['default'] == 'undefined' || ! KLS['default']) {
      KLS['default'] = [''];
    }

    // There must always be at least a plain 'shift' layer.
    if(typeof KLS['shift'] == 'undefined' || ! KLS['shift']) {
      KLS['shift'] = [''];
    }

    return KLS;
  }

  static dfltShiftToCaps: LayoutSubKey = {
    // Needs to be something special and unique.  Typing restricts us from
    // using a reserved key-id prefix, though.
    id: "T_*_MT_SHIFT_TO_CAPS",
    text: '*ShiftLock*',
    sp: 1,
    nextlayer: 'caps'
  }

  static dfltShiftToDefault: LayoutSubKey = {
    // Needs to be something special and unique.  Typing restricts us from
    // using a reserved key-id prefix, though.
    id: "T_*_MT_SHIFT_TO_DEFAULT",
    text: '*Shift*',
    sp: 1,
    nextlayer: 'default'
  }

  static dfltShiftToShift: LayoutSubKey = {
    id: "T_*_MT_SHIFT_TO_SHIFT",
    text: '*Shift*',
    sp: 1,
    nextlayer: 'shift'
  }

  // Defines the default visual layout for a keyboard.
  /* c8 ignore start */
  static dfltLayout: LayoutSpec = {
      "desktop":
      {
          "defaultHint": 'dot',
          "font": "Tahoma,Helvetica",
          "layer": [
              {
                  "id": "default",
                  "row": [
                      {
                          "id": 1,
                          "key": [
                              { "id": "K_BKQUOTE" },
                              { "id": "K_1" },
                              { "id": "K_2" },
                              { "id": "K_3" },
                              { "id": "K_4" },
                              { "id": "K_5" },
                              { "id": "K_6" },
                              { "id": "K_7" },
                              { "id": "K_8" },
                              { "id": "K_9" },
                              { "id": "K_0" },
                              { "id": "K_HYPHEN" },
                              { "id": "K_EQUAL" },
                              { "id": "K_BKSP", "text": "*BkSp*", "sp": 1, "width": 130 }
                          ]
                      },
                      {
                          "id": 2,
                          "key": [
                              { "id": "K_TAB", "text": "*Tab*", "sp": 1, "width": 130 },
                              { "id": "K_Q" },
                              { "id": "K_W" },
                              { "id": "K_E" },
                              { "id": "K_R" },
                              { "id": "K_T" },
                              { "id": "K_Y" },
                              { "id": "K_U" },
                              { "id": "K_I" },
                              { "id": "K_O" },
                              { "id": "K_P" },
                              { "id": "K_LBRKT" },
                              { "id": "K_RBRKT" },
                              { "id": "K_BKSLASH" }
                          ]
                      },
                      {
                          "id": 3,
                          "key": [
                              { "id": "K_CAPS", "text": "*Caps*", "sp": 1, "width": 165 },
                              { "id": "K_A" },
                              { "id": "K_S" },
                              { "id": "K_D" },
                              { "id": "K_F" },
                              { "id": "K_G" },
                              { "id": "K_H" },
                              { "id": "K_J" },
                              { "id": "K_K" },
                              { "id": "K_L" },
                              { "id": "K_COLON" },
                              { "id": "K_QUOTE" },
                              { "id": "K_ENTER", "text": "*Enter*", "sp": 1, "width": 165 }
                          ]
                      },
                      {
                          "id": 4,
                          "key": [
                              { "id": "K_SHIFT", "text": "*Shift*", "sp": 1, "width": 130 },
                              { "id": "K_oE2" },
                              { "id": "K_Z" },
                              { "id": "K_X" },
                              { "id": "K_C" },
                              { "id": "K_V" },
                              { "id": "K_B" },
                              { "id": "K_N" },
                              { "id": "K_M" },
                              { "id": "K_COMMA" },
                              { "id": "K_PERIOD" },
                              { "id": "K_SLASH" },
                              { "id": "K_RSHIFT", "text": "*Shift*", "sp": 1, "width": 130 }
                          ]
                      },
                      {
                          "id": 5,
                          "key": [
                              { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": 1, "width": 170 },
                              { "id": "K_LALT", "text": "*Alt*", "sp": 1, "width": 160 },
                              { "id": "K_SPACE", "text": "", "width": 770 },
                              { "id": "K_RALT", "text": "*Alt*", "sp": 1, "width": 160 },
                              { "id": "K_RCONTROL", "text": "*Ctrl*", "sp": 1, "width": 170 }
                          ]
                      }
                  ]
              }
          ]
      },
      "tablet":
      {
          "defaultHint": 'dot',
          "font": "Tahoma,Helvetica",
          "layer": [
              {
                  "id": "default",
                  "row": [
                      {
                          "id": 0,
                          "key": [
                              { "id": "K_1" },
                              { "id": "K_2" },
                              { "id": "K_3" },
                              { "id": "K_4" },
                              { "id": "K_5" },
                              { "id": "K_6" },
                              { "id": "K_7" },
                              { "id": "K_8" },
                              { "id": "K_9" },
                              { "id": "K_0" },
                              { "id": "K_HYPHEN" },
                              { "id": "K_EQUAL" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 1,
                          "key": [
                              { "id": "K_Q", "pad": 25 },
                              { "id": "K_W" },
                              { "id": "K_E" },
                              { "id": "K_R" },
                              { "id": "K_T" },
                              { "id": "K_Y" },
                              { "id": "K_U" },
                              { "id": "K_I" },
                              { "id": "K_O" },
                              { "id": "K_P" },
                              { "id": "K_LBRKT" },
                              { "id": "K_RBRKT" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 2,
                          "key": [
                              { "id": "K_A", "pad": 50 },
                              { "id": "K_S" },
                              { "id": "K_D" },
                              { "id": "K_F" },
                              { "id": "K_G" },
                              { "id": "K_H" },
                              { "id": "K_J" },
                              { "id": "K_K" },
                              { "id": "K_L" },
                              { "id": "K_COLON" },
                              { "id": "K_QUOTE" },
                              { "id": "K_BKSLASH", "width": 90 }
                          ]
                      },
                      {
                          "id": 3,
                          "key": [
                              { "id": "K_oE2", "width": 90 },
                              { "id": "K_Z" },
                              { "id": "K_X" },
                              { "id": "K_C" },
                              { "id": "K_V" },
                              { "id": "K_B" },
                              { "id": "K_N" },
                              { "id": "K_M" },
                              { "id": "K_COMMA" },
                              { "id": "K_PERIOD" },
                              { "id": "K_SLASH" },
                              { "id": "K_BKQUOTE" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 4,
                          "key": [
                              {
                                  "id": "K_SHIFT", "text": "*Shift*", "sp": 1, "width": 200, "sk": [
                                      { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": 1, "width": 50, "nextlayer": "ctrl" },
                                      { "id": "K_LCONTROL", "text": "*LCtrl*", "sp": 1, "width": 50, "nextlayer": "leftctrl" },
                                      { "id": "K_RCONTROL", "text": "*RCtrl*", "sp": 1, "width": 50, "nextlayer": "rightctrl" },
                                      { "id": "K_LALT", "text": "*Alt*", "sp": 1, "width": 50, "nextlayer": "alt" },
                                      { "id": "K_LALT", "text": "*LAlt*", "sp": 1, "width": 50, "nextlayer": "leftalt" },
                                      { "id": "K_RALT", "text": "*RAlt*", "sp": 1, "width": 50, "nextlayer": "rightalt" },
                                      { "id": "K_ALTGR", "text": "*AltGr*", "sp": 1, "width": 50, "nextlayer": "ctrl-alt" }]
                              },
                              { "id": "K_LOPT", "text": "*Menu*", "sp": 1, "width": 150 },
                              { "id": "K_SPACE", "text": "", "width": 570 },
                              { "id": "K_BKSP", "text": "*BkSp*", "sp": 1, "width": 150 },
                              { "id": "K_ENTER", "text": "*Enter*", "sp": 1, "width": 200 }
                          ]
                      }
                  ]
              }
          ]
      },
      "phone":
      {
          "defaultHint": 'dot',
          "font": "Tahoma,Helvetica",
          "layer": [
              {
                  "id": "default",
                  "row": [
                      {
                          "id": 0,
                          "key": [
                              { "id": "K_1" },
                              { "id": "K_2" },
                              { "id": "K_3" },
                              { "id": "K_4" },
                              { "id": "K_5" },
                              { "id": "K_6" },
                              { "id": "K_7" },
                              { "id": "K_8" },
                              { "id": "K_9" },
                              { "id": "K_0" },
                              { "id": "K_HYPHEN" },
                              { "id": "K_EQUAL" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 1,
                          "key": [
                              { "id": "K_Q", "pad": 25 },
                              { "id": "K_W" },
                              { "id": "K_E" },
                              { "id": "K_R" },
                              { "id": "K_T" },
                              { "id": "K_Y" },
                              { "id": "K_U" },
                              { "id": "K_I" },
                              { "id": "K_O" },
                              { "id": "K_P" },
                              { "id": "K_LBRKT" },
                              { "id": "K_RBRKT" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 2,
                          "key": [
                              { "id": "K_A", "pad": 50 },
                              { "id": "K_S" },
                              { "id": "K_D" },
                              { "id": "K_F" },
                              { "id": "K_G" },
                              { "id": "K_H" },
                              { "id": "K_J" },
                              { "id": "K_K" },
                              { "id": "K_L" },
                              { "id": "K_COLON" },
                              { "id": "K_QUOTE" },
                              { "id": "K_BKSLASH", "width": 90 }
                          ]
                      },
                      {
                          "id": 3,
                          "key": [
                              { "id": "K_oE2", "width": 90 },
                              { "id": "K_Z" },
                              { "id": "K_X" },
                              { "id": "K_C" },
                              { "id": "K_V" },
                              { "id": "K_B" },
                              { "id": "K_N" },
                              { "id": "K_M" },
                              { "id": "K_COMMA" },
                              { "id": "K_PERIOD" },
                              { "id": "K_SLASH" },
                              { "id": "K_BKQUOTE" },
                              { "sp": 10, "width": 1 }
                          ]
                      },
                      {
                          "id": 4,
                          "key": [
                              {
                                  "id": "K_SHIFT", "text": "*Shift*", "sp": 1, "width": 200, "sk": [
                                      { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": 1, "width": 50, "nextlayer": "ctrl" },
                                      { "id": "K_LCONTROL", "text": "*LCtrl*", "sp": 1, "width": 50, "nextlayer": "leftctrl" },
                                      { "id": "K_RCONTROL", "text": "*RCtrl*", "sp": 1, "width": 50, "nextlayer": "rightctrl" },
                                      { "id": "K_LALT", "text": "*Alt*", "sp": 1, "width": 50, "nextlayer": "alt" },
                                      { "id": "K_LALT", "text": "*LAlt*", "sp": 1, "width": 50, "nextlayer": "leftalt" },
                                      { "id": "K_RALT", "text": "*RAlt*", "sp": 1, "width": 50, "nextlayer": "rightalt" },
                                      { "id": "K_ALTGR", "text": "*AltGr*", "sp": 1, "width": 50, "nextlayer": "ctrl-alt" }]
                              },
                              { "id": "K_LOPT", "text": "*Menu*", "width": 150, "sp": 1 },
                              { "id": "K_SPACE", "width": 570, "text": "" },
                              { "id": "K_BKSP", "text": "*BkSp*", "width": 150, "sp": 1 },
                              { "id": "K_ENTER", "text": "*Enter*", "width": 200, "sp": 1 }
                          ]
                      }
                  ]
              }
          ]
      }
  } as TouchLayout.TouchLayoutFile;
  /* c8 ignore end */
}
