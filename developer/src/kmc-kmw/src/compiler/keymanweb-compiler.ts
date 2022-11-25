import { VisualKeyboard } from "@keymanapp/common-types";

import * as path from 'path';
import { KMXFile } from "../../../../../common/web/types/src/kmx/kmx";
// import VisualKeyboardCompiler from "./visual-keyboard-compiler.js";

export interface KeymanWebCompilerOptions {
  debug?: boolean;
};

export class KeymanWebCompiler {
  private readonly options: KeymanWebCompilerOptions;
  private readonly nl: string;
  private readonly tab: string;

  constructor(options?: KeymanWebCompilerOptions) {
    this.options = options;
    this.nl = this.options.debug ? "\n" : '';
    this.tab = this.options.debug ? "  " : '';
  }

  public compile(name: string, source: KMXFile): string {
    const nl = this.nl, tab = this.tab;

    // TODO: fill in from CompileKeymanWeb.pas

    const sName = 'Keyboard_'+this.cleanName(name);
    const displayUnderlying = true; // TODO-LDML
    const modifierBitmask = 0; // TODO-LDML: define the modifiers used by this keyboard
    const vkDictionary = '';  // TODO-LDML: vk dictionary for touch keys
    const hasSupplementaryPlaneChars = false; // TODO-LDML
    const isRTL = false; // TODO-LDML

    let result =
      `if(typeof keyman === 'undefined') {${nl}` +
      `${tab}console.error('Keyboard requires KeymanWeb ${MINIMUM_KMW_VERSION} or later');${nl}` +
      `} else {${nl}` +
      `${tab}KeymanWeb.KR(new ${sName}());${nl}` +
      `}${nl}` +
      `function ${sName}() {${nl}` +
      // `${tab}${this.setupDebug()}${nl}` + ? we may use this for modifierBitmask in future
      // `${tab}this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;${nl}` + ? we probably don't need this, it's for back-compat
      `${tab}this.KI="${sName}";${nl}` +
      `${tab}this.KN=${JSON.stringify(source.keyboard.names.name[0])};${nl}` +
      `${tab}this.KMINVER=${JSON.stringify(MINIMUM_KMW_VERSION)};${nl}` +
      `${tab}this.KV=${this.compileVisualKeyboard(source)};${nl}` +
      `${tab}this.KDU=${displayUnderlying ? '1' : '0'};${nl}` +
      `${tab}this.KH="";${nl}` +  // TODO-LDML: help text not supported
      `${tab}this.KM=0;${nl}` +  // TODO-LDML: mnemonic layout not supported for LDML keyboards
      `${tab}this.KBVER=${JSON.stringify(source.keyboard.version.number)};${nl}` +
      `${tab}this.KMBM=${modifierBitmask};${nl}`;

    if(isRTL) {
      result += `${tab}this.KRTL=1;${nl}`;
    }

    if(hasSupplementaryPlaneChars) {
      result += `${tab}this.KS=1;${nl}`;
    }

    if(vkDictionary != '') {
      result += `${tab}this.KVKD=${JSON.stringify(vkDictionary)};${nl}`;
    }

    let layoutFile = this.compileTouchLayout(source);
    if(layoutFile != '') {
      result += `${tab}this.KVKL=${layoutFile};${nl}`;
    }
    // TODO-LDML: KCSS not supported

    // TODO-LDML: embed binary keyboard for loading into Core

    // A LDML keyboard has a no-op for its gs() (begin Unicode) function,
    // because the functionality is embedded in Keyman Core
    result += `${tab}this.gs=function(t,e){${nl}`+
              `${tab}${tab}return 0;${nl}`+ // TODO-LDML: we will start by embedding call into Keyman Core here
              `${tab}};${nl}`;

    result += `}${nl}`;
    return result;
  }
}