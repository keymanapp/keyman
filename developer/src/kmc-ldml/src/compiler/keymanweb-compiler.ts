import { CompilerCallbacks, VisualKeyboard, KeymanFileTypes } from "@keymanapp/common-types";
import { LDMLKeyboard, TouchLayoutFileWriter } from "@keymanapp/developer-utils";
import { LdmlCompilerOptions } from "./ldml-compiler-options.js";
import { TouchLayoutCompiler } from "./touch-layout-compiler.js";
import { LdmlKeyboardVisualKeyboardCompiler } from "./visual-keyboard-compiler.js";

const MINIMUM_KMW_VERSION = '16.0';

export class LdmlKeyboardKeymanWebCompiler {
  private readonly options: LdmlCompilerOptions;
  private readonly nl: string;
  private readonly tab: string;
  constructor(private callbacks: CompilerCallbacks, options?: LdmlCompilerOptions) {
    this.options = { ...options };
    this.nl = this.options.saveDebug ? "\n" : '';
    this.tab = this.options.saveDebug ? "  " : '';
  }

  public compileVisualKeyboard(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile) {
    const nl = this.nl, tab = this.tab;
    const vkc = new LdmlKeyboardVisualKeyboardCompiler(this.callbacks);
    const vk: VisualKeyboard.VisualKeyboard = vkc.compile(source);

    let result =
      `{F: ' 1em ${JSON.stringify(vk.header.unicodeFont.name)}', `+
      `K102: ${vk.header.flags & VisualKeyboard.VisualKeyboardHeaderFlags.kvkh102 ? 1 : 0}};${nl}` + // TODO-LDML: escape ' and " in font name correctly
      `${tab}this.KV.KLS={${nl}` +
      `${tab}${tab}TODO_LDML: ${vk.keys.length}${nl}` +
      // TODO-LDML: fill in KLS
      `${tab}}`;

    return result;
  }

  public compileTouchLayout(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile) {
    const tlcompiler = new TouchLayoutCompiler();
    const layout = tlcompiler.compileToJavascript(source);
    const writer = new TouchLayoutFileWriter({formatted: this.options.saveDebug});
    return writer.compile(layout);
  }

  private cleanName(name: string): string {
    let result = this.callbacks.path.basename(name, KeymanFileTypes.Source.LdmlKeyboard);
    if(!result) {
      throw new Error(`Invalid file name ${name}`);
    }

    result = result.replaceAll(/[^a-z0-9]/g, '_');
    if(result.match(/^[0-9]/)) {
      // Can't have a digit as initial
      result = '_' + result;
    }
    return result;
  }

  public compile(name: string, source: LDMLKeyboard.LDMLKeyboardXMLSourceFile): string {
    const nl = this.nl, tab = this.tab;

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
      `${tab}this.KN=${JSON.stringify(source.keyboard3.info.name)};${nl}` +
      `${tab}this.KMINVER=${JSON.stringify(MINIMUM_KMW_VERSION)};${nl}` +
      `${tab}this.KV=${this.compileVisualKeyboard(source)};${nl}` +
      `${tab}this.KDU=${displayUnderlying ? '1' : '0'};${nl}` +
      `${tab}this.KH="";${nl}` +  // TODO-LDML: help text not supported
      `${tab}this.KM=0;${nl}` +  // TODO-LDML: mnemonic layout not supported for LDML keyboards
      `${tab}this.KBVER=${JSON.stringify(source.keyboard3.version?.number || '0.0')};${nl}` +
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
