import { KMX, CompilerOptions, CompilerCallbacks, KvkFileReader, VisualKeyboard, KmxFileReader, KvkFile } from "@keymanapp/common-types";
import { ExpandSentinel, incxstr, xstrlen } from "./util.js";
import { options, nl, FTabStop, setupGlobals, callbacks, FFix183_LadderLength, FCallFunctions, fk, minimumKeymanVersionToString, isKeyboardVersion10OrLater, isKeyboardVersion17OrLater } from "./compiler-globals.js";
import { JavaScript_ContextMatch, JavaScript_KeyAsString, JavaScript_Name, JavaScript_OutputString, JavaScript_Rules, JavaScript_Shift, JavaScript_ShiftAsString, JavaScript_Store, zeroPadHex } from './javascript-strings.js';
import { KmwCompilerMessages } from "./kmw-compiler-messages.js";
import { ValidateLayoutFile } from "./validate-layout-file.js";
import { VisualKeyboardFromFile } from "./visual-keyboard-compiler.js";
import { KmnCompilerResult, STORETYPE_DEBUG, STORETYPE_OPTION, STORETYPE_RESERVED } from "../compiler/compiler.js";

function requote(s: string): string {
  return "'" + s.replaceAll(/(['\\])/g, "\\$1") + "'";
}

export function RequotedString(s: string, RequoteSingleQuotes: boolean = false): string {
  // TODO: use a JSON encode
  let i: number = 0;
  while(i < s.length) {
    if (s.charAt(i) == '"' || s.charAt(i) == '\\') {
      s = s.substring(0, i) + '\\' + s.substring(i);
      i++;
    }
    else if (s.charAt(i) == '\'' && RequoteSingleQuotes) {
      s = s.substring(0, i) + '\\' + s.substring(i);
      i++;
    }
    else if(s.charAt(i) == '\n') {
      s = s.substring(0, i) + ' ' + s.substring(i + 1);
    }
    else if(s.charAt(i) == '\r') {
      // Yes, `\r` gets converted to `\n`, per kmcomp pattern
      // in the future we may change this
      s = s.substring(0, i) + '\\n' + s.substring(i + 1);
    }
    i++;
  }
  return s;
}

export function WriteCompiledKeyboard(
  callbacks: CompilerCallbacks,
  kmnfile: string,
  keyboardData: Uint8Array,
  kvkData: Uint8Array,
  kmxResult: KmnCompilerResult,
  FDebug: boolean = false
): string {
  let opts: CompilerOptions = {
    shouldAddCompilerVersion: false,
    saveDebug: FDebug
  };
  const reader = new KmxFileReader();
  const keyboard: KMX.KEYBOARD = reader.read(keyboardData);

  setupGlobals(callbacks, opts, FDebug?'  ':'', FDebug?'\r\n':'', kmxResult, keyboard, kmnfile);

  const isStoreType = (index:number, type: number) => !!(kmxResult.extra.stores[index].storeType & type);
  const isDebugStore = (index: number) => isStoreType(index, STORETYPE_DEBUG);
  const isReservedStore = (index: number) => isStoreType(index, STORETYPE_RESERVED);
  const isOptionStore = (index: number) => isStoreType(index, STORETYPE_OPTION);
  const getStoreLine = (index: number) => kmxResult.extra.stores[index].line;

  let vMnemonic: number = 0;
  let sRTL: string = "", sHelp: string = "''", sHelpFile: string = "",
      sEmbedJSFilename: string = "", sEmbedCSSFilename: string = "";
  let sVisualKeyboardFilename: string = "", sFullName: string = "";
  let sBegin_NewContext: string = "", sBegin_PostKeystroke: string = "";
  let sLayoutFilename: string = "", sVKDictionary: string = "";
  let linecomment: string;  // I3438
  let sModifierBitmask: string;
  let FOptionStores: string;
  let FKeyboardVersion = "1.0";
  let sHelpFileStoreIndex, sEmbedJSStoreIndex, sEmbedCSSStoreIndex;

  let result = "";
	// Locate the name of the keyboard
	for(let i = 0; i < keyboard.stores.length; i++) {
    const fsp = keyboard.stores[i];
    if(fsp.dwSystemID == KMX.KMXFile.TSS_NAME) {
      sFullName = fsp.dpString;
    }
    else if(fsp.dwSystemID == KMX.KMXFile.TSS_KEYBOARDVERSION) {   // I4155
      FKeyboardVersion = fsp.dpString;
    }
    else if (fsp.dpName == 'HelpFile' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_HELPFILE) {
      sHelpFile = fsp.dpString;
      sHelpFileStoreIndex = i;
    }
    else if (fsp.dpName == 'Help' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_HELPTEXT) {
      sHelp = '"'+RequotedString(fsp.dpString)+'"';
    }
    else if (fsp.dpName == 'VisualKeyboard' || fsp.dwSystemID == KMX.KMXFile.TSS_VISUALKEYBOARD) {
      sVisualKeyboardFilename = fsp.dpString;
    }
    else if (fsp.dpName == 'EmbedJS' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_EMBEDJS) {
      sEmbedJSFilename = fsp.dpString;
      sEmbedJSStoreIndex = i;
    }
    else if (fsp.dpName == 'EmbedCSS' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_EMBEDCSS) {   // I4368
      sEmbedCSSFilename = fsp.dpString;
      sEmbedCSSStoreIndex = i;
    }
    else if (fsp.dpName == 'RTL' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_RTL) {
      sRTL = fsp.dpString == '1' ? FTabStop+'this.KRTL=1;'+nl : '';   // I3681
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_MNEMONIC) {
      vMnemonic = fsp.dpString == '1' ? 1 : 0;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_VKDICTIONARY) {  // I3438
      sVKDictionary = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_LAYOUTFILE) {  // I3483
      sLayoutFilename = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_BEGIN_NEWCONTEXT) {
      sBegin_NewContext = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_BEGIN_POSTKEYSTROKE) {
      sBegin_PostKeystroke = fsp.dpString;
    }
  }

  const sName = 'Keyboard_'+getKeymanWebCompiledNameFromFileName(kmnfile);

  if (sHelpFile != '') {
    sHelp = '';
    sHelpFile = callbacks.resolveFilename(kmnfile, sHelpFile);
    try {
      const data = callbacks.loadFile(sHelpFile);
      let html = new TextDecoder().decode(data);
      if(!html.endsWith('\n')) html += '\n'; // CompileKeymanWeb.pas adds a new line at EOF
      sHelp = html.replace(/\r/g, '').replace(/\n/g, ' ');
      sHelp = requote(sHelp);
    } catch(e) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_HelpFileMissing({line: getStoreLine(sHelpFileStoreIndex), helpFilename: sHelpFile, e}));
      sHelp = '';
    }
  }

  let sEmbedJS = '';
  if (sEmbedJSFilename != '') {
    sEmbedJSFilename = callbacks.resolveFilename(kmnfile, sEmbedJSFilename);
    try {
      const data = callbacks.loadFile(sEmbedJSFilename);
      sEmbedJS = new TextDecoder().decode(data);
    } catch(e) {
      callbacks.reportMessage(KmwCompilerMessages.Warn_EmbedJsFileMissing({line: getStoreLine(sEmbedJSStoreIndex), jsFilename: sEmbedJSFilename, e}));
      sEmbedJS = '';
    }
  }

  let sEmbedCSS = '';
  if (sEmbedCSSFilename != '') {   // I4368
    sEmbedCSSFilename = callbacks.resolveFilename(kmnfile, sEmbedCSSFilename);
    try {
      const data = callbacks.loadFile(sEmbedCSSFilename);
      sEmbedCSS = new TextDecoder().decode(data);
      if(sEmbedCSS != '' && !sEmbedCSS.endsWith('\r\n')) sEmbedCSS += '\r\n';  // match CompileKeymanWeb.pas
    } catch(e) {
      // TODO(lowpri): rename error constant to Warn_EmbedFileMissing
      callbacks.reportMessage(KmwCompilerMessages.Warn_EmbedJsFileMissing({line: getStoreLine(sEmbedCSSStoreIndex), jsFilename: sEmbedCSSFilename, e}));
      sEmbedCSS = '';
    }
  }

  let sLayoutFile = '';
  if (sLayoutFilename != '') {  // I3483
    sLayoutFilename = callbacks.resolveFilename(kmnfile, sLayoutFilename);

    let result = ValidateLayoutFile(keyboard, options.saveDebug, sLayoutFilename, sVKDictionary, kmxResult.displayMap);
    if(!result) {
      sLayoutFile = '';
      return null;
    } else if(!result.result) {
      sLayoutFile = '';
      callbacks.reportMessage(KmwCompilerMessages.Error_InvalidTouchLayoutFile({filename:sLayoutFilename}));
      return null;
    } else {
      sLayoutFile = result.output;
    }
  }

  // Default to hide underlying layout characters. This is overridden by touch
  // layout platform.displayUnderlying property or, if that is not present for
  // the given platform, by the OSK property.
  let fDisplayUnderlying = false;
  let sVisualKeyboard = 'null'; // 'null' as a string is correct

  if (sVisualKeyboardFilename != '') {
    const reader = new KvkFileReader();
    const kvk: VisualKeyboard.VisualKeyboard = reader.read(kvkData);
    fDisplayUnderlying = !!(kvk.header.flags & KvkFile.BUILDER_KVK_HEADER_FLAGS.kvkhDisplayUnderlying);
    sVisualKeyboard = VisualKeyboardFromFile(kvk, options.saveDebug);
  }

  const fMnemonic = vMnemonic == 1;

  sModifierBitmask = GetKeyboardModifierBitmask(keyboard, fMnemonic);

  result +=
    `${JavaScript_SetupProlog()}${nl}` +
    `KeymanWeb.KR(new ${sName}());${nl}` +
    `${JavaScript_SetupEpilog()}${nl}` +
    `function ${sName}()${nl}` +
    `{${nl}` +
    `${FTabStop}${JavaScript_SetupDebug()}${nl}` +
    // Following line caches the Keyman major version
    `${FTabStop}this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;${nl}` +
    `${FTabStop}this.KI="${sName}";${nl}` +
    `${FTabStop}this.KN="${RequotedString(sFullName)}";${nl}`;

  // We split the result text here to allow for setting the minimum required
  // version at the very end of this function
  const resultPrefix = result;

  result =
    `${FTabStop}this.KV=${sVisualKeyboard};${nl}` +
    `${FTabStop}this.KDU=${fDisplayUnderlying?'1':'0'};${nl}` +
    `${FTabStop}this.KH=${sHelp};${nl}` +
    `${FTabStop}this.KM=${vMnemonic};${nl}` +
    `${FTabStop}this.KBVER="${FKeyboardVersion}";${nl}` +   // I4155
    `${FTabStop}this.KMBM=${sModifierBitmask};${nl}` +
    `${sRTL}`;   // I3681

  if (HasSupplementaryPlaneChars()) {
    result += `${FTabStop}this.KS=1;${nl}`;
  }

  if (sVKDictionary != '') {  // I3438
    result += `${FTabStop}this.KVKD="${RequotedString(sVKDictionary)}";${nl}`;   // I3681
  }

  if (sLayoutFile != '') {  // I3483
    result += `${FTabStop}this.KVKL=${sLayoutFile};${nl}`;   // I3681
  }

  if (sEmbedCSS != '') {   // I4368
    result += `${FTabStop}this.KCSS="${RequotedString(sEmbedCSS)}";${nl}`;
  }

	// Write the stores out
  FOptionStores = '';
	for(let i = 0; i < keyboard.stores.length; i++) {
    let fsp = keyboard.stores[i];
    // I3438 - Save all system stores to the keyboard, for now   // I3684

    if (!isDebugStore(i)) {
      if (fsp.dwSystemID == KMX.KMXFile.TSS_COMPARISON) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.dpName)}=${JavaScript_Store(keyboard, getStoreLine(i), fsp.dpString)};${nl}`;
      }
      else if (fsp.dwSystemID == KMX.KMXFile.TSS_COMPILEDVERSION) {
        result += `${FTabStop}this.KVER=${JavaScript_Store(keyboard, getStoreLine(i), fsp.dpString)};${nl}`;
      }
      else if (isOptionStore(i) && !isReservedStore(i)) {
        result += `${FTabStop}this.s${JavaScript_Name(i,fsp.dpName)}=KeymanWeb.KLOAD(this.KI,"${JavaScript_Name(i,fsp.dpName,true)}",`+
          `${JavaScript_Store(keyboard, getStoreLine(i), fsp.dpString)});${nl}`;

        if (FOptionStores != '') {
          FOptionStores += ',';
        }
        FOptionStores += `'s${JavaScript_Name(i, fsp.dpName)}'`;
      }
      else if (fsp.dwSystemID == KMX.KMXFile.TSS_NONE /* aka !isReservedStore(i) */) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.dpName)}=${JavaScript_Store(keyboard, getStoreLine(i), fsp.dpString)};${nl}`;   // I3681
      }
    }
  }

  result += `${FTabStop}this.KVS=[${FOptionStores}];${nl}`;

	// Write the groups out

  // I853 - begin unicode missing causes crash
  if (keyboard.startGroup.unicode == 0xFFFFFFFF) {
    callbacks.reportMessage(KmwCompilerMessages.Error_InvalidBegin());
    return null;
  }

  result += WriteBeginStatement(keyboard, 'gs', keyboard.startGroup.unicode);

  let rec = ExpandSentinel(keyboard, sBegin_NewContext, 0);
  if(rec.Code == KMX.KMXFile.CODE_USE) {
    result += WriteBeginStatement(keyboard, 'gn', rec.Use.GroupIndex);
  }
  rec = ExpandSentinel(keyboard, sBegin_PostKeystroke, 0);
  if(rec.Code == KMX.KMXFile.CODE_USE) {
    result += WriteBeginStatement(keyboard, 'gpk', rec.Use.GroupIndex);
  }

  let fgp = keyboard.groups[keyboard.startGroup.unicode];
  result +=
    `${FTabStop}this.gs=function(t,e) {${nl}` +
    `${FTabStop+FTabStop}return this.g${JavaScript_Name(keyboard.startGroup.unicode, fgp.dpName)}(t,e);${nl}` +
    `${FTabStop}};${nl}`; // I3681

	for(let i = 0; i < keyboard.groups.length; i++) {  // I1964
    let fgp = keyboard.groups[i];
    /*
      Note on `r` and `m` variables in a group function:

      `m` can have one of three values:
        0: no rule from this group was matched
        1: a rule from this group was matched and did not include a `use`
           statement
        2: a rule from this group matched and did include a `use` statement
           (#5440)

      `m` is only used within a rule group to control the firing of the
      `match` and `nomatch` rules.

      `r` can have one of two values:
        0: no rule from the final group matched (even if a rule from an
           higher-level group did)
        1: a rule from the final group did match;

      `r` serves as the rule group's return value and is forwarded
      recursively, best serving as a flag for whether or not default
      output for a key should be emitted (0 means yes, emit the
      default character output for that key).
    */

    result +=
      `${FTabStop}this.g${JavaScript_Name(i, fgp.dpName)}=function(t,e) {${nl}` +
      `${FTabStop+FTabStop}var k=KeymanWeb,r=${fgp.fUsingKeys?0:1},m=0;${nl}`;     //I1959

    // fkp := fgp.dpKeyArray;
    let HasRules = false;

    if (FFix183_LadderLength != 0) {
      result += JavaScript_Rules(keyboard, fMnemonic, fgp);
    }
    else {
      for (let j = 0; j < fgp.keys.length; j++) {    // I1964
        let fkp = fgp.keys[j];
        if (!RuleIsExcludedByPlatform(keyboard, fkp)) {
          result += FTabStop+FTabStop;   // I3681
          if (HasRules) {
            result += 'else ';
          }
          HasRules = true;

          if (fgp.fUsingKeys) {
            result += `if(k.KKM(e,${JavaScript_ShiftAsString(fkp, fMnemonic)},${JavaScript_KeyAsString(fkp, fMnemonic)})`;
          }

          if (xstrlen(fkp.dpContext) > 0) {
            result += fgp.fUsingKeys ? '&&' : 'if(';
            result += JavaScript_ContextMatch(keyboard, fkp, fkp.dpContext);
          }
          else if (!fgp.fUsingKeys) {
            result += 'if(1';
          }

          linecomment = (fkp.Line > 0 && FDebug) ? `   // Line ${fkp.Line}` : '';

          result +=
            `) {${linecomment}${nl}` +
            FTabStop+FTabStop+FTabStop;
          if (fgp.fUsingKeys) {                                                                                      // I1959
            result += 'r=m=1;' + JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, fkp, fkp.dpOutput, fgp);    // I1959   // I3681
          }
          else {
            result += 'm=1;' + JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, fkp, fkp.dpOutput, fgp);    // I1959   // I3681
          }
          result += `${nl}${FTabStop}${FTabStop}}${nl}`;   // I3681
        }
      }
    }

		if(fgp.dpMatch) {
      result +=
        `${FTabStop+FTabStop}if(m==1) {${nl}`+
        `${FTabStop+FTabStop}${JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, null, fgp.dpMatch, fgp)}${nl}`+
        `${FTabStop+FTabStop}}${nl}`;
    }
		if(fgp.dpNoMatch) {
      if(fgp.fUsingKeys) {    // I1382 - fixup m=1 to m=g()
        result +=
          `${FTabStop+FTabStop}if(!m&&k.KIK(e)) {${nl}`+
          `${FTabStop+FTabStop+FTabStop}r=1;${JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, null, fgp.dpNoMatch, fgp)}${nl}`+
          `${FTabStop+FTabStop}}${nl}`;   // I1959. part 2, I2224   // I3681
      }
      else {
        result +=
          `${FTabStop+FTabStop}if(!m) {${nl}`+
          `${FTabStop+FTabStop}${JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, null, fgp.dpNoMatch, fgp)}${nl}`+
          // TODO: add FTabStop to line above, once we have 100% identical match
          //       to legacy compiler, but because legacy compiler missed this
          //       tabstop we'll leave it out here for now
          `${FTabStop+FTabStop}}${nl}`;
      }
    }
    result +=
      `${FTabStop+FTabStop}return r;${nl}`+
      `${FTabStop}};${nl}`;
  }

  for(let n = 0; n < FCallFunctions.length; n++) {
    const s = callbacks.resolveFilename(kmnfile, FCallFunctions[n] + '.call_js');
    if(callbacks.fs.existsSync(s)) {
      const data = new TextDecoder().decode(callbacks.loadFile(s));
      result += `${FTabStop}this.c${n}=function(t,e){${data.trim()}};${nl}`;
    } else {
      result += `${FTabStop}this.c${n}=function(t,e){alert("call(${FCallFunctions[n]}) not defined");};${nl}`;
    }
  }

  result += sEmbedJS + '}' + nl;   // I3681

  const minVer = minimumKeymanVersionToString();
  const resultMinVer = `${FTabStop}this.KMINVER="${minVer}";${nl}`;

  if((fk.flags & KMX.KMXFile.KF_AUTOMATICVERSION) == KMX.KMXFile.KF_AUTOMATICVERSION) {
    // Note: the KeymanWeb compiler is responsible for reporting minimum
    // version for the web targets
    callbacks.reportMessage(KmwCompilerMessages.Info_MinimumWebEngineVersion({version:minVer}));
  }

  return resultPrefix + resultMinVer + result;
}

///
/// Determine the modifiers used in the target keyboard and return a bitmask
/// representing them, or an number value when not in debug mode
///
/// @return string of JavaScript code, e.g. 'modCodes.SHIFT | modCodes.CTRL /* 0x0030 */'
///
function GetKeyboardModifierBitmask(keyboard: KMX.KEYBOARD, fMnemonic: boolean): string {
  let bitMask = 0;
  for(let gp of keyboard.groups) {
    if(gp.fUsingKeys) {
      for(let kp of gp.keys) {
        if(!RuleIsExcludedByPlatform(keyboard, kp)) {
          bitMask |= JavaScript_Shift(kp, fMnemonic);
        }
      }
    }
  }

  if ((bitMask & KMX.KMXFile.MASK_MODIFIER_CHIRAL) && (bitMask & KMX.KMXFile.MASK_MODIFIER_NONCHIRAL)) {
    callbacks.reportMessage(KmwCompilerMessages.Warn_DontMixChiralAndNonChiralModifiers());
  }

  if(options.saveDebug) {
    return FormatModifierAsBitflags(bitMask & KMX.KMXFile.MASK_KEYS); // Exclude KMX_ISVIRTUALKEY, KMX_VIRTUALCHARKEY
  }

  return '0x'+(bitMask & KMX.KMXFile.MASK_KEYS).toString(16).toUpperCase();
}


///
/// If debug mode, then returns Javascript code necessary for
/// accessing constants in the compiled keyboard
///
/// @return string of JavaScript code
///
function JavaScript_SetupDebug() {
  if(isKeyboardVersion10OrLater()) {
    if(options.saveDebug) {
      if(isKeyboardVersion17OrLater()) {
        return 'var modCodes = KeymanWeb.Codes.modifierCodes;'+nl+
              FTabStop+'var keyCodes = KeymanWeb.Codes.keyCodes;'+nl;
      } else {
        return 'var modCodes = keyman.osk.modifierCodes;'+nl+
              FTabStop+'var keyCodes = keyman.osk.keyCodes;'+nl;
      }
    }
  }
  return '';
}

function JavaScript_SetupProlog() {
  if(isKeyboardVersion10OrLater()) {
    return 'if(typeof keyman === \'undefined\') {'+nl+
      FTabStop+'console.log(\'Keyboard requires KeymanWeb 10.0 or later\');'+nl+
      FTabStop+'if(typeof tavultesoft !== \'undefined\') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");'+nl+
      '} else {';
  }
  return '';
}

function JavaScript_SetupEpilog() {
  if(isKeyboardVersion10OrLater()) {
    return '}';
  }
  return '';
}

function HasSupplementaryPlaneChars() {
  const suppChar = /[\uD800-\uDBFF][\uDC00-\uDFFF]/;

  for(let sp of fk.stores) {
    if(suppChar.test(sp.dpString)) {
      return true;
    }
  }

  for(let gp of fk.groups) {
    for(let kp of gp.keys) {
      if(suppChar.test(kp.dpContext) || suppChar.test(kp.dpOutput)) {
        return true;
      }
    }
    if(suppChar.test(gp.dpMatch) || suppChar.test(gp.dpNoMatch)) {
      return true;
    }
  }
  return false;
}

export function RuleIsExcludedByPlatform(keyboard: KMX.KEYBOARD, fkp: KMX.KEY): boolean {
  if(fkp.dpContext == null || fkp.dpContext == '') {
    return false;
  }

  let x = 0;
  while(x < fkp.dpContext.length) {
    let rec = ExpandSentinel(keyboard, fkp.dpContext, x);
    if(rec.IsSentinel &&
        (rec.Code == KMX.KMXFile.CODE_IFSYSTEMSTORE) &&
        (rec.IfSystemStore.dwSystemID == KMX.KMXFile.TSS_PLATFORM) &&
        rec.IfSystemStore.Store.dpString.includes('native')) {
      if(rec.IfSystemStore.Store.dpString.match(/windows|desktop|macosx|linux/)) {
        return true;
      }
    }
    x = incxstr(fkp.dpContext, x);
  }

  return false;
}

function WriteBeginStatement(keyboard: KMX.KEYBOARD, name: string, groupIndex: number): string {
  const fgp = keyboard.groups[groupIndex];
  return `${FTabStop}this.${name}=function(t,e) {${nl}`+
         `${FTabStop+FTabStop}return this.g${JavaScript_Name(groupIndex, fgp.dpName)}(t,e);${nl}`+
         `${FTabStop}};${nl}`;
}

/**
 * Converts a modifier bit mask integer into its component bit flags
 *
 * @param FBitMask A KMX modifier bitmask value
 *
 * @return string of JavaScript code, e.g. 'modCodes.SHIFT | modCodes.CTRL /* 0x0030 ./'
**/
export function FormatModifierAsBitflags(FBitMask: number): string {
  const mask: string[] = [
    'LCTRL',             // 0X0001
    'RCTRL',             // 0X0002
    'LALT',              // 0X0004
    'RALT',              // 0X0008

    'SHIFT',             // 0X0010
    'CTRL',              // 0X0020
    'ALT',               // 0X0040

    '???',               // Reserved

    'CAPS',              // 0X0100
    'NO_CAPS',           // 0X0200

    'NUM_LOCK',          // 0X0400
    'NO_NUM_LOCK',       // 0X0800

    'SCROLL_LOCK',       // 0X1000
    'NO_SCROLL_LOCK',    // 0X2000

    'VIRTUAL_KEY'        // 0X4000
  ];

  let i: number;
  let result = '';

  //TODO: We need to think about mnemonic layouts which are incompletely supported at present
  //tavultesoft.keymanweb.osk.

  if(isKeyboardVersion10OrLater()) {
    // This depends on flags defined in KeymanWeb 10.0
    result = '';

    for(i = 0; i < mask.length; i++) {
      if(FBitMask & (1 << i)) {
        if(result != '') result += ' | ';
        result += 'modCodes.'+mask[i];
      }
    }

    if(result == '') {
      result = '0';
    }

    result += ' /* 0x' + zeroPadHex(FBitMask, 4) + ' */';
  }
  else {
    result = '0x'+zeroPadHex(FBitMask, 4);
  }
  return result;
}

function cleanKeyboardID(name: string): string {
  name = name.toLowerCase();
  if(name.length == 0) {
    return name;
  }
  if(name[0].match(/\d/)) {
    name = '_' + name;
  }

  let result = '';
  for(let i = 0; i < name.length; i++) {
    if(!name[i].match(/[a-z0-9_]/)) {
      result += '_';
    } else {
      result += name[i];
    }
  }
  return result;
}

function getKeymanWebCompiledNameFromFileName(filename: string): string {
  let m = /([^/\\]+)$/.exec(filename);
  if(!m) {
    return null;
  }
  m = /^(.+?)(\.[^.]+)?$/.exec(m[1]);
  if(!m) {
    return null;
  }
  return cleanKeyboardID(m[1]);
}