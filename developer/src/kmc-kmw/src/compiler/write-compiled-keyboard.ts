import { KMX, CompilerOptions, CompilerCallbacks, KvkFileReader, KvksFileReader, VisualKeyboard, KeymanFileTypes } from "@keymanapp/common-types";
import { ExpandSentinel, incxstr, xstrlen } from "../util/util.js";
import { options, nl, FTabStop, setupGlobals, IsKeyboardVersion10OrLater, callbacks } from "./compiler-globals.js";
import { JavaScript_ContextMatch, JavaScript_KeyAsString, JavaScript_Name, JavaScript_OutputString, JavaScript_Rules, JavaScript_Shift, JavaScript_ShiftAsString, JavaScript_Store, zeroPadHex } from './javascript-strings.js';
import { KmwCompilerMessages } from "./messages.js";
import { ValidateLayoutFile } from "./validate-layout-file.js";
import { VisualKeyboardFromFile } from "./visual-keyboard-compiler.js";

export let FFix183_LadderLength: number = 100; // TODO: option

function requote(s: string): string {
  return "'" + s.replaceAll(/(['\\])/, "\\$1") + "'";
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
      s = s.substring(0, i) + '\\n' + s.substring(i + 1);
    }
    else if(s.charAt(i) == '\r') {
      s = s.substring(0, i) + ' ' + s.substring(i + 1);
    }
    i++;
  }
  return s;
}

export function WriteCompiledKeyboard(callbacks: CompilerCallbacks, kmnfile: string, kmxfile: string, name: string, keyboard: KMX.KEYBOARD, FDebug: boolean = false): string {
  let opts: CompilerOptions = {
    shouldAddCompilerVersion: false,
    saveDebug: FDebug
  };
  setupGlobals(callbacks, opts, FDebug?'  ':'', FDebug?'\r\n':'', keyboard, kmnfile);

  // let fgp: GROUP;

  // let fsp: STORE;
	// let fkp: KEY;

  // let j: number;
  // let n: number;

  let vMnemonic: number = 0;
  let /*s: string,*/ sRTL: string = "", sHelp: string = "''", sHelpFile: string = "",
      sEmbedJS: string = "", sEmbedCSS: string = "";
  let sVisualKeyboard: string = "", sFullName: string = "";
  let sBegin_NewContext: string = "", sBegin_PostKeystroke: string = "";
  let sLayoutFile: string = "", sVKDictionary: string;
  let linecomment: string;  // I3438
  // let HasRules: boolean;
  let sModifierBitmask: string;
  let FOptionStores: string;
  let FKeyboardVersion = "1.0";
  // let rec: TSentinelRecord;

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
    }
    else if (fsp.dpName == 'Help' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_HELPTEXT) {
      sHelp = '"'+RequotedString(fsp.dpString)+'"';
    }
    else if (fsp.dpName == 'VisualKeyboard' || fsp.dwSystemID == KMX.KMXFile.TSS_VISUALKEYBOARD) {
      sVisualKeyboard = fsp.dpString;
    }
    else if (fsp.dpName == 'EmbedJS' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_EMBEDJS) {
      sEmbedJS = fsp.dpString;
    }
    else if (fsp.dpName == 'EmbedCSS' || fsp.dwSystemID == KMX.KMXFile.TSS_KMW_EMBEDCSS) {   // I4368
      sEmbedCSS = fsp.dpString;
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
      sLayoutFile = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_BEGIN_NEWCONTEXT) {
      sBegin_NewContext = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMX.KMXFile.TSS_BEGIN_POSTKEYSTROKE) {
      sBegin_PostKeystroke = fsp.dpString;
    }
  }

  const sName = 'Keyboard_'+name; //TODO: verify --> GetKeymanWebCompiledNameFromFileName(FInFile);

  if (sHelpFile != '') {
    sHelp = '';
    //TODO: load sHelpFile from file
    /*with TStringList.Create do
    try
      try
        LoadFromFile(ExtractFilePath(FInFile) + sHelpFile, TEncoding.UTF8);  // I3337
        for n := 0 to Count - 1 do
          sHelp := sHelp + Strings[n] + ' ';
      except
        on E:EFOpenError do
        begin
          ReportError(0, CWARN_HelpFileMissing, E.Message);  // I1971   // I4061
          sHelp := '';
        end;
      end;
    finally
      Free;
    end;*/

    sHelp = requote(sHelp);
  }

  if (sEmbedJS != '') {
    //TODO: load sEmbedJS from file
    /*try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sEmbedJS, TEncoding.UTF8);  // I3337
        sEmbedJS := Text;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_EmbedJsFileMissing, E.Message);   // I4061
        sEmbedJS := '';
      end;
    end;*/
  }

  if (sEmbedCSS != '') {   // I4368
    //TODO: load sEmbedCSS from file
    /*try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sEmbedCSS, TEncoding.UTF8);  // I3337
        sEmbedCSS := Text;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_EmbedJsFileMissing, E.Message);   // I4061
        sEmbedCSS := '';
      end;
    end;*/
  }

  if (sLayoutFile != '') {  // I3483
    let path = callbacks.resolveFilename(kmnfile, sLayoutFile);

    let result = ValidateLayoutFile(keyboard, options.saveDebug, path, sVKDictionary);
    if(!result.result) {
      sLayoutFile = '';
      callbacks.reportMessage(KmwCompilerMessages.Error_TouchLayoutFileInvalid());
      return null;
    } else {
      // TODO: reusing the same variable here is ugly
      sLayoutFile = result.output;
    }
  }

  // Default to hide underlying layout characters. This is overridden by touch
  // layout platform.displayUnderlying property or, if that is not present for
  // the given platform, by the OSK property.
  let fDisplayUnderlying = false;

  if (sVisualKeyboard != '') {
    // TODO: stop reusing sVisualKeyboard for both filename and content
    let path = callbacks.resolveFilename(kmnfile, sVisualKeyboard);

    let kvk: VisualKeyboard.VisualKeyboard;
    if(KeymanFileTypes.filenameIs(path, KeymanFileTypes.Source.VisualKeyboard)) {
      let reader = new KvksFileReader();
      let source = reader.read(callbacks.loadFile(path));
      reader.validate(source, callbacks.loadSchema("kvks")); // TODO: handle exceptions
      kvk = reader.transform(source);
      // TODO: log errors
    }
    else {
      // Note: very old keyboard sources may still have .kvk as an xml
      // file, but we'll treat that as an error rather than silently
      // falling back to KvksFileReader
      let reader = new KvkFileReader();
      kvk = reader.read(callbacks.loadFile(path));
    }

    let result = VisualKeyboardFromFile(kvk, options.saveDebug);
    if(!result.result) {
      // TODO: error
      sVisualKeyboard = 'null';
    }
    else {
      sVisualKeyboard = result.result;
    }
  }
  else {
    sVisualKeyboard = 'null';
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
    `${FTabStop}this.KN="${RequotedString(sFullName)}";${nl}` +
    `${FTabStop}this.KMINVER="${(keyboard.fileVersion & KMX.KMXFile.VERSION_MASK_MAJOR) >> 8}.${keyboard.fileVersion & KMX.KMXFile.VERSION_MASK_MINOR}";${nl}` +
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

  function isDebugStore(fsp: KMX.STORE) {
    return fsp.dwSystemID == KMX.KMXFile.TSS_DEBUG_LINE;
  }

  function isReservedStore(fsp: KMX.STORE) {
    return fsp.dwSystemID != KMX.KMXFile.TSS_NONE;
  }

  function isOptionStore(fsp: KMX.STORE) {
    // TODO: how do we determine this, see CheckStoreUsage()
    return false;
  }

	// Write the stores out
  FOptionStores = '';
	for(let i = 0; i < keyboard.stores.length; i++) {
    let fsp = keyboard.stores[i];
    // I3438 - Save all system stores to the keyboard, for now   // I3684

    if (!isDebugStore(fsp)) { // and not (fsp.dwSystemID in [TSS_BITMAP, TSS_NAME, TSS_VERSION, TSS_CUSTOMKEYMANEDITION, TSS_CUSTOMKEYMANEDITIONNAME, TSS_KEYMANCOPYRIGHT]) then
      if (fsp.dwSystemID == KMX.KMXFile.TSS_COMPARISON) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.dpName)}=${JavaScript_Store(keyboard, 0/*fsp.line*/, fsp.dpString)};${nl}`;
      }
      else if (fsp.dwSystemID == KMX.KMXFile.TSS_COMPILEDVERSION) {
        result += `${FTabStop}this.KVER=${JavaScript_Store(keyboard, 0/*fsp.line*/, fsp.dpString)};${nl}`;
      }
      //else if fsp.dwSystemID = TSS_VKDICTIONARY then // I3438, required for vkdictionary
      //  Result := Result + Format('%sthis.s%s=%s;%s', [FTabStop, JavaScript_Name(i, fsp.szName), JavaScript_Store(fsp.line, fsp.dpString), nl])
      else if (isOptionStore(fsp) && !isReservedStore(fsp)) {
        result += `${FTabStop}this.s${JavaScript_Name(i,fsp.dpName)}=KeymanWeb.KLOAD(this.KI,"${JavaScript_Name(i,fsp.dpName,true)}",`+
          `${JavaScript_Store(keyboard, 0/*fsp.line*/, fsp.dpString)});${nl}`;

        if (FOptionStores != '') {
          FOptionStores += ',';
        }
        FOptionStores += `'s${JavaScript_Name(i, fsp.dpName)}`;
      }
      else if (fsp.dwSystemID == KMX.KMXFile.TSS_NONE /* aka not fsp.fIsReserved */) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.dpName)}=${JavaScript_Store(keyboard, 0/*fsp.line*/, fsp.dpString)};${nl}`;   // I3681
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
          `${FTabStop+FTabStop+FTabStop}${JavaScript_OutputString(keyboard, FTabStop + FTabStop + FTabStop, null, fgp.dpNoMatch, fgp)}${nl}`+
          `${FTabStop+FTabStop}}${nl}`;
      }
    }
    result +=
      `${FTabStop+FTabStop}return r;${nl}`+
      `${FTabStop}};${nl}`;
  }

/* TODO
  for(let n = 0 to FCallFunctions.Count - 1 do
  begin
    s := ExtractFilePath(FInFile) + FCallFunctions[n] + '.call_js';
    if FileExists(s) then
      with TStringList.Create do
      try
        LoadFromFile(s, TEncoding.UTF8);  // I3337
        Result := Result + Format('%sthis.c%d=function(t,e){%s};%s', [FTabstop, n, Trim(Text), nl]);   // I3681
      finally
        Free;
      end
    else
      Result := Result + Format('%sthis.c%d=function(t,e){alert("call(%s) not defined");};%s', [FTabstop, n, FCallFunctions[n], nl]);   // I3681
  end;
*/
  result += sEmbedJS + '}' + nl;   // I3681
  return result;
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
  if(IsKeyboardVersion10OrLater()) {
    if(options.saveDebug) {
      return 'var modCodes = keyman.osk.modifierCodes;'+nl+
             FTabStop+'var keyCodes = keyman.osk.keyCodes;'+nl;
    }
  }
  return '';
}

function JavaScript_SetupProlog() {
  if(IsKeyboardVersion10OrLater()) {
    return 'if(typeof keyman === \'undefined\') {'+nl+
      FTabStop+'console.log(\'Keyboard requires KeymanWeb 10.0 or later\');'+nl+
      FTabStop+'if(typeof tavultesoft !== \'undefined\') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");'+nl+
      '} else {';
  }
  return '';
}

function JavaScript_SetupEpilog() {
  if(IsKeyboardVersion10OrLater()) {
    return '}';
  }
  return '';
}

function HasSupplementaryPlaneChars() {
  return false; // TODO
/*  function StringHasSuppChars(p: PWideChar): Boolean;
  begin
    if not Assigned(p) then
      Exit(False);

    while p^ <> #0 do
    begin
      if Char.IsSurrogate(p, 0) then
        Exit(True);
      p := incxstr(p);
    end;

    Result := False;
  end;

var
  I: number;
  fsp: PFILE_STORE;
  fgp: PFILE_GROUP;
  j: number;
  fkp: PFILE_KEY;
begin
  fsp := fk.dpStoreArray;
  for i := 0 to number(fk.cxStoreArray) - 1 do
  begin
    if StringHasSuppChars(fsp.dpString) then
      Exit(True);
    Inc(fsp);
  end;

  fgp := fk.dpGroupArray;
  for i := 0 to number(fk.cxGroupArray) - 1 do
  begin
    fkp := fgp.dpKeyArray;
    for j := 0 to number(fgp.cxKeyArray) - 1 do
    begin
      if StringHasSuppChars(fkp.dpContext) or
         StringHasSuppChars(fkp.dpOutput) then
        Exit(True);
      Inc(fkp);
    end;

    if StringHasSuppChars(fgp.dpMatch) or
       StringHasSuppChars(fgp.dpNoMatch) then
      Exit(True);

    Inc(fgp);
  end;

  Result := False;
end;*/
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

  if(IsKeyboardVersion10OrLater()) {
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
