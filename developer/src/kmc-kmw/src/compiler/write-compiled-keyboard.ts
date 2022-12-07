import { GROUP, KEY, KMXFile, STORE } from "../../../../../common/web/types/src/kmx/kmx";

function requote(s: string): string {
  return "'" + s.replaceAll(/(['\\])/, "\\$1") + "'";
}

function RequotedString(s: string, RequoteSingleQuotes: boolean = false): string {
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
  }
  return s;
}

export function WriteCompiledKeyboard(name: string, source: KMXFile, FDebug: boolean = false): string {
  let nl = FDebug ? '\n' : '';
  let FTabStop = FDebug ? '  ' : '';

  // let fgp: GROUP;

  let fsp: STORE;
	let fkp: KEY;

  let j: number;
  let n: number;

  let vMnemonic: number = 0;
  let s: string, sRTL: string = "", sHelp: string = "''", sHelpFile: string = "",
      sEmbedJS: string = "", sEmbedCSS: string = "";
  let sVisualKeyboard: string = "", sFullName: string = "";
  let sBegin_NewContext: string, sBegin_PostKeystroke: string;
  let sLayoutFile: string = "", sVKDictionary: string;
  let linecomment: string;  // I3438
  let HasRules: boolean;
  let sModifierBitmask: string;
  let FOptionStores: string;
  let FKeyboardVersion = "1.0";
  // let rec: TSentinelRecord;

  let result = "";
	// Locate the name of the keyboard
	for(let i = 0; i < source.keyboard.stores.length; i++) {
    const fsp = source.keyboard.stores[i];
    if(fsp.dwSystemID == KMXFile.TSS_NAME) {
      sFullName = fsp.dpString;
    }
    else if(fsp.dwSystemID == KMXFile.TSS_KEYBOARDVERSION) {   // I4155
      FKeyboardVersion = fsp.dpString;
    }
    else if (fsp.dpName == 'HelpFile' || fsp.dwSystemID == KMXFile.TSS_KMW_HELPFILE) {
      sHelpFile = fsp.dpString;
    }
    else if (fsp.dpName == 'Help' || fsp.dwSystemID == KMXFile.TSS_KMW_HELPTEXT) {
      sHelp = '"'+RequotedString(fsp.dpString)+'"';
    }
    else if (fsp.dpName == 'VisualKeyboard' || fsp.dwSystemID == KMXFile.TSS_VISUALKEYBOARD) {
      sVisualKeyboard = fsp.dpString;
    }
    else if (fsp.dpName == 'EmbedJS' || fsp.dwSystemID == KMXFile.TSS_KMW_EMBEDJS) {
      sEmbedJS = fsp.dpString;
    }
    else if (fsp.dpName == 'EmbedCSS' || fsp.dwSystemID == KMXFile.TSS_KMW_EMBEDCSS) {   // I4368
      sEmbedCSS = fsp.dpString;
    }
    else if (fsp.dpName == 'RTL' || fsp.dwSystemID == KMXFile.TSS_KMW_RTL) {
      sRTL = fsp.dpString == '1' ? FTabStop+'this.KRTL=1;'+nl : '';   // I3681
    }
    else if (fsp.dwSystemID == KMXFile.TSS_MNEMONIC) {
      vMnemonic = fsp.dpString == '1' ? 1 : 0;
    }
    else if (fsp.dwSystemID == KMXFile.TSS_VKDICTIONARY) {  // I3438
      sVKDictionary = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMXFile.TSS_LAYOUTFILE) {  // I3483
      sLayoutFile = fsp.dpString;
    }
    else if (fsp.dwSystemID == KMXFile.TSS_BEGIN_NEWCONTEXT) {
      sBegin_NewContext = fsp.dpString;
    }
    else if (fsp.dwSystemID -= KMXFile.TSS_BEGIN_POSTKEYSTROKE) {
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
    // TODO: Load sLayoutFile from file
    /*try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sLayoutFile, TEncoding.UTF8);
        sLayoutFile := Text;
        if not ValidateLayoutFile(sLayoutFile, sVKDictionary) then   // I4060
        begin
          sLayoutFile := '';
        end;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_TouchLayoutFileMissing, E.Message);   // I4061
        sLayoutFile := '';
      end;
    end;*/
  }

  // Default to hide underlying layout characters. This is overridden by touch
  // layout platform.displayUnderlying property or, if that is not present for
  // the given platform, by the OSK property.
  let fDisplayUnderlying = false;

  if (sVisualKeyboard != '') {
    //TODO: Load sVisualKeyboard from file
    /*
    try
      // The Keyman .kmx compiler will change the value of this store from a
      // .kvks to a .kvk during the build. Earlier in the build, the visual keyboard
      // would have been compiled, so we need to account for that and use that file.

      sVisualKeyboard := VisualKeyboardFromFile(ExtractFilePath(FOutFile) + sVisualKeyboard, fDisplayUnderlying);
    except
      on E:EFOpenError do   // I3947
      begin
        ReportError(0, CWARN_VisualKeyboardFileMissing, E.Message);   // I4061
        sVisualKeyboard := 'null';
      end;
    end;
    */
  }
  else {
    sVisualKeyboard = 'null';
  }

  sModifierBitmask = GetKeyboardModifierBitmask(source);

  const fMnemonic = vMnemonic == 1;

  result +=
    `${JavaScript_SetupProlog}${nl}` +
    `'KeymanWeb.KR(new ${sName}());${nl}` +
    `${JavaScript_SetupEpilog}${nl}` +
    `function ${sName}()${nl}` +
    `{${nl}` +
    `${FTabStop}${JavaScript_SetupDebug}${nl}` +
    // Following line caches the Keyman major version
    `${FTabStop}this._v=(typeof keyman!="undefined"&&typeof keyman.version=="string")?parseInt(keyman.version,10):9;${nl}` +
    `${FTabStop}this.KI="${sName}";${nl}` +
    `${FTabStop}this.KN="${RequotedString(sFullName)}";${nl}`, nl,
    `${FTabStop}this.KMINVER="${(source.keyboard.fileVersion & KMXFile.VERSION_MASK_MAJOR) >> 8}.${source.keyboard.fileVersion & KMXFile.VERSION_MASK_MINOR}";${nl}` +
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
	for(let i = 0; i < source.keyboard.stores.length; i++) {
    let fsp = source.keyboard.stores[i];
    // I3438 - Save all system stores to the keyboard, for now   // I3684

    if (!fsp.fIsDebug) { // and not (fsp.dwSystemID in [TSS_BITMAP, TSS_NAME, TSS_VERSION, TSS_CUSTOMKEYMANEDITION, TSS_CUSTOMKEYMANEDITIONNAME, TSS_KEYMANCOPYRIGHT]) then
      if (fsp.dwSystemID == KMXFile.TSS_COMPARISON) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.dpName)}=${JavaScript_Store(fsp.line, fsp.dpString)};${nl}`;
      }
      else if (fsp.dwSystemID == KMXFile.TSS_COMPILEDVERSION) {
        result += `${FTabStop}this.KVER=${JavaScript_Store(fsp.line, fsp.dpString)};${nl}`;
      }
      //else if fsp.dwSystemID = TSS_VKDICTIONARY then // I3438, required for vkdictionary
      //  Result := Result + Format('%sthis.s%s=%s;%s', [FTabStop, JavaScript_Name(i, fsp.szName), JavaScript_Store(fsp.line, fsp.dpString), nl])
      else if (fsp.fIsOption && !fsp.fIsReserved) {
        result += `${FTabStop}this.s${JavaScript_Name(i,fsp.szName)}=KeymanWeb.KLOAD(this.KI,"${JavaScript_Name(i,fsp.szName,true)}",`+
          `${JavaScript_Store(fsp.line, fsp.dpString)});${nl}`;

        if (FOptionStores != '') {
          FOptionStores += ',';
        }
        FOptionStores += `'s${JavaScript_Name(i,fsp.szName)}`;
      }
      else if (fsp.dwSystemID == KMXFile.TSS_NONE /* aka not fsp.fIsReserved */) {
        result += `${FTabStop}this.s${JavaScript_Name(i, fsp.szName)}=${JavaScript_Store(fsp.line, fsp.dpString)};${nl}`;   // I3681
      }
    }
  }

  result += `${FTabStop}this.KVS=[${FOptionStores}];${nl}`;

	// Write the groups out

  // I853 - begin unicode missing causes crash
  if (fk.StartGroup[KMXFile.BEGIN_UNICODE] == 0xFFFFFFFF) {
    ReportError(0, CERR_InvalidBegin, 'A "begin unicode" statement is required to compile a KeymanWeb keyboard');
    return null;
  }

  result += WriteBeginStatement('gs', source.keyboard.startGroup.unicode);

  let rec = ExpandSentinel(sBegin_NewContext);
  if(rec.Code == CODE_USE) {
    result += WriteBeginStatement('gn', rec.Use.GroupIndex);
  }
  rec = ExpandSentinel(sBegin_PostKeystroke);
  if(rec.Code == CODE_USE) {
    result += WriteBeginStatement('gpk', rec.Use.GroupIndex);
  }

  let fgp = source.keyboard.groups[source.keyboard.startGroup.unicode];
  result +=
    `${FTabStop}this.gs=function(t,e) {${nl}` +
    `${FTabStop+FTabStop}return this.g${JavaScript_Name(source.keyboard.startGroup.unicode, fgp.dpName)}(t,e);${nl}` +
    `${FTabStop}}${nl}`; // I3681

	for(let i = 0; i < source.keyboard.groups.length; i++) {  // I1964
    let fgp = source.keyboard.groups[i];
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
      result += JavaScript_Rules(fgp);
    }
    else {
      for (let j = 0; j < fgp.keys.length; j++) {    // I1964
        let fkp = fgp.keys[j];
        if (!RuleIsExcludedByPlatform(fkp)) {
          result += FTabstop+FTabstop;   // I3681
          if (HasRules) {
            result += 'else ';
          }
          HasRules = true;

          if (fgp.fUsingKeys) {
            result += `if(k.KKM(e,${JavaScript_ShiftAsString(fkp, fMnemonic)},${JavaScript_KeyAsString(fkp, fMnemonic)})`;
          }

          if (xstrlen(fkp.dpContext) > 0) {
            result += fgp.fUsingKeys ? '&&' : 'if(';
            result += JavaScript_ContextMatch(fkp, fkp.dpContext);
          }
          else if (!fgp.fUsingKeys) {
            result += 'if(1';
          }

          linecomment = (fkp.Line > 0 && FDebug) ? `   // Line ${fkp.Line}` : '';

          result +=
            `) {${linecomment}${nl}` +
            FTabStop+FTabStop+FTabStop;
          if (fgp.fUsingKeys) {                                                                                      // I1959
            result += 'r=m=1;' + JavaScript_OutputString(FTabStop + FTabStop + FTabStop, fkp, fkp.dpOutput, fgp);    // I1959   // I3681
          }
          else {
            result += 'm=1;' + JavaScript_OutputString(FTabStop + FTabStop + FTabStop, fkp, fkp.dpOutput, fgp);    // I1959   // I3681
          }
          result += `${nl}${FTabStop}${FTabStop}}${nl}`;   // I3681
        }
      }
    }

		if Assigned(fgp.dpMatch) then
      Result := Result + Format(
        '%sif(m==1) {%s'+
        '%s%s%s'+
        '%s}%s',
        [FTabstop+FTabstop, nl,
        FTabstop+Ftabstop, JavaScript_OutputString(FTabStop + FTabStop + FTabStop, nil, fgp.dpMatch, fgp), nl,
        FTabstop+FTabstop, nl]);   // I3681
		if Assigned(fgp.dpNoMatch) then
      if fgp.fUsingKeys then    // I1382 - fixup m=1 to m=g()
        Result := Result + Format(
          '%sif(!m&&k.KIK(e)) {%s'+
          '%sr=1;%s%s'+
          '%s}%s',
          [FTabstop+FTabstop, nl,
          FTabstop+FTabstop+FTabstop, JavaScript_OutputString(FTabStop + FTabStop + FTabStop, nil, fgp.dpNoMatch, fgp), nl,
          FTabstop+FTabstop, nl])   // I1959. part 2, I2224   // I3681
      else
        Result := Result + Format(
          '%sif(!m) {%s'+
          '%s%s%s'+
          '%s}%s',
          [FTabstop+FTabstop, nl,
          FTabstop+FTabstop, JavaScript_OutputString(FTabStop + FTabStop + FTabStop, nil, fgp.dpNoMatch, fgp), nl,
          FTabstop+FTabstop, nl]);  // I1959   // I3681

    Result := Result + Format('%sreturn r;%s'+
                              '%s};%s',
                              [FTabstop+FTabstop, nl,
                              FTabstop, nl]); // I1959   // I3681
    Inc(fgp);
  end;

  for n := 0 to FCallFunctions.Count - 1 do
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

  Result := Result + sEmbedJS + '}' + nl;   // I3681
end;
}

///
/// Determine the modifiers used in the target keyboard and return a bitmask
/// representing them, or an integer value when not in debug mode
///
/// @return string of JavaScript code, e.g. 'modCodes.SHIFT | modCodes.CTRL /* 0x0030 */'
///
function GetKeyboardModifierBitmask(source: KMXFile): string {
  let bitMask = 0;
  for(let gp of source.keyboard.groups) {
    if(gp.fUsingKeys) {
      for(let kp of gp.keys) {
        if(!RuleIsExcludedByPlatform(kp)) {
          bitMask |= JavaScript_Shift(kp, fMnemonic);
        }
      }
    }
  }

  if ((bitMask & KMXFile.KMX_MASK_MODIFIER_CHIRAL) && (bitMask & KMXFile.KMX_MASK_MODIFIER_NONCHIRAL)) {
    ReportError(0, CWARN_DontMixChiralAndNonChiralModifiers, 'This keyboard contains Ctrl,Alt and LCtrl,LAlt,RCtrl,RAlt sets of modifiers. Use only one or the other set for web target.');
  }

  if(FDebug) {
    return FormatModifierAsBitflags(bitMask & KMXFile.KMX_MASK_KEYS); // Exclude KMX_ISVIRTUALKEY, KMX_VIRTUALCHARKEY
  }

  return '0x'+(bitMask & KMXFile.KMX_MASK_KEYS).toString(16);
}


///
/// If debug mode, then returns Javascript code necessary for
/// accessing constants in the compiled keyboard
///
/// @return string of JavaScript code
///
function JavaScript_SetupDebug() {
  if(IsKeyboardVersion10OrLater()) {
    if(FDebug) {
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
  I: Integer;
  fsp: PFILE_STORE;
  fgp: PFILE_GROUP;
  j: Integer;
  fkp: PFILE_KEY;
begin
  fsp := fk.dpStoreArray;
  for i := 0 to Integer(fk.cxStoreArray) - 1 do
  begin
    if StringHasSuppChars(fsp.dpString) then
      Exit(True);
    Inc(fsp);
  end;

  fgp := fk.dpGroupArray;
  for i := 0 to Integer(fk.cxGroupArray) - 1 do
  begin
    fkp := fgp.dpKeyArray;
    for j := 0 to Integer(fgp.cxKeyArray) - 1 do
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