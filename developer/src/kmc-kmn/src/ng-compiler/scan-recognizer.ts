/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-05
 *
 * KMC KMN Next Generation Lexer
 *
 * ScanRecognizer for the Lexer
 */

import { TokenTypes } from "./token-types.js";

/**
 * A ScanRecognizer identifies an individual Token as part of the Next Generation Lexer.
 */
export class ScanRecognizer {
  private _tokenType: TokenTypes;
  private _regExp: RegExp;
  private _emit: boolean;

  /**
   * Construct a ScanRecognizer
   *
   * @param tokenType the token type to return if matched
   * @param regExp    the regex to identify the token
   * @param emit      whether to emit the token or not?
   */
  public constructor(tokenType: TokenTypes, regExp: RegExp, emit: boolean) {
    this._tokenType = tokenType;
    this._regExp    = new RegExp(regExp);  // does not preserve lastIndex
    this._emit      = emit;
  }

  public get tokenType(): TokenTypes { return this._tokenType; }
  public get regExp(): RegExp { return this._regExp; }
  public get emit(): boolean { return this._emit; }

  public toString(): string {
    return `[${this._tokenType},${this._regExp},${this._emit}]`;
  }
}

// the ordering of ScanRecognizers is important, with more specific regexs appearing first

export const KMN_SCAN_RECOGNIZERS = [
  // https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenTypes.BASELAYOUT,          regExp: /&baselayout(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/bitmap
  {tokenType: TokenTypes.BITMAP,              regExp: /&bitmap(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/casedkeys
  {tokenType: TokenTypes.CASEDKEYS,           regExp: /&casedkeys(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/copyright
  {tokenType: TokenTypes.COPYRIGHT,           regExp: /&copyright(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/displaymap
  {tokenType: TokenTypes.DISPLAYMAP,          regExp: /&displaymap(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/ethnologuecode
  {tokenType: TokenTypes.ETHNOLOGUECODE,      regExp: /&ethnologuecode(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/hotkey
  {tokenType: TokenTypes.HOTKEY,              regExp: /&hotkey(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/includecodes
  {tokenType: TokenTypes.INCLUDECODES,        regExp: /&includecodes(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/keyboardversion
  {tokenType: TokenTypes.KEYBOARDVERSION,     regExp: /&keyboardversion(?![a-z0-9_\.-])/iy,                    emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_embedcss
  {tokenType: TokenTypes.KMW_EMBEDCSS,        regExp: /&kmw_embedcss(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_embedjs
  {tokenType: TokenTypes.KMW_EMBEDJS,         regExp: /&kmw_embedjs(?![a-z0-9_\.-])/iy,                        emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_helpfile
  {tokenType: TokenTypes.KMW_HELPFILE,        regExp: /&kmw_helpfile(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_helptext
  {tokenType: TokenTypes.KMW_HELPTEXT,        regExp: /&kmw_helptext(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_rtl
  {tokenType: TokenTypes.KMW_RTL,             regExp: /&kmw_rtl(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/language
  {tokenType: TokenTypes.LANGUAGE,            regExp: /&language(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenTypes.LAYER,               regExp: /&layer(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/layoutfile
  {tokenType: TokenTypes.LAYOUTFILE,          regExp: /&layoutfile(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/message
  {tokenType: TokenTypes.MESSAGE,             regExp: /&message(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/mnemoniclayout
  {tokenType: TokenTypes.MNEMONICLAYOUT,      regExp: /&mnemoniclayout(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/name
  {tokenType: TokenTypes.NAME,                regExp: /&name(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/newlayer
  {tokenType: TokenTypes.NEWLAYER,            regExp: /&newlayer(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/oldcharposmatching
  {tokenType: TokenTypes.OLDCHARPOSMATCHING,  regExp: /&oldcharposmatching(?![a-z0-9_\.-])/iy,                 emit: true},
  // https://help.keyman.com/developer/language/reference/oldlayer
  {tokenType: TokenTypes.OLDLAYER,            regExp: /&oldlayer(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenTypes.PLATFORM,            regExp: /&platform(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/targets
  {tokenType: TokenTypes.TARGETS,             regExp: /&targets(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/version
  {tokenType: TokenTypes.VERSION,             regExp: /&version(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/visualkeyboard
  {tokenType: TokenTypes.VISUALKEYBOARD,      regExp: /&visualkeyboard(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/windowslanguages
  {tokenType: TokenTypes.WINDOWSLANGUAGES,    regExp: /&windowslanguages(?![a-z0-9_\.-])/iy,                   emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenTypes.CAPSALWAYSOFF,       regExp: /&capsalwaysoff(?![a-z0-9_\.-])/iy,                      emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenTypes.CAPSONONLY,          regExp: /&capsononly(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenTypes.SHIFTFREESCAPS,      regExp: /&shiftfreescaps(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.CAPS,                regExp: /caps(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.ALWAYS,              regExp: /always(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.OFF,                 regExp: /off(?![a-z0-9_\.-])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.ON,                  regExp: /on(?![a-z0-9_\.-])/iy,                                  emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.ONLY,                regExp: /only(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.SHIFT,               regExp: /shift(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.FREES,               regExp: /frees(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.BITMAP_HEADER,       regExp: /bitmap(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.COPYRIGHT_HEADER,    regExp: /copyright(?=[^\S\r\n])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.HOTKEY_HEADER,       regExp: /hotkey(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.LANGUAGE_HEADER,     regExp: /language(?=[^\S\r\n])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.LAYOUT_HEADER,       regExp: /layout(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.MESSAGE_HEADER,      regExp: /message(?=[^\S\r\n])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.NAME_HEADER,         regExp: /name(?=[^\S\r\n])/iy,                                   emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenTypes.VERSION_HEADER,      regExp: /version(?=[^\S\r\n])/iy,                                emit: true},
  //https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenTypes.BASELAYOUT_SHORTCUT, regExp: /baselayout(?=[^\S\r\n]*\()/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenTypes.LAYER_SHORTCUT,      regExp: /layer(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenTypes.PLATFORM_SHORTCUT,   regExp: /platform(?=[^\S\r\n]*\()/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/any
  {tokenType: TokenTypes.ANY,                 regExp: /any(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/beep
  {tokenType: TokenTypes.BEEP,                regExp: /beep(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenTypes.BEGIN,               regExp: /begin(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/call
  {tokenType: TokenTypes.CALL,                regExp: /call(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/context
  {tokenType: TokenTypes.CONTEXT,             regExp: /context(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/deadkey
  {tokenType: TokenTypes.DEADKEY,             regExp: /(deadkey|dk)(?=[^\S\r\n]*\()/iy,                        emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenTypes.GROUP,               regExp: /group(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenTypes.IF,                  regExp: /if(?=[^\S\r\n]*\()/iy,                                  emit: true},
  // https://help.keyman.com/developer/language/reference/_index
  {tokenType: TokenTypes.INDEX,               regExp: /index(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/match
  {tokenType: TokenTypes.MATCH,               regExp: /match(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/nomatch
  {tokenType: TokenTypes.NOMATCH,             regExp: /nomatch(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/notany
  {tokenType: TokenTypes.NOTANY,              regExp: /notany(?=[^\S\r\n]*\()/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_nul
  {tokenType: TokenTypes.NUL,                 regExp: /nul(?![a-z0-9_\.-])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/outs
  {tokenType: TokenTypes.OUTS,                regExp: /outs(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/reset
  {tokenType: TokenTypes.RESET,               regExp: /reset(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/return
  {tokenType: TokenTypes.RETURN,              regExp: /return(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/save
  {tokenType: TokenTypes.SAVE,                regExp: /save(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/set
  {tokenType: TokenTypes.SET,                 regExp: /set(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/store
  {tokenType: TokenTypes.STORE,               regExp: /store(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/use
  {tokenType: TokenTypes.USE,                 regExp: /use(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenTypes.UNICODE,             regExp: /unicode(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenTypes.NEWCONTEXT,          regExp: /newcontext(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenTypes.POSTKEYSTROKE,       regExp: /postkeystroke(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenTypes.ANSI,                regExp: /ansi(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenTypes.READONLY,            regExp: /readonly(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenTypes.USING,               regExp: /using(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenTypes.KEYS,                regExp: /keys(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenTypes.KEYMAN,              regExp: /\$keyman:/iy,                                           emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenTypes.KEYMANONLY,          regExp: /\$keymanonly:/iy,                                       emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenTypes.KEYMANWEB,           regExp: /\$keymanweb:/iy,                                        emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenTypes.KMFL,                regExp: /\$kmfl:/iy,                                             emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenTypes.WEAVER,              regExp: /\$weaver:/iy,                                           emit: true},
  {tokenType: TokenTypes.LEFT_BR,             regExp: /\(/y,                                                   emit: true},
  {tokenType: TokenTypes.RIGHT_BR,            regExp: /\)/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.LEFT_SQ,             regExp: /\[/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.RIGHT_SQ,            regExp: /\]/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.CHEVRON,             regExp: />/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.PLUS,                regExp: /\+/y,                                                   emit: true},
  {tokenType: TokenTypes.COMMA,               regExp: /,/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenTypes.NOT_EQUAL,           regExp: /!=/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenTypes.EQUAL,               regExp: /=/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/guide/expansions
  {tokenType: TokenTypes.RANGE,               regExp: /\.\./y,                                                 emit: true},
  // https://help.keyman.com/developer/language/guide/unicode
  {tokenType: TokenTypes.U_CHAR,              regExp: /U\+[0-9A-F]{1,6}/iy,                                    emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenTypes.STRING,              regExp: /('[^'\r\n]*?'|"[^"\r\n]*?")/y,                          emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenTypes.DECIMAL,             regExp: /d\d+(?=[\s,\)\]])/iy,                                   emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenTypes.HEXADECIMAL,         regExp: /x[a-f\d]+(?=[\s,\)\]])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenTypes.OCTAL,               regExp: /[0-7]+(?=[\s,\)\]])/y,                                  emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.MODIFIER,            regExp: /(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\S\r\n])/iy, emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenTypes.KEY_CODE,            regExp: /(((K_|T_|U_)[^\]\s]+)|[A-E]\d\d)(?=[^\S\r\n]*\])/iy,    emit: true},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenTypes.HANGUL,              regExp: /\$HANGUL_SYLLABLE_[A-Z]{1,7}/iy,                        emit: true},
  // https://help.keyman.com/developer/language/guide/comments
  {tokenType: TokenTypes.COMMENT,             regExp: /c(([^\S\r\n][^\r\n]*)|(?=(\r\n|\n|\r)))/iy,             emit: false},
  {tokenType: TokenTypes.WHITESPACE,          regExp: /[^\S\r\n]+/y,                                           emit: false},
  // https://help.keyman.com/developer/language/guide/long-lines
  {tokenType: TokenTypes.CONTINUATION,        regExp: /\\(?=([^\S\r\n]*(\r\n|\n|\r)))/y,                       emit: false},
  {tokenType: TokenTypes.NEWLINE,             regExp: /(\r\n|\n|\r)/y,                                         emit: true},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenTypes.NAMED_CONSTANT,      regExp: /\$\S+/y,                                                emit: true},
  {tokenType: TokenTypes.PARAMETER,           regExp: /[^=,\)\s]+/y,                                           emit: true},
];
