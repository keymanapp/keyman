/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-05
 *
 * KMC KMN Next Generation Lexer
 *
 * ScanRecognizer for the Lexer
 */

import { TokenType } from "./token-type.js";

/**
 * A ScanRecognizer identifies an individual Token as part of the Next Generation Lexer.
 */
export class ScanRecognizer {
  /**
   * Construct a ScanRecognizer
   */
  public constructor(
    /** the token type to return if matched */
    public readonly tokenType: TokenType,
    /** the regex to identify the token */
    public readonly regExp: RegExp,
    /**  whether to emit the token or not? */
    public readonly emit: boolean
  ) {
    this.regExp = new RegExp(regExp); // does not preserve lastIndex
  }

  public toString(): string {
    return `[${this.tokenType},${this.regExp},${this.emit}]`;
  }
}

// the ordering of ScanRecognizers is important, with more specific regexs appearing first

export const KMN_SCAN_RECOGNIZERS = [
  // https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenType.BASELAYOUT,          regExp: /&baselayout(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/bitmap
  {tokenType: TokenType.BITMAP,              regExp: /&bitmap(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/casedkeys
  {tokenType: TokenType.CASEDKEYS,           regExp: /&casedkeys(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/copyright
  {tokenType: TokenType.COPYRIGHT,           regExp: /&copyright(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/displaymap
  {tokenType: TokenType.DISPLAYMAP,          regExp: /&displaymap(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/ethnologuecode
  {tokenType: TokenType.ETHNOLOGUECODE,      regExp: /&ethnologuecode(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/hotkey
  {tokenType: TokenType.HOTKEY,              regExp: /&hotkey(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/includecodes
  {tokenType: TokenType.INCLUDECODES,        regExp: /&includecodes(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/keyboardversion
  {tokenType: TokenType.KEYBOARDVERSION,     regExp: /&keyboardversion(?![a-z0-9_\.-])/iy,                    emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_embedcss
  {tokenType: TokenType.KMW_EMBEDCSS,        regExp: /&kmw_embedcss(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_embedjs
  {tokenType: TokenType.KMW_EMBEDJS,         regExp: /&kmw_embedjs(?![a-z0-9_\.-])/iy,                        emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_helpfile
  {tokenType: TokenType.KMW_HELPFILE,        regExp: /&kmw_helpfile(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_helptext
  {tokenType: TokenType.KMW_HELPTEXT,        regExp: /&kmw_helptext(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/kmw_rtl
  {tokenType: TokenType.KMW_RTL,             regExp: /&kmw_rtl(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/language
  {tokenType: TokenType.LANGUAGE,            regExp: /&language(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenType.LAYER,               regExp: /&layer(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/layoutfile
  {tokenType: TokenType.LAYOUTFILE,          regExp: /&layoutfile(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/message
  {tokenType: TokenType.MESSAGE,             regExp: /&message(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/mnemoniclayout
  {tokenType: TokenType.MNEMONICLAYOUT,      regExp: /&mnemoniclayout(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/name
  {tokenType: TokenType.NAME,                regExp: /&name(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/newlayer
  {tokenType: TokenType.NEWLAYER,            regExp: /&newlayer(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/oldcharposmatching
  {tokenType: TokenType.OLDCHARPOSMATCHING,  regExp: /&oldcharposmatching(?![a-z0-9_\.-])/iy,                 emit: true},
  // https://help.keyman.com/developer/language/reference/oldlayer
  {tokenType: TokenType.OLDLAYER,            regExp: /&oldlayer(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenType.PLATFORM,            regExp: /&platform(?![a-z0-9_\.-])/iy,                           emit: true},
  // https://help.keyman.com/developer/language/reference/targets
  {tokenType: TokenType.TARGETS,             regExp: /&targets(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/version
  {tokenType: TokenType.VERSION,             regExp: /&version(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/visualkeyboard
  {tokenType: TokenType.VISUALKEYBOARD,      regExp: /&visualkeyboard(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/windowslanguages
  {tokenType: TokenType.WINDOWSLANGUAGES,    regExp: /&windowslanguages(?![a-z0-9_\.-])/iy,                   emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.CAPSALWAYSOFF,       regExp: /&capsalwaysoff(?![a-z0-9_\.-])/iy,                      emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.CAPSONONLY,          regExp: /&capsononly(?![a-z0-9_\.-])/iy,                         emit: true},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.SHIFTFREESCAPS,      regExp: /&shiftfreescaps(?![a-z0-9_\.-])/iy,                     emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.CAPS,                regExp: /caps(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ALWAYS,              regExp: /always(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.OFF,                 regExp: /off(?![a-z0-9_\.-])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ON,                  regExp: /on(?![a-z0-9_\.-])/iy,                                  emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ONLY,                regExp: /only(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.SHIFT,               regExp: /shift(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.FREES,               regExp: /frees(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.BITMAP_HEADER,       regExp: /bitmap(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.COPYRIGHT_HEADER,    regExp: /copyright(?=[^\S\r\n])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.HOTKEY_HEADER,       regExp: /hotkey(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.LANGUAGE_HEADER,     regExp: /language(?=[^\S\r\n])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.LAYOUT_HEADER,       regExp: /layout(?=[^\S\r\n])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.MESSAGE_HEADER,      regExp: /message(?=[^\S\r\n])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.NAME_HEADER,         regExp: /name(?=[^\S\r\n])/iy,                                   emit: true},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.VERSION_HEADER,      regExp: /version(?=[^\S\r\n])/iy,                                emit: true},
  //https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenType.BASELAYOUT_SHORTCUT, regExp: /baselayout(?=[^\S\r\n]*\()/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenType.LAYER_SHORTCUT,      regExp: /layer(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenType.PLATFORM_SHORTCUT,   regExp: /platform(?=[^\S\r\n]*\()/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/any
  {tokenType: TokenType.ANY,                 regExp: /any(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/beep
  {tokenType: TokenType.BEEP,                regExp: /beep(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.BEGIN,               regExp: /begin(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/call
  {tokenType: TokenType.CALL,                regExp: /call(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/context
  {tokenType: TokenType.CONTEXT,             regExp: /context(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/deadkey
  {tokenType: TokenType.DEADKEY,             regExp: /(deadkey|dk)(?=[^\S\r\n]*\()/iy,                        emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.GROUP,               regExp: /group(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.IF,                  regExp: /if(?=[^\S\r\n]*\()/iy,                                  emit: true},
  // https://help.keyman.com/developer/language/reference/_index
  {tokenType: TokenType.INDEX,               regExp: /index(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/match
  {tokenType: TokenType.MATCH,               regExp: /match(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/nomatch
  {tokenType: TokenType.NOMATCH,             regExp: /nomatch(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/notany
  {tokenType: TokenType.NOTANY,              regExp: /notany(?=[^\S\r\n]*\()/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/_nul
  {tokenType: TokenType.NUL,                 regExp: /nul(?![a-z0-9_\.-])/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/outs
  {tokenType: TokenType.OUTS,                regExp: /outs(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/reset
  {tokenType: TokenType.RESET,               regExp: /reset(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/return
  {tokenType: TokenType.RETURN,              regExp: /return(?![a-z0-9_\.-])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/reference/save
  {tokenType: TokenType.SAVE,                regExp: /save(?=[^\S\r\n]*\()/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/set
  {tokenType: TokenType.SET,                 regExp: /set(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/store
  {tokenType: TokenType.STORE,               regExp: /store(?=[^\S\r\n]*\()/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/use
  {tokenType: TokenType.USE,                 regExp: /use(?=[^\S\r\n]*\()/iy,                                 emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.UNICODE,             regExp: /unicode(?![a-z0-9_\.-])/iy,                             emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.NEWCONTEXT,          regExp: /newcontext(?![a-z0-9_\.-])/iy,                          emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.POSTKEYSTROKE,       regExp: /postkeystroke(?![a-z0-9_\.-])/iy,                       emit: true},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.ANSI,                regExp: /ansi(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.READONLY,            regExp: /readonly(?![a-z0-9_\.-])/iy,                            emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.USING,               regExp: /using(?![a-z0-9_\.-])/iy,                               emit: true},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.KEYS,                regExp: /keys(?![a-z0-9_\.-])/iy,                                emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMAN,              regExp: /\$keyman:/iy,                                           emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMANONLY,          regExp: /\$keymanonly:/iy,                                       emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMANWEB,           regExp: /\$keymanweb:/iy,                                        emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KMFL,                regExp: /\$kmfl:/iy,                                             emit: true},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.WEAVER,              regExp: /\$weaver:/iy,                                           emit: true},
  {tokenType: TokenType.LEFT_BR,             regExp: /\(/y,                                                   emit: true},
  {tokenType: TokenType.RIGHT_BR,            regExp: /\)/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.LEFT_SQ,             regExp: /\[/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.RIGHT_SQ,            regExp: /\]/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.CHEVRON,             regExp: />/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.PLUS,                regExp: /\+/y,                                                   emit: true},
  {tokenType: TokenType.COMMA,               regExp: /,/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.NOT_EQUAL,           regExp: /!=/y,                                                   emit: true},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.EQUAL,               regExp: /=/y,                                                    emit: true},
  // https://help.keyman.com/developer/language/guide/expansions
  {tokenType: TokenType.RANGE,               regExp: /\.\./y,                                                 emit: true},
  // https://help.keyman.com/developer/language/guide/unicode
  {tokenType: TokenType.U_CHAR,              regExp: /U\+[0-9A-F]{1,6}/iy,                                    emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.STRING,              regExp: /('[^'\r\n]*?'|"[^"\r\n]*?")/y,                          emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.DECIMAL,             regExp: /d\d+(?=[\s,\)\]])/iy,                                   emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.HEXADECIMAL,         regExp: /x[a-f\d]+(?=[\s,\)\]])/iy,                              emit: true},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.OCTAL,               regExp: /[0-7]+(?=[\s,\)\]])/y,                                  emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.MODIFIER,            regExp: /(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\S\r\n])/iy, emit: true},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.KEY_CODE,            regExp: /(((K_|T_|U_)[^\]\s]+)|[A-E]\d\d)(?=[^\S\r\n]*\])/iy,    emit: true},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenType.HANGUL,              regExp: /\$HANGUL_SYLLABLE_[A-Z]{1,7}/iy,                        emit: true},
  // https://help.keyman.com/developer/language/guide/comments
  {tokenType: TokenType.COMMENT,             regExp: /c(([^\S\r\n][^\r\n]*)|(?=(\r\n|\n|\r)))/iy,             emit: false},
  {tokenType: TokenType.WHITESPACE,          regExp: /[^\S\r\n]+/y,                                           emit: false},
  // https://help.keyman.com/developer/language/guide/long-lines
  {tokenType: TokenType.CONTINUATION,        regExp: /\\(?=([^\S\r\n]*(\r\n|\n|\r)))/y,                       emit: false},
  {tokenType: TokenType.NEWLINE,             regExp: /(\r\n|\n|\r)/y,                                         emit: true},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenType.NAMED_CONSTANT,      regExp: /\$\S+/y,                                                emit: true},
  {tokenType: TokenType.PARAMETER,           regExp: /[^=,\(\)\[\]\s]+/y,                                     emit: true},
];
