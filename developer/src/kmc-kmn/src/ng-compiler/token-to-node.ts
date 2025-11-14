/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-04
 *
 * KMC KMN Next Generation Compiler
 *
 * TokenType to NodeTypes mapping for TokenRule
 */

import { NodeType } from "./node-type.js";
import { TokenType } from "./token-type.js";

export const TOKEN_TO_NODE = [
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ALWAYS,              nodeType: NodeType.ALWAYS},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.ANSI,                nodeType: NodeType.ANSI},
  // https://help.keyman.com/developer/language/reference/any
  {tokenType: TokenType.ANY,                 nodeType: NodeType.ANY},
  // https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenType.BASELAYOUT,          nodeType: NodeType.BASELAYOUT},
  // https://help.keyman.com/developer/language/reference/baselayout
  {tokenType: TokenType.BASELAYOUT_SHORTCUT, nodeType: NodeType.BASELAYOUT_SHORTCUT},
  // https://help.keyman.com/developer/language/reference/beep
  {tokenType: TokenType.BEEP,                nodeType: NodeType.BEEP},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.BEGIN,               nodeType: NodeType.BEGIN},
  // https://help.keyman.com/developer/language/reference/bitmap
  {tokenType: TokenType.BITMAP,              nodeType: NodeType.BITMAP},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.BITMAP_HEADER,       nodeType: NodeType.BITMAP_HEADER},
  // https://help.keyman.com/developer/language/reference/call
  {tokenType: TokenType.CALL,                nodeType: NodeType.CALL},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.CAPS,                nodeType: NodeType.CAPS},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.CAPSALWAYSOFF,       nodeType: NodeType.CAPSALWAYSOFF},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.CAPSONONLY,          nodeType: NodeType.CAPSONONLY},
  // https://help.keyman.com/developer/language/reference/casedkeys
  {tokenType: TokenType.CASEDKEYS,           nodeType: NodeType.CASEDKEYS},
  // https://help.keyman.com/developer/language/reference/context
  {tokenType: TokenType.CONTEXT,             nodeType: NodeType.CONTEXT},
  // https://help.keyman.com/developer/language/reference/copyright
  {tokenType: TokenType.COPYRIGHT,           nodeType: NodeType.COPYRIGHT},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.COPYRIGHT_HEADER,    nodeType: NodeType.COPYRIGHT_HEADER},
  // https://help.keyman.com/developer/language/reference/deadkey
  {tokenType: TokenType.DEADKEY,             nodeType: NodeType.DEADKEY},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.DECIMAL,             nodeType: NodeType.DECIMAL},
  // https://help.keyman.com/developer/language/reference/displaymap
  {tokenType: TokenType.DISPLAYMAP,          nodeType: NodeType.DISPLAYMAP},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.EQUAL,               nodeType: NodeType.EQUAL},
  // https://help.keyman.com/developer/language/reference/ethnologuecode
  {tokenType: TokenType.ETHNOLOGUECODE,      nodeType: NodeType.ETHNOLOGUECODE},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.FREES,               nodeType: NodeType.FREES},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.GROUP,               nodeType: NodeType.GROUP},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenType.HANGUL,              nodeType: NodeType.HANGUL},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.HEXADECIMAL,         nodeType: NodeType.HEXADECIMAL},
  // https://help.keyman.com/developer/language/reference/hotkey
  {tokenType: TokenType.HOTKEY,              nodeType: NodeType.HOTKEY},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.HOTKEY_HEADER,       nodeType: NodeType.HOTKEY_HEADER},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.IF,                  nodeType: NodeType.IF},
  // https://help.keyman.com/developer/language/reference/includecodes
  {tokenType: TokenType.INCLUDECODES,        nodeType: NodeType.INCLUDECODES},
  // https://help.keyman.com/developer/language/reference/_index
  {tokenType: TokenType.INDEX,               nodeType: NodeType.INDEX},
  // https://help.keyman.com/developer/language/reference/keyboardversion
  {tokenType: TokenType.KEYBOARDVERSION,     nodeType: NodeType.KEYBOARDVERSION},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMAN,              nodeType: NodeType.KEYMAN},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMANONLY,          nodeType: NodeType.KEYMANONLY},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KEYMANWEB,           nodeType: NodeType.KEYMANWEB},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.KEYS,                nodeType: NodeType.KEYS},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.KEY_CODE,            nodeType: NodeType.KEY_CODE},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.KMFL,                nodeType: NodeType.KMFL},
  // https://help.keyman.com/developer/language/reference/kmw_embedcss
  {tokenType: TokenType.KMW_EMBEDCSS,        nodeType: NodeType.KMW_EMBEDCSS},
  // https://help.keyman.com/developer/language/reference/kmw_embedjs
  {tokenType: TokenType.KMW_EMBEDJS,         nodeType: NodeType.KMW_EMBEDJS},
  // https://help.keyman.com/developer/language/reference/kmw_helpfile
  {tokenType: TokenType.KMW_HELPFILE,        nodeType: NodeType.KMW_HELPFILE},
  // https://help.keyman.com/developer/language/reference/kmw_helptext
  {tokenType: TokenType.KMW_HELPTEXT,        nodeType: NodeType.KMW_HELPTEXT},
  // https://help.keyman.com/developer/language/reference/kmw_rtl
  {tokenType: TokenType.KMW_RTL,             nodeType: NodeType.KMW_RTL},
  // https://help.keyman.com/developer/language/reference/language
  {tokenType: TokenType.LANGUAGE,            nodeType: NodeType.LANGUAGE},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.LANGUAGE_HEADER,     nodeType: NodeType.LANGUAGE_HEADER},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenType.LAYER,               nodeType: NodeType.LAYER},
  // https://help.keyman.com/developer/language/reference/layer
  {tokenType: TokenType.LAYER_SHORTCUT,      nodeType: NodeType.LAYER_SHORTCUT},
  // https://help.keyman.com/developer/language/reference/layoutfile
  {tokenType: TokenType.LAYOUTFILE,          nodeType: NodeType.LAYOUTFILE},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.LAYOUT_HEADER,       nodeType: NodeType.LAYOUT_HEADER},
  // https://help.keyman.com/developer/language/reference/match
  {tokenType: TokenType.MATCH,               nodeType: NodeType.MATCH},
  // https://help.keyman.com/developer/language/reference/message
  {tokenType: TokenType.MESSAGE,             nodeType: NodeType.MESSAGE},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.MESSAGE_HEADER,      nodeType: NodeType.MESSAGE_HEADER},
  // https://help.keyman.com/developer/language/reference/mnemoniclayout
  {tokenType: TokenType.MNEMONICLAYOUT,      nodeType: NodeType.MNEMONICLAYOUT},
  // https://help.keyman.com/developer/language/guide/virtual-keys
  {tokenType: TokenType.MODIFIER,            nodeType: NodeType.MODIFIER},
  // https://help.keyman.com/developer/language/reference/name
  {tokenType: TokenType.NAME,                nodeType: NodeType.NAME},
  // https://help.keyman.com/developer/language/guide/constants
  {tokenType: TokenType.NAMED_CONSTANT,      nodeType: NodeType.NAMED_CONSTANT},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.NAME_HEADER,         nodeType: NodeType.NAME_HEADER},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.NEWCONTEXT,          nodeType: NodeType.NEWCONTEXT},
  // https://help.keyman.com/developer/language/reference/newlayer
  {tokenType: TokenType.NEWLAYER,            nodeType: NodeType.NEWLAYER},
  {tokenType: TokenType.NEWLINE,             nodeType: NodeType.LINE},
  // https://help.keyman.com/developer/language/reference/nomatch
  {tokenType: TokenType.NOMATCH,             nodeType: NodeType.NOMATCH},
  // https://help.keyman.com/developer/language/reference/notany
  {tokenType: TokenType.NOTANY,              nodeType: NodeType.NOTANY},
  // https://help.keyman.com/developer/language/reference/if
  {tokenType: TokenType.NOT_EQUAL,           nodeType: NodeType.NOT_EQUAL},
  // https://help.keyman.com/developer/language/reference/_nul
  {tokenType: TokenType.NUL,                 nodeType: NodeType.NUL},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.OCTAL,               nodeType: NodeType.OCTAL},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.OFF,                 nodeType: NodeType.OFF},
  // https://help.keyman.com/developer/language/reference/oldlayer
  {tokenType: TokenType.OLDLAYER,            nodeType: NodeType.OLDLAYER},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ON,                  nodeType: NodeType.ON},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.ONLY,                nodeType: NodeType.ONLY},
  // https://help.keyman.com/developer/language/reference/outs
  {tokenType: TokenType.OUTS,                nodeType: NodeType.OUTS},
  {tokenType: TokenType.PARAMETER,           nodeType: NodeType.PARAMETER},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenType.PLATFORM,            nodeType: NodeType.PLATFORM},
  // https://help.keyman.com/developer/language/reference/platform
  {tokenType: TokenType.PLATFORM_SHORTCUT,   nodeType: NodeType.PLATFORM_SHORTCUT},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.POSTKEYSTROKE,       nodeType: NodeType.POSTKEYSTROKE},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.READONLY,            nodeType: NodeType.READONLY},
  // https://help.keyman.com/developer/language/reference/reset
  {tokenType: TokenType.RESET,               nodeType: NodeType.RESET},
  // https://help.keyman.com/developer/language/reference/return
  {tokenType: TokenType.RETURN,              nodeType: NodeType.RETURN},
  // https://help.keyman.com/developer/language/reference/save
  {tokenType: TokenType.SAVE,                nodeType: NodeType.SAVE},
  // https://help.keyman.com/developer/language/reference/set
  {tokenType: TokenType.SET,                 nodeType: NodeType.SET},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.SHIFT,               nodeType: NodeType.SHIFT},
  // https://help.keyman.com/developer/language/reference/caps
  {tokenType: TokenType.SHIFTFREESCAPS,      nodeType: NodeType.SHIFTFREESCAPS},
  // https://help.keyman.com/developer/language/reference/store
  {tokenType: TokenType.STORE,               nodeType: NodeType.STORE},
  // https://help.keyman.com/developer/language/guide/strings
  {tokenType: TokenType.STRING,              nodeType: NodeType.STRING},
  // https://help.keyman.com/developer/language/reference/targets
  {tokenType: TokenType.TARGETS,             nodeType: NodeType.TARGETS},
  // https://help.keyman.com/developer/language/reference/begin
  {tokenType: TokenType.UNICODE,             nodeType: NodeType.UNICODE},
  // https://help.keyman.com/developer/language/reference/use
  {tokenType: TokenType.USE,                 nodeType: NodeType.USE},
  // https://help.keyman.com/developer/language/reference/group
  {tokenType: TokenType.USING,               nodeType: NodeType.USING},
  // https://help.keyman.com/developer/language/guide/unicode
  {tokenType: TokenType.U_CHAR,              nodeType: NodeType.U_CHAR},
  // https://help.keyman.com/developer/language/reference/version
  {tokenType: TokenType.VERSION,             nodeType: NodeType.VERSION},
  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  {tokenType: TokenType.VERSION_HEADER,      nodeType: NodeType.VERSION_HEADER},
  // https://help.keyman.com/developer/language/reference/visualkeyboard
  {tokenType: TokenType.VISUALKEYBOARD,      nodeType: NodeType.VISUALKEYBOARD},
  // https://help.keyman.com/developer/language/guide/compile-targets
  {tokenType: TokenType.WEAVER,              nodeType: NodeType.WEAVER},
  // https://help.keyman.com/developer/language/reference/windowslanguages
  {tokenType: TokenType.WINDOWSLANGUAGES,    nodeType: NodeType.WINDOWSLANGUAGES},
];
