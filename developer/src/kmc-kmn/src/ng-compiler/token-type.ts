/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-04
 *
 * KMC KMN Next Generation Lexer
 *
 * TokenTypes for the Lexer
 */

/* These have been placed in alphabetical order for ease of access */

export enum TokenType {
  ALWAYS              = "ALWAYS",              // https://help.keyman.com/developer/language/reference/_keywordsbytype
  ANSI                = "ANSI",                // https://help.keyman.com/developer/language/reference/begin
  ANY                 = "ANY",                 // https://help.keyman.com/developer/language/reference/any
  BASELAYOUT          = "BASELAYOUT",          // https://help.keyman.com/developer/language/reference/baselayout
  BASELAYOUT_SHORTCUT = "BASELAYOUT_SHORTCUT", // https://help.keyman.com/developer/language/reference/baselayout
  BEEP                = "BEEP",                // https://help.keyman.com/developer/language/reference/beep
  BEGIN               = "BEGIN",               // https://help.keyman.com/developer/language/reference/begin
  BITMAP              = "BITMAP",              // https://help.keyman.com/developer/language/reference/bitmap
  BITMAP_HEADER       = "BITMAP_HEADER",       // https://help.keyman.com/developer/language/reference/_keywordsbytype
  CALL                = "CALL",                // https://help.keyman.com/developer/language/reference/call
  CAPS                = "CAPS",                // https://help.keyman.com/developer/language/reference/_keywordsbytype
  CAPSALWAYSOFF       = "CAPSALWAYSOFF",       // https://help.keyman.com/developer/language/reference/caps
  CAPSONONLY          = "CAPSONONLY",          // https://help.keyman.com/developer/language/reference/caps
  CASEDKEYS           = "CASEDKEYS",           // https://help.keyman.com/developer/language/reference/casedkeys
  CHEVRON             = "CHEVRON",             // https://help.keyman.com/developer/language/guide/virtual-keys
  COMMA               = "COMMA",
  COMMENT             = "COMMENT",             // https://help.keyman.com/developer/language/guide/comments
  CONTEXT             = "CONTEXT",             // https://help.keyman.com/developer/language/reference/context
  CONTINUATION        = "CONTINUATION",        // https://help.keyman.com/developer/language/guide/long-lines
  COPYRIGHT           = "COPYRIGHT",           // https://help.keyman.com/developer/language/reference/copyright
  COPYRIGHT_HEADER    = "COPYRIGHT_HEADER",    // https://help.keyman.com/developer/language/reference/_keywordsbytype
  DEADKEY             = "DEADKEY",             // https://help.keyman.com/developer/language/reference/deadkey
  DECIMAL             = "DECIMAL",             // https://help.keyman.com/developer/language/guide/strings
  DISPLAYMAP          = "DISPLAYMAP",          // https://help.keyman.com/developer/language/reference/displaymap
  EOF                 = "EOF",
  EQUAL               = "EQUAL",               // https://help.keyman.com/developer/language/reference/if
  ETHNOLOGUECODE      = "ETHNOLOGUECODE",      // https://help.keyman.com/developer/language/reference/ethnologuecode
  FREES               = "FREES",               // https://help.keyman.com/developer/language/reference/_keywordsbytype
  GROUP               = "GROUP",               // https://help.keyman.com/developer/language/reference/group
  HANGUL              = "HANGUL",              // https://help.keyman.com/developer/language/guide/constants
  HEXADECIMAL         = "HEXADECIMAL",         // https://help.keyman.com/developer/language/guide/strings
  HOTKEY              = "HOTKEY",              // https://help.keyman.com/developer/language/reference/hotkey
  HOTKEY_HEADER       = "HOTKEY_HEADER",       // https://help.keyman.com/developer/language/reference/_keywordsbytype
  IF                  = "IF",                  // https://help.keyman.com/developer/language/reference/if
  INCLUDECODES        = "INCLUDECODES",        // https://help.keyman.com/developer/language/reference/includecodes
  INDEX               = "INDEX",               // https://help.keyman.com/developer/language/reference/_index
  KEYBOARDVERSION     = "KEYBOARDVERSION",     // https://help.keyman.com/developer/language/reference/keyboardversion
  KEYMAN              = "KEYMAN",              // https://help.keyman.com/developer/language/guide/compile-targets
  KEYMANONLY          = "KEYMANONLY",          // https://help.keyman.com/developer/language/guide/compile-targets
  KEYMANWEB           = "KEYMANWEB",           // https://help.keyman.com/developer/language/guide/compile-targets
  KEYS                = "KEYS",                // https://help.keyman.com/developer/language/reference/group
  KEY_CODE            = "KEY_CODE",            // https://help.keyman.com/developer/language/guide/virtual-keys
  KMFL                = "KMFL",                // https://help.keyman.com/developer/language/guide/compile-targets
  KMW_EMBEDCSS        = "KMW_EMBEDCSS",        // https://help.keyman.com/developer/language/reference/kmw_embedcss
  KMW_EMBEDJS         = "KMW_EMBEDJS",         // https://help.keyman.com/developer/language/reference/kmw_embedjs
  KMW_HELPFILE        = "KMW_HELPFILE",        // https://help.keyman.com/developer/language/reference/kmw_helpfile
  KMW_HELPTEXT        = "KMW_HELPTEXT",        // https://help.keyman.com/developer/language/reference/kmw_helptext
  KMW_RTL             = "KMW_RTL",             // https://help.keyman.com/developer/language/reference/kmw_rtl
  LANGUAGE            = "LANGUAGE",            // https://help.keyman.com/developer/language/reference/language
  LANGUAGE_HEADER     = "LANGUAGE_HEADER",     // https://help.keyman.com/developer/language/reference/_keywordsbytype
  LAYER               = "LAYER",               // https://help.keyman.com/developer/language/reference/layer
  LAYER_SHORTCUT      = "LAYER_SHORTCUT",      // https://help.keyman.com/developer/language/reference/layer
  LAYOUTFILE          = "LAYOUTFILE",          // https://help.keyman.com/developer/language/reference/layoutfile
  LAYOUT_HEADER       = "LAYOUT_HEADER",       // https://help.keyman.com/developer/language/reference/_keywordsbytype
  LEFT_BR             = "LEFT_BR",
  LEFT_SQ             = "LEFT_SQ",             // https://help.keyman.com/developer/language/guide/virtual-keys
  MATCH               = "MATCH",               // https://help.keyman.com/developer/language/reference/match
  MESSAGE             = "MESSAGE",             // https://help.keyman.com/developer/language/reference/message
  MESSAGE_HEADER      = "MESSAGE_HEADER",      // https://help.keyman.com/developer/language/reference/_keywordsbytype
  MNEMONICLAYOUT      = "MNEMONICLAYOUT",      // https://help.keyman.com/developer/language/reference/mnemoniclayout
  MODIFIER            = "MODIFIER",            // https://help.keyman.com/developer/language/guide/virtual-keys
  NAME                = "NAME",                // https://help.keyman.com/developer/language/reference/name
  NAMED_CONSTANT      = "NAMED_CONSTANT",      // https://help.keyman.com/developer/language/guide/constants
  NAME_HEADER         = "NAME_HEADER",         // https://help.keyman.com/developer/language/reference/_keywordsbytype
  NEWCONTEXT          = "NEWCONTEXT",          // https://help.keyman.com/developer/language/reference/begin
  NEWLAYER            = "NEWLAYER",            // https://help.keyman.com/developer/language/reference/newlayer
  NEWLINE             = "NEWLINE",
  NOMATCH             = "NOMATCH",             // https://help.keyman.com/developer/language/reference/nomatch
  NOTANY              = "NOTANY",              // https://help.keyman.com/developer/language/reference/notany
  NOT_EQUAL           = "NOT_EQUAL",           // https://help.keyman.com/developer/language/reference/if
  NUL                 = "NUL",                 // https://help.keyman.com/developer/language/reference/_nul
  OCTAL               = "OCTAL",               // https://help.keyman.com/developer/language/guide/strings
  OFF                 = "OFF",                 // https://help.keyman.com/developer/language/reference/_keywordsbytype
  OLDCHARPOSMATCHING  = "OLDCHARPOSMATCHING",  // https://help.keyman.com/developer/language/reference/oldcharposmatching
  OLDLAYER            = "OLDLAYER",            // https://help.keyman.com/developer/language/reference/oldlayer
  ON                  = "ON",                  // https://help.keyman.com/developer/language/reference/_keywordsbytype
  ONLY                = "ONLY",                // https://help.keyman.com/developer/language/reference/_keywordsbytype
  OUTS                = "OUTS",                // https://help.keyman.com/developer/language/reference/outs
  PARAMETER           = "PARAMETER",
  PLATFORM            = "PLATFORM",            // https://help.keyman.com/developer/language/reference/platform
  PLATFORM_SHORTCUT   = "PLATFORM_SHORTCUT",   // https://help.keyman.com/developer/language/reference/platform
  PLUS                = "PLUS",                // https://help.keyman.com/developer/language/guide/virtual-keys
  POSTKEYSTROKE       = "POSTKEYSTROKE",       // https://help.keyman.com/developer/language/reference/begin
  RANGE               = "RANGE",               // https://help.keyman.com/developer/language/guide/expansions
  READONLY            = "READONLY",            // https://help.keyman.com/developer/language/reference/group
  RESET               = "RESET",               // https://help.keyman.com/developer/language/reference/reset
  RETURN              = "RETURN",              // https://help.keyman.com/developer/language/reference/return
  RIGHT_BR            = "RIGHT_BR",
  RIGHT_SQ            = "RIGHT_SQ",            // https://help.keyman.com/developer/language/guide/virtual-keys
  SAVE                = "SAVE",                // https://help.keyman.com/developer/language/reference/save
  SET                 = "SET",                 // https://help.keyman.com/developer/language/reference/set
  SHIFT               = "SHIFT",               // https://help.keyman.com/developer/language/reference/_keywordsbytype
  SHIFTFREESCAPS      = "SHIFTFREESCAPS",      // https://help.keyman.com/developer/language/reference/caps
  STORE               = "STORE",               // https://help.keyman.com/developer/language/reference/store
  STRING              = "STRING",              // https://help.keyman.com/developer/language/guide/strings
  TARGETS             = "TARGETS",             // https://help.keyman.com/developer/language/reference/targets
  UNICODE             = "UNICODE",             // https://help.keyman.com/developer/language/reference/begin
  USE                 = "USE",                 // https://help.keyman.com/developer/language/reference/use
  USING               = "USING",               // https://help.keyman.com/developer/language/reference/group
  U_CHAR              = "U_CHAR",              // https://help.keyman.com/developer/language/guide/unicode
  VERSION             = "VERSION",             // https://help.keyman.com/developer/language/reference/version
  VERSION_HEADER      = "VERSION_HEADER",      // https://help.keyman.com/developer/language/reference/_keywordsbytype
  VISUALKEYBOARD      = "VISUALKEYBOARD",      // https://help.keyman.com/developer/language/reference/visualkeyboard
  WEAVER              = "WEAVER",              // https://help.keyman.com/developer/language/guide/compile-targets
  WHITESPACE          = "WHITESPACE",
  WINDOWSLANGUAGES    = "WINDOWSLANGUAGES",    // https://help.keyman.com/developer/language/reference/windowslanguages
};
