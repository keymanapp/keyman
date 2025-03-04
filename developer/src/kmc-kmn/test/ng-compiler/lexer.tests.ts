/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { Lexer, ScanRecogniser, Token, TokenTypes } from '../../src/ng-compiler/lexer.js'

describe("Lexer Tests", () => {
  describe("ScanRecogniser", () => {
    it("can construct a ScanRecogniser", () => {
      const sr = new ScanRecogniser(TokenTypes.TT_STORE, new RegExp("^store"));
      assert.deepEqual(sr.toString(), '[TT_STORE,/^store/]');
    });
  });
  describe("Token", () => {
    it("can construct a Token", () => {
      const token = new Token(TokenTypes.TT_STORE, 'store');
      assert.deepEqual(token.toString(), '[TT_STORE,store]');
    });
  });
  describe("Lexer", () => {
    it("can recognise a TT_BITMAP token", () => {
      recogniseToken(TokenTypes.TT_BITMAP, 'bitmap');
    });
    it("can recognise a TT_CASEDKEYS token", () => {
      recogniseToken(TokenTypes.TT_CASEDKEYS, 'casedkeys');
    });
    it("can recognise a TT_COPYRIGHT token", () => {
      recogniseToken(TokenTypes.TT_COPYRIGHT, 'copyright');
    });
    it("can recognise a TT_DISPLAYMAP token", () => {
      recogniseToken(TokenTypes.TT_DISPLAYMAP, 'displaymap');
    });
    it("can recognise a TT_ETHNOLOGUECODE token", () => {
      recogniseToken(TokenTypes.TT_ETHNOLOGUECODE, 'ethnologuecode');
    });
    it("can recognise a TT_HOTKEY token", () => {
      recogniseToken(TokenTypes.TT_HOTKEY, 'hotkey');
    });
    it("can recognise a TT_INCLUDECODES token", () => {
      recogniseToken(TokenTypes.TT_INCLUDECODES, 'includecodes');
    });
    it("can recognise a TT_KEYBOARDVERSION token", () => {
      recogniseToken(TokenTypes.TT_KEYBOARDVERSION, 'keyboardversion');
    });
    it("can recognise a TT_KMW_EMBEDCSS token", () => {
      recogniseToken(TokenTypes.TT_KMW_EMBEDCSS, 'kmw_embedcss');
    });
    it("can recognise a TT_KMW_EMBEDJS token", () => {
      recogniseToken(TokenTypes.TT_KMW_EMBEDJS, 'kmw_embedjs');
    });
    it("can recognise a TT_KMW_HELPFILE token", () => {
      recogniseToken(TokenTypes.TT_KMW_HELPFILE, 'kmw_helpfile');
    });
    it("can recognise a TT_KMW_HELPTEXT token", () => {
      recogniseToken(TokenTypes.TT_KMW_HELPTEXT, 'kmw_helptext');
    });
    it("can recognise a TT_KMW_RTL token", () => {
      recogniseToken(TokenTypes.TT_KMW_RTL, 'kmw_rtl');
    });
    it("can recognise a TT_LANGUAGE token", () => {
      recogniseToken(TokenTypes.TT_LANGUAGE, 'language');
    });
    it("can recognise a TT_LAYER token", () => {
      recogniseToken(TokenTypes.TT_LAYER, 'layer');
    });
    it("can recognise a TT_LAYOUTFILE token", () => {
      recogniseToken(TokenTypes.TT_LAYOUTFILE, 'layoutfile');
    });
    it("can recognise a TT_MESSAGE token", () => {
      recogniseToken(TokenTypes.TT_MESSAGE, 'message');
    });
    it("can recognise a TT_MNEMONICLAYOUT token", () => {
      recogniseToken(TokenTypes.TT_MNEMONICLAYOUT, 'mnemoniclayout');
    });
    it("can recognise a TT_NAME token", () => {
      recogniseToken(TokenTypes.TT_NAME, 'name');
    });
    it("can recognise a TT_NEWLAYER token", () => {
      recogniseToken(TokenTypes.TT_NEWLAYER, 'newlayer');
    });
    it("can recognise a TT_OLDCHARPOSMATCHING token", () => {
      recogniseToken(TokenTypes.TT_OLDCHARPOSMATCHING, 'oldcharposmatching');
    });
    it("can recognise a TT_OLDLAYER token", () => {
      recogniseToken(TokenTypes.TT_OLDLAYER, 'oldlayer');
    });
    it("can recognise a TT_TARGETS token", () => {
      recogniseToken(TokenTypes.TT_TARGETS, 'targets');
    });
    it("can recognise a TT_VERSION token", () => {
      recogniseToken(TokenTypes.TT_VERSION, 'version');
    });
    it("can recognise a TT_VISUALKEYBOARD token", () => {
      recogniseToken(TokenTypes.TT_VISUALKEYBOARD, 'visualkeyboard');
    });
    it("can recognise a TT_WINDOWSLANGUAGES token", () => {
      recogniseToken(TokenTypes.TT_WINDOWSLANGUAGES, 'windowslanguages');
    });
    it("can recognise a TT_ANY token", () => {
      recogniseToken(TokenTypes.TT_ANY, 'any');
    });
    it("can recognise a TT_BASELAYOUT token", () => {
      recogniseToken(TokenTypes.TT_BASELAYOUT, 'baselayout');
    });
    it("can recognise a TT_BEEP token", () => {
      recogniseToken(TokenTypes.TT_BEEP, 'beep');
    });
    it("can recognise a TT_BEGIN token", () => {
      recogniseToken(TokenTypes.TT_BEGIN, 'begin');
    });
    it("can recognise a TT_CALL token", () => {
      recogniseToken(TokenTypes.TT_CALL, 'call');
    });
    it("can recognise a TT_CONTEXT token", () => {
      recogniseToken(TokenTypes.TT_CONTEXT, 'context');
    });
    it("can recognise a TT_GROUP token", () => {
      recogniseToken(TokenTypes.TT_GROUP, 'group');
    });
    it("can recognise a TT_IF token", () => {
      recogniseToken(TokenTypes.TT_IF, 'if');
    });
    it("can recognise a TT_MATCH token", () => {
      recogniseToken(TokenTypes.TT_MATCH, 'match');
    });
    it("can recognise a TT_NOMATCH token", () => {
      recogniseToken(TokenTypes.TT_NOMATCH, 'nomatch');
    });
    it("can recognise a TT_OUTS token", () => {
      recogniseToken(TokenTypes.TT_OUTS, 'outs');
    });
    it("can recognise a TT_PLATFORM token", () => {
      recogniseToken(TokenTypes.TT_PLATFORM, 'platform');
    });
    it("can recognise a TT_STORE token", () => {
      recogniseToken(TokenTypes.TT_STORE, 'store');
    });
    it("can recognise a TT_STORE token in upper case", () => {
      recogniseToken(TokenTypes.TT_STORE, 'STORE');
    });
    it("can recognise a TT_STORE token in mixed case", () => {
      recogniseToken(TokenTypes.TT_STORE, 'Store');
    });
    it("can recognise a TT_USE token", () => {
      recogniseToken(TokenTypes.TT_USE, 'use');
    });
    it("can recognise a TT_UNICODE token", () => {
      recogniseToken(TokenTypes.TT_UNICODE, 'unicode');
    });
    it("can recognise a TT_NEWCONTEXT token", () => {
      recogniseToken(TokenTypes.TT_NEWCONTEXT, 'newcontext');
    });
    it("can recognise a TT_POSTKEYSTROKE token", () => {
      recogniseToken(TokenTypes.TT_POSTKEYSTROKE, 'postkeystroke');
    });
    it("can recognise a TT_READONLY token", () => {
      recogniseToken(TokenTypes.TT_READONLY, 'readonly');
    });
    it("can recognise a TT_USING token", () => {
      recogniseToken(TokenTypes.TT_USING, 'using');
    });
    it("can recognise a TT_KEYS token", () => {
      recogniseToken(TokenTypes.TT_KEYS, 'keys');
    });
    it("can recognise a TT_ANSI token", () => {
      recogniseToken(TokenTypes.TT_ANSI, 'ansi');
    });
    it("can recognise a TT_LEFT_BR token", () => {
      recogniseToken(TokenTypes.TT_LEFT_BR, '(');
    });
    it("can recognise a TT_RIGHT_BR token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_BR, ')');
    });
    it("can recognise a TT_LEFT_SQ token", () => {
      recogniseToken(TokenTypes.TT_LEFT_SQ, '[');
    });
    it("can recognise a TT_RIGHT_SQ token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_SQ, ']');
    });
    it("can recognise a TT_AMPHASAND token", () => {
      recogniseToken(TokenTypes.TT_AMPHASAND, '&');
    });
    it("can recognise a TT_CHEVRON token", () => {
      recogniseToken(TokenTypes.TT_CHEVRON, '>');
    });
    it("can recognise a TT_PLUS token", () => {
      recogniseToken(TokenTypes.TT_PLUS, '+');
    });
    it("can recognise a TT_COMMA token", () => {
      recogniseToken(TokenTypes.TT_COMMA, ',');
    });
    it("can recognise a TT_NOT_EQUAL token", () => {
      recogniseToken(TokenTypes.TT_NOT_EQUAL, '!=');
    });
    it("can recognise a TT_EQUAL token", () => {
      recogniseToken(TokenTypes.TT_EQUAL, '=');
    });
    it("can recognise a TT_U_CHAR token", () => {
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+A');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1B');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1CD');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1EF0');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+23456');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+789ABC');
    });
    it("can recognise a TT_STRING token (single quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '\'10.0\'');
    });
    it("can recognise a TT_STRING token (double quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '"10.0"');
    });
    it("can recognise a TT_STRING token (repeated quotes)", () => {
      recogniseTokens(
        '\'hello\' \'world\'',
        [
          new Token(TokenTypes.TT_STRING,     '\'hello\''),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_STRING,     '\'world\''),
        ]
      );
    });
    it("does not pick out tokens from inside strings", () => {
      recogniseToken(TokenTypes.TT_STRING, '"store"');
    });
    it("can recognise a TT_SHIFT_CODE token", () => {
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'SHIFT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'CTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'LCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'RCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'ALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'LALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'RALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'CAPS');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'NCAPS');
    });
    it("can recognise a TT_KEY_CODE token", () => {
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'K_K');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'K_COLON');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'T_17D2_1780');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'U_0030');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'A21');
    });
    it("can recognise a TT_KEY_CODE token (followed by space)", () => {
      recogniseTokens(
        'K_K ]',
        [
          new Token(TokenTypes.TT_KEY_CODE,   'K_K'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_RIGHT_SQ,   ']'),
        ]
      );
    });
    it("can recognise a TT_COMMENT token", () => {
      recogniseToken(TokenTypes.TT_COMMENT, 'c This tells Keyman which keys should have casing behavior applied');
    });
    it("can recognise a TT_COMMENT token (followed by LF)", () => {
      recogniseTokens(
        'c This tells Keyman which keys should have casing behavior applied\n',
        [
          new Token(TokenTypes.TT_COMMENT,  'c This tells Keyman which keys should have casing behavior applied'),
          new Token(TokenTypes.TT_NEWLINE,  '\n'),
        ]
      );
    });
    it("can recognise a TT_WHITESPACE token (single space)", () => {
      recogniseToken(TokenTypes.TT_WHITESPACE, ' ');
    });
    it("can recognise a TT_CONTINUATION token (no space after)", () => {
      recogniseTokens(
        '\\\n',
        [
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
        ]
      );
    });
    it("can recognise a TT_CONTINUATION token (space after)", () => {
      recogniseTokens(
        '\\ \n',
        [
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
        ]
      );
    });
    it("can recognise multiple TT_CONTINUATION tokens", () => {
      recogniseTokens(
        'store(LaoConsonants) U+0E81 U+0E82 U+0E84 U+0E87 U+0E88 U+0E8A U+0E8D U+0E94 \\\n' +
        '                     U+0E95 U+0E96 U+0E97 U+0E99 U+0E9A U+0E9B U+0E9C U+0E9D \\\n' +
        '                     U+0E9E U+0E9F U+0EA1 U+0EA2 U+0EA3 U+0EA5 U+0EA7 U+0EAA \\\n' +
        '                     U+0EAB U+0EAD U+0EAE    c list of all the Lao consonants\n',
        [
          new Token(TokenTypes.TT_STORE,        'store'),
          new Token(TokenTypes.TT_LEFT_BR,      '('),
          new Token(TokenTypes.TT_PARAMETER,    'LaoConsonants'),
          new Token(TokenTypes.TT_RIGHT_BR,     ')'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E81'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E82'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E84'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E87'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E88'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E8A'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E8D'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E94'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
          new Token(TokenTypes.TT_WHITESPACE,   '                     '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E95'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E96'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E97'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E99'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9A'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9B'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9C'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9D'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
          new Token(TokenTypes.TT_WHITESPACE,   '                     '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9E'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0E9F'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EA1'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EA2'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EA3'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EA5'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EA7'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EAA'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
          new Token(TokenTypes.TT_WHITESPACE,   '                     '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EAB'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EAD'),
          new Token(TokenTypes.TT_WHITESPACE,   ' '),
          new Token(TokenTypes.TT_U_CHAR,       'U+0EAE'),
          new Token(TokenTypes.TT_WHITESPACE,   '    '),
          new Token(TokenTypes.TT_COMMENT,      'c list of all the Lao consonants'),
          new Token(TokenTypes.TT_NEWLINE,      '\n'),
        ]
      );
    });
    it("can recognise a TT_NEWLINE token (LF)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\n');
    });
    it("can recognise a TT_NEWLINE token (CR)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\r');
    });
    it("can recognise a TT_NEWLINE token (CRLF)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\r\n');
    });
    it("can recognise a TT_PARAMETER token", () => {
      recogniseTokens(
        'main)',
        [
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets", () => {
      recogniseTokens(
        '(main)',
        [
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets (single space before)", () => {
      recogniseTokens(
        '( main)',
        [
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets (single space after)", () => {
      recogniseTokens(
        '(main )',
        [
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise two TT_PARAMETER tokens in brackets (comma separated)", () => {
      recogniseTokens(
        '(main,2)',
        [
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_COMMA,      ','),
          new Token(TokenTypes.TT_PARAMETER,  '2'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise two TT_PARAMETER tokens in brackets (space separated)", () => {
      recogniseTokens(
        '(main 2)',
        [
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_PARAMETER,  '2'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise a bitmap store", () => {
      recogniseStoreWithString(TokenTypes.TT_BITMAP, 'khmer_angkor.ico');
    });
    it("can recognise a copyright store", () => {
      recogniseStoreWithString(TokenTypes.TT_COPYRIGHT, '© SIL Global');
    });
    it("can recognise a displaymap store", () => {
      recogniseStoreWithString(TokenTypes.TT_DISPLAYMAP, '../../../shared/fonts/kbd/kbdkhmr/KbdKhmr.json');
    });
    it("can recognise a keyboardversion store", () => {
      recogniseStoreWithString(TokenTypes.TT_KEYBOARDVERSION, '2.0');
    });
    it("can recognise a layoutfile store", () => {
      recogniseStoreWithString(TokenTypes.TT_LAYOUTFILE, 'khmer_angkor.keyman-touch-layout');
    });
    it("can recognise a name store", () => {
      recogniseStoreWithString(TokenTypes.TT_NAME, "Khmer Angkor");
    });
    it("can recognise a message store", () => {
      recogniseStoreWithString(TokenTypes.TT_MESSAGE, "More than just a Khmer Unicode keyboard.");
    });
    it("can recognise a targets store", () => {
      recogniseStoreWithString(TokenTypes.TT_TARGETS, 'any');
    });
    it("can recognise a version store", () => {
      recogniseStoreWithString(TokenTypes.TT_VERSION, 'khmer_angkor.kvks');
    });
    it("can recognise a visualkeyboard store", () => {
      recogniseStoreWithString(TokenTypes.TT_VISUALKEYBOARD, '10.0');
    });
    it("can recognise a begin statement (unicode)", () => {
      recogniseTokens(
        'begin Unicode > use(main)',
        [
          new Token(TokenTypes.TT_BEGIN,      'begin'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_UNICODE,    'Unicode'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_CHEVRON,    '>'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_USE,        'use'),
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_PARAMETER,  'main'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
        ]
      );
    });
    it("can recognise a begin statement (postkeystroke)", () => {
      recogniseTokens(
        'begin PostKeystroke > use(PostKeystroke)',
        [
          new Token(TokenTypes.TT_BEGIN,         'begin'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_POSTKEYSTROKE, 'PostKeystroke'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CHEVRON,       '>'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_USE,           'use'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_POSTKEYSTROKE, 'PostKeystroke'), // recognised as keyword
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a group statement (readonly)", () => {
      recogniseTokens(
        'group(NewContext) readonly',
        [
          new Token(TokenTypes.TT_GROUP,         'group'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_NEWCONTEXT,     'NewContext'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_READONLY,      'readonly'),
        ]
      );
    });
    it("can recognise a group statement (using keys)", () => {
      recogniseTokens(
        'group(main) using keys',
        [
          new Token(TokenTypes.TT_GROUP,         'group'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'main'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_USING,         'using'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_KEYS,          'keys'),
        ]
      );
    });
    it("can recognise an if statement (using any, context, layer)", () => {
      recogniseTokens(
        'if(&newLayer = "") if(&layer = \'shift\') any(ShiftOutSingle) > context layer(\'default\')',
        [
          new Token(TokenTypes.TT_IF,            'if'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_AMPHASAND,     '&'),
          new Token(TokenTypes.TT_NEWLAYER,      'newLayer'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_EQUAL,         '='),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_STRING,        '""'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_IF,            'if'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_AMPHASAND,     '&'),
          new Token(TokenTypes.TT_LAYER,         'layer'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_EQUAL,         '='),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_STRING,        '\'shift\''),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_ANY,           'any'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'ShiftOutSingle'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CHEVRON,       '>'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CONTEXT,       'context'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_LAYER,         'layer'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_STRING,        '\'default\''),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a match statement", () => {
      recogniseTokens(
        'match > use(CombineDiacritics)',
        [
          new Token(TokenTypes.TT_MATCH,         'match'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CHEVRON,       '>'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_USE,           'use'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'CombineDiacritics'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a nomatch statement", () => {
      recogniseTokens(
        'nomatch > layer(\'default\')',
        [
          new Token(TokenTypes.TT_NOMATCH,       'nomatch'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CHEVRON,       '>'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_LAYER,         'layer'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_STRING,        '\'default\''),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a platform statement", () => {
      recogniseTokens(
        'platform(\'touch\') > use(detectStartOfSentence)',
        [
          new Token(TokenTypes.TT_PLATFORM,      'platform'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_STRING,        '\'touch\''),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_CHEVRON,       '>'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_USE,           'use'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'detectStartOfSentence'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a store statement using outs", () => {
      recogniseTokens(
        'store(ShiftOutAll)  outs(ShiftOutSingle) outs(vCombo1) outs(vCombo2) outs(vCombo3)',
        [
          new Token(TokenTypes.TT_STORE,         'store'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,      'ShiftOutAll'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    '  '),
          new Token(TokenTypes.TT_OUTS,          'outs'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,      'ShiftOutSingle'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_OUTS,          'outs'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,      'vCombo1'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_OUTS,          'outs'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,      'vCombo2'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_OUTS,          'outs'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,      'vCombo3'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
        ]
      );
    });
    it("can recognise a store statement (khmer string)", () => {
      recogniseTokens(
        'store(ShiftOutSingle) \'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'',
        [
          new Token(TokenTypes.TT_STORE,         'store'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'ShiftOutSingle'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_STRING,        '\'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\''),
        ]
      );
    });
    it("can recognise a store statement (unicode chars)", () => {
      recogniseTokens(
        'store(whitespace) \' \' U+00A0 U+000D U+000A',
        [
          new Token(TokenTypes.TT_STORE,         'store'),
          new Token(TokenTypes.TT_LEFT_BR,       '('),
          new Token(TokenTypes.TT_PARAMETER,     'whitespace'),
          new Token(TokenTypes.TT_RIGHT_BR,      ')'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_STRING,        '\' \''),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_U_CHAR,        'U+00A0'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_U_CHAR,        'U+000D'),
          new Token(TokenTypes.TT_WHITESPACE,    ' '),
          new Token(TokenTypes.TT_U_CHAR,        'U+000A'),
        ]
      );
    });
  });
});

function recogniseToken(type: TokenTypes, text: String): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse();
  const expected = [new Token(type, text)];
  assert.deepEqual(actual, expected);
}

function recogniseTokens(text: String, expected: Token[]): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse();
  assert.deepEqual(actual, expected);
}

function recogniseTokenFollowedBySpace(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text} `,
    [
      new Token(type, text),
      new Token(TokenTypes.TT_WHITESPACE, ' '),
    ]
  );
}

function recogniseTokenFollowedByRightSquare(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text}]`,
    [
      new Token(type, text),
      new Token(TokenTypes.TT_RIGHT_SQ, ']'),
    ]
  );
}

function recogniseStoreWithString(type: TokenTypes, text: String) {
  const value = TokenTypes[type].substring(3).toLowerCase();
  recogniseTokens(
    `store(&${value}) '${text}'`,
    [
      new Token(TokenTypes.TT_STORE,      'store'),
      new Token(TokenTypes.TT_LEFT_BR,    '('),
      new Token(TokenTypes.TT_AMPHASAND,  '&'),
      new Token(type, value),
      new Token(TokenTypes.TT_RIGHT_BR,   ')'),
      new Token(TokenTypes.TT_WHITESPACE, ' '),
      new Token(TokenTypes.TT_STRING,     `'${text}'`),
    ]
  );
}
