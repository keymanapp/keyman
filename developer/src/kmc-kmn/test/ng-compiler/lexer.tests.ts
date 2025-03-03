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
    it("can recognise a TT_BEGIN token", () => {
      recogniseToken(TokenTypes.TT_BEGIN, 'begin');
    });
    it("can recognise a TT_OUTS token", () => {
      recogniseToken(TokenTypes.TT_OUTS, 'outs');
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
    it("can recognise a TT_ANSI token", () => {
      recogniseToken(TokenTypes.TT_ANSI, 'ansi');
    });
    it("can recognise a TT_LEFT_BR token", () => {
      recogniseToken(TokenTypes.TT_LEFT_BR, '(');
    });
    it("can recognise a TT_RIGHT_BR token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_BR, ')');
    });
    it("can recognise a TT_AMPHASAND token", () => {
      recogniseToken(TokenTypes.TT_AMPHASAND, '&');
    });
    it("can recognise a TT_CHEVRON token", () => {
      recogniseToken(TokenTypes.TT_CHEVRON, '>');
    });
    it("can recognise a TT_COMMA token", () => {
      recogniseToken(TokenTypes.TT_COMMA, ',');
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
    it("does not pick out tokens from inside strings", () => {
      recogniseToken(TokenTypes.TT_STRING, '"store"');
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
