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
      const sr = new ScanRecogniser(TokenTypes.TT_STORE, new RegExp("^store"),      true);
      assert.deepEqual(sr.toString(), '[TT_STORE,/^store/,true]');
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
    it("can recognise a TT_VERSION token", () => {
      recogniseToken(TokenTypes.TT_VERSION, 'version');
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
    it("can recognise a TT_LEFT_BR token", () => {
      recogniseToken(TokenTypes.TT_LEFT_BR, '(');
    });
    it("can recognise a TT_RIGHT_BR token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_BR, ')');
    });
    it("can recognise a TT_AMPHASAND token", () => {
      recogniseToken(TokenTypes.TT_AMPHASAND, '&');
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
    it("can recognise a name store", () => {
      recogniseTokens(
        'store(&NAME) "Khmer Angkor"',
        [
          new Token(TokenTypes.TT_STORE,      'store'),
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_AMPHASAND,  '&'),
          new Token(TokenTypes.TT_NAME,       'NAME'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_STRING,     '"Khmer Angkor"'),
        ]
      );
    });
    it("can recognise a version store", () => {
      recogniseTokens(
        'store(&VERSION) \'10.0\'',
        [
          new Token(TokenTypes.TT_STORE,      'store'),
          new Token(TokenTypes.TT_LEFT_BR,    '('),
          new Token(TokenTypes.TT_AMPHASAND,  '&'),
          new Token(TokenTypes.TT_VERSION,    'VERSION'),
          new Token(TokenTypes.TT_RIGHT_BR,   ')'),
          new Token(TokenTypes.TT_WHITESPACE, ' '),
          new Token(TokenTypes.TT_STRING,     '\'10.0\''),
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
