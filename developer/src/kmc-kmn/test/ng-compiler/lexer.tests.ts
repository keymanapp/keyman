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
    it("can recognise a TT_VERSION token", () => {
      recogniseToken(TokenTypes.TT_VERSION, 'version');
    });
    it("can recognise a TT_NAME token", () => {
      recogniseToken(TokenTypes.TT_NAME, 'name');
    });
    it("can recognise a TT_STRING token (single quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '\'10.0\'');
    });
    it("can recognise a TT_STRING token (double quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '"10.0"');
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
