/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * Tests for KMC KMN Next Generation Parser (TokenBuffer)
 */

import 'mocha';
import { assert } from 'chai';
import { Lexer, Token, TokenTypes } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { readFileSync } from 'fs';

// nomatch > layer('default')
const LIST: Token[] = [
  new Token(TokenTypes.NOMATCH, 'nomatch'),
  new Token(TokenTypes.WHITESPACE, ' ', 1, 8),
  new Token(TokenTypes.CHEVRON, '>', 1, 9),
  new Token(TokenTypes.WHITESPACE, ' ', 1, 10),
  new Token(TokenTypes.LAYER, 'layer', 1, 11),
  new Token(TokenTypes.LEFT_BR, '(', 1, 16),
  new Token(TokenTypes.STRING, '\'default\'', 1, 17),
  new Token(TokenTypes.RIGHT_BR, ')', 1, 26),
];

describe("TokenBuffer Tests", () => {
  describe("TokenBuffer constructor", () => {
    it("can construct a TokenBuffer with list", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
    it("can construct a TokenBuffer with empty list", () => {
      const tb: TokenBuffer = new TokenBuffer([]);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
    it("can construct a TokenBuffer with null", () => {
      const tb: TokenBuffer = new TokenBuffer(null);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
  });
  describe("TokenBuffer currentPosition getter", () => {
    it("can provide the correct initial currentPosition", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
    });
  });
  describe("TokenBuffer nextToken()", () => {
    it("provides the correct initial Token", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
    });
  });
  describe("TokenBuffer popToken()", () => {
    it("can handle a single pop", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
    });
    it("can handle popping the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenTypes.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenTypes.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenTypes.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenTypes.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenTypes.RIGHT_BR);
    });
    it("can handle popping beyond the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenTypes.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenTypes.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenTypes.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenTypes.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenTypes.RIGHT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenTypes.EOF);
    });
  });
  describe("TokenBuffer resetCurrentPosition()", () => {
    it("can handle resetting to zero", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenTypes.WHITESPACE);
      tb.resetCurrentPosition(0);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
    });
    it("can handle resetting to buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.resetCurrentPosition(8);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenTypes.EOF);
    });
    it("can handle resetting to beyond buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.resetCurrentPosition(9);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenTypes.EOF);
    });
    it("can handle resetting to a negative value", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
      tb.resetCurrentPosition(-1);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenTypes.NOMATCH);
    });
  });
  describe("TokenBuffer toText()", () => {
    it("can provide round trip text", () => {
      const buffer: String  = new String(readFileSync('test/fixtures/keyboards/khmer_angkor.kmn'));
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse();
      const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
      const output: String = tokenBuffer.toText();
      assert.deepEqual(output.toString(), buffer.toString());
    });
  });
});