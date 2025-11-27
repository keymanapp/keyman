/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * Tests for KMC KMN Next Generation Compiler (TokenBuffer)
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { Lexer, Token } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { BASELINE_KEYBOARD_NAMES, REPOSITORY_KEYBOARD_NAMES } from '../../src/ng-compiler/keyboard-names.js';
import { readFileSync } from 'fs';

// nomatch > layer('default')
const LIST: Token[] = [
  new Token(TokenType.NOMATCH, 'nomatch'),
  new Token(TokenType.WHITESPACE, ' ', 1, 8),
  new Token(TokenType.CHEVRON, '>', 1, 9),
  new Token(TokenType.WHITESPACE, ' ', 1, 10),
  new Token(TokenType.LAYER, 'layer', 1, 11),
  new Token(TokenType.LEFT_BR, '(', 1, 16),
  new Token(TokenType.STRING, '\'default\'', 1, 17),
  new Token(TokenType.RIGHT_BR, ')', 1, 26),
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
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer popToken()", () => {
    it("can handle a single pop", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
    });
    it("can handle popping the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenType.RIGHT_BR);
    });
    it("can handle popping beyond the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenType.RIGHT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
  });
  describe("TokenBuffer resetCurrentPosition()", () => {
    it("can handle resetting to zero", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.resetCurrentPosition(0);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
    it("can handle resetting to buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(8);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to beyond buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(9);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to a negative value", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(-1);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer round trip tests [toText()]", () => {
    it("can provide round trip text for khmer_angkor.kmn", () => {
      const buffer: string = readFileSync('test/fixtures/keyboards/khmer_angkor.kmn').toString();
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
      const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
      const output: string = tokenBuffer.toText();
      assert.equal(output, buffer);
    });
    it("can provide round trip text for baseline keyboards", () => {
      BASELINE_KEYBOARD_NAMES.forEach((name) => {
        const buffer: string = readFileSync(`../../../common/test/keyboards/baseline/${name}.kmn`).toString();
        const lexer = new Lexer(buffer);
        const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
        const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
        const output: string = tokenBuffer.toText();
        assert.equal(output, buffer, `${name}.kmn`);
      });
    });
    it("can provide round trip text for repository keyboards (0-99)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(0, 100));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (100-199)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(100, 200));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (200-299)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(200, 300));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (300-399)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(300, 400));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (400-499)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(400, 500));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (500-599)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(500, 600));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (600-699)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(600, 700));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (700-799)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(700, 800));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (800-899)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(800, 900));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (900-end)", () => {
      testRepositoryKeyboards(REPOSITORY_KEYBOARD_NAMES.slice(900));
    }).timeout(50000);
  });
});

function testRepositoryKeyboards(names: string[]) {
  names.forEach((name) => {
    const buffer: string = readFileSync(`../../../../keyboards/${name}.kmn`).toString();
    const lexer = new Lexer(buffer);
    const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
    const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
    const output: string = tokenBuffer.toText();
    assert.equal(output, buffer, `${name}.kmn`);
  });
}