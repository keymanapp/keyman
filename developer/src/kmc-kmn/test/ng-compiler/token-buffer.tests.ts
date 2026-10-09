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
import { baselineKeyboardNames, PATH_TO_BASELINE, PATH_TO_REPOSITORY, repositoryKeyboardNames } from './keyboard-names.js';
import { existsSync, readFileSync } from 'node:fs';

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
  describe("TokenBuffer currentToken()", () => {
    it("provides the correct initial Token", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer popToken()", () => {
    it("can handle a single pop", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
    });
    it("can handle popping the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.currentToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.currentToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.currentToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.currentToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.currentToken().tokenType,TokenType.RIGHT_BR);
    });
    it("can handle popping beyond the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.currentToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.currentToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.currentToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.currentToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.currentToken().tokenType,TokenType.RIGHT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.currentToken().tokenType,TokenType.EOF);
    });
  });
  describe("TokenBuffer resetCurrentPosition()", () => {
    it("can handle resetting to zero", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.currentToken().tokenType,TokenType.WHITESPACE);
      tb.resetCurrentPosition(0);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
    });
    it("can handle resetting to buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(8);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.currentToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to beyond buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(9);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.currentToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to a negative value", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(-1);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.currentToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer round trip tests [toText()]", () => {
    it("can provide round trip text for khmer_angkor.kmn", () => {
      const buffer: string = readFile('test/fixtures/keyboards/khmer_angkor.kmn');
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
      const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
      const output: string = tokenBuffer.toText();
      assert.equal(output, buffer);
    });
    it("can provide round trip text for baseline keyboards", function() {
      if (!existsSync(PATH_TO_BASELINE)) {
        this.skip();
      }
      baselineKeyboardNames().forEach((name) => {
        const buffer: string = readFile(`${PATH_TO_BASELINE}${name}.kmn`);
        const lexer = new Lexer(buffer);
        const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
        const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
        const output: string = tokenBuffer.toText();
        assert.equal(output, buffer, `${name}.kmn`);
      });
    });
    // if the keyboard repository is absent, this loop will not execute as it will be of
    // zero length, so all repository keyboard round trip tests will be (silently) skipped
    for (let i = 0; i < repositoryKeyboardNames().length; i+=100) {
      const start = i;
      const end   = Math.min(i+100, repositoryKeyboardNames().length);
      it(`can provide round trip text for repository keyboards (${start}-${end-1})`, function() {
        repositoryKeyboardNames().slice(start, end).forEach((name) => {
        const buffer: string = readFile(`${PATH_TO_REPOSITORY}${name}.kmn`);
        const lexer = new Lexer(buffer);
        const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
        const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
        const output: string = tokenBuffer.toText();
        assert.equal(output, buffer, `${name}.kmn`);
        });
      }).timeout(50000);
    }
  });
});

/**
 * Read a file as a string, handling utf-8, utf16-le or cp1252 encodings
 *
 * @param path the path to the file
 * @returns the file contents
 */
export function readFile(path: string): string {
  return bufferToString(readFileSync(path) as Uint8Array);
}

function bufferToString(buffer: Uint8Array): string {
  let decoder: TextDecoder;
  if (buffer.at(0) === 0xff && buffer.at(1) === 0xfe) {
    // we don't fail on encoding errors for UTF16 w/BOM
    decoder = new TextDecoder('utf-16le', { ignoreBOM: true, fatal: false });
  } else {
    // attempt to read as UTF8, fallback to cp1252
    decoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
  }

  try {
    return decoder.decode(buffer);
  } catch(e) {
    // fallback to ANSI
    // TODO-NG-COMPILER: emit a hint KmnCompilerMessages::HINT_NonUnicodeFile
    decoder = new TextDecoder('cp1252', { fatal: false });
    return decoder.decode(buffer);
  }
}
