/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import 'mocha';
import { assert } from 'chai';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { Lexer, Token } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { BitmapStoreRule } from '../../src/ng-compiler/kmn-analyser.js';

describe("KMN Analyser Tests", () => {
  describe("BitmapStoreRule Tests", () => {
    it("can construct a BitmapStoreRule", () => {
      const buffer: String = "store(&VERSION) '10.0'";
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse();
      const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
      const bitmap: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isNotNull(bitmap);
    });
  });
});

