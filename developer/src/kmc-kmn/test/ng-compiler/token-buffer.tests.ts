/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * Tests for KMC KMN Next Generation Parser (TokenBuffer)
 */

import 'mocha';
import { assert } from 'chai';
import { Token, TokenTypes } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';

// nomatch > layer('default')
const LIST: Token[] = [
  new Token(TokenTypes.TT_NOMATCH, 'nomatch'),
  new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 8),
  new Token(TokenTypes.TT_CHEVRON, '>', 1, 9),
  new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 10),
  new Token(TokenTypes.TT_LAYER, 'layer', 1, 11),
  new Token(TokenTypes.TT_LEFT_BR, '(', 1, 16),
  new Token(TokenTypes.TT_STRING, '\'default\'', 1, 17),
  new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 26),
];

describe("TokenBuffer Tests", () => {
  it("can construct a TokenBuffer with list", () => {
    const tb: TokenBuffer = new TokenBuffer(LIST);
    assert.isNotNull(tb);
    assert.equal(tb.currentPosition, 0);
  });
});