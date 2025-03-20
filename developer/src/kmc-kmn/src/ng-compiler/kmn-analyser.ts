/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenTypes } from "./lexer.js";
import { KeywordRule, Rule, SequenceRule, SingleChildRule } from "./recursive-descent.js";
import { TokenBuffer } from "./token-buffer.js";

export class BitmapStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule        = new KeywordRule(tokenBuffer, TokenTypes.STORE);
    const leftBracket: Rule  = new KeywordRule(tokenBuffer, TokenTypes.LEFT_BR);
    const amphasand: Rule    = new KeywordRule(tokenBuffer, TokenTypes.AMPHASAND);
    const bitmap: Rule       = new KeywordRule(tokenBuffer, TokenTypes.BITMAP);
    const rightBracket: Rule = new KeywordRule(tokenBuffer, TokenTypes.RIGHT_BR);
    const whitespace: Rule   = new KeywordRule(tokenBuffer, TokenTypes.WHITESPACE);
    const stringRule: Rule   = new KeywordRule(tokenBuffer, TokenTypes.STRING);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, amphasand, bitmap, rightBracket, whitespace, stringRule,
    ]);
  }
}