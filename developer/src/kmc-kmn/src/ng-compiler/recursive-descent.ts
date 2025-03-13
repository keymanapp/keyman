/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (TokenBuffer)
 */

import { Token } from "./lexer.js";
import { TokenBuffer } from "./token-buffer.js";

export class RecursiveDescent {
  protected tokenBuffer: TokenBuffer ;

  protected constructor(list: Token[]) {
    this.tokenBuffer = new TokenBuffer(list);
  }
}

export class Rule {
  protected rule: Rule;

  protected constructor(rule: Rule=null) {
    this.rule = rule;
  }

  protected parse(node: ASTNode): Boolean {
    return this.rule.parse(node);
  }
}

