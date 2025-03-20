/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenTypes } from "./lexer.js";
import { KeywordRule, OptionalRule, Rule, SequenceRule, SingleChildRule } from "./recursive-descent.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export class BitmapStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
    const whitespace: Rule  = new KeywordRule(tokenBuffer, TokenTypes.WHITESPACE);
    const stringRule: Rule  = new KeywordRule(tokenBuffer, TokenTypes.STRING, true);
    this.rule = new SequenceRule(tokenBuffer, [
      bitmapStore, whitespace, stringRule,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const bitmapNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.BITMAP);
      const stringNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.STRING);
      bitmapNode.addChild(stringNode);
      node.addChild(bitmapNode);
    }
    return parseSuccess;
  }
}

export class BitmapStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule         = new KeywordRule(tokenBuffer, TokenTypes.STORE);
    const leftBracket: Rule   = new KeywordRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpace(tokenBuffer);
    const amphasand: Rule     = new KeywordRule(tokenBuffer, TokenTypes.AMPHASAND);
    const bitmap: Rule        = new KeywordRule(tokenBuffer, TokenTypes.BITMAP, true);
    const rightBracket: Rule  = new KeywordRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, optWhitespace, amphasand, bitmap, optWhitespace, rightBracket,
    ]);
  }
}

export class OptionalWhiteSpace extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const whitespace: Rule  = new KeywordRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.rule = new OptionalRule(tokenBuffer, whitespace);
  }
}
