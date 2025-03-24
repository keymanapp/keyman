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

export abstract class SystemStoreAssignRule extends SingleChildRule {
  protected whitespace: Rule     = null;
  protected stringRule: Rule     = null;
  protected storeType: NodeTypes = null;

  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.whitespace = new KeywordRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.stringRule = new KeywordRule(tokenBuffer, TokenTypes.STRING, true);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode  = tmp.getSoleChildOfType(this.storeType);
      const stringNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.STRING);
      storeNode.addChild(stringNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export abstract class SystemStoreRule extends SingleChildRule {
  protected store: Rule           = null;
  protected leftBracket: Rule     = null;
  protected optWhitespace: Rule   = null;
  protected amphasand: Rule       = null;
  protected rightBracket: Rule    = null;

  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.store         = new KeywordRule(tokenBuffer, TokenTypes.STORE);
    this.leftBracket   = new KeywordRule(tokenBuffer, TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpace(tokenBuffer);
    this.amphasand     = new KeywordRule(tokenBuffer, TokenTypes.AMPHASAND);
    this.rightBracket  = new KeywordRule(tokenBuffer, TokenTypes.RIGHT_BR);
  }
}

export class BitmapStoreAssignRule extends SystemStoreAssignRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.BITMAP;
    const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      bitmapStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class BitmapStoreRule extends SystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmap: Rule = new KeywordRule(tokenBuffer, TokenTypes.BITMAP, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, bitmap, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class CopyrightStoreAssignRule extends SystemStoreAssignRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.COPYRIGHT;
    const copyrightStore: Rule = new CopyrightStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      copyrightStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class CopyrightStoreRule extends SystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const copyright: Rule = new KeywordRule(tokenBuffer, TokenTypes.COPYRIGHT, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, copyright, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IncludecodesStoreAssignRule extends SystemStoreAssignRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.INCLUDECODES;
    const includecodesStore: Rule = new IncludecodesStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      includecodesStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class IncludecodesStoreRule extends SystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const includecodes: Rule = new KeywordRule(tokenBuffer, TokenTypes.INCLUDECODES, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, includecodes, this.optWhitespace, this.rightBracket,
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
