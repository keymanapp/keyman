/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenTypes } from "./lexer.js";
import { AlternateRule, KeywordRule, OptionalRule, Rule, SequenceRule, SingleChildRule } from "./recursive-descent.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export class ContinuationLineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule   = new OptionalWhiteSpace(tokenBuffer);
    const content: Rule         = new ContentRule(tokenBuffer);
    const whitespace: Rule      = new KeywordRule(tokenBuffer, TokenTypes.WHITESPACE);
    const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
    const newline: Rule         = new KeywordRule(tokenBuffer, TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(tokenBuffer, [
      optWhitespace, content, whitespace, continuationEnd, newline
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNode: ASTNode   = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const contentNode: ASTNode = tmp.getSoleChild();
      lineNode.addChild(contentNode);
      node.addChild(lineNode);
    }
    return parseSuccess;
  }
}

export class CommentEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule = new OptionalWhiteSpace(tokenBuffer);
    const comment: Rule       = new KeywordRule(tokenBuffer, TokenTypes.COMMENT);
    this.rule = new SequenceRule(tokenBuffer, [optWhitespace, comment]);
  }
}

export class ContinuationEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const continuation: Rule  = new KeywordRule(tokenBuffer, TokenTypes.CONTINUATION);
    const optWhitespace: Rule = new OptionalWhiteSpace(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [continuation, optWhitespace]);
  }
}

export class ContentRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.rule = new SystemStoreAssignRule(tokenBuffer);
  }
}

export class SystemStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmapStoreAssign: Rule       = new BitmapStoreAssignRule(tokenBuffer);
    const copyrightStoreAssign: Rule    = new CopyrightStoreAssignRule(tokenBuffer);
    const includecodesStoreAssign: Rule = new IncludecodesStoreAssignRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [
      bitmapStoreAssign, copyrightStoreAssign, includecodesStoreAssign,
    ]);
  }
}

abstract class SystemStoreAssignAbstractRule extends SingleChildRule {
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

abstract class SystemStoreAbstractRule extends SingleChildRule {
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

export class BitmapStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.BITMAP;
    const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      bitmapStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class BitmapStoreRule extends SystemStoreAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmap: Rule = new KeywordRule(tokenBuffer, TokenTypes.BITMAP, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, bitmap, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class CopyrightStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.COPYRIGHT;
    const copyrightStore: Rule = new CopyrightStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      copyrightStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class CopyrightStoreRule extends SystemStoreAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const copyright: Rule = new KeywordRule(tokenBuffer, TokenTypes.COPYRIGHT, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, copyright, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IncludecodesStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.INCLUDECODES;
    const includecodesStore: Rule = new IncludecodesStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      includecodesStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class IncludecodesStoreRule extends SystemStoreAbstractRule {
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
