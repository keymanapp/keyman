/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-02
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Statement Analyser)
 *
 * Statement Rule Tests
 */

import { TokenTypes } from "./lexer.js";
import { Rule, SequenceRule, SingleChildRule, TokenRule } from "./recursive-descent.js";
import { NormalStoreNameRule } from "./store-analyser.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

abstract class AbstractBracketedStoreNameStatementRule extends SingleChildRule {
  protected leftBracket: Rule;
  protected normalStoreName: Rule;
  protected rightBracket: Rule;
  protected cmdNodeType: NodeTypes;

  public constructor() {
    super();
    this.leftBracket     = new TokenRule(TokenTypes.LEFT_BR);
    this.normalStoreName = new NormalStoreNameRule();
    this.rightBracket    = new TokenRule(TokenTypes.RIGHT_BR);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const cmdNode       = tmp.getSoleChildOfType(this.cmdNodeType);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      cmdNode.addChild(storeNameNode);
      node.addChild(cmdNode);
    }
    return parseSuccess;
  }
}

export class AnyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const any: Rule  = new TokenRule(TokenTypes.ANY, true);
    this.cmdNodeType = NodeTypes.ANY;
    this.rule = new SequenceRule([
      any, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

export class CallStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const call: Rule = new TokenRule(TokenTypes.CALL, true);
    this.cmdNodeType = NodeTypes.CALL;
    this.rule = new SequenceRule([
      call, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

export class DeadKeyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const deadKey: Rule = new TokenRule(TokenTypes.DEADKEY, true);
    this.cmdNodeType = NodeTypes.DEADKEY;
    this.rule = new SequenceRule([
      deadKey, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

export class NotAnyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const notAny: Rule = new TokenRule(TokenTypes.NOTANY, true);
    this.cmdNodeType   = NodeTypes.NOTANY;
    this.rule = new SequenceRule([
      notAny, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

export class SaveStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const save: Rule = new TokenRule(TokenTypes.SAVE, true);
    this.cmdNodeType = NodeTypes.SAVE;
    this.rule = new SequenceRule([
      save, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

