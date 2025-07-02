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
import { PlainTextRule } from "./kmn-analyser.js";
import { OneOrManyRule, Rule, SequenceRule, SingleChildRule, TokenRule } from "./recursive-descent.js";
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

abstract class AbstractShortcutRule extends SingleChildRule {
  protected leftBracket: Rule;
  protected plainText: Rule;
  protected oneOrManyPlainText: Rule;
  protected rightBracket: Rule;
  protected shortcutNodeType: NodeTypes;

  public constructor() {
    super();
    this.leftBracket        = new TokenRule(TokenTypes.LEFT_BR);
    this.plainText          = new PlainTextRule();
    this.oneOrManyPlainText = new OneOrManyRule(this.plainText);
    this.rightBracket       = new TokenRule(TokenTypes.RIGHT_BR);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const shortcutNode = tmp.removeSoleChildOfType(this.shortcutNodeType);
      shortcutNode.addChildren(tmp.getChildren());
      node.addChild(shortcutNode);
    }
    return parseSuccess;
  }
}

export class BaselayoutStatementRule extends AbstractShortcutRule {
  public constructor() {
    super();
    this.shortcutNodeType = NodeTypes.BASELAYOUT_SHORTCUT;
    const baselayout: Rule   = new TokenRule(TokenTypes.BASELAYOUT_SHORTCUT, true);
    this.rule = new SequenceRule([
      baselayout, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}

export class LayerStatementRule extends AbstractShortcutRule {
  public constructor() {
    super();
    this.shortcutNodeType = NodeTypes.LAYER_SHORTCUT;
    const layer: Rule     = new TokenRule(TokenTypes.LAYER_SHORTCUT, true);
    this.rule = new SequenceRule([
      layer, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}

export class PlatformStatementRule extends AbstractShortcutRule {
  public constructor() {
    super();
    this.shortcutNodeType = NodeTypes.PLATFORM_SHORTCUT;
    const platform: Rule  = new TokenRule(TokenTypes.PLATFORM_SHORTCUT, true);
    this.rule = new SequenceRule([
      platform, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}
