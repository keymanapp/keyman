/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-02
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Statement Analyser)
 *
 * Statement Rule Tests
 */

import { TokenTypes } from "./token-types.js";
import { PlainTextRule } from "./kmn-analyser.js";
import { AlternateRule, OneOrManyRule, Rule, SequenceRule, SingleChildRule, TokenRule } from "./recursive-descent.js";
import { NormalStoreNameRule, StoreNameRule, SystemStoreNameRule } from "./store-analyser.js";
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

export class IfLikeStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const ifStatement: Rule         = new IfStatementRule();
    const platformStatement: Rule   = new PlatformStatementRule();
    const baselayoutStatement: Rule = new BaselayoutStatementRule();
    this.rule = new AlternateRule([
      ifStatement, platformStatement, baselayoutStatement,
    ]);
  }
}

export class IfStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
    const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
    this.rule = new AlternateRule([ifNormalStoreStatement, ifSystemStoreStatement]);
  }
}

abstract class AbstractIfStoreStatementRule extends SingleChildRule {
  protected ifRule: Rule;
  protected leftBracket: Rule;
  protected comparison: Rule;
  protected rightBracket: Rule;
  protected plainText: Rule;
  protected oneOrManyPlainText: Rule;

  public constructor() {
    super();
    this.ifRule             = new TokenRule(TokenTypes.IF, true);
    this.leftBracket        = new TokenRule(TokenTypes.LEFT_BR);
    this.comparison         = new ComparisonRule();
    this.rightBracket       = new TokenRule(TokenTypes.RIGHT_BR);
    this.plainText          = new PlainTextRule();
    this.oneOrManyPlainText = new OneOrManyRule(this.plainText);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const ifNode   = tmp.removeSoleChildOfType(NodeTypes.IF);
      const children = tmp.getChildren();
      ifNode.addChildren(children);
      node.addChild(ifNode);
    }
    return parseSuccess;
  }
}

export class IfNormalStoreStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const normalStoreName: Rule = new NormalStoreNameRule();
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, normalStoreName,
      this.comparison, this.oneOrManyPlainText, this.rightBracket,
    ]);
  }
}

export class IfSystemStoreStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, systemStoreNameForIf,
      this.comparison, this.oneOrManyPlainText, this.rightBracket,
    ]);
  }
}

export class SystemStoreNameForIfRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    const baselayout: Rule      = new TokenRule(TokenTypes.BASELAYOUT, true);
    const layer: Rule           = new TokenRule(TokenTypes.LAYER, true);
    const newLayer: Rule        = new TokenRule(TokenTypes.NEWLAYER, true);
    const oldLayer: Rule        = new TokenRule(TokenTypes.OLDLAYER, true);
    const platform: Rule        = new TokenRule(TokenTypes.PLATFORM, true);
    this.rule = new AlternateRule([
      systemStoreName,
      baselayout,
      layer,
      newLayer,
      oldLayer,
      platform,
    ]);
  }
}

export class ComparisonRule extends SingleChildRule {
  public constructor() {
    super();
    const equal: Rule    = new TokenRule(TokenTypes.EQUAL, true);
    const notEqual: Rule = new TokenRule(TokenTypes.NOT_EQUAL, true);
    this.rule = new AlternateRule([equal, notEqual]);
  }
}

export class ContextStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const index: Rule         = new TokenRule(TokenTypes.CONTEXT, true);
    const leftBracket: Rule   = new TokenRule(TokenTypes.LEFT_BR);
    const offset: Rule        = new OffsetRule();
    const rightBracket: Rule  = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      index,
      leftBracket,
      offset,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const contextNode = tmp.removeSoleChildOfType(NodeTypes.CONTEXT);
      contextNode.addChildren(tmp.getChildren());
      node.addChild(contextNode);
    }
    return parseSuccess;
  }
}

export class IndexStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const index: Rule           = new TokenRule(TokenTypes.INDEX, true);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const comma: Rule           = new TokenRule(TokenTypes.COMMA);
    const offset: Rule          = new OffsetRule();
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      index,
      leftBracket,
      normalStoreName,
      comma,
      offset,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const indexNode = tmp.removeSoleChildOfType(NodeTypes.INDEX);
      indexNode.addChildren(tmp.getChildren());
      node.addChild(indexNode);
    }
    return parseSuccess;
  }
}

export class OffsetRule extends SingleChildRule {
  public constructor() {
    super();
    const octal: Rule     = new TokenRule(TokenTypes.OCTAL, true);
    const parameter: Rule = new TokenRule(TokenTypes.PARAMETER, true);
    this.rule = new AlternateRule([octal, parameter]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const child = tmp.getSoleChild();
      node.addToken(NodeTypes.OFFSET, child.token);
    }
    return parseSuccess;
  };
}

export class OutsStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const outs: Rule         = new TokenRule(TokenTypes.OUTS, true);
    const leftBracket: Rule  = new TokenRule(TokenTypes.LEFT_BR);
    const storeName: Rule    = new StoreNameRule();
    const rightBracket: Rule = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      outs, leftBracket, storeName, rightBracket
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const outsNode = tmp.removeSoleChildOfType(NodeTypes.OUTS);
      outsNode.addChild(tmp.getSoleChild());
      node.addChild(outsNode);
    }
    return parseSuccess;
  }
}
