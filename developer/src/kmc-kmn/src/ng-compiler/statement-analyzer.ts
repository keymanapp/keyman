/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-02
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Statement Analyser)
 *
 * Statement Rule Tests
 */

import { TokenType } from "./token-type.js";
import { PlainTextRule } from "./kmn-analyzer.js";
import { AlternateRule, OneOrManyRule, Rule, SequenceRule, SingleChildRule, SingleChildRuleWithASTStrategy, TokenRule } from "./recursive-descent.js";
import { DeadkeyNameRule, NormalStoreNameRule, StoreNameRule, SystemStoreNameRule } from "./store-analyzer.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TokenBuffer } from "./token-buffer.js";
import { GivenNode, StackedPair } from "./ast-strategy.js";

abstract class AbstractBracketedStoreNameStatementRule extends SingleChildRule {
  protected leftBracket: Rule;
  protected normalStoreName: Rule;
  protected rightBracket: Rule;

  public constructor(
    /** the node type of the command in a child class */
    protected cmdNodeType: NodeType
  ) {
    super();
    this.leftBracket     = new TokenRule(TokenType.LEFT_BR);
    this.normalStoreName = new NormalStoreNameRule();
    this.rightBracket    = new TokenRule(TokenType.RIGHT_BR);
  }

  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      const cmdNode       = tmp.getSoleChildOfType(this.cmdNodeType);
      const storeNameNode = tmp.getSoleChildOfType(NodeType.STORENAME);
      cmdNode.addChild(storeNameNode);
      node.addChild(cmdNode);
    }
    return parseSuccess;
  }
}

/**
 * (BNF) anyStatement: ANY LEFT_BR normalStoreName RIGHT_BR
 */
export class AnyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super(NodeType.ANY);
    const any: Rule = new TokenRule(TokenType.ANY, true);
    this.rule = new SequenceRule([
      any, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

/**
 * (BNF) callStatement: CALL LEFT_BR normalStoreName RIGHT_BR
 */
export class CallStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super(NodeType.CALL);
    const call: Rule = new TokenRule(TokenType.CALL, true);
    this.rule = new SequenceRule([
      call, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

/**
 * (BNF) deadkeyStatement: DEADKEY LEFT_BR deadkeyName RIGHT_BR
 */
export class DeadkeyStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new StackedPair(NodeType.DEADKEY, NodeType.DEADKEYNAME));
    const deadkey: Rule      = new TokenRule(TokenType.DEADKEY, true);
    const leftBracket: Rule  = new TokenRule(TokenType.LEFT_BR);
    const deadkeyName: Rule  = new DeadkeyNameRule();
    const rightBracket: Rule = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([
      deadkey, leftBracket, deadkeyName, rightBracket
    ]);
  }
}

/**
 * (BNF) notanyStatement: NOTANY LEFT_BR normalStoreName RIGHT_BR
 */
export class NotanyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super(NodeType.NOTANY);
    const notany: Rule = new TokenRule(TokenType.NOTANY, true);
    this.rule = new SequenceRule([
      notany, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

/**
 * (BNF) saveStatement: SAVE LEFT_BR normalStoreName RIGHT_BR
 */
export class SaveStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super(NodeType.SAVE);
    const save: Rule = new TokenRule(TokenType.SAVE, true);
    this.rule = new SequenceRule([
      save, this.leftBracket, this.normalStoreName, this.rightBracket
    ]);
  }
}

abstract class AbstractShortcutRule extends SingleChildRuleWithASTStrategy {
  protected leftBracket: Rule;
  protected plainText: Rule;
  protected oneOrManyPlainText: Rule;
  protected rightBracket: Rule;

  public constructor(nodeType: NodeType) {
    super(new GivenNode(nodeType));
    this.leftBracket        = new TokenRule(TokenType.LEFT_BR);
    this.plainText          = new PlainTextRule();
    this.oneOrManyPlainText = new OneOrManyRule(this.plainText);
    this.rightBracket       = new TokenRule(TokenType.RIGHT_BR);
  }
}

/**
 * (BNF) baselayoutStatement: BASELAYOUT_SHORTCUT LEFT_BR plainText+ RIGHT_BR
 */
export class BaselayoutStatementRule extends AbstractShortcutRule {
  public constructor() {
    super(NodeType.BASELAYOUT_SHORTCUT);
    const baselayout: Rule = new TokenRule(TokenType.BASELAYOUT_SHORTCUT, true);
    this.rule = new SequenceRule([
      baselayout, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}

/**
 * (BNF) layerStatement: LAYER_SHORTCUT LEFT_BR plainText+ RIGHT_BR
 */
export class LayerStatementRule extends AbstractShortcutRule {
  public constructor() {
    super(NodeType.LAYER_SHORTCUT);
    const layer: Rule = new TokenRule(TokenType.LAYER_SHORTCUT, true);
    this.rule = new SequenceRule([
      layer, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}

/**
 * (BNF) platformStatement: PLATFORM_SHORTCUT LEFT_BR plainText+ RIGHT_BR
 */
export class PlatformStatementRule extends AbstractShortcutRule {
  public constructor() {
    super(NodeType.PLATFORM_SHORTCUT);
    const platform: Rule  = new TokenRule(TokenType.PLATFORM_SHORTCUT, true);
    this.rule = new SequenceRule([
      platform, this.leftBracket, this.oneOrManyPlainText, this.rightBracket
    ]);
  }
}

/**
 * (BNF) ifLikeStatement: ifStatement|platformStatement|baselayoutStatement
 */
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

/**
 * (BNF) ifStatement: ifNormalStoreStatement|ifSystemStoreStatement
 */
export class IfStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
    const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
    this.rule = new AlternateRule([ifNormalStoreStatement, ifSystemStoreStatement]);
  }
}

abstract class AbstractIfStoreStatementRule extends SingleChildRuleWithASTStrategy {
  protected ifRule: Rule;
  protected leftBracket: Rule;
  protected comparison: Rule;
  protected rightBracket: Rule;
  protected plainText: Rule;
  protected oneOrManyPlainText: Rule;

  public constructor() {
    super(new GivenNode(NodeType.IF));
    this.ifRule             = new TokenRule(TokenType.IF, true);
    this.leftBracket        = new TokenRule(TokenType.LEFT_BR);
    this.comparison         = new ComparisonRule();
    this.rightBracket       = new TokenRule(TokenType.RIGHT_BR);
    this.plainText          = new PlainTextRule();
    this.oneOrManyPlainText = new OneOrManyRule(this.plainText);
  }
}

/**
 * (BNF) ifNormalStoreStatement: IF LEFT_BR normalStoreName comparison plainText+ RIGHT_BR
 */
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

/**
 * (BNF) ifSystemStoreStatement: IF LEFT_BR systemStoreNameForIf comparison plainText+ RIGHT_BR
 */
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

/**
 * (BNF) systemStoreNameForIf: systemStoreName|BASELAYOUT|LAYER|NEWLAYER|OLDLAYER|PLATFORM
 */
export class SystemStoreNameForIfRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    const baselayout: Rule      = new TokenRule(TokenType.BASELAYOUT, true);
    const layer: Rule           = new TokenRule(TokenType.LAYER, true);
    const newlayer: Rule        = new TokenRule(TokenType.NEWLAYER, true);
    const oldlayer: Rule        = new TokenRule(TokenType.OLDLAYER, true);
    const platform: Rule        = new TokenRule(TokenType.PLATFORM, true);
    this.rule = new AlternateRule([
      systemStoreName,
      baselayout,
      layer,
      newlayer,
      oldlayer,
      platform,
    ]);
  }
}

/**
 * (BNF) comparison:EQUAL|NOT_EQUAL
 */
export class ComparisonRule extends SingleChildRule {
  public constructor() {
    super();
    const equal: Rule    = new TokenRule(TokenType.EQUAL, true);
    const notEqual: Rule = new TokenRule(TokenType.NOT_EQUAL, true);
    this.rule = new AlternateRule([equal, notEqual]);
  }
}

/**
 * (BNF) contextStatement: CONTEXT LEFT_BR offset RIGHT_BR
 */
export class ContextStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.CONTEXT));
    const context: Rule       = new TokenRule(TokenType.CONTEXT, true);
    const leftBracket: Rule   = new TokenRule(TokenType.LEFT_BR);
    const offset: Rule        = new OffsetRule();
    const rightBracket: Rule  = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([
      context,
      leftBracket,
      offset,
      rightBracket,
    ]);
  }
}

/**
 * (BNF) indexStatement: INDEX LEFT_BR normalStoreName COMMA offset RIGHT_BR
 */
export class IndexStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.INDEX));
    const index: Rule           = new TokenRule(TokenType.INDEX, true);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const comma: Rule           = new TokenRule(TokenType.COMMA);
    const offset: Rule          = new OffsetRule();
    const rightBracket: Rule    = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([
      index,
      leftBracket,
      normalStoreName,
      comma,
      offset,
      rightBracket,
    ]);
  }
}

/**
 * (BNF) offset: OCTAL|PARAMETER
 *
 * OCTAL is included as this could be a valid offset
 */
export class OffsetRule extends SingleChildRule {
  public constructor() {
    super();
    const octal: Rule     = new TokenRule(TokenType.OCTAL, true);
    const parameter: Rule = new TokenRule(TokenType.PARAMETER, true);
    this.rule = new AlternateRule([octal, parameter]);
  }

  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      node.addNewChildWithToken(NodeType.OFFSET, tmp.getSoleChild().token);
    }
    return parseSuccess;
  };
}

/**
 * (BNF) outsStatement: OUTS LEFT_BR storeName RIGHT_BR
 */
export class OutsStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.OUTS));
    const outs: Rule         = new TokenRule(TokenType.OUTS, true);
    const leftBracket: Rule  = new TokenRule(TokenType.LEFT_BR);
    const storeName: Rule    = new StoreNameRule();
    const rightBracket: Rule = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([
      outs, leftBracket, storeName, rightBracket
    ]);
  }
}
