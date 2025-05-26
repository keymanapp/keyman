/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { Token, TokenTypes } from "./lexer.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule } from "./recursive-descent.js";
import { SingleChildRule, parameterSequence, OneOrManyRule, ManyRule } from "./recursive-descent.js";
import { BracketedStoreNameRule, CapsAlwaysOffRule, CapsOnOnlyRule, PermittedKeywordRule, ResetStoreRule, SetLayerRule, SetStoreRule, ShiftFreesCapsRule, SystemStoreNameRule } from "./store-analyser.js";
import { SystemStoreAssignRule, VariableStoreAssignRule, VariableStoreNameRule } from "./store-analyser.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export class KmnTreeRule extends SingleChildRule {
  public constructor() {
    super();
    const line: Rule = new LineRule();
    this.rule = new ManyRule(line);
  }
}

export class LineRule extends SingleChildRule {
  public constructor() {
    super();
    const contentLine: Rule = new ContentLineRule();
    const blankLine: Rule   = new BlankLineRule();
    this.rule = new AlternateRule([contentLine, blankLine]);
  }
}

export class ContentLineRule extends SingleChildRule {
  public constructor() {
    super();
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const content: Rule       = new ContentRule();
    const optComment: Rule    = new OptionalCommentRule();
    const newline: Rule       = new TokenRule(TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule([
      optWhitespace, content, optWhitespace, optComment, newline
    ]);
  }
}

export class BlankLineRule extends SingleChildRule {
  public constructor() {
    super();
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const optComment: Rule    = new OptionalCommentRule();
    const newline: Rule       = new TokenRule(TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule([optWhitespace, optComment, newline]);
  }
}

export class OptionalCommentRule extends SingleChildRule {
  public constructor() {
    super();
    const commentRule: Rule = new TokenRule(TokenTypes.COMMENT);
    this.rule = new OptionalRule(commentRule);
  }
}

export class PaddingRule extends SingleChildRule {
  public constructor() {
    super();
    const whitespace          = new TokenRule(TokenTypes.WHITESPACE);
    const continuationNewline = new ContinuationNewlineRule();
    this.rule = new AlternateRule([continuationNewline, whitespace]);
  }
}

export class ContinuationNewlineRule extends SingleChildRule {
  public constructor() {
    super();
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const continuation: Rule  = new TokenRule(TokenTypes.CONTINUATION);
    const newline: Rule       = new TokenRule(TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(
      [optWhitespace, continuation, optWhitespace, newline, optWhitespace]
    );
  }
}

export class ContentRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreAssign: Rule    = new SystemStoreAssignRule();
    const capsAlwaysOff: Rule        = new CapsAlwaysOffRule();
    const capsOnOnly: Rule           = new CapsOnOnlyRule();
    const shiftFreesCaps: Rule       = new ShiftFreesCapsRule();
    const variableStoreAssign: Rule  = new VariableStoreAssignRule();
    const ruleBlock: Rule            = new RuleBlockRule();
    this.rule = new AlternateRule([
      systemStoreAssign,
      capsAlwaysOff,
      capsOnOnly,
      shiftFreesCaps,
      variableStoreAssign,
      ruleBlock,
    ]);
  }
}

export class OptionalWhiteSpaceRule extends SingleChildRule {
  public constructor() {
    super();
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    this.rule = new OptionalRule(whitespace);
  }
}

export class PrePadTextRule extends SingleChildRule {
  public constructor() {
    super();
    const padding: Rule = new PaddingRule();
    const text: Rule    = new TextRule();
    this.rule = new SequenceRule([padding, text]);
  }
}

export class TextRule extends SingleChildRule {
  public constructor() {
    super();
    const plainText: Rule     = new PlainTextRule();
    const outsStatement: Rule = new OutsStatementRule();
    this.rule = new AlternateRule([plainText, outsStatement]);
  }
}

export class PlainTextRule extends SingleChildRule {
  public constructor() {
    super();
    const textRange: Rule     = new TextRangeRule();
    const simpleText: Rule    = new SimpleTextRule();
    this.rule = new AlternateRule([textRange, simpleText]);
  }
}

export class SimpleTextRule extends SingleChildRule {
  public constructor() {
    super();
    const stringRule = new TokenRule(TokenTypes.STRING, true);
    const virtualKey = new VirtualKeyRule();
    const uChar      = new TokenRule(TokenTypes.U_CHAR, true);
    this.rule = new AlternateRule([ stringRule, virtualKey, uChar]);
  }
}

export class TextRangeRule extends SingleChildRule {
  public constructor() {
    super();
    const simpleText: Rule        = new SimpleTextRule();
    const rangeEnd: Rule          = new RangeEndRule();
    const oneOrManyRangeEnd: Rule = new OneOrManyRule(rangeEnd);
    this.rule = new SequenceRule([simpleText, oneOrManyRangeEnd]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const rangeNode: ASTNode = new ASTNode(NodeTypes.RANGE);
      rangeNode.addChildren(tmp.getChildren());
      node.addChild(rangeNode);
    }
    return parseSuccess;
  }
}

export class RangeEndRule extends SingleChildRule {
  public constructor() {
    super();
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const range: Rule         = new TokenRule(TokenTypes.RANGE);
    const simpleText: Rule    = new SimpleTextRule();
    this.rule = new SequenceRule([
      optWhitespace, range, optWhitespace, simpleText
    ]);
  }
}

export class VirtualKeyRule extends SingleChildRule {
  public constructor() {
    super();
    const leftSquare: Rule         = new TokenRule(TokenTypes.LEFT_SQ);
    const optWhitespace: Rule      = new OptionalWhiteSpaceRule();
    const spacedModifier: Rule     = new SpacedModifierRule();
    const manySpacedModifier: Rule = new ManyRule(spacedModifier);
    const keyCode: Rule            = new TokenRule(TokenTypes.KEY_CODE, true);
    const rightSquare: Rule        = new TokenRule(TokenTypes.RIGHT_SQ);
    this.rule = new SequenceRule([
      leftSquare, optWhitespace, manySpacedModifier, keyCode, optWhitespace, rightSquare
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const virtualKeyNode       = new ASTNode(NodeTypes.VIRTUAL_KEY);
      virtualKeyNode.addChildren(tmp.getChildren());
      node.addChild(virtualKeyNode);
      node.addChildren(lineNodes);
    }
    return parseSuccess;
  }
}

export class SpacedModifierRule extends SingleChildRule {
  public constructor() {
    super();
    const modifier: Rule   = new ModifierRule();
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    this.rule = new SequenceRule([modifier, whitespace]);
  }
}

export class ModifierRule extends SingleChildRule {
  public constructor() {
    super();
    const shift: Rule    = new TokenRule(TokenTypes.SHIFT, true);
    const caps: Rule     = new TokenRule(TokenTypes.CAPS, true);
    const modifier: Rule = new TokenRule(TokenTypes.MODIFIER, true);
    this.rule = new AlternateRule([shift, caps, modifier]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      let modifierNode = tmp.getSoleChildOfType(NodeTypes.MODIFIER);
      if (modifierNode === null) {
        modifierNode = new ASTNode(NodeTypes.MODIFIER, tmp.getSoleChild().token);
      }
      node.addChild(modifierNode);
    }
    return parseSuccess;
  }
}

abstract class AbstractBracketedStoreNameStatementRule extends SingleChildRule {
  protected bracketedStoreName: Rule;
  protected cmdNodeType: NodeTypes;

  public constructor() {
    super();
    this.bracketedStoreName = new BracketedStoreNameRule();
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

export class OutsStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const outs: Rule = new TokenRule(TokenTypes.OUTS, true);
    this.cmdNodeType = NodeTypes.OUTS;
    this.rule = new SequenceRule([outs, this.bracketedStoreName]);
  }
}

export class RuleBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginBlock: Rule               = new BeginBlockRule();
    const groupBlock: Rule               = new GroupBlockRule();
    const usingKeysProductionBlock: Rule = new UsingKeysProductionBlockRule();
    const readOnlyProductionBlock: Rule  = new ReadOnlyProductionBlockRule();
    const contextProductionBlock: Rule   = new ContextProductionBlockRule();
    this.rule = new AlternateRule([
      beginBlock,
      groupBlock,
      usingKeysProductionBlock,
      readOnlyProductionBlock,
      contextProductionBlock,
    ]);
  }
}

export class BeginBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginStatement: Rule = new BeginStatementRule();
    const spacedChevron: Rule  = new SpacedChevronRule();
    const useStatement: Rule   = new UseStatementRule();
    this.rule = new SequenceRule([beginStatement, spacedChevron, useStatement]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const beginNode = tmp.getSoleChildOfType(NodeTypes.BEGIN);
      const useNode   = tmp.getSoleChildOfType(NodeTypes.USE);
      beginNode.addChild(useNode);
      node.addChild(beginNode);
    }
    return parseSuccess;
  }
}

export class BeginStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const begin: Rule                = new TokenRule(TokenTypes.BEGIN, true);
    const optSpacedEntryPoint: Rule  = new OptionalSpacedEntryPointRule();
    this.rule = new SequenceRule([begin, optSpacedEntryPoint]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const beginNode      = tmp.removeSoleChildOfType(NodeTypes.BEGIN);
      const entryPointNode = tmp.getSoleChild();
      beginNode.addChild(entryPointNode);
      node.addChild(beginNode);
    }
    return parseSuccess;
  }
}

export class SpacedEntryPointRule extends SingleChildRule {
  public constructor() {
    super();
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    const entryPoint: Rule = new EntryPointRule();
    this.rule = new SequenceRule([whitespace, entryPoint]);
  }
}

export class EntryPointRule extends SingleChildRule {
  public constructor() {
    super();
    const unicode: Rule       = new TokenRule(TokenTypes.UNICODE, true);
    const newcontext: Rule    = new TokenRule(TokenTypes.NEWCONTEXT, true);
    const postkeystroke: Rule = new TokenRule(TokenTypes.POSTKEYSTROKE, true);
    const ansi: Rule          = new TokenRule(TokenTypes.ANSI, true);
    this.rule = new AlternateRule([
      unicode, newcontext, postkeystroke, ansi,
    ]);
  }
}

export class OptionalSpacedEntryPointRule extends SingleChildRule {
  public constructor() {
    super();
    const spacedEntryPointRule: Rule = new SpacedEntryPointRule();
    this.rule = new OptionalRule(spacedEntryPointRule);
  }
}

export class SpacedChevronRule extends SingleChildRule {
  public constructor() {
    super();
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    const chevron: Rule    = new TokenRule(TokenTypes.CHEVRON);
    this.rule = new SequenceRule([whitespace, chevron, whitespace]);
  }
}

export class UseStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const use: Rule                = new TokenRule(TokenTypes.USE, true);
    const bracketedGroupName: Rule = new BracketedGroupNameRule();
    this.rule = new SequenceRule([use, bracketedGroupName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const useNode       = tmp.getSoleChildOfType(NodeTypes.USE);
      const groupNameNode = tmp.getSoleChildOfType(NodeTypes.GROUPNAME);
      useNode.addChild(groupNameNode);
      node.addChild(useNode);
    }
    return parseSuccess;
  }
}

export class BracketedGroupNameRule extends SingleChildRule {
  public constructor() {
    super();
    const leftBracket: Rule        = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule      = new OptionalWhiteSpaceRule();
    const groupNameOrKeyword: Rule = new GroupNameOrKeywordRule();
    const rightBracket: Rule       = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      leftBracket, optWhitespace, groupNameOrKeyword, optWhitespace, rightBracket,
    ]);
  }
}

export class GroupBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const groupStatement: Rule          = new GroupStatementRule();
    const optSpacedGroupQualifier: Rule = new OptionalSpacedGroupQualifierRule();
    this.rule = new SequenceRule([groupStatement, optSpacedGroupQualifier]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const groupNode          = tmp.removeSoleChildOfType(NodeTypes.GROUP);
      const groupQualifierNode = tmp.getSoleChild();
      groupNode.addChild(groupQualifierNode);
      node.addChild(groupNode);
    }
    return parseSuccess;
  }
}

export class GroupStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const group: Rule              = new TokenRule(TokenTypes.GROUP, true);
    const bracketedGroupName: Rule = new BracketedGroupNameRule();
    this.rule = new SequenceRule([group, bracketedGroupName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const groupNode     = tmp.getSoleChildOfType(NodeTypes.GROUP);
      const groupNameNode = tmp.getSoleChildOfType(NodeTypes.GROUPNAME);
      groupNode.addChild(groupNameNode);
      node.addChild(groupNode);
    }
    return parseSuccess;
  }
}

export class GroupNameOrKeywordRule extends SingleChildRule {
  public constructor() {
    super();
    const groupName: Rule        = new GroupNameRule();
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([groupName, permittedKeyword]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      if (!tmp.hasChildOfType(NodeTypes.GROUPNAME)) {
        node.addToken(NodeTypes.GROUPNAME, tmp.getSoleChild().token);
      } else {
        node.addChild(tmp.getSoleChildOfType(NodeTypes.GROUPNAME));
      }
    }
    return parseSuccess;
  }
}

export class GroupNameRule extends SingleChildRule {
  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    const parameters: Token[]   = [];
    const parseSuccess: boolean = parameterSequence(parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.GROUPNAME, parameters[0]);
    }
    return parseSuccess;
  };
}

export class OptionalSpacedGroupQualifierRule extends SingleChildRule {
  public constructor() {
    super();
    const spacedGroupQualifierRule: Rule = new SpacedGroupQualifierRule();
    this.rule = new OptionalRule(spacedGroupQualifierRule);
  }
}

export class SpacedGroupQualifierRule extends SingleChildRule {
  public constructor() {
    super();
    const whitespace: Rule     = new TokenRule(TokenTypes.WHITESPACE);
    const groupQualifier: Rule = new GroupQualifierRule();
    this.rule = new SequenceRule([whitespace, groupQualifier]);
  }
}

export class GroupQualifierRule extends SingleChildRule {
  public constructor() {
    super();
    const usingKeys: Rule = new UsingKeysRule();
    const readonly: Rule  = new TokenRule(TokenTypes.READONLY, true);
    this.rule = new AlternateRule([usingKeys, readonly]);
  }
}

export class UsingKeysRule extends SingleChildRule {
  public constructor() {
    super();
    const using: Rule      = new TokenRule(TokenTypes.USING);
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    const keys: Rule       = new TokenRule(TokenTypes.KEYS);
    this.rule = new SequenceRule([
      using, whitespace, keys,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      node.addChild(new ASTNode(NodeTypes.USING_KEYS));
    }
    return parseSuccess;
  }
}

abstract class AbstractProductionBlockRule extends SingleChildRule {
  protected spacedChevron: Rule;
  protected rhsBlock: Rule;
  protected lhsNodeType: NodeTypes;
  protected productionNodeType: NodeTypes;

  public constructor() {
    super();
    this.spacedChevron = new SpacedChevronRule();
    this.rhsBlock      = new RhsBlockRule();
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lhsNode        = tmp.getSoleChildOfType(this.lhsNodeType);
      const rhsNode        = tmp.getSoleChildOfType(NodeTypes.RHS);
      const lhsLines       = lhsNode.removeChildrenOfType(NodeTypes.LINE);
      const rhsLines       = rhsNode.removeChildrenOfType(NodeTypes.LINE);
      const productionNode = new ASTNode(this.productionNodeType);
      productionNode.addChild(lhsNode);
      productionNode.addChild(rhsNode);
      productionNode.addChildren(lhsLines);
      productionNode.addChildren(rhsLines);
      node.addChild(productionNode);
    }
    return parseSuccess;
  }
}

export class UsingKeysProductionBlockRule extends AbstractProductionBlockRule {
  public constructor() {
    super();
    this.lhsNodeType        = NodeTypes.LHS_USING_KEYS;
    this.productionNodeType = NodeTypes.PRODUCTION_USING_KEYS;
    const usingKeysLhsBlock = new UsingKeysLhsBlockRule();
    this.rule = new SequenceRule([usingKeysLhsBlock, this.spacedChevron, this.rhsBlock]);
  }
}

export class ReadOnlyProductionBlockRule extends AbstractProductionBlockRule {
  public constructor() {
    super();
    this.lhsNodeType        = NodeTypes.LHS_READONLY;
    this.productionNodeType = NodeTypes.PRODUCTION_READONLY;
    const readOnlyLhsBlock  = new ReadOnlyLhsBlockRule();
    this.rule = new SequenceRule([readOnlyLhsBlock, this.spacedChevron, this.rhsBlock]);
  }
}

export class ContextProductionBlockRule extends AbstractProductionBlockRule {
  public constructor() {
    super();
    this.lhsNodeType        = NodeTypes.LHS_CONTEXT;
    this.productionNodeType = NodeTypes.PRODUCTION_CONTEXT;
    const contextLhsBlock   = new ContextLhsBlockRule();
    this.rule = new SequenceRule([contextLhsBlock, this.spacedChevron, this.rhsBlock]);
  }
}

abstract class AbstractLhsBlockRule extends SingleChildRule {
  protected lhsNodeType: NodeTypes;

  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes = tmp.removeChildrenOfType(NodeTypes.LINE);
      const lhsNode   = new ASTNode(this.lhsNodeType);
      lhsNode.addChildren(tmp.getChildren());
      lhsNode.addChildren(lineNodes);
      node.addChild(lhsNode);
    }
    return parseSuccess;
  }
}

export class UsingKeysLhsBlockRule extends AbstractLhsBlockRule {
  public constructor() {
    super();
    this.lhsNodeType = NodeTypes.LHS_USING_KEYS;
    this.rule        = new UsingKeysInputBlockRule();
  }
}

export class ReadOnlyLhsBlockRule extends AbstractLhsBlockRule {
  public constructor() {
    super();
    this.lhsNodeType                = NodeTypes.LHS_READONLY;
    const match: Rule               = new TokenRule(TokenTypes.MATCH, true);
    const noMatch: Rule             = new TokenRule(TokenTypes.NOMATCH, true);
    const readOnlyInputBlock: Rule  = new ReadOnlyInputBlockRule();
    const ifLikeBlock: Rule         = new IfLikeBlockRule();
    this.rule = new AlternateRule([
      match, noMatch, readOnlyInputBlock, ifLikeBlock,
    ]);
  }
}

export class ContextLhsBlockRule extends AbstractLhsBlockRule {
  public constructor() {
    super();
    this.lhsNodeType              = NodeTypes.LHS_CONTEXT;
    const contextInputBlock: Rule = new ContextInputBlockRule();
    const nulInputBlock: Rule     = new NulInputBlockRule();
    this.rule = new AlternateRule([contextInputBlock, nulInputBlock]);
  }
}

export class UsingKeysInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPostPadNul: Rule          = new OptionalPostPadNulRule();
    const optPostPadIfLikeBlock: Rule  = new OptionalPostPadIfLikeBlockRule();
    const optPostPadInputContext: Rule = new OptionalPostPadInputContextRule();
    const plus: Rule                   = new TokenRule(TokenTypes.PLUS);
    const padding: Rule                = new PaddingRule();
    const keystoke: Rule               = new KeystrokeRule();
    this.rule = new SequenceRule([
      optPostPadNul,
      optPostPadIfLikeBlock,
      optPostPadInputContext,
      plus,
      padding,
      keystoke,
    ]);
  }
}

export class ReadOnlyInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPostPadNul: Rule         = new OptionalPostPadNulRule();
    const optPostPadIfLikeBlock: Rule = new OptionalPostPadIfLikeBlockRule();
    const keystoke: Rule              = new KeystrokeRule();
    this.rule = new SequenceRule([
      optPostPadNul, optPostPadIfLikeBlock, keystoke,
    ]);
  }
}

export class ContextInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPostPadNul: Rule         = new OptionalPostPadNulRule();
    const optPostPadIfLikeBlock: Rule = new OptionalPostPadIfLikeBlockRule();
    const inputContext: Rule          = new InputContextRule();
    this.rule = new SequenceRule([
      optPostPadNul, optPostPadIfLikeBlock, inputContext,
    ]);
  }
}

export class NulInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const nulRule: Rule              = new TokenRule(TokenTypes.NUL, true);
    const optPrePadIfLikeBlock: Rule = new OptionalPrePadIfLikeBlockRule();
    this.rule = new SequenceRule([nulRule, optPrePadIfLikeBlock]);
  }
}

export class OptionalPostPadNulRule extends SingleChildRule {
  public constructor() {
    super();
    const postPadNul: Rule = new PostPadNulRule();
    this.rule = new OptionalRule(postPadNul);
  }
}

export class PostPadNulRule extends SingleChildRule {
  public constructor() {
    super();
    const nulRule: Rule = new TokenRule(TokenTypes.NUL, true);
    const padding: Rule = new PaddingRule();
    this.rule = new SequenceRule([nulRule, padding]);
  }
}

export class OptionalPrePadIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const prePadIfLikeBlock: Rule = new PrePadIfLikeBlockRule();
    this.rule = new OptionalRule(prePadIfLikeBlock);
  }
}

export class OptionalPostPadIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const postPadIfLikeBlock: Rule = new PostPadIfLikeBlockRule();
    this.rule = new OptionalRule(postPadIfLikeBlock);
  }
}

export class PrePadIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const padding: Rule     = new PaddingRule();
    const ifLikeBlock: Rule = new IfLikeBlockRule();
    this.rule = new SequenceRule([padding, ifLikeBlock]);
  }
}

export class PostPadIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const ifLikeBlock: Rule = new IfLikeBlockRule();
    const padding: Rule     = new PaddingRule();
    this.rule = new SequenceRule([ifLikeBlock, padding]);
  }
}

export class OptionalPostPadInputContextRule extends SingleChildRule {
  public constructor() {
    super();
    const postPadInputContext: Rule = new PostPadInputContextRule();
    this.rule = new OptionalRule(postPadInputContext);
  }
}

export class PostPadInputContextRule extends SingleChildRule {
  public constructor() {
    super();
    const inputContext: Rule = new InputContextRule();
    const padding: Rule      = new PaddingRule();
    this.rule = new SequenceRule([inputContext, padding]);
  }
}

export class InputContextRule extends SingleChildRule {
  public constructor() {
    super();
    const inputElement           = new InputElementRule();
    const prePadInputElement     = new PrePadInputElementRule();
    const manyPrePadInputElement = new ManyRule(prePadInputElement);
    this.rule = new SequenceRule([inputElement, manyPrePadInputElement]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes        = tmp.removeChildrenOfType(NodeTypes.LINE);
      const inputContextNode = new ASTNode(NodeTypes.INPUT_CONTEXT);
      inputContextNode.addChildren(tmp.getChildren());
      inputContextNode.addChildren(lineNodes);
      node.addChild(inputContextNode);
    }
    return parseSuccess;
  }
}

export class PrePadInputElementRule extends SingleChildRule {
  public constructor() {
    super();
    const padding: Rule      = new PaddingRule();
    const inputElement: Rule = new InputElementRule();
    this.rule = new SequenceRule([padding, inputElement]);
  }
}

export class InputElementRule extends SingleChildRule {
  public constructor() {
    super();
    const any: Rule              = new AnyStatementRule();
    const notAny: Rule           = new NotAnyStatementRule();
    const deadKey: Rule          = new DeadKeyStatementRule();
    const contextStatement: Rule = new ContextStatementRule();
    const text: Rule             = new TextRule();
    this.rule = new AlternateRule([
      any, notAny, deadKey, contextStatement, text,
    ]);
  }
}

export class KeystrokeRule extends SingleChildRule {
  public constructor() {
    super();
    const any: Rule  = new AnyStatementRule();
    const text: Rule = new TextRule();
    this.rule = new AlternateRule([any, text]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const inputCharNode = new ASTNode(NodeTypes.KEYSTROKE);
      inputCharNode.addChild(tmp.getSoleChild());
      node.addChild(inputCharNode);
    }
    return parseSuccess;
  }
}

export class AnyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const any: Rule  = new TokenRule(TokenTypes.ANY, true);
    this.cmdNodeType = NodeTypes.ANY;
    this.rule = new SequenceRule([any, this.bracketedStoreName]);
  }
}

export class NotAnyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const notAny: Rule = new TokenRule(TokenTypes.NOTANY, true);
    this.cmdNodeType   = NodeTypes.NOTANY;
    this.rule = new SequenceRule([notAny, this.bracketedStoreName]);
  }
}

export class IfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const ifLikeStatement           = new IfLikeStatementRule();
    const prePadIfLikeStatement     = new PrePadIfLikeStatementRule();
    const manyPrePadIfLikeStatement = new ManyRule(prePadIfLikeStatement);
    this.rule = new SequenceRule([ifLikeStatement, manyPrePadIfLikeStatement]);
  }
}

export class PrePadIfLikeStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const padding: Rule         = new PaddingRule();
    const ifLikeStatement: Rule = new IfLikeStatementRule();
    this.rule = new SequenceRule([padding, ifLikeStatement]);
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
    const ifStoreStringStatement: Rule       = new IfStoreStringStatementRule();
    const ifSystemStoreStringStatement: Rule = new IfSystemStoreStringStatementRule();
    this.rule = new AlternateRule([ifStoreStringStatement, ifSystemStoreStringStatement]);
  }
}

export class AbstractIfStoreStatementRule extends SingleChildRule {
  protected ifRule: Rule;
  protected leftBracket: Rule;
  protected optWhitespace: Rule
  protected whitespace: Rule;
  protected comparison: Rule;
  protected rightBracket: Rule;

  public constructor() {
    super();
    this.ifRule        = new TokenRule(TokenTypes.IF, true);
    this.leftBracket   = new TokenRule(TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpaceRule();
    this.whitespace    = new TokenRule(TokenTypes.WHITESPACE);
    this.comparison    = new ComparisonRule();
    this.rightBracket  = new TokenRule(TokenTypes.RIGHT_BR);
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

export class IfStoreStringStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const variableStoreName: Rule = new VariableStoreNameRule();
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, this.optWhitespace, variableStoreName,
      this.whitespace, this.comparison, this.whitespace, stringRule,
      this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IfSystemStoreStringStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, this.optWhitespace, ifSystemStoreName,
      this.whitespace, this.comparison, this.whitespace, stringRule,
      this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IfSystemStoreNameRule extends SingleChildRule {
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

export class LayerStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const layer: Rule           = new TokenRule(TokenTypes.LAYER_SHORTCUT, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([layer, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const layerNode  = tmp.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      const stringNode = tmp.getSoleChildOfType(NodeTypes.STRING);
      layerNode.addChild(stringNode);
      node.addChild(layerNode);
    }
    return parseSuccess;
  }
}

export class PlatformStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const platform: Rule        = new TokenRule(TokenTypes.PLATFORM_SHORTCUT, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([platform, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const platformNode = tmp.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT);
      const stringNode   = tmp.getSoleChildOfType(NodeTypes.STRING);
      platformNode.addChild(stringNode);
      node.addChild(platformNode);
    }
    return parseSuccess;
  }
}

export class BaselayoutStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const baselayout: Rule      = new TokenRule(TokenTypes.BASELAYOUT_SHORTCUT, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([baselayout, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const baselayoutNode = tmp.getSoleChildOfType(NodeTypes.BASELAYOUT_SHORTCUT);
      const stringNode     = tmp.getSoleChildOfType(NodeTypes.STRING);
      baselayoutNode.addChild(stringNode);
      node.addChild(baselayoutNode);
    }
    return parseSuccess;
  }
}

export class BracketedStringRule extends SingleChildRule {
  public constructor() {
    super();
    const leftBracket: Rule   = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const string: Rule        = new TokenRule(TokenTypes.STRING, true);
    const rightBracket: Rule  = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      leftBracket, optWhitespace, string, optWhitespace, rightBracket,
    ]);
  }
}

export class RhsBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const contextOutputBlock: Rule = new ContextOutputBlockRule();
    const context: Rule            = new TokenRule(TokenTypes.CONTEXT, true);
    const returnRule: Rule         = new TokenRule(TokenTypes.RETURN, true);
    const nul: Rule                = new TokenRule(TokenTypes.NUL, true);
    this.rule = new AlternateRule([contextOutputBlock, context, returnRule, nul]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes = tmp.removeChildrenOfType(NodeTypes.LINE);
      const children  = tmp.getChildren();
      const rhsNode   = new ASTNode(NodeTypes.RHS);
      rhsNode.addChildren(children);
      rhsNode.addChildren(lineNodes);
      node.addChild(rhsNode);
    }
    return parseSuccess;
  }
}

export class ContextOutputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPostPadContext: Rule = new OptionalPostPadContextRule();
    const outputBlock: Rule       = new OutputBlockRule();
    this.rule = new SequenceRule([optPostPadContext, outputBlock]);
  }
}

export class OptionalPostPadContextRule extends SingleChildRule {
  public constructor() {
    super();
    const postPadContextRule: Rule = new PostPadContextRule();
    this.rule = new OptionalRule(postPadContextRule);
  }
}

export class PostPadContextRule extends SingleChildRule {
  public constructor() {
    super();
    const context: Rule = new TokenRule(TokenTypes.CONTEXT, true);
    const padding: Rule = new PaddingRule();
    this.rule = new SequenceRule([context, padding]);
  }
}

export class OutputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const outputStatement           = new OutputStatementRule();
    const prePadOutputStatement     = new PrePadOutputStatementRule();
    const manyPrePadOutputStatement = new ManyRule(prePadOutputStatement);
    this.rule = new SequenceRule([outputStatement, manyPrePadOutputStatement]);
  }
}

export class PrePadOutputStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const padding: Rule         = new PaddingRule();
    const outputStatement: Rule = new OutputStatementRule();
    this.rule = new SequenceRule([padding, outputStatement]);
  }
}

export class OutputStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const useStatement: Rule     = new UseStatementRule();
    const callStatement: Rule    = new CallStatementRule();
    const setStore: Rule         = new SetStoreRule();
    const saveStatement: Rule    = new SaveStatementRule();
    const resetStore: Rule       = new ResetStoreRule();
    const deadKeyStatement: Rule = new DeadKeyStatementRule();
    const setLayer: Rule         = new SetLayerRule();
    const layerStatement: Rule   = new LayerStatementRule();
    const indexStatement: Rule   = new IndexStatementRule();
    const contextStatement: Rule = new ContextStatementRule();
    const text: Rule             = new TextRule();
    const beep: Rule             = new TokenRule(TokenTypes.BEEP, true);
    this.rule = new AlternateRule([
      useStatement,
      callStatement,
      setStore,
      saveStatement,
      resetStore,
      deadKeyStatement,
      setLayer,
      layerStatement,
      indexStatement,
      contextStatement,
      text,
      beep,
    ]);
  }
}

export class CallStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const call: Rule = new TokenRule(TokenTypes.CALL, true);
    this.cmdNodeType = NodeTypes.CALL;
    this.rule = new SequenceRule([call, this.bracketedStoreName]);
  }
}

export class SaveStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const save: Rule = new TokenRule(TokenTypes.SAVE, true);
    this.cmdNodeType = NodeTypes.SAVE;
    this.rule = new SequenceRule([save, this.bracketedStoreName]);
  }
}

export class DeadKeyStatementRule extends AbstractBracketedStoreNameStatementRule {
  public constructor() {
    super();
    const deadKey: Rule = new TokenRule(TokenTypes.DEADKEY, true);
    this.cmdNodeType = NodeTypes.DEADKEY;
    this.rule = new SequenceRule([deadKey, this.bracketedStoreName]);
  }
}

export class IndexStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const index: Rule             = new TokenRule(TokenTypes.INDEX, true);
    const leftBracket: Rule       = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule();
    const variableStoreName: Rule = new VariableStoreNameRule();
    const spacedComma: Rule       = new SpacedCommaRule();
    const offset: Rule            = new OffsetRule();
    const rightBracket: Rule  = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      index,
      leftBracket,
      optWhitespace,
      variableStoreName,
      spacedComma,
      offset,
      optWhitespace,
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

export class SpacedCommaRule extends SingleChildRule {
  public constructor() {
    super();
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const comma: Rule         = new TokenRule(TokenTypes.COMMA);
    this.rule = new SequenceRule([optWhitespace, comma, optWhitespace]);
  }
}

export class OffsetRule extends SingleChildRule {
  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    const parameters: Token[]   = [];
    const parseSuccess: boolean = parameterSequence(parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.OFFSET, parameters[0]);
    }
    return parseSuccess;
  };
}

export class ContextStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const index: Rule             = new TokenRule(TokenTypes.CONTEXT, true);
    const leftBracket: Rule       = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule();
    const offset: Rule            = new OffsetRule();
    const rightBracket: Rule  = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      index,
      leftBracket,
      optWhitespace,
      offset,
      optWhitespace,
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
