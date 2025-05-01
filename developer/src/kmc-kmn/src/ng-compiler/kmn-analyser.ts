/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { Token, TokenTypes } from "./lexer.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule, SingleChildRule, parameterSequence, OneOrManyRule, ManyRule, AlternateTokenRule } from "./recursive-descent.js";
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
    const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
    const casedkeysStoreAssign: Rule    = new CasedkeysStoreAssignRule();
    const hotkeyStoreAssign: Rule       = new HotkeyStoreAssignRule();
    const variableStoreAssign: Rule     = new VariableStoreAssignRule();
    const ruleBlock: Rule               = new RuleBlockRule();
    this.rule = new AlternateRule([
      stringSystemStoreAssign,
      casedkeysStoreAssign,
      hotkeyStoreAssign,
      variableStoreAssign,
      ruleBlock,
    ]);
  }
}

export class StringSystemStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const stringSystemStore: Rule = new StringSystemStoreRule();
    const padding: Rule           = new PaddingRule();
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    this.rule = new SequenceRule([
      stringSystemStore, padding, stringRule,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const stringNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.STRING);
      const lineNode: ASTNode   = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const storeNode: ASTNode  = tmp.getSoleChild();
      storeNode.addChild(stringNode);
      storeNode.addChild(lineNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

abstract class AbstractSystemStoreRule extends SingleChildRule {
  protected store: Rule;
  protected leftBracket: Rule;
  protected optWhitespace: Rule;
  protected amphasand: Rule;
  protected rightBracket: Rule;

  public constructor() {
    super();
    this.store         = new TokenRule(TokenTypes.STORE);
    this.leftBracket   = new TokenRule(TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpaceRule();
    this.amphasand     = new TokenRule(TokenTypes.AMPHASAND);
    this.rightBracket  = new TokenRule(TokenTypes.RIGHT_BR);
  }
}

export class StringSystemStoreRule extends AbstractSystemStoreRule {
  public constructor() {
    super();
    const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
    this.rule = new SequenceRule([
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      stringSystemStoreName,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class StringSystemStoreNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.BITMAP,
      TokenTypes.COPYRIGHT,
      TokenTypes.DISPLAYMAP,
      TokenTypes.ETHNOLOGUECODE,
      TokenTypes.INCLUDECODES,
      TokenTypes.KEYBOARDVERSION,
      TokenTypes.KMW_EMBEDCSS,
      TokenTypes.KMW_EMBEDJS,
      TokenTypes.KMW_HELPFILE,
      TokenTypes.KMW_HELPTEXT,
      TokenTypes.KMW_RTL,
      TokenTypes.LANGUAGE,
      TokenTypes.LAYOUTFILE,
      TokenTypes.MESSAGE,
      TokenTypes.MNEMONICLAYOUT,
      TokenTypes.NAME,
      TokenTypes.TARGETS,
      TokenTypes.VERSION,
      TokenTypes.VISUALKEYBOARD,
      TokenTypes.WINDOWSLANGUAGES,
    ], true);
  }
}

export class CasedkeysStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const casedkeysStore: Rule      = new CasedkeysStoreRule();
    const paddedText: Rule          = new PaddedTextRule();
    const oneOrManyPaddedText: Rule = new OneOrManyRule(paddedText);
    this.rule = new SequenceRule([casedkeysStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const casedkeysNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.CASEDKEYS);
      const lineNodes: ASTNode[]   = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[]   = tmp.getChildren();
      casedkeysNode.addChildren(textNodes);
      casedkeysNode.addChildren(lineNodes);
      node.addChild(casedkeysNode);
    }
    return parseSuccess;
  }
}

export class CasedkeysStoreRule extends AbstractSystemStoreRule {
  public constructor() {
    super();
    const casedkeys: Rule = new TokenRule(TokenTypes.CASEDKEYS, true);
    this.rule = new SequenceRule([
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      casedkeys,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class HotkeyStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const hotkeyStore: Rule = new HotkeyStoreRule();
    const padding: Rule     = new PaddingRule();
    const virtualKey: Rule  = new VirtualKeyRule();
    this.rule = new SequenceRule([hotkeyStore, padding, virtualKey]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const hotkeyNode: ASTNode     = tmp.removeSoleChildOfType(NodeTypes.HOTKEY);
      const lineNode: ASTNode       = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const virtualKeyNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      hotkeyNode.addChild(virtualKeyNode);
      hotkeyNode.addChild(lineNode);
      node.addChild(hotkeyNode);
    }
    return parseSuccess;
  }
}

export class HotkeyStoreRule extends AbstractSystemStoreRule {
  public constructor() {
    super();
    const hotkey: Rule = new TokenRule(TokenTypes.HOTKEY, true);
    this.rule = new SequenceRule([
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      hotkey,
      this.optWhitespace,
      this.rightBracket,
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

export class VariableStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const variableStore: Rule       = new VariableStoreRule();
    const paddedText: Rule          = new PaddedTextRule();
    const oneOrManyPaddedText: Rule = new OneOrManyRule(paddedText);
    this.rule = new SequenceRule([variableStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode     = tmp.removeSoleChildOfType(NodeTypes.STORE);
      const lineNodes: ASTNode[]   = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[]   = tmp.getChildren();
      storeNode.addChildren(textNodes);
      storeNode.addChildren(lineNodes);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class PaddedTextRule extends SingleChildRule {
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
    const textRange: Rule     = new TextRangeRule();
    const simpleText: Rule    = new SimpleTextRule();
    const outsStatement: Rule = new OutsStatementRule();
    this.rule = new AlternateRule([
      textRange, simpleText, outsStatement,
    ]);
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
    const leftSquare: Rule          = new TokenRule(TokenTypes.LEFT_SQ);
    const optWhitespace: Rule       = new OptionalWhiteSpaceRule();
    const spacedShiftCode: Rule     = new SpacedShiftCodeRule();
    const manySpacedShiftCode: Rule = new ManyRule(spacedShiftCode);
    const keyCode: Rule             = new TokenRule(TokenTypes.KEY_CODE, true);
    const rightSquare: Rule          = new TokenRule(TokenTypes.RIGHT_SQ);
    this.rule = new SequenceRule([
      leftSquare, optWhitespace, manySpacedShiftCode, keyCode, optWhitespace, rightSquare
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

export class SpacedShiftCodeRule extends SingleChildRule {
  public constructor() {
    super();
    const shiftCode: Rule  = new TokenRule(TokenTypes.SHIFT_CODE, true);
    const whitespace: Rule = new TokenRule(TokenTypes.WHITESPACE);
    this.rule = new SequenceRule([shiftCode, whitespace]);
  }
}

export class VariableStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule              = new TokenRule(TokenTypes.STORE, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([store, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode     = tmp.getSoleChildOfType(NodeTypes.STORE);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      storeNode.addChild(storeNameNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class BracketedStoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const leftBracket: Rule       = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule();
    const variableStoreName: Rule = new VariableStoreNameRule();
    const rightBracket: Rule      = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      leftBracket, optWhitespace, variableStoreName, optWhitespace, rightBracket,
    ]);
  }
}

export class VariableStoreNameRule extends SingleChildRule {
  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    const parameters: Token[]   = [];
    const parseSuccess: boolean = parameterSequence(parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.STORENAME, parameters[0]);
    }
    return parseSuccess;
  };
}

export class OutsStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const outs: Rule               = new TokenRule(TokenTypes.OUTS, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([outs, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const outsNode      = tmp.getSoleChildOfType(NodeTypes.OUTS);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      outsNode.addChild(storeNameNode);
      node.addChild(outsNode);
    }
    return parseSuccess;
  }
}

export class RuleBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginBlock: Rule               = new BeginBlockRule();
    const groupBlock: Rule               = new GroupBlockRule();
    const usingKeysProductionBlock: Rule = new UsingKeysProductionBlockRule()
    const readOnlyProductionBlock: Rule  = new ReadOnlyProductionBlockRule()
    this.rule = new AlternateRule([
      beginBlock, groupBlock, usingKeysProductionBlock, readOnlyProductionBlock,
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

export class PermittedKeywordRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.NEWCONTEXT,
      TokenTypes.POSTKEYSTROKE,
    ], true);
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
    this.lhsNodeType          = NodeTypes.LHS_READONLY;
    const match               = new TokenRule(TokenTypes.MATCH, true);
    const noMatch             = new TokenRule(TokenTypes.NOMATCH, true);
    const readOnlyInputBlock  = new ReadOnlyInputBlockRule();
    const ifLikeBlock         = new IfLikeBlockRule();
    this.rule = new AlternateRule([
      match, noMatch, readOnlyInputBlock, ifLikeBlock,
    ]);
  }
}

export class UsingKeysInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPaddedIfLikeBlock: Rule  = new OptionalPaddedIfLikeBlockRule();
    const optPaddedInputContext: Rule = new OptionalPaddedInputContextRule();
    const plus: Rule                  = new TokenRule(TokenTypes.PLUS);
    const padding: Rule               = new PaddingRule();
    const keystoke: Rule              = new KeystrokeRule();
    this.rule = new SequenceRule([
      optPaddedIfLikeBlock,
      optPaddedInputContext,
      plus,
      padding,
      keystoke,
    ]);
  }
}

export class ReadOnlyInputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const optPaddedIfLikeBlock: Rule = new OptionalPaddedIfLikeBlockRule();
    const keystoke: Rule             = new KeystrokeRule();
    this.rule = new SequenceRule([optPaddedIfLikeBlock, keystoke]);
  }
}

export class OptionalPaddedIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const paddedIfLikeBlock: Rule = new PaddedIfLikeBlockRule();
    this.rule = new OptionalRule(paddedIfLikeBlock);
  }
}

export class PaddedIfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const ifLikeBlock: Rule = new IfLikeBlockRule();
    const padding: Rule     = new PaddingRule();
    this.rule = new SequenceRule([ifLikeBlock, padding]);
  }
}

export class OptionalPaddedInputContextRule extends SingleChildRule {
  public constructor() {
    super();
    const paddedInputContext: Rule = new PaddedInputContextRule();
    this.rule = new OptionalRule(paddedInputContext);
  }
}

export class PaddedInputContextRule extends SingleChildRule {
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
    const paddedInputElement     = new PaddedInputElementRule();
    const manyPaddedInputElement = new ManyRule(paddedInputElement);
    this.rule = new SequenceRule([inputElement, manyPaddedInputElement]);
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

export class PaddedInputElementRule extends SingleChildRule {
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
    const any: Rule  = new AnyStatementRule();
    const text: Rule = new TextRule();
    this.rule = new AlternateRule([any, text]);
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

export class AnyStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const any: Rule                = new TokenRule(TokenTypes.ANY, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([any, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const anyNode       = tmp.getSoleChildOfType(NodeTypes.ANY);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      anyNode.addChild(storeNameNode);
      node.addChild(anyNode);
    }
    return parseSuccess;
  }
}

export class IfLikeBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const ifLikeStatement           = new IfLikeStatementRule();
    const paddedIfLikeStatement     = new PaddedIfLikeStatementRule();
    const manyPaddedIfLikeStatement = new ManyRule(paddedIfLikeStatement);
    this.rule = new SequenceRule([ifLikeStatement, manyPaddedIfLikeStatement]);
  }
}

export class PaddedIfLikeStatementRule extends SingleChildRule {
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
    const ifStoreStoreStatement: Rule        = new IfStoreStoreStatementRule();
    const ifStoreStringStatement: Rule       = new IfStoreStringStatementRule();
    const ifSystemStoreStoreStatement: Rule  = new IfSystemStoreStoreStatementRule();
    const ifSystemStoreStringStatement: Rule = new IfSystemStoreStringStatementRule();
    this.rule = new AlternateRule([
      ifStoreStoreStatement, ifStoreStringStatement,
      ifSystemStoreStoreStatement, ifSystemStoreStringStatement,
    ]);
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

export class IfStoreStoreStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const variableStoreName: Rule = new VariableStoreNameRule();
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, this.optWhitespace, variableStoreName,
      this.whitespace, this.comparison, this.whitespace, variableStoreName,
      this.optWhitespace, this.rightBracket,
    ]);
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

export class IfSystemStoreStoreStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const amphasand: Rule         = new TokenRule(TokenTypes.AMPHASAND);
    const systemStoreName: Rule   = new SystemStoreNameRule();
    const variableStoreName: Rule = new VariableStoreNameRule();
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, this.optWhitespace, amphasand,
      systemStoreName, this.whitespace, this.comparison, this.whitespace,
      variableStoreName, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IfSystemStoreStringStatementRule extends AbstractIfStoreStatementRule {
  public constructor() {
    super();
    const amphasand: Rule         = new TokenRule(TokenTypes.AMPHASAND);
    const systemStoreName: Rule   = new SystemStoreNameRule();
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    this.rule = new SequenceRule([
      this.ifRule, this.leftBracket, this.optWhitespace, amphasand,
      systemStoreName, this.whitespace, this.comparison, this.whitespace,
      stringRule, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IfSystemStoreNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.PLATFORM,
      TokenTypes.LAYER,
      TokenTypes.BASELAYOUT,
      TokenTypes.NEWLAYER,
      TokenTypes.OLDLAYER,
    ], true);
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

export class SystemStoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
    const casedkeys: Rule             = new TokenRule(TokenTypes.CASEDKEYS, true);
    const hotkey: Rule                = new TokenRule(TokenTypes.HOTKEY, true);
    const layer: Rule                 = new TokenRule(TokenTypes.LAYER, true);
    const newLayer: Rule              = new TokenRule(TokenTypes.NEWLAYER, true);
    const oldLayer: Rule              = new TokenRule(TokenTypes.OLDLAYER, true);
    this.rule = new AlternateRule([
      stringSystemStoreName, casedkeys, hotkey, layer, newLayer, oldLayer,
    ]);
  }
}

export class LayerStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const layer: Rule           = new TokenRule(TokenTypes.LAYER, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([layer, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const layerNode  = tmp.getSoleChildOfType(NodeTypes.LAYER);
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
    const platform: Rule        = new TokenRule(TokenTypes.PLATFORM, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([platform, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const platformNode = tmp.getSoleChildOfType(NodeTypes.PLATFORM);
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
    const baselayout: Rule      = new TokenRule(TokenTypes.BASELAYOUT, true);
    const bracketedString: Rule = new BracketedStringRule();
    this.rule = new SequenceRule([baselayout, bracketedString]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const baselayoutNode = tmp.getSoleChildOfType(NodeTypes.BASELAYOUT);
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
    this.rule = new AlternateRule([contextOutputBlock, context]);
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
    const optPaddedContext: Rule = new OptionalPaddedContextRule();
    const outputBlock: Rule      = new OutputBlockRule();
    this.rule = new SequenceRule([optPaddedContext, outputBlock]);
  }
}

export class OptionalPaddedContextRule extends SingleChildRule {
  public constructor() {
    super();
    const paddedContextRule: Rule = new PaddedContextRule();
    this.rule = new OptionalRule(paddedContextRule);
  }
}

export class PaddedContextRule extends SingleChildRule {
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
    const paddedOutputStatement     = new PaddedOutputStatementRule();
    const manyPaddedOutputStatement = new ManyRule(paddedOutputStatement);
    this.rule = new SequenceRule([outputStatement, manyPaddedOutputStatement]);
  }
}

export class PaddedOutputStatementRule extends SingleChildRule {
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
    const useStatement: Rule   = new UseStatementRule();
    const layerStatement: Rule = new LayerStatementRule();
    const indexStatement: Rule = new IndexStatementRule();
    const text: Rule           = new TextRule();
    const beep: Rule           = new TokenRule(TokenTypes.BEEP, true);
    this.rule = new AlternateRule([
      useStatement, layerStatement, indexStatement, text, beep,
    ]);
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
