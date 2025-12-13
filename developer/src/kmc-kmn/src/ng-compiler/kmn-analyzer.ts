/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenType } from "./token-type.js";
import { AlternateRule, AlternateTokenRule, ManyRule, OneOrManyRule, OptionalRule, SingleChildRuleWithASTStrategy } from "./recursive-descent.js";
import { Rule, SequenceRule, SingleChildRule } from "./recursive-descent.js";
import { TokenRule } from "./recursive-descent.js";
import { AnyStatementRule, CallStatementRule, ContextStatementRule, DeadkeyStatementRule, IfLikeStatementRule } from "./statement-analyzer.js";
import { IndexStatementRule, LayerStatementRule, NotanyStatementRule, OutsStatementRule, SaveStatementRule } from "./statement-analyzer.js";
import { CapsAlwaysOffRule, CapsOnOnlyRule, HeaderAssignRule, NormalStoreAssignRule, ResetStoreRule } from "./store-analyzer.js";
import { SetNormalStoreRule, SetSystemStoreRule, ShiftFreesCapsRule, SystemStoreAssignRule } from "./store-analyzer.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTStrategy, GivenNode, NewNode, NewNodeOrTree } from "./ast-strategy.js";

/**
 * The Next Generation Parser for the Keyman Keyboard Language.
 *
 * The Parser builds an Abstract Syntax Tree from the supplied TokenBuffer.
 *
 */
export class Parser {
  /**
   * Construct a Parser
   *
   * @param tokenBuffer 
   */
  public constructor(
    /** the TokenBuffer to parse */
    private readonly tokenBuffer: TokenBuffer
  ) {
  }

  /**
   * Parses the tokenBuffer to create an abstract syntax
   * tree (AST) of a KMN file.
   *
   * @returns the abstract syntax tree (AST)
   */
  public parse(): ASTNode {
    const kmnTreeRule: Rule = new KmnTreeRule();
    const root: ASTNode = new ASTNode(NodeType.TMP);
    // TODO-NG-COMPILER: fatal error if parse returns false
    return kmnTreeRule.parse(this.tokenBuffer, root) ? root : null;
  }
}

/**
 * (BNF) kmnTree: line*
 */
export class KmnTreeRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new KmnTreeStrategy());
    const line: Rule = new LineRule();
    this.rule = new ManyRule(line);
  }
}

/**
 * An ASTStrategy that rebuilds the KMN tree after the initial parse
 */
export class KmnTreeStrategy extends ASTStrategy {
  /**
   * Rebuilds the tree by gathering the source code, groups and
   * stores and arranging these into stores, other nodes,
   * groups and then source code
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the first node found
   */
  public apply(node: ASTNode): ASTNode {
    const children: ASTNode[] = [];
    const sourceCodeNode: ASTNode = this.gatherSourceCode(node);
    const groupNodes: ASTNode[]   = this.gatherGroups(node);
    const storesNode: ASTNode     = this.gatherStores(node);
    children.unshift(storesNode);
    children.push(...node.removeChildren());
    children.push(...groupNodes);
    children.push(sourceCodeNode);
    node.addChildren(children);
    return node;
  };

  private static STORES_NODETYPES = [
    NodeType.BITMAP,
    NodeType.CASEDKEYS,
    NodeType.COPYRIGHT,
    NodeType.DISPLAYMAP,
    NodeType.ETHNOLOGUECODE,
    NodeType.HOTKEY,
    NodeType.INCLUDECODES,
    NodeType.KEYBOARDVERSION,
    NodeType.KMW_EMBEDCSS,
    NodeType.KMW_EMBEDJS,
    NodeType.KMW_HELPFILE,
    NodeType.KMW_HELPTEXT,
    NodeType.KMW_RTL,
    NodeType.LANGUAGE,
    NodeType.LAYOUTFILE,
    NodeType.MESSAGE,
    NodeType.MNEMONICLAYOUT,
    NodeType.NAME,
    NodeType.TARGETS,
    NodeType.VERSION,
    NodeType.VISUALKEYBOARD,
    NodeType.WINDOWSLANGUAGES,
    NodeType.CAPSALWAYSOFF,
    NodeType.CAPSONONLY,
    NodeType.SHIFTFREESCAPS,
    NodeType.BITMAP_HEADER,
    NodeType.COPYRIGHT_HEADER,
    NodeType.HOTKEY_HEADER,
    NodeType.LANGUAGE_HEADER,
    NodeType.LAYOUT_HEADER,
    NodeType.MESSAGE_HEADER,
    NodeType.NAME_HEADER,
    NodeType.VERSION_HEADER,
    NodeType.STORE,
  ];

  /**
   * Removes the all store nodes (i.e. matching STORES_NODETYPES) from
   * the abstract syntax tree (AST) and returns them
   *
   * @param node the abstract syntax tree (AST)
   * @returns the stores nodes removed from the tree
   */
  private gatherStores(node: ASTNode): ASTNode {
    const storeNodes: ASTNode[] = node.removeChildrenOfTypes(KmnTreeStrategy.STORES_NODETYPES);
    const storesNode: ASTNode   = new ASTNode(NodeType.STORES);
    storesNode.addChildren(storeNodes);
    return storesNode;
  }

  /**
   * Removes all groups from the abstract syntax tree (AST) and returns them
   *
   * @param node the abstract syntax tree (AST)
   * @returns the groups removed from the tree
   */
  private gatherGroups(node: ASTNode): ASTNode[] {
    return node.removeBlocks(NodeType.GROUP, NodeType.PRODUCTION);
  }

  /**
   * Removes all source code nodes (i.e LINE nodes) from the
   * abstract syntax tree (AST) and returns them as a tree
   * rooted at a SOURCE_CODE node
   *
   * @param node the abstract syntax tree (AST)
   * @returns the removed source code nodes as a SOURCE_CODE tree
   */
  private gatherSourceCode(node: ASTNode): ASTNode {
    const lineNodes: ASTNode[]    = node.removeChildrenOfType(NodeType.LINE);
    const sourceCodeNode: ASTNode = new ASTNode(NodeType.SOURCE_CODE);
    sourceCodeNode.addChildren(lineNodes);
    return sourceCodeNode;
  }
}

/**
 * (BNF) line: compileTarget? content? NEWLINE
 */
export class LineRule extends SingleChildRule {
  public constructor() {
    super();
    const compileTarget: Rule    = new CompileTargetRule();
    const optCompileTarget: Rule = new OptionalRule(compileTarget);
    const content: Rule          = new ContentRule();
    const optContent: Rule       = new OptionalRule(content);
    const newline: Rule          = new TokenRule(TokenType.NEWLINE, true);
    this.rule = new SequenceRule([optCompileTarget, optContent, newline]);
  }
}

/**
 * (BNF) compileTarget: KEYMAN|KEYMANONLY|KEYMANWEB|KMFL|WEAVER
 *
 * https://help.keyman.com/developer/language/guide/compile-targets
 */
export class CompileTargetRule extends AlternateTokenRule {
  // TODO-NG-COMPILER: warning/error for compile targets
  public constructor() {
    super([
      TokenType.KEYMAN,
      TokenType.KEYMANONLY,
      TokenType.KEYMANWEB,
      TokenType.KMFL,
      TokenType.WEAVER,
    ], true);
  }
}

/**
 * (BNF) content: systemStoreAssign|capsAlwaysOff|capsOnOnly|shiftFreesCaps|
 * headerAssign|normalStoreAssign|ruleBlock
 */
export class ContentRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreAssign: Rule  = new SystemStoreAssignRule();
    const capsAlwaysOff: Rule      = new CapsAlwaysOffRule();
    const capsOnOnly: Rule         = new CapsOnOnlyRule();
    const shiftFreesCaps: Rule     = new ShiftFreesCapsRule();
    const headerAssign: Rule       = new HeaderAssignRule();
    const normalStoreAssign: Rule  = new NormalStoreAssignRule();
    const ruleBlock: Rule          = new RuleBlockRule();
    this.rule = new AlternateRule([
      systemStoreAssign,
      capsAlwaysOff,
      capsOnOnly,
      shiftFreesCaps,
      headerAssign,
      normalStoreAssign,
      ruleBlock,
    ]);
  }
}

/**
 * (BNF) text: plainText|outsStatement
 */
export class TextRule extends SingleChildRule {
  public constructor() {
    super();
    const plainText: Rule     = new PlainTextRule();
    const outsStatement: Rule = new OutsStatementRule();
    this.rule = new AlternateRule([plainText, outsStatement]);
  }
}

/**
 * (BNF) plainText: textRange|simpleText
 */
export class PlainTextRule extends SingleChildRule {
  public constructor() {
    super();
    const textRange: Rule  = new TextRangeRule();
    const simpleText: Rule = new SimpleTextRule();
    this.rule = new AlternateRule([textRange, simpleText]);
  }
}

/**
 * (BNF) simpleText: STRING|virtualKey|U_CHAR|NAMED_CONSTANT|HANGUL|
 * DECIMAL|HEXADECIMAL|OCTAL|NUL|deadkeyStatement|BEEP
 *
 * https://help.keyman.com/developer/language/guide/strings
 * https://help.keyman.com/developer/language/guide/unicode
 * https://help.keyman.com/developer/language/guide/constants
 * https://help.keyman.com/developer/language/reference/_nul
 * https://help.keyman.com/developer/language/reference/beep
 */
export class SimpleTextRule extends SingleChildRule {
  // TODO-NG-COMPILER: warning/error for DECIMAL, HEXADECIMAL and OCTAL
  public constructor() {
    super();
    const stringRule: Rule       = new TokenRule(TokenType.STRING, true);
    const virtualKey: Rule       = new VirtualKeyRule();
    const uChar: Rule            = new TokenRule(TokenType.U_CHAR, true);
    const namedConstant: Rule    = new TokenRule(TokenType.NAMED_CONSTANT, true);
    const hangul: Rule           = new TokenRule(TokenType.HANGUL, true);
    const decimal: Rule          = new TokenRule(TokenType.DECIMAL, true);
    const hexadecimal: Rule      = new TokenRule(TokenType.HEXADECIMAL, true);
    const octal: Rule            = new TokenRule(TokenType.OCTAL, true);
    const nul: Rule              = new TokenRule(TokenType.NUL, true);
    const deadkeyStatement: Rule = new DeadkeyStatementRule();
    const beep: Rule             = new TokenRule(TokenType.BEEP, true);
    this.rule = new AlternateRule([
      stringRule,
      virtualKey,
      uChar,
      namedConstant,
      hangul,
      decimal,
      hexadecimal,
      octal,
      nul,
      deadkeyStatement,
      beep,
    ]);
  }
}

/**
 * (BNF) textRange: simpleText rangeEnd+
 *
 * https://help.keyman.com/developer/language/guide/expansions
 */
export class TextRangeRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.RANGE));
    const simpleText: Rule        = new SimpleTextRule();
    const rangeEnd: Rule          = new RangeEndRule();
    const oneOrManyRangeEnd: Rule = new OneOrManyRule(rangeEnd);
    this.rule = new SequenceRule([simpleText, oneOrManyRangeEnd]);
  }
}

/**
 * (BNF) rangeEnd: RANGE simpleText
 *
 * https://help.keyman.com/developer/language/guide/expansions
 */
export class RangeEndRule extends SingleChildRule {
  public constructor() {
    super();
    const range: Rule         = new TokenRule(TokenType.RANGE);
    const simpleText: Rule    = new SimpleTextRule();
    this.rule = new SequenceRule([range, simpleText]);
  }
}

/**
 * (BNF) virtualKey: LEFT_SQ modifier* keyCode RIGHT_SQ
 *
 * https://help.keyman.com/developer/language/guide/virtual-keys
 */
export class VirtualKeyRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.VIRTUAL_KEY));
    const leftSquare: Rule   = new TokenRule(TokenType.LEFT_SQ);
    const modifier: Rule     = new ModifierRule();
    const manyModifier: Rule = new ManyRule(modifier);
    const keyCode: Rule      = new KeyCodeRule();
    const rightSquare: Rule  = new TokenRule(TokenType.RIGHT_SQ);
    this.rule = new SequenceRule([
      leftSquare, manyModifier, keyCode, rightSquare
    ]);
  }
}

/**
 * (BNF) modifier: SHIFT|CAPS|MODIFIER
 *
 * https://help.keyman.com/developer/language/guide/virtual-keys
 *
 * SHIFT and CAPS are distinct from other modifiers because
 * they are also used in 'CAPS ALWAYS OFF', 'CAPS ON ONLY' and
 * 'SHIFT FREES CAPS'
 */
export class ModifierRule extends SingleChildRule {
  public constructor() {
    super();
    const shift: Rule    = new TokenRule(TokenType.SHIFT, true);
    const caps: Rule     = new TokenRule(TokenType.CAPS, true);
    const modifier: Rule = new TokenRule(TokenType.MODIFIER, true);
    this.rule = new AlternateRule([shift, caps, modifier]);
  }

  /**
   * Parse a ModifierRule.  As SHIFT and CAPS are separately
   * identified as they are used in other rules, a MODIFIER
   * node must be created for them if needed.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      let modifierNode = tmp.getSoleChildOfType(NodeType.MODIFIER);
      if (modifierNode === null) {
        modifierNode = new ASTNode(NodeType.MODIFIER, tmp.getSoleChild().token);
      }
      node.addChild(modifierNode);
    }
    return parseSuccess;
  }
}

/**
 * (BNF) keyCode: KEY_CODE|STRING|DECIMAL
 *
 * https://help.keyman.com/developer/language/guide/virtual-keys
 * https://help.keyman.com/developer/language/guide/strings
 *
 * DECIMAL is included because of e.g. d10, which could be ISO9995 code
 */
export class KeyCodeRule extends SingleChildRule {
  public constructor() {
    super();
    const keyCode: Rule    = new TokenRule(TokenType.KEY_CODE, true);
    const stringRule: Rule = new TokenRule(TokenType.STRING, true);
    const decimal: Rule    = new TokenRule(TokenType.DECIMAL, true);
    this.rule = new AlternateRule([keyCode, stringRule, decimal]);
  }
}

/**
 * (BNF) ruleBlock: beginStatement|groupStatement|productionBlock
 */
export class RuleBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginStatement: Rule  = new BeginStatementRule();
    const groupStatement: Rule  = new GroupStatementRule();
    const productionBlock: Rule = new ProductionBlockRule();
    this.rule = new AlternateRule([beginStatement, groupStatement, productionBlock]);
  }
}

/**
 * (BNF) beginStatement: BEGIN entryPoint? CHEVRON useStatement
 *
 * https://help.keyman.com/developer/language/reference/begin
 */
export class BeginStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.BEGIN));
    const begin: Rule          = new TokenRule(TokenType.BEGIN, true);
    const entryPointRule: Rule = new EntryPointRule();
    const optEntryPoint: Rule  = new OptionalRule(entryPointRule);
    const chevron: Rule        = new TokenRule(TokenType.CHEVRON);
    const useStatement: Rule   = new UseStatementRule();
    this.rule = new SequenceRule([begin, optEntryPoint, chevron, useStatement]);
  }
}

/**
 * (BNF) entryPoint: UNICODE|NEWCONTEXT|POSTKEYSTROKE|ANSI
 *
 * https://help.keyman.com/developer/language/reference/begin
 */
export class EntryPointRule extends SingleChildRule {
  public constructor() {
    super();
    const unicode: Rule       = new TokenRule(TokenType.UNICODE, true);
    const newcontext: Rule    = new TokenRule(TokenType.NEWCONTEXT, true);
    const postkeystroke: Rule = new TokenRule(TokenType.POSTKEYSTROKE, true);
    const ansi: Rule          = new TokenRule(TokenType.ANSI, true);
    this.rule = new AlternateRule([unicode, newcontext, postkeystroke, ansi]);
  }
}

/**
 * (BNF) useStatement: USE LEFT_BR groupName RIGHT_BR
 *
 * https://help.keyman.com/developer/language/reference/use
 */
export class UseStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.USE));
    const use: Rule          = new TokenRule(TokenType.USE, true);
    const leftBracket: Rule  = new TokenRule(TokenType.LEFT_BR);
    const groupName: Rule    = new GroupNameRule();
    const rightBracket: Rule = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([use, leftBracket, groupName, rightBracket]);
  }
}

/**
 * (BNF) groupStatement: GROUP LEFT_BR groupName RIGHT_BR groupQualifier?
 *
 * https://help.keyman.com/developer/language/reference/group
 */
export class GroupStatementRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.GROUP));
    const group: Rule              = new TokenRule(TokenType.GROUP, true);
    const leftBracket: Rule        = new TokenRule(TokenType.LEFT_BR);
    const groupName: Rule          = new GroupNameRule();
    const rightBracket: Rule       = new TokenRule(TokenType.RIGHT_BR);
    const groupQualifierRule: Rule = new GroupQualifierRule();
    const optGroupQualifier: Rule  = new OptionalRule(groupQualifierRule);
    this.rule = new SequenceRule([
      group,
      leftBracket,
      groupName,
      rightBracket,
      optGroupQualifier,
    ]);
  }
}

/**
 * (BNF) groupName: groupNameElement+
 *
 * https://help.keyman.com/developer/language/reference/group
 */
export class GroupNameRule extends SingleChildRuleWithASTStrategy {
  // TODO-NG-COMPILER: warning/error if group name consists of multiple elements
  public constructor() {
    super(new NewNodeOrTree(NodeType.GROUPNAME));
    const groupNameElement: Rule = new GroupNameElementRule();
    this.rule = new OneOrManyRule(groupNameElement);
  }
}

/**
 * (BNF) groupNameElement: PARAMETER|OCTAL|permittedKeyword
 *
 * https://help.keyman.com/developer/language/reference/group
 *
 * OCTAL and permitted keywords are included as these could be
 * valid group name elements
 */
export class GroupNameElementRule extends SingleChildRule {
  public constructor() {
    super();
    const parameter: Rule        = new TokenRule(TokenType.PARAMETER, true);
    const octal: Rule            = new TokenRule(TokenType.OCTAL, true);
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([parameter, octal, permittedKeyword]);
  }
}

/**
 * (BNF) see the BNF file (kmn-file.bnf)
 *
 * These keywords may all be valid normal store, deadkey or group name elements
 */
export class PermittedKeywordRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenType.ALWAYS,
      TokenType.ANSI,
      TokenType.BEEP,
      TokenType.BEGIN,
      TokenType.BITMAP_HEADER,
      TokenType.CAPS,
      TokenType.CONTEXT,
      TokenType.COPYRIGHT_HEADER,
      TokenType.DECIMAL,
      TokenType.FREES,
      TokenType.HEXADECIMAL,
      TokenType.HOTKEY_HEADER,
      TokenType.KEY_CODE,
      TokenType.KEYS,
      TokenType.LANGUAGE_HEADER,
      TokenType.LAYOUT_HEADER,
      TokenType.MATCH,
      TokenType.MESSAGE_HEADER,
      TokenType.NAME_HEADER,
      TokenType.NEWCONTEXT,
      TokenType.NOMATCH,
      TokenType.NUL,
      TokenType.OCTAL,
      TokenType.OFF,
      TokenType.ON,
      TokenType.ONLY,
      TokenType.POSTKEYSTROKE,
      TokenType.READONLY,
      TokenType.RETURN,
      TokenType.SHIFT,
      TokenType.UNICODE,
      TokenType.USING,
      TokenType.VERSION_HEADER,
    ], true);
  }
}

/**
 * (BNF) groupQualifier: usingKeys|READONLY
 *
 * https://help.keyman.com/developer/language/reference/group
 */
export class GroupQualifierRule extends SingleChildRule {
  public constructor() {
    super();
    const usingKeys: Rule = new UsingKeysRule();
    const readonly: Rule  = new TokenRule(TokenType.READONLY, true);
    this.rule = new AlternateRule([usingKeys, readonly]);
  }
}

/**
 * (BNF) usingKeys: USING KEYS
 *
 * https://help.keyman.com/developer/language/reference/group
 */
export class UsingKeysRule extends SingleChildRule {
  public constructor() {
    super();
    const using: Rule = new TokenRule(TokenType.USING);
    const keys: Rule  = new TokenRule(TokenType.KEYS);
    this.rule = new SequenceRule([using, keys]);
  }

  /**
   * Parse a UsingKeysRule
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      node.addChild(new ASTNode(NodeType.USING_KEYS));
    }
    return parseSuccess;
  }
}

/**
 * (BNF) productionBlock: lhsBlock CHEVRON rhsBlock
 *
 * https://help.keyman.com/developer/language/guide/rules
 */
export class ProductionBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const lhsBlock = new LhsBlockRule();
    const chevron  = new TokenRule(TokenType.CHEVRON);
    const rhsBlock = new RhsBlockRule();
    this.rule = new SequenceRule([lhsBlock, chevron, rhsBlock]);
  }

  /**
   * Parse a ProductionBlockRule
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      const lhsNode        = tmp.getSoleChildOfType(NodeType.LHS);
      const rhsNode        = tmp.getSoleChildOfType(NodeType.RHS);
      const productionNode = new ASTNode(NodeType.PRODUCTION);
      productionNode.addChild(lhsNode);
      productionNode.addChild(rhsNode);
      node.addChild(productionNode);
    }
    return parseSuccess;
  }
}

/**
 * (BNF) lhsBlock: MATCH|NOMATCH|inputBlock
 *
 * https://help.keyman.com/developer/language/reference/match
 * https://help.keyman.com/developer/language/reference/nomatch
 */
export class LhsBlockRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.LHS));
    const match: Rule      = new TokenRule(TokenType.MATCH, true);
    const nomatch: Rule    = new TokenRule(TokenType.NOMATCH, true);
    const inputBlock: Rule = new InputBlockRule();
    this.rule = new AlternateRule([match, nomatch,inputBlock]);
  }
}

/**
 * (BNF) inputBlock: NUL? ifLikeStatement* inputContext? keystroke?
 *
 * https://help.keyman.com/developer/language/reference/_nul
 */
export class InputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const nulRule: Rule             = new TokenRule(TokenType.NUL, true);
    const optNul: Rule              = new OptionalRule(nulRule);
    const ifLikeStatement: Rule     = new IfLikeStatementRule();
    const manyIfLikeStatement: Rule = new ManyRule(ifLikeStatement);
    const inputContext: Rule        = new InputContextRule();
    const optInputContext: Rule     = new OptionalRule(inputContext);
    const keystoke: Rule            = new KeystrokeRule();
    const optKeystroke: Rule        = new OptionalRule(keystoke);
    this.rule = new SequenceRule([
      optNul, manyIfLikeStatement, optInputContext, optKeystroke,
    ]);
  }
}

/**
 * (BNF) inputContext: inputElement+
 */
export class InputContextRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.INPUT_CONTEXT));
    const inputElement = new InputElementRule();
    this.rule = new OneOrManyRule(inputElement);
  }
}

/**
 * (BNF) inputElement: anyStatement|notanyStatement|contextStatement|indexStatement|text
 */
export class InputElementRule extends SingleChildRule {
  public constructor() {
    super();
    const anyStatement: Rule     = new AnyStatementRule();
    const notanyStatement: Rule  = new NotanyStatementRule();
    const contextStatement: Rule = new ContextStatementRule();
    const indexStatement: Rule   = new IndexStatementRule();
    const text: Rule             = new TextRule();
    this.rule = new AlternateRule([
      anyStatement,
      notanyStatement,
      contextStatement,
      indexStatement,
      text
    ]);
  }
}

/**
 * (BNF) keystroke: PLUS inputElement+
 */
export class KeystrokeRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.KEYSTROKE));
    const plus: Rule            = new TokenRule(TokenType.PLUS);
    const inputElement          = new InputElementRule();
    const oneOrManyInputElement = new OneOrManyRule(inputElement);
    this.rule = new SequenceRule([plus, oneOrManyInputElement]);
  }
}

/**
 * (BNF) rhsBlock: outputStatement+
 */
export class RhsBlockRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new NewNode(NodeType.RHS));
    const outputStatement: Rule = new OutputStatementRule();
    this.rule = new OneOrManyRule(outputStatement);
  }
}

/**
 * (BNF) outputStatement: useStatement|callStatement|setNormalStore|saveStatement|
 * resetStore|setSystemStore|layerStatement|indexStatement|
 * contextStatement|CONTEXT|RETURN|text|BEEP
 */
export class OutputStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const useStatement: Rule     = new UseStatementRule();
    const callStatement: Rule    = new CallStatementRule();
    const setNormalStore: Rule   = new SetNormalStoreRule();
    const saveStatement: Rule    = new SaveStatementRule();
    const resetStore: Rule       = new ResetStoreRule();
    const setSystemStore: Rule   = new SetSystemStoreRule();
    const layerStatement: Rule   = new LayerStatementRule();
    const indexStatement: Rule   = new IndexStatementRule();
    const contextStatement: Rule = new ContextStatementRule();
    const context: Rule          = new TokenRule(TokenType.CONTEXT, true);
    const returnRule: Rule       = new TokenRule(TokenType.RETURN, true);
    const text: Rule             = new TextRule();
    const beep: Rule             = new TokenRule(TokenType.BEEP, true);
    this.rule = new AlternateRule([
      useStatement,
      callStatement,
      setNormalStore,
      saveStatement,
      resetStore,
      setSystemStore,
      layerStatement,
      indexStatement,
      contextStatement,
      context,
      returnRule,
      text,
      beep,
    ]);
  }
}
