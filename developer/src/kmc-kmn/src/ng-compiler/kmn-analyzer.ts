/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenType } from "./token-type.js";
import { AlternateRule, AlternateTokenRule, ManyRule, OneOrManyRule, OptionalRule } from "./recursive-descent.js";
import { SingleChildRuleWithASTRebuild, SequenceRule, SingleChildRule } from "./recursive-descent.js";
import { TokenRule } from "./recursive-descent.js";
import { AnyStatementRule, CallStatementRule, ContextStatementRule, DeadkeyStatementRule, IfLikeStatementRule } from "./statement-analyzer.js";
import { IndexStatementRule, LayerStatementRule, NotanyStatementRule, OutsStatementRule, SaveStatementRule } from "./statement-analyzer.js";
import { CapsLockHeaderRule, HeaderAssignRule, NormalStoreAssignRule, ResetStoreRule } from "./store-analyzer.js";
import { SetNormalStoreRule, SetSystemStoreRule, SystemStoreAssignRule } from "./store-analyzer.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTRebuild, GivenNode, NewNode, NewNodeOrTree } from "./ast-rebuild.js";

/**
 * The Next Generation Parser for the Keyman Keyboard Language.
 *
 * The Parser builds an Abstract Syntax Tree from the supplied TokenBuffer.
 */
export class Parser {
  /**
   * Construct a Parser
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
    const kmnTreeRule = new KmnTreeRule();
    const root        = new ASTNode(NodeType.ROOT);
    // TODO-NG-COMPILER: fatal error if parse returns false
    kmnTreeRule.parse(this.tokenBuffer, root);
    return root;
  }
}

/**
 * (BNF) kmnTree: line*
 *
 * Uses a KmnTreeRebuild to rebuild the KMN tree after the initial parse
 */
export class KmnTreeRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new KmnTreeRebuild());
    const line = new LineRule();
    this.rule  = new ManyRule(line);
  }
}

/**
 * Rebuilds the KMN tree after the initial parse
 */
export class KmnTreeRebuild extends ASTRebuild {
  /**
   * Rebuilds the tree by gathering the source code, groups and
   * stores and arranging these into stores, other nodes,
   * groups and then source code
   *
   * @param node the tree to be rebuilt
   * @returns the rebuilt tree, rooted at the first node found
   */
  public apply(node: ASTNode): ASTNode {
    const sourceCodeNode = this.gatherSourceCode(node);
    const groupNodes     = this.gatherGroups(node);
    const storesNode     = this.gatherStores(node);
    node.addChildren([storesNode, ...node.removeChildren(), ...groupNodes, sourceCodeNode]);
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
    const storeNodes = node.removeChildrenOfTypes(KmnTreeRebuild.STORES_NODETYPES);
    const storesNode = new ASTNode(NodeType.STORES);
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
    const lineNodes      = node.removeChildrenOfType(NodeType.LINE);
    const sourceCodeNode = new ASTNode(NodeType.SOURCE_CODE);
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
    const compileTarget    = new CompileTargetRule();
    const optCompileTarget = new OptionalRule(compileTarget);
    const content          = new ContentRule();
    const optContent       = new OptionalRule(content);
    const newline          = new TokenRule(TokenType.NEWLINE, true);
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
 * (BNF) content: systemStoreAssign|capsLockHeader|headerAssign|normalStoreAssign|ruleBlock
 */
export class ContentRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreAssign     = new SystemStoreAssignRule();
    const capsLockHeader        = new CapsLockHeaderRule();
    const headerAssign          = new HeaderAssignRule();
    const normalStoreAssign     = new NormalStoreAssignRule();
    const ruleBlock             = new RuleBlockRule();
    this.rule = new AlternateRule([
      systemStoreAssign,
      capsLockHeader,
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
    const plainText     = new PlainTextRule();
    const outsStatement = new OutsStatementRule();
    this.rule = new AlternateRule([plainText, outsStatement]);
  }
}

/**
 * (BNF) plainText: textRange|simpleText
 */
export class PlainTextRule extends SingleChildRule {
  public constructor() {
    super();
    const textRange  = new TextRangeRule();
    const simpleText = new SimpleTextRule();
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
    const stringRule       = new TokenRule(TokenType.STRING, true);
    const virtualKey       = new VirtualKeyRule();
    const uChar            = new TokenRule(TokenType.U_CHAR, true);
    const namedConstant    = new TokenRule(TokenType.NAMED_CONSTANT, true);
    const hangul           = new TokenRule(TokenType.HANGUL, true);
    const decimal          = new TokenRule(TokenType.DECIMAL, true);
    const hexadecimal      = new TokenRule(TokenType.HEXADECIMAL, true);
    const octal            = new TokenRule(TokenType.OCTAL, true);
    const nul              = new TokenRule(TokenType.NUL, true);
    const deadkeyStatement = new DeadkeyStatementRule();
    const beep             = new TokenRule(TokenType.BEEP, true);
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
 *
 * Uses a NewNode to rebuild the tree to be rooted at a RANGE node
 */
export class TextRangeRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.RANGE));
    const simpleText        = new SimpleTextRule();
    const rangeEnd          = new RangeEndRule();
    const oneOrManyRangeEnd = new OneOrManyRule(rangeEnd);
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
    const range      = new TokenRule(TokenType.RANGE);
    const simpleText = new SimpleTextRule();
    this.rule        = new SequenceRule([range, simpleText]);
  }
}

/**
 * (BNF) virtualKey: LEFT_SQ MODIFIER* keyCode RIGHT_SQ
 *
 * https://help.keyman.com/developer/language/guide/virtual-keys
 *
 * Uses a NewNode to rebuild the tree to be rooted at a VIRTUAL_KEY node
 */
export class VirtualKeyRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.VIRTUAL_KEY));
    const leftSquare   = new TokenRule(TokenType.LEFT_SQ);
    const modifier     = new TokenRule(TokenType.MODIFIER, true);
    const manyModifier = new ManyRule(modifier);
    const keyCode      = new KeyCodeRule();
    const rightSquare  = new TokenRule(TokenType.RIGHT_SQ);
    this.rule = new SequenceRule([
      leftSquare, manyModifier, keyCode, rightSquare
    ]);
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
    const keyCode    = new TokenRule(TokenType.KEY_CODE, true);
    const stringRule = new TokenRule(TokenType.STRING, true);
    const decimal    = new TokenRule(TokenType.DECIMAL, true);
    this.rule        = new AlternateRule([keyCode, stringRule, decimal]);
  }
}

/**
 * (BNF) ruleBlock: beginStatement|groupStatement|productionBlock
 */
export class RuleBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginStatement  = new BeginStatementRule();
    const groupStatement  = new GroupStatementRule();
    const productionBlock = new ProductionBlockRule();
    this.rule = new AlternateRule([beginStatement, groupStatement, productionBlock]);
  }
}

/**
 * (BNF) beginStatement: BEGIN entryPoint? CHEVRON useStatement
 *
 * https://help.keyman.com/developer/language/reference/begin
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the BEGIN node
 */
export class BeginStatementRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new GivenNode(NodeType.BEGIN));
    const begin          = new TokenRule(TokenType.BEGIN, true);
    const entryPointRule = new EntryPointRule();
    const optEntryPoint  = new OptionalRule(entryPointRule);
    const chevron        = new TokenRule(TokenType.CHEVRON);
    const useStatement   = new UseStatementRule();
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
    const unicode       = new TokenRule(TokenType.UNICODE, true);
    const newcontext    = new TokenRule(TokenType.NEWCONTEXT, true);
    const postkeystroke = new TokenRule(TokenType.POSTKEYSTROKE, true);
    const ansi          = new TokenRule(TokenType.ANSI, true);
    this.rule = new AlternateRule([unicode, newcontext, postkeystroke, ansi]);
  }
}

/**
 * (BNF) useStatement: USE LEFT_BR groupName RIGHT_BR
 *
 * https://help.keyman.com/developer/language/reference/use
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the USE node
 */
export class UseStatementRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new GivenNode(NodeType.USE));
    const use          = new TokenRule(TokenType.USE, true);
    const leftBracket  = new TokenRule(TokenType.LEFT_BR);
    const groupName    = new GroupNameRule();
    const rightBracket = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([use, leftBracket, groupName, rightBracket]);
  }
}

/**
 * (BNF) groupStatement: GROUP LEFT_BR groupName RIGHT_BR groupQualifier?
 *
 * https://help.keyman.com/developer/language/reference/group
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the GROUP node
 */
export class GroupStatementRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new GivenNode(NodeType.GROUP));
    const group              = new TokenRule(TokenType.GROUP, true);
    const leftBracket        = new TokenRule(TokenType.LEFT_BR);
    const groupName          = new GroupNameRule();
    const rightBracket       = new TokenRule(TokenType.RIGHT_BR);
    const groupQualifierRule = new GroupQualifierRule();
    const optGroupQualifier  = new OptionalRule(groupQualifierRule);
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
 *
 * Uses a NewNodeOrTree to add either a single new GROUPNAME
 * node or build a tree rooted at a GROUPNAME node, depending
 * on the number of children found (one or more)
 */
export class GroupNameRule extends SingleChildRuleWithASTRebuild {
  // TODO-NG-COMPILER: warning/error if group name consists of multiple elements
  public constructor() {
    super(new NewNodeOrTree(NodeType.GROUPNAME));
    const groupNameElement = new GroupNameElementRule();
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
    const parameter        = new TokenRule(TokenType.PARAMETER, true);
    const octal            = new TokenRule(TokenType.OCTAL, true);
    const permittedKeyword = new PermittedKeywordRule();
    this.rule              = new AlternateRule([parameter, octal, permittedKeyword]);
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
      TokenType.ANSI,
      TokenType.BEEP,
      TokenType.BEGIN,
      TokenType.BITMAP_HEADER,
      TokenType.CONTEXT,
      TokenType.COPYRIGHT_HEADER,
      TokenType.DECIMAL,
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
      TokenType.POSTKEYSTROKE,
      TokenType.READONLY,
      TokenType.RETURN,
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
    const usingKeys = new UsingKeysRule();
    const readonly  = new TokenRule(TokenType.READONLY, true);
    this.rule       = new AlternateRule([usingKeys, readonly]);
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
    const using = new TokenRule(TokenType.USING);
    const keys  = new TokenRule(TokenType.KEYS);
    this.rule   = new SequenceRule([using, keys]);
  }

  /**
   * Parse a UsingKeysRule
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    if (this.rule.parse(tokenBuffer, new ASTNode())) {
      node.addChild(new ASTNode(NodeType.USING_KEYS));
      return true;
    }
    return false;
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
    this.rule      = new SequenceRule([lhsBlock, chevron, rhsBlock]);
  }

  /**
   * Parse a ProductionBlockRule
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp = new ASTNode();
    if (this.rule.parse(tokenBuffer, tmp)) {
      const productionNode = new ASTNode(NodeType.PRODUCTION);
      productionNode.addChild(tmp.getSoleChildOfType(NodeType.LHS));
      productionNode.addChild(tmp.getSoleChildOfType(NodeType.RHS));
      node.addChild(productionNode);
      return true;
    }
    return false;
  }
}

/**
 * (BNF) lhsBlock: MATCH|NOMATCH|inputBlock
 *
 * https://help.keyman.com/developer/language/reference/match
 * https://help.keyman.com/developer/language/reference/nomatch
 *
 * Uses a NewNode to rebuild the tree to be rooted at a LHS node
 */
export class LhsBlockRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.LHS));
    const match      = new TokenRule(TokenType.MATCH, true);
    const nomatch    = new TokenRule(TokenType.NOMATCH, true);
    const inputBlock = new InputBlockRule();
    this.rule        = new AlternateRule([match, nomatch,inputBlock]);
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
    const nulRule             = new TokenRule(TokenType.NUL, true);
    const optNul              = new OptionalRule(nulRule);
    const ifLikeStatement     = new IfLikeStatementRule();
    const manyIfLikeStatement = new ManyRule(ifLikeStatement);
    const inputContext        = new InputContextRule();
    const optInputContext     = new OptionalRule(inputContext);
    const keystoke            = new KeystrokeRule();
    const optKeystroke        = new OptionalRule(keystoke);
    this.rule = new SequenceRule([
      optNul, manyIfLikeStatement, optInputContext, optKeystroke,
    ]);
  }
}

/**
 * (BNF) inputContext: inputElement+
 *
 * Uses a NewNode to rebuild the tree to be rooted at an INPUT_CONTEXT node
 */
export class InputContextRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.INPUT_CONTEXT));
    const inputElement = new InputElementRule();
    this.rule          = new OneOrManyRule(inputElement);
  }
}

/**
 * (BNF) inputElement: anyStatement|notanyStatement|contextStatement|indexStatement|text
 */
export class InputElementRule extends SingleChildRule {
  public constructor() {
    super();
    const anyStatement     = new AnyStatementRule();
    const notanyStatement  = new NotanyStatementRule();
    const contextStatement = new ContextStatementRule();
    const indexStatement   = new IndexStatementRule();
    const text             = new TextRule();
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
 * (BNF) keystroke: PLUS keystrokeElement+
 *
 * Uses a NewNode to rebuild the tree to be rooted at a KEYSTROKE node
 */
export class KeystrokeRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.KEYSTROKE));
    const plus                      = new TokenRule(TokenType.PLUS);
    const keystrokeElement          = new KeystrokeElementRule();
    const oneOrManyKeystrokeElement = new OneOrManyRule(keystrokeElement);
    this.rule = new SequenceRule([plus, oneOrManyKeystrokeElement]);
  }
}

/**
 * (BNF) keystrokeElement: anyStatement|simpleText|outsStatement
 */
export class KeystrokeElementRule extends SingleChildRule {
  public constructor() {
    super();
    const anyStatement  = new AnyStatementRule();
    const simpleText    = new SimpleTextRule();
    const outsStatement = new OutsStatementRule()
    this.rule = new AlternateRule([anyStatement, simpleText, outsStatement]);
  }
}

/**
 * (BNF) rhsBlock: outputStatement+
 *
 * Uses a NewNode to rebuild the tree to be rooted at a RHS node
 */
export class RhsBlockRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new NewNode(NodeType.RHS));
    const outputStatement = new OutputStatementRule();
    this.rule             = new OneOrManyRule(outputStatement);
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
    const useStatement     = new UseStatementRule();
    const callStatement    = new CallStatementRule();
    const setNormalStore   = new SetNormalStoreRule();
    const saveStatement    = new SaveStatementRule();
    const resetStore       = new ResetStoreRule();
    const setSystemStore   = new SetSystemStoreRule();
    const layerStatement   = new LayerStatementRule();
    const indexStatement   = new IndexStatementRule();
    const contextStatement = new ContextStatementRule();
    const context          = new TokenRule(TokenType.CONTEXT, true);
    const returnRule       = new TokenRule(TokenType.RETURN, true);
    const text             = new TextRule();
    const beep             = new TokenRule(TokenType.BEEP, true);
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
