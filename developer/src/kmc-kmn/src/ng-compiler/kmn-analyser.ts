/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 *
 * KMN Analyser Tests
 */

import { TokenTypes } from "./token-types.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule, AlternateTokenRule } from "./recursive-descent.js";
import { SingleChildRule, OneOrManyRule, ManyRule } from "./recursive-descent.js";
import { AnyStatementRule, CallStatementRule, ContextStatementRule, DeadKeyStatementRule, IfLikeStatementRule, IndexStatementRule, LayerStatementRule, NotAnyStatementRule, OutsStatementRule, SaveStatementRule } from "./statement-analyser.js";
import { CapsAlwaysOffRule, CapsOnOnlyRule, ResetStoreRule, SetSystemStoreRule, SetNormalStoreRule, ShiftFreesCapsRule, HeaderAssignRule } from "./store-analyser.js";
import { SystemStoreAssignRule, NormalStoreAssignRule } from "./store-analyser.js";
import { NodeTypes } from "./node-types.js";
import { ASTNode } from "./tree-construction.js";

export class KmnTreeRule extends SingleChildRule {
  public constructor() {
    super();
    const line: Rule = new LineRule();
    this.rule = new ManyRule(line);
  }

  public parse(node: ASTNode): boolean {
    if (this.rule === null) {
      return false;
    }
    const parseSuccess = this.rule.parse(node);
    let children: ASTNode[] = [];
    const storesNode: ASTNode     = this.gatherStores(node);
    const sourceCodeNode: ASTNode = this.gatherSourceCode(node);
    children.unshift(storesNode);
    children.push(...node.removeChildren());
    children.push(sourceCodeNode);
    node.addChildren(children);
    return parseSuccess;
  }

  private static STORES_NODETYPES = [
    NodeTypes.BITMAP,
    NodeTypes.CASEDKEYS,
    NodeTypes.COPYRIGHT,
    NodeTypes.DISPLAYMAP,
    NodeTypes.ETHNOLOGUECODE,
    NodeTypes.HOTKEY,
    NodeTypes.INCLUDECODES,
    NodeTypes.KEYBOARDVERSION,
    NodeTypes.KMW_EMBEDCSS,
    NodeTypes.KMW_EMBEDJS,
    NodeTypes.KMW_HELPFILE,
    NodeTypes.KMW_HELPTEXT,
    NodeTypes.KMW_RTL,
    NodeTypes.LANGUAGE,
    NodeTypes.LAYOUTFILE,
    NodeTypes.MESSAGE,
    NodeTypes.MNEMONICLAYOUT,
    NodeTypes.NAME,
    NodeTypes.TARGETS,
    NodeTypes.VERSION,
    NodeTypes.VISUALKEYBOARD,
    NodeTypes.WINDOWSLANGUAGES,
    NodeTypes.CAPSALWAYSOFF,
    NodeTypes.CAPSONONLY,
    NodeTypes.SHIFTFREESCAPS,
    NodeTypes.BITMAP_HEADER,
    NodeTypes.COPYRIGHT_HEADER,
    NodeTypes.HOTKEY_HEADER,
    NodeTypes.LANGUAGE_HEADER,
    NodeTypes.LAYOUT_HEADER,
    NodeTypes.MESSAGE_HEADER,
    NodeTypes.NAME_HEADER,
    NodeTypes.VERSION_HEADER,
    NodeTypes.STORE,
  ];

  private gatherStores(node: ASTNode): ASTNode {
    const storeNodes: ASTNode[] = node.removeChildrenOfTypes(KmnTreeRule.STORES_NODETYPES);
    const storesNode: ASTNode   = new ASTNode(NodeTypes.STORES);
    storesNode.addChildren(storeNodes);
    return storesNode;
  }

  private gatherSourceCode(node: ASTNode): ASTNode {
    const lineNodes: ASTNode[]    = node.removeChildrenOfType(NodeTypes.LINE);
    const sourceCodeNode: ASTNode = new ASTNode(NodeTypes.SOURCE_CODE);
    sourceCodeNode.addChildren(lineNodes);
    return sourceCodeNode;
  }
}

export class LineRule extends SingleChildRule {
  public constructor() {
    super();
    const compileTarget: Rule    = new CompileTargetRule();
    const optCompileTarget: Rule = new OptionalRule(compileTarget);
    const content: Rule          = new ContentRule();
    const optContent: Rule       = new OptionalRule(content);
    const newline: Rule          = new TokenRule(TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule([optCompileTarget, optContent, newline]);
  }
}

export class CompileTargetRule extends SingleChildRule {
  public constructor() {
    super();
    const keyman: Rule     = new TokenRule(TokenTypes.KEYMAN, true);
    const keymanonly: Rule = new TokenRule(TokenTypes.KEYMANONLY, true);
    const keymanweb: Rule  = new TokenRule(TokenTypes.KEYMANWEB, true);
    const kmfl: Rule       = new TokenRule(TokenTypes.KMFL, true);
    const weaver: Rule     = new TokenRule(TokenTypes.WEAVER, true);
    this.rule = new AlternateRule([
      keyman, keymanonly, keymanweb, kmfl, weaver,
    ]);
  }
}

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
    const stringRule: Rule    = new TokenRule(TokenTypes.STRING, true);
    const virtualKey: Rule    = new VirtualKeyRule();
    const uChar: Rule         = new TokenRule(TokenTypes.U_CHAR, true);
    const namedConstant: Rule = new TokenRule(TokenTypes.NAMED_CONSTANT, true);
    const hangul: Rule        = new TokenRule(TokenTypes.HANGUL, true);
    const decimal: Rule       = new TokenRule(TokenTypes.DECIMAL, true);
    const hexadecimal: Rule   = new TokenRule(TokenTypes.HEXADECIMAL, true);
    const octal: Rule         = new TokenRule(TokenTypes.OCTAL, true);
    const nul: Rule           = new TokenRule(TokenTypes.NUL, true);
    const deadKey: Rule       = new DeadKeyStatementRule();
    const beep: Rule          = new TokenRule(TokenTypes.BEEP, true);
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
      deadKey,
      beep,
    ]);
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
    const range: Rule         = new TokenRule(TokenTypes.RANGE);
    const simpleText: Rule    = new SimpleTextRule();
    this.rule = new SequenceRule([range, simpleText]);
  }
}

export class VirtualKeyRule extends SingleChildRule {
  public constructor() {
    super();
    const leftSquare: Rule   = new TokenRule(TokenTypes.LEFT_SQ);
    const modifier: Rule     = new ModifierRule();
    const manyModifier: Rule = new ManyRule(modifier);
    const keyCode: Rule      = new KeyCodeRule();
    const rightSquare: Rule  = new TokenRule(TokenTypes.RIGHT_SQ);
    this.rule = new SequenceRule([
      leftSquare, manyModifier, keyCode, rightSquare
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const virtualKeyNode = new ASTNode(NodeTypes.VIRTUAL_KEY);
      virtualKeyNode.addChildren(tmp.getChildren());
      node.addChild(virtualKeyNode);
    }
    return parseSuccess;
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

export class KeyCodeRule extends SingleChildRule {
  // DECIMAL is included because of e.g. d10, which could be ISO9995 code
  public constructor() {
    super();
    const keyCode: Rule    = new TokenRule(TokenTypes.KEY_CODE, true);
    const stringRule: Rule = new TokenRule(TokenTypes.STRING, true);
    const decimal: Rule    = new TokenRule(TokenTypes.DECIMAL, true);
    this.rule = new AlternateRule([keyCode, stringRule, decimal]);
  }
}

export class RuleBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const beginStatement: Rule  = new BeginStatementRule();
    const groupStatement: Rule  = new GroupStatementRule();
    const productionBlock: Rule = new ProductionBlockRule();
    this.rule = new AlternateRule([
      beginStatement,
      groupStatement,
      productionBlock,
    ]);
  }
}

export class BeginStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const begin: Rule          = new TokenRule(TokenTypes.BEGIN, true);
    const entryPointRule: Rule = new EntryPointRule();
    const optEntryPoint: Rule  = new OptionalRule(entryPointRule);
    const chevron: Rule        = new TokenRule(TokenTypes.CHEVRON);
    const useStatement: Rule   = new UseStatementRule();
    this.rule = new SequenceRule([begin, optEntryPoint, chevron, useStatement]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const beginNode = tmp.removeSoleChildOfType(NodeTypes.BEGIN);
      beginNode.addChildren(tmp.getChildren());
      node.addChild(beginNode);
    }
    return parseSuccess;
  }
}

export class EntryPointRule extends SingleChildRule {
  public constructor() {
    super();
    const unicode: Rule       = new TokenRule(TokenTypes.UNICODE, true);
    const newcontext: Rule    = new TokenRule(TokenTypes.NEWCONTEXT, true);
    const postkeystroke: Rule = new TokenRule(TokenTypes.POSTKEYSTROKE, true);
    const ansi: Rule          = new TokenRule(TokenTypes.ANSI, true);
    this.rule = new AlternateRule([unicode, newcontext, postkeystroke, ansi]);
  }
}

export class UseStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const use: Rule          = new TokenRule(TokenTypes.USE, true);
    const leftBracket: Rule  = new TokenRule(TokenTypes.LEFT_BR);
    const groupName: Rule    = new GroupNameRule();
    const rightBracket: Rule = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([use, leftBracket, groupName, rightBracket]);
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

export class GroupStatementRule extends SingleChildRule {
  public constructor() {
    super();
    const group: Rule              = new TokenRule(TokenTypes.GROUP, true);
    const leftBracket: Rule        = new TokenRule(TokenTypes.LEFT_BR);
    const groupName: Rule          = new GroupNameRule();
    const rightBracket: Rule       = new TokenRule(TokenTypes.RIGHT_BR);
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

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const groupNode = tmp.removeSoleChildOfType(NodeTypes.GROUP);
      groupNode.addChildren(tmp.getChildren());
      node.addChild(groupNode);
    }
    return parseSuccess;
  }
}

export class GroupNameRule extends SingleChildRule {
  public constructor() {
    super();
    const groupNameElement: Rule = new GroupNameElementRule();
    this.rule = new OneOrManyRule(groupNameElement);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const children = tmp.getChildren();
      if (children.length === 1) {
        node.addToken(NodeTypes.GROUPNAME, children[0].token);
      } else {
        const groupNameNode = new ASTNode(NodeTypes.GROUPNAME);
        groupNameNode.addChildren(children);
        node.addChild(groupNameNode);
      }
    }
    return parseSuccess;
  }
}

export class GroupNameElementRule extends SingleChildRule {
  public constructor() {
    super();
    const parameter: Rule        = new TokenRule(TokenTypes.PARAMETER, true);
    const octal: Rule            = new TokenRule(TokenTypes.OCTAL, true);
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([parameter, octal, permittedKeyword]);
  }
}

export class PermittedKeywordRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.ALWAYS,
      TokenTypes.ANSI,
      TokenTypes.BEEP,
      TokenTypes.BEGIN,
      TokenTypes.BITMAP_HEADER,
      TokenTypes.CAPS,
      TokenTypes.CLEARCONTEXT,
      TokenTypes.CONTEXT,
      TokenTypes.COPYRIGHT_HEADER,
      TokenTypes.DECIMAL,
      TokenTypes.FIX,
      TokenTypes.FREES,
      TokenTypes.HEXADECIMAL,
      TokenTypes.HOTKEY_HEADER,
      TokenTypes.KEY_CODE,
      TokenTypes.KEYS,
      TokenTypes.LANGUAGE_HEADER,
      TokenTypes.LAYOUT_HEADER,
      TokenTypes.MATCH,
      TokenTypes.MESSAGE_HEADER,
      TokenTypes.NAME_HEADER,
      TokenTypes.NEWCONTEXT,
      TokenTypes.NOMATCH,
      TokenTypes.NUL,
      TokenTypes.OCTAL,
      TokenTypes.OFF,
      TokenTypes.ON,
      TokenTypes.ONLY,
      TokenTypes.POSTKEYSTROKE,
      TokenTypes.READONLY,
      TokenTypes.RETURN,
      TokenTypes.SHIFT,
      TokenTypes.UNICODE,
      TokenTypes.USING,
      TokenTypes.VERSION_HEADER,
    ], true);
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
    const keys: Rule       = new TokenRule(TokenTypes.KEYS);
    this.rule = new SequenceRule([using, keys]);
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

export class ProductionBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const lhsBlock = new LhsBlockRule();
    const chevron  = new TokenRule(TokenTypes.CHEVRON);
    const rhsBlock = new RhsBlockRule();
    this.rule = new SequenceRule([lhsBlock, chevron, rhsBlock]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lhsNode        = tmp.getSoleChildOfType(NodeTypes.LHS);
      const rhsNode        = tmp.getSoleChildOfType(NodeTypes.RHS);
      const productionNode = new ASTNode(NodeTypes.PRODUCTION);
      productionNode.addChild(lhsNode);
      productionNode.addChild(rhsNode);
      node.addChild(productionNode);
    }
    return parseSuccess;
  }
}

export class LhsBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const match: Rule      = new TokenRule(TokenTypes.MATCH, true);
    const noMatch: Rule    = new TokenRule(TokenTypes.NOMATCH, true);
    const inputBlock: Rule = new InputBlockRule();
    this.rule = new AlternateRule([match, noMatch,inputBlock]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lhsNode = new ASTNode(NodeTypes.LHS);
      lhsNode.addChildren(tmp.getChildren());
      node.addChild(lhsNode);
    }
    return parseSuccess;
  }
}

export class InputBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const nulRule: Rule             = new TokenRule(TokenTypes.NUL, true);
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

export class InputContextRule extends SingleChildRule {
  public constructor() {
    super();
    const inputElement = new InputElementRule();
    this.rule = new OneOrManyRule(inputElement);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const inputContextNode = new ASTNode(NodeTypes.INPUT_CONTEXT);
      inputContextNode.addChildren(tmp.getChildren());
      node.addChild(inputContextNode);
    }
    return parseSuccess;
  }
}

export class InputElementRule extends SingleChildRule {
  public constructor() {
    super();
    const any: Rule              = new AnyStatementRule();
    const notAny: Rule           = new NotAnyStatementRule();
    const contextStatement: Rule = new ContextStatementRule();
    const indexStatement: Rule   = new IndexStatementRule();
    const text: Rule             = new TextRule();
    this.rule = new AlternateRule([
      any,
      notAny,
      contextStatement,
      indexStatement,
      text
    ]);
  }
}

export class KeystrokeRule extends SingleChildRule {
  public constructor() {
    super();
    const plus: Rule            = new TokenRule(TokenTypes.PLUS);
    const inputElement          = new InputElementRule();
    const oneOrManyInputElement = new OneOrManyRule(inputElement);
    this.rule = new SequenceRule([plus, oneOrManyInputElement]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const keystrokeNode = new ASTNode(NodeTypes.KEYSTROKE);
      keystrokeNode.addChildren(tmp.getChildren());
      node.addChild(keystrokeNode);
    }
    return parseSuccess;
  }
}

export class RhsBlockRule extends SingleChildRule {
  public constructor() {
    super();
    const outputStatement: Rule = new OutputStatementRule();
    this.rule = new OneOrManyRule(outputStatement);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const rhsNode = new ASTNode(NodeTypes.RHS);
      rhsNode.addChildren(tmp.getChildren());
      node.addChild(rhsNode);
    }
    return parseSuccess;
  }
}

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
    const context: Rule          = new TokenRule(TokenTypes.CONTEXT, true);
    const returnRule: Rule       = new TokenRule(TokenTypes.RETURN, true);
    const text: Rule             = new TextRule();
    const beep: Rule             = new TokenRule(TokenTypes.BEEP, true);
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
