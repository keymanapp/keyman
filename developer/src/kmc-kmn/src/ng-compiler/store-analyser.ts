/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-05-05
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 * 
 * System and Variable Store Rules
 */

import { PaddingRule, OptionalWhiteSpaceRule, PrePadTextRule, VirtualKeyRule } from "./kmn-analyser.js";
import { Token, TokenTypes } from "./lexer.js";
import { SingleChildRule, Rule, TokenRule, SequenceRule, AlternateTokenRule } from "./recursive-descent.js";
import { OneOrManyRule, parameterSequence } from "./recursive-descent.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";


export class SystemStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStore: Rule         = new SystemStoreRule();
    const prePadText: Rule          = new PrePadTextRule();
    const oneOrManyPaddedText: Rule = new OneOrManyRule(prePadText);
    this.rule = new SequenceRule([systemStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const children: ASTNode[]  = tmp.getChildren();
      const storeNode: ASTNode   = children.splice(0, 1)[0];
      storeNode.addChildren(children);
      storeNode.addChildren(lineNodes);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

abstract class AbstractSystemStoreRule extends SingleChildRule {
  protected store: Rule;
  protected leftBracket: Rule;
  protected optWhitespace: Rule;
  protected ampersand: Rule;
  protected rightBracket: Rule;

  public constructor() {
    super();
    this.store = new TokenRule(TokenTypes.STORE);
    this.leftBracket = new TokenRule(TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpaceRule();
    this.ampersand = new TokenRule(TokenTypes.AMPERSAND);
    this.rightBracket = new TokenRule(TokenTypes.RIGHT_BR);
  }
}

export class SystemStoreRule extends AbstractSystemStoreRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    this.rule = new SequenceRule([
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.ampersand,
      systemStoreName,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class SystemStoreNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.BITMAP,
      TokenTypes.CASEDKEYS,
      TokenTypes.COPYRIGHT,
      TokenTypes.DISPLAYMAP,
      TokenTypes.ETHNOLOGUECODE,
      TokenTypes.HOTKEY,
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
      TokenTypes.CAPSALWAYSOFF,
      TokenTypes.CAPSONONLY,
      TokenTypes.SHIFTFREESCAPS,
    ], true);
  }
}

export class CasedkeysStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const casedkeysStore: Rule = new CasedkeysStoreRule();
    const prePadText: Rule = new PrePadTextRule();
    const oneOrManyPaddedText: Rule = new OneOrManyRule(prePadText);
    this.rule = new SequenceRule([casedkeysStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const casedkeysNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.CASEDKEYS);
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[] = tmp.getChildren();
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
      this.ampersand,
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
    const padding: Rule = new PaddingRule();
    const virtualKey: Rule = new VirtualKeyRule();
    this.rule = new SequenceRule([hotkeyStore, padding, virtualKey]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const hotkeyNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.HOTKEY);
      const lineNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.LINE);
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
      this.ampersand,
      hotkey,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class VariableStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const variableStore: Rule = new VariableStoreRule();
    const prePadText: Rule = new PrePadTextRule();
    const oneOrManyPaddedText: Rule = new OneOrManyRule(prePadText);
    this.rule = new SequenceRule([variableStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.STORE);
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[] = tmp.getChildren();
      storeNode.addChildren(textNodes);
      storeNode.addChildren(lineNodes);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class VariableStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule = new TokenRule(TokenTypes.STORE, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([store, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode = tmp.getSoleChildOfType(NodeTypes.STORE);
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
    const leftBracket: Rule = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule();
    const variableStoreName: Rule = new VariableStoreNameRule();
    const rightBracket: Rule = new TokenRule(TokenTypes.RIGHT_BR);
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
    const parameters: Token[] = [];
    const parseSuccess: boolean = parameterSequence(parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.STORENAME, parameters[0]);
    }
    return parseSuccess;
  };
}

export class PermittedKeywordRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.NEWCONTEXT,
      TokenTypes.POSTKEYSTROKE,
    ], true);
  }
}

export class SetStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const set: Rule               = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule       = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule();
    const variableStoreName: Rule = new VariableStoreNameRule();
    const whitespace: Rule        = new TokenRule(TokenTypes.WHITESPACE);
    const equal: Rule             = new TokenRule(TokenTypes.EQUAL);
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    const rightBracket: Rule      = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      optWhitespace,
      variableStoreName,
      whitespace,
      equal,
      whitespace,
      stringRule,
      optWhitespace,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const setNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.SET);
      setNode.addChildren(tmp.getChildren());
      node.addChild(setNode);
    }
    return parseSuccess;
  }
}

export class SetLayerRule extends SingleChildRule {
  public constructor() {
    super();
    const set: Rule               = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule       = new TokenRule(TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule();
    const ampersand: Rule         = new TokenRule(TokenTypes.AMPERSAND);
    const layer: Rule             = new TokenRule(TokenTypes.LAYER, true);
    const whitespace: Rule        = new TokenRule(TokenTypes.WHITESPACE);
    const equal: Rule             = new TokenRule(TokenTypes.EQUAL);
    const stringRule: Rule        = new TokenRule(TokenTypes.STRING, true);
    const rightBracket: Rule      = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      optWhitespace,
      ampersand,
      layer,
      whitespace,
      equal,
      whitespace,
      stringRule,
      optWhitespace,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const setNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.SET);
      setNode.addChildren(tmp.getChildren());
      node.addChild(setNode);
    }
    return parseSuccess;
  }
}

export class ResetStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const reset: Rule = new TokenRule(TokenTypes.RESET, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([reset, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const resetNode = tmp.getSoleChildOfType(NodeTypes.RESET);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      resetNode.addChild(storeNameNode);
      node.addChild(resetNode);
    }
    return parseSuccess;
  }
}

abstract class CapsLockStatementRule extends SingleChildRule {
  protected whitespace: Rule;
  protected tokenType: TokenTypes;
  protected nodeType: NodeTypes;

  public constructor() {
    super();
    this.whitespace = new TokenRule(TokenTypes.WHITESPACE);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      // TODO: warning deprecated
      const token: Token = new Token(this.tokenType, '1');
      node.addToken(this.nodeType, token);
    }
    return parseSuccess;
  }
}

export class CapsAlwaysOffRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType     = TokenTypes.CAPSALWAYSOFF;
    this.nodeType      = NodeTypes.CAPSALWAYSOFF;
    const caps: Rule   = new TokenRule(TokenTypes.CAPS);
    const always: Rule = new TokenRule(TokenTypes.ALWAYS);
    const off: Rule    = new TokenRule(TokenTypes.OFF);
    this.rule = new SequenceRule([
      caps, this.whitespace, always, this.whitespace, off,
    ]);
  }
}

export class CapsOnOnlyRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType     = TokenTypes.CAPSONONLY;
    this.nodeType      = NodeTypes.CAPSONONLY;
    const caps: Rule   = new TokenRule(TokenTypes.CAPS);
    const on: Rule     = new TokenRule(TokenTypes.ON);
    const only: Rule   = new TokenRule(TokenTypes.ONLY);
    this.rule = new SequenceRule([
      caps, this.whitespace, on, this.whitespace, only,
    ]);
  }
}

export class ShiftFreesCapsRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType    = TokenTypes.SHIFTFREESCAPS;
    this.nodeType     = NodeTypes.SHIFTFREESCAPS;
    const shift: Rule = new TokenRule(TokenTypes.SHIFT);
    const frees: Rule = new TokenRule(TokenTypes.FREES);
    const caps: Rule  = new TokenRule(TokenTypes.CAPS);
    this.rule = new SequenceRule([
      shift, this.whitespace, frees, this.whitespace, caps,
    ]);
  }
}
