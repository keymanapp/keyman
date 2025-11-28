/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import 'mocha';
import { assert } from 'chai';
import { AlternateRule, TokenRule, ManyRule, OneOrManyRule, OptionalRule } from '../../src/ng-compiler/recursive-descent.js';
import { Rule, SequenceRule, parameterSequence, AlternateTokenRule } from '../../src/ng-compiler/recursive-descent.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { NodeType } from "../../src/ng-compiler/node-type.js";
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { Token } from '../../src/ng-compiler/lexer.js';

const LIST_OF_ONE: Token[] = [
  new Token(TokenType.STRING, ''),
];
const LIST_OF_THREE: Token[] = [
  new Token(TokenType.STRING, ''),
  new Token(TokenType.STRING, ''),
  new Token(TokenType.STRING, ''),
];

class TrueRule extends Rule {
  public constructor() {
    super();
  }

  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    if (tokenBuffer.currentToken().isTokenType(TokenType.EOF))
      return false;
    tokenBuffer.popToken();
    node.addChild(new ASTNode(NodeType.TMP));
    return true;
  }
}

class FalseRule extends Rule {
  public constructor() {
    super();
  }

  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    tokenBuffer.popToken();
    return false;
  }
}

let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;
let trueRule: Rule           = null;
let falseRule: Rule          = null;
let parameters: Token[]      = null;

describe("Recursive Descent Tests", () => {
  beforeEach(() => {
    tokenBuffer = new TokenBuffer(LIST_OF_ONE);
    root        = new ASTNode(NodeType.TMP);
    trueRule    = new TrueRule();
    falseRule   = new FalseRule();
    parameters  = [];
  });
  describe("SequenceRule Tests", () => {
    it("can construct a SequenceRule", () => {
      const sequence: Rule = new SequenceRule([trueRule, trueRule, trueRule]);
      assert.isNotNull(sequence);
    });
    it("can parse with a successful child Rule (single child Rule)", () => {
      const sequence: Rule = new SequenceRule([trueRule]);
      assert.isTrue(sequence.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three child Rules)", () => {
      tokenBuffer          = new TokenBuffer(LIST_OF_THREE);
      const sequence: Rule = new SequenceRule([trueRule, trueRule, trueRule]);
      assert.isTrue(sequence.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with an unsuccessful child Rule (three child Rules)", () => {
      tokenBuffer          = new TokenBuffer(LIST_OF_THREE);
      const sequence: Rule = new SequenceRule([trueRule, falseRule, trueRule]);
      assert.isFalse(sequence.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("AlternateRule Tests", () => {
    it("can construct an AlternateRule", () => {
      const alternate: Rule = new AlternateRule([falseRule, falseRule, trueRule]);
      assert.isNotNull(alternate);
    });
    it("can parse with a successful child Rule (single child Rule)", () => {
      const alternate: Rule = new AlternateRule([trueRule]);
      assert.isTrue(alternate.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three child Rules)", () => {
      tokenBuffer           = new TokenBuffer(LIST_OF_THREE);
      const alternate: Rule = new AlternateRule([trueRule, trueRule, falseRule]);
      assert.isTrue(alternate.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule (three child Rules)", () => {
      tokenBuffer           = new TokenBuffer(LIST_OF_THREE);
      const alternate: Rule = new AlternateRule([falseRule, falseRule, falseRule]);
      assert.isFalse(alternate.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("OptionalRule Tests", () => {
    it("can construct an OptionalRule", () => {
      const optional: Rule = new OptionalRule(trueRule);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule", () => {
      const optional: Rule = new OptionalRule(trueRule);
      assert.isTrue(optional.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const optional: Rule = new OptionalRule(falseRule);
      assert.isTrue(optional.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("ManyRule Tests", () => {
    it("can construct a ManyRule", () => {
      const many: Rule = new ManyRule(trueRule);
      assert.isNotNull(many);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const many: Rule = new ManyRule(trueRule);
      assert.isTrue(many.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer      = new TokenBuffer(LIST_OF_THREE);
      const many: Rule = new ManyRule(trueRule);
      assert.isTrue(many.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const many: Rule = new ManyRule(falseRule);
      assert.isTrue(many.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("OneOrManyRule Tests", () => {
    it("can construct a OneOrManyRule", () => {
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isNotNull(oneOrMany);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isTrue(oneOrMany.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer           = new TokenBuffer(LIST_OF_THREE);
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isTrue(oneOrMany.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a successful child Rule (two Tokens)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenType.STRING, ''),
        new Token(TokenType.STRING, ''),
        new Token(TokenType.STORE, ''),
      ]);
      const tokenRule: Rule = new TokenRule(TokenType.STRING, true);
      const oneOrMany: Rule = new OneOrManyRule(tokenRule);
      assert.isTrue(oneOrMany.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeType.STRING));
      assert.equal(root.getChildren().length, 2);
      assert.equal(tokenBuffer.currentPosition, 2);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const oneOrMany: Rule = new OneOrManyRule(falseRule);
      assert.isFalse(oneOrMany.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("TokenRule Tests", () => {
    it("can construct a TokenRule", () => {
      const tokenRule: Rule = new TokenRule(TokenType.STRING);
      assert.isNotNull(tokenRule);
    });
    it("can parse with a successful child Rule (addNode false)", () => {
      const tokenRule: Rule = new TokenRule(TokenType.STRING);
      assert.isTrue(tokenRule.parse(tokenBuffer, root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (addNode true)", () => {
      const tokenRule: Rule = new TokenRule(TokenType.STRING, true);
      assert.isTrue(tokenRule.parse(tokenBuffer, root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasSoleChildOfType(NodeType.STRING));
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const tokenRule: Rule = new TokenRule(TokenType.STORE);
      assert.isFalse(tokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("AlternateTokenRule Tests", () => {
    it("can construct an AlternateTokenRule", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenType.STRING]);
      assert.isNotNull(alternateTokenRule);
    });
    it("can parse a matched token (from a list of one)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenType.STRING]);
      assert.isTrue(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse an unmatched token (from a list of one)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenType.PARAMETER]);
      assert.isFalse(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
    it("can parse a matched token (from a list of three)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([
        TokenType.BITMAP, TokenType.STRING, TokenType.PARAMETER,
      ]);
      assert.isTrue(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse an unmatched token (from a list of three)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([
        TokenType.BITMAP, TokenType.COPYRIGHT, TokenType.PARAMETER,
      ]);
      assert.isFalse(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
    it("can parse a matched token (from a list of one, addNode is true)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenType.STRING], true);
      assert.isTrue(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 1);
      assert.isTrue(root.hasChildOfType(NodeType.STRING));
    });
    it("can parse an unmatched token (from an empty list)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([]);
      assert.isFalse(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
    it("can parse an unmatched token (from a null list)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule(null);
      assert.isFalse(alternateTokenRule.parse(tokenBuffer, root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("parameterSequence() Tests", () => {
    it("can parse a single parameter", () => {
      const tokens = [
        new Token(TokenType.PARAMETER, ''),
      ];
      tokenBuffer = new TokenBuffer(tokens);
      assert.isTrue(parameterSequence(tokenBuffer, parameters, 1));
      assert.deepEqual(parameters, tokens);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse three parameters", () => {
      const tokens = [
        new Token(TokenType.PARAMETER, ''),
        new Token(TokenType.PARAMETER, ''),
        new Token(TokenType.PARAMETER, ''),
      ];
      tokenBuffer = new TokenBuffer(tokens);
      assert.isTrue(parameterSequence(tokenBuffer, parameters, 3));
      assert.deepEqual(parameters, tokens);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can handle a missing parameter", () => {
      assert.isFalse(parameterSequence(tokenBuffer, parameters, 1));
      assert.deepEqual(parameters, []);
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
});