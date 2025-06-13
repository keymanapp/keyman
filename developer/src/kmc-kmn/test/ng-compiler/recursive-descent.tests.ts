/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import 'mocha';
import { assert } from 'chai';
import { AlternateRule, TokenRule, ManyRule, OneOrManyRule, OptionalRule, Rule, SequenceRule, parameterSequence, AlternateTokenRule } from '../../src/ng-compiler/recursive-descent.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { Token, TokenTypes } from '../../src/ng-compiler/lexer.js';

const LIST_OF_ONE: Token[] = [
  new Token(TokenTypes.STRING, ''),
];

class TrueRule extends Rule {
  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    if (Rule.tokenBuffer.nextToken().isTokenType(TokenTypes.EOF))
      return false;
    Rule.tokenBuffer.popToken();
    node.addChild(new ASTNode(NodeTypes.TMP));
    return true;
  }
}

class FalseRule extends Rule {
  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    Rule.tokenBuffer.popToken();
    return false;
  }
}

let root: ASTNode            = null;
let trueRule: Rule           = null;
let falseRule: Rule          = null;
let parameters: Token[]      = null;

describe("Recursive Descent Tests", () => {
  beforeEach(() => {
    Rule.tokenBuffer = new TokenBuffer(LIST_OF_ONE);
    root             = new ASTNode(NodeTypes.TMP);
    trueRule         = new TrueRule();
    falseRule        = new FalseRule();
    parameters       = [];
  });
  describe("SequenceRule Tests", () => {
    it("can construct a SequenceRule", () => {
      const sequence: Rule = new SequenceRule([trueRule, trueRule, trueRule]);
      assert.isNotNull(sequence);
    });
    it("can parse with a successful child Rule (single child Rule)", () => {
      const sequence: Rule = new SequenceRule([trueRule]);
      assert.isTrue(sequence.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three child Rules)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      trueRule             = new TrueRule();
      const sequence: Rule = new SequenceRule([trueRule, trueRule, trueRule]);
      assert.isTrue(sequence.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(Rule.tokenBuffer.currentPosition, 3);
    });
    it("can parse with an unsuccessful child Rule (three child Rules)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      trueRule             = new TrueRule();
      falseRule            = new FalseRule();
      const sequence: Rule = new SequenceRule([trueRule, falseRule, trueRule]);
      assert.isFalse(sequence.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("AlternateRule Tests", () => {
    it("can construct an AlternateRule", () => {
      const alternate: Rule = new AlternateRule([falseRule, falseRule, trueRule]);
      assert.isNotNull(alternate);
    });
    it("can parse with a successful child Rule (single child Rule)", () => {
      const alternate: Rule = new AlternateRule([trueRule]);
      assert.isTrue(alternate.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three child Rules)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      trueRule              = new TrueRule();
      falseRule             = new FalseRule();
      const alternate: Rule = new AlternateRule([trueRule, trueRule, falseRule]);
      assert.isTrue(alternate.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule (three child Rules)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      falseRule             = new FalseRule();
      const alternate: Rule = new AlternateRule([falseRule, falseRule, falseRule]);
      assert.isFalse(alternate.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("OptionalRule Tests", () => {
    it("can construct an OptionalRule", () => {
      const child: Rule    = new TrueRule();
      const optional: Rule = new OptionalRule(child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule", () => {
      const optional: Rule = new OptionalRule(trueRule);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const optional: Rule  = new OptionalRule(falseRule);
      assert.isTrue(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("ManyRule Tests", () => {
    it("can construct a ManyRule", () => {
      const many: Rule = new ManyRule(trueRule);
      assert.isNotNull(many);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const many: Rule = new ManyRule(trueRule);
      assert.isTrue(many.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      trueRule         = new TrueRule();
      const many: Rule = new ManyRule(trueRule);
      assert.isTrue(many.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(Rule.tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const many: Rule = new ManyRule(falseRule);
      assert.isTrue(many.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("OneOrManyRule Tests", () => {
    it("can construct a OneOrManyRule", () => {
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isNotNull(oneOrMany);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isTrue(oneOrMany.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      Rule.tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
        new Token(TokenTypes.STRING, ''),
      ]);
      trueRule              = new TrueRule();
      const oneOrMany: Rule = new OneOrManyRule(trueRule);
      assert.isTrue(oneOrMany.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(Rule.tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const oneOrMany: Rule = new OneOrManyRule(falseRule);
      assert.isFalse(oneOrMany.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("TokenRule Tests", () => {
    it("can construct a TokenRule", () => {
      const tokenRule: Rule = new TokenRule(TokenTypes.STRING);
      assert.isNotNull(tokenRule);
    });
    it("can parse with a successful child Rule", () => {
      const tokenRule: Rule = new TokenRule(TokenTypes.STRING);
      assert.isTrue(tokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const tokenRule: Rule = new TokenRule(TokenTypes.STORE);
      assert.isFalse(tokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("AlternateTokenRule Tests", () => {
    it("can construct an AlternateTokenRule", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenTypes.STRING]);
      assert.isNotNull(alternateTokenRule);
    });
    it("can parse a matched token (from a list of one)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenTypes.STRING]);
      assert.isTrue(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse an unmatched token (from a list of one)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenTypes.PARAMETER]);
      assert.isFalse(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
    it("can parse a matched token (from a list of three)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([
        TokenTypes.BITMAP, TokenTypes.STRING, TokenTypes.PARAMETER,
      ]);
      assert.isTrue(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse an unmatched token (from a list of three)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([
        TokenTypes.BITMAP, TokenTypes.COPYRIGHT, TokenTypes.PARAMETER,
      ]);
      assert.isFalse(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
    it("can parse a matched token (from a list of one, addNode is true)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([TokenTypes.STRING], true);
      assert.isTrue(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
      assert.isTrue(root.hasChildOfType(NodeTypes.STRING));
    });
    it("can parse an unmatched token (from an empty list)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule([]);
      assert.isFalse(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
    it("can parse an unmatched token (from a null list)", () => {
      const alternateTokenRule: Rule = new AlternateTokenRule(null);
      assert.isFalse(alternateTokenRule.parse(root));
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
  describe("parameterSequence() Tests", () => {
    it("can parse a single parameter", () => {
      const tokens = [
        new Token(TokenTypes.PARAMETER, ''),
      ];
      Rule.tokenBuffer = new TokenBuffer(tokens);
      assert.isTrue(parameterSequence(parameters, 1));
      assert.deepEqual(parameters, tokens);
      assert.equal(Rule.tokenBuffer.currentPosition, 1);
    });
    it("can parse three parameters", () => {
      const tokens = [
        new Token(TokenTypes.PARAMETER, ''),
        new Token(TokenTypes.PARAMETER, ''),
        new Token(TokenTypes.PARAMETER, ''),
      ];
      Rule.tokenBuffer = new TokenBuffer(tokens);
      assert.isTrue(parameterSequence(parameters, 3));
      assert.deepEqual(parameters, tokens);
      assert.equal(Rule.tokenBuffer.currentPosition, 3);
    });
    it("can handle a missing parameter", () => {
      assert.isFalse(parameterSequence(parameters, 1));
      assert.deepEqual(parameters, []);
      assert.equal(Rule.tokenBuffer.currentPosition, 0);
    });
  });
});