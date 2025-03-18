/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import 'mocha';
import { assert } from 'chai';
import { KeywordRule, ManyRule, OneOrManyRule, OptionalRule, Rule, SequenceRule } from '../../src/ng-compiler/recursive-descent.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { Token, TokenTypes } from '../../src/ng-compiler/lexer.js';

const LIST_OF_ONE: Token[] = [
  new Token(TokenTypes.TT_STRING, ''),
];

class TrueRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    if (this.tokenBuffer.nextToken().isTokenType(TokenTypes.TT_EOF))
      return false;
    this.tokenBuffer.popToken();
    node.addChild(new ASTNode(NodeTypes.TMP));
    return true;
  }
}

class FalseRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    this.tokenBuffer.popToken();
    return false;
  }
}

let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;
let trueRule: Rule           = null;
let falseRule: Rule          = null;

describe("Recursive Descent Tests", () => {
  beforeEach(() => {
    tokenBuffer = new TokenBuffer(LIST_OF_ONE);
    root        = new ASTNode(NodeTypes.TMP);
    trueRule    = new TrueRule(tokenBuffer);
    falseRule   = new FalseRule(tokenBuffer);
  });
  describe("SequenceRule Tests", () => {
    it("can construct a SequenceRule", () => {
      const sequence: Rule = new SequenceRule(tokenBuffer, [trueRule, trueRule, trueRule]);
      assert.isNotNull(sequence);
    });
    it("can parse with a successful child Rule (single child Rule)", () => {
      const sequence: Rule = new SequenceRule(tokenBuffer, [trueRule]);
      assert.isTrue(sequence.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three child Rules)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      trueRule         = new TrueRule(tokenBuffer);
      const sequence: Rule = new SequenceRule(tokenBuffer, [trueRule, trueRule, trueRule]);
      assert.isTrue(sequence.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with an unsuccessful child Rule (three child Rules)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      trueRule         = new TrueRule(tokenBuffer);
      falseRule        = new FalseRule(tokenBuffer);
      const sequence: Rule = new SequenceRule(tokenBuffer, [trueRule, falseRule, trueRule]);
      assert.isFalse(sequence.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("OptionalRule Tests", () => {
    it("can construct an OptionalRule", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OptionalRule(tokenBuffer, child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule", () => {
      const optional: Rule = new OptionalRule(tokenBuffer, trueRule);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const optional: Rule  = new OptionalRule(tokenBuffer, falseRule);
      assert.isTrue(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("ManyRule Tests", () => {
    it("can construct a ManyRule", () => {
      const many: Rule = new ManyRule(tokenBuffer, trueRule);
      assert.isNotNull(many);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const many: Rule = new ManyRule(tokenBuffer, trueRule);
      assert.isTrue(many.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      trueRule         = new TrueRule(tokenBuffer);
      const many: Rule = new ManyRule(tokenBuffer, trueRule);
      assert.isTrue(many.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const many: Rule = new ManyRule(tokenBuffer, falseRule);
      assert.isTrue(many.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("OneOrManyRule Tests", () => {
    it("can construct a OneOrManyRule", () => {
      const oneOrMany: Rule = new OneOrManyRule(tokenBuffer, trueRule);
      assert.isNotNull(oneOrMany);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const oneOrMany: Rule = new OneOrManyRule(tokenBuffer, trueRule);
      assert.isTrue(oneOrMany.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      trueRule              = new TrueRule(tokenBuffer);
      const oneOrMany: Rule = new OneOrManyRule(tokenBuffer, trueRule);
      assert.isTrue(oneOrMany.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const oneOrMany: Rule = new OneOrManyRule(tokenBuffer, falseRule);
      assert.isFalse(oneOrMany.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("KeywordRule Tests", () => {
    it("can construct a KeywordRule", () => {
      const keywordRule: Rule = new KeywordRule(tokenBuffer, TokenTypes.TT_STRING);
      assert.isNotNull(keywordRule);
    });
    it("can parse with a successful child Rule", () => {
      const keywordRule: Rule = new KeywordRule(tokenBuffer, TokenTypes.TT_STRING);
      assert.isTrue(keywordRule.parse(root));
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const keywordRule: Rule = new KeywordRule(tokenBuffer, TokenTypes.TT_STORE);
      assert.isFalse(keywordRule.parse(root));
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
});