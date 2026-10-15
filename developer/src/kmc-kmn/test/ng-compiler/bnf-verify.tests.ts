/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-30
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Verify against BNF)
 */

/*
 * This grammar rule test is somewhat brittle as it depends on the local variable
 * names used in the Rule assigned to this.rule in each of the implemented source
 * code rules. These are processed and then checked against the processed
 * non-terminals and terminals in the BNF grammar rules for equality. A more thorough
 * test would check the actual classes of each source code variable, but would
 * require much more parsing of the TypeScript source code.
 */

import 'mocha';
import { assert } from 'chai';
import ts from "typescript";
import { readFileSync } from 'node:fs';
import { TokenType } from '../../src/ng-compiler/token-type.js';

interface Dictionary {
  [key: string]: string;
}

const BNF_FILENAME = '../../src/kmc-kmn/src/ng-compiler/kmn-file.bnf';

describe("Verify Parser Against BNF Tests", () => {
  it("matches the BNF grammar rules", () => {
    const bnfBuffer: string = readFileSync(BNF_FILENAME).toString();
    const bnfRules: Dictionary = getBnfRules(bnfBuffer);
    const sourceBuffer: string = [
      '../../src/kmc-kmn/src/ng-compiler/kmn-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/statement-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/store-analyzer.ts',
    ].reduce((str, filename) => { return str + readFileSync(filename).toString(); }, '');
    const sourceRules: Dictionary = getSourceRules(sourceBuffer);
    assert.deepEqual(bnfRules, sourceRules);
  });
});
describe("Verify BNF Against Lexer Tests", () => {
  it("contains all the Lexer tokens", () => {
    const bnfBuffer: string = readFileSync(BNF_FILENAME).toString()
    const match = [...bnfBuffer.matchAll(/[A-Z_]{2,}/g)];
    const bnfTokens = match.map((x) => x[0]);
    bnfTokens.push('COMMENT', 'CONTINUATION', 'EOF', 'WHITESPACE');
    bnfTokens.sort();
    const bnfTokenSet = new Set<String>(bnfTokens);
    const tokenTypes: string[] = Object.keys(TokenType).sort();
    assert.deepEqual([...bnfTokenSet], tokenTypes);
  });
});

function getBnfRules(buffer:string): Dictionary {
  const rules: Dictionary = {};
  buffer = removeComments(buffer);
  buffer = wrapLines(buffer);
  const lines: string[] = buffer.split(/\r\n|\n|\r/g);
  lines.forEach((line) => {
    const match = line.match(/(.*)\s*:\s*(.*)/)
    if (match) {
      rules[match[1]] = match[2];
    }
  });
  return rules;
}

function removeComments(buffer:string): string {
  return buffer.replaceAll(/#(.*)(\r\n|\n|\r)/g, '');
}

function wrapLines(buffer:string): string {
  return buffer.replaceAll(/[^\S\r\n]*(\r\n|\n|\r)[^\S\r\n]+/g, '');
}

function getSourceRules(buffer: string): Dictionary {
  const rules: Dictionary = {};

  const sourceFile = ts.createSourceFile(
    "combined-source.ts",
    buffer,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS
  );

  visit(sourceFile, sourceFile, rules);

  return rules;
}

function visit(node: ts.Node, sourceFile: ts.SourceFile, rules: Dictionary): void {
  if (ts.isClassDeclaration(node) || ts.isClassExpression(node)) {
    const className = node.name?.text;

    if (className?.endsWith("Rule")) {
      const name       = lowerFirst(className.replace(/Rule$/, ""));
      const expression = inspectRuleExpression(node, sourceFile);

      if (expression !== undefined) {
        rules[name] = expression;
      }
    }
  }

  ts.forEachChild(node, child => visit(child, sourceFile, rules));
}

function inspectRuleExpression(
  cls: ts.ClassDeclaration | ts.ClassExpression,
  sourceFile: ts.SourceFile
): string | undefined {
  const constructor = cls.members.find(ts.isConstructorDeclaration);

  if (!constructor?.body) return undefined;

  const variables  = new Map<string, ts.Expression>();
  const properties = new Map<string, ts.Expression>();

  for (const statement of constructor.body.statements) {
    if (!ts.isVariableStatement(statement)) continue;

    for (const declaration of statement.declarationList.declarations) {
      if (ts.isIdentifier(declaration.name) && declaration.initializer) {
        variables.set(declaration.name.text, declaration.initializer);
      }
    }
  }

  let currentClass: ts.ClassDeclaration | ts.ClassExpression | undefined = cls;

  while (currentClass) {
    const constructor = currentClass.members.find(ts.isConstructorDeclaration);

    if (constructor?.body) {
      for (const statement of constructor.body.statements) {
        if (
          ts.isExpressionStatement(statement) &&
          ts.isBinaryExpression(statement.expression) &&
          statement.expression.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
          ts.isPropertyAccessExpression(statement.expression.left) &&
          statement.expression.left.expression.kind === ts.SyntaxKind.ThisKeyword
        ) {
          properties.set(
            statement.expression.left.name.text,
            statement.expression.right
          );
        }
      }
    }

    const parentName = currentClass.heritageClauses
      ?.find(clause => clause.token === ts.SyntaxKind.ExtendsKeyword)
      ?.types[0]?.expression.getText(sourceFile);

    currentClass = parentName
      ? findClassDeclaration(parentName, sourceFile)
      : undefined;
  }

  const alternateTokens = inspectAlternateTokenRule(cls);
    if (alternateTokens !== undefined) {
      return alternateTokens;
  }

  const assignment = constructor.body.statements.find(statement =>
    ts.isExpressionStatement(statement) &&
    ts.isBinaryExpression(statement.expression) &&
    statement.expression.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
    ts.isPropertyAccessExpression(statement.expression.left) &&
    statement.expression.left.expression.kind === ts.SyntaxKind.ThisKeyword &&
    statement.expression.left.name.text === "rule"
  );

  if (!assignment || !ts.isExpressionStatement(assignment)) {
    return undefined;
  }

  const expression = (assignment.expression as ts.BinaryExpression).right;

  function describe(node: ts.Expression): string {
    if (
      ts.isPropertyAccessExpression(node) &&
      node.expression.kind === ts.SyntaxKind.ThisKeyword
    ) {
      const initializer = properties.get(node.name.text);
      return initializer ? describe(initializer) : node.name.text;
    }

    if (ts.isIdentifier(node)) {
      const initializer = variables.get(node.text);
      return initializer ? describe(initializer) : lowerFirst(node.text.replace(/Rule$/, ""));
    }

    if (ts.isArrayLiteralExpression(node)) {
      return node.elements.map(describe).join(", ");
    }

    if (ts.isNewExpression(node)) {
      const name = node.expression.getText(sourceFile);
      const args: readonly ts.Expression[] = node.arguments ?? [];

      switch (name) {
        case "AlternateRule":
          return args.length && ts.isArrayLiteralExpression(args[0])
            ? args[0].elements.map(describe).join("|")
            : `AlternateRule(${args.map(describe).join(", ")})`;

        case "SequenceRule":
          return args.length && ts.isArrayLiteralExpression(args[0])
            ? args[0].elements.map(describe).join(" ")
            : `SequenceRule(${args.map(describe).join(", ")})`;

        case "TokenRule":
          if (args.length > 0 && ts.isPropertyAccessExpression(args[0])) {
            return args[0].name.text;
          }
          return `TokenRule(${args.map(describe).join(", ")})`;

        case "ManyRule":
          return args.length > 0
            ? `${describe(args[0])}*`
            : "ManyRule";

        case "OneOrManyRule":
          return args.length > 0
            ? `${describe(args[0])}+`
            : "OneOrManyRule";

        case "OptionalRule":
          return args.length > 0
            ? `${describe(args[0])}?`
            : "OptionalRule";

        default:
          return args.length
            ? `${lowerFirst(name.replace(/Rule$/, ""))}(${args.map(describe).join(", ")})`
            : lowerFirst(name.replace(/Rule$/, ""));
      }
    }

    return node.getText(sourceFile);
  }

  function inspectAlternateTokenRule(
    cls: ts.ClassDeclaration | ts.ClassExpression
  ): string | undefined {
    if (cls.heritageClauses?.some(clause =>
      clause.token === ts.SyntaxKind.ExtendsKeyword &&
      clause.types.some(type => type.expression.getText() === "AlternateTokenRule")
    )) {
      const constructor = cls.members.find(ts.isConstructorDeclaration);
      const superCall = constructor?.body?.statements.find(statement =>
        ts.isExpressionStatement(statement) &&
        ts.isCallExpression(statement.expression) &&
        statement.expression.expression.kind === ts.SyntaxKind.SuperKeyword
      );

      if (superCall && ts.isExpressionStatement(superCall)) {
        const args = (superCall.expression as ts.CallExpression).arguments;
        const tokens = args[0];

        if (tokens && ts.isArrayLiteralExpression(tokens)) {
          return tokens.elements.map(token =>
            ts.isPropertyAccessExpression(token)
              ? token.name.text
              : token.getText()
          ).join("|");
        }
      }
    }

    return undefined;
  }

  return describe(expression);
}

function lowerFirst(text: string): string {
  return text.charAt(0).toLowerCase() + text.slice(1);
}

function findClassDeclaration(
  name: string,
  sourceFile: ts.SourceFile
): ts.ClassDeclaration | undefined {
  let result: ts.ClassDeclaration | undefined;

  function visit(node: ts.Node): void {
    if (
      ts.isClassDeclaration(node) &&
      node.name?.text === name
    ) {
      result = node;
      return;
    }

    ts.forEachChild(node, visit);
  }

  visit(sourceFile);
  return result;
}