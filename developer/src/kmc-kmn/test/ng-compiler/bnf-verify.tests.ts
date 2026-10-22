/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-30
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Verify against BNF)
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
const BNF_BUFFER: string = readFileSync(BNF_FILENAME).toString();

describe("Verify BNF Against Lexer Tests", () => {
  it("contains all the Lexer tokens", () => {
    const match = [...BNF_BUFFER.matchAll(/[A-Z_]{2,}/g)];
    const bnfTokens = match.map((x) => x[0]);
    bnfTokens.push('COMMENT', 'CONTINUATION', 'EOF', 'WHITESPACE');
    bnfTokens.sort();
    const bnfTokenSet = new Set<String>(bnfTokens);
    const tokenTypes: string[] = Object.keys(TokenType).sort();
    assert.deepEqual([...bnfTokenSet], tokenTypes);
  });
});
describe("getSourceRules() Tests", () => {
  it("can handle a rule with one variable", () => {
    const sourceBuffer = `
      import { SingleChildRule, TokenRule } from '../../src/ng-compiler/recursive-descent.js';
      import { TokenType } from '../../src/ng-compiler/token-type.js';

      export class DummyRule extends SingleChildRule {
        public constructor() {
          super();
          const dummyRule = new TokenRule(TokenType.STRING, true);
          this.rule       = dummyRule;
        }
      }`;
      const sourceRules = getSourceRules(sourceBuffer);
      assert.deepEqual({ dummy: "STRING" }, sourceRules);
  });
});
describe("Verify Parser Against BNF Tests", () => {
  it("matches the BNF grammar rules", () => {
    const bnfRules    = getBnfRules(BNF_BUFFER);
    const sourceBuffer: string = [
      '../../src/kmc-kmn/src/ng-compiler/kmn-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/statement-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/store-analyzer.ts',
  ].reduce((str, filename) => { return str + readFileSync(filename).toString(); }, '');
    const sourceRules = getSourceRules(sourceBuffer);
    assert.deepEqual(bnfRules, sourceRules);
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
    "",
    buffer,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS
  );

  visitRules(sourceFile, sourceFile, rules);

  return rules;
}

function visitRules(
  node: ts.Node,
  sourceFile: ts.SourceFile,
  rules: Dictionary
): void {
  if ((ts.isClassDeclaration(node) || ts.isClassExpression(node)) &&
    node.name?.text.endsWith("Rule")
  ) {
    const name = baseName(node.name.text);
    const expr = inspectRuleExpression(node, sourceFile);

    if (expr !== undefined) {
      rules[name] = expr;
    }
  }

  ts.forEachChild(node, child => visitRules(child, sourceFile, rules));
}

function baseName(text: string): string {
  return (text.charAt(0).toLowerCase() + text.slice(1)).replace(/Rule$/, "");
}

type RuleContext = {
  variables: Map<string, ts.Expression>;
  properties: Map<string, ts.Expression>;
  sourceFile: ts.SourceFile;
};

function inspectRuleExpression(
  cls: ts.ClassDeclaration | ts.ClassExpression,
  sourceFile: ts.SourceFile
): string | undefined {
  const constructor = getConstructor(cls);

  if (!constructor?.body) {
    return undefined;
  }

  // It is assumed parent constructors do not use local variables in property
  // assignments, as the Maps do not record constructor context
  const variables  = new Map<string, ts.Expression>();
  const properties = new Map<string, ts.Expression>();

  const context: RuleContext = { variables, properties, sourceFile };

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
    const parentConstructor = getConstructor(currentClass);

    if (parentConstructor?.body) {
      for (const statement of parentConstructor.body.statements) {
        const assignment = getThisPropertyAssignment(statement);

        if (assignment) {
          const propertyName = (assignment.left as ts.PropertyAccessExpression).name.text;
          if (!properties.has(propertyName)) { // child supercedes parent
            properties.set(propertyName, assignment.right);
          }
        }
      }
    }

    const parentName = getParentClassName(currentClass, sourceFile);

    currentClass = parentName
      ? findClassDeclaration(parentName, sourceFile)
      : undefined;
  }

  const alternateTokens = inspectAlternateTokenRule(cls);

  if (alternateTokens !== undefined) {
    return alternateTokens;
  }

  const assignment = findThisPropertyAssignment(constructor.body.statements, "rule");

  if (!assignment) {
    return undefined;
  }

  return describeExpression(assignment.right, context);
}

function getConstructor(
  cls: ts.ClassDeclaration | ts.ClassExpression
): ts.ConstructorDeclaration | undefined {
  return cls.members.find(ts.isConstructorDeclaration);
}

function getParentClassName(
  cls: ts.ClassDeclaration | ts.ClassExpression,
  sourceFile: ts.SourceFile
): string | undefined {
  const clause = cls.heritageClauses?.find(
    clause => clause.token === ts.SyntaxKind.ExtendsKeyword
  );

  return clause?.types[0]?.expression.getText(sourceFile);
}

function getThisPropertyAssignment(
  statement: ts.Statement
): ts.BinaryExpression | undefined {
  if (!ts.isExpressionStatement(statement)) {
    return undefined;
  }

  const expression = statement.expression;

  if (
    !ts.isBinaryExpression(expression) ||
    expression.operatorToken.kind !== ts.SyntaxKind.EqualsToken ||
    !ts.isPropertyAccessExpression(expression.left) ||
    expression.left.expression.kind !== ts.SyntaxKind.ThisKeyword
  ) {
    return undefined;
  }

  return expression;
}

function findThisPropertyAssignment(
  statements: ts.NodeArray<ts.Statement>,
  propertyName: string
): ts.BinaryExpression | undefined {
  return statements
    .map(getThisPropertyAssignment)
    .find(assignment =>
      assignment !== undefined &&
      (assignment.left as ts.PropertyAccessExpression).name.text === propertyName
    );
}

function inspectAlternateTokenRule(
  cls: ts.ClassDeclaration | ts.ClassExpression
): string | undefined {
  const extendsAlternateTokenRule = cls.heritageClauses?.some(clause =>
    clause.token === ts.SyntaxKind.ExtendsKeyword &&
    clause.types.some(type => type.expression.getText() === "AlternateTokenRule")
  );

  if (!extendsAlternateTokenRule) {
    return undefined;
  }

  const constructor = getConstructor(cls);
  const statements  = constructor?.body?.statements;

  if (!statements) {
    return undefined;
  }

  const superCall = getSuperCall(statements);
  const tokens    = superCall?.arguments[0];

  if (!tokens || !ts.isArrayLiteralExpression(tokens)) {
    return undefined;
  }

  return tokens.elements
    .map(token =>
      ts.isPropertyAccessExpression(token)
        ? token.name.text
        : token.getText()
    )
    .join("|");
}

function getSuperCall(
  statements: ts.NodeArray<ts.Statement>
): ts.CallExpression | undefined {
  for (const statement of statements) {
    if (
      ts.isExpressionStatement(statement) &&
      ts.isCallExpression(statement.expression) &&
      statement.expression.expression.kind === ts.SyntaxKind.SuperKeyword
    ) {
      return statement.expression;
    }
  }

  return undefined;
}

// separator, suffix and token describe the special handling for each rule type
type RuleDescription = {
  separator?: string;
  suffix?: string;
  token?: boolean;
};

const ruleDescriptions = new Map<string, RuleDescription>([
  ["AlternateRule", { separator: "|" }],
  ["SequenceRule",  { separator: " " }],
  ["TokenRule",     { token: true }],
  ["ManyRule",      { suffix: "*" }],
  ["OneOrManyRule", { suffix: "+" }],
  ["OptionalRule",  { suffix: "?" }],
]);

function describeExpression(
  node: ts.Expression,
  context: RuleContext
): string {
  if (
    ts.isPropertyAccessExpression(node) &&
    node.expression.kind === ts.SyntaxKind.ThisKeyword
  ) {
    const initializer = context.properties.get(node.name.text);
    return initializer
      ? describeExpression(initializer, context)
      : node.name.text;
  }

  if (ts.isIdentifier(node)) {
    const initializer = context.variables.get(node.text);
    return initializer
      ? describeExpression(initializer, context)
      : baseName(node.text);
  }

  if (ts.isNewExpression(node)) {
    const name = node.expression.getText(context.sourceFile);
    const args: readonly ts.Expression[] = node.arguments ?? [];

    // Describe each argument using the same context
    const describeArgs = () =>
      args.map(arg => describeExpression(arg, context));

    const rule = ruleDescriptions.get(name);

    if (rule?.token) {
      if (args.length > 0 && ts.isPropertyAccessExpression(args[0])) {
        return args[0].name.text;
      }
      return `TokenRule(${describeArgs().join(", ")})`;
    }

    if (rule?.separator !== undefined) {
      return args.length > 0 && ts.isArrayLiteralExpression(args[0])
        ? describeArray(args[0].elements, context, rule.separator)
        : `${name}(${describeArgs().join(", ")})`;
    }

    if (rule?.suffix !== undefined) {
      return args.length > 0
        ? `${describeExpression(args[0], context)}${rule.suffix}`
        : name;
    }

    const ruleName = baseName(name);

    return args.length
      ? `${ruleName}(${describeArgs().join(", ")})`
      : ruleName;
  }

  return node.getText(context.sourceFile);
}

function describeArray(
  elements: ts.NodeArray<ts.Expression>,
  context: RuleContext,
  separator: string
): string {
  return elements
    .map(element => describeExpression(element, context))
    .join(separator);
}

function findClassDeclaration(
  name: string,
  sourceFile: ts.SourceFile
): ts.ClassDeclaration | undefined {
  let result: ts.ClassDeclaration | undefined;

  function visit(node: ts.Node): void {
    if (result) return;

    if (ts.isClassDeclaration(node) && node.name?.text === name) {
      result = node;
      return;
    }

    ts.forEachChild(node, visit);
  }

  visit(sourceFile);

  return result;
}
