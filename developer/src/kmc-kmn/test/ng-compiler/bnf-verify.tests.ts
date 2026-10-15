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
  it.skip("really matches the BNF grammar rules", () => {
    //const bnfBuffer: string = readFileSync(BNF_FILENAME).toString();
    //const bnfRules: Dictionary = getBnfRules(bnfBuffer);
    const sourceBuffer: string = [
      '../../src/kmc-kmn/src/ng-compiler/kmn-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/statement-analyzer.ts',
      '../../src/kmc-kmn/src/ng-compiler/store-analyzer.ts',
    ].reduce((str, filename) => { return str + readFileSync(filename).toString(); }, '');
    const sourceRules: Dictionary = getSourceRules2(sourceBuffer);
    //assert.deepEqual(bnfRules, sourceRules);
    assert.deepEqual(null, sourceRules);
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
      rules[match[1]] = replaceElementNames(match[2]);
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

function replaceElementNames(str: string): string {
  if (str != null) {
    str = str.replaceAll(/\b([a-zA-Z]+)_([a-zA-Z])([a-zA-Z]*)\b/g,
      (match, p1, p2, p3, offset, string, groups) =>
        { return p1.toLowerCase() + p2.toUpperCase() + p3.toLowerCase(); });
    str = str.replaceAll(/\b([A-Z]+)\b/g,
      (match, p1, offset, string, groups) => { return p1.toLowerCase(); })
    str = str.replaceAll(/\b(if|string|return)\b/g,
      (match, p1, offset, string, groups) => { return p1 + 'Rule'; })
    str = str.replaceAll(/\b(baselayout|layer|platform)Shortcut\b/g, '$1');
    str = str.replaceAll(/\bleftBr\b/g, 'leftBracket');
    str = str.replaceAll(/\brightBr\b/g, 'rightBracket');
    str = str.replaceAll(/\bleftSq\b/g, 'leftSquare');
    str = str.replaceAll(/\brightSq\b/g, 'rightSquare');
  }
  return str;
}

function getSourceRules(buffer:string): Dictionary {
    const rules: Dictionary = {};
    // Rules that extend AlternateTokenRule
    let matches = buffer.matchAll(/export\s+class\s+(\S+)Rule\s+extends\s+AlternateTokenRule.+?super\(\[([^\]]+)/sg);
    for (const match of matches) {
      const name  = lowerCaseFirstLetter(match[1]);
      rules[name] = removeOneLineComments(match[2]);
      rules[name] = removeWhiteSpace(rules[name]);
      rules[name] = replaceCommas(rules[name]);
      rules[name] = replaceTokenElements(rules[name]);
      rules[name] = replaceElementNames(rules[name]);
    }
    // Other Rules
    matches = buffer.matchAll(/export\s+class\s+(\S+)Rule\s+extends\s+(?!AlternateTokenRule).+?constructor\(\)\s*\{[^}]+this.rule\s*=\s*new([^;}]+)[^}]+\}/sg);
    for (const match of matches) {
      const name  = lowerCaseFirstLetter(match[1]);
      rules[name] = removeWhiteSpace(match[2]);
      rules[name] = replaceSequenceRule(rules[name]);
      rules[name] = replaceAlternateRule(rules[name]);
      rules[name] = replaceOneOrManyRule(rules[name]);
      rules[name] = replaceManyRule(rules[name]);
      rules[name] = replaceThisOnElements(rules[name]);
      rules[name] = replaceOptElements(rules[name]);
      rules[name] = replaceOneOrManyElements(rules[name]);
      rules[name] = replaceManyElements(rules[name]);
      rules[name] = replaceElementNames(rules[name]);
    }
    return rules;
}

function lowerCaseFirstLetter(str: string): string {
  return (str == null || str.length < 1) ? str :
    str.charAt(0).toLowerCase() + str.slice(1);
}

function removeOneLineComments(str: string): string {
  return str.replaceAll(/\/\/[^\r\n]*/g, '');
}

function removeWhiteSpace(str: string): string {
  return str.replaceAll(/\s/g, '');
}

function replaceCommas(str: string): string {
  if (str != null) {
    str = str.replace(/,$/, '');
    str = str.replaceAll(/,/g, '|');
  }
  return str;
}

function replaceSequenceRule(str: string): string {
  if (str != null && str.includes('SequenceRule')) {
    str = str.replace(/^SequenceRule\(\[/, '');
    str = str.replace(/,?\]\)$/, '');
    str = str.replaceAll(/,/g, ' ');
  }
  return str;
}

function replaceAlternateRule(str: string): string {
  if (str != null && str.includes('AlternateRule')) {
    str = str.replace(/^AlternateRule\(\[/, '');
    str = str.replace(/,?\]\)$/, '');
    str = str.replaceAll(/,/g, '|');
  }
  return str;
}

function replaceOneOrManyRule(str: string): string {
  if (str != null && str.includes('OneOrManyRule')) {
    str = str.replace(/^OneOrManyRule\(/, '');
    str = str.replace(/\)$/, '');
    str = replaceSubRule(str);
    str = str + '+';
  }
  return str;
}

function replaceManyRule(str: string): string {
  if (str != null && str.includes('ManyRule')) {
    str = str.replace(/ManyRule\(/, '');
    str = str.replace(/\)$/, '');
    str = replaceSubRule(str);
    str = str + '*';
  }
  return str;
}

function replaceThisOnElements(str: string): string {
  if (str != null && str.includes('this.')) {
    str = str.replaceAll(/(?:this\.)([^|\s]*)/g, '$1')
  }
  return str;
}

function replaceOptElements(str: string): string {
  if (str != null && str.includes('opt')) {
    str = str.replaceAll(/(?:opt)(\S)([^|\s]*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'?'; });
  }
  return str;
}

function replaceOneOrManyElements(str: string): string {
  if (str != null && str.includes('oneOrMany')) {
    str = str.replaceAll(/(?:oneOrMany)(\S)([^|\s]*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'+'; });
  }
  return str;
}

function replaceManyElements(str: string): string {
  if (str != null && str.includes('many')) {
    str = str.replaceAll(/(?:many)(\S)([^|\s]*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'*'; });
  }
  return str;
}

function replaceTokenElements(str: string): string {
  if (str != null && str.includes('TokenType.')) {
    str = str.replaceAll(/(?:TokenType\.)([^|\s]*)/g,
      (match, p1, offset, string, groups) =>
        { return p1.toLowerCase(); });
  }
  return str;
}

function replaceSubRule(str: string): string {
  if (str != null && str.includes('new')) {
    str = str.replaceAll(/(?:new)(\S+)Rule\(\)/g,
      (match, p1, offset, string, groups) =>
        { return lowerCaseFirstLetter(p1); });
  }
  return str;
}

interface RuleInfo {
    name: string;
    expression: string | undefined;
}

function getSourceRules2(buffer: string): Dictionary {
  const rules: Dictionary = {};

  const sourceFile = ts.createSourceFile(
    "combined-source.ts",
    buffer,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS
  );

  const extractedRules: RuleInfo[] = [];

  function visit(node: ts.Node): void {
    if (ts.isClassDeclaration(node) || ts.isClassExpression(node)) {
      const name = node.name?.text;

      if (name?.endsWith("Rule")) {
        extractedRules.push({
          name,
          expression: inspectRuleExpression(node, sourceFile)
        });
      }
    }

    ts.forEachChild(node, visit);
  }

  visit(sourceFile);

  // Set a breakpoint here to inspect extractedRules.
  return rules;
}

function inspectRuleExpression(
  cls: ts.ClassDeclaration | ts.ClassExpression,
  sourceFile: ts.SourceFile
): string | undefined {
  const constructor = cls.members.find(ts.isConstructorDeclaration);

  if (!constructor?.body) return undefined;

  const variables = new Map<string, ts.Expression>();

  for (const statement of constructor.body.statements) {
    if (!ts.isVariableStatement(statement)) continue;

    for (const declaration of statement.declarationList.declarations) {
      if (ts.isIdentifier(declaration.name) && declaration.initializer) {
        variables.set(declaration.name.text, declaration.initializer);
      }
    }
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
    if (ts.isIdentifier(node)) {
      const initializer = variables.get(node.text);
      return initializer ? describe(initializer) : node.text;
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
            ? args[0].elements.map(describe).join(" | ")
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
            ? `${name}(${args.map(describe).join(", ")})`
            : name;
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
          ).join(" | ");
        }
      }
    }

    return undefined;
  }

  return describe(expression);
}