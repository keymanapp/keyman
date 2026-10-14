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
  it("really matches the BNF grammar rules", () => {
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

interface MemberInfo {
    name: string;
    type: string;
    kind: "property" | "parameter" | "parameter-property";
}

interface ClassInfo {
    name:    string;
    members: MemberInfo[];
}

interface RuleInfo {
    name: string;
    expression: string | undefined;
}

function getSourceRules2(buffer:string): Dictionary {
    const rules: Dictionary = {};

    const sourceFile = ts.createSourceFile(
      "",
      buffer,
      ts.ScriptTarget.Latest,
      true,
      ts.ScriptKind.TS
    );

    function getType(n: ts.ParameterDeclaration | ts.PropertyDeclaration): string {
      let result = n.type ? n.type.getText(sourceFile) : "(no explicit type)";

      if (n.questionToken) result += "?";

      if (ts.isPropertyDeclaration(n) && n.exclamationToken) {
        result += "!";
      }

      return result;
    }

    const classes: ClassInfo[] = [];
    const extractedRules: RuleInfo[] = [];

    function visit(n: ts.Node): void {
      if (ts.isClassDeclaration(n) ||
        ts.isClassExpression(n)) {

        const name = n.name?.text ?? "(anonymous class)";
        const members: MemberInfo[] = [];

        for (const member of n.members) {
          if (ts.isPropertyDeclaration(member)) {
            members.push({
              name: member.name.getText(sourceFile),
              type: getType(member),
              kind: "property"
            });
          }

          if (ts.isConstructorDeclaration(member)) {
            for (const param of member.parameters) {
              const isProperty = ts.isParameterPropertyDeclaration(param, member);
              members.push({
                name: param.name.getText(sourceFile),
                type: getType(param),
                kind: isProperty
                  ? "parameter-property"
                  : "parameter"
              });
            }
          }

          extractedRules.push({
            name,
            expression: inspectRuleExpression(n, sourceFile)
          });
        }

        classes.push({ name, members });
      }

      ts.forEachChild(n, visit);
    }

    visit(sourceFile);

    return rules;
}

function inspectRuleExpression(
  cls: ts.ClassDeclaration | ts.ClassExpression,
  sourceFile: ts.SourceFile
): string | undefined {
  const constructor = cls.members.find(ts.isConstructorDeclaration);

  if (!constructor?.body) return undefined;

  // Map local variable names to their initializer expressions.
  const variables = new Map<string, ts.Expression>();

  for (const statement of constructor.body.statements) {
    if (ts.isVariableStatement(statement)) {
      for (const declaration of statement.declarationList.declarations) {
        if (ts.isIdentifier(declaration.name) && declaration.initializer) {
          variables.set(declaration.name.text, declaration.initializer);
        }
      }
    }
  }

  // Find: this.rule = expression
  let ruleExpression: ts.Expression | undefined;

  for (const statement of constructor.body.statements) {
    if (!ts.isExpressionStatement(statement)) continue;

    const expression = statement.expression;

    if (ts.isBinaryExpression(expression) &&
        expression.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
        ts.isPropertyAccessExpression(expression.left) &&
        expression.left.expression.kind === ts.SyntaxKind.ThisKeyword &&
        expression.left.name.text === "rule"
    ) {
        ruleExpression = expression.right;
      }
  }

  if (!ruleExpression) return undefined;

  function describe(expression: ts.Expression): string {
    if (ts.isIdentifier(expression)) {
       const initializer = variables.get(expression.text);

      return initializer ? describe(initializer) : expression.text;
    }

    if (ts.isNewExpression(expression)) {
      const name = expression.expression.getText(sourceFile);
      const args: readonly ts.Expression[] = expression.arguments ?? [];

      if (name === "AlternateRule" && args.length > 0 &&
        ts.isArrayLiteralExpression(args[0]))
        {
          return "AlternateRule\n" +
            args[0].elements
            .map(e => "  " + describe(e))
            .join("\n");
        }

      return name + (args.length
        ? "(" + args.map(describe).join(", ") + ")"
        : "");
    }

    return expression.getText(sourceFile);
  }

  return describe(ruleExpression);
}