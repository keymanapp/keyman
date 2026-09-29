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
    str = str + '+';
  }
  return str;
}

function replaceManyRule(str: string): string {
  if (str != null && str.includes('ManyRule')) {
    str = str.replace(/ManyRule\(/, '');
    str = str.replace(/\)$/, '');
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
