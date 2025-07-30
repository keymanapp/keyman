/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-30
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Verify against BNF)
 */

import 'mocha';
import { assert } from 'chai';
import { readFileSync } from 'fs';

interface Dictionary {
  [key: string]: string;
}

describe("Verify Parser Against BNF Tests", () => {
  it("matches the BNF grammar rules", () => {
    const bnfBuffer: string = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-file.bnf').toString();
    const bnfRules: Dictionary = getBnfRules(bnfBuffer);
    const sourceBuffer: string = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-analyser.ts').toString();
    const sourceRules: Dictionary = getSourceRules(sourceBuffer);
    assert.equal(sourceRules, {});
    assert.equal(bnfRules, {});
    assert.isTrue(true);
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

function getSourceRules(buffer:string): Dictionary {
    const rules: Dictionary = {};
    const matches = buffer.matchAll(/export class (\S+)Rule.*?constructor\(\)\s*\{[^}]*this.rule\s*=\s*new([^;}]*)[^}]*\}/sg);
    for (let match of matches) {
      const name = lowerCaseFirstLetter(match[1]);
      rules[name] = removeWhiteSpace(match[2]);
      rules[name] = replaceSequenceRule(rules[name]);
      rules[name] = replaceAlternateRule(rules[name]);
      rules[name] = replaceOneOrManyRule(rules[name]);
      rules[name] = replaceManyRule(rules[name]);
      rules[name] = replaceOptElements(rules[name]);
      rules[name] = replaceOneOrManyElements(rules[name]);
      rules[name] = replaceManyElements(rules[name]);
    }
    return rules;
}

function lowerCaseFirstLetter(str: string): string {
  return (str == null || str.length < 1) ? str :
    str.charAt(0).toLowerCase() + str.slice(1);
}

function removeWhiteSpace(str: string): string {
  return str.replaceAll(/\s/g, '');
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

function replaceOptElements(str: string): string {
  if (str != null && str.includes('opt')) {
    str = str.replaceAll(/(?:opt)(\S)(\S*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'?'; });
  }
  return str;
}

function replaceOneOrManyElements(str: string): string {
  if (str != null && str.includes('oneOrMany')) {
    str = str.replaceAll(/(?:oneOrMany)(\S)(\S*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'+'; });
  }
  return str;
}

function replaceManyElements(str: string): string {
  if (str != null && str.includes('many')) {
    str = str.replaceAll(/(?:many)(\S)(\S*)/g,
      (match, p1, p2, offset, string, groups) =>
        { return p1.toLowerCase()+p2+'*'; });
  }
  return str;
}
