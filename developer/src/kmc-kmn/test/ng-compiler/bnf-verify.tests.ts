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
    const sourceBuffer: string = [
      '../../src/kmc-kmn/src/ng-compiler/kmn-analyser.ts',
      '../../src/kmc-kmn/src/ng-compiler/statement-analyser.ts',
      '../../src/kmc-kmn/src/ng-compiler/store-analyser.ts',
    ].reduce((str, filename) => { return str + readFileSync(filename).toString(); }, '');
    const sourceRules: Dictionary = getSourceRules(sourceBuffer);
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
    str = str.replaceAll(/\b([A-Z_]+)\b/g,
      (match, p1, offset, string, groups) => { return p1.toLowerCase(); });
    str = str.replaceAll(/\bleft_br\b/g, 'leftBracket');
    str = str.replaceAll(/\bright_br\b/g, 'rightBracket');
    str = str.replaceAll(/\bleft_sq\b/g, 'leftSquare');
    str = str.replaceAll(/\bright_sq\b/g, 'rightSquare');
    str = str.replaceAll(/\bif\b/g, 'ifRule');
    str = str.replaceAll(/\bstring\b/g, 'stringRule');
    str = str.replaceAll(/\breturn\b/g, 'returnRule');
    str = str.replaceAll(/\bkey_code\b/g, 'keyCode');
    str = str.replaceAll(/\bnot_equal\b/g, 'notEqual');
    str = str.replaceAll(/\bu_char\b/g, 'uChar');
    str = str.replaceAll(/\bnamed_constant\b/g, 'namedConstant');
    str = str.replaceAll(/\bbaselayout_shortcut\b/g, 'baselayout');
    str = str.replaceAll(/\blayer_shortcut\b/g, 'layer');
    str = str.replaceAll(/\bplatform_shortcut\b/g, 'platform');
    str = str.replaceAll(/\bdeadkey\b/g, 'deadKey');
    str = str.replaceAll(/\bnewlayer\b/g, 'newLayer');
    str = str.replaceAll(/\bnomatch\b/g, 'noMatch');
    str = str.replaceAll(/\bnotany\b/g, 'notAny');
    str = str.replaceAll(/\boldlayer\b/g, 'oldLayer');
  }
  return str;
}

function getSourceRules(buffer:string): Dictionary {
    const rules: Dictionary = {};
    // Rules that extend AlternateTokenRule
    let matches = buffer.matchAll(/export class (\S+)Rule extends AlternateTokenRule.*?super\(\[([^\]]*)/sg);
    for (let match of matches) {
      const name  = lowerCaseFirstLetter(match[1]);
      rules[name] = removeWhiteSpace(match[2]);
      rules[name] = replaceCommas(rules[name]);
      rules[name] = replaceTokenElements(rules[name]);
    }
    // Other Rules
    matches = buffer.matchAll(/export class (\S+)Rule extends (?!AlternateTokenRule).*?constructor\(\)\s*\{[^}]*this.rule\s*=\s*new([^;}]*)[^}]*\}/sg);
    for (let match of matches) {
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
  if (str != null && str.includes('TokenTypes.')) {
    str = str.replaceAll(/(?:TokenTypes\.)([^|\s]*)/g,
      (match, p1, offset, string, groups) =>
        { return p1.toLowerCase(); });
  }
  return str;
}
