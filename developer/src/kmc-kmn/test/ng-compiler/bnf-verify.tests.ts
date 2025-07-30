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
  beforeEach(() => {
  });
  describe("Parser Tests", () => {
    it("contains all the BNF grammar rules", () => {
      const bnfBuffer: string = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-file.bnf').toString();
      const bnfRules: Dictionary = getBnfRules(bnfBuffer);
      const sourceBuffer: string = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-analyser.ts').toString();
      const sourceRules: Dictionary = getSourceRules(sourceBuffer);
      assert.equal(sourceRules, {});
      assert.equal(bnfRules, {});
      assert.isTrue(true);
    });
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
    const matches = buffer.matchAll(/export class (\S+)Rule/g);
    for (let match of matches) {
      rules[match[1]] = 'tbd';
    }
    return rules;
}
