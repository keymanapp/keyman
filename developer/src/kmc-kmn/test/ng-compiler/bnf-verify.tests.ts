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
      const buffer: string  = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-file.bnf').toString();
      const rules: Dictionary = bnfRules(buffer);
      assert.equal(rules, {});
      assert.isTrue(true);
    });
  });
});

function bnfRules(buffer:string): Dictionary {
  const rules: Dictionary = {};
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

function wrapLines(buffer:string): string {
  return buffer.replaceAll(/[^\S\r\n]*(\r\n|\n|\r)[^\S\r\n]+/g, '');
}
