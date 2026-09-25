/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * Tests for KMC KMN Next Generation Semantic Model Builder (KmxBuilder)
 */

import 'mocha';
import { assert } from 'chai';
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { KmxBuilder } from '../../src/ng-compiler/kmx-builder.js';
import { existsSync } from 'node:fs';
import { baselineKeyboardNames, PATH_TO_BASELINE} from './keyboard-names.js';
import { readFile } from './token-buffer.tests.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { stringToTokenBuffer } from './kmn-analyzer.tests.js';
import { Parser } from '../../src/ng-compiler/kmn-analyzer.js';


let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;

describe("KmxBuilder Tests", () => {
  beforeEach(() => {
    root = new ASTNode();
  });
  it("can construct a KmxBuilder", () => {
    const builder: KmxBuilder = new KmxBuilder(root);
    assert.isNotNull(builder);
  });
  it("can provide in-memory model for baseline keyboards", function() {
    if (!existsSync(PATH_TO_BASELINE)) {
      this.skip();
    }
    baselineKeyboardNames().forEach((name) => {
      const buffer: string = readFile(`${PATH_TO_BASELINE}${name}.kmn`);
      tokenBuffer = stringToTokenBuffer(buffer);
      const parser: Parser = new Parser(tokenBuffer);
      root = parser.parse();
      const builder: KmxBuilder = new KmxBuilder(root);
      assert.isNotNull(builder.build(), `${name}.kmn`);
    });
  });
});
