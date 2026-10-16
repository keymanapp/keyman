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
import { CompStore, KmxBuilder } from '../../src/ng-compiler/kmx-builder.js';
import { existsSync } from 'node:fs';
import { baselineKeyboardNames, PATH_TO_BASELINE} from './keyboard-names.js';
import { readFile } from './token-buffer.tests.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { stringToTokenBuffer } from './kmn-analyzer.tests.js';
import { Parser } from '../../src/ng-compiler/kmn-analyzer.js';
import { DwFileVersion, DwSystemID } from '../../src/ng-compiler/kmx-constants.js';
import { NodeType } from '../../src/ng-compiler/node-type.js';
import { Token } from '../../src/ng-compiler/lexer.js';
import { TokenType } from '../../src/ng-compiler/token-type.js';


let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;
let storesNode: ASTNode      = null;

describe("KmxBuilder Tests", () => {
  describe("KmxBuilder", () => {
    it("can construct a KmxBuilder", () => {
      root = new ASTNode();
      const builder: KmxBuilder = new KmxBuilder(root);
      assert.isNotNull(builder);
    });
    describe("KmxBuilder.buildHeader", () => {
      beforeEach(() => {
        root       = new ASTNode();
        storesNode = new ASTNode(NodeType.STORES);
        root.addChild(storesNode);
      });
      it("always adds required demprecated COMP_STOREs", () => {
        const builder = new KmxBuilder(root);
        builder['buildHeader']();
        assert.isTrue(builder['model'].compStore.some(
          (cs: CompStore) => (cs.dwSystemID == DwSystemID.TSS_CUSTOMKEYMANEDITION && cs.dpString == '0')));
        assert.isTrue(builder['model'].compStore.some(
          (cs: CompStore) => (cs.dwSystemID == DwSystemID.TSS_CUSTOMKEYMANEDITIONNAME && cs.dpString == 'Keyman')));
      });
      it("can handle a VERSION store", () => {
        const versionNode = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '&version'));
        storesNode.addChild(versionNode);
        const stringNode = new ASTNode(NodeType.STRING, new Token(TokenType.STRING, "'6.0'"));
        versionNode.addChild(stringNode);
        const builder = new KmxBuilder(root);
        builder['buildHeader']();
        assert.equal(builder['model'].dwFileVersion, DwFileVersion.VERSION_60);
        assert.isTrue(builder['model'].compStore.some(
          (cs: CompStore) => (cs.dwSystemID == DwSystemID.TSS_VERSION && cs.dpString == '&version')));
      });
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
});
