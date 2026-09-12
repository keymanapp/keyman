/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-05
 *
 * Tests for KMC KMN Next Generation Lexer
 *
 * ScanRecognizer for the Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { ScanRecognizer } from '../../src/ng-compiler/scan-recognizer.js'

describe("ScanRecognizer Tests", () => {
  it("can construct a ScanRecognizer", () => {
    const sr = new ScanRecognizer(TokenType.STORE, /^store/, true);
    assert.deepEqual(sr.toString(), '[STORE,/^store/,true]');
  });
});