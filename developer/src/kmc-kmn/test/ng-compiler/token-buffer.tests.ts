/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * Tests for KMC KMN Next Generation Compiler (TokenBuffer)
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { Lexer, Token } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { KEYBOARD_NAMES } from '../../src/ng-compiler/keyboard-names.js';
import { readFileSync } from 'fs';

// nomatch > layer('default')
const LIST: Token[] = [
  new Token(TokenType.NOMATCH, 'nomatch'),
  new Token(TokenType.WHITESPACE, ' ', 1, 8),
  new Token(TokenType.CHEVRON, '>', 1, 9),
  new Token(TokenType.WHITESPACE, ' ', 1, 10),
  new Token(TokenType.LAYER, 'layer', 1, 11),
  new Token(TokenType.LEFT_BR, '(', 1, 16),
  new Token(TokenType.STRING, '\'default\'', 1, 17),
  new Token(TokenType.RIGHT_BR, ')', 1, 26),
];

describe("TokenBuffer Tests", () => {
  describe("TokenBuffer constructor", () => {
    it("can construct a TokenBuffer with list", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
    it("can construct a TokenBuffer with empty list", () => {
      const tb: TokenBuffer = new TokenBuffer([]);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
    it("can construct a TokenBuffer with null", () => {
      const tb: TokenBuffer = new TokenBuffer(null);
      assert.isNotNull(tb);
      assert.equal(tb.currentPosition, 0);
    });
  });
  describe("TokenBuffer currentPosition getter", () => {
    it("can provide the correct initial currentPosition", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
    });
  });
  describe("TokenBuffer nextToken()", () => {
    it("provides the correct initial Token", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer popToken()", () => {
    it("can handle a single pop", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
    });
    it("can handle popping the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenType.RIGHT_BR);
    });
    it("can handle popping beyond the whole buffer", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 2);
      assert.equal(tb.nextToken().tokenType,TokenType.CHEVRON);
      tb.popToken()
      assert.equal(tb.currentPosition, 3);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.popToken()
      assert.equal(tb.currentPosition, 4);
      assert.equal(tb.nextToken().tokenType,TokenType.LAYER);
      tb.popToken()
      assert.equal(tb.currentPosition, 5);
      assert.equal(tb.nextToken().tokenType,TokenType.LEFT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 6);
      assert.equal(tb.nextToken().tokenType,TokenType.STRING);
      tb.popToken()
      assert.equal(tb.currentPosition, 7);
      assert.equal(tb.nextToken().tokenType,TokenType.RIGHT_BR);
      tb.popToken()
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
  });
  describe("TokenBuffer resetCurrentPosition()", () => {
    it("can handle resetting to zero", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.popToken()
      assert.equal(tb.currentPosition, 1);
      assert.equal(tb.nextToken().tokenType,TokenType.WHITESPACE);
      tb.resetCurrentPosition(0);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
    it("can handle resetting to buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(8);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to beyond buffer length", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(9);
      assert.equal(tb.currentPosition, 8);
      assert.equal(tb.nextToken().tokenType,TokenType.EOF);
    });
    it("can handle resetting to a negative value", () => {
      const tb: TokenBuffer = new TokenBuffer(LIST);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
      tb.resetCurrentPosition(-1);
      assert.equal(tb.currentPosition, 0);
      assert.equal(tb.nextToken().tokenType,TokenType.NOMATCH);
    });
  });
  describe("TokenBuffer round trip tests [toText()]", () => {
    it("can provide round trip text for khmer_angkor.kmn", () => {
      const buffer: string = readFileSync('test/fixtures/keyboards/khmer_angkor.kmn').toString();
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
      const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
      const output: string = tokenBuffer.toText();
      assert.equal(output, buffer);
    });
    it("can provide round trip text for baseline keyboards", () => {
      [
        'k_000___null_keyboard',
        'k_001___basic_input_unicodei',
        'k_002___basic_input_unicode',
        'k_003___nul',
        'k_004___basic_input__shift_2_',
        'k_005___nul_with_initial_context',
        'k_006___vkey_input__shift_ctrl_',
        'k_007___vkey_input__ctrl_alt_',
        'k_008___vkey_input__ctrl_alt_2_',
        'k_012___ralt',
        'k_013___deadkeys',
        'k_014___groups_and_virtual_keys',
        'k_015___ralt_2',
        'k_017___space_mnemonic_kbd',
        'k_018___nul_testing',
        'k_019___multiple_deadkeys',
        'k_020___deadkeys_and_backspace',
        'k_021___options',
        'k_022___options_with_preset',
        'k_023___options_with_save',
        'k_024___options_with_save_and_preset',
        'k_025___options_with_reset',
        'k_026___system_stores',
        'k_027___system_stores_2',
        'k_028___smp',
        'k_029___beep',
        'k_030___multiple_groups',
        'k_031___caps_lock',
        'k_032___caps_control',
        'k_033___caps_always_off',
        'k_034___options_double_set_reset',
        'k_035___options_double_set_staged',
        'k_036___options___double_reset_staged',
        'k_037___options___double_reset',
        'k_038___punctkeys',
        'k_039___generic_ctrlalt',
        'k_040___long_context',
        'k_041___long_context_and_deadkeys',
        'k_042___long_context_and_split_deadkeys',
        'k_043___output_and_keystroke',
        'k_044___if_and_context',
        'k_045___deadkey_and_context',
        'k_046___deadkey_and_contextex',
        'k_047___caps_always_off_initially_on',
        'k_048___modifier_keys_keep_context',
        'k_049___enter_invalidates_context',
        'k_050___nul_and_context',
        'k_051___if_and_context',
        'k_052___nul_and_index',
        'k_054___nul_and_contextex',
        'k_055___deadkey_cancelled_by_arrow',
      ].forEach((name) => {
        const buffer: string = readFileSync(`../../../common/test/keyboards/baseline/${name}.kmn`).toString();
        const lexer = new Lexer(buffer);
        const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
        const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
        const output: string = tokenBuffer.toText();
        assert.equal(output, buffer, `${name}.kmn`);
      });
    });
    it("can provide round trip text for repository keyboards (0-99)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(0, 100));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (100-199)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(100, 200));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (200-299)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(200, 300));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (300-399)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(300, 400));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (400-499)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(400, 500));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (500-599)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(500, 600));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (600-699)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(600, 700));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (700-799)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(700, 800));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (800-899)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(800, 900));
    }).timeout(50000);
    it("can provide round trip text for repository keyboards (900-end)", () => {
      testRepositoryKeyboards(KEYBOARD_NAMES.slice(900));
    }).timeout(50000);
  });
});

function testRepositoryKeyboards(names: string[]) {
  names.forEach((name) => {
    const buffer: string = readFileSync(`../../../../keyboards/${name}.kmn`).toString();
    const lexer = new Lexer(buffer);
    const tokens: Token[] = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
    const tokenBuffer: TokenBuffer = new TokenBuffer(tokens);
    const output: string = tokenBuffer.toText();
    assert.equal(output, buffer, `${name}.kmn`);
  });
}