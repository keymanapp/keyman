/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { Lexer, ScanRecogniser, Token, TokenTypes } from '../../src/ng-compiler/lexer.js'

describe("Lexer Tests", () => {
  describe("ScanRecogniser", () => {
    it("can construct a ScanRecogniser", () => {
      const sr = new ScanRecogniser(TokenTypes.STORE, new RegExp("^store"));
      assert.deepEqual(sr.toString(), '[STORE,/^store/]');
    });
  });
  describe("Token", () => {
    it("can construct a Token", () => {
      const token = new Token(TokenTypes.STORE, 'store');
      assert.deepEqual(token.toString(), '[STORE,store]');
    });
  });
  describe("Lexer", () => {
    it("can recognise a BITMAP token", () => {
      recogniseToken(TokenTypes.BITMAP, 'bitmap');
    });
    it("can recognise a CASEDKEYS token", () => {
      recogniseToken(TokenTypes.CASEDKEYS, 'casedkeys');
    });
    it("can recognise a COPYRIGHT token", () => {
      recogniseToken(TokenTypes.COPYRIGHT, 'copyright');
    });
    it("can recognise a DISPLAYMAP token", () => {
      recogniseToken(TokenTypes.DISPLAYMAP, 'displaymap');
    });
    it("can recognise a ETHNOLOGUECODE token", () => {
      recogniseToken(TokenTypes.ETHNOLOGUECODE, 'ethnologuecode');
    });
    it("can recognise a HOTKEY token", () => {
      recogniseToken(TokenTypes.HOTKEY, 'hotkey');
    });
    it("can recognise a INCLUDECODES token", () => {
      recogniseToken(TokenTypes.INCLUDECODES, 'includecodes');
    });
    it("can recognise a KEYBOARDVERSION token", () => {
      recogniseToken(TokenTypes.KEYBOARDVERSION, 'keyboardversion');
    });
    it("can recognise a KMW_EMBEDCSS token", () => {
      recogniseToken(TokenTypes.KMW_EMBEDCSS, 'kmw_embedcss');
    });
    it("can recognise a KMW_EMBEDJS token", () => {
      recogniseToken(TokenTypes.KMW_EMBEDJS, 'kmw_embedjs');
    });
    it("can recognise a KMW_HELPFILE token", () => {
      recogniseToken(TokenTypes.KMW_HELPFILE, 'kmw_helpfile');
    });
    it("can recognise a KMW_HELPTEXT token", () => {
      recogniseToken(TokenTypes.KMW_HELPTEXT, 'kmw_helptext');
    });
    it("can recognise a KMW_RTL token", () => {
      recogniseToken(TokenTypes.KMW_RTL, 'kmw_rtl');
    });
    it("can recognise a LANGUAGE token", () => {
      recogniseToken(TokenTypes.LANGUAGE, 'language');
    });
    it("can recognise a LAYER token", () => {
      recogniseToken(TokenTypes.LAYER, 'layer');
    });
    it("can recognise a LAYOUTFILE token", () => {
      recogniseToken(TokenTypes.LAYOUTFILE, 'layoutfile');
    });
    it("can recognise a MESSAGE token", () => {
      recogniseToken(TokenTypes.MESSAGE, 'message');
    });
    it("can recognise a MNEMONICLAYOUT token", () => {
      recogniseToken(TokenTypes.MNEMONICLAYOUT, 'mnemoniclayout');
    });
    it("can recognise a NAME token", () => {
      recogniseToken(TokenTypes.NAME, 'name');
    });
    it("can recognise a NEWLAYER token", () => {
      recogniseToken(TokenTypes.NEWLAYER, 'newlayer');
    });
    it("can recognise a OLDCHARPOSMATCHING token", () => {
      recogniseToken(TokenTypes.OLDCHARPOSMATCHING, 'oldcharposmatching');
    });
    it("can recognise a OLDLAYER token", () => {
      recogniseToken(TokenTypes.OLDLAYER, 'oldlayer');
    });
    it("can recognise a TARGETS token", () => {
      recogniseToken(TokenTypes.TARGETS, 'targets');
    });
    it("can recognise a VERSION token", () => {
      recogniseToken(TokenTypes.VERSION, 'version');
    });
    it("can recognise a VISUALKEYBOARD token", () => {
      recogniseToken(TokenTypes.VISUALKEYBOARD, 'visualkeyboard');
    });
    it("can recognise a WINDOWSLANGUAGES token", () => {
      recogniseToken(TokenTypes.WINDOWSLANGUAGES, 'windowslanguages');
    });
    it("can recognise a CAPSALWAYSOFF token", () => {
      recogniseToken(TokenTypes.CAPSALWAYSOFF, 'capsalwaysoff');
    });
    it("can recognise a CAPSONONLY token", () => {
      recogniseToken(TokenTypes.CAPSONONLY, 'capsononly');
    });
    it("can recognise a SHIFTFREECAPS token", () => {
      recogniseToken(TokenTypes.SHIFTFREESCAPS, 'shiftfreescaps');
    });
    it("can recognise an ANY token", () => {
      recogniseToken(TokenTypes.ANY, 'any');
    });
    it("can recognise an ANY token (followed by comma)", () => {
      recogniseTokens(
        'any,',
        [
          new Token(TokenTypes.ANY, 'any'),
          new Token(TokenTypes.COMMA, ',', 1, 4),
        ]
      );
    });
    it("can recognise a BASELAYOUT token", () => {
      recogniseToken(TokenTypes.BASELAYOUT, 'baselayout');
    });
    it("can recognise a BEEP token", () => {
      recogniseToken(TokenTypes.BEEP, 'beep');
    });
    it("can recognise a BEGIN token", () => {
      recogniseToken(TokenTypes.BEGIN, 'begin');
    });
    it("can recognise a CALL token", () => {
      recogniseToken(TokenTypes.CALL, 'call');
    });
    it("can recognise a CONTEXT token", () => {
      recogniseToken(TokenTypes.CONTEXT, 'context');
    });
    it("can recognise a DEADKEY token (deadkey)", () => {
      recogniseToken(TokenTypes.DEADKEY, 'deadkey');
    });
    it("can recognise a DEADKEY token (dk)", () => {
      recogniseToken(TokenTypes.DEADKEY, 'dk');
    });
    it("can recognise a GROUP token", () => {
      recogniseToken(TokenTypes.GROUP, 'group');
    });
    it("can recognise a IF token", () => {
      recogniseToken(TokenTypes.IF, 'if');
    });
    it("can recognise a INDEX token", () => {
      recogniseToken(TokenTypes.INDEX, 'index');
    });
    it("can recognise a MATCH token", () => {
      recogniseToken(TokenTypes.MATCH, 'match');
    });
    it("can recognise a NOMATCH token", () => {
      recogniseToken(TokenTypes.NOMATCH, 'nomatch');
    });
    it("can recognise a NOTANY token", () => {
      recogniseToken(TokenTypes.NOTANY, 'notany');
    });
    it("can recognise a NUL token", () => {
      recogniseToken(TokenTypes.NUL, 'nul');
    });
    it("can recognise a OUTS token", () => {
      recogniseToken(TokenTypes.OUTS, 'outs');
    });
    it("can recognise a PLATFORM token", () => {
      recogniseToken(TokenTypes.PLATFORM, 'platform');
    });
    it("can recognise a RESET token", () => {
      recogniseToken(TokenTypes.RESET, 'reset');
    });
    it("can recognise a RETURN token", () => {
      recogniseToken(TokenTypes.RETURN, 'return');
    });
    it("can recognise a SAVE token", () => {
      recogniseToken(TokenTypes.SAVE, 'save');
    });
    it("can recognise a SET token", () => {
      recogniseToken(TokenTypes.SET, 'set');
    });
    it("can recognise a STORE token", () => {
      recogniseToken(TokenTypes.STORE, 'store');
    });
    it("can recognise a STORE token in upper case", () => {
      recogniseToken(TokenTypes.STORE, 'STORE');
    });
    it("can recognise a STORE token in mixed case", () => {
      recogniseToken(TokenTypes.STORE, 'Store');
    });
    it("can recognise a USE token", () => {
      recogniseToken(TokenTypes.USE, 'use');
    });
    it("can recognise a UNICODE token", () => {
      recogniseToken(TokenTypes.UNICODE, 'unicode');
    });
    it("can recognise a NEWCONTEXT token", () => {
      recogniseToken(TokenTypes.NEWCONTEXT, 'newcontext');
    });
    it("can recognise a POSTKEYSTROKE token", () => {
      recogniseToken(TokenTypes.POSTKEYSTROKE, 'postkeystroke');
    });
    it("can recognise a ANSI token", () => {
      recogniseToken(TokenTypes.ANSI, 'ansi');
    });
    it("can recognise a READONLY token", () => {
      recogniseToken(TokenTypes.READONLY, 'readonly');
    });
    it("can recognise a USING token", () => {
      recogniseToken(TokenTypes.USING, 'using');
    });
    it("can recognise a KEYS token", () => {
      recogniseToken(TokenTypes.KEYS, 'keys');
    });
    it("can recognise a LEFT_BR token", () => {
      recogniseToken(TokenTypes.LEFT_BR, '(');
    });
    it("can recognise a RIGHT_BR token", () => {
      recogniseToken(TokenTypes.RIGHT_BR, ')');
    });
    it("can recognise a LEFT_SQ token", () => {
      recogniseToken(TokenTypes.LEFT_SQ, '[');
    });
    it("can recognise a RIGHT_SQ token", () => {
      recogniseToken(TokenTypes.RIGHT_SQ, ']');
    });
    it("can recognise a AMPHASAND token", () => {
      recogniseToken(TokenTypes.AMPHASAND, '&');
    });
    it("can recognise a CHEVRON token", () => {
      recogniseToken(TokenTypes.CHEVRON, '>');
    });
    it("can recognise a PLUS token", () => {
      recogniseToken(TokenTypes.PLUS, '+');
    });
    it("can recognise a COMMA token", () => {
      recogniseToken(TokenTypes.COMMA, ',');
    });
    it("can recognise a NOT_EQUAL token", () => {
      recogniseToken(TokenTypes.NOT_EQUAL, '!=');
    });
    it("can recognise a EQUAL token", () => {
      recogniseToken(TokenTypes.EQUAL, '=');
    });
    it("can recognise a RANGE token", () => {
      recogniseToken(TokenTypes.RANGE, '..');
    });
    it("can recognise a U_CHAR token", () => {
      recogniseToken(TokenTypes.U_CHAR, 'U+1');
      recogniseToken(TokenTypes.U_CHAR, 'U+A');
      recogniseToken(TokenTypes.U_CHAR, 'U+1B');
      recogniseToken(TokenTypes.U_CHAR, 'U+1CD');
      recogniseToken(TokenTypes.U_CHAR, 'U+1EF0');
      recogniseToken(TokenTypes.U_CHAR, 'U+23456');
      recogniseToken(TokenTypes.U_CHAR, 'U+789ABC');
    });
    it("can recognise a STRING token (single quote)", () => {
      recogniseToken(TokenTypes.STRING, '\'10.0\'');
    });
    it("can recognise a STRING token (double quote)", () => {
      recogniseToken(TokenTypes.STRING, '"10.0"');
    });
    it("can recognise a STRING token (repeated quotes)", () => {
      recogniseTokens(
        '\'hello\' \'world\'',
        [
          new Token(TokenTypes.STRING,     '\'hello\''),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.STRING,     '\'world\'', 1, 9),
        ]
      );
    });
    it("does not pick out tokens from inside strings", () => {
      recogniseToken(TokenTypes.STRING, '"store"');
    });
    it("can recognise a SHIFT_CODE token", () => {
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'SHIFT');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'CTRL');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'LCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'RCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'ALT');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'LALT');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'RALT');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'CAPS');
      recogniseTokenFollowedBySpace(TokenTypes.SHIFT_CODE, 'NCAPS');
    });
    it("can recognise a KEY_CODE token", () => {
      recogniseTokenFollowedByRightSquare(TokenTypes.KEY_CODE, 'K_K');
      recogniseTokenFollowedByRightSquare(TokenTypes.KEY_CODE, 'K_COLON');
      recogniseTokenFollowedByRightSquare(TokenTypes.KEY_CODE, 'T_17D2_1780');
      recogniseTokenFollowedByRightSquare(TokenTypes.KEY_CODE, 'U_0030');
      recogniseTokenFollowedByRightSquare(TokenTypes.KEY_CODE, 'A21');
    });
    it("can recognise a KEY_CODE token (followed by space)", () => {
      recogniseTokens(
        'K_K ]',
        [
          new Token(TokenTypes.KEY_CODE, 'K_K'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 4),
          new Token(TokenTypes.RIGHT_SQ, ']', 1, 5),
        ]
      );
    });
    it("can recognise a COMMENT token", () => {
      recogniseToken(TokenTypes.COMMENT, 'c This tells Keyman which keys should have casing behavior applied');
    });
    it("can recognise a COMMENT token (followed by LF)", () => {
      const comment: String = 'c This tells Keyman which keys should have casing behavior applied';
      recogniseTokens(
        'c This tells Keyman which keys should have casing behavior applied\n',
        [
          new Token(TokenTypes.COMMENT, comment),
          new Token(TokenTypes.NEWLINE, '\n', 1, 67, `${comment}\n`),
        ]
      );
    });
    it("can recognise a WHITESPACE token (single space)", () => {
      recogniseToken(TokenTypes.WHITESPACE, ' ');
    });
    it("can recognise a CONTINUATION token (no space after)", () => {
      recogniseTokens(
        '\\\n',
        [
          new Token(TokenTypes.CONTINUATION, '\\'),
          new Token(TokenTypes.NEWLINE, '\n', 1, 2, '\\\n'),
        ]
      );
    });
    it("can recognise a CONTINUATION token (space after)", () => {
      recogniseTokens(
        '\\ \n',
        [
          new Token(TokenTypes.CONTINUATION, '\\'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 2),
          new Token(TokenTypes.NEWLINE, '\n', 1, 3, '\\ \n'),
        ]
      );
    });
    it("can recognise multiple CONTINUATION tokens", () => {
      const line1 = 'store(LaoConsonants) U+0E81 U+0E82 U+0E84 U+0E87 U+0E88 U+0E8A U+0E8D U+0E94 \\\n';
      const line2 = '                     U+0E95 U+0E96 U+0E97 U+0E99 U+0E9A U+0E9B U+0E9C U+0E9D \\\n';
      const line3 = '                     U+0E9E U+0E9F U+0EA1 U+0EA2 U+0EA3 U+0EA5 U+0EA7 U+0EAA \\\n';
      const line4 = '                     U+0EAB U+0EAD U+0EAE    c list of all the Lao consonants\n';
      recogniseTokens(
        `${line1}${line2}${line3}${line4}`,
        [
          new Token(TokenTypes.STORE, 'store'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.PARAMETER, 'LaoConsonants', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 20),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 21),
          new Token(TokenTypes.U_CHAR, 'U+0E81', 1, 22),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 28),
          new Token(TokenTypes.U_CHAR, 'U+0E82', 1, 29),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 35),
          new Token(TokenTypes.U_CHAR, 'U+0E84', 1, 36),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 42),
          new Token(TokenTypes.U_CHAR, 'U+0E87', 1, 43),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 49),
          new Token(TokenTypes.U_CHAR, 'U+0E88', 1, 50),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 56),
          new Token(TokenTypes.U_CHAR, 'U+0E8A', 1, 57),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 63),
          new Token(TokenTypes.U_CHAR, 'U+0E8D', 1, 64),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 70),
          new Token(TokenTypes.U_CHAR, 'U+0E94', 1, 71),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 77),
          new Token(TokenTypes.CONTINUATION, '\\', 1, 78),
          new Token(TokenTypes.NEWLINE, '\n', 1, 79, line1),
          new Token(TokenTypes.WHITESPACE, '                     ', 2, 1),
          new Token(TokenTypes.U_CHAR, 'U+0E95', 2, 22),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 28),
          new Token(TokenTypes.U_CHAR, 'U+0E96', 2, 29),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 35),
          new Token(TokenTypes.U_CHAR, 'U+0E97', 2, 36),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 42),
          new Token(TokenTypes.U_CHAR, 'U+0E99', 2, 43),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 49),
          new Token(TokenTypes.U_CHAR, 'U+0E9A', 2, 50),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 56),
          new Token(TokenTypes.U_CHAR, 'U+0E9B', 2, 57),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 63),
          new Token(TokenTypes.U_CHAR, 'U+0E9C', 2, 64),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 70),
          new Token(TokenTypes.U_CHAR, 'U+0E9D', 2, 71),
          new Token(TokenTypes.WHITESPACE, ' ', 2, 77),
          new Token(TokenTypes.CONTINUATION, '\\', 2, 78),
          new Token(TokenTypes.NEWLINE, '\n', 2, 79, line2),
          new Token(TokenTypes.WHITESPACE, '                     ', 3, 1),
          new Token(TokenTypes.U_CHAR, 'U+0E9E', 3, 22),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 28),
          new Token(TokenTypes.U_CHAR, 'U+0E9F', 3, 29),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 35),
          new Token(TokenTypes.U_CHAR, 'U+0EA1', 3, 36),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 42),
          new Token(TokenTypes.U_CHAR, 'U+0EA2', 3, 43),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 49),
          new Token(TokenTypes.U_CHAR, 'U+0EA3', 3, 50),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 56),
          new Token(TokenTypes.U_CHAR, 'U+0EA5', 3, 57),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 63),
          new Token(TokenTypes.U_CHAR, 'U+0EA7', 3, 64),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 70),
          new Token(TokenTypes.U_CHAR, 'U+0EAA', 3, 71),
          new Token(TokenTypes.WHITESPACE, ' ', 3, 77),
          new Token(TokenTypes.CONTINUATION, '\\', 3, 78),
          new Token(TokenTypes.NEWLINE, '\n', 3, 79, line3),
          new Token(TokenTypes.WHITESPACE, '                     ', 4, 1),
          new Token(TokenTypes.U_CHAR, 'U+0EAB', 4, 22),
          new Token(TokenTypes.WHITESPACE, ' ', 4, 28),
          new Token(TokenTypes.U_CHAR, 'U+0EAD', 4, 29),
          new Token(TokenTypes.WHITESPACE, ' ', 4, 35),
          new Token(TokenTypes.U_CHAR, 'U+0EAE', 4, 36),
          new Token(TokenTypes.WHITESPACE, '    ', 4, 42),
          new Token(TokenTypes.COMMENT, 'c list of all the Lao consonants', 4, 46),
          new Token(TokenTypes.NEWLINE, '\n', 4, 78, line4),
        ]
      );
    });
    it("can recognise a NEWLINE token (LF)", () => {
      recogniseToken(TokenTypes.NEWLINE, '\n');
    });
    it("can recognise a NEWLINE token (CR)", () => {
      recogniseToken(TokenTypes.NEWLINE, '\r');
    });
    it("can recognise a NEWLINE token (CRLF)", () => {
      recogniseToken(TokenTypes.NEWLINE, '\r\n');
    });
    it("can recognise a PARAMETER token", () => {
      recogniseTokens(
        'main)',
        [
          new Token(TokenTypes.PARAMETER, 'main'),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets", () => {
      recogniseTokens(
        '(main)',
        [
          new Token(TokenTypes.LEFT_BR, '('),
          new Token(TokenTypes.PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 6),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets (single space before)", () => {
      recogniseTokens(
        '( main)',
        [
          new Token(TokenTypes.LEFT_BR, '('),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 2),
          new Token(TokenTypes.PARAMETER, 'main', 1, 3),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets (single space after)", () => {
      recogniseTokens(
        '(main )',
        [
          new Token(TokenTypes.LEFT_BR, '('),
          new Token(TokenTypes.PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise two PARAMETER tokens in brackets (comma separated)", () => {
      recogniseTokens(
        '(main,2)',
        [
          new Token(TokenTypes.LEFT_BR, '('),
          new Token(TokenTypes.PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.COMMA, ',', 1, 6),
          new Token(TokenTypes.PARAMETER, '2', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 8),
        ]
      );
    });
    it("can recognise two PARAMETER tokens in brackets (space separated)", () => {
      recogniseTokens(
        '(main 2)',
        [
          new Token(TokenTypes.LEFT_BR, '('),
          new Token(TokenTypes.PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.PARAMETER, '2', 1, 7),
          new Token(TokenTypes.RIGHT_BR,   ')', 1, 8),
        ]
      );
    });
    it("can recognise a PARAMETER token (that starts with any, then a)", () => {
      recogniseTokens(
        'anya)',
        [
          new Token(TokenTypes.PARAMETER, 'anya'),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (that starts with any, then A)", () => {
      recogniseTokens(
        'anyA)',
        [
          new Token(TokenTypes.PARAMETER, 'anyA'),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a bitmap store", () => {
      recogniseStoreWithString(TokenTypes.BITMAP, 'khmer_angkor.ico');
    });
    it("can recognise a copyright store", () => {
      recogniseStoreWithString(TokenTypes.COPYRIGHT, '© SIL Global');
    });
    it("can recognise a displaymap store", () => {
      recogniseStoreWithString(TokenTypes.DISPLAYMAP, '../../../shared/fonts/kbd/kbdkhmr/KbdKhmr.json');
    });
    it("can recognise a keyboardversion store", () => {
      recogniseStoreWithString(TokenTypes.KEYBOARDVERSION, '2.0');
    });
    it("can recognise a layoutfile store", () => {
      recogniseStoreWithString(TokenTypes.LAYOUTFILE, 'khmer_angkor.keyman-touch-layout');
    });
    it("can recognise a name store", () => {
      recogniseStoreWithString(TokenTypes.NAME, "Khmer Angkor");
    });
    it("can recognise a message store", () => {
      recogniseStoreWithString(TokenTypes.MESSAGE, "More than just a Khmer Unicode keyboard.");
    });
    it("can recognise a targets store", () => {
      recogniseStoreWithString(TokenTypes.TARGETS, 'any');
    });
    it("can recognise a version store", () => {
      recogniseStoreWithString(TokenTypes.VERSION, 'khmer_angkor.kvks');
    });
    it("can recognise a visualkeyboard store", () => {
      recogniseStoreWithString(TokenTypes.VISUALKEYBOARD, '10.0');
    });
    it("can recognise a capsalwaysoff store", () => {
      recogniseStoreWithString(TokenTypes.CAPSALWAYSOFF, "1");
    });
    it("can recognise a capsononly store", () => {
      recogniseStoreWithString(TokenTypes.CAPSONONLY, "1");
    });
    it("can recognise a shiftfreescaps store", () => {
      recogniseStoreWithString(TokenTypes.SHIFTFREESCAPS, "1");
    });
    it("can recognise a begin statement (unicode)", () => {
      recogniseTokens(
        'begin Unicode > use(main)',
        [
          new Token(TokenTypes.BEGIN, 'begin'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.UNICODE, 'Unicode', 1, 7),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 14),
          new Token(TokenTypes.CHEVRON, '>', 1, 15),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 16),
          new Token(TokenTypes.USE, 'use', 1, 17),
          new Token(TokenTypes.LEFT_BR, '(', 1, 20),
          new Token(TokenTypes.PARAMETER, 'main', 1, 21),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 25),
        ]
      );
    });
    it("can recognise a begin statement (postkeystroke)", () => {
      recogniseTokens(
        'begin PostKeystroke > use(PostKeystroke)',
        [
          new Token(TokenTypes.BEGIN, 'begin'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.POSTKEYSTROKE, 'PostKeystroke', 1, 7),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 20),
          new Token(TokenTypes.CHEVRON, '>', 1, 21),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.USE, 'use', 1, 23),
          new Token(TokenTypes.LEFT_BR, '(', 1, 26),
          new Token(TokenTypes.POSTKEYSTROKE, 'PostKeystroke', 1, 27), // recognised as keyword
          new Token(TokenTypes.RIGHT_BR, ')', 1, 40),
        ]
      );
    });
    it("can recognise a group statement (readonly)", () => {
      recogniseTokens(
        'group(NewContext) readonly',
        [
          new Token(TokenTypes.GROUP, 'group'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.NEWCONTEXT, 'NewContext', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.READONLY, 'readonly', 1, 19),
        ]
      );
    });
    it("can recognise a group statement (using keys)", () => {
      recogniseTokens(
        'group(main) using keys',
        [
          new Token(TokenTypes.GROUP, 'group'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.PARAMETER, 'main', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 11),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 12),
          new Token(TokenTypes.USING, 'using', 1, 13),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.KEYS, 'keys', 1, 19),
        ]
      );
    });
    it("can recognise an if statement (using any, context, layer)", () => {
      recogniseTokens(
        'if(&newLayer = "") if(&layer = \'shift\') any(ShiftOutSingle) > context layer(\'default\')',
        [
          new Token(TokenTypes.IF, 'if'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 3),
          new Token(TokenTypes.AMPHASAND, '&', 1, 4),
          new Token(TokenTypes.NEWLAYER, 'newLayer', 1, 5),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 13),
          new Token(TokenTypes.EQUAL, '=', 1, 14),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 15),
          new Token(TokenTypes.STRING, '""', 1, 16),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 18),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 19),
          new Token(TokenTypes.IF, 'if', 1, 20),
          new Token(TokenTypes.LEFT_BR, '(', 1, 22),
          new Token(TokenTypes.AMPHASAND, '&', 1, 23),
          new Token(TokenTypes.LAYER, 'layer', 1, 24),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 29),
          new Token(TokenTypes.EQUAL, '=', 1, 30),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 31),
          new Token(TokenTypes.STRING, '\'shift\'', 1, 32),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 39),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 40),
          new Token(TokenTypes.ANY, 'any', 1, 41),
          new Token(TokenTypes.LEFT_BR, '(', 1, 44),
          new Token(TokenTypes.PARAMETER, 'ShiftOutSingle', 1, 45),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 59),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 60),
          new Token(TokenTypes.CHEVRON, '>', 1, 61),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 62),
          new Token(TokenTypes.CONTEXT, 'context', 1, 63),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 70),
          new Token(TokenTypes.LAYER, 'layer', 1, 71),
          new Token(TokenTypes.LEFT_BR, '(', 1, 76),
          new Token(TokenTypes.STRING, '\'default\'', 1, 77),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 86),
        ]
      );
    });
    it("can recognise a match statement", () => {
      recogniseTokens(
        'match > use(CombineDiacritics)',
        [
          new Token(TokenTypes.MATCH, 'match'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.CHEVRON, '>', 1, 7),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.USE, 'use', 1, 9),
          new Token(TokenTypes.LEFT_BR, '(', 1, 12),
          new Token(TokenTypes.PARAMETER, 'CombineDiacritics', 1, 13),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 30),
        ]
      );
    });
    it("can recognise a nomatch statement", () => {
      recogniseTokens(
        'nomatch > layer(\'default\')',
        [
          new Token(TokenTypes.NOMATCH, 'nomatch'),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.CHEVRON, '>', 1, 9),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 10),
          new Token(TokenTypes.LAYER, 'layer', 1, 11),
          new Token(TokenTypes.LEFT_BR, '(', 1, 16),
          new Token(TokenTypes.STRING, '\'default\'', 1, 17),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 26),
        ]
      );
    });
    it("can recognise a platform statement", () => {
      recogniseTokens(
        'platform(\'touch\') > use(detectStartOfSentence)',
        [
          new Token(TokenTypes.PLATFORM, 'platform'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 9),
          new Token(TokenTypes.STRING, '\'touch\'', 1, 10),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.WHITESPACE, ' ',1, 18),
          new Token(TokenTypes.CHEVRON, '>', 1, 19),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 20),
          new Token(TokenTypes.USE, 'use', 1, 21),
          new Token(TokenTypes.LEFT_BR, '(', 1, 24),
          new Token(TokenTypes.PARAMETER, 'detectStartOfSentence', 1, 25),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 46),
        ]
      );
    });
    it("can recognise a store statement using outs", () => {
      recogniseTokens(
        'store(ShiftOutAll)  outs(ShiftOutSingle) outs(vCombo1) outs(vCombo2) outs(vCombo3)',
        [
          new Token(TokenTypes.STORE, 'store'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.PARAMETER, 'ShiftOutAll', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 18),
          new Token(TokenTypes.WHITESPACE, '  ', 1, 19),
          new Token(TokenTypes.OUTS, 'outs', 1, 21),
          new Token(TokenTypes.LEFT_BR, '(', 1, 25),
          new Token(TokenTypes.PARAMETER, 'ShiftOutSingle', 1, 26),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 40),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 41),
          new Token(TokenTypes.OUTS, 'outs', 1, 42),
          new Token(TokenTypes.LEFT_BR, '(', 1, 46),
          new Token(TokenTypes.PARAMETER, 'vCombo1', 1, 47),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 54),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 55),
          new Token(TokenTypes.OUTS, 'outs', 1, 56),
          new Token(TokenTypes.LEFT_BR, '(', 1, 60),
          new Token(TokenTypes.PARAMETER, 'vCombo2', 1, 61),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 68),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 69),
          new Token(TokenTypes.OUTS, 'outs', 1, 70),
          new Token(TokenTypes.LEFT_BR, '(', 1, 74),
          new Token(TokenTypes.PARAMETER, 'vCombo3', 1, 75),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 82),
        ]
      );
    });
    it("can recognise a store statement (khmer string)", () => {
      recogniseTokens(
        'store(ShiftOutSingle) \'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'',
        [
          new Token(TokenTypes.STORE, 'store'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.PARAMETER, 'ShiftOutSingle', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 21),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.STRING, '\'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'', 1, 23),
        ]
      );
    });
    it("can recognise a store statement (unicode chars)", () => {
      recogniseTokens(
        'store(whitespace) \' \' U+00A0 U+000D U+000A',
        [
          new Token(TokenTypes.STORE, 'store'),
          new Token(TokenTypes.LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.PARAMETER, 'whitespace', 1, 7),
          new Token(TokenTypes.RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.STRING, '\' \'', 1, 19),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.U_CHAR, 'U+00A0', 1, 23),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 29),
          new Token(TokenTypes.U_CHAR, 'U+000D', 1, 30),
          new Token(TokenTypes.WHITESPACE, ' ', 1, 36),
          new Token(TokenTypes.U_CHAR, 'U+000A', 1, 37),
        ]
      );
    });
    it("can handle no newline at end of file", () => {
      const lexer    = new Lexer('beep');
      const actual   = lexer.parse(true);
      const expected = [
        new Token(TokenTypes.BEEP, 'beep'),
        new Token(TokenTypes.EOF, '', 1, 1, 'beep'),
      ];
      assert.deepEqual(actual, expected);
    });
  });
});

function recogniseToken(type: TokenTypes, text: String): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse(false);
  const line     = (type === TokenTypes.NEWLINE) ? text : null;
  const expected = [new Token(type, text, 1, 1, line)];
  assert.deepEqual(actual, expected);
}

function recogniseTokens(text: String, expected: Token[]): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse(false);
  assert.deepEqual(actual, expected);
}

function recogniseTokenFollowedBySpace(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text} `,
    [
      new Token(type, text),
      new Token(TokenTypes.WHITESPACE, ' ', 1, 1+text.length),
    ]
  );
}

function recogniseTokenFollowedByRightSquare(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text}]`,
    [
      new Token(type, text),
      new Token(TokenTypes.RIGHT_SQ, ']', 1, 1+text.length),
    ]
  );
}

function recogniseStoreWithString(type: TokenTypes, text: String) {
  const value = TokenTypes[type].toLowerCase();
  recogniseTokens(
    `store(&${value}) '${text}'`,
    [
      new Token(TokenTypes.STORE, 'store'),
      new Token(TokenTypes.LEFT_BR, '(', 1, 6),
      new Token(TokenTypes.AMPHASAND, '&', 1, 7),
      new Token(type, value, 1, 8),
      new Token(TokenTypes.RIGHT_BR, ')', 1, 8+value.length),
      new Token(TokenTypes.WHITESPACE, ' ', 1, 9+value.length),
      new Token(TokenTypes.STRING, `'${text}'`, 1, 10+value.length),
    ]
  );
}
