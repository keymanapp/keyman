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
      const sr = new ScanRecogniser(TokenTypes.TT_STORE, new RegExp("^store"));
      assert.deepEqual(sr.toString(), '[TT_STORE,/^store/]');
    });
  });
  describe("Token", () => {
    it("can construct a Token", () => {
      const token = new Token(TokenTypes.TT_STORE, 'store');
      assert.deepEqual(token.toString(), '[TT_STORE,store]');
    });
  });
  describe("Lexer", () => {
    it("can recognise a TT_BITMAP token", () => {
      recogniseToken(TokenTypes.TT_BITMAP, 'bitmap');
    });
    it("can recognise a TT_CASEDKEYS token", () => {
      recogniseToken(TokenTypes.TT_CASEDKEYS, 'casedkeys');
    });
    it("can recognise a TT_COPYRIGHT token", () => {
      recogniseToken(TokenTypes.TT_COPYRIGHT, 'copyright');
    });
    it("can recognise a TT_DISPLAYMAP token", () => {
      recogniseToken(TokenTypes.TT_DISPLAYMAP, 'displaymap');
    });
    it("can recognise a TT_ETHNOLOGUECODE token", () => {
      recogniseToken(TokenTypes.TT_ETHNOLOGUECODE, 'ethnologuecode');
    });
    it("can recognise a TT_HOTKEY token", () => {
      recogniseToken(TokenTypes.TT_HOTKEY, 'hotkey');
    });
    it("can recognise a TT_INCLUDECODES token", () => {
      recogniseToken(TokenTypes.TT_INCLUDECODES, 'includecodes');
    });
    it("can recognise a TT_KEYBOARDVERSION token", () => {
      recogniseToken(TokenTypes.TT_KEYBOARDVERSION, 'keyboardversion');
    });
    it("can recognise a TT_KMW_EMBEDCSS token", () => {
      recogniseToken(TokenTypes.TT_KMW_EMBEDCSS, 'kmw_embedcss');
    });
    it("can recognise a TT_KMW_EMBEDJS token", () => {
      recogniseToken(TokenTypes.TT_KMW_EMBEDJS, 'kmw_embedjs');
    });
    it("can recognise a TT_KMW_HELPFILE token", () => {
      recogniseToken(TokenTypes.TT_KMW_HELPFILE, 'kmw_helpfile');
    });
    it("can recognise a TT_KMW_HELPTEXT token", () => {
      recogniseToken(TokenTypes.TT_KMW_HELPTEXT, 'kmw_helptext');
    });
    it("can recognise a TT_KMW_RTL token", () => {
      recogniseToken(TokenTypes.TT_KMW_RTL, 'kmw_rtl');
    });
    it("can recognise a TT_LANGUAGE token", () => {
      recogniseToken(TokenTypes.TT_LANGUAGE, 'language');
    });
    it("can recognise a TT_LAYER token", () => {
      recogniseToken(TokenTypes.TT_LAYER, 'layer');
    });
    it("can recognise a TT_LAYOUTFILE token", () => {
      recogniseToken(TokenTypes.TT_LAYOUTFILE, 'layoutfile');
    });
    it("can recognise a TT_MESSAGE token", () => {
      recogniseToken(TokenTypes.TT_MESSAGE, 'message');
    });
    it("can recognise a TT_MNEMONICLAYOUT token", () => {
      recogniseToken(TokenTypes.TT_MNEMONICLAYOUT, 'mnemoniclayout');
    });
    it("can recognise a TT_NAME token", () => {
      recogniseToken(TokenTypes.TT_NAME, 'name');
    });
    it("can recognise a TT_NEWLAYER token", () => {
      recogniseToken(TokenTypes.TT_NEWLAYER, 'newlayer');
    });
    it("can recognise a TT_OLDCHARPOSMATCHING token", () => {
      recogniseToken(TokenTypes.TT_OLDCHARPOSMATCHING, 'oldcharposmatching');
    });
    it("can recognise a TT_OLDLAYER token", () => {
      recogniseToken(TokenTypes.TT_OLDLAYER, 'oldlayer');
    });
    it("can recognise a TT_TARGETS token", () => {
      recogniseToken(TokenTypes.TT_TARGETS, 'targets');
    });
    it("can recognise a TT_VERSION token", () => {
      recogniseToken(TokenTypes.TT_VERSION, 'version');
    });
    it("can recognise a TT_VISUALKEYBOARD token", () => {
      recogniseToken(TokenTypes.TT_VISUALKEYBOARD, 'visualkeyboard');
    });
    it("can recognise a TT_WINDOWSLANGUAGES token", () => {
      recogniseToken(TokenTypes.TT_WINDOWSLANGUAGES, 'windowslanguages');
    });
    it("can recognise a TT_CAPSALWAYSOFF token", () => {
      recogniseToken(TokenTypes.TT_CAPSALWAYSOFF, 'capsalwaysoff');
    });
    it("can recognise a TT_CAPSONONLY token", () => {
      recogniseToken(TokenTypes.TT_CAPSONONLY, 'capsononly');
    });
    it("can recognise a TT_SHIFTFREECAPS token", () => {
      recogniseToken(TokenTypes.TT_SHIFTFREESCAPS, 'shiftfreescaps');
    });
    it("can recognise a TT_ANY token", () => {
      recogniseToken(TokenTypes.TT_ANY, 'any');
    });
    it("can recognise a TT_BASELAYOUT token", () => {
      recogniseToken(TokenTypes.TT_BASELAYOUT, 'baselayout');
    });
    it("can recognise a TT_BEEP token", () => {
      recogniseToken(TokenTypes.TT_BEEP, 'beep');
    });
    it("can recognise a TT_BEGIN token", () => {
      recogniseToken(TokenTypes.TT_BEGIN, 'begin');
    });
    it("can recognise a TT_CALL token", () => {
      recogniseToken(TokenTypes.TT_CALL, 'call');
    });
    it("can recognise a TT_CONTEXT token", () => {
      recogniseToken(TokenTypes.TT_CONTEXT, 'context');
    });
    it("can recognise a TT_DEADKEY token (deadkey)", () => {
      recogniseToken(TokenTypes.TT_DEADKEY, 'deadkey');
    });
    it("can recognise a TT_DEADKEY token (dk)", () => {
      recogniseToken(TokenTypes.TT_DEADKEY, 'dk');
    });
    it("can recognise a TT_GROUP token", () => {
      recogniseToken(TokenTypes.TT_GROUP, 'group');
    });
    it("can recognise a TT_IF token", () => {
      recogniseToken(TokenTypes.TT_IF, 'if');
    });
    it("can recognise a TT_INDEX token", () => {
      recogniseToken(TokenTypes.TT_INDEX, 'index');
    });
    it("can recognise a TT_LAYER token", () => {
      recogniseToken(TokenTypes.TT_LAYER, 'layer');
    });
    it("can recognise a TT_MATCH token", () => {
      recogniseToken(TokenTypes.TT_MATCH, 'match');
    });
    it("can recognise a TT_NOMATCH token", () => {
      recogniseToken(TokenTypes.TT_NOMATCH, 'nomatch');
    });
    it("can recognise a TT_NOTANY token", () => {
      recogniseToken(TokenTypes.TT_NOTANY, 'notany');
    });
    it("can recognise a TT_NUL token", () => {
      recogniseToken(TokenTypes.TT_NUL, 'nul');
    });
    it("can recognise a TT_OUTS token", () => {
      recogniseToken(TokenTypes.TT_OUTS, 'outs');
    });
    it("can recognise a TT_PLATFORM token", () => {
      recogniseToken(TokenTypes.TT_PLATFORM, 'platform');
    });
    it("can recognise a TT_RESET token", () => {
      recogniseToken(TokenTypes.TT_RESET, 'reset');
    });
    it("can recognise a TT_RETURN token", () => {
      recogniseToken(TokenTypes.TT_RETURN, 'return');
    });
    it("can recognise a TT_SAVE token", () => {
      recogniseToken(TokenTypes.TT_SAVE, 'save');
    });
    it("can recognise a TT_SET token", () => {
      recogniseToken(TokenTypes.TT_SET, 'set');
    });
    it("can recognise a TT_STORE token", () => {
      recogniseToken(TokenTypes.TT_STORE, 'store');
    });
    it("can recognise a TT_STORE token in upper case", () => {
      recogniseToken(TokenTypes.TT_STORE, 'STORE');
    });
    it("can recognise a TT_STORE token in mixed case", () => {
      recogniseToken(TokenTypes.TT_STORE, 'Store');
    });
    it("can recognise a TT_USE token", () => {
      recogniseToken(TokenTypes.TT_USE, 'use');
    });
    it("can recognise a TT_UNICODE token", () => {
      recogniseToken(TokenTypes.TT_UNICODE, 'unicode');
    });
    it("can recognise a TT_NEWCONTEXT token", () => {
      recogniseToken(TokenTypes.TT_NEWCONTEXT, 'newcontext');
    });
    it("can recognise a TT_POSTKEYSTROKE token", () => {
      recogniseToken(TokenTypes.TT_POSTKEYSTROKE, 'postkeystroke');
    });
    it("can recognise a TT_READONLY token", () => {
      recogniseToken(TokenTypes.TT_READONLY, 'readonly');
    });
    it("can recognise a TT_USING token", () => {
      recogniseToken(TokenTypes.TT_USING, 'using');
    });
    it("can recognise a TT_KEYS token", () => {
      recogniseToken(TokenTypes.TT_KEYS, 'keys');
    });
    it("can recognise a TT_ANSI token", () => {
      recogniseToken(TokenTypes.TT_ANSI, 'ansi');
    });
    it("can recognise a TT_LEFT_BR token", () => {
      recogniseToken(TokenTypes.TT_LEFT_BR, '(');
    });
    it("can recognise a TT_RIGHT_BR token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_BR, ')');
    });
    it("can recognise a TT_LEFT_SQ token", () => {
      recogniseToken(TokenTypes.TT_LEFT_SQ, '[');
    });
    it("can recognise a TT_RIGHT_SQ token", () => {
      recogniseToken(TokenTypes.TT_RIGHT_SQ, ']');
    });
    it("can recognise a TT_AMPHASAND token", () => {
      recogniseToken(TokenTypes.TT_AMPHASAND, '&');
    });
    it("can recognise a TT_CHEVRON token", () => {
      recogniseToken(TokenTypes.TT_CHEVRON, '>');
    });
    it("can recognise a TT_PLUS token", () => {
      recogniseToken(TokenTypes.TT_PLUS, '+');
    });
    it("can recognise a TT_COMMA token", () => {
      recogniseToken(TokenTypes.TT_COMMA, ',');
    });
    it("can recognise a TT_NOT_EQUAL token", () => {
      recogniseToken(TokenTypes.TT_NOT_EQUAL, '!=');
    });
    it("can recognise a TT_EQUAL token", () => {
      recogniseToken(TokenTypes.TT_EQUAL, '=');
    });
    it("can recognise a TT_U_CHAR token", () => {
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+A');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1B');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1CD');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+1EF0');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+23456');
      recogniseToken(TokenTypes.TT_U_CHAR, 'U+789ABC');
    });
    it("can recognise a TT_STRING token (single quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '\'10.0\'');
    });
    it("can recognise a TT_STRING token (double quote)", () => {
      recogniseToken(TokenTypes.TT_STRING, '"10.0"');
    });
    it("can recognise a TT_STRING token (repeated quotes)", () => {
      recogniseTokens(
        '\'hello\' \'world\'',
        [
          new Token(TokenTypes.TT_STRING,     '\'hello\''),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.TT_STRING,     '\'world\'', 1, 9),
        ]
      );
    });
    it("does not pick out tokens from inside strings", () => {
      recogniseToken(TokenTypes.TT_STRING, '"store"');
    });
    it("can recognise a TT_SHIFT_CODE token", () => {
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'SHIFT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'CTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'LCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'RCTRL');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'ALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'LALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'RALT');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'CAPS');
      recogniseTokenFollowedBySpace(TokenTypes.TT_SHIFT_CODE, 'NCAPS');
    });
    it("can recognise a TT_KEY_CODE token", () => {
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'K_K');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'K_COLON');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'T_17D2_1780');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'U_0030');
      recogniseTokenFollowedByRightSquare(TokenTypes.TT_KEY_CODE, 'A21');
    });
    it("can recognise a TT_KEY_CODE token (followed by space)", () => {
      recogniseTokens(
        'K_K ]',
        [
          new Token(TokenTypes.TT_KEY_CODE, 'K_K'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 4),
          new Token(TokenTypes.TT_RIGHT_SQ, ']', 1, 5),
        ]
      );
    });
    it("can recognise a TT_COMMENT token", () => {
      recogniseToken(TokenTypes.TT_COMMENT, 'c This tells Keyman which keys should have casing behavior applied');
    });
    it("can recognise a TT_COMMENT token (followed by LF)", () => {
      recogniseTokens(
        'c This tells Keyman which keys should have casing behavior applied\n',
        [
          new Token(TokenTypes.TT_COMMENT, 'c This tells Keyman which keys should have casing behavior applied'),
          new Token(TokenTypes.TT_NEWLINE, '\n', 1, 67),
        ]
      );
    });
    it("can recognise a TT_WHITESPACE token (single space)", () => {
      recogniseToken(TokenTypes.TT_WHITESPACE, ' ');
    });
    it("can recognise a TT_CONTINUATION token (no space after)", () => {
      recogniseTokens(
        '\\\n',
        [
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_NEWLINE, '\n', 1, 2),
        ]
      );
    });
    it("can recognise a TT_CONTINUATION token (space after)", () => {
      recogniseTokens(
        '\\ \n',
        [
          new Token(TokenTypes.TT_CONTINUATION, '\\'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 2),
          new Token(TokenTypes.TT_NEWLINE, '\n', 1, 3),
        ]
      );
    });
    it("can recognise multiple TT_CONTINUATION tokens", () => {
      recogniseTokens(
        'store(LaoConsonants) U+0E81 U+0E82 U+0E84 U+0E87 U+0E88 U+0E8A U+0E8D U+0E94 \\\n' +
        '                     U+0E95 U+0E96 U+0E97 U+0E99 U+0E9A U+0E9B U+0E9C U+0E9D \\\n' +
        '                     U+0E9E U+0E9F U+0EA1 U+0EA2 U+0EA3 U+0EA5 U+0EA7 U+0EAA \\\n' +
        '                     U+0EAB U+0EAD U+0EAE    c list of all the Lao consonants\n',
        [
          new Token(TokenTypes.TT_STORE, 'store'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, 'LaoConsonants', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 20),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 21),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E81', 1, 22),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 28),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E82', 1, 29),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 35),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E84', 1, 36),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 42),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E87', 1, 43),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 49),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E88', 1, 50),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 56),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E8A', 1, 57),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 63),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E8D', 1, 64),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 70),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E94', 1, 71),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 77),
          new Token(TokenTypes.TT_CONTINUATION, '\\', 1, 78),
          new Token(TokenTypes.TT_NEWLINE, '\n', 1, 79),
          new Token(TokenTypes.TT_WHITESPACE, '                     ', 2, 1),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E95', 2, 22),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 28),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E96', 2, 29),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 35),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E97', 2, 36),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 42),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E99', 2, 43),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 49),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9A', 2, 50),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 56),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9B', 2, 57),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 63),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9C', 2, 64),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 70),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9D', 2, 71),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 2, 77),
          new Token(TokenTypes.TT_CONTINUATION, '\\', 2, 78),
          new Token(TokenTypes.TT_NEWLINE, '\n', 2, 79),
          new Token(TokenTypes.TT_WHITESPACE, '                     ', 3, 1),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9E', 3, 22),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 28),
          new Token(TokenTypes.TT_U_CHAR, 'U+0E9F', 3, 29),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 35),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EA1', 3, 36),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 42),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EA2', 3, 43),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 49),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EA3', 3, 50),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 56),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EA5', 3, 57),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 63),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EA7', 3, 64),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 70),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EAA', 3, 71),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 3, 77),
          new Token(TokenTypes.TT_CONTINUATION, '\\', 3, 78),
          new Token(TokenTypes.TT_NEWLINE, '\n', 3, 79),
          new Token(TokenTypes.TT_WHITESPACE, '                     ', 4, 1),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EAB', 4, 22),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 4, 28),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EAD', 4, 29),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 4, 35),
          new Token(TokenTypes.TT_U_CHAR, 'U+0EAE', 4, 36),
          new Token(TokenTypes.TT_WHITESPACE, '    ', 4, 42),
          new Token(TokenTypes.TT_COMMENT, 'c list of all the Lao consonants', 4, 46),
          new Token(TokenTypes.TT_NEWLINE, '\n', 4, 78),
        ]
      );
    });
    it("can recognise a TT_NEWLINE token (LF)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\n');
    });
    it("can recognise a TT_NEWLINE token (CR)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\r');
    });
    it("can recognise a TT_NEWLINE token (CRLF)", () => {
      recogniseToken(TokenTypes.TT_NEWLINE, '\r\n');
    });
    it("can recognise a TT_PARAMETER token", () => {
      recogniseTokens(
        'main)',
        [
          new Token(TokenTypes.TT_PARAMETER, 'main'),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets", () => {
      recogniseTokens(
        '(main)',
        [
          new Token(TokenTypes.TT_LEFT_BR, '('),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 6),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets (single space before)", () => {
      recogniseTokens(
        '( main)',
        [
          new Token(TokenTypes.TT_LEFT_BR, '('),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 2),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 3),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise a TT_PARAMETER token in brackets (single space after)", () => {
      recogniseTokens(
        '(main )',
        [
          new Token(TokenTypes.TT_LEFT_BR, '('),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise two TT_PARAMETER tokens in brackets (comma separated)", () => {
      recogniseTokens(
        '(main,2)',
        [
          new Token(TokenTypes.TT_LEFT_BR, '('),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.TT_COMMA, ',', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, '2', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 8),
        ]
      );
    });
    it("can recognise two TT_PARAMETER tokens in brackets (space separated)", () => {
      recogniseTokens(
        '(main 2)',
        [
          new Token(TokenTypes.TT_LEFT_BR, '('),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 2),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, '2', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR,   ')', 1, 8),
        ]
      );
    });
    it("can recognise a bitmap store", () => {
      recogniseStoreWithString(TokenTypes.TT_BITMAP, 'khmer_angkor.ico');
    });
    it("can recognise a copyright store", () => {
      recogniseStoreWithString(TokenTypes.TT_COPYRIGHT, '© SIL Global');
    });
    it("can recognise a displaymap store", () => {
      recogniseStoreWithString(TokenTypes.TT_DISPLAYMAP, '../../../shared/fonts/kbd/kbdkhmr/KbdKhmr.json');
    });
    it("can recognise a keyboardversion store", () => {
      recogniseStoreWithString(TokenTypes.TT_KEYBOARDVERSION, '2.0');
    });
    it("can recognise a layoutfile store", () => {
      recogniseStoreWithString(TokenTypes.TT_LAYOUTFILE, 'khmer_angkor.keyman-touch-layout');
    });
    it("can recognise a name store", () => {
      recogniseStoreWithString(TokenTypes.TT_NAME, "Khmer Angkor");
    });
    it("can recognise a message store", () => {
      recogniseStoreWithString(TokenTypes.TT_MESSAGE, "More than just a Khmer Unicode keyboard.");
    });
    it("can recognise a targets store", () => {
      recogniseStoreWithString(TokenTypes.TT_TARGETS, 'any');
    });
    it("can recognise a version store", () => {
      recogniseStoreWithString(TokenTypes.TT_VERSION, 'khmer_angkor.kvks');
    });
    it("can recognise a visualkeyboard store", () => {
      recogniseStoreWithString(TokenTypes.TT_VISUALKEYBOARD, '10.0');
    });
    it("can recognise a capsalwaysoff store", () => {
      recogniseStoreWithString(TokenTypes.TT_CAPSALWAYSOFF, "1");
    });
    it("can recognise a capsononly store", () => {
      recogniseStoreWithString(TokenTypes.TT_CAPSONONLY, "1");
    });
    it("can recognise a shiftfreescaps store", () => {
      recogniseStoreWithString(TokenTypes.TT_SHIFTFREESCAPS, "1");
    });
    it("can recognise a begin statement (unicode)", () => {
      recogniseTokens(
        'begin Unicode > use(main)',
        [
          new Token(TokenTypes.TT_BEGIN, 'begin'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.TT_UNICODE, 'Unicode', 1, 7),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 14),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 15),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 16),
          new Token(TokenTypes.TT_USE, 'use', 1, 17),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 20),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 21),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 25),
        ]
      );
    });
    it("can recognise a begin statement (postkeystroke)", () => {
      recogniseTokens(
        'begin PostKeystroke > use(PostKeystroke)',
        [
          new Token(TokenTypes.TT_BEGIN, 'begin'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.TT_POSTKEYSTROKE, 'PostKeystroke', 1, 7),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 20),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 21),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.TT_USE, 'use', 1, 23),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 26),
          new Token(TokenTypes.TT_POSTKEYSTROKE, 'PostKeystroke', 1, 27), // recognised as keyword
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 40),
        ]
      );
    });
    it("can recognise a group statement (readonly)", () => {
      recogniseTokens(
        'group(NewContext) readonly',
        [
          new Token(TokenTypes.TT_GROUP, 'group'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_NEWCONTEXT, 'NewContext', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.TT_READONLY, 'readonly', 1, 19),
        ]
      );
    });
    it("can recognise a group statement (using keys)", () => {
      recogniseTokens(
        'group(main) using keys',
        [
          new Token(TokenTypes.TT_GROUP, 'group'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, 'main', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 11),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 12),
          new Token(TokenTypes.TT_USING, 'using', 1, 13),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.TT_KEYS, 'keys', 1, 19),
        ]
      );
    });
    it("can recognise an if statement (using any, context, layer)", () => {
      recogniseTokens(
        'if(&newLayer = "") if(&layer = \'shift\') any(ShiftOutSingle) > context layer(\'default\')',
        [
          new Token(TokenTypes.TT_IF, 'if'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 3),
          new Token(TokenTypes.TT_AMPHASAND, '&', 1, 4),
          new Token(TokenTypes.TT_NEWLAYER, 'newLayer', 1, 5),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 13),
          new Token(TokenTypes.TT_EQUAL, '=', 1, 14),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 15),
          new Token(TokenTypes.TT_STRING, '""', 1, 16),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 18),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 19),
          new Token(TokenTypes.TT_IF, 'if', 1, 20),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 22),
          new Token(TokenTypes.TT_AMPHASAND, '&', 1, 23),
          new Token(TokenTypes.TT_LAYER, 'layer', 1, 24),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 29),
          new Token(TokenTypes.TT_EQUAL, '=', 1, 30),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 31),
          new Token(TokenTypes.TT_STRING, '\'shift\'', 1, 32),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 39),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 40),
          new Token(TokenTypes.TT_ANY, 'any', 1, 41),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 44),
          new Token(TokenTypes.TT_PARAMETER, 'ShiftOutSingle', 1, 45),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 59),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 60),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 61),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 62),
          new Token(TokenTypes.TT_CONTEXT, 'context', 1, 63),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 70),
          new Token(TokenTypes.TT_LAYER, 'layer', 1, 71),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 76),
          new Token(TokenTypes.TT_STRING, '\'default\'', 1, 77),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 86),
        ]
      );
    });
    it("can recognise a match statement", () => {
      recogniseTokens(
        'match > use(CombineDiacritics)',
        [
          new Token(TokenTypes.TT_MATCH, 'match'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 6),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 7),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.TT_USE, 'use', 1, 9),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 12),
          new Token(TokenTypes.TT_PARAMETER, 'CombineDiacritics', 1, 13),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 30),
        ]
      );
    });
    it("can recognise a nomatch statement", () => {
      recogniseTokens(
        'nomatch > layer(\'default\')',
        [
          new Token(TokenTypes.TT_NOMATCH, 'nomatch'),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 8),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 9),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 10),
          new Token(TokenTypes.TT_LAYER, 'layer', 1, 11),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 16),
          new Token(TokenTypes.TT_STRING, '\'default\'', 1, 17),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 26),
        ]
      );
    });
    it("can recognise a platform statement", () => {
      recogniseTokens(
        'platform(\'touch\') > use(detectStartOfSentence)',
        [
          new Token(TokenTypes.TT_PLATFORM, 'platform'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 9),
          new Token(TokenTypes.TT_STRING, '\'touch\'', 1, 10),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.TT_WHITESPACE, ' ',1, 18),
          new Token(TokenTypes.TT_CHEVRON, '>', 1, 19),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 20),
          new Token(TokenTypes.TT_USE, 'use', 1, 21),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 24),
          new Token(TokenTypes.TT_PARAMETER, 'detectStartOfSentence', 1, 25),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 46),
        ]
      );
    });
    it("can recognise a store statement using outs", () => {
      recogniseTokens(
        'store(ShiftOutAll)  outs(ShiftOutSingle) outs(vCombo1) outs(vCombo2) outs(vCombo3)',
        [
          new Token(TokenTypes.TT_STORE, 'store'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, 'ShiftOutAll', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 18),
          new Token(TokenTypes.TT_WHITESPACE, '  ', 1, 19),
          new Token(TokenTypes.TT_OUTS, 'outs', 1, 21),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 25),
          new Token(TokenTypes.TT_PARAMETER, 'ShiftOutSingle', 1, 26),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 40),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 41),
          new Token(TokenTypes.TT_OUTS, 'outs', 1, 42),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 46),
          new Token(TokenTypes.TT_PARAMETER, 'vCombo1', 1, 47),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 54),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 55),
          new Token(TokenTypes.TT_OUTS, 'outs', 1, 56),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 60),
          new Token(TokenTypes.TT_PARAMETER, 'vCombo2', 1, 61),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 68),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 69),
          new Token(TokenTypes.TT_OUTS, 'outs', 1, 70),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 74),
          new Token(TokenTypes.TT_PARAMETER, 'vCombo3', 1, 75),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 82),
        ]
      );
    });
    it("can recognise a store statement (khmer string)", () => {
      recogniseTokens(
        'store(ShiftOutSingle) \'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'',
        [
          new Token(TokenTypes.TT_STORE, 'store'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, 'ShiftOutSingle', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 21),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.TT_STRING, '\'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'', 1, 23),
        ]
      );
    });
    it("can recognise a store statement (unicode chars)", () => {
      recogniseTokens(
        'store(whitespace) \' \' U+00A0 U+000D U+000A',
        [
          new Token(TokenTypes.TT_STORE, 'store'),
          new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
          new Token(TokenTypes.TT_PARAMETER, 'whitespace', 1, 7),
          new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 17),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 18),
          new Token(TokenTypes.TT_STRING, '\' \'', 1, 19),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 22),
          new Token(TokenTypes.TT_U_CHAR, 'U+00A0', 1, 23),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 29),
          new Token(TokenTypes.TT_U_CHAR, 'U+000D', 1, 30),
          new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 36),
          new Token(TokenTypes.TT_U_CHAR, 'U+000A', 1, 37),
        ]
      );
    });
  });
});

function recogniseToken(type: TokenTypes, text: String): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse();
  const expected = [new Token(type, text)];
  assert.deepEqual(actual, expected);
}

function recogniseTokens(text: String, expected: Token[]): void {
  const lexer    = new Lexer(new String(text));
  const actual   = lexer.parse();
  assert.deepEqual(actual, expected);
}

function recogniseTokenFollowedBySpace(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text} `,
    [
      new Token(type, text),
      new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 1+text.length),
    ]
  );
}

function recogniseTokenFollowedByRightSquare(type: TokenTypes, text: String): void {
  recogniseTokens(
    `${text}]`,
    [
      new Token(type, text),
      new Token(TokenTypes.TT_RIGHT_SQ, ']', 1, 1+text.length),
    ]
  );
}

function recogniseStoreWithString(type: TokenTypes, text: String) {
  const value = TokenTypes[type].substring(3).toLowerCase();
  recogniseTokens(
    `store(&${value}) '${text}'`,
    [
      new Token(TokenTypes.TT_STORE, 'store'),
      new Token(TokenTypes.TT_LEFT_BR, '(', 1, 6),
      new Token(TokenTypes.TT_AMPHASAND, '&', 1, 7),
      new Token(type, value, 1, 8),
      new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 8+value.length),
      new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 9+value.length),
      new Token(TokenTypes.TT_STRING, `'${text}'`, 1, 10+value.length),
    ]
  );
}
