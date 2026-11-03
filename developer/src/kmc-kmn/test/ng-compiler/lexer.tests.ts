/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { Lexer, Token } from '../../src/ng-compiler/lexer.js'

describe("Lexer Tests", () => {
  describe("Lexer", () => {
    it("can recognise a BASELAYOUT token", () => {
      recogniseToken(TokenType.BASELAYOUT, '&baselayout');
      recogniseToken(TokenType.BASELAYOUT, '&BASELAYOUT');
      recogniseToken(TokenType.BASELAYOUT, '&BaseLayout');
    });
    it("can recognise a BITMAP token", () => {
      recogniseToken(TokenType.BITMAP, '&bitmap');
      recogniseToken(TokenType.BITMAP, '&BITMAP');
      recogniseToken(TokenType.BITMAP, '&Bitmap');
    });
    it("can recognise a CASEDKEYS token", () => {
      recogniseToken(TokenType.CASEDKEYS, '&casedkeys');
      recogniseToken(TokenType.CASEDKEYS, '&CASEDKEYS');
      recogniseToken(TokenType.CASEDKEYS, '&CasedKeys');
    });
    it("can recognise a COPYRIGHT token", () => {
      recogniseToken(TokenType.COPYRIGHT, '&copyright');
      recogniseToken(TokenType.COPYRIGHT, '&COPYRIGHT');
      recogniseToken(TokenType.COPYRIGHT, '&Copyright');
    });
    it("can recognise a DISPLAYMAP token", () => {
      recogniseToken(TokenType.DISPLAYMAP, '&displaymap');
      recogniseToken(TokenType.DISPLAYMAP, '&DISPLAYMAP');
      recogniseToken(TokenType.DISPLAYMAP, '&DisplayMap');
    });
    it("can recognise a ETHNOLOGUECODE token", () => {
      recogniseToken(TokenType.ETHNOLOGUECODE, '&ethnologuecode');
      recogniseToken(TokenType.ETHNOLOGUECODE, '&ETHNOLOGUECODE');
      recogniseToken(TokenType.ETHNOLOGUECODE, '&EthnologueCode');
    });
    it("can recognise a HOTKEY token", () => {
      recogniseToken(TokenType.HOTKEY, '&hotkey');
      recogniseToken(TokenType.HOTKEY, '&HOTKEY');
      recogniseToken(TokenType.HOTKEY, '&HotKey');
    });
    it("can recognise a INCLUDECODES token", () => {
      recogniseToken(TokenType.INCLUDECODES, '&includecodes');
      recogniseToken(TokenType.INCLUDECODES, '&INCLUDECODES');
      recogniseToken(TokenType.INCLUDECODES, '&IncludeCodes');
    });
    it("can recognise a KEYBOARDVERSION token", () => {
      recogniseToken(TokenType.KEYBOARDVERSION, '&keyboardversion');
      recogniseToken(TokenType.KEYBOARDVERSION, '&KEYBOARDVERSION');
      recogniseToken(TokenType.KEYBOARDVERSION, '&KeyboardVersion');
    });
    it("can recognise a KMW_EMBEDCSS token", () => {
      recogniseToken(TokenType.KMW_EMBEDCSS, '&kmw_embedcss');
      recogniseToken(TokenType.KMW_EMBEDCSS, '&KMW_EMBEDCSS');
      recogniseToken(TokenType.KMW_EMBEDCSS, '&Kmw_embedcss');
    });
    it("can recognise a KMW_EMBEDJS token", () => {
      recogniseToken(TokenType.KMW_EMBEDJS, '&kmw_embedjs');
      recogniseToken(TokenType.KMW_EMBEDJS, '&KMW_EMBEDJS');
      recogniseToken(TokenType.KMW_EMBEDJS, '&Kmw_embedjs');
    });
    it("can recognise a KMW_HELPFILE token", () => {
      recogniseToken(TokenType.KMW_HELPFILE, '&kmw_helpfile');
      recogniseToken(TokenType.KMW_HELPFILE, '&KMW_HELPFILE');
      recogniseToken(TokenType.KMW_HELPFILE, '&Kmw_helpfile');
    });
    it("can recognise a KMW_HELPTEXT token", () => {
      recogniseToken(TokenType.KMW_HELPTEXT, '&kmw_helptext');
      recogniseToken(TokenType.KMW_HELPTEXT, '&KMW_HELPTEXT');
      recogniseToken(TokenType.KMW_HELPTEXT, '&Kmw_helptext');
    });
    it("can recognise a KMW_RTL token", () => {
      recogniseToken(TokenType.KMW_RTL, '&kmw_rtl');
      recogniseToken(TokenType.KMW_RTL, '&KMW_RTL');
      recogniseToken(TokenType.KMW_RTL, '&Kmw_rtl');
    });
    it("can recognise a LANGUAGE token", () => {
      recogniseToken(TokenType.LANGUAGE, '&language');
      recogniseToken(TokenType.LANGUAGE, '&LANGUAGE');
      recogniseToken(TokenType.LANGUAGE, '&Language');
    });
    it("can recognise a LAYER token", () => {
      recogniseToken(TokenType.LAYER, '&layer');
      recogniseToken(TokenType.LAYER, '&LAYER');
      recogniseToken(TokenType.LAYER, '&Layer');
    });
    it("can recognise a LAYOUTFILE token", () => {
      recogniseToken(TokenType.LAYOUTFILE, '&layoutfile');
      recogniseToken(TokenType.LAYOUTFILE, '&LAYOUTFILE');
      recogniseToken(TokenType.LAYOUTFILE, '&LayoutFile');
    });
    it("can recognise a MESSAGE token", () => {
      recogniseToken(TokenType.MESSAGE, '&message');
      recogniseToken(TokenType.MESSAGE, '&MESSAGE');
      recogniseToken(TokenType.MESSAGE, '&Message');
    });
    it("can recognise a MNEMONICLAYOUT token", () => {
      recogniseToken(TokenType.MNEMONICLAYOUT, '&mnemoniclayout');
      recogniseToken(TokenType.MNEMONICLAYOUT, '&MNEMONICLAYOUT');
      recogniseToken(TokenType.MNEMONICLAYOUT, '&MnemonicLayout');
    });
    it("can recognise a NAME token", () => {
      recogniseToken(TokenType.NAME, '&name');
      recogniseToken(TokenType.NAME, '&NAME');
      recogniseToken(TokenType.NAME, '&Name');
    });
    it("can recognise a NEWLAYER token", () => {
      recogniseToken(TokenType.NEWLAYER, '&newlayer');
      recogniseToken(TokenType.NEWLAYER, '&NEWLAYER');
      recogniseToken(TokenType.NEWLAYER, '&NewLayer');
    });
    it("can recognise a OLDCHARPOSMATCHING token", () => {
      recogniseToken(TokenType.OLDCHARPOSMATCHING, '&oldcharposmatching');
      recogniseToken(TokenType.OLDCHARPOSMATCHING, '&OLDCHARPOSMATCHING');
      recogniseToken(TokenType.OLDCHARPOSMATCHING, '&OldCharPosMatching');
    });
    it("can recognise a OLDLAYER token", () => {
      recogniseToken(TokenType.OLDLAYER, '&oldlayer');
      recogniseToken(TokenType.OLDLAYER, '&OLDLAYER');
      recogniseToken(TokenType.OLDLAYER, '&OldLayer');
    });
    it("can recognise a PLATFORM token", () => {
      recogniseToken(TokenType.PLATFORM, '&platform');
      recogniseToken(TokenType.PLATFORM, '&PLATFORM');
      recogniseToken(TokenType.PLATFORM, '&Platform');
    });
    it("can recognise a TARGETS token", () => {
      recogniseToken(TokenType.TARGETS, '&targets');
      recogniseToken(TokenType.TARGETS, '&TARGETS');
      recogniseToken(TokenType.TARGETS, '&Targets');
    });
    it("can recognise a VERSION token", () => {
      recogniseToken(TokenType.VERSION, '&version');
      recogniseToken(TokenType.VERSION, '&VERSION');
      recogniseToken(TokenType.VERSION, '&Version');
    });
    it("can recognise a VISUALKEYBOARD token", () => {
      recogniseToken(TokenType.VISUALKEYBOARD, '&visualkeyboard');
      recogniseToken(TokenType.VISUALKEYBOARD, '&VISUALKEYBOARD');
      recogniseToken(TokenType.VISUALKEYBOARD, '&VisualKeyboard');
    });
    it("can recognise a WINDOWSLANGUAGES token", () => {
      recogniseToken(TokenType.WINDOWSLANGUAGES, '&windowslanguages');
      recogniseToken(TokenType.WINDOWSLANGUAGES, '&WINDOWSLANGUAGES');
      recogniseToken(TokenType.WINDOWSLANGUAGES, '&WindowsLanguages');
    });
    it("can recognise a CAPSALWAYSOFF token", () => {
      recogniseToken(TokenType.CAPSALWAYSOFF, '&capsalwaysoff');
      recogniseToken(TokenType.CAPSALWAYSOFF, '&CAPSALWAYSOFF');
      recogniseToken(TokenType.CAPSALWAYSOFF, '&CapsAlwaysOff');
    });
    it("can recognise a CAPSONONLY token", () => {
      recogniseToken(TokenType.CAPSONONLY, '&capsononly');
      recogniseToken(TokenType.CAPSONONLY, '&CAPSONONLY');
      recogniseToken(TokenType.CAPSONONLY, '&CapsOnOnly');
    });
    it("can recognise a SHIFTFREECAPS token", () => {
      recogniseToken(TokenType.SHIFTFREESCAPS, '&shiftfreescaps');
      recogniseToken(TokenType.SHIFTFREESCAPS, '&SHIFTFREESCAPS');
      recogniseToken(TokenType.SHIFTFREESCAPS, '&ShiftFreesCaps');
    });
    it("can recognise a CAPS token", () => {
      recogniseToken(TokenType.CAPS, 'caps');
      recogniseToken(TokenType.CAPS, 'CAPS');
      recogniseToken(TokenType.CAPS, 'Caps');
    });
    it("can recognise an ALWAYS token", () => {
      recogniseToken(TokenType.ALWAYS, 'always');
      recogniseToken(TokenType.ALWAYS, 'ALWAYS');
      recogniseToken(TokenType.ALWAYS, 'Always');
    });
    it("can recognise an OFF token", () => {
      recogniseToken(TokenType.OFF, 'off');
      recogniseToken(TokenType.OFF, 'OFF');
      recogniseToken(TokenType.OFF, 'Off');
    });
    it("can recognise an ON token", () => {
      recogniseToken(TokenType.ON, 'on');
      recogniseToken(TokenType.ON, 'ON');
      recogniseToken(TokenType.ON, 'On');
    });
    it("can recognise an ONLY token", () => {
      recogniseToken(TokenType.ONLY, 'only');
      recogniseToken(TokenType.ONLY, 'ONLY');
      recogniseToken(TokenType.ONLY, 'Only');
    });
    it("can recognise a SHIFT token", () => {
      recogniseToken(TokenType.SHIFT, 'shift');
      recogniseToken(TokenType.SHIFT, 'SHIFT');
      recogniseToken(TokenType.SHIFT, 'Shift');
    });
    it("can recognise a FREES token", () => {
      recogniseToken(TokenType.FREES, 'frees');
      recogniseToken(TokenType.FREES, 'FREES');
      recogniseToken(TokenType.FREES, 'Frees');
    });
    it("can recognise a BITMAP_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.BITMAP_HEADER, 'bitmap');
      recogniseTokenFollowedBySpace(TokenType.BITMAP_HEADER, 'BITMAP');
      recogniseTokenFollowedBySpace(TokenType.BITMAP_HEADER, 'Bitmap');
    });
    it("can recognise a COPYRIGHTP_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.COPYRIGHT_HEADER, 'copyright');
      recogniseTokenFollowedBySpace(TokenType.COPYRIGHT_HEADER, 'COPYRIGHT');
      recogniseTokenFollowedBySpace(TokenType.COPYRIGHT_HEADER, 'Copyright');
    });
    it("can recognise a HOTKEY_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.HOTKEY_HEADER, 'hotkey');
      recogniseTokenFollowedBySpace(TokenType.HOTKEY_HEADER, 'HOTKEY');
      recogniseTokenFollowedBySpace(TokenType.HOTKEY_HEADER, 'HotKey');
    });
    it("can recognise a LANGUAGE_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.LANGUAGE_HEADER, 'language');
      recogniseTokenFollowedBySpace(TokenType.LANGUAGE_HEADER, 'LANGUAGE');
      recogniseTokenFollowedBySpace(TokenType.LANGUAGE_HEADER, 'Language');
    });
    it("can recognise a LAYOUT_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.LAYOUT_HEADER, 'layout');
      recogniseTokenFollowedBySpace(TokenType.LAYOUT_HEADER, 'LAYOUT');
      recogniseTokenFollowedBySpace(TokenType.LAYOUT_HEADER, 'Layout');
    });
    it("can recognise a MESSAGE_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.MESSAGE_HEADER, 'message');
      recogniseTokenFollowedBySpace(TokenType.MESSAGE_HEADER, 'MESSAGE');
      recogniseTokenFollowedBySpace(TokenType.MESSAGE_HEADER, 'Message');
    });
    it("can recognise a NAME_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.NAME_HEADER, 'name');
      recogniseTokenFollowedBySpace(TokenType.NAME_HEADER, 'NAME');
      recogniseTokenFollowedBySpace(TokenType.NAME_HEADER, 'Name');
    });
    it("can recognise a VERSION_HEADER token", () => {
      recogniseTokenFollowedBySpace(TokenType.VERSION_HEADER, 'version');
      recogniseTokenFollowedBySpace(TokenType.VERSION_HEADER, 'VERSION');
      recogniseTokenFollowedBySpace(TokenType.VERSION_HEADER, 'Version');
    });
    it("can recognise a BASELAYOUT_SHORTCUT token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.BASELAYOUT_SHORTCUT, 'baselayout');
      recogniseTokenFollowedByLeftBracket(TokenType.BASELAYOUT_SHORTCUT, 'BASELAYOUT');
      recogniseTokenFollowedByLeftBracket(TokenType.BASELAYOUT_SHORTCUT, 'Baselayout');
    });
    it("can recognise a LAYER_SHORTCUT token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.LAYER_SHORTCUT, 'layer');
      recogniseTokenFollowedByLeftBracket(TokenType.LAYER_SHORTCUT, 'LAYER');
      recogniseTokenFollowedByLeftBracket(TokenType.LAYER_SHORTCUT, 'Layer');
    });
    it("can recognise a PLATFORM_SHORTCUT token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.PLATFORM_SHORTCUT, 'platform');
      recogniseTokenFollowedByLeftBracket(TokenType.PLATFORM_SHORTCUT, 'PLATFORM');
      recogniseTokenFollowedByLeftBracket(TokenType.PLATFORM_SHORTCUT, 'Platform');
    });
    it("can recognise an ANY token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.ANY, 'any');
      recogniseTokenFollowedByLeftBracket(TokenType.ANY, 'ANY');
      recogniseTokenFollowedByLeftBracket(TokenType.ANY, 'Any');
    });
    it("can recognise a BEEP token", () => {
      recogniseToken(TokenType.BEEP, 'beep');
      recogniseToken(TokenType.BEEP, 'BEEP');
      recogniseToken(TokenType.BEEP, 'Beep');
    });
    it("can recognise a BEGIN token", () => {
      recogniseToken(TokenType.BEGIN, 'begin');
      recogniseToken(TokenType.BEGIN, 'BEGIN');
      recogniseToken(TokenType.BEGIN, 'Begin');
    });
    it("can recognise a CALL token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.CALL, 'call');
      recogniseTokenFollowedByLeftBracket(TokenType.CALL, 'CALL');
      recogniseTokenFollowedByLeftBracket(TokenType.CALL, 'Call');
    });
    it("can recognise a CONTEXT token", () => {
      recogniseToken(TokenType.CONTEXT, 'context');
      recogniseToken(TokenType.CONTEXT, 'CONTEXT');
      recogniseToken(TokenType.CONTEXT, 'Context');
    });
    it("can recognise a DEADKEY token (deadkey)", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'deadkey');
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'DEADKEY');
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'DeadKey');
    });
    it("can recognise a DEADKEY token (dk)", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'dk');
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'DK');
      recogniseTokenFollowedByLeftBracket(TokenType.DEADKEY, 'Dk');
    });
    it("can recognise a GROUP token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.GROUP, 'group');
      recogniseTokenFollowedByLeftBracket(TokenType.GROUP, 'GROUP');
      recogniseTokenFollowedByLeftBracket(TokenType.GROUP, 'Group');
    });
    it("can recognise a IF token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.IF, 'if');
      recogniseTokenFollowedByLeftBracket(TokenType.IF, 'IF');
      recogniseTokenFollowedByLeftBracket(TokenType.IF, 'If');
    });
    it("can recognise a INDEX token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.INDEX, 'index');
      recogniseTokenFollowedByLeftBracket(TokenType.INDEX, 'INDEX');
      recogniseTokenFollowedByLeftBracket(TokenType.INDEX, 'Index');
    });
    it("can recognise a MATCH token", () => {
      recogniseToken(TokenType.MATCH, 'match');
      recogniseToken(TokenType.MATCH, 'MATCH');
      recogniseToken(TokenType.MATCH, 'Match');
    });
    it("can recognise a NOMATCH token", () => {
      recogniseToken(TokenType.NOMATCH, 'nomatch');
      recogniseToken(TokenType.NOMATCH, 'NOMATCH');
      recogniseToken(TokenType.NOMATCH, 'NoMatch');
    });
    it("can recognise a NOTANY token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.NOTANY, 'notany');
      recogniseTokenFollowedByLeftBracket(TokenType.NOTANY, 'NOTANY');
      recogniseTokenFollowedByLeftBracket(TokenType.NOTANY, 'NotAny');
    });
    it("can recognise a NUL token", () => {
      recogniseToken(TokenType.NUL, 'nul');
      recogniseToken(TokenType.NUL, 'NUL');
      recogniseToken(TokenType.NUL, 'Nul');
    });
    it("can recognise a OUTS token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.OUTS, 'outs');
      recogniseTokenFollowedByLeftBracket(TokenType.OUTS, 'OUTS');
      recogniseTokenFollowedByLeftBracket(TokenType.OUTS, 'Outs');
    });
    it("can recognise a RESET token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.RESET, 'reset');
      recogniseTokenFollowedByLeftBracket(TokenType.RESET, 'RESET');
      recogniseTokenFollowedByLeftBracket(TokenType.RESET, 'Reset');
    });
    it("can recognise a RETURN token", () => {
      recogniseToken(TokenType.RETURN, 'return');
      recogniseToken(TokenType.RETURN, 'RETURN');
      recogniseToken(TokenType.RETURN, 'Return');
    });
    it("can recognise a SAVE token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.SAVE, 'save');
      recogniseTokenFollowedByLeftBracket(TokenType.SAVE, 'SAVE');
      recogniseTokenFollowedByLeftBracket(TokenType.SAVE, 'Save');
    });
    it("can recognise a SET token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.SET, 'set');
      recogniseTokenFollowedByLeftBracket(TokenType.SET, 'SET');
      recogniseTokenFollowedByLeftBracket(TokenType.SET, 'Set');
    });
    it("can recognise a STORE token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.STORE, 'store');
      recogniseTokenFollowedByLeftBracket(TokenType.STORE, 'STORE');
      recogniseTokenFollowedByLeftBracket(TokenType.STORE, 'Store');
    });
    it("can recognise a USE token", () => {
      recogniseTokenFollowedByLeftBracket(TokenType.USE, 'use');
      recogniseTokenFollowedByLeftBracket(TokenType.USE, 'USE');
      recogniseTokenFollowedByLeftBracket(TokenType.USE, 'Use');
    });
    it("can recognise a UNICODE token", () => {
      recogniseToken(TokenType.UNICODE, 'unicode');
      recogniseToken(TokenType.UNICODE, 'UNICODE');
      recogniseToken(TokenType.UNICODE, 'Unicode');
    });
    it("can recognise a NEWCONTEXT token", () => {
      recogniseToken(TokenType.NEWCONTEXT, 'newcontext');
      recogniseToken(TokenType.NEWCONTEXT, 'NEWCONTEXT');
      recogniseToken(TokenType.NEWCONTEXT, 'NewContext');
    });
    it("can recognise a POSTKEYSTROKE token", () => {
      recogniseToken(TokenType.POSTKEYSTROKE, 'postkeystroke');
      recogniseToken(TokenType.POSTKEYSTROKE, 'POSTKEYSTROKE');
      recogniseToken(TokenType.POSTKEYSTROKE, 'PostKeystroke');
    });
    it("can recognise a ANSI token", () => {
      recogniseToken(TokenType.ANSI, 'ansi');
      recogniseToken(TokenType.ANSI, 'ANSI');
      recogniseToken(TokenType.ANSI, 'Ansi');
    });
    it("can recognise a READONLY token", () => {
      recogniseToken(TokenType.READONLY, 'readonly');
      recogniseToken(TokenType.READONLY, 'READONLY');
      recogniseToken(TokenType.READONLY, 'ReadOnly');
    });
    it("can recognise a USING token", () => {
      recogniseToken(TokenType.USING, 'using');
      recogniseToken(TokenType.USING, 'USING');
      recogniseToken(TokenType.USING, 'Using');
    });
    it("can recognise a KEYS token", () => {
      recogniseToken(TokenType.KEYS, 'keys');
      recogniseToken(TokenType.KEYS, 'KEYS');
      recogniseToken(TokenType.KEYS, 'Keys');
    });
    it("can recognise a KEYMAN token", () => {
      recogniseToken(TokenType.KEYMAN, '$keyman:');
      recogniseToken(TokenType.KEYMAN, '$KEYMAN:');
      recogniseToken(TokenType.KEYMAN, '$Keyman:');
    });
    it("can recognise a KEYMANONLY token", () => {
      recogniseToken(TokenType.KEYMANONLY, '$keymanonly:');
      recogniseToken(TokenType.KEYMANONLY, '$KEYMANONLY:');
      recogniseToken(TokenType.KEYMANONLY, '$KeymanOnly:');
    });
    it("can recognise a KEYMANWEB token", () => {
      recogniseToken(TokenType.KEYMANWEB, '$keymanweb:');
      recogniseToken(TokenType.KEYMANWEB, '$KEYMANWEB:');
      recogniseToken(TokenType.KEYMANWEB, '$KeymanWeb:');
    });
    it("can recognise a KMFL token", () => {
      recogniseToken(TokenType.KMFL, '$kmfl:');
      recogniseToken(TokenType.KMFL, '$KMFL:');
      recogniseToken(TokenType.KMFL, '$Kmfl:');
    });
    it("can recognise a WEAVER token", () => {
      recogniseToken(TokenType.WEAVER, '$weaver:');
      recogniseToken(TokenType.WEAVER, '$WEAVER:');
      recogniseToken(TokenType.WEAVER, '$Weaver:');
    });
    it("can recognise a LEFT_BR token", () => {
      recogniseToken(TokenType.LEFT_BR, '(');
    });
    it("can recognise a RIGHT_BR token", () => {
      recogniseToken(TokenType.RIGHT_BR, ')');
    });
    it("can recognise a LEFT_SQ token", () => {
      recogniseToken(TokenType.LEFT_SQ, '[');
    });
    it("can recognise a RIGHT_SQ token", () => {
      recogniseToken(TokenType.RIGHT_SQ, ']');
    });
    it("can recognise a CHEVRON token", () => {
      recogniseToken(TokenType.CHEVRON, '>');
    });
    it("can recognise a PLUS token", () => {
      recogniseToken(TokenType.PLUS, '+');
    });
    it("can recognise a COMMA token", () => {
      recogniseToken(TokenType.COMMA, ',');
    });
    it("can recognise a NOT_EQUAL token", () => {
      recogniseToken(TokenType.NOT_EQUAL, '!=');
    });
    it("can recognise a EQUAL token", () => {
      recogniseToken(TokenType.EQUAL, '=');
    });
    it("can recognise a RANGE token", () => {
      recogniseToken(TokenType.RANGE, '..');
    });
    it("can recognise a U_CHAR token", () => {
      recogniseToken(TokenType.U_CHAR, 'U+1');
      recogniseToken(TokenType.U_CHAR, 'U+A');
      recogniseToken(TokenType.U_CHAR, 'U+1B');
      recogniseToken(TokenType.U_CHAR, 'U+1CD');
      recogniseToken(TokenType.U_CHAR, 'U+1EF0');
      recogniseToken(TokenType.U_CHAR, 'U+23456');
      recogniseToken(TokenType.U_CHAR, 'U+789ABC');
    });
    it("can recognise a STRING token (single quote)", () => {
      recogniseToken(TokenType.STRING, '\'10.0\'');
    });
    it("can recognise a STRING token (double quote)", () => {
      recogniseToken(TokenType.STRING, '"10.0"');
    });
    it("can recognise a STRING token (repeated quotes)", () => {
      recogniseTokens(
        '\'hello\' \'world\'',
        [
          new Token(TokenType.STRING,     '\'hello\''),
          new Token(TokenType.WHITESPACE, ' ', 1, 8),
          new Token(TokenType.STRING,     '\'world\'', 1, 9),
        ]
      );
    });
    it("does not pick out tokens from inside strings", () => {
      recogniseToken(TokenType.STRING, '"store"');
      recogniseToken(TokenType.STRING, '"\'"');
      recogniseToken(TokenType.STRING, "'\"'");
    });
    it("can recognise a DECIMAL token", () => {
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'd0');
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'd99');
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'd999');
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'D0');
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'D99');
      recogniseTokenFollowedByRightSquare(TokenType.DECIMAL, 'D999');
    });
    it("can recognise a HEXDECIMAL token", () => {
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'x0');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'x99');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'xa99');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'xA99');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'X0');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'X99');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'Xa99');
      recogniseTokenFollowedByRightSquare(TokenType.HEXADECIMAL, 'XA99');
    });
    it("can recognise an OCTAL token", () => {
      recogniseTokenFollowedByRightSquare(TokenType.OCTAL, '0');
      recogniseTokenFollowedByRightSquare(TokenType.OCTAL, '77');
      recogniseTokenFollowedByRightSquare(TokenType.OCTAL, '777');
    });
    it("can recognise a MODIFIER token", () => {
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'ctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'lctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'rctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'alt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'lalt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'ralt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'ncaps');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'CTRL');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'LCTRL');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'RCTRL');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'ALT');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'LALT');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'RALT');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'NCAPS');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Ctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Lctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Rctrl');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Alt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Lalt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Ralt');
      recogniseTokenFollowedBySpace(TokenType.MODIFIER, 'Ncaps');
    });
    it("can recognise a KEY_CODE token", () => {
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'K_K');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'K_COLON');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'T_17D2_1780');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'U_0030');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'U_0030_0031');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'U_10030');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'U_100030_1000');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'A21');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'k_k');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'k_colon');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 't_17D2_1780');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'u_0030');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'u_0030_0031');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'u_10030');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'u_100030_1000');
      recogniseTokenFollowedByRightSquare(TokenType.KEY_CODE, 'a21');
    });
    it("can recognise a KEY_CODE token (followed by space)", () => {
      recogniseTokens(
        'K_K ]',
        [
          new Token(TokenType.KEY_CODE, 'K_K'),
          new Token(TokenType.WHITESPACE, ' ', 1, 4),
          new Token(TokenType.RIGHT_SQ, ']', 1, 5),
        ]
      );
    });
    it("can recognise an ambiguous DECIMAL token (could be KEY_CODE)", () => {
      recogniseTokens(
        'd99 ]',
        [
          new Token(TokenType.DECIMAL, 'd99'),
          new Token(TokenType.WHITESPACE, ' ', 1, 4),
          new Token(TokenType.RIGHT_SQ, ']', 1, 5),
        ]
      );
    });
    it("can recognise a HANGUL token", () => {
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_A');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GA');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GGA');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GGAE');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GGAEG');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GGAEGG');
      recogniseToken(TokenType.HANGUL, '$HANGUL_SYLLABLE_GGYAEGG');
    });
    it("can recognise a COMMENT token", () => {
      recogniseToken(TokenType.COMMENT, 'c This tells Keyman which keys should have casing behavior applied');
    });
    it("can recognise a COMMENT token (emitAll:false)", () => {
      const comment: string = 'c This tells Keyman which keys should have casing behavior applied';
      recogniseTokens(
        'c This tells Keyman which keys should have casing behavior applied\n',
        [
          new Token(TokenType.NEWLINE, '\n', 1, 67, `${comment}\n`),
        ],
        {addEOF:false, emitAll:false, handleContinuation:true}
      );
    });
    it("can recognise a COMMENT token (followed by LF)", () => {
      const comment: string = 'c This tells Keyman which keys should have casing behavior applied';
      recogniseTokens(
        'c This tells Keyman which keys should have casing behavior applied\n',
        [
          new Token(TokenType.COMMENT, comment),
          new Token(TokenType.NEWLINE, '\n', 1, 67, `${comment}\n`),
        ]
      );
    });
    it("can recognise a COMMENT token (immediately followed by LF)", () => {
      const comment: string = 'c';
      recogniseTokens(
        'c\n',
        [
          new Token(TokenType.COMMENT, comment),
          new Token(TokenType.NEWLINE, '\n', 1, 2, `c\n`),
        ]
      );
    });
    it("can recognise a COMMENT token (immediately followed by LF)", () => {
      const comment: string = 'c';
      recogniseTokens(
        'c\n',
        [
          new Token(TokenType.COMMENT, comment),
          new Token(TokenType.NEWLINE, '\n', 1, 2, `c\n`),
        ]
      );
    });
    it("can recognise a WHITESPACE token (single space)", () => {
      recogniseToken(TokenType.WHITESPACE, ' ');
    });
    it("can recognise a WHITESPACE token (two spaces)", () => {
      recogniseToken(TokenType.WHITESPACE, '  ');
    });
    it("can recognise a WHITESPACE token (single tab)", () => {
      recogniseToken(TokenType.WHITESPACE, '\t');
    });
    it("can recognise a WHITESPACE token (two tabs)", () => {
      recogniseToken(TokenType.WHITESPACE, '\t\t');
    });
    it("can recognise a WHITESPACE token (mixed)", () => {
      recogniseToken(TokenType.WHITESPACE, '\t \t ');
    });
    it("can recognise a CONTINUATION token (no space after)", () => {
      recogniseTokens(
        '\\\n',
        [
          new Token(TokenType.CONTINUATION, '\\'),
          new Token(TokenType.NEWLINE, '\n', 1, 2, '\\\n'),
        ]
      );
    });
    it("can recognise a CONTINUATION token (space after)", () => {
      recogniseTokens(
        '\\ \n',
        [
          new Token(TokenType.CONTINUATION, '\\'),
          new Token(TokenType.WHITESPACE, ' ', 1, 2),
          new Token(TokenType.NEWLINE, '\n', 1, 3, '\\ \n'),
        ]
      );
    });
    it("recognise a continuation followed by non-whitespace as a PARAMETER", () => {
      recogniseTokens(
        '\\beep\n',
        [
          new Token(TokenType.PARAMETER, '\\beep'),
          new Token(TokenType.NEWLINE, '\n', 1, 6, '\\beep\n'),
        ]
      );
    });
    it("recognise a continuation followed by whitespace and a diallowed token as a PARAMETER", () => {
      recogniseTokens(
        '\\ beep\n',
        [
          new Token(TokenType.PARAMETER, '\\'),
          new Token(TokenType.WHITESPACE, ' ', 1, 2),
          new Token(TokenType.BEEP, 'beep', 1, 3),
          new Token(TokenType.NEWLINE, '\n', 1, 7, '\\ beep\n'),
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
          new Token(TokenType.STORE, 'store'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'LaoConsonants', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 20),
          new Token(TokenType.WHITESPACE, ' ', 1, 21),
          new Token(TokenType.U_CHAR, 'U+0E81', 1, 22),
          new Token(TokenType.WHITESPACE, ' ', 1, 28),
          new Token(TokenType.U_CHAR, 'U+0E82', 1, 29),
          new Token(TokenType.WHITESPACE, ' ', 1, 35),
          new Token(TokenType.U_CHAR, 'U+0E84', 1, 36),
          new Token(TokenType.WHITESPACE, ' ', 1, 42),
          new Token(TokenType.U_CHAR, 'U+0E87', 1, 43),
          new Token(TokenType.WHITESPACE, ' ', 1, 49),
          new Token(TokenType.U_CHAR, 'U+0E88', 1, 50),
          new Token(TokenType.WHITESPACE, ' ', 1, 56),
          new Token(TokenType.U_CHAR, 'U+0E8A', 1, 57),
          new Token(TokenType.WHITESPACE, ' ', 1, 63),
          new Token(TokenType.U_CHAR, 'U+0E8D', 1, 64),
          new Token(TokenType.WHITESPACE, ' ', 1, 70),
          new Token(TokenType.U_CHAR, 'U+0E94', 1, 71),
          new Token(TokenType.WHITESPACE, ' ', 1, 77),
          new Token(TokenType.CONTINUATION, '\\', 1, 78),
          new Token(TokenType.NEWLINE, '\n', 1, 79, line1),
          new Token(TokenType.WHITESPACE, '                     ', 2, 1),
          new Token(TokenType.U_CHAR, 'U+0E95', 2, 22),
          new Token(TokenType.WHITESPACE, ' ', 2, 28),
          new Token(TokenType.U_CHAR, 'U+0E96', 2, 29),
          new Token(TokenType.WHITESPACE, ' ', 2, 35),
          new Token(TokenType.U_CHAR, 'U+0E97', 2, 36),
          new Token(TokenType.WHITESPACE, ' ', 2, 42),
          new Token(TokenType.U_CHAR, 'U+0E99', 2, 43),
          new Token(TokenType.WHITESPACE, ' ', 2, 49),
          new Token(TokenType.U_CHAR, 'U+0E9A', 2, 50),
          new Token(TokenType.WHITESPACE, ' ', 2, 56),
          new Token(TokenType.U_CHAR, 'U+0E9B', 2, 57),
          new Token(TokenType.WHITESPACE, ' ', 2, 63),
          new Token(TokenType.U_CHAR, 'U+0E9C', 2, 64),
          new Token(TokenType.WHITESPACE, ' ', 2, 70),
          new Token(TokenType.U_CHAR, 'U+0E9D', 2, 71),
          new Token(TokenType.WHITESPACE, ' ', 2, 77),
          new Token(TokenType.CONTINUATION, '\\', 2, 78),
          new Token(TokenType.NEWLINE, '\n', 2, 79, line2),
          new Token(TokenType.WHITESPACE, '                     ', 3, 1),
          new Token(TokenType.U_CHAR, 'U+0E9E', 3, 22),
          new Token(TokenType.WHITESPACE, ' ', 3, 28),
          new Token(TokenType.U_CHAR, 'U+0E9F', 3, 29),
          new Token(TokenType.WHITESPACE, ' ', 3, 35),
          new Token(TokenType.U_CHAR, 'U+0EA1', 3, 36),
          new Token(TokenType.WHITESPACE, ' ', 3, 42),
          new Token(TokenType.U_CHAR, 'U+0EA2', 3, 43),
          new Token(TokenType.WHITESPACE, ' ', 3, 49),
          new Token(TokenType.U_CHAR, 'U+0EA3', 3, 50),
          new Token(TokenType.WHITESPACE, ' ', 3, 56),
          new Token(TokenType.U_CHAR, 'U+0EA5', 3, 57),
          new Token(TokenType.WHITESPACE, ' ', 3, 63),
          new Token(TokenType.U_CHAR, 'U+0EA7', 3, 64),
          new Token(TokenType.WHITESPACE, ' ', 3, 70),
          new Token(TokenType.U_CHAR, 'U+0EAA', 3, 71),
          new Token(TokenType.WHITESPACE, ' ', 3, 77),
          new Token(TokenType.CONTINUATION, '\\', 3, 78),
          new Token(TokenType.NEWLINE, '\n', 3, 79, line3),
          new Token(TokenType.WHITESPACE, '                     ', 4, 1),
          new Token(TokenType.U_CHAR, 'U+0EAB', 4, 22),
          new Token(TokenType.WHITESPACE, ' ', 4, 28),
          new Token(TokenType.U_CHAR, 'U+0EAD', 4, 29),
          new Token(TokenType.WHITESPACE, ' ', 4, 35),
          new Token(TokenType.U_CHAR, 'U+0EAE', 4, 36),
          new Token(TokenType.WHITESPACE, '    ', 4, 42),
          new Token(TokenType.COMMENT, 'c list of all the Lao consonants', 4, 46),
          new Token(TokenType.NEWLINE, '\n', 4, 78, line4),
        ]
      );
    });
    it("can recognise a NEWLINE token (LF)", () => {
      recogniseToken(TokenType.NEWLINE, '\n');
    });
    it("can recognise a NEWLINE token (CR)", () => {
      recogniseToken(TokenType.NEWLINE, '\r');
    });
    it("can recognise a NEWLINE token (CRLF)", () => {
      recogniseToken(TokenType.NEWLINE, '\r\n');
    });
    it("can recognise a NAMED_CONSTANT token", () => {
      recogniseToken(TokenType.NAMED_CONSTANT, '$abc');
    });
    it("can recognise a NAMED_CONSTANT token (followed by space)", () => {
      recogniseTokenFollowedBySpace(TokenType.NAMED_CONSTANT, '$abc');
    });
    it("can recognise a NAMED_CONSTANT token (khmer)", () => {
      recogniseToken(TokenType.NAMED_CONSTANT, '$សួស្តី');
    });
    it("can recognise a PARAMETER token", () => {
      recogniseTokens(
        'main)',
        [
          new Token(TokenType.PARAMETER, 'main'),
          new Token(TokenType.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets", () => {
      recogniseTokens(
        '(main)',
        [
          new Token(TokenType.LEFT_BR, '('),
          new Token(TokenType.PARAMETER, 'main', 1, 2),
          new Token(TokenType.RIGHT_BR, ')', 1, 6),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets (single space before)", () => {
      recogniseTokens(
        '( main)',
        [
          new Token(TokenType.LEFT_BR, '('),
          new Token(TokenType.WHITESPACE, ' ', 1, 2),
          new Token(TokenType.PARAMETER, 'main', 1, 3),
          new Token(TokenType.RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise a PARAMETER token in brackets (single space after)", () => {
      recogniseTokens(
        '(main )',
        [
          new Token(TokenType.LEFT_BR, '('),
          new Token(TokenType.PARAMETER, 'main', 1, 2),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.RIGHT_BR, ')', 1, 7),
        ]
      );
    });
    it("can recognise PARAMETER/OCTAL tokens in brackets (comma separated)", () => {
      recogniseTokens(
        '(main,2)',
        [
          new Token(TokenType.LEFT_BR, '('),
          new Token(TokenType.PARAMETER, 'main', 1, 2),
          new Token(TokenType.COMMA, ',', 1, 6),
          new Token(TokenType.OCTAL, '2', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 8),
        ]
      );
    });
    it("can recognise PARAMETER/OCTAL tokens in brackets (space separated)", () => {
      recogniseTokens(
        '(main 2)',
        [
          new Token(TokenType.LEFT_BR, '('),
          new Token(TokenType.PARAMETER, 'main', 1, 2),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.OCTAL, '2', 1, 7),
          new Token(TokenType.RIGHT_BR,   ')', 1, 8),
        ]
      );
    });
    it("can recognise a PARAMETER token (that starts with any, then a)", () => {
      recogniseTokens(
        'anya)',
        [
          new Token(TokenType.PARAMETER, 'anya'),
          new Token(TokenType.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (that starts with any, then A)", () => {
      recogniseTokens(
        'anyA)',
        [
          new Token(TokenType.PARAMETER, 'anyA'),
          new Token(TokenType.RIGHT_BR, ')', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (contains an equal sign)", () => {
      recogniseTokens(
        'foo = "1"',
        [
          new Token(TokenType.PARAMETER, 'foo'),
          new Token(TokenType.WHITESPACE, ' ', 1, 4),
          new Token(TokenType.EQUAL, '=', 1, 5),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.STRING, '"1"', 1, 7),
        ]
      );
    });
    it("can recognise a PARAMETER token (contains an equal sign without whitespace)", () => {
      recogniseTokens(
        'foo="2"',
        [
          new Token(TokenType.PARAMETER, 'foo'),
          new Token(TokenType.EQUAL, '=', 1, 4),
          new Token(TokenType.STRING, '"2"', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (contains a left-bracket)", () => {
      recogniseTokens(
        'foo("2"',
        [
          new Token(TokenType.PARAMETER, 'foo'),
          new Token(TokenType.LEFT_BR, '(', 1, 4),
          new Token(TokenType.STRING, '"2"', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (contains a left-square-bracket)", () => {
      recogniseTokens(
        'foo["2"',
        [
          new Token(TokenType.PARAMETER, 'foo'),
          new Token(TokenType.LEFT_SQ, '[', 1, 4),
          new Token(TokenType.STRING, '"2"', 1, 5),
        ]
      );
    });
    it("can recognise a PARAMETER token (contains a right-square-bracket)", () => {
      recogniseTokens(
        'foo]"2"',
        [
          new Token(TokenType.PARAMETER, 'foo'),
          new Token(TokenType.RIGHT_SQ, ']', 1, 4),
          new Token(TokenType.STRING, '"2"', 1, 5),
        ]
      );
    });
    it("can recognise a dummy token (empty brackets)", () => {
      const lexer    = new Lexer('dummy()');
      const actual   = lexer.parse({addEOF:false, emitAll:false});
      const expected = [new Token(TokenType.PARAMETER, 'dummy')];
      assert.deepEqual(actual, expected);
    });
    it("can recognise a dummy token (space before empty brackets)", () => {
      const lexer    = new Lexer('dummy ()');
      const actual   = lexer.parse({addEOF:false, emitAll:false});
      const expected = [new Token(TokenType.PARAMETER, 'dummy')];
      assert.deepEqual(actual, expected);
    });
    it("can recognise a dummy token (space after empty brackets)", () => {
      const lexer    = new Lexer('dummy() ');
      const actual   = lexer.parse({addEOF:false, emitAll:false});
      const expected = [new Token(TokenType.PARAMETER, 'dummy')];
      assert.deepEqual(actual, expected);
    });
    it("can recognise a dummy token (space in empty brackets)", () => {
      const lexer    = new Lexer('dummy( )');
      const actual   = lexer.parse({addEOF:false, emitAll:false});
      const expected = [new Token(TokenType.PARAMETER, 'dummy')];
      assert.deepEqual(actual, expected);
    });
    it("can recognise a bitmap store", () => {
      recogniseSystemStoreWithString(TokenType.BITMAP, 'khmer_angkor.ico');
    });
    it("can recognise a copyright store", () => {
      recogniseSystemStoreWithString(TokenType.COPYRIGHT, '© SIL Global');
    });
    it("can recognise a displaymap store", () => {
      recogniseSystemStoreWithString(TokenType.DISPLAYMAP, '../../../shared/fonts/kbd/kbdkhmr/KbdKhmr.json');
    });
    it("can recognise a keyboardversion store", () => {
      recogniseSystemStoreWithString(TokenType.KEYBOARDVERSION, '2.0');
    });
    it("can recognise a layoutfile store", () => {
      recogniseSystemStoreWithString(TokenType.LAYOUTFILE, 'khmer_angkor.keyman-touch-layout');
    });
    it("can recognise a name store", () => {
      recogniseSystemStoreWithString(TokenType.NAME, "Khmer Angkor");
    });
    it("can recognise a message store", () => {
      recogniseSystemStoreWithString(TokenType.MESSAGE, "More than just a Khmer Unicode keyboard.");
    });
    it("can recognise a targets store", () => {
      recogniseSystemStoreWithString(TokenType.TARGETS, 'any');
    });
    it("can recognise a version store", () => {
      recogniseSystemStoreWithString(TokenType.VERSION, 'khmer_angkor.kvks');
    });
    it("can recognise a visualkeyboard store", () => {
      recogniseSystemStoreWithString(TokenType.VISUALKEYBOARD, '10.0');
    });
    it("can recognise a capsalwaysoff store", () => {
      recogniseSystemStoreWithString(TokenType.CAPSALWAYSOFF, "1");
    });
    it("can recognise a 'caps always off' statement", () => {
      recogniseTokens(
        'caps always off',
        [
          new Token(TokenType.CAPS, 'caps'),
          new Token(TokenType.WHITESPACE, ' ', 1, 5),
          new Token(TokenType.ALWAYS, 'always', 1, 6),
          new Token(TokenType.WHITESPACE, ' ', 1, 12),
          new Token(TokenType.OFF, 'off', 1, 13),
        ]
      );
    });
    it("can recognise a capsononly store", () => {
      recogniseSystemStoreWithString(TokenType.CAPSONONLY, "1");
    });
    it("can recognise a 'caps on only' statement", () => {
      recogniseTokens(
        'caps on only',
        [
          new Token(TokenType.CAPS, 'caps'),
          new Token(TokenType.WHITESPACE, ' ', 1, 5),
          new Token(TokenType.ON, 'on', 1, 6),
          new Token(TokenType.WHITESPACE, ' ', 1, 8),
          new Token(TokenType.ONLY, 'only', 1, 9),
        ]
      );
    });
    it("can recognise a shiftfreescaps store", () => {
      recogniseSystemStoreWithString(TokenType.SHIFTFREESCAPS, "1");
    });
    it("can recognise a 'shift frees caps' statement", () => {
      recogniseTokens(
        'shift frees caps',
        [
          new Token(TokenType.SHIFT, 'shift'),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.FREES, 'frees', 1, 7),
          new Token(TokenType.WHITESPACE, ' ', 1, 12),
          new Token(TokenType.CAPS, 'caps', 1, 13),
        ]
      );
    });
    it("can recognise a begin statement (unicode)", () => {
      recogniseTokens(
        'begin Unicode > use(main)',
        [
          new Token(TokenType.BEGIN, 'begin'),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.UNICODE, 'Unicode', 1, 7),
          new Token(TokenType.WHITESPACE, ' ', 1, 14),
          new Token(TokenType.CHEVRON, '>', 1, 15),
          new Token(TokenType.WHITESPACE, ' ', 1, 16),
          new Token(TokenType.USE, 'use', 1, 17),
          new Token(TokenType.LEFT_BR, '(', 1, 20),
          new Token(TokenType.PARAMETER, 'main', 1, 21),
          new Token(TokenType.RIGHT_BR, ')', 1, 25),
        ]
      );
    });
    it("can recognise a begin statement (postkeystroke)", () => {
      recogniseTokens(
        'begin PostKeystroke > use(PostKeystroke)',
        [
          new Token(TokenType.BEGIN, 'begin'),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.POSTKEYSTROKE, 'PostKeystroke', 1, 7),
          new Token(TokenType.WHITESPACE, ' ', 1, 20),
          new Token(TokenType.CHEVRON, '>', 1, 21),
          new Token(TokenType.WHITESPACE, ' ', 1, 22),
          new Token(TokenType.USE, 'use', 1, 23),
          new Token(TokenType.LEFT_BR, '(', 1, 26),
          new Token(TokenType.POSTKEYSTROKE, 'PostKeystroke', 1, 27), // recognised as keyword
          new Token(TokenType.RIGHT_BR, ')', 1, 40),
        ]
      );
    });
    it("can recognise a group statement (readonly)", () => {
      recogniseTokens(
        'group(NewContext) readonly',
        [
          new Token(TokenType.GROUP, 'group'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.NEWCONTEXT, 'NewContext', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 17),
          new Token(TokenType.WHITESPACE, ' ', 1, 18),
          new Token(TokenType.READONLY, 'readonly', 1, 19),
        ]
      );
    });
    it("can recognise a group statement (using keys)", () => {
      recogniseTokens(
        'group(main) using keys',
        [
          new Token(TokenType.GROUP, 'group'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'main', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 11),
          new Token(TokenType.WHITESPACE, ' ', 1, 12),
          new Token(TokenType.USING, 'using', 1, 13),
          new Token(TokenType.WHITESPACE, ' ', 1, 18),
          new Token(TokenType.KEYS, 'keys', 1, 19),
        ]
      );
    });
    it("can recognise an if statement (using any, context, layer)", () => {
      recogniseTokens(
        'if(&newLayer = "") if(&layer = \'shift\') any(ShiftOutSingle) > context layer(\'default\')',
        [
          new Token(TokenType.IF, 'if'),
          new Token(TokenType.LEFT_BR, '(', 1, 3),
          new Token(TokenType.NEWLAYER, '&newLayer', 1, 4),
          new Token(TokenType.WHITESPACE, ' ', 1, 13),
          new Token(TokenType.EQUAL, '=', 1, 14),
          new Token(TokenType.WHITESPACE, ' ', 1, 15),
          new Token(TokenType.STRING, '""', 1, 16),
          new Token(TokenType.RIGHT_BR, ')', 1, 18),
          new Token(TokenType.WHITESPACE, ' ', 1, 19),
          new Token(TokenType.IF, 'if', 1, 20),
          new Token(TokenType.LEFT_BR, '(', 1, 22),
          new Token(TokenType.LAYER, '&layer', 1, 23),
          new Token(TokenType.WHITESPACE, ' ', 1, 29),
          new Token(TokenType.EQUAL, '=', 1, 30),
          new Token(TokenType.WHITESPACE, ' ', 1, 31),
          new Token(TokenType.STRING, '\'shift\'', 1, 32),
          new Token(TokenType.RIGHT_BR, ')', 1, 39),
          new Token(TokenType.WHITESPACE, ' ', 1, 40),
          new Token(TokenType.ANY, 'any', 1, 41),
          new Token(TokenType.LEFT_BR, '(', 1, 44),
          new Token(TokenType.PARAMETER, 'ShiftOutSingle', 1, 45),
          new Token(TokenType.RIGHT_BR, ')', 1, 59),
          new Token(TokenType.WHITESPACE, ' ', 1, 60),
          new Token(TokenType.CHEVRON, '>', 1, 61),
          new Token(TokenType.WHITESPACE, ' ', 1, 62),
          new Token(TokenType.CONTEXT, 'context', 1, 63),
          new Token(TokenType.WHITESPACE, ' ', 1, 70),
          new Token(TokenType.LAYER_SHORTCUT, 'layer', 1, 71),
          new Token(TokenType.LEFT_BR, '(', 1, 76),
          new Token(TokenType.STRING, '\'default\'', 1, 77),
          new Token(TokenType.RIGHT_BR, ')', 1, 86),
        ]
      );
    });
    it("can recognise a match statement", () => {
      recogniseTokens(
        'match > use(CombineDiacritics)',
        [
          new Token(TokenType.MATCH, 'match'),
          new Token(TokenType.WHITESPACE, ' ', 1, 6),
          new Token(TokenType.CHEVRON, '>', 1, 7),
          new Token(TokenType.WHITESPACE, ' ', 1, 8),
          new Token(TokenType.USE, 'use', 1, 9),
          new Token(TokenType.LEFT_BR, '(', 1, 12),
          new Token(TokenType.PARAMETER, 'CombineDiacritics', 1, 13),
          new Token(TokenType.RIGHT_BR, ')', 1, 30),
        ]
      );
    });
    it("can recognise a nomatch statement", () => {
      recogniseTokens(
        'nomatch > layer(\'default\')',
        [
          new Token(TokenType.NOMATCH, 'nomatch'),
          new Token(TokenType.WHITESPACE, ' ', 1, 8),
          new Token(TokenType.CHEVRON, '>', 1, 9),
          new Token(TokenType.WHITESPACE, ' ', 1, 10),
          new Token(TokenType.LAYER_SHORTCUT, 'layer', 1, 11),
          new Token(TokenType.LEFT_BR, '(', 1, 16),
          new Token(TokenType.STRING, '\'default\'', 1, 17),
          new Token(TokenType.RIGHT_BR, ')', 1, 26),
        ]
      );
    });
    it("can recognise a platform statement", () => {
      recogniseTokens(
        'platform(\'touch\') > use(detectStartOfSentence)',
        [
          new Token(TokenType.PLATFORM_SHORTCUT, 'platform'),
          new Token(TokenType.LEFT_BR, '(', 1, 9),
          new Token(TokenType.STRING, '\'touch\'', 1, 10),
          new Token(TokenType.RIGHT_BR, ')', 1, 17),
          new Token(TokenType.WHITESPACE, ' ',1, 18),
          new Token(TokenType.CHEVRON, '>', 1, 19),
          new Token(TokenType.WHITESPACE, ' ', 1, 20),
          new Token(TokenType.USE, 'use', 1, 21),
          new Token(TokenType.LEFT_BR, '(', 1, 24),
          new Token(TokenType.PARAMETER, 'detectStartOfSentence', 1, 25),
          new Token(TokenType.RIGHT_BR, ')', 1, 46),
        ]
      );
    });
    it("can recognise a store statement using outs", () => {
      recogniseTokens(
        'store(ShiftOutAll)  outs(ShiftOutSingle) outs(vCombo1) outs(vCombo2) outs(vCombo3)',
        [
          new Token(TokenType.STORE, 'store'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'ShiftOutAll', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 18),
          new Token(TokenType.WHITESPACE, '  ', 1, 19),
          new Token(TokenType.OUTS, 'outs', 1, 21),
          new Token(TokenType.LEFT_BR, '(', 1, 25),
          new Token(TokenType.PARAMETER, 'ShiftOutSingle', 1, 26),
          new Token(TokenType.RIGHT_BR, ')', 1, 40),
          new Token(TokenType.WHITESPACE, ' ', 1, 41),
          new Token(TokenType.OUTS, 'outs', 1, 42),
          new Token(TokenType.LEFT_BR, '(', 1, 46),
          new Token(TokenType.PARAMETER, 'vCombo1', 1, 47),
          new Token(TokenType.RIGHT_BR, ')', 1, 54),
          new Token(TokenType.WHITESPACE, ' ', 1, 55),
          new Token(TokenType.OUTS, 'outs', 1, 56),
          new Token(TokenType.LEFT_BR, '(', 1, 60),
          new Token(TokenType.PARAMETER, 'vCombo2', 1, 61),
          new Token(TokenType.RIGHT_BR, ')', 1, 68),
          new Token(TokenType.WHITESPACE, ' ', 1, 69),
          new Token(TokenType.OUTS, 'outs', 1, 70),
          new Token(TokenType.LEFT_BR, '(', 1, 74),
          new Token(TokenType.PARAMETER, 'vCombo3', 1, 75),
          new Token(TokenType.RIGHT_BR, ')', 1, 82),
        ]
      );
    });
    it("can recognise a store statement (khmer string)", () => {
      recogniseTokens(
        'store(ShiftOutSingle) \'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'',
        [
          new Token(TokenType.STORE, 'store'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'ShiftOutSingle', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 21),
          new Token(TokenType.WHITESPACE, ' ', 1, 22),
          new Token(TokenType.STRING, '\'ឈ៊ទ៌ៗ៍ភ័គ៏អៀឯឲធឿឌឬឫឍះឃៈជពុំណំឡ\'', 1, 23),
        ]
      );
    });
    it("can recognise a store statement (unicode chars)", () => {
      recogniseTokens(
        'store(whitespace) \' \' U+00A0 U+000D U+000A',
        [
          new Token(TokenType.STORE, 'store'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'whitespace', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 17),
          new Token(TokenType.WHITESPACE, ' ', 1, 18),
          new Token(TokenType.STRING, '\' \'', 1, 19),
          new Token(TokenType.WHITESPACE, ' ', 1, 22),
          new Token(TokenType.U_CHAR, 'U+00A0', 1, 23),
          new Token(TokenType.WHITESPACE, ' ', 1, 29),
          new Token(TokenType.U_CHAR, 'U+000D', 1, 30),
          new Token(TokenType.WHITESPACE, ' ', 1, 36),
          new Token(TokenType.U_CHAR, 'U+000A', 1, 37),
        ]
      );
    });
    it("can recognise compile targets (keyman, followed by space)", () => {
      recogniseTokenFollowedBySpace(TokenType.KEYMAN, '$keyman:');
    });
    it("can handle no newline at end of file", () => {
      const lexer    = new Lexer('beep');
      const actual   = lexer.parse({addEOF:true, emitAll:true, handleContinuation:false});
      const expected = [
        new Token(TokenType.BEEP, 'beep'),
        new Token(TokenType.NEWLINE, '', 1, 1, 'beep'),
        new Token(TokenType.EOF, '', 1, 1, ''),
      ];
      assert.deepEqual(actual, expected);
    });
    it("can handle a single line continuation (handleContinuation:false)", () => {
      const lexer    = new Lexer('beep\\\nbeep\n');
      const actual   = lexer.parse({addEOF:false, emitAll:true, handleContinuation:false});
      const expected = [
        new Token(TokenType.BEEP, 'beep', 1, 1),
        new Token(TokenType.CONTINUATION, '\\', 1, 5),
        new Token(TokenType.NEWLINE, '\n', 1, 6, 'beep\\\n'),
        new Token(TokenType.BEEP, 'beep', 2, 1),
        new Token(TokenType.NEWLINE, '\n', 2, 5, 'beep\n'),
      ];
      assert.deepEqual(actual, expected);
    });
    it("can handle a single line continuation (emitAll:false, handleContinuation:false)", () => {
      const lexer    = new Lexer('beep\\\nbeep\n');
      const actual   = lexer.parse({addEOF:false, emitAll:false, handleContinuation:false});
      const expected = [
        new Token(TokenType.BEEP, 'beep', 1, 1),
        new Token(TokenType.NEWLINE, '\n', 1, 6, 'beep\\\n'),
        new Token(TokenType.BEEP, 'beep', 2, 1),
        new Token(TokenType.NEWLINE, '\n', 2, 5, 'beep\n'),
      ];
      assert.deepEqual(actual, expected);
    });
    it("can handle a single line continuation (handleContinuation:true)", () => {
      const lexer    = new Lexer('beep\\\nbeep\n');
      const actual   = lexer.parse({addEOF:false, emitAll:false, handleContinuation:true});
      const expected = [
        new Token(TokenType.BEEP, 'beep', 1, 1),
        new Token(TokenType.BEEP, 'beep', 2, 1),
        new Token(TokenType.NEWLINE, '\n', 2, 5, 'beep\\\nbeep\n'),
      ];
      assert.deepEqual(actual, expected);
    });
    it("can handle multiple line continuations (handleContinuation:true)", () => {
      const line1 = 'store(LaoConsonants) U+0E81 U+0E82 U+0E84 U+0E87 U+0E88 U+0E8A U+0E8D U+0E94 \\\n';
      const line2 = '                     U+0E95 U+0E96 U+0E97 U+0E99 U+0E9A U+0E9B U+0E9C U+0E9D \\\n';
      const line3 = '                     U+0E9E U+0E9F U+0EA1 U+0EA2 U+0EA3 U+0EA5 U+0EA7 U+0EAA \\\n';
      const line4 = '                     U+0EAB U+0EAD U+0EAE    c list of all the Lao consonants\n';
      const lexer    = new Lexer(`${line1}${line2}${line3}${line4}`);
      const actual   = lexer.parse({addEOF:false, emitAll:false, handleContinuation:true});
      const expected = [
          new Token(TokenType.STORE, 'store'),
          new Token(TokenType.LEFT_BR, '(', 1, 6),
          new Token(TokenType.PARAMETER, 'LaoConsonants', 1, 7),
          new Token(TokenType.RIGHT_BR, ')', 1, 20),
          new Token(TokenType.U_CHAR, 'U+0E81', 1, 22),
          new Token(TokenType.U_CHAR, 'U+0E82', 1, 29),
          new Token(TokenType.U_CHAR, 'U+0E84', 1, 36),
          new Token(TokenType.U_CHAR, 'U+0E87', 1, 43),
          new Token(TokenType.U_CHAR, 'U+0E88', 1, 50),
          new Token(TokenType.U_CHAR, 'U+0E8A', 1, 57),
          new Token(TokenType.U_CHAR, 'U+0E8D', 1, 64),
          new Token(TokenType.U_CHAR, 'U+0E94', 1, 71),
          new Token(TokenType.U_CHAR, 'U+0E95', 2, 22),
          new Token(TokenType.U_CHAR, 'U+0E96', 2, 29),
          new Token(TokenType.U_CHAR, 'U+0E97', 2, 36),
          new Token(TokenType.U_CHAR, 'U+0E99', 2, 43),
          new Token(TokenType.U_CHAR, 'U+0E9A', 2, 50),
          new Token(TokenType.U_CHAR, 'U+0E9B', 2, 57),
          new Token(TokenType.U_CHAR, 'U+0E9C', 2, 64),
          new Token(TokenType.U_CHAR, 'U+0E9D', 2, 71),
          new Token(TokenType.U_CHAR, 'U+0E9E', 3, 22),
          new Token(TokenType.U_CHAR, 'U+0E9F', 3, 29),
          new Token(TokenType.U_CHAR, 'U+0EA1', 3, 36),
          new Token(TokenType.U_CHAR, 'U+0EA2', 3, 43),
          new Token(TokenType.U_CHAR, 'U+0EA3', 3, 50),
          new Token(TokenType.U_CHAR, 'U+0EA5', 3, 57),
          new Token(TokenType.U_CHAR, 'U+0EA7', 3, 64),
          new Token(TokenType.U_CHAR, 'U+0EAA', 3, 71),
          new Token(TokenType.U_CHAR, 'U+0EAB', 4, 22),
          new Token(TokenType.U_CHAR, 'U+0EAD', 4, 29),
          new Token(TokenType.U_CHAR, 'U+0EAE', 4, 36),
          new Token(TokenType.NEWLINE, '\n', 4, 78, `${line1}${line2}${line3}${line4}`),
        ]
      assert.deepEqual(actual, expected);
    });
    it("can handle an invalid system store or keyword token", () => {
      [
        '&baselayout',
        '&bitmap',
        '&casedkeys',
        '&copyright',
        '&displaymap',
        '&ethnologuecode',
        '&hotkey',
        '&includecodes',
        '&keyboardversion',
        '&kmw_embedcss',
        '&kmw_embedjs',
        '&kmw_helpfile',
        '&kmw_helptext',
        '&kmw_rtl',
        '&language',
        '&layer',
        '&layoutfile',
        '&message',
        '&mnemoniclayout',
        '&name',
        '&newlayer',
        '&oldcharposmatching',
        '&oldlayer',
        '&platform',
        '&targets',
        '&version',
        '&visualkeyboard',
        '&windowslanguages',
        '&capsalwaysoff',
        '&capsononly',
        '&shiftfreescaps',
        'caps',
        'always',
        'off',
        'on',
        'only',
        'shift',
        'frees',
        'beep',
        'begin',
        'context',
        'match',
        'nomatch',
        'nul',
        'return',
        'unicode',
        'newcontext',
        'postkeystroke',
        'ansi',
        'readonly',
        'using',
        'keys',
        'bitmap',
        'copyright',
        'hotkey',
        'language',
        'layout',
        'message',
        'name',
        'version',
        'ctrl',
        'lctrl',
        'rctrl',
        'alt',
        'lalt',
        'ralt',
        'ncaps',
      ].forEach((text) => { handleInvalidKeyword(text); });
    });
    it("can recognise a command with whitespace before the bracket", () => {
      [
        {type: TokenType.BASELAYOUT_SHORTCUT, text: 'baselayout'},
        {type: TokenType.LAYER_SHORTCUT,      text: 'layer'},
        {type: TokenType.PLATFORM_SHORTCUT,   text: 'platform'},
        {type: TokenType.ANY,                 text: 'any'},
        {type: TokenType.CALL,                text: 'call'},
        {type: TokenType.DEADKEY,             text: 'deadkey'},
        {type: TokenType.DEADKEY,             text: 'dk'},
        {type: TokenType.GROUP,               text: 'group'},
        {type: TokenType.IF,                  text: 'if'},
        {type: TokenType.INDEX,               text: 'index'},
        {type: TokenType.MATCH,               text: 'match'},
        {type: TokenType.NOMATCH,             text: 'nomatch'},
        {type: TokenType.NOTANY,              text: 'notany'},
        {type: TokenType.OUTS,                text: 'outs'},
        {type: TokenType.RESET,               text: 'reset'},
        {type: TokenType.SAVE,                text: 'save'},
        {type: TokenType.SET,                 text: 'set'},
        {type: TokenType.STORE,               text: 'store'},
        {type: TokenType.USE,                 text: 'use'},
      ].forEach((testCase) => {
        recogniseTokenFollowedBySpacedLeftBracket(testCase.type, testCase.text);
      });
    });
    it("can handle an invalid command tokens", () => {
      [
        'baselayout',
        'layer',
        'platform',
        'any',
        'call',
        'deadkey',
        'dk',
        'group',
        'if',
        'index',
        'notany',
        'outs',
        'reset',
        'save',
        'set',
        'store',
        'use',
      ].forEach((text) => { handleInvalidCommand(text); });
    });
    it("can handle a command with a missing bracket", () => {
      [
        'baselayout',
        'layer',
        'platform',
        'any',
        'call',
        'deadkey',
        'dk',
        'group',
        'if',
        'index',
        'notany',
        'outs',
        'reset',
        'save',
        'set',
        'store',
        'use',
      ].forEach((text) => { handleCommandWithMissingLeftBracket(text); });
    });
  });
  describe("Token", () => {
    describe("Token.constructor()", () => {
      it("can construct a Token", () => {
        const token = new Token(TokenType.STORE, 'store');
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 1);
        assert.isNull(token.line);
      });
      it("can construct a Token with line and char numbers", () => {
        const token = new Token(TokenType.STORE, 'store', 2, 3);
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 2);
        assert.equal(token.charNum, 3);
        assert.isNull(token.line);
      });
      it("can handle a negative line number", () => {
        const token = new Token(TokenType.STORE, 'store', -2, 3);
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 3);
        assert.isNull(token.line);
      });
      it("can handle a negative char number", () => {
        const token = new Token(TokenType.STORE, 'store', 2, -3);
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 2);
        assert.equal(token.charNum, 1);
        assert.isNull(token.line);
      });
      it("can handle a zero line number", () => {
        const token = new Token(TokenType.STORE, 'store', 0, 3);
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 3);
        assert.isNull(token.line);
      });
      it("can handle a zero char number", () => {
        const token = new Token(TokenType.STORE, 'store', 2, 0);
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 2);
        assert.equal(token.charNum, 1);
        assert.isNull(token.line);
      });
      it("can construct a Token with a line (NEWLINE)", () => {
        const token = new Token(TokenType.NEWLINE, '\n', 1, 19, 'store(one) "value"\n');
        assert.deepEqual(token.toString(), '[NEWLINE]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 19);
        assert.equal(token.line, 'store(one) "value"\n');
      });
      it("can construct a Token with a line (EOF)", () => {
        const token = new Token(TokenType.EOF, '', 1, 18, 'store(one) "value"');
        assert.deepEqual(token.toString(), '[EOF]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 18);
        assert.isNull(token.line);
      });
      it("can handle invalid line in constructor", () => {
        const token = new Token(TokenType.STORE, 'store', 1, 1, 'store(one) "value"\n');
        assert.deepEqual(token.toString(), '[STORE,store]');
        assert.equal(token.lineNum, 1);
        assert.equal(token.charNum, 1);
        assert.isNull(token.line);
      });
    });
    describe("Token.isTokenType()", () => {
      it("can report match", () => {
        const token = new Token(TokenType.STORE, 'store');
        assert.isTrue(token.isTokenType(TokenType.STORE));
      });
      it("can report non-match", () => {
        const token = new Token(TokenType.STORE, 'store');
        assert.isFalse(token.isTokenType(TokenType.BITMAP));
      });
    });
    describe("Token.tokenType", () => {
      it("can get tokenType", () => {
        const token = new Token(TokenType.STORE, 'store');
        assert.equal(token.tokenType, TokenType.STORE);
      });
    });
    describe("Token.text", () => {
      it("can get text", () => {
        const token = new Token(TokenType.STORE, 'store');
        assert.equal(token.text, 'store');
      });
    });
    describe("Token.lineNum", () => {
      it("can get line number", () => {
        const token = new Token(TokenType.STORE, 'store', 2, 3);
        assert.equal(token.lineNum, 2);
      });
    });
    describe("Token.charNum", () => {
      it("can get char number", () => {
        const token = new Token(TokenType.STORE, 'store', 2, 3);
        assert.equal(token.charNum, 3);
      });
    });
    describe("Token.line", () => {
      it("can get line", () => {
        const token = new Token(TokenType.NEWLINE, '\n', 1, 19, 'store(one) "value"\n');
        assert.equal(token.line, 'store(one) "value"\n');
      });
    });
    describe("Token.filename", () => {
      it("can get filename", () => {
        const token = new Token(TokenType.NEWLINE, '\n', 1, 19, 'store(one) "value"\n', 'dummy.kmn');
        assert.equal(token.filename, 'dummy.kmn');
      });
    });
  });
});

function recogniseToken(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  const lexer    = new Lexer(text);
  const actual   = lexer.parse({addEOF, emitAll, handleContinuation});
  const line     = (type === TokenType.NEWLINE) ? text : null;
  const expected = [new Token(type, text, 1, 1, line)];
  assert.deepEqual(actual, expected);
}

function recogniseTokens(text: string, expected: Token[], {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  const lexer    = new Lexer(text);
  const actual   = lexer.parse({addEOF, emitAll, handleContinuation});
  assert.deepEqual(actual, expected);
}

function recogniseTokenFollowedBySpace(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  recogniseTokens(
    `${text} `,
    [
      new Token(type, text),
      new Token(TokenType.WHITESPACE, ' ', 1, 1+text.length),
    ],
    {addEOF, emitAll, handleContinuation}
  );
}

function recogniseTokenFollowedByLeftBracket(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  recogniseTokens(
    `${text}(`,
    [
      new Token(type, text),
      new Token(TokenType.LEFT_BR, '(', 1, 1+text.length),
    ],
   {addEOF, emitAll, handleContinuation}
  );
}

function recogniseTokenFollowedBySpacedLeftBracket(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  recogniseTokens(
    `${text} (`,
    [
      new Token(type, text),
      new Token(TokenType.WHITESPACE, ' ', 1, 1+text.length),
      new Token(TokenType.LEFT_BR, '(', 1, 2+text.length),
    ],
   {addEOF, emitAll, handleContinuation}
  );
}

function recogniseTokenFollowedByRightSquare(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  recogniseTokens(
    `${text}]`,
    [
      new Token(type, text),
      new Token(TokenType.RIGHT_SQ, ']', 1, 1+text.length),
    ],
   {addEOF, emitAll, handleContinuation}
  );
}

function recogniseSystemStoreWithString(type: TokenType, text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}) {
  const value = TokenType[type].toLowerCase();
  recogniseTokens(
    `store(&${value}) '${text}'`,
    [
      new Token(TokenType.STORE, 'store'),
      new Token(TokenType.LEFT_BR, '(', 1, 6),
      new Token(type, `&${value}`, 1, 7),
      new Token(TokenType.RIGHT_BR, ')', 1, 8+value.length),
      new Token(TokenType.WHITESPACE, ' ', 1, 9+value.length),
      new Token(TokenType.STRING, `'${text}'`, 1, 10+value.length),
    ],
    {addEOF, emitAll, handleContinuation}
  );
}

function handleInvalidKeyword(text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}) {
  ['a', '_a', '.a', '-a', '1'].forEach((suffix) => {
    const textWithSuffix = `${text}${suffix}`;
    recogniseTokens(
      textWithSuffix,
      [new Token(TokenType.PARAMETER, textWithSuffix)],
      {addEOF, emitAll, handleContinuation}
    );
  });
}

function handleInvalidCommand(text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}) {
  ['a', '_a', '.a', '-a', '1'].forEach((suffix) => {
    const textWithSuffix = `${text}${suffix}`;
    recogniseTokens(
      `${textWithSuffix}(`,
      [
        new Token(TokenType.PARAMETER, textWithSuffix),
        new Token(TokenType.LEFT_BR, '(', 1, 1+textWithSuffix.length),
      ],
      {addEOF, emitAll, handleContinuation}
    );
  });
  ['a', '_a', '.a', '-a', '1'].forEach((suffix) => {
    const textWithSuffix = `${text}${suffix}`;
    recogniseTokens(
      `${textWithSuffix} (`,
      [
        new Token(TokenType.PARAMETER, textWithSuffix),
        new Token(TokenType.WHITESPACE, ' ', 1, 1+textWithSuffix.length),
        new Token(TokenType.LEFT_BR, '(', 1, 2+textWithSuffix.length),
      ],
      {addEOF, emitAll, handleContinuation}
    );
  });
}

function handleCommandWithMissingLeftBracket(text: string, {addEOF=false, emitAll=true, handleContinuation=false}={}): void {
  const line = `${text} \n`;
  recogniseTokens(
    line,
    [
      new Token(TokenType.PARAMETER, text),
      new Token(TokenType.WHITESPACE, ' ', 1, 1+text.length),
      new Token(TokenType.NEWLINE, '\n', 1, 2+text.length, line),
    ],
   {addEOF, emitAll, handleContinuation}
  );
}
