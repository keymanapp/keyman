/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for XkbToKmnConverter, XkbFileReader, KmnFileWriter
 *
 */

import 'mocha';
//import { assert } from 'chai';
import { compilerTestCallbacks/*, compilerTestOptions , makePathToFixture */} from './../helpers/index.js';
//import { XkbFileReader } from '../../src/kmc-convert-read/xkb-file-reader.js';
//import { /*ActionStateOutput, */ XkbToKmnConverter } from '../../src/kmc-convert-convert/xkb-to-kmn-converter.js';


describe('XkbFileReader', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('Xkb-kmn: read', function () { });

  /* describe('Xkb-kmn: validate', function () {
     const part_xkb = 'xkb_symbols "deadtilde" {';
     const part_key = ' \nkey <AE01>  { [         1,     exclam,  onesuperior,   exclamdown ]\t}; ';
     const part_end = ' \n };\n';

     const sutR = new XkbFileReader(compilerTestCallbacks);
     [
       // return true
       ['string with complete correct definition', 'xkb_symbols "deadtilde" { \nkey <AE01>  { [         1,     exclam,  onesuperior,   exclamdown ]\t}; \nkey <AE02>  { [         2,   quotedbl,  twosuperior,    oneeighth ]\t}; \nkey <AE03>  { [         3,    section, threesuperior,    sterling ]\t}; \nkey <AE04>  { [         4,     dollar,   onequarter,     currency ]\t}; \nkey <AE05>  { [         5,    percent,      onehalf, threeeighths ]\t}; \nkey <AE06>  { [         6,  ampersand,      notsign,  fiveeighths ]\t}; \nkey <AE07>  { [         7,      slash,  …mark ] }; \nkey <AB06>  { [         n,          N, rightdoublequotemark, rightsinglequotemark ]\t}; \nkey <AB07>  { [         m,          M,           mu,    masculine ]\t}; \nkey <AB08>  { [     comma,  semicolon,       periodcentered,     multiply\t] }; \nkey <AB09>  { [    period,      colon,                U2026,     division \t] }; \nkey <AB10>  { [     minus, underscore,               endash,     emdash\t] }; \nkey <LSGT>  { [     less,     greater,                  bar, dead_belowmacron ] }; \n };\n', true],
       ['string with minimal layout', 'xkb_symbols "deadtilde" { \nkey <AE01>  { [         1,     exclam,  onesuperior,   exclamdown ]\t}; \n };\n', true],
       ['string with correct definition', part_xkb + part_key + part_end, true],
       ['string with short key definition', part_xkb + '\nkey <AE01>  { [ exclamdown ]};' + part_end, true],
       // return false
       ['string with missing text at the beginning', part_key + part_end, false],
       ['string with missing text for key definitions', part_xkb + part_end, false],
       ['string with missing text at the end', part_xkb + part_key, false],
       ['string with missing bracket in key definition', part_xkb + '\nkey <AE01>  { [         1,     exclam,  onesuperior,   exclamdown ]\t;', false],
       ['string with unknown text', 'some text', false],
       ['empty string', '', false],
       ['null', null, false,],
     ].forEach(function (files) {
       it(files[0] + " should return: " + files[2], async function () {
         const out = sutR.validate(files[1] as string, 'filename');

         assert.strictEqual(out, files[2]);
         if (out === false) {
           assert.isTrue(compilerTestCallbacks.messages.length === 1);
           assert.equal(compilerTestCallbacks.messages[0].code, 5292036);
         }
       });
     });
   });
  */

  /*describe('Xkb-kmn: validate_noWS', function () {
    const part_xkb = 'xkb_symbols"deadtilde"{';
    const part_key = 'key<AE01>{[1,exclam,onesuperior,exclamdown]};';
    const part_end = '};';
    const sutR = new XkbFileReader(compilerTestCallbacks);
    [ // return true
      ['string with minimal layout', 'xkb_symbols"deadtilde"{key<AE01>{[1,exclam,onesuperior,exclamdown]};};', true],
      ['string with complete correct definition', 'xkb_symbols"deadtilde"{key<AE01>{[1,exclam,onesuperior,exclamdown]};key<AE02>{[2,quotedbl,twosuperior,oneeighth]};\nkey<AE03>{[3,section,threesuperior,sterling]};\nkey<AE04>{[4,dollar,onequarter,currency]};\nkey<AE05>{[5,percent,onehalf,threeeighths]};\nkey<AE06>{[6,ampersand,notsign,fiveeighths]};\nkey<AE07>{[7,slash,…mark]};\nkey<AB06>{[n,N,rightdoublequotemark,rightsinglequotemark]};\nkey<AB07>{[m,M,mu,masculine]};\nkey<AB08>{[comma,semicolon,periodcentered,multiply]};\nkey<AB09>{[period,colon,U2026,division]};\nkey<AB10>{[minus,underscore,endash,emdash]};\nkey<LSGT>{[less,greater,bar,dead_belowmacron]};};', true],
      ['string with correct definition', part_xkb + part_key + part_end, true],
      ['string with short key definition', part_xkb + 'key<AE01>{[exclamdown]};' + part_end, true],
      // return false
      ['string with missing text at the beginning', part_key + part_end, false],
      ['string with missing text for key definitions', part_xkb + part_end, false],
      ['string with missing text at the end', part_xkb + part_key, false],
      ['string with missing bracket in key definition', part_xkb + 'key<AE01>{[1,exclam,onesuperior,exclamdown];', false],
      ['string with unknown text', 'some text', false],
      ['empty string', '', false],
      ['null', null, false,],
    ].forEach(function (files) {
      it(files[0] + " should return: " + files[2], async function () {
        const out = sutR.validate(files[1] as string, 'filename');
        assert.strictEqual(out, files[2]);
      });
    });
  });*/

  /* describe('Xkb-kmn: find_included_layoutVariant', function () {
     const sutR = new XkbFileReader(compilerTestCallbacks);
     [
       ['../data/tests-xkb-kmn/de-complex-rec(deadtilde)', '..\\data\\tests-xkb-kmn\\de(basic)'],
       ['../data/tests-xkb-kmn/de-complex-rec(deadacute)', '..\\data\\tests-xkb-kmn\\de(deadgraveacute)'],
       ['../data/tests-xkb-kmn/de-complex(deadtilde)', '..\\data\\tests-xkb-kmn\\de(basic)'],
       ['../data/tests-xkb-kmn/de-simple-rec', '..\\data\\tests-xkb-kmn\\latin(type4)'],
       ['../data/tests-xkb-kmn/de-complex-rec', '..\\data\\tests-xkb-kmn\\latin(type4)'],
       ['../data/tests-xkb-kmn/latin(type4)', '..\\data\\tests-xkb-kmn\\latin(basic)'],
 
       ['../data/tests-xkb-kmn/de-simple-rec', '..\\data\\tests-xkb-kmn\\latin(type4)'],
       ['../data/tests-xkb-kmn/de-complex-rec', '..\\data\\tests-xkb-kmn\\latin(type4)'],
 
       ['../data/tests-xkb-kmn/de-simple(basic)', '---'],
       ['../data/tests-xkb-kmn/de-simple', '---'],
       ['../data/tests-xkb-kmn/de-complex', '---'],
 
        TODO-kmc-convert add tests for null etc
       ['../data/tests-xkb-kmn/noname', '--'],
       ['', '--'],
       [null, '--'],
       
 
     ].forEach(function (files) {
       let inputFilename = files[0];
       const variant_name = files[0].substring(files[0].indexOf('(') + 1, files[0].indexOf(')'));
 
       if (variant_name)
         inputFilename = files[0].substring(0, files[0].indexOf('('));
 
       const binaryData = compilerTestCallbacks.loadFile(makePathToFixture(inputFilename));
       const xkb_data = new TextDecoder().decode(binaryData);
 
       it(files[0] + " should find name: " + files[1], async function () {
         const out = sutR.unitTestEndpoints.find_included_layoutVariant(xkb_data, files[0]);
         assert.strictEqual(out, files[1]);
       });
     });
   });*/

  /*
    describe("Xkb-kmn:: read() ", function () {
      const sutR = new XkbFileReader(compilerTestCallbacks);

      it('read() should return filled array on correct input', async function () {
        const inputFilename = makePathToFixture('../data/tests-xkb-kmn/de-simple');
        const result = sutR.read(compilerTestCallbacks.loadFile(inputFilename));
        assert.isNotEmpty(result);
      });

          it('read() should return empty array on empty input', async function () {
            const result = sutR.read(compilerTestCallbacks.loadFile(""));
            assert.isNull(result);
          });

          it('read() should return empty array on space as input', async function () {
            const result = sutR.read(compilerTestCallbacks.loadFile(" "));
            assert.isNull(result);
          });

          it('read() should return empty array on unavailable file name', async function () {
            const inputFilenameUnavailable = makePathToFixture('../data/tests-xkb-kmn/X.xkb');
            const result = sutR.read(compilerTestCallbacks.loadFile(inputFilenameUnavailable));
            assert.isNull(result);
          });

          it('read() should return empty array on typo in path', async function () {
            const result = sutR.read(compilerTestCallbacks.loadFile(makePathToFixture('../data|Test.xkb')));
            assert.isNull(result);
          });
    });*/

});
