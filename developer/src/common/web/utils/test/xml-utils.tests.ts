/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2024-09-27
 *
 * Test for abstraction for XML reading and writing
 */

import { assert } from 'chai';
import 'mocha';
import { env } from 'node:process';
import { readFileSync, writeFileSync } from 'node:fs';


import { KeymanXMLType, KeymanXMLReader, KeymanXMLWriter, findInstanceObject } from '../src/xml-utils.js';
import { LineFinder } from '../src/line-utils.js';
import { makePathToFixture } from './helpers/index.js';
import { SymbolUtils } from '../src/symbol-utils.js';

// if true, attempt to WRITE the fixtures
const { GEN_XML_FIXTURES } = env;

class Case {
  type: KeymanXMLType;
  paths: string[];
};

const read_cases: Case[] = [
  {
    type: 'keyboard3',
    paths: [
      // keyboards
      'disp_maximal.xml',
      'k_020_fr.xml',
      'strs_invalid-illegal.xml',
      'tran_fail-empty.xml',
      'tran_fail-matches-nothing-1.xml',
    ],
  }, {
    type: 'keyboardTest3',
    paths: [
      // keyboard test
      'k_020_fr-test.xml',
    ],
  }, {
    type: 'kvks',
    paths: [
      // kvks
      'khmer_angkor.kvks',
    ],
  }, {
    type: 'kps',
    paths: [
      // kps
      'test_valid.kps',
      // 'error_invalid_package_file.kps',
    ],
  }, {
    type: 'kpj',
    paths: [
      // kpj
      'khmer_angkor.kpj',
    ],
  },
];

const write_cases: Case[] = [
  {
    type: 'kvks',
    paths: [
      // kvks
      'khmer_angkor2.kvks', // similar to the 'read case' with the similar name, except for whitespace differences and the prologue
    ],
  },
];

/** read data, or null */
function readData(path: string): string | null {
  try {
    return readFileSync(path, 'utf-8');
  } catch (e) {
    if (e?.code !== 'ENOENT') console.error(`reading ${path}`, e);
    return null;
  }
}

function readJson(path: string): any | null {
  const data = readData(path);
  if (data === null) return null;
  return JSON.parse(data);
}

function writeJson(path: string, data: any) {
  writeFileSync(path, JSON.stringify(data, null, ' '));
}

describe(`XML Reader Test ${GEN_XML_FIXTURES && '(update mode!)' || ''}`, () => {
  for (const c of read_cases) {
    const { type, paths } = c;
    describe(`test reading ${type}`, () => {
      for (const path of paths) {
        const xmlPath = makePathToFixture('xml', `${path}`);
        const jsonPath = makePathToFixture('xml', `${path}.json`);
        it(`read: xml/${path}`, () => {
          // get the string data
          const xml = readData(xmlPath);
          assert.ok(xml, `Could not read ${xmlPath}`);

          const reader = new KeymanXMLReader(type);
          assert.ok(reader);

          // now, parse. subsitute endings for Win
          const actual = reader.parse(xml.replace(/\r\n/g, '\n'));
          assert.ok(actual, `Parser failed on ${xmlPath}`);

          // get the expected
          const expect = readJson(jsonPath);

          if (GEN_XML_FIXTURES) {
            console.log(`GEN_XML_FIXTURES: writing ${jsonPath} from actual`);
            writeJson(jsonPath, actual);
          } else {
            assert.ok(expect, `Could not read ${jsonPath} - run with env GEN_XML_FIXTURES=1 to update.`);
            assert.deepEqual(SymbolUtils.removeSymbols(actual), expect, `Mismatch of ${xmlPath} vs ${jsonPath}`);
          }
        });
      }
    });
  }
});


describe(`XML Writer Test ${GEN_XML_FIXTURES && '(update mode!)' || ''}`, () => {
  for (const c of write_cases) {
    const { type, paths } = c;
    describe(`test writing ${type}`, () => {
      const writer = new KeymanXMLWriter(type);
      assert.ok(writer);
      for (const path of paths) {
        const jsonPath = makePathToFixture('xml', `${path}.json`);
        const xmlPath = makePathToFixture('xml', `${path}`);
        it(`write: xml/${path}`, () => {
          // get the object data
          const data = readJson(jsonPath);
          assert.ok(data, `Could not read input ${jsonPath}`);

          // now, write.
          const actual = writer.write(data);
          assert.ok(actual, `Writer failed on ${jsonPath}`);

          if (GEN_XML_FIXTURES) {
            console.log(`GEN_XML_FIXTURES: writing ${xmlPath} from actual`);
            writeFileSync(xmlPath, actual);
          } else {
            // get the expected data
            const expect = readData(xmlPath).replace(/\r\n/g, '\n');
            assert.ok(expect, `Could not read expected output ${xmlPath} - run with env=GEN_XML_FIXTURES=1 to update`);
            assert.deepEqual(actual.trim(), expect.trim(), `Mismatch of ${xmlPath} vs ${jsonPath}`);
          }
        });
      }
    });
  }
});

describe(`XML Reader line number test`, () => {
  const path = 'tran_fail-empty.xml';
  const xmlPath = makePathToFixture('xml', `${path}`);
  const type: KeymanXMLType = 'keyboard3';
  it(`Should report line numbers on parse of ${type} ${path}`, () => {
    let xml = readData(xmlPath);
    assert.ok(xml, `Could not read ${xmlPath}`);
    xml = xml.replace(/\r\n/g, '\n');
    const reader = new KeymanXMLReader(type);
    assert.ok(reader);

    // now, parse. subsitute endings for Win
    const actual = reader.parse(xml);
    const lines = LineFinder.textToLines(xml);
    assert.ok(actual, `Parser failed on ${xmlPath}`);

    // now, assert char offset
    const getMetaData = KeymanXMLReader.getMetaData;
    assert.ok(getMetaData(actual.keyboard3));
    assert.equal(
      getMetaData(actual.keyboard3)?.startIndex, 40); // index of <keyboard3> element
    assert.equal(
      getMetaData(actual.keyboard3.info)?.startIndex, 136);  // index of <info> etc
    assert.equal(
      getMetaData(actual.keyboard3.transforms)?.startIndex, 186);
    assert.deepEqual(
      LineFinder.offsetToLineColumn(
        getMetaData(actual.keyboard3).startIndex, lines), { line: 3, column: 0 });
    assert.deepEqual(
      LineFinder.offsetToLineColumn(
        getMetaData(actual.keyboard3.info).startIndex, lines), { line: 4, column: 2 });
    assert.deepEqual(
      LineFinder.offsetToLineColumn(
        getMetaData(actual.keyboard3.transforms).startIndex, lines), { line: 8, column: 2 });
  });
  describe('findInstanceObject test', () => {
    const path0 = '/keyboard3/layers/0';
    const TARGET = Symbol("Looking for this!");
    it(`Should be able to parse ${path0}`, () => {
      const o = {
        keyboard3: {
          layers: [
            TARGET,
          ]
        }
      };
      assert.equal(findInstanceObject(o, path0.split('/')), TARGET);
    });
    // path to property
    const path1 = '/keyboard3/conformsTo';
    it(`Should be able to parse ${path1}`, () => {
      const keyboard3 = { conformsTo: "1234"};
      const o = {
        keyboard3,
      };
      assert.equal(findInstanceObject(o, path1.split('/')), keyboard3);
    });
    const path2 = '/keyboard3/bad/path';
    it(`Should be able to handle ${path2}`, () => {
      const keyboard3 = { conformsTo: "1234"};
      const o = {
        keyboard3,
      };
      assert.equal(findInstanceObject(o, path2.split('/')), undefined);
    });
  });
});

