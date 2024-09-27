import { assert } from 'chai';
import 'mocha';
import { env } from 'node:process';
import { readFileSync, writeFileSync } from 'node:fs';


import { KeymanXMLOptions, KeymanXMLReader, KeymanXMLWriter } from '../src/xml-utils.js';
import { makePathToFixture } from './helpers/index.js';

// if true, attempt to WRITE the fixtures
const { GEN_XML_FIXTURES } = env;

class Case {
  options: KeymanXMLOptions;
  paths: string[];
};

const read_cases : Case[] = [
  {
    options: { type: 'keyboard3' },
    paths: [
      // keyboards
      'disp_maximal.xml',
      'k_020_fr.xml',
      'strs_invalid-illegal.xml',
      'tran_fail-empty.xml',
    ],
  }, {
    options: { type: 'keyboard3-test' },
    paths: [
      // keyboard test
      'k_020_fr-test.xml',
    ],
  }, {
    options: { type: 'kvks' },
    paths: [
      // kvks
      'khmer_angkor.kvks',
    ],
  }, {
    options: { type: 'kps' },
    paths: [
      // kps
      'test_valid.kps',
      // 'error_invalid_package_file.kps',
    ],
  }, {
    options: { type: 'kpj' },
    paths: [
      // kpj
      'khmer_angkor.kpj',
    ],
  },
];

/** read data, or null */
function readData(path: string) : string | null {
  try {
    return readFileSync(path, 'utf-8');
  } catch(e) {
    if (e?.code !== 'ENOENT') console.error(`reading ${path}`, e);
    return null;
  }
}

function readJson(path: string) : any | null {
  const data = readData(path);
  if(data === null) return null;
  return JSON.parse(data);
}

function writeJson(path: string, data: any) {
  writeFileSync(path, JSON.stringify(data, null, ' '));
}

describe(`XML Reader Test ${GEN_XML_FIXTURES && '(update mode!)' || ''}`, () => {
  for (const c of read_cases) {
    const {options, paths} = c;
    describe(`test reading ${JSON.stringify(options)}`, () => {
      const reader = new KeymanXMLReader(options);
      assert.ok(reader);
      for (const path of paths) {
        const xmlPath = makePathToFixture('xml', `${path}`);
        const jsonPath = makePathToFixture('xml', `${path}.json`);
        it(`read: ${xmlPath}`, () => {
          // get the string data
          const xml = readData(xmlPath);
          assert.ok(xml, `Could not read ${xmlPath}`);

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
            assert.deepEqual(actual, expect, `Mismatch of ${xmlPath} vs ${jsonPath}`);
          }
        });
      }
    });
  }
});

describe('XML Writer Test', () => {
  it('null test', () => assert.ok(new KeymanXMLWriter({type: 'kpj'})));
});

