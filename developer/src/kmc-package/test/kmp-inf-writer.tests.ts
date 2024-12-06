import 'mocha';
import * as fs from 'fs';
import { assert } from 'chai';

import { KmpJsonFile } from '@keymanapp/common-types';

import { makePathToFixture } from './helpers/index.js';

import { KmpInfWriter } from '../src/compiler/kmp-inf-writer.js';
import { transcodeToCP1252 } from '../src/compiler/cp1252.js';

describe('KmpInfWriter', function () {
  it(`should transform kmp.json to kmp.inf`, function () {
    const data: KmpJsonFile.KmpJsonFile = JSON.parse(fs.readFileSync(makePathToFixture('kmp.inf', 'kmp.json'), 'utf-8'));
    const writer = new KmpInfWriter(data);
    const fixtureKmpInf = fs.readFileSync(makePathToFixture('kmp.inf', 'kmp.inf'));
    const value = writer.write();
    const test = transcodeToCP1252(value);

    const fixtureANSI = new TextDecoder('cp1252').decode(fixtureKmpInf);
    const testANSI = new TextDecoder('cp1252').decode(test);

    let testItems = parseInf(testANSI);
    let fixtureItems = parseInf(fixtureANSI);
    assert.deepEqual(testItems, fixtureItems);
  });
});

type Section = {[name:string]: string};
type InfFile = {[name:string]: Section};

/**
 * Parses a .ini-style file into name=value pairs grouped by [section] headers.
 * Note that this is not a "fully compliant" .ini parser, as .ini allows
 * repeated names, and this ignores that, but we need this only for kmp.inf,
 * which never repeats
 */
function parseInf(inf: string): InfFile {
  let items = inf.replaceAll(/\r\n/g, '\n').split('\n');
  let sections: InfFile = {}, newSection: Section = {};
  sections[0] = newSection;
  for(let item of items) {
    item = item.trim();
    let v = item.match(/^\[(.+)\]$/);
    if(v) {
      //[section]
      newSection = {};
      sections[v[1]] = newSection;
    } else {
      v = item.match(/^(.+?)=(.*)$/);
      if(v) {
        //name=value
        newSection[v[1]]=v[2];
      } else {
        //ignore, it isn't the inf format we want, maybe blank
      }
    }
  }
  return sections;
}

