import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler, KeyboardInfoCompilerResult, unitTestEndpoints } from '../src/keyboard-info-compiler.js';
import langtags from "../src/imports/langtags.js";
import { KmpCompiler, KmpCompilerOptions } from '@keymanapp/kmc-package';
import { CompilerCallbacks } from '@keymanapp/common-types';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

const ENLANGTAG = {
  "full": "en-Latn-US",
  "iana": [ "English" ],
  "iso639_3": "eng",
  "localname": "American English",
  "localnames": [ "English" ],
 "name": "English",
  "names": [ "Anglais", "Angleščina", "Anglisy", "Angličtina", "Anglų", "Angol", "Angļu", "Engels", "Engelsk", "Engelska", "Engelski", "Englaisa", "Englanti", "Englesch", "Engleză", "Englisch", "Ingilizce", "Inglese", "Ingliż", "Inglés", "Inglês", "Język angielski", "Kiingereza", "anglais" ],
  "region": "US",
  "regionname": "United States",
  "regions": [ "AD", "AF", "AR", "AS", "AW", "BD", "BG", "BH", "BL", "BN", "BQ", "BT", "BY", "CL", "CN", "CO", "CR", "CW", "CY", "CZ", "DO", "EC", "EE", "ES", "ET", "FM", "FR", "GQ", "GR", "GW", "HN", "HR", "HU", "ID", "IS", "IT", "JP", "KH", "KR", "KW", "LB", "LK", "LT", "LU", "LV", "LY", "MC", "ME", "MF", "MX", "NO", "NP", "OM", "PA", "PL", "PM", "PR", "PT", "RO", "RS", "RU", "SA", "SK", "SO", "SR", "ST", "SV", "TC", "TH", "TN", "TW", "UA", "UM", "UY", "VE", "VG", "VI" ],
  "script": "Latn",
  "sldr": true,
  "suppress": true,
  "tag": "en",
  "tags": [ "en-Latn", "en-US" ],
  "variants": [ "basiceng", "boont", "cornu", "emodeng", "oxendict", "scotland", "scouse", "spanglis", "unifon" ],
  "windows": "en-US"
};

describe('keyboard-info-compiler', function () {
  it('compile a .keyboard_info file correctly', async function() {
    const kpjFilename = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const buildKeyboardInfoFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    let result: KeyboardInfoCompilerResult = null;
    try {
      result = await compiler.run(kpjFilename, null);
    } catch(e) {
      callbacks.printMessages();
      throw e;
    }
    if(result == null) {
      callbacks.printMessages();
    }
    assert.isNotNull(result);

    const actual = JSON.parse(new TextDecoder().decode(result.artifacts.keyboard_info.data));
    const expected = JSON.parse(fs.readFileSync(buildKeyboardInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });

  it('check preinit creates langtagsByTag correctly', async function() {
    const compiler = new KeyboardInfoCompiler(); // indirectly call preinit()
    assert.isNotNull(compiler);
    assert.deepEqual(langtags.find(({ tag }) => tag === 'en'), ENLANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en'], ENLANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-Latn-US'], ENLANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-Latn'], ENLANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-US'], ENLANGTAG);
  });

  it('check init initialises KeyboardInfoCompiler correctly', async function() {
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    assert.deepEqual(compiler['callbacks'], callbacks);
    assert.deepEqual(compiler['options'], {sources});
  });  

  it('check run returns null if KmpCompiler.init fails', async function() {
    const kpjFilename = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const origKmpCompilerInit = KmpCompiler.prototype.init;
    KmpCompiler.prototype.init = (callbacks: CompilerCallbacks, options: KmpCompilerOptions) => { return null; }
    const result = await compiler.run(kpjFilename, null);
    KmpCompiler.prototype.init = origKmpCompilerInit;
    assert.isNull(result);
  });
  
  it('check run returns null if KmpCompiler.transformKpsToKmpObject fails', async function() {
    const kpjFilename = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const origKmpCompilerTransformKpsToKmpObject = KmpCompiler.prototype.transformKpsToKmpObject;
    KmpCompiler.prototype.transformKpsToKmpObject = (kpsFilename: string) => { return null; }
    const result = await compiler.run(kpjFilename, null);
    KmpCompiler.prototype.transformKpsToKmpObject = origKmpCompilerTransformKpsToKmpObject;
    assert.isNull(result);
  });
  
  it('check run returns null if loadJsFile fails', async function() {
    const kpjFilename = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
    const jsFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const kpsFilename = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    compiler['loadJsFile'] = (filename: string): string => { return null; }
    const result = await compiler.run(kpjFilename, null);
    assert.isNull(result);
  }); 
});
