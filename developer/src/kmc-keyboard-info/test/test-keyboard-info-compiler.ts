import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KeyboardInfoCompiler, KeyboardInfoCompilerResult, unitTestEndpoints } from '../src/keyboard-info-compiler.js';
import langtags from "../src/imports/langtags.js";
import { KmpCompiler, KmpCompilerOptions } from '@keymanapp/kmc-package';
import { CompilerCallbacks, KMX, KeymanFileTypes, KeymanTargets, KmpJsonFile } from '@keymanapp/common-types';
import { KeyboardInfoFile, KeyboardInfoFilePlatform } from './keyboard-info-file.js';
//import { PackageVersionValidator } from '../../kmc-package/src/compiler/package-version-validator.js';
//import { KeyboardMetadataCollection } from '../../kmc-package/src/compiler/package-metadata-collector.js';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

const KHMER_ANGKOR_KPJ = makePathToFixture('khmer_angkor', 'khmer_angkor.kpj');
const KHMER_ANGKOR_JS  = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
const KHMER_ANGKOR_KPS = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
const KHMER_ANGKOR_KMP = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');

const EN_LANGTAG = {
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
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;
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
    assert.deepEqual(langtags.find(({ tag }) => tag === 'en'), EN_LANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en'], EN_LANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-Latn-US'], EN_LANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-Latn'], EN_LANGTAG);
    assert.deepEqual((<any>unitTestEndpoints.langtagsByTag)['en-US'], EN_LANGTAG);
  });

  it('check init initialises KeyboardInfoCompiler correctly', async function() {
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

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
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

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
    KmpCompiler.prototype.init = async (_callbacks: CompilerCallbacks, _options: KmpCompilerOptions): Promise<boolean> => false;
    const result = await compiler.run(kpjFilename, null);
    KmpCompiler.prototype.init = origKmpCompilerInit;
    assert.isNull(result);
  });
  
  it('check run returns null if KmpCompiler.transformKpsToKmpObject fails', async function() {
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

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
    KmpCompiler.prototype.transformKpsToKmpObject = (_kpsFilename: string): KmpJsonFile.KmpJsonFile => null;
    const result = await compiler.run(kpjFilename, null);
    KmpCompiler.prototype.transformKpsToKmpObject = origKmpCompilerTransformKpsToKmpObject;
    assert.isNull(result);
  });
  
  it('check run returns null if loadJsFile fails', async function() {
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    compiler['loadJsFile'] = (_filename: string): string => null;
    const result = await compiler.run(kpjFilename, null);
    assert.isNull(result);
  }); 

  it('check run returns null if license is not MIT', async function() {
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    compiler['isLicenseMIT'] = (_filename: string): boolean => false;
    const result = await compiler.run(kpjFilename, null);
    assert.isNull(result);
  });

  it('check run returns null if fillLanguages fails', async function() {
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    compiler['fillLanguages'] = async (_kpsFilename: string, _keyboard_info: KeyboardInfoFile, _kmpJsonData:  KmpJsonFile.KmpJsonFile): Promise<boolean> => false;
    const result = await compiler.run(kpjFilename, null);
    assert.isNull(result);
  }); 

  it('should write artifacts to disk', async function() {
    const kpjFilename = KHMER_ANGKOR_KPJ;
    const jsFilename = KHMER_ANGKOR_JS;
    const kpsFilename = KHMER_ANGKOR_KPS;
    const kmpFilename = KHMER_ANGKOR_KMP;
    const actualFilename = makePathToFixture('khmer_angkor', 'build', 'actual.keyboard_info');
    const expectedFilename = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.keyboard_info');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/khmer_angkor',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kpjFilename, null);
    assert.isNotNull(result);

    if(fs.existsSync(actualFilename)) {
      fs.rmSync(actualFilename);
    }

    result.artifacts.keyboard_info.filename = actualFilename;
    assert.isTrue(await compiler.write(result.artifacts));
    assert(fs.existsSync(actualFilename))

    const actual = JSON.parse(fs.readFileSync(actualFilename, 'utf-8'));
    const expected = JSON.parse(fs.readFileSync(expectedFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });

  it('check mapKeymanTargetToPlatform returns correct platforms', async function() {
    const compiler = new KeyboardInfoCompiler();
    const map: {[index in KeymanTargets.KeymanTarget]: KeyboardInfoFilePlatform[]} = {
      any: [], 
      androidphone: ['android'],
      androidtablet: ['android'],
      desktop: [], 
      ipad: ['ios'],
      iphone: ['ios'],
      linux: ['linux'],
      macosx: ['macos'],
      mobile: [], 
      tablet: [], 
      web: ['desktopWeb'],  
      windows: ['windows']
    }
    for (const [target, platform] of Object.entries(map)) {
      assert.deepEqual(compiler['mapKeymanTargetToPlatform'](<KeymanTargets.KeymanTarget>target), platform);
    }
  }); 

  it('check kmxFileVersionToString returns correct strings', async function() {
    const compiler = new KeyboardInfoCompiler();
    const convs = [
      {num: 0x0000, str: '0.0'},
      {num: 0x0001, str: '0.1'},
      {num: 0x0100, str: '1.0'},
      {num: 0x0101, str: '1.1'},
      {num: 0x0A0A, str: '10.10'},
    ];
    convs.forEach((conv) => {
      assert.equal(compiler['kmxFileVersionToString'](conv.num), conv.str);
    });
  });
  
  it('check loadKmxFiles returns empty array if .kmx file is missing from .kmp', async function() {
    const kpsFilename = KHMER_ANGKOR_KPS;
    const compiler = new KeyboardInfoCompiler();
    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, {}));
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename);
    assert.isNotNull(kmpJsonData);
    // remove .kps file
    kmpJsonData.files = kmpJsonData.files.filter(file => !KeymanFileTypes.filenameIs(file.name, KeymanFileTypes.Binary.Keyboard));
    const kmxFiles: {
      filename: string,
      data: KMX.KEYBOARD
    }[] = compiler['loadKmxFiles'](kpsFilename, kmpJsonData);
    assert.deepEqual(kmxFiles, []);
  });

  it('check loadKmxFiles throws error if .kmx file is missing from disk', async function() {
    const kpsFilename = KHMER_ANGKOR_KPS;
    const compiler = new KeyboardInfoCompiler();
    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, {}));
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename);
    assert.isNotNull(kmpJsonData);
    // rename .kmx file in files list so it cannot be loaded from disk
    const kmpIndex = kmpJsonData.files.findIndex(file => KeymanFileTypes.filenameIs(file.name, KeymanFileTypes.Binary.Keyboard));
    kmpJsonData.files[kmpIndex].name = '../build/throw_error.kmx';
    assert.throws(() => compiler['loadKmxFiles'](kpsFilename, kmpJsonData));
  });  

  it('check loadKmxFiles can handle two .kmx files', async function() {
    const jsFilename = makePathToFixture('two-kmx', 'build', 'two_kmx.js');
    const kpsFilename = makePathToFixture('two-kmx', 'source', 'two_kmx.kps');
    const kmpFilename = makePathToFixture('two-kmx', 'build', 'two_kmx.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/two-kmx',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const kmx_filename_001 = 'k_001___basic_input_unicodei.kmx';
    const kmx_filename_002 = 'k_002___basic_input_unicode.kmx';
    
    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const kmpJsonData: KmpJsonFile.KmpJsonFile = {
      system: { fileVersion: '', keymanDeveloperVersion: '' },
      options: null,
      files: [
        { name: '../build/' + kmx_filename_001, description: 'Keyboard 001' },
        { name: '../build/' + kmx_filename_002, description: 'Keyboard 002' },
      ]
    };
    const kmxFiles: {
      filename: string,
      data: KMX.KEYBOARD
    }[] = compiler['loadKmxFiles'](kpsFilename, kmpJsonData);
    assert.equal(kmxFiles.length, 2);
    assert.deepEqual(kmxFiles[0].filename, kmx_filename_001);
    assert.deepEqual(kmxFiles[1].filename, kmx_filename_002);
    assert.isNotNull(kmxFiles[0].data);
    assert.isNotNull(kmxFiles[1].data);
  });    

  it('check loadJsFile throws error if .js file is invalid', async function() {
    const jsFilename = makePathToFixture('invalid-js-file', 'build', 'invalid_js_file.js');
    const kpsFilename = makePathToFixture('invalid-js-file', 'source', 'invalid_js_file.kps');
    const kmpFilename = makePathToFixture('invalid-js-file', 'build', 'invalid_js_file.kmp');

    const sources = {
      kmpFilename,
      sourcePath: 'release/k/invalid-js-file',
      kpsFilename,
      jsFilename: jsFilename,
      forPublishing: true,
    };

    const compiler = new KeyboardInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const origTextDecoderDecode = TextDecoder.prototype.decode;
    TextDecoder.prototype.decode = () => { throw new TypeError(); }
    assert.throws(() => compiler['loadJsFile'](jsFilename));
    TextDecoder.prototype.decode = origTextDecoderDecode;
  });
});
