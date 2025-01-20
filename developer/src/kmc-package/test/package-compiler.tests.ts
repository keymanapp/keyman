import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { assert } from 'chai';
import JSZip from 'jszip';

import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { KmpJsonFile } from '@keymanapp/common-types';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { makePathToFixture } from './helpers/index.js';

import { KmpCompiler } from '../src/compiler/kmp-compiler.js';
import { PackageCompilerMessages } from '../src/compiler/package-compiler-messages.js';
import { PackageValidation } from '../src/compiler/package-validation.js';
import { unitTestEndpoints as getFileDataEndpoints, unitTestEndpoints } from '../src/compiler/get-file-data.js';
import { env } from 'process';

const { TEST_SAVE_FIXTURES } = env;

describe('KmpCompiler', function () {
  const MODELS : string[] = [
    'example.qaa.sencoten',
    'withfolders.qaa.sencoten',
  ];
  const callbacks = new TestCompilerCallbacks(makePathToFixture('online'));
  let kmpCompiler: KmpCompiler = null;

  this.beforeAll(async function() {
    kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, null));
  });

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  for (let modelID of MODELS) {
    const kpsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.kps`) : makePathToFixture(modelID, `${modelID}.model.kps`);
    const jsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.js`) : makePathToFixture(modelID, `${modelID}.model.js`);
    const kmpJsonIntermediatePath = makePathToFixture(modelID, `${modelID}.model.kmp.intermediate.json`);
    const kmpJsonZippedPath = makePathToFixture(modelID, `${modelID}.model.kmp.zipped.json`);
    const kmpJsonIntermediateFixture = JSON.parse(fs.readFileSync(kmpJsonIntermediatePath, 'utf-8'));
    const kmpJsonZippedFixture = JSON.parse(fs.readFileSync(kmpJsonZippedPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonIntermediateFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    //
    // Test just the transform from kps to kmp.json
    //
    it(`should transform ${modelID}.model.kps to kmp.json`, async function () {
      const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJsonData, kmpJsonIntermediateFixture);

      // Note that in-memory kmp.json still contains paths in the files array.
      // However, when building the .kmp, the final written kmp.json data is
      // modified to strip paths.

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });

    it(`should build a full .kmp for ${modelID}`, async function() {
      const zip = JSZip();
      // Build kmp.json in memory
      const { kmpJsonData, fileData } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
      // Build file.kmp in memory
      const promise = kmpCompiler.buildKmpFile(kmpJsonData, fileData);
      promise.then(data => {
        // Check that file.kmp contains just 2 files - kmp.json and file.model.js,
        // and that they match exactly what we expect
        return zip.loadAsync(data, {checkCRC32: true}).then(zipFile => {
          assert.equal(zipFile.length, 2);
          return Promise.all([
            zipFile.file("kmp.json").async('uint8array').then(kmpJsonOutput => {
              assert.deepEqual(kmpJsonOutput, kmpJsonZippedFixture);
            }),
            zipFile.file(`${modelID}.model.js`).async('uint8array').then(modelJsFile => {
              assert.deepEqual(modelJsFile, fs.readFileSync(jsPath));
            })
          ]);
        });
      });

      return promise;
    });
  }

  it('should generate a valid .kmp (zip) file', async function() {
    this.timeout(10000); // building a zip file can sometimes be slow

    const kpsPath = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpJsonRefPath = makePathToFixture('khmer_angkor', 'ref', 'kmp.json');

    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, null));

    const kmpJsonFixture: KmpJsonFile.KmpJsonFile = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    const { kmpJsonData, fileData } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
    assert.isNotNull(kmpJsonData);
    assert.isNotNull(fileData);
    const kmpData = await kmpCompiler.buildKmpFile(kmpJsonData, fileData);
    assert.isNotNull(kmpData);

    const zip = JSZip();

    let jszip = await zip.loadAsync(kmpData);
    assert.isNotNull(jszip.file('kmp.json')); // kmp.json should be present
    // kmp file should contain the following files
    const expectedFiles = [
      'FONTLOG.txt', 'image002.png', 'KAK_Documentation_EN.pdf', 'KAK_Documentation_KH.pdf',
      'keyboard_layout.png', 'khmer_angkor.js', 'khmer_angkor.kmx', 'khmer_angkor.kvk',
      'khmer_busra_kbd.ttf', 'Mondulkiri-R.ttf', 'OFL.txt', 'OFL-FAQ.txt', 'readme.htm',
      'splash.gif', 'welcome.htm',
      'kmp.json', // standard .kmp metadata file
      'kmp.inf', // legacy keyboard .kmp metdata file
    ];

    assert.sameMembers(Object.entries(jszip.files).map(([s, o]) => o.name).sort(),
      expectedFiles.sort(),
      'khmer_angkor.kmp file should have exactly the expected files');

    let finalKmpJsonData = JSON.parse(await jszip.file('kmp.json').async('string'));
    assert.deepEqual(finalKmpJsonData, kmpJsonFixture);
  });

  /*
   * Testing kmp.json generation
   */

  it(`should transform a .kps file for a keyboard package to a correct kmp.json`, async function () {
    const kpsPath = makePathToFixture('kmp.json', 'ahom_star.kps');
    const kmpJsonRefPath = makePathToFixture('kmp.json', 'kmp.json');

    let { kmpJsonData: kmpJsonActual } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
    if(kmpJsonActual == null) {
      callbacks.printMessages();
      assert.isNotNull(kmpJsonActual);
    }
    let kmpJsonFixture = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));
    assert.isNotNull(kmpJsonFixture);

    // Blank out system.keymanDeveloperVersion which will vary
    kmpJsonActual.system.keymanDeveloperVersion = '-';
    kmpJsonFixture.system.keymanDeveloperVersion = '-';

    // Strip file paths to basename for the actual (this is currently part of the
    // zip phase and not unit testable without refactoring)
    for(const file of kmpJsonActual.files) {
      file.name = path.basename(file.name);
    }

    assert.deepEqual(kmpJsonActual, kmpJsonFixture);
  });

  it(`should support .kps 17.0 metadata correctly`, async function () {
    const kpsPath = makePathToFixture('kmp_2.0', 'khmer_angkor.kps');
    const kmpJsonRefPath = makePathToFixture('kmp_2.0', 'kmp.json');

    let { kmpJsonData: kmpJsonActual } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
    if(kmpJsonActual == null) {
      callbacks.printMessages();
      assert.isNotNull(kmpJsonActual);
    }
    let kmpJsonFixture = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));
    assert.isNotNull(kmpJsonFixture);

    // Blank out system.keymanDeveloperVersion which will vary
    kmpJsonActual.system.keymanDeveloperVersion = '-';
    kmpJsonFixture.system.keymanDeveloperVersion = '-';

    // Strip file paths to basename for the actual (this is currently part of the
    // zip phase and not unit testable without refactoring)
    for(const file of kmpJsonActual.files) {
      file.name = path.basename(file.name);
    }

    assert.deepEqual(kmpJsonActual, kmpJsonFixture);
  });

  /*
   * Testing Warnings and Errors
   */

  it('should warn on absolute paths', async function() {
    this.timeout(10000); // building a zip file can sometimes be slow

    const kpsPath = makePathToFixture('absolute_path', 'source', 'absolute_path.kps');
    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, null));

    let { kmpJsonData, fileData } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
    assert.isNull(kmpJsonData);
    assert.isNull(fileData);

    assert.lengthOf(callbacks.messages, 2);
    assert.deepEqual(callbacks.messages[0].code, PackageCompilerMessages.WARN_AbsolutePath);
    assert.deepEqual(callbacks.messages[1].code, PackageCompilerMessages.ERROR_FileDoesNotExist);
  });

  // Testing path normalization

  it('should normalize DOS pathnames from \\ to /', async function() {

    const kpsPath = makePathToFixture('normalize_paths', 'source', 'khmer_angkor.kps');
    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, null));

    const { kmpJsonData: kmpJson } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};

    callbacks.printMessages();
    assert.lengthOf(callbacks.messages, 0);
    /*
    <ExecuteProgram>exe\myprogram.exe</ExecuteProgram>
    <ReadMeFile>..\source\readme.htm</ReadMeFile>
    <GraphicFile>..\source\splash.gif</GraphicFile>
    <WelcomeFile>..\source\welcome.htm</WelcomeFile>
    <MSIFileName>msi\myprogram.msi</MSIFileName>
    */
    assert.equal(kmpJson.options.executeProgram, 'exe/myprogram.exe');
    assert.equal(kmpJson.options.readmeFile, '../source/readme.htm');
    assert.equal(kmpJson.options.graphicFile, '../source/splash.gif');
    assert.equal(kmpJson.options.welcomeFile, '../source/welcome.htm');
    assert.equal(kmpJson.options.msiFilename, 'msi/myprogram.msi');
    assert.equal(kmpJson.files[0].name, '../build/khmer_angkor.js');

    /*
      <OSKFont>..\shared\fonts\khmer\busrakbd\khmer_busra_kbd.ttf</OSKFont>
      <DisplayFont>..\shared\fonts\khmer\mondulkiri\Mondulkiri-R.ttf</DisplayFont>
    */

    // These are stripped to basename; note that there may be platform
    // differences
    assert.equal(kmpJson.keyboards[0].oskFont, 'khmer_busra_kbd.ttf');
    assert.equal(kmpJson.keyboards[0].displayFont, 'Mondulkiri-R.ttf');
  });

  //
  // Test some invalid package metadata
  //
  it(`should load a package with missing keyboard ID metadata`, async function () {
    const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(makePathToFixture('invalid', 'missing_keyboard_id.kps')) ?? {};
    assert.isNull(kmpJsonData); // with a missing keyboard_id, the package shouldn't load, but it shouldn't crash either
    assert.deepEqual(callbacks.messages[0].code, PackageCompilerMessages.ERROR_KeyboardContentFileNotFound);
  });

  it(`should load a package with missing keyboard name metadata`, async function () {
    const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(makePathToFixture('invalid', 'missing_keyboard_name.kps')) ?? {};
    assert.equal(kmpJsonData.keyboards[0].name, 'version 4'); // picks up example.kmx's name
  });

  it(`should load a package with missing keyboard version metadata`, async function () {
    const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(makePathToFixture('invalid', 'missing_keyboard_version.kps')) ?? {};
    assert.equal(kmpJsonData.keyboards[0].version, '4.0');  // picks up example.kmx's version
  });

  it(`should handle a range of valid BCP47 tags`, async function () {
    const inputFilename = makePathToFixture('bcp47', 'valid_bcp47.kps');
    const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(inputFilename);
    assert.isNotNull(kmpJsonData);
    const validation = new PackageValidation(callbacks, {});
    assert.isTrue(validation.validate(inputFilename, kmpJsonData));
  });

  it(`should reject an invalid BCP47 tag`, async function () {
    const inputFilename = makePathToFixture('bcp47', 'invalid_bcp47_1.kps');
    const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(inputFilename);
    assert.isNotNull(kmpJsonData);
    const validation = new PackageValidation(callbacks, {});
    assert.isFalse(validation.validate(inputFilename, kmpJsonData));
  });

  it(`should download a file from a GitHub raw url`, async function () {
    if(TEST_SAVE_FIXTURES) {
      this.timeout(10000);
    }

    // https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf
    const source = 'https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf';
    const matches = getFileDataEndpoints.GITHUB_STABLE_SOURCE.exec(source);
    assert.isNotNull(matches);
    assert.equal(matches.groups.owner, 'silnrsi');
    assert.equal(matches.groups.repo, 'fonts');
    assert.equal(matches.groups.branch, 'b88c7af5d16681bd137156929ff8baec82526560');
    assert.equal(matches.groups.path, 'fonts/sil/alkalami/Alkalami-Regular.ttf');
    const res = await getFileDataEndpoints.getFileDataFromGitHub(callbacks, '', matches);
    assert.isNotNull(res);
    assert.isNotNull(res.data);
    assert.equal(res.basename, 'Alkalami-Regular.ttf');
    // TODO: should we check file validity as a real TTF?
    assert.equal(res.data.length, 145540); // size of Alkalami-Regular.ttf (for that hash)
  });

  it(`should resolve a source filename from fonts.languagetechnology.org`, async function () {
    if(TEST_SAVE_FIXTURES) {
      this.timeout(10000);
    }

    // https://fonts.languagetechnology.org/families.json / andika
    const source = 'flo:andika';
    const matches = getFileDataEndpoints.FLO_SOURCE.exec(source);
    assert.isNotNull(matches);
    assert.equal(matches.groups.family, 'andika');
    const res = await getFileDataEndpoints.getFileStableRefFromFlo(callbacks, 'flo:andika');
    assert.isNotNull(res);
    assert.equal(res, 'https://github.com/silnrsi/fonts/raw/044a10426cdabfa3dd3b13d040a56d4b70ea4058/fonts/sil/andika/Andika-Regular.ttf');
    // TODO: should we check file validity as a real TTF?
  });

  it(`should resolve a FLO source file`, async function() {
    assert.isTrue(await unitTestEndpoints.checkSourceFile(callbacks, 'https://github.com/silnrsi/fonts/raw/000000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf',
      'flo:andika'));
    assert.lengthOf(callbacks.messages, 1);
    assert.equal(callbacks.messages[0].code, PackageCompilerMessages.HINT_SourceFileHasChanged);
  });

  const refs = [
    ['raw, short form',               'https://github.com/silnrsi/fonts/raw/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['raw, long form',                'https://github.com/silnrsi/fonts/raw/refs/heads/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['tree, short form',              'https://github.com/silnrsi/fonts/tree/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['tree, long form',               'https://github.com/silnrsi/fonts/tree/refs/heads/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['blob, short form',              'https://github.com/silnrsi/fonts/blob/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['blob, long form',               'https://github.com/silnrsi/fonts/blob/refs/heads/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['raw, githubusercontent, short form', 'https://raw.githubusercontent.com/silnrsi/fonts/main/fonts/sil/andika/Andika-Regular.ttf'],
    ['raw, githubusercontent, long form',  'https://raw.githubusercontent.com/silnrsi/fonts/refs/heads/main/fonts/sil/andika/Andika-Regular.ttf'],
  ];

  refs.forEach(ref => {
    it(`should resolve a GitHub source file in ${ref[0]}`, async function() {
      assert.isTrue(await unitTestEndpoints.checkSourceFile(callbacks, 'https://github.com/silnrsi/fonts/raw/000000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf',
        ref[1]));
      assert.lengthOf(callbacks.messages, 1);
      assert.equal(callbacks.messages[0].code, PackageCompilerMessages.HINT_SourceFileHasChanged);
    });
  });

  it(`should return ERROR_InvalidSourceFileReference if an invalid source is given`, async function() {
    assert.isFalse(await unitTestEndpoints.checkSourceFile(callbacks, 'https://github.com/silnrsi/fonts/raw/000000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf',
      'garbage:unknown'));
    assert.lengthOf(callbacks.messages, 1);
    assert.equal(callbacks.messages[0].code, PackageCompilerMessages.ERROR_InvalidSourceFileReference);
  });

  it(`should return ERROR_UriIsNotARecognizedGitHubUri if an invalid GitHub ref is given`, async function() {
    // xxx instead of raw below
    assert.isNull(await unitTestEndpoints.getFileStableRefFromGitHub(callbacks, 'https://github.com/silnrsi/fonts/xxx/000000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf'));
    assert.lengthOf(callbacks.messages, 1);
    assert.equal(callbacks.messages[0].code, PackageCompilerMessages.ERROR_UriIsNotARecognizedGitHubUri);
  });

  [
    // xxx instead of raw
    'https://github.com/silnrsi/fonts/xxx/000000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf',
    // 39 instead of 40 chars
    'https://github.com/silnrsi/fonts/raw/00000000000000000000000000000/fonts/sil/andika/Andika-Regular.ttf',
  ].forEach(url => {
    it(`should return ERROR_UriIsNotARecognizedStableGitHubUri if the non-stable GitHub URL '${url}' is given`, async function() {

      assert.isNull(await unitTestEndpoints.getFileDataFromRemote(callbacks, url, ''));
      assert.lengthOf(callbacks.messages, 1);
      assert.equal(callbacks.messages[0].code, PackageCompilerMessages.ERROR_UriIsNotARecognizedStableGitHubUri);
    });
  });
});
