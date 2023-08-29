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
import { CompilerMessages } from '../src/compiler/messages.js';

const debug = false;

describe('KmpCompiler', function () {
  const MODELS : string[] = [
    'example.qaa.sencoten',
    'withfolders.qaa.sencoten',
  ];

  const callbacks = new TestCompilerCallbacks();
  let kmpCompiler = new KmpCompiler(callbacks);

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
    it(`should transform ${modelID}.model.kps to kmp.json`, function () {
      let kmpJson: KmpJsonFile.KmpJsonFile;

      assert.doesNotThrow(() => {
        kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
      });

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJson, kmpJsonIntermediateFixture);

      // Note that in-memory kmp.json still contains paths in the files array.
      // However, when building the .kmp, the final written kmp.json data is
      // modified to strip paths.

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });
    it(`should build a full .kmp for ${modelID}`, async function() {
      const zip = JSZip();
      // Build kmp.json in memory
      const kmpJson: KmpJsonFile.KmpJsonFile = kmpCompiler.transformKpsToKmpObject(kpsPath);
      // Build file.kmp in memory
      const promise = kmpCompiler.buildKmpFile(kpsPath, kmpJson);
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

  it('should generates a valid .kmp (zip) file', async function() {
    this.timeout(10000); // building a zip file can sometimes be slow

    const kpsPath = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpJsonRefPath = makePathToFixture('khmer_angkor', 'ref', 'kmp.json');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonFixture: KmpJsonFile.KmpJsonFile = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    let kmpJson = null;
    assert.doesNotThrow(() => {
      kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    });

    const kmpData = await kmpCompiler.buildKmpFile(kpsPath, kmpJson);

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

    let kmpJsonData = JSON.parse(await jszip.file('kmp.json').async('string'));
    assert.deepEqual(kmpJsonData, kmpJsonFixture);
  });

  /*
   * Testing kmp.json generation
   */

  it(`should transform a .kps file for a keyboard package to a correct kmp.json`, function () {
    callbacks.clear();

    const kpsPath = makePathToFixture('kmp.json', 'ahom_star.kps');
    const kmpJsonRefPath = makePathToFixture('kmp.json', 'kmp.json');

    let kmpJsonActual = kmpCompiler.transformKpsToKmpObject(kpsPath);
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

    callbacks.clear();

    const kpsPath = makePathToFixture('absolute_path', 'source', 'absolute_path.kps');
    const kmpCompiler = new KmpCompiler(callbacks);

    let kmpJson: KmpJsonFile.KmpJsonFile = null;

    assert.doesNotThrow(() => {
      kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath);
    });

    await assert.isNull(kmpCompiler.buildKmpFile(kpsPath, kmpJson));

    if(debug) callbacks.printMessages();

    assert.lengthOf(callbacks.messages, 2);
    assert.deepEqual(callbacks.messages[0].code, CompilerMessages.WARN_AbsolutePath);
    assert.deepEqual(callbacks.messages[1].code, CompilerMessages.ERROR_FileDoesNotExist);
  });
});
