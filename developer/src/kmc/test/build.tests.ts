/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import 'mocha';
import { assert } from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { clearOptions } from '../src/util/options.js';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { unitTestEndpoints } from '../src/commands/build.js';

import { clearOptions } from '../src/util/options.js';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { BuildLdmlKeyboard } from '../src/commands/buildClasses/BuildLdmlKeyboard.js';
import { KMX, KmxFileReader } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '@keymanapp/kmc-ldml';

interface CompilerWarningsAsErrorsTruthTable {
  cli: boolean;
  kpj: boolean;
  result: boolean;
};

describe('compilerWarningsAsErrors', function () {
  const callbacks = new TestCompilerCallbacks(this);
  let outPath: string = null;

  this.beforeEach(function() {
    clearOptions();
    outPath = fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-'));
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
      console.error(`Output kept at ${outPath}`);
    } else {
      if(outPath) fs.rmSync(outPath, {recursive: true, force: true});
    }
    outPath = null;
  });

  describe('compilerWarningsAsErrors', function () {

    // The CLI option should override the project setting

    const truthTable: CompilerWarningsAsErrorsTruthTable[] = [
      {cli:undefined, kpj:undefined, result:true}, // default setting (false) wins
      {cli:undefined, kpj:false, result:true},  // kpj setting wins
      {cli:undefined, kpj:true, result:false},  // kpj setting wins
      {cli:false, kpj:undefined, result:true},  // cli setting wins
      {cli:false, kpj:false, result:true},      // cli setting wins
      {cli:false, kpj:true, result:true},       // cli setting wins
      {cli:true, kpj:undefined, result:false},  // cli setting wins
      {cli:true, kpj:false, result:false},      // cli setting wins
      {cli:true, kpj:true, result:false},       // cli setting wins
    ]

    for(const truth of truthTable) {
      it(`should ${truth.result?'':'fail to '}build a project when kpj option=${truth.kpj} and cli option=${truth.cli}`, async function() {
        const builder = new BuildProject();
        const path = makePathToFixture('compiler-warnings-as-errors',
          `compiler_warnings_as_errors_${truth.kpj === true ? 'true' : (truth.kpj === false ? 'false' : 'undefined')}.kpj`);
        const result = await builder.build(path, null, callbacks, {
          compilerWarningsAsErrors: truth.cli,
        });
        if(truth.result) {
          assert.isTrue(result);
        } else {
          assert.isFalse(result);
        }
      });
    }
  });

  describe('targetVersion', function() {
    [
      [17, KMX.KMXFile.VERSION_170],
      [19, KMX.KMXFile.VERSION_190],
    ].forEach(function(v) {
      it(`should build a v${v[0]}.0 LDML keyboard`, async function() {
        const builder = new BuildLdmlKeyboard();
        const infile = makePathToFixture('ldml', 'basic.xml');
        const outfile = path.join(outPath, 'basic.kmx');
        const result = await builder.build(infile, outfile, callbacks, {  targetVersion: v[1] });
        assert.isTrue(result);

        const reader = new KmxFileReader();
        const keyboard = reader.read(callbacks.loadFile(outfile));
        assert.equal(keyboard.fileVersion, v[1]);
      });
    });

    it(`should error if an unsupported version is passed when compiling an LDML keyboard`, async function() {
      const builder = new BuildLdmlKeyboard();
      const infile = makePathToFixture('ldml', 'basic.xml');
      const outfile = path.join(outPath, 'basic.kmx');
      const result = await builder.build(infile, outfile, callbacks, {  targetVersion: KMX.KMX_Version.VERSION_70 });
      assert.isFalse(result);
      assert.isTrue(callbacks.hasMessage(LdmlCompilerMessages.ERROR_InvalidTargetVersion));
    });
  });
});

describe('interpretOutFile()', function() {

  function statSyncStub(path: fs.PathLike, options?: fs.StatSyncOptions): fs.Stats {
    if(path === 'does_not_exist') return null;
    return {
      isFile: () => !path.toString().startsWith('existing_folder'),
      isDirectory: () => path.toString().startsWith('existing_folder'),
      isBlockDevice: () => false,
      isCharacterDevice: () => false,
      isSymbolicLink: () => false,
      isFIFO: () => false,
      isSocket: () => false,
      dev: null,
      ino: null,
      mode: null,
      nlink: null,
      uid: null,
      gid: null,
      rdev: null,
      size: null,
      blksize: null,
      blocks: null,
      atimeMs: null,
      mtimeMs: null,
      ctimeMs: null,
      birthtimeMs: null,
      atime: null,
      mtime: null,
      ctime: null,
      birthtime: null
    }
  }

  it(`should throw with invalid parameters`, function() {
    assert.throws(() => unitTestEndpoints.interpretOutFile(null, 1, statSyncStub));
    assert.throws(() => unitTestEndpoints.interpretOutFile(undefined, null, statSyncStub));
    assert.throws(() => unitTestEndpoints.interpretOutFile(undefined, -1, statSyncStub));
    assert.throws(() => unitTestEndpoints.interpretOutFile(undefined, 0, statSyncStub));
    assert.doesNotThrow(() => unitTestEndpoints.interpretOutFile(undefined, 1, statSyncStub));
    assert.doesNotThrow(() => unitTestEndpoints.interpretOutFile(undefined, 2, statSyncStub));
  });

  it(`should return undefined path if no path is specified`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile(undefined, 1, statSyncStub);
    assert.isFalse(spec1.isFolder);
    assert.isUndefined(spec1.path);

    const spec2 = unitTestEndpoints.interpretOutFile(undefined, 2, statSyncStub);
    assert.isFalse(spec2.isFolder);
    assert.isUndefined(spec2.path);
  });

  it(`should return isFolder=true if path is specified and more than 1 input filename is given`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile('does_not_exist', 2, statSyncStub);
    assert.isNotNull(spec1);
    assert.isTrue(spec1.isFolder);
    assert.equal(spec1.path, 'does_not_exist');
  });

  it(`should return path without trailing delimiter if trailing delimiter is specified`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile('does_not_exist/', 1, statSyncStub);
    assert.isNotNull(spec1);
    assert.isTrue(spec1.isFolder);
    assert.equal(spec1.path, 'does_not_exist');

    // existing folder test for trimming delimiter is covered in next test
  });

  it(`should return path as a folder if target path is a folder`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile('existing_folder', 1, statSyncStub);
    assert.isNotNull(spec1);
    assert.isTrue(spec1.isFolder);
    assert.equal(spec1.path, 'existing_folder');

    const spec2 = unitTestEndpoints.interpretOutFile('existing_folder/', 1, statSyncStub);
    assert.isNotNull(spec2);
    assert.isTrue(spec2.isFolder);
    assert.equal(spec2.path, 'existing_folder');
  });

  it(`should return null if a file exists in the target folder location`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile('existing_file', 2, statSyncStub);
    assert.isNull(spec1);

    const spec2 = unitTestEndpoints.interpretOutFile('existing_file/', 1, statSyncStub);
    assert.isNull(spec2);
  });

  it(`should return isFolder = false if a single input file and no trailing delimiter and no folder exists`, function() {
    const spec1 = unitTestEndpoints.interpretOutFile('anything', 1, statSyncStub);
    assert.isNotNull(spec1);
    assert.isFalse(spec1.isFolder);
    assert.equal(spec1.path, 'anything');
  });
});