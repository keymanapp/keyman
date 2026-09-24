/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-08-11
 */
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
// import { KmnCompiler } from '../../src/compiler/compiler.js';
import { KMX, TouchLayout } from '@keymanapp/common-types';
import { KmwCompilerMessages } from '../../src/kmw-compiler/kmw-compiler-messages.js';
import { unitTestEndpoints } from '../../src/kmw-compiler/compile-layout-file.js';
import { setupGlobals } from '../../src/kmw-compiler/compiler-globals.js';

describe('compile-layout-file', function() {
  const callbacks = new TestCompilerCallbacks(this);
  const options = {
      shouldAddCompilerVersion: false,
      saveDebug: true,
    };
  // const kmnCompiler: KmnCompiler = new KmnCompiler();

  this.beforeAll(async function() {
    // assert.isTrue(await kmnCompiler.init(callbacks, options));
    setupGlobals(callbacks, options, '', '', null, { groups: [], isMnemonic: false, startGroup: { ansi: 0, newContext: 0, postKeystroke: 0, unicode: 0 }, stores: [], targets: '', fileVersion: KMX.KMXFile.VERSION_100}, '');
  });

  this.afterEach(function() {
    if (this.currentTest.state !== 'passed') {
      callbacks.printMessages();
    }
  })

  const fixtures: {name: string, data: TouchLayout.TouchLayoutFile}[] = [
    { name: 'a null platform.layer property', data: { desktop: { layer: null, defaultHint: null } } },
    { name: 'an invalid platform.layer property', data: { desktop: { layer: {} as any, defaultHint: null } } },
    { name: 'a null platform.layer.row property', data: { desktop: { layer: [{id:'', row: null}], defaultHint: null } } },
    { name: 'an invalid platform.layer.row property', data: { desktop: { layer: [{id:'', row: "" as any}], defaultHint: null } } },
    { name: 'a null platform.layer.row.key property', data: { desktop: { layer: [{id:'', row: [{id:'', key: null}]}], defaultHint: null } } },
    { name: 'an invalid platform.layer.row.key property', data: { desktop: { layer: [{id:'', row: [{id:'', key: 100}]}], defaultHint: null } } },
    { name: 'an invalid platform.layer.row.key.sk property', data: { desktop: { layer: [{id:'', row: [{id:'', key: [{sk: "x" as any}]}]}], defaultHint: null } } },
    { name: 'an invalid platform.layer.row.key.multitap property', data: { desktop: { layer: [{id:'', row: [{id:'', key: [{multitap: "x" as any}]}]}], defaultHint: null } } },
    { name: 'an invalid platform.layer.row.key.flick property', data: { desktop: { layer: [{id:'', row: [{id:'', key: [{flick: "x" as any}]}]}], defaultHint: null } } },
  ]

  fixtures.forEach(fixture => {
    it(`should raise an error if a file has ${fixture.name}`, function() {
      assert.isNull(unitTestEndpoints.validateLayoutFileContent(fixture.data, []));
      assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.ERROR_InvalidTouchLayoutFileFormat));
    });
  });
});
