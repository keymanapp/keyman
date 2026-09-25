/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'fs';
import 'mocha';
import { assert } from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { RegressionTestSourceFileReader } from '../../src/types/regression-test/regression-test-file-reader.js';
import { RegTestShiftState } from '../../src/types/regression-test/regression-test-file.js';

describe('regression-test-source-file-reader', function() {
  it('should read a valid file', function() {
    const path = makePathToFixture('regression-test', '1a.xml');
    const input = fs.readFileSync(path);

    const reader = new RegressionTestSourceFileReader();
    const regtest = reader.read(input);

    assert.deepEqual(regtest?.info, {
      version: '6.0',
      systemkeyboard: '00000409',
      keyboard: 'C:\\keyman\\keyboards\\experimental-keyboards\\khmer10\\source\\khmer10.1.kmx',
      beginmode: 'Unicode'
    });
    assert.deepEqual(regtest?.events, [
      { key: { shiftstate: [], vkey:'K_S' }, postcontext: [ 'ស' ] },
      { key: { shiftstate: [], vkey: 'K_SLASH' }, postcontext: [ 'ស៊' ] },
      {
        key: { shiftstate: [ RegTestShiftState.shift ], vkey: 'K_I' },
        postcontext: [ 'ស៊ី' ]
      }
    ]);
    // console.dir(regtest, {depth:12});
    // console.dir(regtest.regressiontest.events, {depth:4});
  });
  it('should handle interleaved deadkeys and text', function() {
    const path = makePathToFixture('regression-test', 'deadkey-interleaved.xml');
    const input = fs.readFileSync(path);

    const reader = new RegressionTestSourceFileReader();
    const regtest = reader.read(input);

    assert.deepEqual(regtest?.events, [
      { key: { shiftstate: [], vkey:'K_Q' }, postcontext: [ 'ប៉ាគ់', { deadkey: 'middle_q' }, 'foo'] },
    ]);
    // console.dir(regtest, {depth:12});
  });
});
