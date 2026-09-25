/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import { env } from 'node:process';

import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KeymanTester } from '../src/KeymanTester.js';
import { makePathToFixture } from './helpers/index.js';

const { TEST_SAVE_ARTIFACTS } = env;
let outputRoot: string = '/an/imaginary/root/';

describe('KeymanTester', function() {
  const callbacks = new TestCompilerCallbacks();

  this.beforeAll(function() {
    if(TEST_SAVE_ARTIFACTS) {
      outputRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-test-'));
      console.log(`Output written to '${outputRoot}'`);
    }
  });

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should run a .kmn regression test', async function() {
    const projectSource = makePathToFixture('khmer_angkor/khmer_angkor.kpj');

    const tester = new KeymanTester();
    assert.isTrue(await tester.init(callbacks, {}));
    const result = await tester.run(projectSource);

    //
    // Verify that we have no messages and a successful result
    //

    assert.isOk(result);
    assert.isEmpty(callbacks.filteredMessages());
  });

  it('should run a .kmn regression test against an LDML keyboard', async function() {
    this.timeout(5000);

    const projectSource = makePathToFixture('khmer_ldml/khmer_ldml.kpj');

    const tester = new KeymanTester();
    assert.isTrue(await tester.init(callbacks, {}));
    const result = await tester.run(projectSource);

    //
    // Verify that we have no messages and a successful result
    //

    assert.isOk(result);
    assert.isEmpty(callbacks.filteredMessages());
  });
});

