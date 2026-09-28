/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'node:fs';
import * as path from 'node:path';
import {assert} from 'chai';
import 'mocha';
import { serverBasePath, serverSitePath } from '../src/environment.js';

describe('serverBasePath', function() {
  it('should find index.js in the base path', function() {
    assert.isTrue(fs.existsSync(path.join(serverBasePath(), 'index.js')));
  });
});

describe('serverSitePath', function() {
  it('should find index.html in the base path', function() {
    assert.isTrue(fs.existsSync(path.join(serverSitePath(), 'index.html')));
  });
});
