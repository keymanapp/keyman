/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-04-03
 *
 * Test for LineFinder
 */

import { assert } from 'chai';
import { readFileSync } from 'node:fs';
import 'mocha';
import { LineFinder } from '../src/line-utils.js';
import { makePathToFixture } from './helpers/index.js';

describe(`LineFinder test`, () => {
    const path = 'tran_fail-empty.xml';

    it(`Should be able to process ${path}`, () => {
        const xmlPath = makePathToFixture('xml', `${path}`);
        const data = readFileSync(xmlPath, 'utf-8');
        assert.ok(data);
        const lf = new LineFinder(data);
        assert.ok(lf);
        assert.deepEqual(lf.findOffset(40), {line: 3, column: 0});
        assert.deepEqual(lf.findOffset(136), {line: 4, column: 2});
        assert.deepEqual(lf.findOffset(186), {line: 8, column: 2});
    });
});
