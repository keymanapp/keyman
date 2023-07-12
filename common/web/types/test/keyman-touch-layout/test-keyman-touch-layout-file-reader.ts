import * as fs from 'fs';
import 'mocha';
import { assert } from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { TouchLayoutFileReader } from "../../src/keyman-touch-layout/keyman-touch-layout-file-reader.js";

describe('TouchLayoutFileReader', function () {
  it('should read a valid file', function() {
    const path = makePathToFixture('keyman-touch-layout', 'khmer_angkor.keyman-touch-layout');
    const input = fs.readFileSync(path);
    const reader = new TouchLayoutFileReader();
    const layout = reader.read(input);
    reader.validate(layout);

    // We don't really need to validate the JSON parser. We'll do a handful of
    // tests for object integrity and a couple of object members inside the file
    // just as a sanity check
    assert.hasAllKeys(layout, ['tablet', 'phone']);
    assert.hasAllKeys(layout.tablet, ['layer', 'displayUnderlying', 'font', 'fontsize']);
    assert.isFalse(layout.tablet.displayUnderlying);
    assert.strictEqual(layout.tablet.font, 'Khmer Busra Kbd');
    assert.strictEqual(layout.tablet.fontsize, '0.8em');
    assert.isArray(layout.tablet.layer);
    assert.strictEqual(layout.tablet.layer.length, 3);
    assert.strictEqual(layout.tablet.layer[0].id, 'default');
    assert.isArray(layout.tablet.layer[0].row);
    assert.isArray(layout.tablet.layer[0].row);
    assert.strictEqual(layout.tablet.layer[0].row.length, 5);
    assert.strictEqual(layout.tablet.layer[0].row[0].id, 1);
    assert.isArray(layout.tablet.layer[0].row[0].key);
    assert.strictEqual(layout.tablet.layer[0].row[0].key.length, 13);
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].id, "K_1");
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].text, "១");
  });

  it('should fixup numeric types in a valid legacy file', function() {
    const path = makePathToFixture('keyman-touch-layout', 'legacy.keyman-touch-layout');
    const input = fs.readFileSync(path);
    const reader = new TouchLayoutFileReader();
    const layout = reader.read(input);
    reader.validate(layout);

    // row.id can be a string in legacy files
    assert.strictEqual(layout.tablet.layer[0].row[0].id, 1);

    // key pad, width and sp can be a string in legacy files
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].pad, 25);
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].width, 100);
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].sp, 1);

    // sk pad, width and sp can be a string in legacy files
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].sk[0].pad, 10);
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].sk[0].width, 25);
    assert.strictEqual(layout.tablet.layer[0].row[0].key[0].sk[0].sp, 8);
  });

  it('should fixup remove empty arrays in a valid legacy file', function() {
    const path = makePathToFixture('keyman-touch-layout', 'legacy.keyman-touch-layout');
    const input = fs.readFileSync(path);
    const reader = new TouchLayoutFileReader();
    const layout = reader.read(input);
    reader.validate(layout);

    // sk was defined as [] in legacy.keyman-touch-layout but we want to treat
    // it as not present
    assert.isUndefined(layout.tablet.layer[0].row[0].key[1].sk);
  });
});
