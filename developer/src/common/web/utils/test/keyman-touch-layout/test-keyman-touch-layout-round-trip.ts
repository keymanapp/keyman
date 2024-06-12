import * as fs from 'fs';
import 'mocha';
import { assert } from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { TouchLayoutFileReader } from "../../src/types/keyman-touch-layout/keyman-touch-layout-file-reader.js";
import { TouchLayoutFileWriter } from "../../src/types/keyman-touch-layout/keyman-touch-layout-file-writer.js";

describe('TouchLayoutFile', function () {
  it('should round-trip from TouchLayoutFileReader to TouchLayoutFileWriter', function() {
    const path = makePathToFixture('keyman-touch-layout', 'khmer_angkor.keyman-touch-layout');
    const input = fs.readFileSync(path);
    const reader = new TouchLayoutFileReader();
    const layout = reader.read(input);
    const writer = new TouchLayoutFileWriter();

    // We don't want to assert equality on formatting differences, or on the
    // fixups that we do on input files (empty arrays, stringified numbers), so
    // we'll re-read the output and compare it
    let output = writer.write(layout);
    let newLayout = reader.read(output);
    assert.deepEqual(layout, newLayout);

    // And do the same without formatted output
    output = writer.write(layout);
    newLayout = reader.read(output);
    assert.deepEqual(layout, newLayout);

    // And do the same without any options
    const output2 = writer.write(layout);
    assert.deepEqual(output, output2);

    newLayout = reader.read(output2);
    assert.deepEqual(layout, newLayout);
  });
});
