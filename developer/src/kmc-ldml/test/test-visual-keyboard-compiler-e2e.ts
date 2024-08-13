import 'mocha';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import { KvkFileWriter } from '@keymanapp/common-types';
import {checkMessages,  compilerTestOptions,  compileVisualKeyboard, makePathToFixture} from './helpers/index.js';

describe('visual-keyboard-compiler', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should build fixtures', async function() {
    // Let's build basic.xml
    // It should match basic.kvk (built from basic-kvk.txt)

    const inputFilename = makePathToFixture('basic.xml');
    const binaryFilename = makePathToFixture('basic-kvk.txt');

    // Compile the visual keyboard
    const vk = await compileVisualKeyboard(inputFilename, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
    assert.isNotNull(vk);

    // Use the builder to generate the binary output file
    const writer = new KvkFileWriter();
    const code = writer.write(vk);
    checkMessages();
    assert.isNotNull(code);

    // Compare output
    let expected = await hextobin(binaryFilename, undefined, {silent:true});
    assert.deepEqual<Uint8Array>(code, expected);
  });
});
