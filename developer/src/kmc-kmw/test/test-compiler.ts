import 'mocha';
import { assert } from 'chai';
// import sinonChai from 'sinon-chai';
import { WriteCompiledKeyboard } from '../src/compiler/write-compiled-keyboard.js';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { KMX, KmxFileReader } from '@keymanapp/common-types';
import { extractTouchLayout } from './util.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const fixturesDir = __dirname + '/../../test/fixtures/';
//const baselineDir = __dirname + '/../../../../../common/test/keyboards/baseline/';
// chai.use(sinonChai);

describe('Compiler class', function() {
  const callbacks = new TestCompilerCallbacks();

  this.afterEach(function() {
    callbacks.printMessages();
    callbacks.clear();
  });

  it('should compile a basic keyboard', async function() {
    // const compiler = new KeymanWebCompiler(callbacks, {addCompilerVersion: false, debug: true});

    const fixtureName = fixturesDir + 'khmer_angkor.js';
    const infile = fixturesDir + 'khmer_angkor.kmn';
    const outfile = fixturesDir + 'khmer_angkor.kmx'; // intermediate outfile
    const testOutfile = fixturesDir + 'khmer_angkor.test.js';

    if(fs.existsSync(testOutfile)) {
      fs.unlinkSync(testOutfile);
    }

    const kmnCompiler = new KmnCompiler();
    assert.isTrue(await kmnCompiler.init(callbacks));

    // TODO: runToMemory, add option to kmxCompiler to store debug-data for conversion to .js (e.g. store metadata, group readonly metadata, visual keyboard source filename, etc)
    let result = kmnCompiler.runCompiler(infile, outfile, {
      shouldAddCompilerVersion: false,
      saveDebug: true,  // TODO: we should probably use passed debug flag
      target: 'js'
    });

    assert.isNotNull(result);

    const reader = new KmxFileReader();
    const keyboard: KMX.KEYBOARD = reader.read(result.kmx.data);

    const js = WriteCompiledKeyboard(callbacks, infile, outfile, 'khmer_angkor', keyboard, true);

    const fjs = fs.readFileSync(fixtureName, 'utf8');

    const expected = extractTouchLayout(fjs);
    const actual = extractTouchLayout(js);

    fs.writeFileSync(testOutfile + '.strip.js', actual.js);
    fs.writeFileSync(fixtureName + '.strip.js', expected.js);
    fs.writeFileSync(testOutfile, js);

    assert.deepEqual(actual.js, expected.js);
    assert.deepEqual(JSON.parse(actual.touchLayout), JSON.parse(expected.touchLayout));
  });
});