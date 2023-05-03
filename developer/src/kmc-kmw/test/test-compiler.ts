import 'mocha';
import { assert } from 'chai';
// import sinonChai from 'sinon-chai';
import { WriteCompiledKeyboard } from '../src/compiler/write-compiled-keyboard.js';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { Compiler } from '@keymanapp/kmc-kmn';
import { KMX, KmxFileReader } from '@keymanapp/common-types';

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

    const kmxCompiler = new Compiler();
    assert.isTrue(await kmxCompiler.init());

    // TODO: runToMemory, add option to kmxCompiler to store debug-data for conversion to .js (e.g. store metadata, group readonly metadata, etc)
    assert.isTrue(kmxCompiler.run(infile, outfile, callbacks));

    const reader = new KmxFileReader();
    const keyboard: KMX.KEYBOARD = reader.read(callbacks.loadFile(outfile));

    const js = WriteCompiledKeyboard(callbacks, 'khmer_angkor', keyboard, true);
    // const js = compiler.compile('khmer_angkor', keyboard);

    const fjs = fs.readFileSync(fixtureName, 'utf8');
    if(fjs !== js) {
      fs.writeFileSync(testOutfile, js);
      assert.fail('JS not equal');
    }
  });
});