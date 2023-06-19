import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { WriteCompiledKeyboard } from '../src/compiler/write-compiled-keyboard.js';
import { extractTouchLayout } from './util.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const fixturesDir = __dirname + '/../../test/fixtures/';
const fixtureName = fixturesDir + 'khmer_angkor.js';
const infile = fixturesDir + 'khmer_angkor.kmn';
const outfile = fixturesDir + 'khmer_angkor.kmx'; // intermediate outfile
const testOutfile = fixturesDir + 'khmer_angkor.test.js';

if(fs.existsSync(testOutfile)) {
  fs.unlinkSync(testOutfile);
}

const callbacks = new TestCompilerCallbacks();

const kmnCompiler = new KmnCompiler();
if(!await kmnCompiler.init(callbacks)) {
  console.error('kmx compiler failed to init');
  process.exit(1);
}

// TODO: this needs rewrite due to circular deps

let result = kmnCompiler.runCompiler(infile, outfile, {
  shouldAddCompilerVersion: false,
  saveDebug: true,  // TODO: we should probably use passed debug flag
});

if(!result) {
  callbacks.printMessages();
  process.exit(1);
}

const js = WriteCompiledKeyboard(callbacks, infile, result.kmx.data, result.kvk.data, null, true);

callbacks.printMessages();

const fjs = fs.readFileSync(fixtureName, 'utf8');

const expected = extractTouchLayout(fjs);
const actual = extractTouchLayout(js);

if(expected.js !== actual.js) {
  fs.writeFileSync(testOutfile, js);
  console.error('JS not equal');
  process.exit(1);
}
