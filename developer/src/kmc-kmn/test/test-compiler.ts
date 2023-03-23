import 'mocha';
import sinon from 'sinon';
// import chai, { expect } from 'chai';
import chai, { assert } from 'chai';
import sinonChai from 'sinon-chai';
import { Compiler } from '../src/main.js';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');

chai.use(sinonChai);

describe('Compiler class', function() {
  let consoleLog: any;

  beforeEach(function() {
    consoleLog = sinon.spy(console, 'log');
  });

  afterEach(function() {
    consoleLog.restore();
  });

  it('should start', async function() {
    const compiler = new Compiler();
    assert(await compiler.init());
  });

  it('should compile a basic keyboard', async function() {
    const compiler = new Compiler();
    assert(await compiler.init());

    const fixtureName = __dirname + '/../../test/fixtures/000.kmx';
    const infile = __dirname + '/../../test/fixtures/000.kmn';
    const outfile = __dirname + '/000.kmx';

    assert(compiler.run(infile, outfile, {saveDebug: true, shouldAddCompilerVersion: false}));

    assert(fs.existsSync(outfile));
    const outfileData = fs.readFileSync(outfile);
    const fixtureData = fs.readFileSync(fixtureName);
    assert.equal(outfileData.byteLength, fixtureData.byteLength);
    assert.deepEqual(outfileData, fixtureData);
  });
});