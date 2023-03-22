import 'mocha';
import sinon from 'sinon';
import chai, { expect } from 'chai';
import sinonChai from 'sinon-chai';
import { Compiler } from '../src/main.js';

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

    compiler.run();

    expect( consoleLog.calledOnce ).to.be.true;
    expect( consoleLog.calledWith('TODO: implement WASM hooks') ).to.be.true;
  });
});