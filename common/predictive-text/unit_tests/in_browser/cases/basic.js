var assert = chai.assert;
describe('LMLayerWorkerCode', function() {
  it('should exist!', function() {
    assert.isFunction(LMLayerWorkerCode,
      'Could not find LMLayerWorkerCode! Does embedded_worker.js exist?'
    );
  });
});
