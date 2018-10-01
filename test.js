import test from 'ava';

test('new worker', _t => {
  const LMLayer = require('./').LMLayer;
  const _lm = new LMLayer;
});
