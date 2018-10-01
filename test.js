import test from 'ava';

test('Can provide context to LMLayer', async t => {
  const LMLayer = require('./').LMLayer;
  const lm = new LMLayer;

  t.plan(1);

  let suggestions = await lm.predictWithContext({
    transform: { insert: 'D', deleteLeft: 0, deleteRight: 0 },
    context: { left: '' }
  });

  t.deepEqual(suggestions, [
    { insert: 'Derek', deleteLeft: 1, deleteRight: 0 }
  ]);
});
