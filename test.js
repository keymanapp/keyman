import test from 'ava';
import {LMLayer} from './';

const TYPE_D = { insert: 'D', deleteLeft: 0, deleteRight: 0 };
const EMPTY_CONTEXT = { left: '' };


test('Can provide context to LMLayer', async t => {
  t.plan(1);

  const lm = new LMLayer;

  let message = await lm.predictWithContext({
    transform: TYPE_D, context: EMPTY_CONTEXT
  });

  let {suggestions} = message;
  t.deepEqual(suggestions, [
    { insert: 'Derek', deleteLeft: 1, deleteRight: 0 }
  ]);
});


test('Can reject when predictions crash', async t => {
  t.plan(1);

  const lm = new LMLayer;
  try {
    await lm.predictWithContext({
      transform: TYPE_D, context: EMPTY_CONTEXT, customToken: NaN
    });
    t.fail();
  } catch (_e) {
    t.pass();
  }
});

