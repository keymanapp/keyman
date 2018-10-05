import test from 'ava';
import {LMLayer} from './';

const TYPE_D = { insert: 'D', deleteLeft: 0, deleteRight: 0 };
const EMPTY_CONTEXT = { left: '' };


test('It provide context to LMLayer', async t => {
  t.plan(2);

  const lm = new LMLayer;

  // Wait for the language model to initialize and declare its configuration.
  let configuration = await lm.initialize({ model: 'en-x-derek' });
  // The model should as for 32 code units of context to the left of the
  // cursor.
  t.is(configuration.leftContextCodeUnits, 32);

  // Now tell it the user typed 'D'.
  let message = await lm.predictWithContext({
    transform: TYPE_D, context: EMPTY_CONTEXT
  });

  // This dummy language model will always suggest 'Derek' as its return.
  let {suggestions} = message;
  t.deepEqual(suggestions, [
    { insert: 'Derek', deleteLeft: 1, deleteRight: 0 }
  ]);
});

test('It should not be able to predict() before initialized', async t => {
  t.plan(1);

  const lm = new LMLayer;
  try {
    await lm.predictWithContext({
      transform: TYPE_D, context: EMPTY_CONTEXT, customToken: null
    });
    t.fail();
  } catch (e) {
    t.regex(e.message, /(not |un)initialized/i);
  }
});

test('It should reject when predictions crash', async t => {
  t.plan(1);

  const lm = new LMLayer;
  let configuration = await lm.initialize({ model: 'en-x-derek' });

  try {
    await lm.predictWithContext({
      transform: TYPE_D, context: EMPTY_CONTEXT, customToken: null
    });
    t.fail();
  } catch (e) {
    t.regex(e.message, /invalid/i);
  }
});

