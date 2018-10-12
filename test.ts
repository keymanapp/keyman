import test from 'ava';
import {LMLayer} from './';

const TYPE_D = { insert: 'D', delete: 0, deleteRight: 0 };
const EMPTY_CONTEXT = { left: '', right: '', startOfBuffer: true, endOfBuffer: true };


test('It provides context to LMLayer', async t => {
  t.plan(2);

  const lm = new LMLayer;

  // Wait for the language model to initialize and declare its configuration.
  let configuration = await lm.initialize({ model: './models/en-x-test-derek.js' });
  // The model should as for 32 code units of context to the left of the
  // cursor.
  t.is(configuration.leftContextCodeUnits, 32);

  // Now tell it the user typed 'D'.
  let message = await lm.predict({
    transform: TYPE_D, context: EMPTY_CONTEXT
  });

  // This dummy language model will always suggest 'Derek' as its return.
  let {suggestions} = message;
  t.deepEqual(suggestions, [
    {
      transform: {insert: 'Derek', delete: 1, deleteRight: 0},
      displayAs: 'Derek'
    }
  ]);
});

test('It should not be able to predict() before initialized', async t => {
  t.plan(1);

  const lm = new LMLayer;
  try {
    await lm.predict({ transform: TYPE_D, context: EMPTY_CONTEXT });
    t.fail();
  } catch (e) {
    t.regex(e.message, /(not |un)initialized/i);
  }
});

test('It should reject when predictions crash', async t => {
  t.plan(1);

  const lm = new LMLayer;
  await lm.initialize({ model: './models/en-x-test-derek.js' });

  try {
    await lm.predict({
      transform: TYPE_D, context: EMPTY_CONTEXT,
      // @ts-ignore
      customToken: null
    });
    t.fail();
  } catch (e) {
    t.regex(e.message, /invalid/i);
  }
});

/**
 * This is the 'teapots' test we use when describing n-grams.
 */
test('It should load a model from a file path', async t => {
  t.plan(1);

  const lm = new LMLayer;

  // Wait for the language model to initialize.
  await lm.initialize({ model: './models/en-x-test-teapot.js' });

  // Now tell it that the user typed "t" in a buffer
  // that reads "I'm a little "
  let message = await lm.predict({
    transform: {
      insert: 't',
      delete: 0,
      deleteRight: 0
    },
    context: {
      left: "I'm a little ",
      right: undefined,
      startOfBuffer: false,
      endOfBuffer: true
    }
  });

  // This dummy language model will always suggest 'Derek' as its return.
  let {suggestions} = message;
  t.deepEqual(suggestions, [
    {
      transform: {
        insert: 'teapot', delete: 1, deleteRight: 0,
      },
      displayAs: '🍵'
    }
  ]);
});
