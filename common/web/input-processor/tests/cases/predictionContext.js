import { assert } from 'chai';
import sinon from 'sinon';

import LanguageProcessor from '#./text/prediction/languageProcessor.js';
import PredictionContext from '#./text/prediction/predictionContext.js';
import { Worker as LMWorker } from "@keymanapp/lexical-model-layer/node";
import { DeviceSpec, KeyboardProcessor, Mock } from '@keymanapp/keyboard-processor';

function compileDummyModel(suggestionSets) {
  return `
LMLayerWorker.loadModel(new models.DummyModel({
  futureSuggestions: ${JSON.stringify(suggestionSets, null, 2)},
}));
`;
}

// Common spec used for each test's setup.  It's actually irrelevant for the tests,
// but KeyboardProcessor needs an instance.
const deviceSpec = new DeviceSpec(
  DeviceSpec.Browser.Chrome,
  DeviceSpec.FormFactor.Desktop,
  DeviceSpec.OperatingSystem.Windows
);

const appleDummySuggestionSets = [[
  // Set 1:
  {
    transform: { insert: 'e', deleteLeft: 0},
    displayAs: 'apple',
  }, {
    transform: { insert: 'y', deleteLeft: 0},
    displayAs: 'apply'
  }, {
    transform: { insert: 'es', deleteLeft: 0},
    displayAs: 'apples'
  }
], [
  // Set 2:
  {
    transform: { insert: '', deleteLeft: 0},
    displayAs: 'apple',
    tag: 'keep'
  }, {
    transform: { insert: 'y', deleteLeft: 0},
    displayAs: 'apply'
  }, {
    transform: { insert: 's', deleteLeft: 1},
    displayAs: 'apps'
  }
], [
  // Set 3:
  {
    transform: { insert: 'ied', deleteLeft: 2},
    displayAs: 'applied'
  }
], [
  {
    transform: { insert: ' reverted', deleteLeft: 5},
    displayAs: 'reverted'
  }
]];

const appleDummyModel = {
  id: 'dummy',
  languages: ['en'],
  code: compileDummyModel(appleDummySuggestionSets)
};

describe("PredictionContext", () => {
  it('receives predictions as they are generated', async () => {
    const langProcessor = new LanguageProcessor(LMWorker.constructInstance());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const kbdProcessor = new KeyboardProcessor(deviceSpec);

    let mock = new Mock("appl", 4); // "appl|", with '|' as the caret position.
    const initialMock = Mock.from(mock);

    let context = new PredictionContext(langProcessor, kbdProcessor, mock);

    let updateFake = sinon.fake();
    context.on('update', updateFake);
    context.sendUpdateEvent(); // Allows external code to request a re-retrieval of current suggestion state.

    // Initial predictive state:  no suggestions.  context.initializeState() has not yet been called.
    assert.equal(updateFake.callCount, 1);
    assert.isEmpty(updateFake.firstCall.args[0]); // should have no suggestions. (if convenient for testing)

    // Now, let's initialize the predictive state - this will load the initial suggestions.
    await context.initializeState();
    let suggestions;

    // Initialization results:  our first set of dummy suggestions.
    assert.equal(updateFake.callCount, 2);
    suggestions = updateFake.secondCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apples']);
    assert.isNotOk(suggestions.find((obj) => obj.tag == 'keep'));
    assert.isNotOk(suggestions.find((obj) => obj.transform.deleteLeft != 0));


    mock.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = mock.buildTranscriptionFrom(initialMock, null, true);
    await langProcessor.predict(transcription, kbdProcessor.layerId);

    // First predict call results:  our second set of dummy suggestions, the first of which includes
    // a 'keep' of the original text.
    assert.equal(updateFake.callCount, 3);
    suggestions = updateFake.thirdCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apps']);
    assert.equal(suggestions.find((obj) => obj.tag == 'keep').displayAs, 'apple');
    assert.equal(suggestions.find((obj) => obj.transform.deleteLeft != 0).displayAs, 'apps');
  });

  it('sendUpdateState retrieves the most recent suggestion set', async () => {
    const langProcessor = new LanguageProcessor(LMWorker.constructInstance());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const kbdProcessor = new KeyboardProcessor(deviceSpec);

    let mock = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    let context = new PredictionContext(langProcessor, kbdProcessor, mock);
    let initialSuggestions = await context.initializeState();

    let updateFake = sinon.fake();
    context.on('update', updateFake);
    context.sendUpdateEvent(); // Allows external code to request a re-retrieval of current suggestion state.

    // Now, let's initialize the predictive state - this will load the initial suggestions.
    let suggestions;

    // Initialization results:  our first set of dummy suggestions.
    assert.isTrue(updateFake.calledOnce);
    suggestions = updateFake.firstCall.args[0];
    assert.deepEqual(suggestions, initialSuggestions);

    // The array instances may be different, but their contents should be the same instances and in the same order.
    assert.sameOrderedMembers(suggestions, initialSuggestions);
  });

  it('suggestion application logic & triggered effects', async () => {
    const langProcessor = new LanguageProcessor(LMWorker.constructInstance());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const kbdProcessor = new KeyboardProcessor(deviceSpec);

    let textState = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    let predictiveContext = new PredictionContext(langProcessor, kbdProcessor, textState);
    // Pre-initialize the predictive state.
    await predictiveContext.initializeState();

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    let suggestions;

    let previousTextState = Mock.from(textState);
    textState.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = textState.buildTranscriptionFrom(previousTextState, null, true);
    await langProcessor.predict(transcription, kbdProcessor.layerId);

    // Verify setup.
    assert.equal(updateFake.callCount, 1);
    suggestions = updateFake.firstCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apps']);
    assert.equal(suggestions.find((obj) => obj.tag == 'keep').displayAs, 'apple');
    assert.equal(suggestions.find((obj) => obj.transform.deleteLeft != 0).displayAs, 'apps');

    // Now for the real test.
    previousTextState = Mock.from(textState); // snapshot it!

    const suggestionApply = suggestions.find((obj) => obj.displayAs == 'apply');
    assert.isOk(suggestionApply);

    let promiseForApplyReversion = predictiveContext.accept(suggestionApply);

    assert.equal(updateFake.callCount, 1); // No new 'update' has been raised yet.

    // Check 1:  did our active text context get changed?  We DID just ask to apply
    //           a suggestion...
    assert.notEqual(textState.getText(), previousTextState.getText());
    assert.equal(textState.getText(), 'apply ');

    let reversion = await promiseForApplyReversion;
    // We don't seem to need to additionally rig a wait for the triggered predict call; it
    // always completes first.  Neat.  If test becomes unstable... yeah, rig up a wait for it.

    // Check 2:  a second 'update' - post-application predictions!
    assert.equal(updateFake.callCount, 2);
    suggestions = updateFake.secondCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['applied']);
    assert.isNotOk(suggestions.find((obj) => obj.tag == 'keep'));
    assert.equal(suggestions[0].transform.deleteLeft, 2);

    // Check 4+:  for the returned reversion object.
    assert.isOk(reversion);

    // All other reversion details are tested in the 'reversion application logic...' section defined below.
  });

  it('reversion application logic & triggered effects', async () => {
    const langProcessor = new LanguageProcessor(LMWorker.constructInstance());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const kbdProcessor = new KeyboardProcessor(deviceSpec);

    let textState = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    // Test setup - return to the state at the end of the prior-defined unit test ('suggestion application...')

    let predictiveContext = new PredictionContext(langProcessor, kbdProcessor, textState);
    // Pre-initialize the predictive state.
    await predictiveContext.initializeState();

    // This is the point in time that a reversion operation will rewind the context to.
    const revertBaseTextState = Mock.from(textState);
    textState.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = textState.buildTranscriptionFrom(revertBaseTextState, null, true);

    let suggestionCaptureFake = sinon.fake();
    predictiveContext.once('update', suggestionCaptureFake);
    await langProcessor.predict(transcription, kbdProcessor.layerId);

    // We need to capture the suggestion we wish to apply.  We could hardcode a forced
    // value, but that might become brittle in the long-term.
    const originalSuggestionSet = suggestionCaptureFake.firstCall.args[0];
    const suggestionApply = originalSuggestionSet.find((obj) => obj.displayAs == 'apply');
    assert.isOk(suggestionApply);

    let previousTextState = Mock.from(textState);
    let reversion = await predictiveContext.accept(suggestionApply);

    // Test setup complete.

    // Check the assertion object (from end of 'suggestion application' checks) - verify setup.
    assert.isOk(reversion);

    // Reversion IDs are inverses of the applied suggestion.
    // This is the important link used to rewind the context when reverting suggestions.
    assert.equal(reversion.id, -suggestionApply.id);
    assert.equal(reversion.transformId, -suggestionApply.transformId);

    // Revert display strings include quotes.
    //
    // We could check this more rigorously by importing from @keymanapp/models-wordbreakers for
    // the default quotes.
    assert.isTrue(reversion.displayAs.includes(previousTextState.getText()));
    assert.equal(reversion.displayAs.length, previousTextState.getText().length + 2); // +2:  opening + closing quotes.

    // Fire away!  Time to apply the reversion.
    previousTextState = Mock.from(textState);
    let returnValue = predictiveContext.accept(reversion);

    // 'accepting' a reversion performs a rewind; there's no need for async ops here.
    assert.isNull(returnValue); // as per the method's spec.

    // Verify that the rewind + application of reversion worked!
    let rewoundTextStateWithInput = Mock.from(revertBaseTextState); // appl
    rewoundTextStateWithInput.apply(reversion.transform); // + e
    assert.equal(rewoundTextStateWithInput.getText(), 'apple'); // For visual clarity.

    // Note:  no space appended.
    assert.equal(textState.getText(), rewoundTextStateWithInput.getText());

    // Note:  accepting a reversion will trigger a new prediction that completes
    // asynchronously, and unfortunately... we have no handle for it.

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    // Now, in order to synchronize... we rely on a Promise.  The callback is indeed
    // called synchronously.
    await new Promise((resolve) => {
      predictiveContext.once('update', resolve);
    });

    assert.equal(updateFake.callCount, 1);
    const suggestionsPostReversion = updateFake.firstCall.args[0];
    assert.deepEqual(suggestionsPostReversion.map((obj) => obj.displayAs), ['reverted']);
  });
});