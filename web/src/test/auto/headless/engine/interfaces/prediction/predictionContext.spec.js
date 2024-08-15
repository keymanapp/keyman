import { assert } from 'chai';
import sinon from 'sinon';

import { LanguageProcessor, TranscriptionCache } from 'keyman/engine/main';
import { PredictionContext } from 'keyman/engine/interfaces';
import { Worker as LMWorker } from "@keymanapp/lexical-model-layer/node";
import { DeviceSpec } from 'keyman/engine/keyboard';
import { Mock } from 'keyman/engine/js-processor';

function compileDummyModel(suggestionSets) {
  return `
LMLayerWorker.loadModel(new models.DummyModel({
  futureSuggestions: ${JSON.stringify(suggestionSets, null, 2)},
}));
`;
}

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
    transform: { insert: 'e', deleteLeft: 0},
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

function dummiedGetLayer() {
  return 'default';
}

describe("PredictionContext", () => {
  let worker;

  beforeEach(function() {
    worker = LMWorker.constructInstance();
  });

  afterEach(function() {
    worker.terminate();
  });

  it('receives predictions as they are generated', async function () {
    const langProcessor = new LanguageProcessor(worker, new TranscriptionCache());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const predictiveContext = new PredictionContext(langProcessor, dummiedGetLayer);

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    let mock = new Mock("appl", 4); // "appl|", with '|' as the caret position.
    const initialMock = Mock.from(mock);
    const promise = predictiveContext.setCurrentTarget(mock);

    // Initial predictive state:  no suggestions.  context.initializeState() has not yet been called.
    assert.equal(updateFake.callCount, 1);
    assert.isEmpty(updateFake.firstCall.args[0]); // should have no suggestions. (if convenient for testing)

    await promise;
    let suggestions;

    // Initialization results:  our first set of dummy suggestions.
    assert.equal(updateFake.callCount, 2);
    suggestions = updateFake.secondCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apples']);
    assert.isNotOk(suggestions.find((obj) => obj.tag == 'keep'));
    assert.isNotOk(suggestions.find((obj) => obj.transform.deleteLeft != 0));

    mock.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = mock.buildTranscriptionFrom(initialMock, null, true);
    await langProcessor.predict(transcription, dummiedGetLayer());

    // First predict call results:  our second set of dummy suggestions, the first of which includes
    // a 'keep' of the original text.
    assert.equal(updateFake.callCount, 3);
    suggestions = updateFake.thirdCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apps']);
    assert.equal(suggestions.find((obj) => obj.tag == 'keep').displayAs, 'apple');
    assert.equal(suggestions.find((obj) => obj.transform.deleteLeft != 0).displayAs, 'apps');
  });

  it('ignores outdated predictions', async function () {
    const langProcessor = new LanguageProcessor(worker, new TranscriptionCache());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const kbdProcessor = new KeyboardProcessor(deviceSpec);
    const predictiveContext = new PredictionContext(langProcessor, kbdProcessor);

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    let mock = new Mock("appl", 4); // "appl|", with '|' as the caret position.
    const initialMock = Mock.from(mock);
    const promise = predictiveContext.setCurrentTarget(mock);

    // Initial predictive state:  no suggestions.  context.initializeState() has not yet been called.
    assert.equal(updateFake.callCount, 1);
    assert.isEmpty(updateFake.firstCall.args[0]); // should have no suggestions. (if convenient for testing)

    await promise;
    let suggestions;

    // Initialization results:  our first set of dummy suggestions.
    assert.equal(updateFake.callCount, 2);
    suggestions = updateFake.secondCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apples']);
    assert.isNotOk(suggestions.find((obj) => obj.tag == 'keep'));
    assert.isNotOk(suggestions.find((obj) => obj.transform.deleteLeft != 0));

    const baseTranscription = mock.buildTranscriptionFrom(initialMock, null, true);

    // Mocking:  corresponds to the second set of mocked predictions - round 2 of
    // 'apple', 'apply', 'apples'.
    const skippedPromise = langProcessor.predict(baseTranscription, kbdProcessor.layerId);

    mock.insertTextBeforeCaret('e'); // appl| + e = apple
    const finalTranscription = mock.buildTranscriptionFrom(initialMock, null, true);

    // Mocking:  corresponds to the third set of mocked predictions - 'applied'.
    const expectedPromise = langProcessor.predict(finalTranscription, kbdProcessor.layerId);

    await Promise.all([skippedPromise, expectedPromise]);
    const expected = await expectedPromise;

    // Despite two predict calls, we should only increase the counter by ONE - we ignore
    // the 'outdated' / 'skipped' round because it could not respond before its followup.
    assert.equal(updateFake.callCount, 3);
    suggestions = updateFake.thirdCall.args[0];

    // This does re-use the apply-revert oriented mocking.
    // Should skip the (second) "apple", "apply", "apps" round, as it became outdated
    // by its following request before its response could be received.
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['“apple”', 'applied']);
    assert.equal(suggestions.find((obj) => obj.tag == 'keep').displayAs, '“apple”');
    assert.equal(suggestions.find((obj) => obj.transform.deleteLeft != 0).displayAs, 'applied');
    // Our reused mocking doesn't directly provide the 'keep' suggestion; we
    // need to remove it before testing for set equality.
    assert.deepEqual(suggestions.splice(1), expected);
  });

  it('sendUpdateState retrieves the most recent suggestion set', async function() {
    const langProcessor = new LanguageProcessor(worker, new TranscriptionCache());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const predictiveContext = new PredictionContext(langProcessor, dummiedGetLayer);

    let mock = new Mock("appl", 4); // "appl|", with '|' as the caret position.
    const initialSuggestions = await predictiveContext.setCurrentTarget(mock);

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);
    predictiveContext.sendUpdateEvent(); // Allows external code to request a re-retrieval of current suggestion state.

    // Now, let's initialize the predictive state - this will load the initial suggestions.
    let suggestions;

    // Initialization results:  our first set of dummy suggestions.
    assert.isTrue(updateFake.calledOnce);
    suggestions = updateFake.firstCall.args[0];
    assert.deepEqual(suggestions, initialSuggestions);

    // The array instances may be different, but their contents should be the same instances and in the same order.
    assert.sameOrderedMembers(suggestions, initialSuggestions);
  });

  it('suggestion application logic & triggered effects', async function () {
    const langProcessor = new LanguageProcessor(worker, new TranscriptionCache());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const predictiveContext = new PredictionContext(langProcessor, dummiedGetLayer);

    let textState = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    await predictiveContext.setCurrentTarget(textState);

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    let suggestions;

    let previousTextState = Mock.from(textState);
    textState.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = textState.buildTranscriptionFrom(previousTextState, null, true);
    await langProcessor.predict(transcription, dummiedGetLayer());

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

    // For awaiting the suggestions generated upon applying our desired suggestion.
    // We aren't given a direct Promise for that, but we can construct one this way.
    let postApplySuggestions = new Promise((resolve) => {
      predictiveContext.once('update', resolve);
    });

    // Apply the desired suggestion.  Also passively generates new, post-acceptance
    // suggestions, but this function itself don't provide a Promise for that...
    // hence the previous block.
    let promiseForApplyReversion = predictiveContext.accept(suggestionApply);

    assert.equal(updateFake.callCount, 1); // No new 'update' has been raised yet.

    // Check 1:  did our active text context get changed?  We DID just ask to apply
    //           a suggestion...
    assert.notEqual(textState.getText(), previousTextState.getText());
    assert.equal(textState.getText(), 'apply ');

    let reversion = await promiseForApplyReversion;
    await postApplySuggestions;

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

  it('reversion application logic & triggered effects', async function () {
    const langProcessor = new LanguageProcessor(worker, new TranscriptionCache());
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const predictiveContext = new PredictionContext(langProcessor, dummiedGetLayer);

    let textState = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    // Test setup - return to the state at the end of the prior-defined unit test ('suggestion application...')

    await predictiveContext.setCurrentTarget(textState);

    // This is the point in time that a reversion operation will rewind the context to.
    const revertBaseTextState = Mock.from(textState);
    textState.insertTextBeforeCaret('e'); // appl| + e = apple
    let transcription = textState.buildTranscriptionFrom(revertBaseTextState, null, true);

    let suggestionCaptureFake = sinon.fake();
    predictiveContext.once('update', suggestionCaptureFake);
    await langProcessor.predict(transcription, dummiedGetLayer());

    // We need to capture the suggestion we wish to apply.  We could hardcode a forced
    // value, but that might become brittle in the long-term.
    const originalSuggestionSet = suggestionCaptureFake.firstCall.args[0];
    const suggestionApply = originalSuggestionSet.find((obj) => obj.displayAs == 'apply');
    assert.isOk(suggestionApply);

    let previousTextState = Mock.from(textState);

    // For awaiting the suggestions generated upon applying our desired suggestion.
    // We aren't given a direct Promise for that, but we can construct one this way.
    let postApplySuggestions = new Promise((resolve) => {
      predictiveContext.once('update', resolve);
    });

    let reversion = await predictiveContext.accept(suggestionApply);
    await postApplySuggestions;

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

    // Since the test uses a separate thread via Worker, make sure to set up any important event handlers
    // before we request the reversion.
    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    // Now, in order to synchronize... we rely on a Promise.  The callback is indeed
    // called synchronously.
    let postRevertSuggestions = new Promise((resolve) => {
      predictiveContext.once('update', resolve);
    });

    // And now, apply the reversion itself.
    let returnValue = await predictiveContext.accept(reversion);

    // 'accepting' a reversion performs a rewind; there's no need for async ops here.
    assert.isNull(returnValue); // as per the method's spec.

    // Verify that the rewind + application of reversion worked!
    let rewoundTextStateWithInput = Mock.from(revertBaseTextState); // appl
    rewoundTextStateWithInput.apply(reversion.transform); // + e
    assert.equal(rewoundTextStateWithInput.getText(), 'apple'); // For visual clarity.

    // Note:  no space appended.
    assert.equal(textState.getText(), rewoundTextStateWithInput.getText());

    // Re-synchronize once we've received word of the new post-reversion predictions.
    await postRevertSuggestions;

    assert.equal(updateFake.callCount, 1);
    const suggestionsPostReversion = updateFake.firstCall.args[0];
    assert.deepEqual(suggestionsPostReversion.map((obj) => obj.displayAs), ['reverted']);
  });
});
