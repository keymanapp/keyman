import { assert } from 'chai';
import sinon from 'sinon';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { Worker as LMWorker } from "@keymanapp/lexical-model-layer/node";

import { LanguageProcessor, TranscriptionCache } from 'keyman/engine/main';
import { PredictionContext } from 'keyman/engine/interfaces';
import { Mock } from 'keyman/engine/js-processor';

import Suggestion = LexicalModelTypes.Suggestion;

function compileDummyModel(suggestionSets: Suggestion[][]) {
  return `
LMLayerWorker.loadModel(new models.DummyModel({
  futureSuggestions: ${JSON.stringify(suggestionSets, null, 2)},
}));
`;
}

const appleDummySuggestionSets: Suggestion[][] = [[
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
  let langProcessor: LanguageProcessor;

  beforeEach(function() {
    // @ts-ignore
    langProcessor = new LanguageProcessor(LMWorker, new TranscriptionCache());
  });

  afterEach(function() {
    langProcessor.shutdown();
  });

  it('receives predictions as they are generated', async function () {
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
    let suggestions: Suggestion[];

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
    let suggestions: Suggestion[];

    // Initialization results:  our first set of dummy suggestions.
    assert.equal(updateFake.callCount, 2);
    suggestions = updateFake.secondCall.args[0];
    assert.deepEqual(suggestions.map((obj) => obj.displayAs), ['apple', 'apply', 'apples']);
    assert.isNotOk(suggestions.find((obj) => obj.tag == 'keep'));
    assert.isNotOk(suggestions.find((obj) => obj.transform.deleteLeft != 0));

    const baseTranscription = mock.buildTranscriptionFrom(initialMock, null, true);

    // Mocking:  corresponds to the second set of mocked predictions - round 2 of
    // 'apple', 'apply', 'apples'.
    const skippedPromise = langProcessor.predict(baseTranscription, dummiedGetLayer());

    mock.insertTextBeforeCaret('e'); // appl| + e = apple
    const finalTranscription = mock.buildTranscriptionFrom(initialMock, null, true);

    // Mocking:  corresponds to the third set of mocked predictions - 'applied'.
    const expectedPromise = langProcessor.predict(finalTranscription, dummiedGetLayer());

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
    // Our reused mocking doesn't directly provide the 'keep' suggestion; we
    // need to remove it before testing for set equality.
    assert.deepEqual(suggestions, expected);
  });

  it('retrieves the most recent suggestion set when sendUpdateState is called', async function() {
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

    // The array instances may be different, but their contents should be the same instances and in the same order.
    assert.sameDeepOrderedMembers(suggestions, initialSuggestions.filter(s => s.tag != 'keep'));
  });

  it('properly applies manually-selected suggestions & related effects', async function () {
    await langProcessor.loadModel(appleDummyModel);  // await:  must fully 'configure', load script into worker.

    const predictiveContext = new PredictionContext(langProcessor, dummiedGetLayer);

    let textState = new Mock("appl", 4); // "appl|", with '|' as the caret position.

    await predictiveContext.setCurrentTarget(textState);

    let updateFake = sinon.fake();
    predictiveContext.on('update', updateFake);

    let suggestions: Suggestion[];

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

    // Apply the desired suggestion.  Also passively generates new, post-acceptance
    // suggestions, but this function itself don't provide a Promise for that...
    // hence the previous block.
    predictiveContext.accept(suggestionApply);

    assert.equal(updateFake.callCount, 1); // No new 'update' has been raised yet.

    // Check 1:  did our active text context get changed?  We DID just ask to apply
    //           a suggestion...
    assert.notEqual(textState.getText(), previousTextState.getText());
    assert.equal(textState.getText(), 'apply ');

    // Check 2:  no suggestions should be automatically triggered, so no new 'update'
    // events should occur.
    assert.equal(updateFake.callCount, 1);
  });
});
