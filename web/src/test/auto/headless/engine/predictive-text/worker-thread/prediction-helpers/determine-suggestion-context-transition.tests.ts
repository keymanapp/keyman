import { assert } from 'chai';
import sinon from 'sinon';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { ContextTracker, determineContextTransition, ModelCompositor, models } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import Suggestion = LexicalModelTypes.Suggestion;
import TrieModel = models.TrieModel;
import Transform = LexicalModelTypes.Transform;

const plainApplyCasing: CasingFunction = function(caseToApply, text) {
  switch(caseToApply) {
    case 'lower':
      return text.toLowerCase();
    case 'upper':
      return text.toUpperCase();
    case 'initial':
      return plainApplyCasing('upper', text.charAt(0)) . concat(text.substring(1));
    default:
      return text;
  }
};

const plainCasedModel = new TrieModel(
  jsonFixture('models/tries/english-1000'), {
    languageUsesCasing: true,
    applyCasing: plainApplyCasing,
    wordBreaker: defaultBreaker,
    searchTermToKey: function(text: string) {
      // We're dealing with very simple English text; no need to normalize or remove diacritics here.
      return plainApplyCasing('lower', text);
    }
  }
);

describe('determineContextTransition', () => {
  it('reuses last transition when empty input is received', () => {
    const baseContext: Context = {
      left: 'this is for tech',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const tracker = new ContextTracker(plainCasedModel, baseContext, 0, plainCasedModel.configuration);

    const targetContext: Context = {
      left: 'this is for tech',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const inputDistribution: Distribution<Transform> = [{sample: {insert: '', deleteLeft: 0, id: 1}, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const transition = determineContextTransition(
        tracker,
        tracker.latest.base,
        targetContext,
        inputDistribution
      );

      assert.isOk(transition);
      assert.isFalse(warningEmitterSpy.called);
      assert.equal(transition, tracker.latest);
      assert.equal(transition.final, tracker.latest.base);
      assert.equal(transition.final, transition.base);
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('computes new transition when receiving an output key without corrections enabled', () => {
    const baseContext: Context = {
      left: 'this is for tech',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const tracker = new ContextTracker(plainCasedModel, baseContext, 0, plainCasedModel.configuration);

    const targetContext: Context = {
      left: 'this is for techn',
      startOfBuffer: true,
      endOfBuffer: true
    };

    const inputDistribution: Distribution<Transform> = [{sample: {insert: 'n', deleteLeft: 0, id: 1}, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const transition = determineContextTransition(
        tracker,
        tracker.latest.base,
        baseContext,
        inputDistribution
      );

      assert.isOk(transition);
      assert.equal(transition, tracker.latest);
      assert.isFalse(warningEmitterSpy.called);
      assert.equal(transition.final.tokenizations.length, 1);
      assert.sameOrderedMembers(transition.final.tokenizations[0].exampleInput, ['this', ' ', 'is', ' ', 'for', ' ', 'techn']);
      assert.isOk(transition.final.tokenizations[0].transitionEdits);
      assert.equal(transition.final.context.left, targetContext.left);
      assert.equal(transition.final.context.right ?? "", targetContext.right ?? "");
      assert.sameDeepOrderedMembers(transition.inputDistribution, inputDistribution);
      assert.isNotOk(transition.final.tokenizations[0].taillessTrueKeystroke);
      assert.equal(transition.transitionId, 1);
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('reuses last transition when processing input that triggered autocorrect', () => {
    const compositor = new ModelCompositor(plainCasedModel, true);

    const baseContext: Context = {
      left: 'this is for test',
      startOfBuffer: true,
      endOfBuffer: true
    };
    compositor.initContextTracker(baseContext, 0);
    const tracker = compositor.contextTracker;

    const baseTransition = tracker.latest;
    const pred_testing: Suggestion = {
      transform: {
        insert: 'testing',
        deleteLeft: 4,
        id: 0
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0
      },
      transformId: 0,
      displayAs: 'testing'
    };
    baseTransition.final.suggestions = [pred_testing];

    const applied_testing = {
      ...pred_testing,
      appendedTransform: {
        insert: '.',
        deleteLeft: 0,
        id: 1
      }
    };

    compositor.acceptSuggestion(applied_testing, baseContext, { insert: '', deleteLeft: 0 });
    const acceptingTransition = tracker.latest;

    const inputDistribution: Distribution<Transform> = [{sample: applied_testing.appendedTransform, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const reuseTransition = determineContextTransition(
        tracker,
        tracker.latest.base, {
          left: 'this is for testing',
          startOfBuffer: true,
          endOfBuffer: true
        },
        inputDistribution
      );

      assert.isOk(baseTransition);
      assert.isFalse(warningEmitterSpy.called);
      assert.strictEqual(reuseTransition, acceptingTransition, 'Did not reuse transition as expected');
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('preserves applied suggestion ids on unedited tokens when adding text', () => {
    const compositor = new ModelCompositor(plainCasedModel, true);

    const baseContext: Context = {
      left: 'this is for test',
      startOfBuffer: true,
      endOfBuffer: true
    };
    compositor.initContextTracker(baseContext, 0);
    const tracker = compositor.contextTracker;

    const baseTransition = tracker.latest;
    const pred_testing: Suggestion = {
      transform: {
        insert: 'testing',
        deleteLeft: 4,
        id: 1
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 0,
      displayAs: 'testing'
    };
    baseTransition.final.suggestions = [pred_testing];

    compositor.acceptSuggestion(pred_testing, baseContext, { insert: '', deleteLeft: 0 });

    const inputDistribution: Distribution<Transform> = [{sample: { insert: 'a', deleteLeft: 0, id: 5 }, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const extendingTransition = determineContextTransition(
        tracker,
        tracker.latest.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true
        },
        inputDistribution
      );

      assert.isOk(baseTransition);
      assert.isFalse(warningEmitterSpy.called);
      assert.notEqual(extendingTransition, baseTransition);

      // These values support delayed reversions.
      assert.equal(extendingTransition.final.tokenizations.length, 1);
      assert.equal(extendingTransition.final.tokenizations[0].tokens[6].appliedTransitionId, pred_testing.transformId);
      assert.equal(extendingTransition.final.tokenizations[0].tokens[7].appliedTransitionId, pred_testing.transformId);

      // We start a new token here, rather than continue (and/or replace) an old one;
      // this shouldn't be set here yet.
      assert.isUndefined(extendingTransition.revertableTransitionId);
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('indicates delayed reversion possibility when deleting up to suggestion-appended whitespace', () => {
    const compositor = new ModelCompositor(plainCasedModel, true);

    const baseContext: Context = {
      left: 'this is for test',
      startOfBuffer: true,
      endOfBuffer: true
    };
    compositor.initContextTracker(baseContext, 0);
    const tracker = compositor.contextTracker;

    const baseTransition = tracker.latest;
    const pred_testing: Suggestion = {
      transform: {
        insert: 'testing',
        deleteLeft: 4,
        id: 1
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 0,
      displayAs: 'testing'
    };
    baseTransition.final.suggestions = [pred_testing];

    compositor.acceptSuggestion(pred_testing, baseContext, { insert: '', deleteLeft: 0 });

    const inputDistribution: Distribution<Transform> = [{sample: { insert: 'a', deleteLeft: 0, id: 5 }, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const extendingTransition = determineContextTransition(
        tracker,
        tracker.latest.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true
        },
        inputDistribution
      );
      tracker.latest = extendingTransition;

      const extensionDeletingTransition = determineContextTransition(
        tracker,
        extendingTransition.final, {
          left: 'this is for testing a',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );

      assert.equal(extensionDeletingTransition.revertableTransitionId, pred_testing.transformId);
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('indicates delayed reversion possibility when deleting up to end of applied suggestion', () => {
    const compositor = new ModelCompositor(plainCasedModel, true);

    const baseContext: Context = {
      left: 'this is for test',
      startOfBuffer: true,
      endOfBuffer: true
    };
    compositor.initContextTracker(baseContext, 0);
    const tracker = compositor.contextTracker;

    const baseTransition = tracker.latest;
    const pred_testing: Suggestion = {
      transform: {
        insert: 'testing',
        deleteLeft: 4,
        id: 1
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 0,
      displayAs: 'testing'
    };
    baseTransition.final.suggestions = [pred_testing];

    compositor.acceptSuggestion(pred_testing, baseContext, { insert: '', deleteLeft: 0 });

    const inputDistribution: Distribution<Transform> = [{sample: { insert: 'a', deleteLeft: 0, id: 5 }, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const extendingTransition = determineContextTransition(
        tracker,
        tracker.latest.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true
        },
        inputDistribution
      );
      tracker.latest = extendingTransition;

      const extensionDeletingTransition = determineContextTransition(
        tracker,
        extendingTransition.final, {
          left: 'this is for testing a',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );
      tracker.latest = extensionDeletingTransition;

      const appendDeletingTransition = determineContextTransition(
        tracker,
        extensionDeletingTransition.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );

      assert.equal(appendDeletingTransition.revertableTransitionId, pred_testing.transformId);
    } finally {
      warningEmitterSpy.restore();
    }
  });

  it('does not indicate delayed reversion possibility after deleting part of applied suggestion', () => {
    const compositor = new ModelCompositor(plainCasedModel, true);

    const baseContext: Context = {
      left: 'this is for test',
      startOfBuffer: true,
      endOfBuffer: true
    };
    compositor.initContextTracker(baseContext, 0);
    const tracker = compositor.contextTracker;

    const baseTransition = tracker.latest;
    const pred_testing: Suggestion = {
      transform: {
        insert: 'testing',
        deleteLeft: 4,
        id: 1
      },
      appendedTransform: {
        insert: ' ',
        deleteLeft: 0,
        id: 2
      },
      transformId: 0,
      displayAs: 'testing'
    };
    baseTransition.final.suggestions = [pred_testing];

    compositor.acceptSuggestion(pred_testing, baseContext, { insert: '', deleteLeft: 0 });

    const inputDistribution: Distribution<Transform> = [{sample: { insert: 'a', deleteLeft: 0, id: 5 }, p: 1}];

    const warningEmitterSpy = sinon.spy(console, 'warn');
    try {
      const extendingTransition = determineContextTransition(
        tracker,
        tracker.latest.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true
        },
        inputDistribution
      );
      tracker.latest = extendingTransition;

      const extensionDeletingTransition = determineContextTransition(
        tracker,
        extendingTransition.final, {
          left: 'this is for testing a',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );
      tracker.latest = extensionDeletingTransition;

      const appendDeletingTransition = determineContextTransition(
        tracker,
        extensionDeletingTransition.final, {
          left: 'this is for testing ',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );
      tracker.latest = appendDeletingTransition;

      const editingBkspTransition = determineContextTransition(
        tracker,
        appendDeletingTransition.final, {
          left: 'this is for testing',
          startOfBuffer: true,
          endOfBuffer: true,
        },
        // Backspace over the input 'a', leading to the end of the whitespace after testing.
        [{sample: { insert: '', deleteLeft: 1 }, p: 1}]
      );

      assert.isUndefined(editingBkspTransition.revertableTransitionId);
    } finally {
      warningEmitterSpy.restore();
    }
  });
});