import { assert } from 'chai';
import sinon from 'sinon';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { default as defaultBreaker } from '@keymanapp/models-wordbreakers';
import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';

import { ContextTracker, matchBaseContextState, models } from "@keymanapp/lm-worker/test-index";

import CasingFunction = LexicalModelTypes.CasingFunction;
import Context = LexicalModelTypes.Context;
import TrieModel = models.TrieModel;

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

describe('matchBaseContextState', () => {
  it('handles simplest common-case well:  context aligns with result of last transition', () => {
    const context: Context = {
      left: 'this is tech',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const contextTracker = new ContextTracker(plainCasedModel, context, 1, plainCasedModel.configuration);
    const baseState = contextTracker.latest.final;

    const transition = baseState.analyzeTransition({
      left: 'this is tech',
      startOfBuffer: true,
      endOfBuffer: true
    }, [{sample: { insert: 'n', deleteLeft: 0 }, p: 1}]);
    contextTracker.latest = transition;

    const warningSpy = sinon.spy(console, 'warn');
    try {
      const matchedState = matchBaseContextState(contextTracker, {
        left: 'this is techn',
        startOfBuffer: true,
        endOfBuffer: true
      }, 1);

      assert.isFalse(warningSpy.called);
      assert.equal(matchedState, transition.final);
    } finally {
      warningSpy.restore();
    }
  });

  it('handles sliding common-case well:  context aligns with result of last transition', () => {
    const context: Context = {
      left: 'a lot of test here might cause the sliding context window to shi', // 64 chars
      startOfBuffer: true,
      endOfBuffer: true
    };
    const contextTracker = new ContextTracker(plainCasedModel, context, 1, plainCasedModel.configuration);
    const baseState = contextTracker.latest.final;

    const transition = baseState.analyzeTransition({
      left: 'a lot of test here might cause the sliding context window to shi',
      startOfBuffer: false, // We're sliding now.
      endOfBuffer: true
    }, [{sample: { insert: 'f', deleteLeft: 0 }, p: 1}]);
    contextTracker.latest = transition;

    const warningSpy = sinon.spy(console, 'warn');
    try {
      const matchedState = matchBaseContextState(contextTracker, {
        left: ' lot of test here might cause the sliding context window to shif',
        startOfBuffer: false,
        endOfBuffer: true
      }, 1);

      assert.isFalse(warningSpy.called);
      assert.equal(matchedState, transition.final);
    } finally {
      warningSpy.restore();
    }
  });

  it("handles multitap scenarios - uses prior transition's base state instead of final", () => {
    const context: Context = {
      left: 'meet you at the caf',
      startOfBuffer: true,
      endOfBuffer: true
    };
    const contextTracker = new ContextTracker(plainCasedModel, context, 1, plainCasedModel.configuration);
    const baseState = contextTracker.latest.final;

    const transition = baseState.analyzeTransition({
      left: 'meet you at the caf',
      startOfBuffer: true,
      endOfBuffer: true
    }, [{sample: { insert: 'e', deleteLeft: 0 }, p: 1}]);
    contextTracker.latest = transition;

    const matchedState = matchBaseContextState(contextTracker, {
      left: 'meet you at the caf',  // with input:  é
      startOfBuffer: true,
      endOfBuffer: true
    }, 1);

    assert.equal(matchedState, transition.base);
  });

  it('handles sliding context with large jump from applying a suggestion', () => {
    const context: Context = {
      left: 'ot of test here might cause the sliding context window to shift ', // 64 chars
      startOfBuffer: false,
      endOfBuffer: true
    };
    const contextTracker = new ContextTracker(plainCasedModel, context, 1, plainCasedModel.configuration);
    const baseState = contextTracker.latest.final;

    const transition = baseState.analyzeTransition({
      left: 'ot of test here might cause the sliding context window to shift ',
      startOfBuffer: false, // We're sliding now.
      endOfBuffer: true
    }, [{sample: { insert: 'dramatically', deleteLeft: 0 }, p: 1}], true);
    contextTracker.latest = transition;

    const warningSpy = sinon.spy(console, 'warn');
    try {
      const matchedState = matchBaseContextState(contextTracker, {
        left: 'ere might cause the sliding context window to shift dramatically',
        startOfBuffer: false,
        endOfBuffer: true
      }, 1);

      assert.isFalse(warningSpy.called);
      assert.equal(matchedState, transition.final);
    } finally {
      warningSpy.restore();
    }
  });

it('handles backward-sliding context after big delete', () => {
    const context: Context = {
      left: 'ere might cause the sliding context window to shift dramatically', // 64 chars
      startOfBuffer: false,
      endOfBuffer: true
    };
    const contextTracker = new ContextTracker(plainCasedModel, context, 1, plainCasedModel.configuration);
    const baseState = contextTracker.latest.final;

    const transition = baseState.analyzeTransition({
      left: 'ere might cause the sliding context window to shift dramatically',
      startOfBuffer: false, // We're sliding now.
      endOfBuffer: true
    }, [{sample: { insert: '', deleteLeft: 'dramatically'.length }, p: 1}], true);
    contextTracker.latest = transition;

    const warningSpy = sinon.spy(console, 'warn');
    try {
      const matchedState = matchBaseContextState(contextTracker, {
        left: 'ot of test here might cause the sliding context window to shift ',
        startOfBuffer: false,
        endOfBuffer: true
      }, 1);

      assert.isFalse(warningSpy.called);
      assert.equal(matchedState, transition.final);
    } finally {
      warningSpy.restore();
    }
  });
});