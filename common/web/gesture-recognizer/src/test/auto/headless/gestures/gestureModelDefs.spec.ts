import { assert } from 'chai';

import { GestureModelDefs, gestures } from '@keymanapp/gesture-recognizer';

const getGestureModel    = gestures.specs.getGestureModel;
const getGestureModelSet = gestures.specs.getGestureModelSet;

import {
  LongpressModel,
  MultitapModel,
  SimpleTapModel,
  SubkeySelectModel
} from './isolatedGestureSpecs.js';

const TestGestureModelDefinitions: GestureModelDefs<string> = {
  gestures: [
    LongpressModel,
    MultitapModel,
    SimpleTapModel,
    SubkeySelectModel,
    // TODO:  add something for a starting modipress.
  ],
  sets: {
    default: [LongpressModel.id, SimpleTapModel.id, /* TODO: add a 'starting modipress' model */],
    // TODO:  modipress: [LongpressModel.id, SimpleTapModel.id], // no nested modipressing
    malformed: [LongpressModel.id, 'unavailable-model'],
    // For subkey-select mode - no new gestures should be allowed to start.
    none: []
  }
}

describe("Gesture model definitions", () => {
  it('getGestureModel', () => {
    assert.equal(LongpressModel, getGestureModel(TestGestureModelDefinitions, LongpressModel.id));
    assert.throws(() => getGestureModel(TestGestureModelDefinitions, "unavailable-model"));
  });

  it('getGestureModelSet', () => {
    assert.sameMembers(getGestureModelSet(TestGestureModelDefinitions, 'default'), [LongpressModel, SimpleTapModel]);
    // An empty set is fine; this can disable detection of new gestures in certain conditions.
    assert.sameMembers(getGestureModelSet(TestGestureModelDefinitions, 'none'), []);

    // These two, on the other hand, should always be considered errors.
    assert.throws(() => getGestureModelSet(TestGestureModelDefinitions, 'malformed'));
    assert.throws(() => getGestureModelSet(TestGestureModelDefinitions, 'unavailable-set'));
  });
});