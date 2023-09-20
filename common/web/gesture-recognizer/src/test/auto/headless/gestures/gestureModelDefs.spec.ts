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
    malformed: [LongpressModel.id, 'unavailable-model']
  }
}

describe("Gesture model definitions", () => {
  it('getGestureModel', () => {
    assert.equal(LongpressModel, getGestureModel(TestGestureModelDefinitions, LongpressModel.id));
    assert.throws(() => getGestureModel(TestGestureModelDefinitions, "unavailable-model"));
  });

  it('getGestureModelSet', () => {
    assert.sameMembers([LongpressModel, SimpleTapModel], getGestureModelSet(TestGestureModelDefinitions, 'default'));
    assert.throws(() => getGestureModelSet(TestGestureModelDefinitions, 'malformed'));
    assert.throws(() => getGestureModelSet(TestGestureModelDefinitions, 'unavailable-set'));
  });
});