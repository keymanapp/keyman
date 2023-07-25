import { gestures } from '@keymanapp/gesture-recognizer';
import * as specs from './isolatedPathSpecs.js';
import specTypeDefs = gestures.specs;

type GestureModel = specTypeDefs.GestureModel<string>;

export const LongpressModel: GestureModel = {
  id: 'longpress',
  resolutionPriority: 0,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...specs.MainLongpressSourceModel,
        itemPriority: 1,
        pathInheritance: 'chop'
      },
      endOnResolve: false
    }, {
      model: specs.InstantRejectionModel
    }
  ],
  resolutionAction: {
    type: 'chain',
    next: 'subkeyselect',
    item: 'none'
  },
  rejectionActions: {
    item: {
      type: 'optional-chain',
      allowNext: 'longpress'
    },
    path: {
      type: 'optional-chain',
      allowNext: 'longpress'
    }
  }
}

export const MultitapModel: GestureModel = {
  id: 'multitap',
  resolutionPriority: 1,
  itemPriority: 1,
  contacts: [
    {
      model: {
        ...specs.SimpleTapModel,
        itemPriority: 1,
        pathInheritance: 'reject',
        allowsInitialState(incomingSample, comparisonSample, baseItem) {
          return incomingSample.item == baseItem;
        },
      },
      endOnResolve: true
    }, {
      model: specs.InstantResolutionModel
    }
  ],
  sustainTimer: {
    duration: 500,
    expectedResult: false,
    baseItem: 'base'
  },
  resolutionAction: {
    type: 'chain',
    next: 'multitap'
  }
}

export const SimpleTapModel: GestureModel = {
  id: 'simple-tap',
  resolutionPriority: 0,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...specs.SimpleTapModel,
        pathInheritance: 'chop',
        itemPriority: 1
      },
      endOnResolve: true
    }, {
      model: specs.InstantResolutionModel
    }
  ],
  resolutionAction: {
    type: 'optional-chain',
    allowNext: 'multitap'
  }
}

export const SubkeySelectModel: GestureModel = {
  id: 'subkey-select',
  resolutionPriority: 0,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...specs.SubkeySelectModel,
        pathInheritance: 'full',
        itemPriority: 1
      },
      endOnResolve: true,
      endOnReject: true
    }, {
      // A second touch while selecting a subkey will trigger instant cancellation
      // of subkey mode.  (With this setting in place, anyway.)
      //
      // Might not be ideal for actual production... but it does have benefits for
      // unit testing the gesture-matching engine.
      model: specs.InstantRejectionModel
    }
  ],
  resolutionAction: {
    type: 'complete',
    item: 'current'
  }
}