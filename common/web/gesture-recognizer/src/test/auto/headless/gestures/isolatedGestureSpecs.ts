import { gestures } from '@keymanapp/gesture-recognizer';
import * as specs from './isolatedPathSpecs.js';
import specTypeDefs = gestures.specs;

type GestureModel = specTypeDefs.GestureModel<string>;

// Is kind of a mix of the two longpress styles.
export const LongpressModel: GestureModel = {
  id: 'longpress',
  resolutionPriority: 0,
  itemPriority: 0,
  contacts: [
    {
      model: {
        // Is the version without the up-flick shortcut.
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
    next: 'subkey-select',
    selectionMode: 'none',
    item: 'none'
  },
  /*
   * Note:  these actions make sense in a 'roaming-touch' context, but not when
   * flicks are also enabled.
   */
  rejectionActions: {
    item: {
      type: 'replace',
      replace: 'longpress'
    },
    path: {
      type: 'replace',
      replace: 'longpress'
    }
  }
}

export const MultitapModel: GestureModel = {
  id: 'multitap',
  resolutionPriority: 2,
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
    next: 'multitap',
    item: 'current'
  }
}

export const SimpleTapModel: GestureModel = {
  id: 'simple-tap',
  resolutionPriority: 1,
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
      model: specs.InstantResolutionModel,
      resetOnResolve: true
    }
  ],
  resolutionAction: {
    type: 'chain',
    next: 'multitap',
    item: 'current'
  },
  rejectionActions: {
    item: {
      type: 'replace',
      replace: 'simple-tap'
    }
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

export const ModipressStartModel: GestureModel = {
  id: 'modipress-start',
  resolutionPriority: 5,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...specs.ModipressStartModel,
        allowsInitialState(incomingSample, comparisonSample, baseItem) {
          const modifierKeyIds = ['shift', 'alt', 'ctrl'];
          return modifierKeyIds.indexOf(baseItem) != -1;
        },
        itemChangeAction: 'reject',
        itemPriority: 1
      }
    }
  ],
  resolutionAction: {
    type: 'chain',
    next: 'modipress-end',
    selectionMode: 'modipress',
    item: 'current' // return the modifier key ID so that we know to shift to it!
  }
}

export const ModipressEndModel: GestureModel = {
  id: 'modipress-end',
  resolutionPriority: 5,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...specs.ModipressEndModel,
        itemChangeAction: 'reject'
      }
    }
  ],
  resolutionAction: {
    type: 'complete'
  }
}