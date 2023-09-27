import {
  gestures,
  GestureModelDefs
} from '@keymanapp/gesture-recognizer';

import {
  Codes,
  Keyboard
} from '@keymanapp/keyboard-processor';

import { type KeyElement } from '../../keyElement.js';

import specs = gestures.specs;

/**
 * Defines the set of gestures appropriate for use with the specified Keyman keyboard.
 * @param keyboard
 * @returns
 */
export function modelSetForKeyboard(keyboard: Keyboard): GestureModelDefs<KeyElement> {
  // TODO:  keyboard-specific config stuff
  // if `null`, assume a no-flick keyboard (assuming our default layout has no flicks)

  // Idea:  if we want to get fancy, we could detect if the keyboard even _supports_ some of
  // the less common gestures and just... not include the model if it doesn't.  Should only
  // do that if it's computationally "cheap", though.
  return {
    gestures: [
      // TODO:  some, if not all, will probably utilize methods, rather than constant definitions.
      // But, this should be fine for a first-pass integration attempt.
      LongpressModel,
      MultitapModel,
      SimpleTapModel,
      SubkeySelectModel,
      ModipressStartModel,
      ModipressEndModel
    ],
    sets: {
      default: [LongpressModel.id, SimpleTapModel.id, ModipressStartModel.id],
      modipress: [LongpressModel.id, SimpleTapModel.id], // no nested modipressing
      none: []
    }
  }
}

// #region Definition of models for paths comprising gesture-stage models

type ContactModel = specs.ContactModel<KeyElement>;

export const InstantContactRejectionModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'reject',
  pathModel: {
    evaluate: (path) => 'resolve'
  }
}

export const InstantContactResolutionModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => 'resolve'
  }
}

export const LongpressDistanceThreshold = 10;
export const MainContactLongpressSourceModel: ContactModel = {
  itemChangeAction: 'reject',
  itemPriority: 0,
  pathResolutionAction: 'resolve',
  timer: {
    duration: 500,
    expectedResult: true
  },
  pathModel: {
    evaluate: (path) => {
      const stats = path.stats;
      if(stats.rawDistance > LongpressDistanceThreshold) {
        return 'reject';
      }

      if(path.isComplete) {
        return 'reject';
      }
    }
  }
};

export const LongpressFlickDistanceThreshold = 6;
export const MainContactLongpressSourceModelWithShortcut: ContactModel = {
  ...MainContactLongpressSourceModel,
  pathModel: {
    evaluate: (path) => {
      const stats = path.stats;

      // Adds up-flick support!
      if(stats.rawDistance > LongpressFlickDistanceThreshold && stats.cardinalDirection == 'n') {
        return 'resolve';
      }

      return MainContactLongpressSourceModel.pathModel.evaluate(path);
    }
  }
}

export const ModipressContactStartModel: ContactModel = {
  itemPriority: -1,
  pathResolutionAction: 'resolve',
  pathModel: {
    // Consideration of whether the underlying item supports the corresponding
    // gesture will be handled elsewhere.
    evaluate: (path) => 'resolve'
  }
}

export const ModipressContactEndModel: ContactModel = {
  itemPriority: -1,
  itemChangeAction: 'resolve',
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete) {
        return 'resolve';
      }
    }
  }
}

export const SimpleTapContactModel: ContactModel = {
  itemPriority: 0,
  itemChangeAction: 'reject',
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete && !path.wasCancelled) {
        return 'resolve';
      }
    }
  }
}

export const SubkeySelectContactModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete && !path.wasCancelled) {
        return 'resolve';
      }
    }
  }
}
// #endregion

// #region Gesture-stage model definitions
type GestureModel = specs.GestureModel<KeyElement>;

// TODO:  customization of the gesture models depending upon properties of the keyboard.
// - has flicks?  no longpress shortcut, also no longpress reset(?)
// - modipress:  keyboard-specific modifier keys - which may require inspection of a
//   key's properties.

// Is kind of a mix of the two longpress styles.
export const LongpressModel: GestureModel = {
  id: 'longpress',
  resolutionPriority: 0,
  itemPriority: 0,
  contacts: [
    {
      model: {
        // Is the version without the up-flick shortcut.
        ...MainContactLongpressSourceModel,
        itemPriority: 1,
        pathInheritance: 'chop'
      },
      endOnResolve: false
    }, {
      model: InstantContactRejectionModel
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
        ...SimpleTapContactModel,
        itemPriority: 1,
        pathInheritance: 'reject',
        allowsInitialState(incomingSample, comparisonSample, baseItem) {
          // By default, the state token is set to whatever the current layer is for a source.
          //
          // So, if the first tap of a key swaps layers, the second tap will be on the wrong layer and
          // thus have a different state token.  This is the perfect place to detect and correct that.
          if(comparisonSample.stateToken != incomingSample.stateToken) {
            incomingSample.stateToken = comparisonSample.stateToken;

            // TODO: specialized item lookup required here for proper 'correction', corresponding to
            // the owning VisualKeyboard.  That rigging doesn't exist quite yet, at the time of writing this.
            incomingSample.item = undefined;
          }
          return incomingSample.item == baseItem;
        },
      },
      endOnResolve: true
    }, {
      model: InstantContactResolutionModel
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
        ...SimpleTapContactModel,
        pathInheritance: 'chop',
        itemPriority: 1
      },
      endOnResolve: true
    }, {
      model: InstantContactResolutionModel,
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
        ...SubkeySelectContactModel,
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
      model: InstantContactRejectionModel
    }
  ],
  resolutionAction: {
    type: 'complete',
    item: 'current'
  },
  sustainWhenNested: true
}

export const ModipressStartModel: GestureModel = {
  id: 'modipress-start',
  resolutionPriority: 5,
  itemPriority: 0,
  contacts: [
    {
      model: {
        ...ModipressContactStartModel,
        allowsInitialState(incomingSample, comparisonSample, baseItem) {
          // TODO:  needs better abstraction, probably.

          // But, to get started... we can just use a simple hardcoded approach.
          const modifierKeyIds = ['K_SHIFT', 'K_ALT', 'K_CTRL'];
          for(const modKeyId of modifierKeyIds) {
            if(baseItem.key.spec.id == modKeyId) {
              return true;
            }
          }

          return false;
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
        ...ModipressContactEndModel,
        itemChangeAction: 'reject'
      }
    }
  ],
  resolutionAction: {
    type: 'complete',
    item: 'none'
  }
}
// #endregion