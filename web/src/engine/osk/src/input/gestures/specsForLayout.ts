import {
  gestures,
  GestureModelDefs,
  InputSample
} from '@keymanapp/gesture-recognizer';


// // TODO:  can only use directly once updates from the base feature-branch propagate through ancestors.
// import {
//   type TouchLayout
// } from '@keymanapp/common-types';
// import type ButtonClasses = TouchLayout.TouchLayoutKeySp;

import {
  deepCopy
} from '@keymanapp/keyboard-processor';
import OSKLayerGroup from '../../keyboard-layout/oskLayerGroup.js';

import { type KeyElement } from '../../keyElement.js';

import specs = gestures.specs;

export interface GestureParams {
  longpress: {
    /**
     * Allows enabling or disabling the longpress up-flick shortcut for keyboards that do not
     * include any defined flick gestures.
     *
     * Will be ignored (in favor of `false`) for keyboards that do have defined flicks.
     */
    permitFlick: boolean,

    /**
     * The minimum _net_ distance traveled before a longpress flick-shortcut will trigger.
     */
    flickDist: number,

    /**
     * The maximum amount of raw-distance movement allowed for a longpress before it is
     * aborted in favor of roaming touch and/or a timer reset.  Only applied when
     * roaming touch behaviors are permitted / when flicks are disabled.
     *
     * This threshold is not applied if the movement meets all criteria to trigger a
     * flick-shortcut but the distance traveled.
     */
    noiseTolerance: number,

    /**
     * The duration (in ms) that the base key must be held before the subkey menu will be
     * displayed should the up-flick shortcut not be utilized.
     */
    waitLength: number
  },
  multitap: {
    /**
     * The duration (in ms) permitted between taps.  Taps with a greater time interval
     * between them will be considered separate.
     */
    waitLength: number;

    /**
     * The duration (in ms) permitted for a tap to be held before it will no longer
     * be considered part of a multitap.
     */
    holdLength: number;
  },
  flick: {
    /**
     * The minimum _net_ touch-path distance that must be traversed to "lock in" on
     * a flick gesture.  When keys support both longpresses and flicks, this distance
     * must be traversed before the longpress timer elapses.
     *
     * This distance does _not_ trigger an actual flick keystroke; it is intended to
     * ensure that paths meeting this criteria have the chance to meet the full
     * distance criteria for a flick even if longpresses are also supported on a key.
     */
    startDist: number,

    /**
     * The minimum _net_ touch-path distance that must be traversed for flicks
     * to be triggered.
     */
    triggerDist: number
  }
}

export const DEFAULT_GESTURE_PARAMS: GestureParams = {
  longpress: {
    permitFlick: true,
    flickDist: 5,
    waitLength: 500,
    noiseTolerance: 10
  },
  multitap: {
    waitLength: 500,
    holdLength: 500
  },
  flick: {
    startDist: 10,
    triggerDist: 40 // should probably be based on row-height?
  }
}

export function keySupportsModipress(key: KeyElement) {
  // Future enhancement idea:  allow some extra way for a key to say "hi, I'm modipressable".
  const keySpec = key.key.spec;
  const modifierKeyIds = ['K_SHIFT', 'K_ALT', 'K_CTRL', 'K_NUMERALS', 'K_SYMBOLS', 'K_CURRENCIES'];
  for(const modKeyId of modifierKeyIds) {
    if(keySpec.id == modKeyId) {
      return true;
    }
  }

  // Allows special-formatted keys with a next-layer property to be modipressable.
  return keySpec.sp > 0 /* ButtonClasses.normal */ && keySpec.sp < 8 /* ButtonClasses.deadkey */ && !!keySpec.nextlayer;
}

/**
 * Defines the set of gestures appropriate for use with the specified Keyman keyboard.
 * @param layerGroup  The active keyboard's layer group
 * @param params      A set of tweakable gesture parameters.  It will be closure-captured
 *                    and referred to by reference; changes to its values will take
 *                    immediate effect during gesture processing.
 * @returns
 */
export function gestureSetForLayout(layerGroup: OSKLayerGroup, params: GestureParams): GestureModelDefs<KeyElement, string> {
  const layout = layerGroup.spec;

  // To be used among the `allowsInitialState` contact-model specifications as needed.
  const gestureKeyFilter = (key: KeyElement, gestureId: string) => {
    if(!key) {
      return false;
    }

    const keySpec = key.key.spec;
    switch(gestureId) {
      case 'modipress-start':
        return keySupportsModipress(key);
      case 'special-key-start':
        return ['K_LOPT', 'K_ROPT', 'K_BKSP'].indexOf(keySpec.baseKeyID) != -1;
      case 'longpress':
        return !!keySpec.sk;
      case 'multitap-start':
      case 'modipress-multitap-start':
        if(layout.hasMultitaps) {
          return !!keySpec.multitap;
        } else {
          return false;
        }
      case 'flick-start':
        // This is a gesture-start check; there won't yet be any directional info available.
        return !!keySpec.flick;
      default:
        return true;
    }
  };

  const _initialTapModel: GestureModel<KeyElement> = deepCopy(layout.hasFlicks ? initialTapModel(params) : initialTapModelWithReset(params));
  const simpleTapModel: GestureModel<KeyElement> = deepCopy(layout.hasFlicks ? SimpleTapModel : SimpleTapModelWithReset);
  const longpressModel: GestureModel<KeyElement> = deepCopy(layout.hasFlicks ? basicLongpressModel(params) : longpressModelWithShortcut(params));

  // #region Functions for implementing and/or extending path initial-state checks
  function withKeySpecFiltering(model: GestureModel<KeyElement>, contactIndices: number | number[]) {
    // Creates deep copies of the model specifications that are safe to customize to the
    // keyboard layout.
    model = deepCopy(model);
    const modelId = model.id;

    if(typeof contactIndices == 'number') {
      contactIndices = [contactIndices];
    }

    model.contacts.forEach((contact, index) => {
      if((contactIndices as number[]).indexOf(index) != -1) {
        const baseInitialStateCheck = contact.model.allowsInitialState ?? (() => true);

        contact.model = {
          ...contact.model,
          allowsInitialState: (sample, ancestorSample, key) => {
            return baseInitialStateCheck(sample, ancestorSample, key) && gestureKeyFilter(key, modelId);
          }
        };
      }
    });

    return model;
  }
  // #endregion

  const gestureModels = [
    withKeySpecFiltering(longpressModel, 0),
    withKeySpecFiltering(multitapStartModel(params), 0),
    multitapEndModel(params),
    _initialTapModel,
    simpleTapModel,
    withKeySpecFiltering(SpecialKeyStartModel, 0),
    SpecialKeyEndModel,
    SubkeySelectModel,
    withKeySpecFiltering(ModipressStartModel, 0),
    modipressHoldModel(params),
    ModipressEndModel,
    ModipressMultitapTransitionModel,
    withKeySpecFiltering(modipressMultitapStartModel(params), 0),
    modipressMultitapEndModel(params)
  ];

  const defaultSet = [
    longpressModel.id, _initialTapModel.id, ModipressStartModel.id, SpecialKeyStartModel.id
  ];

  if(layout.hasFlicks) {
    gestureModels.push(withKeySpecFiltering(flickStartModel(params), 0));
    gestureModels.push(flickEndModel(params));
    gestureModels.push(FlickRejectModel);

    defaultSet.push('flick-start');
  } else {
    // A post-roam version of longpress with the up-flick shortcut disabled but roaming still on.
    gestureModels.push(withKeySpecFiltering(longpressModelWithRoaming(params), 0));
  }

  return {
    gestures: gestureModels,
    sets: {
      default: defaultSet,
      modipress: defaultSet.filter((entry) => entry != ModipressStartModel.id), // no nested modipressing
      none: []
    }
  }
}

// #region Definition of models for paths comprising gesture-stage models

// Note:  as specified below, none of the raw specs actually need access to KeyElement typing.

type ContactModel = specs.ContactModel<any>;

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

export function flickStartContactModel(params: GestureParams): ContactModel {
  return {
    itemPriority: 1,
    pathModel: {
      evaluate: (path) => path.stats.netDistance > params.flick.startDist ? 'resolve' : null
    },
    pathResolutionAction: 'resolve',
    pathInheritance: 'chop'
  }
}

export function flickEndContactModel(params: GestureParams): ContactModel {
  return {
    itemPriority: 1,
    pathModel: {
      evaluate: (path) => {
        // Remove `path.isComplete` to have an instant-trigger once the distance threshold is reached.
        if(path.isComplete && path.stats.netDistance > params.flick.triggerDist) {
          // We _could_ add other criteria if desired, such as for straightness.
          // - What's the angle variance look like?
          // - or, take a regression & look at the coefficient of determination.
          return 'resolve';
        }
      }
    },
    pathResolutionAction: 'resolve',
    pathInheritance: 'full'
  }
}

export function BasicLongpressContactModel(params: GestureParams): ContactModel {
  const spec = params.longpress;

  return {
    itemChangeAction: 'reject',
    itemPriority: 0,
    pathResolutionAction: 'resolve',
    timer: {
      duration: spec.waitLength,
      expectedResult: true
    },
    pathModel: {
      evaluate: (path) => {
        if(path.isComplete) {
          return 'reject';
        }

        return null;
      }
    }
  };
}

export function LongpressContactModelWithShortcut(params: GestureParams, enabledFlicks?: false | undefined): ContactModel {
  const spec = params.longpress;
  const base = BasicLongpressContactModel(params);

  return {
    ...base,
    // We want to selectively ignore this during an up-flick.
    itemChangeAction: undefined,
    pathModel: {
      evaluate: (path) => {
        const stats = path.stats;

        /* The flick-dist threshold may be higher than the noise tolerance,
         * so we don't check the latter if we're in the right direction for
         * the flick shortcut to trigger.
         *
         * The 'indexOf' allows 'n', 'nw', and 'ne' - approx 67.5 degrees on
         * each side of due N in total.
         */
        if((enabledFlicks ?? spec.permitFlick) && (stats.cardinalDirection?.indexOf('n') != -1 ?? false)) {
          if(stats.netDistance > spec.flickDist) {
            return 'resolve';
          }
        } else {
          // If roaming, reject (so that we restart)
          if(stats.rawDistance > spec.noiseTolerance || stats.lastSample.item != stats.initialSample.item) {
            return 'reject';
          }
        }

        if(path.isComplete) {
          return 'reject';
        }

        return null;
      }
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

export const ModipressContactHoldModel: ContactModel = {
  itemPriority: -1,
  itemChangeAction: 'resolve',
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete) {
        return 'reject';
      }
    }
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

// Note:  as specified below, most of the raw specs actually need access to KeyElement typing.
// That only becomes relevant with some of the modifier functions in the `gestureSetForLayout`
// func at the top.
type GestureModel<Type> = specs.GestureModel<Type>;

// TODO:  customization of the gesture models depending upon properties of the keyboard.
// - has flicks?  no longpress shortcut, also no longpress reset(?)
// - modipress:  keyboard-specific modifier keys - which may require inspection of a
//   key's properties.

export const SpecialKeyStartModel: GestureModel<KeyElement> = {
  id: 'special-key-start',
  resolutionPriority: 0,
  contacts : [
    {
      model: {
        ...InstantContactResolutionModel,
        allowsInitialState: (incoming, dummy, baseItem) => {
          // TODO:  needs better abstraction, probably.

          // But, to get started... we can just use a simple hardcoded approach.
          const modifierKeyIds = ['K_LOPT', 'K_ROPT', 'K_BKSP'];
          for(const modKeyId of modifierKeyIds) {
            if(baseItem.key.spec.id == modKeyId) {
              return true;
            }
          }

          return false;
        }
      },
      endOnResolve: false  // keyboard-selection longpress - would be nice to not need to lift the finger
                           // in app/browser form.
    }
  ],
  resolutionAction: {
    type: 'chain',
    next: 'special-key-end',
    item: 'current'
  }
}

export const SpecialKeyEndModel: GestureModel<any> = {
  id: 'special-key-end',
  resolutionPriority: 0,
  contacts : [
    {
      model: {
        ...SimpleTapContactModel,
        itemChangeAction: 'resolve'
      },
      endOnResolve: true,
    }
  ],
  resolutionAction: {
    type: 'complete',
    item: 'none'
  }
}

/**
 * The flickless, roaming-touch-less version.
 */
export function basicLongpressModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'longpress',
    resolutionPriority: 0,
    contacts: [
      {
        model: {
          // Is the version without the up-flick shortcut.
          ...BasicLongpressContactModel(params),
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
    }
  }
}

/**
 * For use when a layout doesn't have flicks; has the up-flick shortcut
 * and facilitates roaming-touch.
 */
export function longpressModelWithShortcut(params: GestureParams): GestureModel<any> {
  return {
    ...basicLongpressModel(params),

    id: 'longpress',
    resolutionPriority: 0,
    contacts: [
      {
        model: {
          ...LongpressContactModelWithShortcut(params),
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
        replace: 'longpress-roam'
      },
      path: {
        type: 'replace',
        replace: 'longpress-roam'
      }
    }
  }
}

/**
 * For use when a layout doesn't have flicks; has the up-flick shortcut
 * and facilitates roaming-touch.
 */
export function longpressModelWithRoaming(params: GestureParams): GestureModel<any> {
  return {
    ...basicLongpressModel(params),

    id: 'longpress-roam',
    resolutionPriority: 0,
    contacts: [
      {
        model: {
          // false - disabled shortcut regardless of params
          ...LongpressContactModelWithShortcut(params, false),
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
        replace: 'longpress-roam'
      },
      path: {
        type: 'replace',
        replace: 'longpress-roam'
      }
    }
  }
}

export function flickStartModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'flick-start',
    resolutionPriority: 3,
    contacts: [
      {
        model: flickStartContactModel(params)
      }
    ],
    resolutionAction: {
      type: 'chain',
      item: 'none',
      next: 'flick-end',
    },
  }
}

export function flickEndModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'flick-end',
    resolutionPriority: 0,
    contacts: [
      {
        model: flickEndContactModel(params)
      },
    ],
    rejectionActions: {
      path: {
        type: 'replace',
        replace: 'flick-reject'
      }
    },
    resolutionAction: {
      type: 'complete',
      item: 'current'
    }
  }
}

export const FlickRejectModel: GestureModel<any> = {
  id: 'flick-reject',
  resolutionPriority: 5,
  contacts: [
    // None - exists as a way to have a different model-id for flick-rejection.
  ],
  sustainTimer: {
    duration: 0,
    expectedResult: true
  },
  resolutionAction: {
    type: 'complete',
    item: 'base'
  }
}

export function multitapStartModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'multitap-start',
    resolutionPriority: 2,
    contacts: [
      {
        model: {
          ...InstantContactResolutionModel,
          itemPriority: 1,
          pathInheritance: 'reject',
          allowsInitialState(incomingSample, comparisonSample, baseItem) {
            return incomingSample.item == baseItem;
          },
        },
      }
    ],
    sustainTimer: {
      duration: params.multitap.waitLength,
      expectedResult: false,
      baseItem: 'base'
    },
    resolutionAction: {
      type: 'chain',
      next: 'multitap-end',
      item: 'current'
    }
  }
}

export function multitapEndModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'multitap-end',
    resolutionPriority: 2,
    contacts: [
      {
        model: {
          ...SimpleTapContactModel,
          itemPriority: 1,
          timer: {
            duration: params.multitap.holdLength,
            expectedResult: false
          }
        },
        endOnResolve: true
      }, {
        model: InstantContactResolutionModel,
        resetOnResolve: true
      }
    ],
    rejectionActions: {
      timer: {
        type: 'replace',
        replace: 'simple-tap'
      }
    },
    resolutionAction: {
      type: 'chain',
      next: 'multitap-start',
      item: 'none'
    }
  }
}

export function initialTapModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'initial-tap',
    resolutionPriority: 1,
    contacts: [
      {
        model: {
          ...SimpleTapContactModel,
          pathInheritance: 'chop',
          itemPriority: 1,
          timer: {
            duration: params.multitap.holdLength,
            expectedResult: false
          }
        },
        endOnResolve: true
      }, {
        model: InstantContactResolutionModel,
        resetOnResolve: true
      }
    ],
    rejectionActions: {
      timer: {
        type: 'replace',
        replace: 'simple-tap'
      }
    },
    resolutionAction: {
      type: 'chain',
      next: 'multitap-start',
      item: 'current'
    }
  }
}

export const SimpleTapModel: GestureModel<any> = {
  id: 'simple-tap',
  resolutionPriority: 1,
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
    type: 'complete',
    item: 'current'
  }
}

export function initialTapModelWithReset(params: GestureParams): GestureModel<any> {
  const base = initialTapModel(params);
  return {
    ...base,
    rejectionActions: {
      ...base.rejectionActions,
      item: {
        type: 'replace',
        replace: 'initial-tap'
      }
    }
  }
}

export const SimpleTapModelWithReset: GestureModel<any> = {
  ...SimpleTapModel,
  rejectionActions: {
    ...SimpleTapModel.rejectionActions,
    item: {
      type: 'replace',
      replace: 'simple-tap'
    }
  }
}

export const SubkeySelectModel: GestureModel<any> = {
  id: 'subkey-select',
  resolutionPriority: 0,
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

export const ModipressStartModel: GestureModel<KeyElement> = {
  id: 'modipress-start',
  resolutionPriority: 5,
  contacts: [
    {
      model: {
        ...ModipressContactStartModel,
        allowsInitialState(incomingSample, comparisonSample, baseItem) {
          return keySupportsModipress(baseItem);
        },
        itemChangeAction: 'reject',
        itemPriority: 1
      }
    }
  ],
  resolutionAction: {
    type: 'chain',
    next: 'modipress-hold',
    selectionMode: 'modipress',
    item: 'current' // return the modifier key ID so that we know to shift to it!
  }
}

export function modipressHoldModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'modipress-hold',
    resolutionPriority: 5,
    contacts: [
      {
        model: {
          ...ModipressContactHoldModel,
          itemChangeAction: 'reject',
          pathInheritance: 'full',
          timer: {
            duration: params.multitap.holdLength,
            expectedResult: true,
            // If entered due to 'reject' on 'modipress-multitap-end',
            // we want to immediately resolve.
            inheritElapsed: true
          }
        }
      }
    ],
    // To be clear:  any time modipress-hold is triggered and the timer duration elapses,
    // we disable any potential to multitap on the modipress key.
    resolutionAction: {
      type: 'chain',
      next: 'modipress-end',
      selectionMode: 'modipress',
      // Key was already emitted from the 'modipress-start' stage.
      item: 'none'
    },
    rejectionActions: {
      path: {
        type: 'replace',
        // Because SHIFT -> CAPS multitap is a thing.  Shift gets handled as a modipress first.
        // Modipresses resolve before multitaps... unless there's a model designed to handle & disambiguate both.
        replace: 'modipress-end-multitap-transition'
      }
    }
  }
}

export const ModipressMultitapTransitionModel: GestureModel<any> = {
  id: 'modipress-end-multitap-transition',
  resolutionPriority: 5,
  contacts: [
    // None.  This exists as an intermediate state to transition from
    // a basic modipress into a combined multitap + modipress.
  ],
  sustainTimer: {
    duration: 0,
    expectedResult: true
  },
  resolutionAction: {
    type: 'chain',
    next: 'modipress-multitap-start',
    item: 'none'
  }
}

export const ModipressEndModel: GestureModel<any> = {
  id: 'modipress-end',
  resolutionPriority: 5,
  contacts: [
    {
      model: {
        ...ModipressContactEndModel,
        itemChangeAction: 'reject',
        pathInheritance: 'full'
      }
    }
  ],
  resolutionAction: {
    type: 'complete',
    // Key was already emitted from the 'modipress-start' stage.
    item: 'none'
  }
}

export function modipressMultitapStartModel(params: GestureParams): GestureModel<KeyElement> {
  return {
    id: 'modipress-multitap-start',
    resolutionPriority: 6,
    contacts: [
      {
        model: {
          ...ModipressContactStartModel,
          pathInheritance: 'reject',
          allowsInitialState(incomingSample, comparisonSample, baseItem) {
            if(incomingSample.item != baseItem) {
              return false;
            }

            return keySupportsModipress(baseItem);
          },
          itemChangeAction: 'reject',
          itemPriority: 1
        }
      }
    ],
    sustainTimer: {
      duration: params.multitap.waitLength,
      expectedResult: false,
      baseItem: 'base'
    },
    resolutionAction: {
      type: 'chain',
      next: 'modipress-multitap-end',
      selectionMode: 'modipress',
      item: 'current' // return the modifier key ID so that we know to shift to it!
    }
  }
}

export function modipressMultitapEndModel(params: GestureParams): GestureModel<any> {
  return {
    id: 'modipress-multitap-end',
    resolutionPriority: 5,
    contacts: [
      {
        model: {
          ...ModipressContactEndModel,
          itemChangeAction: 'reject',
          pathInheritance: 'full',
          timer: {
            duration: params.multitap.holdLength,
            expectedResult: false
          }
        }
      }
    ],
    resolutionAction: {
      type: 'chain',
      // Because SHIFT -> CAPS multitap is a thing.  Shift gets handled as a modipress first.
      next: 'modipress-multitap-start',
      // Key was already emitted from the 'modipress-start' stage.
      item: 'none'
    },
    rejectionActions: {
      timer: {
        type: 'replace',
        replace: 'modipress-end'
      }
    }
  }
}
// #endregion