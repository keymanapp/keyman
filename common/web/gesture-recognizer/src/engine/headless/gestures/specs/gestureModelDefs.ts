import * as gestures from "../index.js";

export interface GestureModelDefs<Type, StateToken = any> {
  /**
   * The full set of gesture models to be utilized by the gesture-recognition engine.
   */
  gestures: gestures.specs.GestureModel<Type, StateToken>[],

  /**
   * Sets _of sets_ of gesture models accessible as initial gesture stages while
   * within various states of the gesture-engine.
   *
   * `'default'` must be specified, as it is the default state.
   *
   * A 'chain'-type model resolution has the option to specify a `selectionMode` property;
   * the value set there will activate a different gesture-recognition mode _for new
   * gestures_ corresponding to the sets specified here.
   *
   * These sets may be defined to either restrict the range of options for new incoming
   * gestures or to restrict them.  Specifying an empty set will disable all incoming
   * gestures while the alternate state is active, allowing one gesture to block any
   * further gestures from starting until it is completed.
   */
  sets: {
    default: string[],
  } & Record<string, string[]>;
}


export function getGestureModel<Type, StateToken>(defs: GestureModelDefs<Type, StateToken>, id: string): gestures.specs.GestureModel<Type, StateToken> {
  const result = defs.gestures.find((spec) => spec.id == id);
  if(!result) {
    throw new Error(`Could not find spec for gesture with id '${id}'`);
  }

  return result;
}

export function getGestureModelSet<Type, StateToken>(defs: GestureModelDefs<Type, StateToken>, id: string): gestures.specs.GestureModel<Type, StateToken>[] {
  let idSet = defs.sets[id];
  if(!idSet) {
    throw new Error(`Could not find a defined gesture-set with id '${id}'`);
  }

  const set = defs.gestures.filter((spec) => !!idSet.find((id) => spec.id == id));
  const missing = idSet.filter((id) => !set.find((spec) => spec.id == id));

  if(missing.length > 0) {
    throw new Error(`Set '${id}' cannot find definitions for gestures with ids ${missing}`);
  }

  return set;
}

export const EMPTY_GESTURE_DEFS = {
  gestures: [
  ],
  sets: {
    default: []
  }
} as GestureModelDefs<any, any>
