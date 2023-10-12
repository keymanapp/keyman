import * as gestures from "../index.js";

// Prototype spec for the main gesture & gesture-set definitions.
// A work in-progress.  Should probably land somewhere within headless/gestures/specs/.
// ... with the following two functions, as well.
export interface GestureModelDefs<Type> {
  gestures: gestures.specs.GestureModel<Type>[],
  sets: {
    default: string[],
  } & Record<string, string[]>;
}


export function getGestureModel<Type>(defs: GestureModelDefs<Type>, id: string): gestures.specs.GestureModel<Type> {
  const result = defs.gestures.find((spec) => spec.id == id);
  if(!result) {
    throw new Error(`Could not find spec for gesture with id '${id}'`);
  }

  return result;
}

export function getGestureModelSet<Type>(defs: GestureModelDefs<Type>, id: string): gestures.specs.GestureModel<Type>[] {
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
} as GestureModelDefs<any>