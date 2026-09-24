import { FulfillmentCause } from "../matchers/pathMatcher.js";
import { GestureResolutionSpec, RejectionReplace } from "./gestureModel.js";
import { GestureModelDefs } from "./gestureModelDefs.js";

interface ModelDefError {
  error: string,
  instances: string[]
}

/**
 * Validates that all gesture ids and gesture-set ids that are defined are utilized and vice-versa.
 *
 * Note that this function is not currently able to validate the represented behaviors; it only
 * validates internal self-references.
 * @param definitions
 * @returns
 */
export function validateModelDefs(definitions: GestureModelDefs<any, any>): ModelDefError[] | null {
  const errors: ModelDefError[] = [];
  const {gestures, sets} = definitions;

  const definedGestureIds = gestures.map((entry) => entry.id);
  const definedSetIds: string[] = Object.keys(sets);

  const sortedGestureIds = [].concat(definedGestureIds).sort();
  const idError: ModelDefError = {
    error: "Duplicated model IDs",
    instances: []
  };

  for(let i=0; i < sortedGestureIds.length - 1; i++) {
    if(sortedGestureIds[i] == sortedGestureIds[i+1]) {
      idError.instances.push(`duplicated model id: ${sortedGestureIds[i]}`);
    }
  }

  if(idError.instances.length) {
    errors.push(idError);
  }

  const referencedGestureIds = new Set<string>();
  const unreferencedGestureIds = new Set<string>(sortedGestureIds);
  const missingGestureIds = new Map<string, ModelDefError>();

  const referencedSetIds = new Set<string>();
  const unreferencedSetIds = new Set<string>(definedSetIds);
  const missingSetIds = new Map<string, ModelDefError>();

  function observeGestureId(id: string, src: string) {
    if(unreferencedGestureIds.has(id)) {
      referencedGestureIds.add(id);
      unreferencedGestureIds.delete(id);
    } else if(!referencedGestureIds.has(id)) {
      let err = missingGestureIds.get(id);
      if(!err) {
        err = {
          error: `Gesture model ${id} does not exist`,
          instances: []
        };
        errors.push(err);
      }
      err.instances.push(src);
      missingGestureIds.set(id, err);
    }
  }

  function observeSetId(id: string, src: string) {
    if(unreferencedSetIds.has(id)) {
      referencedSetIds.add(id);
      unreferencedSetIds.delete(id);
    } else if(!referencedSetIds.has(id)) {
      let err = missingSetIds.get(id);
      if(!err) {
        err = {
          error: `Gesture set ${id} does not exist`,
          instances: []
        };
        errors.push(err);
      }
      err.instances.push(src);
      missingSetIds.set(id, err);
    }
  }

  // There is an implicit reference to the 'default' set.
  observeSetId('default', "Definition Set");

  function processAction(action: GestureResolutionSpec | RejectionReplace, src: string) {
    if(action.type == 'chain') {
      observeGestureId(action.next, src);
      if(action.selectionMode) {
        observeSetId(action.selectionMode, src);
      }
    } else if(action.type == 'replace') {
      observeGestureId(action.replace, src);
    }
  }

  gestures.forEach((entry) => {
    processAction(entry.resolutionAction, `model: ${entry.id}`);

    Object.keys(entry.rejectionActions ?? {}).forEach((key) => {
      processAction(entry.rejectionActions[key as Exclude<FulfillmentCause, 'cancelled'>], `model: ${entry.id}`);
    });
  });

  Object.keys(sets).forEach((key) => {
    const set = sets[key];
    set.forEach((entry) => observeGestureId(entry, `set: ${key}`));
  });

  unreferencedGestureIds.forEach((entry) => {
    errors.push({
      error: `Gesture model ${entry} is not referenced`,
      instances: ["Definition Set"]
    });
  });

  unreferencedSetIds.forEach((entry) => {
    errors.push({
      error: `Gesture set ${entry} is not referenced`,
      instances: ["Definition Set"]
    });
  });

  return errors.length > 0 ? errors : null;
}