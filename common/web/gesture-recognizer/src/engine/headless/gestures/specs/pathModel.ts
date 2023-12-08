import { CumulativePathStats } from "../../cumulativePathStats.js";
import { GesturePath } from "../../gesturePath.js";

// The GesturePath model only cares about if the path matches... not what that MEANS.
// THAT is the role of the GestureModel (gestureModel.ts).

export interface PathModel<Type = any> {
  /**
   * Given a GesturePath, indicates whether or not the path matches this PathModel.
   *
   * May return null or undefined to signal 'continue'.
   *
   * @param path The current gesture's path, based on the path-inheritance setting of
   * its `ContactModel`.
   * @param basePathStats The stats for the path of the gesture's previous 'stage', if
   * one existed.
   */
  evaluate(path: GesturePath<Type>, basePathStats: CumulativePathStats<Type>): 'reject' | 'resolve' | undefined;
}