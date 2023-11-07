import { CumulativePathStats } from "../../cumulativePathStats.js";
import { GesturePath } from "../../gesturePath.js";

// The TrackedPath model only cares about if the path matches... not what that MEANS.
// THAT is the role of the gesture Model (model.ts).

export interface PathModel {
  /**
   * Given a TrackedPath, indicates whether or not the path matches this PathModel.
   *
   * May return null or undefined to signal 'continue'.
   *
   * @param path The current gesture's path, based on the path-inheritance setting of
   * its `ContactModel`.
   * @param basePathStats The stats for the path of the gesture's previous 'stage', if
   * one existed.
   */
  evaluate(path: GesturePath<any>, basePathStats: CumulativePathStats<any>): 'reject' | 'resolve' | undefined;
}