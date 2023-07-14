import { GesturePath } from "../../gesturePath.js";

// The TrackedPath model only cares about if the path matches... not what that MEANS.
// THAT is the role of the gesture Model (model.ts).

export interface PathModel {
  /**
   * Given a TrackedPath, indicates whether or not the path matches this PathModel.
   *
   * May return null or undefined to signal 'continue'.
   *
   * @param path
   */
  evaluate(path: GesturePath<any>): 'reject' | 'resolve' | undefined;
}