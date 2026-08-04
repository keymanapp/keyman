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
   * @param priorStats The current path's stats at the time of the previous path-evaluate
   * call.  Will be null on _each_ path model's first `evaluate` call.
   * @param baseItem The base 'item' for the path's source.  May match an item not
   * held by any path coordinate if 'partial' path inheritance has occurred at some
   * point in the source's history.
   * @param inheritedStats The stats for the portion of the path 'inherited' from
   * prior stages.  May be null.
   */
  evaluate(
    path: GesturePath<Type>,
    priorStats: CumulativePathStats<Type>,
    baseItem: Type,
    inheritedStats: CumulativePathStats<Type>
  ): 'reject' | 'resolve' | undefined;
}