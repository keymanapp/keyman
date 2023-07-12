import { TrackedPoint } from "../trackedPoint.js";
import { TrackedPath } from "../trackedPath.js";

// The TrackedPath model only cares about if the path matches... not what that MEANS.
// THAT is the role of the gesture Model (model.ts).
export interface PathModel<Type> {
  /*
   * NOTE:  in theory, we could JSON-spec this instead.  Worst case:
   * - resolve if any:
   *   - condition 1: if all:
   *     - conditions on path stats
   *   - condition 2: if all:
   *     - conditions on path stats
   *   - etc.
   * - reject if any:
   *   - same as above
   * - if neither has a match:  implied 'continue'
   * - if both do:  uh... double check that spec, for one.  But also, could
   *   set/specify which set should take precedence.  Or globally say 'reject'
   *   before 'resolve'.
   *
   * TODO:  probably, that.  (JSON-spec'ing here instead of requiring a func)
   * But, for a first pass... there's so much JSON-interpretation stuff to pin down
   * already that this aspect should be triaged 'til later.
   *
   * Metaphorically speaking, we should prove the 'forest' design will work before we
   * commit to fine-tuned landscaping that we may have to trash if proven wrong.
   */

  /**
   * Given a TrackedPath, indicates whether or not the path matches this PathModel.
   *
   * @param path
   * @param initialItem
   */
  evaluate(path: TrackedPath<any> /*, initialItem: Type*/): 'reject' | 'resolve' | 'continue';
  // path contains the 'current' item.
  // but more importantly... I don't think we need to worry about the item at all here.
  // the 'point' level setting for onItemChange, gesture-specific itemIdentifier based
  // on recognizerId, and/or itemIdentifier override on longpress-lockin should all
  // handle anything that we'd need to track the Item here for.
  // (In which case we can drop the generic aspect!  Yay!)
}