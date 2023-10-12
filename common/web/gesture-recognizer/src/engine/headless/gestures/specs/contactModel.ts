import { InputSample } from "../../inputSample.js";
import { PathModel } from "./pathModel.js";

// pop - a signal to reverse actions taken in response to the most-recent 'push'.  (Generally, for 'modipress' gestures)
type SimpleStringResult = 'resolve' | 'reject';

export type PointModelResolution = SimpleStringResult;

export interface ContactModel<Type> {
  pathModel: PathModel,
  pathResolutionAction: PointModelResolution,

  // If multiple touchpoints are active, determines which point's item 'wins' for
  // gesture-state updates and resolution.  Higher = better.
  itemPriority: number;

  /**
   * Used for resolving or rejecting this component of a gesture based on a time threshold
   * for the touch contact point's lifetime.
   *
   * An easy example application:  touch-keyboard longpresses generally trigger after a
   * set amount of time, transitioning to display of a subkey menu once that time has
   * elapsed.
   */
  timer?: {
    duration: number,
    expectedResult: boolean;
  }

  // This field is primarly used at the `GestureMatcher` level, rather than the
  // `PathMatcher` level.
  /**
   * When a parent gesture 'chains' into a gesture with this contact model,
   * the path may be "inherited" in one of four ways:
   *
   * - 'reject' - a still-active path is not allowed
   * - 'chop'   - a still-active path is merely 'trimmed' at the current point in
   *              the path.  The current location's 'item' will become the base item.
   * - 'partial' - like 'chop', except it preserves the original base item.
   * - 'full'   - preserves the entire path and keeps the original base item.
   *
   * Note that for gestures reachable by _optional_ chaining, only the first two modes
   * are properly supported.  Exclusive chaining may safely use all four.
   */
  pathInheritance?: 'reject' | 'chop' | 'partial' | 'full';

  /**
   * Used to either instantly resolve or reject this component of a gesture based on
   * a change in 'currently hovered item' for the path.
   *
   * For example, a longpress component should likely reject on change of base item,
   * allowing a new longpress (with a fresh timer) to start with the new base item.
   */
  readonly itemChangeAction?: 'reject' | 'resolve' | undefined; // may be undefined for 'continue'

  /**
   * Is needed to define whether or not the contact-point should be ignored by this gesture type.
   * If undefined, defaults to () => true.
   *
   * @param incomingSample   The first input sample of the path to be modeled.
   * @param comparisonSample The most recent sample related to the same gesture component, if one exists.
   *                         May be `null`.
   * @param baseItem         The 'base item' for the path corresponding to `comparisonSample`
   * @returns
   */
  readonly allowsInitialState?: (
    incomingSample: InputSample<Type>,
    comparisonSample?: InputSample<Type>,
    baseItem?: Type
  ) => boolean;
}