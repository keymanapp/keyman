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

  // Allows specifying use of one of the configured 'gestureItemIdentifiers'.
  recognizerId?: string;

  /**
   * Used to either instantly resolve or reject this component of a gesture based on
   * a change in 'currently hovered item' for the path.
   *
   * For example, a longpress component should likely reject on change of base item,
   * allowing a new longpress (with a fresh timer) to start with the new base item.
   */
  readonly itemChangeAction?: 'reject' | 'resolve'; // may be undefined for 'continue'

  // Is needed to define whether or not the contact-point should be ignored by this gesture type.
  // If undefined, defaults to () => true.
  // Param 2 only matters for slot 2+ of a multitouch gesture spec; it will receive the latest sample
  // from already-linked.
  readonly allowsInitialState?: (incomingSample: InputSample<Type>, priorityMultitouchSample?: InputSample<Type>) => boolean;
}