import { TrackedPath } from "../trackedPath.js";
import { PathModel } from "./pathModel.js";

// For multitaps, we may need to actually have this be something passed to initialize (the next stage of)
// a multitap gesture.
export interface AwaitResult {
  type: 'await';
  duration: number;
  expectedResult: boolean;
}

// For modipress, in particular - says what gestures are allowed during the 'push' state.
export interface PushResult {
  type: 'push';
  permittedGestures: string[]; // of ids for allowed types
}

export interface ComplexResolveResult {
  type: 'resolve';
  cancelBaseAwait?: boolean; // default false, but if true, will auto-cancel the top-level model's await (if it exists)
  set?: 'initial' | 'set' | 'current'
}

type WrappedString<Text> = { type: Text };

// pop - ends the modipress 'push' state.
type SimpleStringResult = 'resolve' | 'reject' | 'pop';

// DO NOT WORRY ABOUT CARET PAN HERE (for now).
// That'll come later... and there ought be a decent way or two forward.
// - rough idea:  top-level config option:  multipress trumps singlepress, but neither BLOCKS the other.
//   - on multipress stop:  simply... don't terminate the paths.
//   - on new incoming press:  trigger "start new gesture" from currently-pressed position
//     - with trimmed / 'reset' path.
//     - classic multitouch gestures (pinch, zoom) don't care about any "before" path components.
//     - they just start tracking from when they activate.

export type PointModelResolution =  SimpleStringResult | AwaitResult | PushResult |
                                    ComplexResolveResult | WrappedString<SimpleStringResult>;

export interface TouchpointModel<Type> {
  pathModel: PathModel<Type>, // Might be able to fully drop the generic.
  onPathResolve: PointModelResolution,
  itemPriority: number; // If multiple touchpoints are active, determines which point's item 'wins' for
                        // gesture-state updates and resolution.  Higher = better.

  // For longpresses, in particular.
  // Reset:  this one should fail (base item change)
  // - roaming touch behaviors start a new one with new base items; that's the 'reset'.
  timer?: {
    duration: number,
    expectedResult: boolean;
  }

  // Allows specifying use of one of the configured 'gestureItemIdentifiers'.
  recognizerId?: string;

  readonly onItemChange: 'reject' | 'resolve'; // may be undefined for 'continue'
}

/*** Probably safe to eliminate this stuff, but _just_ in case... ***/

// I'm _thinking_ that onItemChange above is sufficient, given BaseItemIdentifier, GestureItemIdentifier, and
// the prospective ability to stack-push a new GestureRecognizerConfiguration for specific gestures (for subkey menus).

// Just in case I'm wrong, though... what's below was as far as I got when attempting a different direction before I
// settled on the model referred to in the previous paragraph.

// // I'm not 100% on the parameterizations below / exactly _which_ items should be considered yet.
// // Or if these actually should exist.
// //
// // ... could these be JSON-spec'd in some form?
// initialItemMatch?: (itemState: Type, path: TrackedPath<Type>) => boolean,
// currentItemMatch?: (itemState: Type, path: TrackedPath<Type>) => boolean // or (pathInitialItem, pathCurrentItem) => boolean //?