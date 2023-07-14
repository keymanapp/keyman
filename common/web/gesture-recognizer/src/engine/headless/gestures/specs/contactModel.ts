import { PathModel } from "./pathModel.js";

// For modipress, in particular - says what gestures are allowed during the 'push' state.
export interface PushResult {
  type: 'push';
  permittedGestures: string[]; // of ids for allowed types
}

export interface ComplexResolveResult {
  type: 'resolve';
  cancelBaseAwait?: boolean; // default false, but if true, will auto-cancel the top-level model's await (if it exists)
}

type WrappedString<Text> = { type: Text };

// pop - ends the modipress 'push' state.
type SimpleStringResult = 'resolve' | 'reject' | 'pop';

export type PointModelResolution =  PushResult | ComplexResolveResult | WrappedString<SimpleStringResult>;


export type SpecPointModelResolution = SimpleStringResult | PointModelResolution;

export interface ContactModel {
  pathModel: PathModel, // Might be able to fully drop the generic.
  onPathResolve: SpecPointModelResolution,
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

  readonly onItemChange?: 'reject' | 'resolve'; // may be undefined for 'continue'
}