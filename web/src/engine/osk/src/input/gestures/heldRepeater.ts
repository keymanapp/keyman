import { GestureSequence } from "@keymanapp/gesture-recognizer";
import { KeyDistribution } from "keyman/engine/keyboard";

import { KeyElement } from "../../keyElement.js";
import { GestureHandler } from './gestureHandler.js';

export class HeldRepeater implements GestureHandler {
  readonly directlyEmitsKeys = true;

  static readonly INITIAL_DELAY = 500;
  static readonly REPEAT_DELAY = 100;

  readonly source: GestureSequence<KeyElement, string>;
  readonly hasModalVisualization = false;
  readonly repeatClosure: () => void;

  timerHandle: number;

  constructor(source: GestureSequence<KeyElement, string>, closureToRepeat: () => void) {
    this.source = source;

    const baseKey = source.stageReports[0].item;
    baseKey.key.highlight(true);

    this.repeatClosure = () => {
      closureToRepeat();
      // The repeat-closure may cancel key highlighting.  This restores it afterward.
      baseKey.key.highlight(true);
    }


    this.timerHandle = window.setTimeout(this.deleteRepeater, HeldRepeater.INITIAL_DELAY);

    this.source.on('complete', () => {
      window.clearTimeout(this.timerHandle);
      this.timerHandle = undefined;
      baseKey.key.highlight(false);
    });
  }

  cancel() {
    this.deleteRepeater();
    this.source.cancel();
  }

  readonly deleteRepeater = () => {
    this.repeatClosure();

    this.timerHandle = window.setTimeout(this.deleteRepeater, HeldRepeater.REPEAT_DELAY);
  }

  currentStageKeyDistribution(): KeyDistribution {
    return null;
  }
}