import { GestureSequence } from "@keymanapp/gesture-recognizer";

import { KeyElement } from "../../keyElement.js";

export class HeldRepeater {
  static readonly INITIAL_DELAY = 500;
  static readonly REPEAT_DELAY = 100;

  readonly source: GestureSequence<KeyElement>;
  readonly repeatClosure: () => void;

  timerHandle: number;

  constructor(source: GestureSequence<KeyElement>, closureToRepeat: () => void) {
    this.source = source;
    this.repeatClosure = closureToRepeat;

    this.timerHandle = window.setTimeout(this.deleteRepeater, HeldRepeater.INITIAL_DELAY);

    this.source.on('complete', () => {
      window.clearTimeout(this.timerHandle);
      this.timerHandle = undefined;
    });
  }

  readonly deleteRepeater = () => {
    this.repeatClosure();

    this.timerHandle = window.setTimeout(this.deleteRepeater, HeldRepeater.REPEAT_DELAY);
  }
}