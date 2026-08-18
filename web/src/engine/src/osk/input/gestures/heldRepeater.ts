/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2023-10-04.
 *
 * The HeldRepeater class models key input that is repeated when its
 * corresponding key is held.  At this time, the class is mostly used to model a
 * repeatable Backspace input.
 */

import { GestureSequence } from "keyman/engine/gesture-processor";
import { KeyDistribution } from "keyman/engine/keyboard";

import { KeyElement } from "../../keyElement.js";
import { GestureHandler } from './gestureHandler.js';

export class HeldRepeater implements GestureHandler {
  public readonly directlyEmitsKeys = true;
  public readonly hasModalVisualization = false;

  public static readonly INITIAL_DELAY = 500; // msec
  public static readonly REPEAT_DELAY = 100;  // msec

  private readonly source: GestureSequence<KeyElement, string>;
  private readonly baseKey: KeyElement
  private readonly actionToRepeat: () => void;
  private timerHandle: number;

  constructor(source: GestureSequence<KeyElement, string>, actionToRepeat: () => void) {
    this.source = source;

    this.baseKey = source.stageReports[0].item;
    this.baseKey.key.highlight(true);
    this.actionToRepeat = actionToRepeat;

    this.timerHandle = window.setTimeout(() => this.repeatAction(), HeldRepeater.INITIAL_DELAY);

    this.source.on('complete', () => {
      this.cancel();
    });
  }

  cancel() {
    if(this.timerHandle !== undefined) {
      window.clearTimeout(this.timerHandle);
      delete this.timerHandle;
    }

    this.baseKey.key.highlight(false);
    this.source.cancel();
  }

  private repeatAction() {
    this.actionToRepeat();
    // In case the action to repeat cancels key highlighting, we restore it
    // afterward.
    this.baseKey.key.highlight(true);
    this.timerHandle = window.setTimeout(() => this.repeatAction(), HeldRepeater.REPEAT_DELAY);
  }

  currentStageKeyDistribution(): KeyDistribution {
    return null;
  }
}