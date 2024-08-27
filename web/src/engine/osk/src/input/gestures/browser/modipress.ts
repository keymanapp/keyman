import { type KeyElement } from '../../../keyElement.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { KeyDistribution, ActiveKeyBase } from 'keyman/engine/keyboard';
import { GestureSequence } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';

/**
 * Represents a potential modipress gesture's implementation within KeymanWeb, including
 * modipresses generated at the end of multitap sequences.
 *
 * This involves "locking" the current layer in place until the modipress is complete.
 */
export default class Modipress implements GestureHandler {
  readonly directlyEmitsKeys = true;

  private completionCallback: () => void;
  private originalLayer: string;
  private shouldRestore: boolean = false;
  private source: GestureSequence<KeyElement, string>;

  constructor(
    source: GestureSequence<KeyElement, string>,
    vkbd: VisualKeyboard,
    completionCallback: () => void,
  ) {
    const initialStage = source.stageReports[0];
    this.originalLayer = initialStage.sources[0].stateToken;
    this.source = source;

    this.completionCallback = () => {
      vkbd.lockLayer(false);
      if(this.shouldRestore) {
        vkbd.layerId = this.originalLayer;
        vkbd.updateState();
      }
      completionCallback?.();
    };

    vkbd.lockLayer(true);

    source.on('stage', (stage) => {
      const stageName = stage.matchedId;
      if(stageName.includes('modipress') && stageName.includes('-end')) {
        this.clear();
      } else if(stageName.includes('modipress') && stageName.includes('-hold')) {
        this.shouldRestore = true;
      }
    });

    source.on('complete', () => this.cancel());
  }

  get isLocked(): boolean {
    return this.shouldRestore;
  }

  setLocked() {
    this.shouldRestore = true;
  }

  get completed(): boolean {
    return this.completionCallback === null;
  }

  clear() {
    const callback = this.completionCallback;
    this.completionCallback = null;
    callback?.();
  }

  cancel() {
    this.clear();
    this.source.cancel();
  }

  readonly hasModalVisualization = false;

  currentStageKeyDistribution(baseDistMap: Map<ActiveKeyBase, number>): KeyDistribution {
    return null;
  }
}