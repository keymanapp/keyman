import { type KeyElement } from '../../../keyElement.js';
import VisualKeyboard from '../../../visualKeyboard.js';

import { KeyDistribution, ActiveKeyBase, ManagedPromise } from '@keymanapp/keyboard-processor';
import { GestureSequence } from '@keymanapp/gesture-recognizer';
import { GestureHandler } from '../gestureHandler.js';

/**
 *
 */
export default class Modipress implements GestureHandler {
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
      completionCallback && completionCallback();
    };

    vkbd.lockLayer(true);

    source.on('stage', (stage) => {
      const stageName = stage.matchedId;
      if(stageName.includes('modipress') && stageName.includes('-end')) {
        this.cancel();
      } else if(stageName.includes('modipress') && stageName.includes('-hold')) {
        this.shouldRestore = true;
      }
    });
  }

  get isLocked(): boolean {
    return this.shouldRestore;
  }

  setLocked() {
    this.shouldRestore = true;
  }

  get completed(): boolean {
    return this.completionCallback === undefined;
  }

  cancel() {
    const callback = this.completionCallback;
    this.completionCallback = null;
    callback && callback();
    this.source.cancel();
  }

  readonly hasModalVisualization = false;

  currentStageKeyDistribution(baseDistMap: Map<ActiveKeyBase, number>): KeyDistribution {
    return null;
  }
}