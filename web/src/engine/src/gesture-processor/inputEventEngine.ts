import { InputEngineBase } from "./headless/inputEngineBase.js";
import { InputSample } from "./headless/inputSample.js";
import { GestureSource } from "./headless/gestureSource.js";
import { GestureRecognizerConfiguration } from "./index.js";
import { reportError } from "./reportError.js";

export function processSampleClientCoords<Type, StateToken>(config: GestureRecognizerConfiguration<Type>, clientX: number, clientY: number) {
  const targetRect = config.targetRoot.getBoundingClientRect();
  return {
    clientX: clientX,
    clientY: clientY,
    targetX: clientX - targetRect.left,
    targetY: clientY - targetRect.top
  } as InputSample<Type, StateToken>;
}

export abstract class InputEventEngine<ItemType, StateToken> extends InputEngineBase<ItemType, StateToken> {
  abstract registerEventHandlers(): void;
  abstract unregisterEventHandlers(): void;

  protected buildSampleFor(clientX: number, clientY: number, target: EventTarget, timestamp: number, source: GestureSource<ItemType, StateToken>): InputSample<ItemType, StateToken> {
    const sample: InputSample<ItemType, StateToken> = {
      ...processSampleClientCoords(this.config, clientX, clientY),
      t: timestamp,
      stateToken: source?.stateToken ?? this.stateToken
    };

    const itemIdentifier = source?.currentRecognizerConfig.itemIdentifier ?? this.config.itemIdentifier;
    const hoveredItem = itemIdentifier(sample, target);
    sample.item = hoveredItem;

    return sample;
  }

  protected onInputStart(identifier: number, sample: InputSample<ItemType, StateToken>, target: EventTarget, isFromTouch: boolean) {
    const touchpoint = this.createTouchpoint(identifier, isFromTouch);
    touchpoint.update(sample);

    this.addTouchpoint(touchpoint);

    // External objects may desire to directly terminate handling of
    // input sequences under specific conditions.
    touchpoint.path.on('invalidated', () => {
      this.dropTouchpoint(touchpoint);
    });

    touchpoint.path.on('complete', () => {
      this.dropTouchpoint(touchpoint);
    });

    try {
      this.emit('pointstart', touchpoint);
    } catch(err) {
      reportError('Engine-internal error while initializing gesture matching for new source', err);
    }

    return touchpoint;
  }

  protected onInputMove(touchpoint: GestureSource<ItemType, StateToken>, sample: InputSample<ItemType, StateToken>, target: EventTarget) {
    if(!touchpoint) {
      return;
    }

    try {
      touchpoint.update(sample);
    } catch(err) {
      reportError('Error occurred while updating source', err);
    }
  }

  protected onInputMoveCancel(touchpoint: GestureSource<ItemType, StateToken>, sample: InputSample<ItemType, StateToken>, target: EventTarget) {
    if(!touchpoint) {
      return;
    }

    try {
      touchpoint.update(sample);
      touchpoint.path.terminate(true);
    } catch(err) {
      reportError('Error occurred while cancelling further input for source', err);
    }
  }

  protected onInputEnd(touchpoint: GestureSource<ItemType, StateToken>, target: EventTarget) {
    if(!touchpoint) {
      return;
    }

    try {
      touchpoint.path.terminate(false);
    } catch(err) {
      reportError('Error occurred while finalizing input for source', err);
    }
  }
}