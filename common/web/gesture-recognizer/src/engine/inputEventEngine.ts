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

export abstract class InputEventEngine<HoveredItemType, StateToken> extends InputEngineBase<HoveredItemType, StateToken> {
  abstract registerEventHandlers(): void;
  abstract unregisterEventHandlers(): void;

  protected buildSampleFor(clientX: number, clientY: number, target: EventTarget, timestamp: number, source: GestureSource<HoveredItemType, StateToken>): InputSample<HoveredItemType, StateToken> {
    const sample: InputSample<HoveredItemType, StateToken> = {
      ...processSampleClientCoords(this.config, clientX, clientY),
      t: timestamp,
      stateToken: source?.stateToken ?? this.stateToken
    };

    const itemIdentifier = source?.currentRecognizerConfig.itemIdentifier ?? this.config.itemIdentifier;
    const hoveredItem = itemIdentifier(sample, target);
    sample.item = hoveredItem;

    return sample;
  }

  protected onInputStart(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget, isFromTouch: boolean) {
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

  protected onInputMove(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget) {
    const activePoint = this.getTouchpointWithId(identifier);
    if(!activePoint) {
      return;
    }

    activePoint.update(sample);
  }

  protected onInputMoveCancel(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget) {
    const touchpoint = this.getTouchpointWithId(identifier);
    if(!touchpoint) {
      return;
    }

    touchpoint.update(sample);
    touchpoint.path.terminate(true);
  }

  protected onInputEnd(identifier: number, target: EventTarget) {
    const touchpoint = this.getTouchpointWithId(identifier);
    if(!touchpoint) {
      return;
    }

    const lastEntry = touchpoint.path.stats.lastSample;
    const sample = this.buildSampleFor(lastEntry.clientX, lastEntry.clientY, target, lastEntry.t, touchpoint);

    /* While an 'end' event immediately follows a 'move' if it occurred simultaneously,
     * this is decidedly _not_ the case if the touchpoint was held for a while without
     * moving, even at the point of its release.
     *
     * We'll never need to worry about the touchpoint moving here, and thus we don't
     * need to worry about `currentHoveredItem` changing.  We're only concerned with
     * recording the _timing_ of the touchpoint's release.
     */
    if(sample.t != lastEntry.t) {
      touchpoint.update(sample);
    }

    this.getTouchpointWithId(identifier)?.path.terminate(false);
  }
}