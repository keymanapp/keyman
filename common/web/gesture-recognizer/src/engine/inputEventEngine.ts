import { InputEngineBase } from "./headless/inputEngineBase.js";
import { InputSample } from "./headless/inputSample.js";
import { GestureSource } from "./headless/gestureSource.js";
import { GestureRecognizerConfiguration } from "./index.js";
import { ManagedPromise } from "@keymanapp/web-utils";
import { EventSequentializationQueue } from "./eventSequentializationQueue.js";

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

  protected sequentializer = new EventSequentializationQueue();

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

    console.log(`onInputStart: ${touchpoint.identifier}, original id ${identifier}, base item ${sample.item?.['keyId'] ?? 'null'}`);

    // Waaaaait.  The touchpoint id map - not the ancestor touchpoint list - is enough for our needs, I think!
    // this._pendingTouchpoints
    // this.addTouchpoint(touchpoint); // NOT now.

    // External objects may desire to directly terminate handling of
    // input sequences under specific conditions.
    touchpoint.path.on('invalidated', () => {
      this.dropTouchpoint(touchpoint);
    });

    touchpoint.path.on('complete', () => {
      this.dropTouchpoint(touchpoint);
    });

    this.addTouchpoint(touchpoint);

    this.emit('pointstart', touchpoint);
    console.log('pointstart: ' + touchpoint.baseItem?.['keyId']);

    // Just... do this.
    return touchpoint;
  }

  public unlockTouchpoint(touchpoint: GestureSource<HoveredItemType, StateToken>) {
  }

  protected onInputMove(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget) {
    const activePoint = this.getTouchpointWithId(identifier);

    console.log(`onInputMove: ${activePoint?.identifier}, original id: ${identifier}`);
    if(!activePoint) {
      return;
    }

    console.log('update: ' + activePoint.baseItem?.['keyId'])
    activePoint.update(sample)
  }

  protected onInputMoveCancel(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget) {
    const touchpoint = this.getTouchpointWithId(identifier);
    console.log(`onInputMoveCancel: ${touchpoint?.identifier}, original id: ${identifier}`);
    if(!touchpoint) {
      return;
    }

    touchpoint.update(sample);
    console.log('cancel update/terminate: ' + touchpoint.baseItem?.['keyId'])
    touchpoint.path.terminate(true);
  }

  protected onInputEnd(identifier: number, target: EventTarget) {
    const touchpoint = this.getTouchpointWithId(identifier);
    console.log(`onInputEnd: ${touchpoint?.identifier}, original id: ${identifier}`);
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

    if(sample.t != lastEntry.t) { // Temp trial
      touchpoint.update(sample);
    } // Temp trial

    console.log('end update/terminate: ' + touchpoint.baseItem?.['keyId'])
    this.getTouchpointWithId(identifier)?.path.terminate(false);
  }
}