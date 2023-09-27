import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputEngineBase } from "./headless/inputEngineBase.js";
import { InputSample } from "./headless/inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { GestureSource } from "./headless/gestureSource.js";

export abstract class InputEventEngine<HoveredItemType, StateToken> extends InputEngineBase<HoveredItemType, StateToken> {
  protected readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>;

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType, StateToken>>) {
    super();
    this.config = config;
  }

  abstract registerEventHandlers(): void;
  abstract unregisterEventHandlers(): void;

  protected buildSampleFor(clientX: number, clientY: number, target: EventTarget, timestamp: number, stateToken: StateToken): InputSample<HoveredItemType, StateToken> {
    const targetRect = this.config.targetRoot.getBoundingClientRect();
    const sample: InputSample<HoveredItemType, StateToken> = {
      clientX: clientX,
      clientY: clientY,
      targetX: clientX - targetRect.left,
      targetY: clientY - targetRect.top,
      t: timestamp,
      stateToken: stateToken
    };

    const hoveredItem = this.config.itemIdentifier(sample, target);
    sample.item = hoveredItem;

    return sample;
  }

  protected onInputStart(identifier: number, sample: InputSample<HoveredItemType, StateToken>, target: EventTarget, isFromTouch: boolean) {
    const touchpoint = new GestureSource<HoveredItemType>(identifier, this.config, isFromTouch);
    touchpoint.update(sample);

    this.addTouchpoint(touchpoint);

    // External objects may desire to directly terminate handling of
    // input sequences under specific conditions.
    touchpoint.path.on('invalidated', () => {
      this.dropTouchpointWithId(identifier);
    });

    touchpoint.path.on('complete', () => {
      this.dropTouchpointWithId(identifier);
    });

    this.emit('pointstart', touchpoint);
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

    const lastEntry = touchpoint.path.coords[touchpoint.path.coords.length-1];
    const sample = this.buildSampleFor(lastEntry.clientX, lastEntry.clientY, target, lastEntry.t, lastEntry.stateToken);

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