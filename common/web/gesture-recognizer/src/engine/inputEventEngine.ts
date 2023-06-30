import EventEmitter from "eventemitter3";
import { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
import { InputSample } from "./headless/inputSample.js";
import { Nonoptional } from "./nonoptional.js";
import { TrackedPoint } from "./headless/trackedPoint.js";

interface EventMap<HoveredItemType> {
  'pointstart': (input: TrackedPoint<HoveredItemType>) => void
}

export abstract class InputEventEngine<HoveredItemType> extends EventEmitter<EventMap<HoveredItemType>> {
  protected readonly config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>;
  private _activeTouchpoints: TrackedPoint<HoveredItemType>[] = [];

  public constructor(config: Nonoptional<GestureRecognizerConfiguration<HoveredItemType>>) {
    super();
    this.config = config;
  }

  abstract registerEventHandlers(): void;
  abstract unregisterEventHandlers(): void;

  /**
   * @param identifier The identifier number corresponding to the input sequence.
   */
  hasActiveTouchpoint(identifier: number) {
    return this.getTouchpointWithId(identifier) !== undefined;
  }

  private getTouchpointWithId(identifier: number) {
    return this._activeTouchpoints.find((point) => point.rawIdentifier == identifier);
  }

  public dropTouchpointWithId(identifier: number) {
    this._activeTouchpoints = this._activeTouchpoints.filter((point) => point.rawIdentifier != identifier);
  }

  protected buildSampleFor(clientX: number, clientY: number, target: EventTarget): InputSample<HoveredItemType> {
    const targetRect = this.config.targetRoot.getBoundingClientRect();
    const sample: InputSample<HoveredItemType> = {
      clientX: clientX,
      clientY: clientY,
      targetX: clientX - targetRect.left,
      targetY: clientY - targetRect.top,
      t: performance.now()
    };

    const hoveredItem = this.config.itemIdentifier(sample, target);
    sample.item = hoveredItem;

    return sample;
  }

  protected onInputStart(identifier: number, sample: InputSample<HoveredItemType>, target: EventTarget, isFromTouch: boolean) {
    const touchpoint = new TrackedPoint<HoveredItemType>(identifier, isFromTouch);
    touchpoint.update(sample);

    this._activeTouchpoints.push(touchpoint);

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

  protected onInputMove(identifier: number, sample: InputSample<HoveredItemType>, target: EventTarget) {
    const activePoint = this.getTouchpointWithId(identifier);
    if(!activePoint) {
      return;
    }

    activePoint.update(sample);
  }

  protected onInputMoveCancel(identifier: number, sample: InputSample<HoveredItemType>, target: EventTarget) {
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
    const sample = this.buildSampleFor(lastEntry.clientX, lastEntry.clientY, target);

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