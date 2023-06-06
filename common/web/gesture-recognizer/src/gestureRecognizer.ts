import EventEmitter from "eventemitter3";
import { GestureRecognizerConfiguration } from "./gestureRecognizerConfiguration.js";
import { MouseEventEngine } from "./mouseEventEngine.js";
import { Mutable } from "./mutable.js";
import { Nonoptional } from "./nonoptional.js";
import { PaddedZoneSource } from "./paddedZoneSource.js";
import { TouchEventEngine } from "./touchEventEngine.js";
import { TrackedInput } from "./trackedInput.js";
import { TrackedPoint } from "./trackedPoint.js";

// Documents the types of events that GestureRecognizer supports.
interface EventMap {
  'inputstart': (input: TrackedInput) => any
}

export class GestureRecognizer extends EventEmitter<EventMap> {
  public readonly config: Nonoptional<GestureRecognizerConfiguration>;

  private readonly mouseEngine: MouseEventEngine;
  private readonly touchEngine: TouchEventEngine;

  private _activeInputs: {[id: string]: TrackedInput} = {};

  protected static preprocessConfig(config: GestureRecognizerConfiguration): Nonoptional<GestureRecognizerConfiguration> {
    // Allows configuration pre-processing during this method.
    let processingConfig: Mutable<Nonoptional<GestureRecognizerConfiguration>> = {...config} as Nonoptional<GestureRecognizerConfiguration>;
    processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
    processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

    processingConfig.inputStartBounds = processingConfig.inputStartBounds ?? processingConfig.targetRoot;
    processingConfig.maxRoamingBounds = processingConfig.maxRoamingBounds ?? processingConfig.targetRoot;
    processingConfig.safeBounds       = processingConfig.safeBounds       ?? new PaddedZoneSource(2);

    if(!config.paddedSafeBounds) {
      let paddingArray = config.safeBoundPadding;
      if(typeof paddingArray == 'number') {
        paddingArray = [ paddingArray ];
      }
      paddingArray = paddingArray ?? [3];

      processingConfig.paddedSafeBounds = new PaddedZoneSource(processingConfig.safeBounds, ...paddingArray as number[]);
    } else {
      // processingConfig.paddedSafeBounds is already set via the spread operator above.
      delete processingConfig.safeBoundPadding;
    }

    return processingConfig;
  }

  public constructor(config: GestureRecognizerConfiguration) {
    super();
    this.config = GestureRecognizer.preprocessConfig(config);

    this.mouseEngine = new MouseEventEngine(this.config);
    this.touchEngine = new TouchEventEngine(this.config);

    this.mouseEngine.registerEventHandlers();
    this.touchEngine.registerEventHandlers();

    const forwardingUpdateHandler = (touchpoint: TrackedPoint) => {
      const newInput = new TrackedInput(touchpoint);
      this._activeInputs[touchpoint.identifier] = newInput;

      this.emit('inputstart', newInput);
      return false;
    }

    this.mouseEngine.on('pointstart', forwardingUpdateHandler);
    this.touchEngine.on('pointstart', forwardingUpdateHandler);
  }

  public destroy() {
    this.mouseEngine.unregisterEventHandlers();
    this.touchEngine.unregisterEventHandlers();

    // Because these two fields are marked readonly, we can't directly delete them.
    // Because they're private, we can't apply Mutable to make them deletable.
    // So... awkward cast + assignment it is.
    (this.mouseEngine as any) = null;
    (this.touchEngine as any) = null;
  }
}