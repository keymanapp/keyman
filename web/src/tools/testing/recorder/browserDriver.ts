import {
  InputEventSpec,
  InputEventSpecSequence,
  OSKInputEventSpec,
  PhysicalInputEventSpec
} from "@keymanapp/recorder-core";
import { ManagedPromise, timedPromise } from "@keymanapp/web-utils";

import { type KeymanEngine } from 'keyman/app/browser';

declare var keyman: KeymanEngine;

export type Mutable<Type> = {
  -readonly [Property in keyof Type]: Type[Property];
};

function asTouchList(arr: any[]) {
  return {
    get length() {
      return arr.length;
    },

    item(index: number) {
      return arr[index];
    }
  }
}
export class BrowserDriver {
  static readonly physicalEventClass: string = "KeyboardEvent";
  static readonly physicalEventType: string = "keydown";

  static readonly oskEventClass: string = "MouseEvent";
  static readonly oskDownMouseType: string = "mousedown";
  static readonly oskUpMouseType: string = "mouseup";
  static readonly oskDownTouchType: string = "touchstart";
  static readonly oskUpTouchType: string = "touchend";

  /** The destination element for use in simulation. */
  private target: HTMLElement;

  constructor(target: HTMLElement) {
    this.target = target;
  }

  async simulateEvent(eventSpec: InputEventSpec) {
    switch(eventSpec.type) {
      case "key":
        this.simulateHardwareEvent(eventSpec as PhysicalInputEventSpec);
        break;
      case "osk":
        const oskEventSpec = eventSpec as OSKInputEventSpec;

        // Undocumented api stuff:  ensure the layer has been built before
        // force-triggering the key, as it may not exist otherwise!
        //
        // Some unit tests bypass emitting keys that change layer, so we set it
        // directly here.
        const keyID = oskEventSpec.keyID;
        const keyIDPieces = keyID.split('-');
        if(keyIDPieces.length > 1) {
          // Drop the actual 'key' part of the key-id, leaving layer components
          // intact.
          keyIDPieces.pop();
          keyman.osk.vkbd.layerId = keyIDPieces.join('-');
        }

        await this.simulateOSKEvent(eventSpec as OSKInputEventSpec);
        break;
    }
  }

  simulateHardwareEvent(eventSpec: PhysicalInputEventSpec) {
    // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
    let event: Mutable<Partial<KeyboardEvent>> = new Event(BrowserDriver.physicalEventType);
    event['key'] = eventSpec.key;
    event['code'] = eventSpec.code;
    event['keyCode'] = eventSpec.keyCode;
    event['location'] = eventSpec.location;
    event['getModifierState'] = eventSpec.getModifierState.bind(eventSpec);

    this.target.dispatchEvent(event as KeyboardEvent);
  }

  async simulateOSKEvent(eventSpec: OSKInputEventSpec) {
    const originalLayer = keyman.osk.vkbd.layerId;

    // Calculations go wrong if the key's layer is not visible.
    const keyID = eventSpec.keyID;
    const targetLayer = keyID.indexOf('-') == -1 ? keyID : keyID.substring(0, keyID.lastIndexOf('-'));

    if(targetLayer != originalLayer) {
      keyman.osk.vkbd.layerGroup.layers[originalLayer].element.style.display = 'none';
      keyman.osk.vkbd.layerGroup.layers[targetLayer].element.style.display = 'block';
      keyman.osk.vkbd.layerId = targetLayer;

      // Only the "current" layer of the OSK is laid out on refresh; a non-default
      // layer won't have proper layout before this!
      keyman.osk.vkbd.refreshLayout();
    }

    try {
      let target = this.target;
      let oskKeyElement = document.getElementById(eventSpec.keyID);
      const boundingBox = oskKeyElement.getBoundingClientRect();
      const center = {
        clientX: boundingBox.left + boundingBox.width/2,
        clientY: boundingBox.top + boundingBox.height/2
      }

      if(!oskKeyElement) {
        console.error('Could not find OSK key "' + eventSpec.keyID + '"!');
        // The following lines will throw an appropriate-enough error.
        return;
      }

      // To be safe, we replicate the MouseEvent similarly to the keystroke event.
      let downEvent: Event;
      var upEvent: Event;
      if(keyman.config.hostDevice.touchable) {
        let touchDownEvent: Mutable<Partial<TouchEvent>>;
        let touchUpEvent: Mutable<Partial<TouchEvent>>;
        touchDownEvent = new Event(BrowserDriver.oskDownTouchType);
        touchUpEvent = new Event(BrowserDriver.oskUpTouchType);
        touchDownEvent['touches'] = asTouchList([{"target": oskKeyElement, ...center}]);
        // The touch should NOT show up in event.touches when a touch ends.
        touchUpEvent['touches'] = asTouchList([]);
        touchDownEvent['changedTouches'] = asTouchList([{"target": oskKeyElement, ...center}]);
        // It should still show up in .changedTouches, though.
        touchUpEvent['changedTouches'] = asTouchList([{"target": oskKeyElement, ...center}]);

        downEvent = touchDownEvent as Event;
        upEvent = touchUpEvent as Event;
      } else {
        let mouseDownEvent: Mutable<Partial<MouseEvent>>;
        let mouseUpEvent: Mutable<Partial<MouseEvent>>;
        mouseDownEvent = new Event(BrowserDriver.oskDownMouseType);
        mouseUpEvent = new Event(BrowserDriver.oskUpMouseType);
        mouseDownEvent.clientX = center.clientX;
        mouseDownEvent.clientY = center.clientY;
        mouseDownEvent['relatedTarget'] = target;
        mouseUpEvent.clientX = center.clientX;
        mouseUpEvent.clientY = center.clientY;
        mouseUpEvent['relatedTarget'] = target;
        // Mouse-click driven OSK use involves use of at least one mouse button.
        mouseDownEvent['button'] = mouseUpEvent['button'] = 0;
        mouseDownEvent['buttons'] = 1;
        mouseUpEvent['buttons'] = 0;

        downEvent = mouseDownEvent as Event;
        upEvent = mouseUpEvent as Event;
      }

      // Note:  our gesture engine's internal structure means that even simple keystrokes like this
      // involve async processing.  We'll need to sync up.
      const defermentPromise = new ManagedPromise<void>();

      // Easiest way to resolve?  Just wait for the key event.  Currently, our integration tests
      // at this level only use simple-taps, so there shouldn't be any cases that emit multiple
      // key events at this time.
      //
      // Certain key events (modifier keys) do trigger on 'keydown', so we establish this handler
      // first.
      keyman.osk.once('keyevent', () => { defermentPromise.resolve() });


      // Allow the 'touchdown' event to resolve fully before proceeding.  There is a bit
      // of asynchronicity involved within the gesture-recognition engine.
      oskKeyElement.dispatchEvent(downEvent);
      await timedPromise(0);

      oskKeyElement.dispatchEvent(upEvent);

      // Just in case something goes wrong and no event occurs, we apply a timeout to keep
      // the tests moving along.
      await Promise.race([defermentPromise.corePromise, timedPromise(50)]);
      if(!defermentPromise.isFulfilled) {
        // Acts as a 'canary' that something is wrong.
        const sourceType = keyman.config.hostDevice.touchable ? 'touch' : 'click';
        throw new Error(`No 'keyevent' detected for the specified ${sourceType} targeting key ${eventSpec.keyID}!`);
      }
    } finally {
      // The alt-layer needs to be maintained until the key is generated.
      if(targetLayer != originalLayer) {
        keyman.osk.vkbd.layerGroup.layers[targetLayer].element.style.display = 'none';
        keyman.osk.vkbd.layerGroup.layers[originalLayer].element.style.display = 'block';
        keyman.osk.vkbd.layerId = originalLayer;
        keyman.osk.vkbd.refreshLayout();
      }
    }

      // Ensure any lingering gesture-system-internal Promises have a chance to clear.
      // (Timeouts use the 'macrotask' queue, which waits for all pending microtasks to clear first.)
      // Certain tests (involving layer-shift keys & AltGr emulation) need the extra chance.
      await timedPromise(0);
  }

  // Execution of a test sequence depends on the testing environment; integrated
  // testing requires browser-specific code.
  async simulateSequence(sequence: InputEventSpecSequence): Promise<string> {
    let ele = this.target;

    for(var i=0; i < sequence.inputs.length; i++) {
      await this.simulateEvent(sequence.inputs[i]);
    }

    if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
      return ele.value;
    } else {
      return ele.textContent;
    }
  }
}