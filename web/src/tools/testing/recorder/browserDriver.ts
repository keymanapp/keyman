import {
  InputEventSpec,
  InputEventSpecSequence,
  OSKInputEventSpec,
  PhysicalInputEventSpec
} from "@keymanapp/recorder-core";
import { ManagedPromise, timedPromise } from "@keymanapp/web-utils";

import { type KeymanEngine } from 'keyman/app/browser';

declare var keyman: KeymanEngine;

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
        await this.simulateOSKEvent(eventSpec as OSKInputEventSpec);
        break;
    }
  }

  simulateHardwareEvent(eventSpec: PhysicalInputEventSpec) {
    // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
    let event = new Event(BrowserDriver.physicalEventType);
    event['key'] = eventSpec.key;
    event['code'] = eventSpec.code;
    event['keyCode'] = eventSpec.keyCode;
    event['location'] = eventSpec.location;
    event['getModifierState'] = eventSpec.getModifierState.bind(eventSpec);

    this.target.dispatchEvent(event);
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
      var downEvent;
      var upEvent;
      if(keyman.config.hostDevice.touchable) {
        downEvent = new Event(BrowserDriver.oskDownTouchType);
        upEvent = new Event(BrowserDriver.oskUpTouchType);
        downEvent['touches'] = asTouchList([{"target": oskKeyElement, ...center}]);
        // The touch should NOT show up in event.touches when a touch ends.
        upEvent['touches'] = asTouchList([]);
        downEvent['changedTouches'] = asTouchList([{"target": oskKeyElement, ...center}]);
        // It should still show up in .changedTouches, though.
        upEvent['changedTouches'] = asTouchList([{"target": oskKeyElement, ...center}]);
      } else {
        downEvent = new Event(BrowserDriver.oskDownMouseType);
        upEvent = new Event(BrowserDriver.oskUpMouseType);
        downEvent.clientX = center.clientX;
        downEvent.clientY = center.clientY;
        downEvent['relatedTarget'] = target;
        upEvent.clientX = center.clientX;
        upEvent.clientY = center.clientY;
        upEvent['relatedTarget'] = target;
        // Mouse-click driven OSK use involves use of at least one mouse button.
        downEvent['button'] = upEvent['button'] = 0;
        downEvent['buttons'] = 1;
        upEvent['buttons'] = 0;
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