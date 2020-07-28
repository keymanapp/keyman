/// <reference path="../../node_modules/@keymanapp/recorder-core/src/index.ts" />

namespace KMWRecorder {
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

    simulateEvent(eventSpec: InputEventSpec) {
      switch(eventSpec.type) {
        case "key":
          this.simulateHardwareEvent(eventSpec as PhysicalInputEventSpec);
          break;
        case "osk":
          this.simulateOSKEvent(eventSpec as OSKInputEventSpec);
          break;
      }
    }

    simulateHardwareEvent(eventSpec: PhysicalInputEventSpec) {
      var event: Event;

      // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
      if(typeof Event == 'function') {
        event = new Event(BrowserDriver.physicalEventType);
        event['key'] = eventSpec.key;
        event['code'] = eventSpec.code;
        event['keyCode'] = eventSpec.keyCode;
        event['location'] = eventSpec.location;
        event['getModifierState'] = eventSpec.getModifierState.bind(eventSpec);
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        event = document.createEvent(BrowserDriver.physicalEventClass);
        // An override to ensure that IE's method gets called.
        // Many thanks to https://gist.github.com/termi/4654819, line 142 at the time of writing this.
        var success = (<any>event).initKeyboardEvent(BrowserDriver.physicalEventType, false, true, null, eventSpec.key, /*this.code,*/ eventSpec.location, 
          eventSpec.generateModifierString(), 0, 0);
      }

      this.target.dispatchEvent(event);
    }

    simulateOSKEvent(eventSpec: OSKInputEventSpec) {
      let target = this.target;
      let oskKeyElement = document.getElementById(eventSpec.keyID);

      if(!oskKeyElement) {
        console.error('Could not find OSK key "' + eventSpec.keyID + '"!');
        // The following lines will throw an appropriate-enough error.
        return;
      }

      // To be safe, we replicate the MouseEvent similarly to the keystroke event.
      var downEvent;
      var upEvent;
      if(typeof Event == 'function') {
        if(target['base'] && target instanceof HTMLDivElement) {
          downEvent = new Event(BrowserDriver.oskDownTouchType);
          upEvent = new Event(BrowserDriver.oskUpTouchType);
          downEvent['touches'] = [{"target": oskKeyElement}];
          upEvent['touches'] = [{"target": oskKeyElement}];
          downEvent['changedTouches'] = [{"target": oskKeyElement}];
          upEvent['changedTouches'] = [{"target": oskKeyElement}];
        } else {
          downEvent = new Event(BrowserDriver.oskDownMouseType);
          upEvent = new Event(BrowserDriver.oskUpMouseType);
          downEvent['relatedTarget'] = target;
          upEvent['relatedTarget'] = target;
        }
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        downEvent = document.createEvent(BrowserDriver.oskEventClass);
        downEvent.initMouseEvent(BrowserDriver.oskDownMouseType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);

        upEvent = document.createEvent(BrowserDriver.oskEventClass);
        upEvent.initMouseEvent(BrowserDriver.oskUpMouseType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);
      }

      oskKeyElement.dispatchEvent(downEvent);
      oskKeyElement.dispatchEvent(upEvent);
    }

    // Execution of a test sequence depends on the testing environment; integrated
    // testing requires browser-specific code.
    simulateSequence(sequence: InputEventSpecSequence): string {
      let ele = this.target;

      for(var i=0; i < sequence.inputs.length; i++) {
        this.simulateEvent(sequence.inputs[i]);
      }

      if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
        return ele.value;
      } else {
        return ele.textContent;
      }
    }
  }
}