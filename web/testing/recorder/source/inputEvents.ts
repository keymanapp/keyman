namespace KMWRecorder {
  export abstract class InputEvent {
    abstract simulateEventOn(ele: HTMLElement): void;

    static fromJSONObject(obj: any): InputEvent {
      if(obj && obj.type) {
        if(obj.type == "key") {
          return new PhysicalInputEvent(obj);
        } else if(obj.type == "osk") {
          return new OSKInputEvent(obj);
        }
      } else {
        throw new SyntaxError("Error in JSON format corresponding to an InputEvent!");
      }
    }
  }

  export class PhysicalInputEvent implements InputEvent {
    static readonly eventClass: string = "KeyboardEvent";
    static readonly eventType: string = "keydown";

    static readonly modifierCodes: { [mod:string]: number } = {
      "Shift":0x0001,
      "Control":0x0002,
      "Alt":0x0004,
      "Meta":0x0008,
      "CapsLock":0x0010,
      "NumLock":0x0020,
      "ScrollLock":0x0040
    };

    // KeyboardEvent properties
    type: string = "key";
    key: string;
    code: string;
    keyCode: number;
    modifierSet: number;
    location: number;

    constructor(e: KeyboardEvent|PhysicalInputEvent) {
      // We condition upon newly-generated events, as a PhysicalInputEvent from JSON
      // will lack its proper prototype, etc.
      if(e instanceof KeyboardEvent) {
        this.key    = e.key;
        this.code    = e.code;
        this.keyCode = e.keyCode;
        this.modifierSet = this.compileModifierState(e);
        this.location = e.location;
      } else {
        this.key = e.key;
        this.code = e.code;
        this.keyCode = e.keyCode;
        this.modifierSet = e.modifierSet;
        this.location = e.location;
      }
    }

    private compileModifierState(e: KeyboardEvent): number {
      var flagSet: number = 0;

      for(var key in PhysicalInputEvent.modifierCodes) {
        if(e.getModifierState(key)) {
          flagSet |= PhysicalInputEvent.modifierCodes[key];
        }
      }

      return flagSet;
    }

    getModifierState(key: string): boolean {
      return (PhysicalInputEvent.modifierCodes[key] & this.modifierSet) != 0;
    }

    private generateModifierString(): string {
      var list: string = "";

      for(var key in PhysicalInputEvent.modifierCodes) {
        if(this.getModifierState(key)) {
          list += (key + list != "" ? " " : "");
        }
      }

      return list;
    }

    simulateEventOn(ele: HTMLElement) {
      var event: Event;

      // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
      var downEvent;
      if(typeof Event == 'function') {
        event = new Event(PhysicalInputEvent.eventType);
        event['key'] = this.key;
        event['code'] = this.code;
        event['keyCode'] = this.keyCode;
        event['location'] = this.location;
        event['getModifierState'] = this.getModifierState.bind(this);
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        event = document.createEvent(PhysicalInputEvent.eventClass);
        // An override to ensure that IE's method gets called.
        (<any>event).initKeyboardEvent(PhysicalInputEvent.eventType, false, true, null, this.key, this.code, this.location, 
          this.generateModifierString(), 0);
      }

      ele.dispatchEvent(event);
    }
  }

  export class OSKInputEvent implements InputEvent {
    static readonly eventClass: string = "MouseEvent";
    static readonly downEventType: string = "mousedown";
    static readonly upEventType: string = "mouseup";

    type: string = "osk";
    keyID: string;

    // osk.clickKey receives the element clicked or touched in OSK interactions.
    constructor(ele: HTMLDivElement|OSKInputEvent) {
      if(ele instanceof HTMLDivElement) {
        this.keyID = ele.id;
      } else {
        this.keyID = ele.keyID;
      }
    }

    simulateEventOn(target: HTMLElement) {
      var oskKeyElement = document.getElementById(this.keyID);

      // To be safe, we replicate the MouseEvent similarly to the keystroke event.
      var downEvent;
      var upEvent;
      if(typeof Event == 'function') {
        downEvent = new Event(OSKInputEvent.downEventType);
        upEvent = new Event(OSKInputEvent.upEventType);
        downEvent['relatedTarget'] = target;
        upEvent['relatedTarget'] = target;
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        downEvent = document.createEvent(OSKInputEvent.eventClass);
        downEvent.initMouseEvent(OSKInputEvent.downEventType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);

        upEvent = document.createEvent(OSKInputEvent.eventClass);
        upEvent.initMouseEvent(OSKInputEvent.upEventType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);
      }

      /* We hack KMW a little bit because the .focus method is insufficient;
       * it won't trigger if the tested browser doesn't have focus.
       * Only one can have focus when testing locally.
       */
      window['DOMEventHandlers'].states.lastActiveElement = target;

      oskKeyElement.dispatchEvent(downEvent);
      oskKeyElement.dispatchEvent(upEvent);
    }
  }

  export class InputTestSequence {
    inputs: InputEvent[];
    output: string;
    msg?: string;

    constructor(ins?: InputEvent[] | InputTestSequence, outs?: string, msg?: string) {
      if(ins) {
        if(ins instanceof Array) {
          this.inputs = [].concat(ins);
        } else {
          // We're constructing from existing JSON.
          this.inputs = [];

          for(var ie=0; ie < ins.inputs.length; ie++) {
            this.inputs.push(InputEvent.fromJSONObject(ins.inputs[ie]));
          }

          this.output = ins.output;
          this.msg = ins.msg;
          return;
        }
      } else {
        this.inputs = [];
      }

      if(outs) {
        this.output = outs;
      }

      if(msg) {
        this.msg = msg;
      }
    }

    simulateSequenceOn(ele: HTMLElement, assertCallback: (s1: any, s2: any, msg?: string) => void): boolean {
      if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
        window['keyman'].resetContext();
        ele.value = "";
      } else {
        window['keyman'].resetContext();
        ele.textContent = "";
      }

      for(var i=0; i < this.inputs.length; i++) {
        this.inputs[i].simulateEventOn(ele);
      }

      var result;
      if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
        result = ele.value;
      } else {
        result = ele.textContent;
      }

      if(assertCallback) {
        assertCallback(result, this.output, this.msg);
      }

      return result == this.output;
    }
  }
}