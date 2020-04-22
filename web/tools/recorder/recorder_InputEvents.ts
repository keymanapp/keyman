// Includes KeymanWeb's Device class, as it's quite useful for ensuring that we target our tests correctly
// to each device.
/// <reference path="../../source/kmwdevice.ts" />

namespace KMWRecorder {
  import Device = com.keyman.Device;
  
  type AssertCallback = (s1: any, s2: any, msg?: string) => void;

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

    toPrettyJSON(): string {
      // We want the default, non-spaced JSON for this class, even when otherwise adding whitespace.
      var str = JSON.stringify(this);
      return str;
    }
  }

  export class PhysicalInputEvent extends InputEvent {
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
      super();
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
          list += ((list != "" ? " " : "") + key);
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
        // Many thanks to https://gist.github.com/termi/4654819, line 142 at the time of writing this.
        var success = (<any>event).initKeyboardEvent(PhysicalInputEvent.eventType, false, true, null, this.key, /*this.code,*/ this.location, 
          this.generateModifierString(), 0, 0);
      }

      ele.dispatchEvent(event);
    }
  }

  export class OSKInputEvent extends InputEvent {
    static readonly eventClass: string = "MouseEvent";
    static readonly downMouseType: string = "mousedown";
    static readonly upMouseType: string = "mouseup";
    static readonly downTouchType: string = "touchstart";
    static readonly upTouchType: string = "touchend";

    type: string = "osk";
    keyID: string;

    // osk.clickKey receives the element clicked or touched in OSK interactions.
    constructor(ele: HTMLDivElement|OSKInputEvent) {
      super();
      if(ele instanceof HTMLDivElement) {
        this.keyID = ele.id;
      } else {
        this.keyID = ele.keyID;
      }
    }

    simulateEventOn(target: HTMLElement) {
      var oskKeyElement = document.getElementById(this.keyID);

      if(!oskKeyElement) {
        console.error('Could not find OSK key "' + this.keyID + '"!');
        // The following lines will throw an appropriate-enough error.
        return;
      }

      // To be safe, we replicate the MouseEvent similarly to the keystroke event.
      var downEvent;
      var upEvent;
      if(typeof Event == 'function') {
        if(target['base'] && target instanceof HTMLDivElement) {
          downEvent = new Event(OSKInputEvent.downTouchType);
          upEvent = new Event(OSKInputEvent.upTouchType);
          downEvent['touches'] = [{"target": oskKeyElement}];
          upEvent['touches'] = [{"target": oskKeyElement}];
          downEvent['changedTouches'] = [{"target": oskKeyElement}];
          upEvent['changedTouches'] = [{"target": oskKeyElement}];
        } else {
          downEvent = new Event(OSKInputEvent.downMouseType);
          upEvent = new Event(OSKInputEvent.upMouseType);
          downEvent['relatedTarget'] = target;
          upEvent['relatedTarget'] = target;
        }
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        downEvent = document.createEvent(OSKInputEvent.eventClass);
        downEvent.initMouseEvent(OSKInputEvent.downMouseType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);

        upEvent = document.createEvent(OSKInputEvent.eventClass);
        upEvent.initMouseEvent(OSKInputEvent.upMouseType, false, true, null,
          null, 0, 0, 0, 0,
          false, false, false, false,
          0, oskKeyElement);
      }

      oskKeyElement.dispatchEvent(downEvent);
      oskKeyElement.dispatchEvent(upEvent);
    }
  }

  var resetElement = function(ele: HTMLElement):void {
    if(ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
      window['keyman'].resetContext();
      ele.value = "";
    } else {
      window['keyman'].resetContext();
      if(ele['base']) {
        // Gotta be extra-careful with the simulated touch fields!
        (<any> ele /*as com.keyman.dom.TouchAliasElement*/).setText("", 0);
      } else {
        ele.textContent = "";
      }
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

    addInput(event: InputEvent, output: string) {
      this.inputs.push(event);
      this.output = output;
    }

    simulateSequenceOn(ele: HTMLElement, assertCallback?: AssertCallback): {success: boolean, result: string} {
      resetElement(ele);

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

      return {success: (result == this.output), result: result};
    }

    toPrettyJSON(): string {
      var str = "{ ";
      if(this.output) {
        str += "\"output\": \"" + this.output + "\", ";
      }
      str += "\"inputs\": [\n";
      for(var i = 0; i < this.inputs.length; i++) {
        str += "  " + this.inputs[i].toPrettyJSON() + ((i == this.inputs.length-1) ? "\n" : ",\n");
      }
      if(this.msg) {
        str += "], \"message\": \"" + this.msg + "\" }";
      } else {
        str += "]}";
      }
      return str;
    }

    hasOSKInteraction(): boolean {
      for(var i=0; i < this.inputs.length; i++) {
        if(this.inputs[i] instanceof OSKInputEvent) {
          return true;
        }
      }

      return false;
    }
  }

  class FontStubForLanguage {
    family: string;
    source: string[];

    constructor(activeStubEntry: any) {
      this.family = activeStubEntry.family;

      var src = activeStubEntry.files;
      if(!(src instanceof Array)) {
        src = [ src ];
      }

      this.source = [];
      for(var i=0; i < src.length; i++) {
        this.source.push(activeStubEntry.path + src[i]);
      }
    }
  }

  class LanguageStubForKeyboard {
    id: string;
    name: string;
    region: string;
    font?: FontStubForLanguage;
    oskFont?: FontStubForLanguage;

    constructor(activeStub: any) {
      if(activeStub.KLC) {
        this.id = activeStub.KLC;
        this.name = activeStub.KL;
        this.region = activeStub.KR;
  
        // Fonts.
        if(activeStub.KFont) {
          this.font = new FontStubForLanguage(activeStub.KFont);
        }
        if(activeStub.KOskFont) {
          this.oskFont = new FontStubForLanguage(activeStub.KOskFont);
        }
      } else {
        this.id = activeStub.id;
        this.name = activeStub.name;
        this.region = activeStub.region;

        // If we end up adding functionality to FontStubForLanguage, we'll need to properly reconstruct these.
        this.font = activeStub.font;
        this.oskFont = activeStub.oskFont;
      }
    }
  }

  export class KeyboardStub {
    id: string;
    name: string;
    filename: string;
    languages: LanguageStubForKeyboard | LanguageStubForKeyboard[];

    // Constructs a stub usable with KeymanWeb's addKeyboards() API function from
    // the internally-tracked ActiveStub value for that keyboard.
    constructor(activeStub: any) {
      if(activeStub.KI) {
        this.id = activeStub.KI;
        this.id = this.id.replace('Keyboard_', '');
  
        this.name = activeStub.KN;
        this.filename = activeStub.KF;
  
        this.languages = [new LanguageStubForKeyboard(activeStub)];
      } else {
        this.id = activeStub.id;
        this.name = activeStub.name;
        this.filename = activeStub.filename;

        if(activeStub.languages instanceof Object) {
          this.languages = new LanguageStubForKeyboard(activeStub.languages);
        } else {
          this.languages = [];
          for(var i=0; i < activeStub.languages.length; i++) {
            this.languages.push(new LanguageStubForKeyboard(activeStub.languages[i]));
          }
        }
      }
    }

    setBasePath(filePath: string, force?: boolean) {
      var linkParser = document.createElement<"a">("a");
      linkParser.href = filePath;

      if(force === undefined) {
        force = true;
      }

      if(force || (this.filename.indexOf(linkParser.protocol) < 0 && this.filename.indexOf('/') != 0)) {
        var file = this.filename.substr(this.filename.lastIndexOf('/')+1);
        this.filename = filePath + '/' + file;
      }
    }

    getFirstLanguage() {
      if(this.languages instanceof LanguageStubForKeyboard) {
        return this.languages.id;
      } else {
        return this.languages[0].id;
      }
    }
  }

  type TARGET = 'hardware'|'desktop'|'phone'|'tablet';
  type OS = 'windows'|'android'|'ios'|'macosx'|'linux';
  type BROWSER = 'ie'|'chrome'|'firefox'|'safari'|'opera';  // ! no 'edge' detection in KMW!

  export class Constraint {
    target: TARGET;
    validOSList?: OS[];
    validBrowsers?: BROWSER[];

    constructor(target: TARGET|Constraint, validOSList?: OS[], validBrowsers?: BROWSER[]) {
      if(typeof(target) == 'string') {
        this.target = target;
        this.validOSList = validOSList;
        this.validBrowsers = validBrowsers;
      } else {
        var json = target;
        this.target = json.target;
        this.validOSList = json.validOSList;
        this.validBrowsers = json.validBrowsers;
      }
    }

    matchesClient(device: Device, usingOSK?: boolean) {
      // #1:  Platform check.
      if(usingOSK === true) {
        if(this.target != device.formFactor) {
          return false;
        }
      } else if(usingOSK === false) {
        if(this.target != 'hardware') {
          return false;
        }
      } else if(this.target != device.formFactor && this.target != 'hardware') {
        return false;
      }

      if(this.validOSList) {
        if(this.validOSList.indexOf(device.OS as OS) == -1) {
          return false;
        }
      }

      if(this.validBrowsers) {
        if(this.validBrowsers.indexOf(device.browser as BROWSER) == -1) {
          return false;
        }
      }

      return true;
    }

    // Checks if another Constraint instance is functionally identical to this one.
    equals(other: Constraint) {
      if(this.target != other.target) {
        return false;
      }

      var list1 = this.validOSList ? this.validOSList : ['any'];
      var list2 = other.validOSList ? other.validOSList : ['any'];

      if(list1.sort().join(',') != list2.sort().join(',')) {
        return false;
      }

      list1 = this.validBrowsers ? this.validBrowsers : ['web'];
      list2 = other.validBrowsers ? other.validBrowsers : ['web'];

      if(list1.sort().join(',') != list2.sort().join(',')) {
        return false;
      }

      return true;
    }
  }

  class TestFailure {
    constraint: Constraint;
    test: InputTestSequence;
    result: string;

    constructor(constraint: Constraint, test: InputTestSequence, output: string) {
      this.constraint = constraint;
      this.test = test;
      this.result = output;
    }
  }

  class InputTestSet {
    constraint: Constraint;
    testSet: InputTestSequence[];

    constructor(constraint: Constraint|InputTestSet) {
      if("target" in constraint) {
        this.constraint = constraint as Constraint;
        this.testSet = [];
      } else {
        var json = constraint as InputTestSet;
        this.constraint = new Constraint(json.constraint);
        this.testSet = [];

        // Clone each test sequence / reconstruct from methodless JSON object.
        for(var i=0; i < json.testSet.length; i++) {
          this.testSet.push(new InputTestSequence(json.testSet[i]));
        }
      }
    }

    addTest(seq: InputTestSequence) {
      this.testSet.push(seq);
    }

    // Validity should be checked before calling this method.
    run(ele: HTMLElement, assertCallback?: AssertCallback): TestFailure[] {
      var failures: TestFailure[] = [];

      for(var i=0; i < this.testSet.length; i++) {
        var testSeq = this.testSet[i];
        var simResult = testSeq.simulateSequenceOn(ele, assertCallback);
        if(!simResult.success) {
          // Failed test!
          failures.push(new TestFailure(this.constraint, testSeq, simResult.result));
        }
      }

      return failures.length > 0 ? failures : null;
    }

    // Used to determine if the current InputTestSet is applicable to be run on a device.
    isValidForCurrentClient(usingOSK?: boolean) {
      var device: Device = new Device();
      device.detect();

      return this.constraint.matchesClient(device, usingOSK);
    }
  }

  export class KeyboardTest {
    /**
     * The stub information to be passed into keyman.addKeyboards() in order to run the test.
     */
    keyboard: KeyboardStub;

    /**
     * The master array of test sets, each of which specifies constraints a client must fulfill for
     * the tests contained therein to be valid.
     */
    inputTestSets: InputTestSet[];

    /**
     * Reconstructs a KeyboardTest object from its JSON representation, restoring its methods. 
     * @param fromJSON 
     */
    constructor(fromJSON: string|KeyboardStub|KeyboardTest) {
      if(!fromJSON) {
        this.keyboard = null;
        this.inputTestSets = [];
        return;
      } else if(typeof(fromJSON) == 'string') {
        fromJSON = JSON.parse(fromJSON);
      } else if(fromJSON instanceof KeyboardStub) {
        this.keyboard = fromJSON;
        this.inputTestSets = [];
        return;
      }

      fromJSON = fromJSON as KeyboardTest;

      this.keyboard = new KeyboardStub(fromJSON.keyboard);
      this.inputTestSets = [];

      for(var i=0; i < fromJSON.inputTestSets.length; i++) {
        this.inputTestSets[i] = new InputTestSet(fromJSON.inputTestSets[i]);
      }
    }

    addTest(constraint: Constraint, seq: InputTestSequence) {
      for(var i=0; i < this.inputTestSets.length; i++) {
        if(this.inputTestSets[i].constraint.equals(constraint)) {
          this.inputTestSets[i].addTest(seq);
          return;
        }
      }

      var newSet = new InputTestSet(new Constraint(constraint));
      this.inputTestSets.push(newSet);
      newSet.addTest(seq);      
    }

    run(ele: HTMLElement, usingOSK?: boolean, assertCallback?: AssertCallback) {
      var setHasRun = false;
      var failures: TestFailure[] = [];

      window['keyman'].setActiveElement(ele['base'] ? ele['base'] : ele);

      for(var i = 0; i < this.inputTestSets.length; i++) {
        var testSet = this.inputTestSets[i];

        if(testSet.isValidForCurrentClient(usingOSK)) {
          var testFailures = testSet.run(ele, assertCallback);
          if(testFailures) {
            failures = failures.concat(testFailures);
          }
          setHasRun = true;
        }
      }

      if(!setHasRun) {
        // The sets CAN be empty, allowing silent failure if/when we actually want that.
        console.warn("No test sets for this keyboard were applicable for this device!");
      }

      // Allow the method's caller to trigger a 'fail'.
      if(failures.length > 0) {
        return failures;
      } else {
        return null;
      }
    }

    isEmpty() {
      return this.inputTestSets.length == 0;
    }
  }
}