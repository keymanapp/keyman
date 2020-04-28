/// <reference path="recorder_InputEvents.ts" />
/// <reference path="../../node_modules/eventemitter3/index.js" />

//Since TS won't recognize the types b/c no "import"/"require" statements.
// A small-scale manual definition.
declare class EventEmitter {
  /** Add a listener for a given event */
  on(event: string, func: (...args: any[]) => boolean, context?: any);
  /** Add a one-time listener for a given event */
  once(event: string, func: (...args: any[]) => boolean, context?: any);
  removeListener(event: string, func: (...args: any[]) => boolean, context?: any, once?: boolean);

  // Defines their alternately-themed aliases.
  addListener: typeof EventEmitter.prototype.on;
  off: typeof EventEmitter.prototype.removeListener;

  // Defines the actual event-raising function.
  emit(eventName: string, ...args: any[]);
}  

namespace KMWRecorder {
  /**
   * Contains browser-dependent code used to transcribe browser-based events 
   * so that thay may be reconstructed for use in KMW testing. 
   */
  export class Scribe extends EventEmitter {
    //#region Static methods for recording input events
    static recordKeyboardEvent(e: KeyboardEvent): PhysicalInputEvent {
      let recording = new PhysicalInputEvent();

      recording.key    = e.key;
      recording.code    = e.code;
      recording.keyCode = e.keyCode;
      recording.location = e.location;

      var flagSet: number = 0;

      for(var key in PhysicalInputEvent.modifierCodes) {
        if(e.getModifierState(key)) {
          flagSet |= PhysicalInputEvent.modifierCodes[key];
        }
      }

      recording.modifierSet = flagSet;

      return recording;
    }

    static recordOSKEvent(e: HTMLDivElement): OSKInputEvent {
      let recording = new OSKInputEvent();
      recording.keyID = e.id;
      return recording;
    }

    static recordKeyboardStub(activeStub: any, basePath: string): KeyboardStub {
      let recording = new KeyboardStub();

      recording.id = activeStub.KI;
      recording.id = recording.id.replace('Keyboard_', '');

      recording.name = activeStub.KN;
      recording.filename = Scribe._setStubBasePath(activeStub.KF, basePath);

      recording.languages = [new LanguageStubForKeyboard(activeStub)];

      return recording;
    }

    private static _setStubBasePath(filename: string, filePath: string, force?: boolean): string {
      var linkParser = document.createElement<"a">("a");
      linkParser.href = filePath;

      if(force === undefined) {
        force = true;
      }

      if(force || (filename.indexOf(linkParser.protocol) < 0 && filename.indexOf('/') != 0)) {
        var file = filename.substr(filename.lastIndexOf('/')+1);
        return filePath + '/' + file;
      } else {
        return file;
      }
    }
    //#endregion

    // TODO:  rename variable to something better.  Currently preserved for easier refactoring comparisons.
    inputJSON: InputTestSequence = new InputTestSequence();
    testDefinition: KeyboardTest = new KeyboardTest();

    addInputRecord(json: InputEvent, currentOutput: string) {
      this.inputJSON.addInput(json, currentOutput);
      this.raiseRecordChanged();
    }

    resetInputRecord() {
      window['keyman'].resetContext();
      this.inputJSON = new KMWRecorder.InputTestSequence();

      this.emit('record-reset', null);
    }

    setInputRecord(record: InputTestSequence) {
      this.inputJSON = record;
      this.raiseRecordChanged();
    }

    errorUpdate(msg: string) {
      if(msg) {
        this.inputJSON.msg = msg;
      } else {
        delete this.inputJSON.msg;
      }
    
      this.raiseRecordChanged();
    }

    private raiseRecordChanged() {
      this.emit('record-changed', this.inputJSON.toPrettyJSON());
    }

    saveInputRecord(config: Constraint) {
      if(this.inputJSON.inputs.length > 0) {
        this.testDefinition.addTest(config, this.inputJSON);
      }
      this.resetInputRecord();
      this.raiseTestChanged();
    }

    setTestDefinition(testDef: KeyboardTest) {
      if(!testDef) {
        testDef = new KeyboardTest();
      }
      this.testDefinition = testDef;
      this.raiseTestChanged();
    }

    private raiseTestChanged() {
      this.emit('test-changed', this.testDefinition ? JSON.stringify(this.testDefinition, null, '  ') : '');
    }
  }
}