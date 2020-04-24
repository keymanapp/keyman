/// <reference path="recorder_InputEvents.ts" />

namespace KMWRecorder {
  /**
   * Contains browser-dependent code used to transcribe browser-based events 
   * so that thay may be reconstructed for use in KMW testing. 
   */
  export class Scribe {
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
  }
}