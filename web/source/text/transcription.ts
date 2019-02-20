///<reference path="keyEvent.ts" />
///<reference path="../includes/lmMsgs.d.ts" />

namespace com.keyman.text {
  export class Transcription {
    readonly token: number;
    readonly keystroke: KeyEvent;
    readonly transform: Transform;
    readonly preInput: Mock;
    readonly removedDks: Deadkey[];
    readonly insertedDks: Deadkey[];

    private static tokenSeed: number = 0;

    constructor(keystroke: KeyEvent, transform: Transform, preInput: Mock, removedDks: Deadkey[], insertedDks: Deadkey[]) {
      this.token = Transcription.tokenSeed++;

      this.keystroke = keystroke;
      this.transform = transform;
      this.preInput = preInput;
      this.removedDks = removedDks;
      this.insertedDks = insertedDks;
    }
  }
}