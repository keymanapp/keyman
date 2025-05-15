/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { OutputTargetInterface } from '../outputTargetInterface.js';
import { KeyEvent } from '../keyEvent.js';
import { Alternate, TextTransform } from './textTransform.js';

export class Transcription {
  readonly token: number;
  readonly keystroke: KeyEvent;
  readonly transform: TextTransform;
  alternates: Alternate[]; // constructed after the rest of the transcription.
  readonly preInput: OutputTargetInterface;

  private static tokenSeed: number = 0;

  constructor(keystroke: KeyEvent, transform: TextTransform, preInput: OutputTargetInterface, alternates?: Alternate[]) {
    const token = this.token = Transcription.tokenSeed++;

    this.keystroke = keystroke;
    this.transform = transform;
    this.alternates = alternates;
    this.preInput = preInput;

    this.transform.id = this.token;

    // Assign the ID to each alternate, as well.
    if (alternates) {
      alternates.forEach(function (alt) {
        alt.sample.id = token;
      });
    }
  }
}

