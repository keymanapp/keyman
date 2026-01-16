import { Transcription } from "keyman/engine/js-processor";
import { RewindableCache } from "@keymanapp/web-utils";

const TRANSCRIPTION_BUFFER_SIZE = 10;

export class TranscriptionCache extends RewindableCache<Transcription> {
  constructor() {
    super(TRANSCRIPTION_BUFFER_SIZE);
  }

  public save(value: Transcription) {
    const key = value.token >= 0 ? value.token : -value.token;
    this.add(key, value);
  }

  public buildLog(): string {
    const entries = [...this.keys()].map((key) => {
      return {
        key: key,
        entry: this.get(key)
      }
    });

    return entries
      .map(({key, entry}) => `Context state ${key}'s keystroke:\n${entry.keystroke.inputBreadcrumb ?? ''}`)
      .join('\n');
  }
}