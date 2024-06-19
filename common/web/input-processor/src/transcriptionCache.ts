import { Transcription } from "@keymanapp/keyboard-processor";

const TRANSCRIPTION_BUFFER_SIZE = 10;

export class TranscriptionCache {
  private readonly map = new Map<number, Transcription>();

  public get(key: number) {
    const value = this.map.get(key);

    // Update the entry's 'age' / position in the keys() ordering.
    if(value) {
      this.save(value);
    }

    return value;
  }

  public save(value: Transcription) {
    const key = value.token >= 0 ? value.token : -value.token;

    // Resets the key's ordering in Map.keys.
    this.map.delete(key);
    this.map.set(key, value);

    if(this.map.size > TRANSCRIPTION_BUFFER_SIZE) {
      /* Deletes the oldest entry.  As per the specification of `Map.keys()`, the keys are in
       * insertion order.  The earlier `map.delete` call resets a key's position in the list,
       * ensuring index 0 corresponds to the entry least-recently referenced.
       *
       * See also:
       * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map/keys
       */
      this.map.delete(this.map.keys().next().value);
    }
  }
}