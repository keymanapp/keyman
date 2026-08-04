import { LexicalModelTypes } from '@keymanapp/common-types';
import { SyntheticTextStore } from "keyman/engine/keyboard";
import { KMWString } from 'keyman/common/web-utils';

export class ContextWindow implements LexicalModelTypes.Context {
  // Used to limit the range of context replicated for use of keyboard rules within
  // the engine, as used for fat-finger prep / `Alternate` generation.
  public static readonly ENGINE_RULE_WINDOW: LexicalModelTypes.Configuration = {
    leftContextCodePoints: 64,
    rightContextCodePoints: 32
  };

  left: string;
  right?: string;

  startOfBuffer: boolean;
  endOfBuffer: boolean;

  casingForm?: LexicalModelTypes.CasingForm;

  constructor(textStore: SyntheticTextStore, config: LexicalModelTypes.Configuration, layerId: string) {
    this.left = textStore.getTextBeforeCaret();
    this.startOfBuffer = KMWString.length(this.left) <= config.leftContextCodePoints;
    if(!this.startOfBuffer) {
      // Our custom substring version will return the last n characters if param #1 is given -n.
      this.left = KMWString.substr(this.left, -config.leftContextCodePoints);
    }

    this.right = textStore.getTextAfterCaret();
    this.endOfBuffer = KMWString.length(this.right) <= config.rightContextCodePoints;
    if(!this.endOfBuffer) {
      this.right = KMWString.substr(this.right, 0, config.rightContextCodePoints);
    }

    this.casingForm =
      layerId == 'shift' ? 'initial' :
      layerId == 'caps' ? 'upper' :
      null;
  }

  public toSyntheticTextStore(): SyntheticTextStore {
    const caretPos = KMWString.length(this.left);

    return new SyntheticTextStore(this.left + (this.right || ""), caretPos);
  }
}