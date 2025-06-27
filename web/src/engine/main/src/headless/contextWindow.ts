import { LexicalModelTypes } from '@keymanapp/common-types';
import { Mock } from "keyman/engine/js-processor";
import { KMWString } from '@keymanapp/web-utils';

export default class ContextWindow implements LexicalModelTypes.Context {
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

  constructor(mock: Mock, config: LexicalModelTypes.Configuration, layerId: string) {
    this.left = mock.getTextBeforeCaret();
    this.startOfBuffer = KMWString.length(this.left) <= config.leftContextCodePoints;
    if(!this.startOfBuffer) {
      // Our custom substring version will return the last n characters if param #1 is given -n.
      this.left = KMWString.substr(this.left, -config.leftContextCodePoints);
    }

    this.right = mock.getTextAfterCaret();
    this.endOfBuffer = KMWString.length(this.right) <= config.rightContextCodePoints;
    if(!this.endOfBuffer) {
      this.right = KMWString.substr(this.right, 0, config.rightContextCodePoints);
    }

    this.casingForm =
      layerId == 'shift' ? 'initial' :
      layerId == 'caps' ? 'upper' :
      null;
  }

  public toMock(): Mock {
    const caretPos = KMWString.length(this.left);

    return new Mock(this.left + (this.right || ""), caretPos);
  }
}