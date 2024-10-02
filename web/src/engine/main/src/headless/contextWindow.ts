import { CasingForm, Configuration, Context } from '@keymanapp/common-types';
import { Mock } from "keyman/engine/js-processor";

export default class ContextWindow implements Context {
  // Used to limit the range of context replicated for use of keyboard rules within
  // the engine, as used for fat-finger prep / `Alternate` generation.
  public static readonly ENGINE_RULE_WINDOW: Configuration = {
    leftContextCodePoints: 64,
    rightContextCodePoints: 32
  };

  left: string;
  right?: string;

  startOfBuffer: boolean;
  endOfBuffer: boolean;

  casingForm?: CasingForm;

  constructor(mock: Mock, config: Configuration, layerId: string) {
    this.left = mock.getTextBeforeCaret();
    this.startOfBuffer = this.left._kmwLength() <= config.leftContextCodePoints;
    if(!this.startOfBuffer) {
      // Our custom substring version will return the last n characters if param #1 is given -n.
      this.left = this.left._kmwSubstr(-config.leftContextCodePoints);
    }

    this.right = mock.getTextAfterCaret();
    this.endOfBuffer = this.right._kmwLength() <= config.rightContextCodePoints;
    if(!this.endOfBuffer) {
      this.right = this.right._kmwSubstr(0, config.rightContextCodePoints);
    }

    this.casingForm =
      layerId == 'shift' ? 'initial' :
      layerId == 'caps' ? 'upper' :
      null;
  }

  public toMock(): Mock {
    let caretPos = this.left._kmwLength();

    return new Mock(this.left + (this.right || ""), caretPos);
  }
}