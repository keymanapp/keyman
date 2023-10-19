import GlobeHint from "../globehint.interface.js";
import PendingGesture from "../input/gestures/pendingGesture.interface.js";
import { KeyElement } from "../keyElement.js";
import KeyTip from "../keytip.interface.js";
import VisualKeyboard from "../visualKeyboard.js";

export default interface EmbeddedGestureConfig {
  createGlobeHint?: (vkbd: VisualKeyboard) => GlobeHint;
}