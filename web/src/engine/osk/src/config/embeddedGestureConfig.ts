import GlobeHint from "../globehint.interface.js";
import VisualKeyboard from "../visualKeyboard.js";

export default interface EmbeddedGestureConfig {
  createGlobeHint?: (vkbd: VisualKeyboard) => GlobeHint;
}