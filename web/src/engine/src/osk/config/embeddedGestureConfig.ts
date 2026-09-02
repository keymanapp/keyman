import { GlobeHint } from "../globehint.interface.js";
import { VisualKeyboard } from "../visualKeyboard.js";

export interface EmbeddedGestureConfig {
  createGlobeHint?: (vkbd: VisualKeyboard) => GlobeHint;
}