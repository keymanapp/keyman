import { RecognitionZoneSource } from "./recognitionZoneSource.js";

export class ViewportZoneSource implements RecognitionZoneSource {
  constructor() {}

  getBoundingClientRect(): DOMRect {
    // Viewport dimension detection is based on https://stackoverflow.com/a/8876069.
    return DOMRect.fromRect({
      y: 0,
      x: 0,
      height: Math.max(document.documentElement.clientWidth  || 0, window.innerWidth || 0),
      width:  Math.max(document.documentElement.clientHeight || 0, window.innerHeight || 0)
    });
  }
}