import { getFontSizeStyle } from "../fontSizeUtils.js";

let metricsCanvas: HTMLCanvasElement;

/**
 * Uses canvas.measureText to compute and return the width of the given text of given font in pixels.
 *
 * @param {String} text The text to be rendered.
 * @param emScale The absolute `px` size expected to match `1em`.
 * @param {String} style The CSSStyleDeclaration for an element to measure against, without modification.
 *
 * @see https://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
 * This version has been substantially modified to work for this particular application.
 */
export function getTextMetrics(text: string, emScale: number, style: {fontFamily?: string, fontSize: string}): TextMetrics {
  // Since we may mutate the incoming style, let's make sure to copy it first.
  // Only the relevant properties, though.
  style = {
    fontFamily: style.fontFamily,
    fontSize: style.fontSize
  };

  // A final fallback - having the right font selected makes a world of difference.
  if(!style.fontFamily) {
    style.fontFamily = getComputedStyle(document.body).fontFamily;
  }

  if(!style.fontSize || style.fontSize == "") {
    style.fontSize = '1em';
  }

  let fontFamily = style.fontFamily;
  let fontSpec = getFontSizeStyle(style.fontSize);

  var fontSize: string;
  if(fontSpec.absolute) {
    // We've already got an exact size - use it!
    fontSize = fontSpec.val + 'px';
  } else {
    fontSize = fontSpec.val * emScale + 'px';
  }

  // re-use canvas object for better performance
  metricsCanvas = metricsCanvas ?? document.createElement("canvas");

  var context = metricsCanvas.getContext("2d");
  context.font = fontSize + " " + fontFamily;
  var metrics = context.measureText(text);

  return metrics;
}