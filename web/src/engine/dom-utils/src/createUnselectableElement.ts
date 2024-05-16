// Found a bit of magic formatting that allows dynamic return typing for a specified element tag!
export default function createUnselectableElement<E extends "p"|"style"|"script"|"div"|"canvas"|"span">(nodeName:E) {
  const e = document.createElement<E>(nodeName);

  e.style.userSelect="none";
  return e;
}