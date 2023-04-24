// Found a bit of magic formatting that allows dynamic return typing for a specified element tag!
export function _CreateElement<E extends "p"|"style"|"script"|"div"|"canvas"|"span">(nodeName:E) {
  const e = document.createElement<E>(nodeName);
  e.style.userSelect="none";

  // @ts-ignore
  e.style.MozUserSelect="none";
  // @ts-ignore
  e.style.KhtmlUserSelect="none";
  // @ts-ignore
  e.style.UserSelect="none";
  // @ts-ignore
  e.style.WebkitUserSelect="none";
  return e;
}