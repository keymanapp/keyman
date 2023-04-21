/**
 * Function     arrayFromNodeList
 * Scope        Public
 * @param       {Object}    nl a node list, as returned from getElementsBy_____ methods.
 * Description  Transforms a node list into an array.   *
 * @return      {Array<Element>}
 */
export function arrayFromNodeList(nl: NodeList|HTMLCollectionOf<Element>): HTMLElement[] {
  let res: (HTMLElement)[] = [];
  for(let i=0; i < nl.length; i++) {
    // Typing says we could get Node instances; it's up to use to use this method responsibly.
    res.push(nl[i] as HTMLElement);
  }
  return res;
}