/**
 * Function     deepCopy
 * Scope        Private
 * @param       {Object}      p           object to copy
 * @return      {Object}                  clone ('deep copy') of object
 * Description  Makes an actual copy (not a reference) of an object, copying simple members,
 *              arrays and member objects but not functions, so use with care!
 */
export default function deepCopy<T extends ({[key: string | number | symbol]: any} | Array<any>)>(p:T): T {
  // typeof undefined == 'undefined', ![] == false, !{} == false
  if(typeof p != 'object' || !p) {
    return p;
  } else {
    const clone = Array.isArray(p) ? [] : {};
    // For arrays, skips over sparse entries.  Not that we use sparse arrays, but still.
    const keys = Object.keys(p);

    for(let key of keys) {
      // @ts-ignore
      if(p[key] !== undefined) {
        // @ts-ignore
        clone[key] = deepCopy(p[key]);
      }
    }
    return clone as T;
  }
}