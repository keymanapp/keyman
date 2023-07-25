/**
 * Function     deepCopy
 * Scope        Private
 * @param       {Object}      p           object to copy
 * @param       {Array=}      c0          array member being copied
 * @return      {Object}                  clone ('deep copy') of object
 * Description  Makes an actual copy (not a reference) of an object, copying simple members,
 *              arrays and member objects but not functions, so use with care!
 */
export default function deepCopy<T>(p:T, c0?): T {
  var c = c0 || {};
  for (var i in p) {
    if(typeof p[i] === 'object' && p[i] != null) {
      c[i] = (p[i].constructor === Array ) ? [] : {};
      deepCopy(p[i],c[i]);
    }
    else {
      c[i] = p[i];
    }
  }

  return c;
}