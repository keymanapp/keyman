/**
 * Get browser-independent computed style value for element
 *
 * @param       {Element}     e             HTML element
 * @param       {string}      s             CSS style name
 * @return      {*}
 */
export function getStyleValue(e:HTMLElement, s:string) {
  // Build 349: error trap added, since on iOS, getPropertyValue may fail
  // and crash in some cases, possibly if passed a text node
  try
  {
    if(e && (typeof(window.getComputedStyle) != 'undefined')) {
        return window.getComputedStyle(e,'').getPropertyValue(s);
    }
  }
  catch(ex){}

  // Return empty string if unable to get style value
  return '';
}