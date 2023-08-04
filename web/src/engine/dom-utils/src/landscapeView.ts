/**
 * Get orientation of tablet or phone  display
 *
 * @return      {boolean}
 */
export default function landscapeView(): boolean	{ // new for I3363 (Build 301)
  var orientation: number;

  // Assume portrait mode if orientation undefined
  if(typeof window.orientation != 'undefined') { // Used by iOS Safari
    // Else landscape for +/-90, portrait for 0, +/-180
    orientation = window.orientation as number;
  } else if(typeof window.screen.orientation != 'undefined') { // Used by Firefox, Chrome
    orientation = window.screen.orientation.angle;
  }

  if(orientation !== undefined) {
    return (Math.abs(orientation/90) == 1);
  } else {
    return false;
  }
}