/**
 * Provided by keyman/engine/keyboard's `KeyboardProcessor` class and utilized by the OSK
 * to provide state feedback on any corresponding keys visible in the OSK.
 */
export default interface StateKeyMap {
  K_CAPS: boolean,
  K_NUMLOCK: boolean,
  K_SCROLL: boolean
}