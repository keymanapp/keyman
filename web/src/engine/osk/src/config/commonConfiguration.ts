import { DeviceSpec } from "@keymanapp/web-utils";

import EmbeddedGestureConfig from './embeddedGestureConfig.js';
import { OSKResourcePathConfiguration } from "keyman/engine/interfaces";
import { GestureParams } from "../input/gestures/specsForLayout.js";

export default interface CommonConfiguration {
  /**
   * Metadata for the type of device to emulate and target with the on-screen keyboard.
   * When not specified, the metadata specified for `hostDevice` will be used by default.
   */
  device?: DeviceSpec,

 /**
  * Metadata about the actual type of computer in use.
  */
  hostDevice: DeviceSpec,

  /**
   * Set to 'true' to utilize embedded-mode formatting.
   */
  isEmbedded?: boolean,

  /**
   * Configured paths usable to find OSK resources.
   */
  pathConfig: OSKResourcePathConfiguration;

  /**
   * Allows control-flow forwarding of gestures that may have special implementation when
   * embedded within a WebView.
   */
  embeddedGestureConfig?: EmbeddedGestureConfig;

  /**
   * Specifies the gesture parameterizations to use for the active keyboard.
   */
  gestureParams?: GestureParams;
}