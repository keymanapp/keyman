import DeviceSpec from "@keymanapp/web-utils/build/obj/deviceSpec.js";

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
   * The base resource path to use for font fetches.
   */
  fontRootPath: string
}