export interface PathOptionSpec {
  /**
   * If defined, specifies the root path of the default location hosting KMW resources.
   * Is typically just the protocol + domain name.
   */
  root?: string;

  /**
   * The base path to prepend on relative paths for other types of resources.
   */
  resources?: string;

  /**
   * The base path to prepend on relative paths when loading keyboards.
   */
  keyboards?: string;

  /**
   * The base path to prepend on relative paths when loading fonts.
   */
  fonts?: string;
}

export const PathOptionDefaults: Required<PathOptionSpec> = {
  root: '',
  resources: '',
  keyboards: '',
  fonts: ''
}