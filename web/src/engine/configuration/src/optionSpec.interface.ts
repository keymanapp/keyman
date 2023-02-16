import { SpacebarText } from "@keymanapp/keyboard-processor";

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

export interface OptionSpec extends PathOptionSpec {
  /**
   * May be used to denote the name of the embedding application
   */
  embeddingApp?: string | undefined;

  // ui?: string;

  /**
   * If set to true || "true" or if left undefined, the engine will automatically select the first available
   * keyboard for activation.
   *
   * Note that keyboards specified locally are synchronously loaded while cloud keyboards are async; as a
   * result, a locally-specified keyboard will generally be available "sooner", even if added "later".
   */
  setActiveOnRegister?: string | boolean; // TODO: Convert to boolean. Option loader needs to be able to receive this as a string or boolean

  /**
   * Determines the default text shown on the spacebar.  If undefined, uses `LANGUAGE_KEYBOARD`
   */
  spacebarText?: SpacebarText;
}

export const OptionDefaults: Required<OptionSpec> = {
  embeddingApp: undefined,
  root: '',
  resources: '',
  keyboards: '',
  fonts: '',
  setActiveOnRegister: true,
  spacebarText: SpacebarText.LANGUAGE_KEYBOARD,
}