import { type PredictionContext } from "@keymanapp/input-processor";
import type Activator from "../views/activator.js";
import CommonConfiguration from "./commonConfiguration.js";

export default interface Configuration extends CommonConfiguration {
  /**
   * If set, the value returned by the function set here will be used instead of any automatic
   * width detection functionality.
   */
  widthOverride?: () => number,

  /**
   * If set, the value returned by the function set here will be used instead of any automatic
   * height detection functionality.
   */
  heightOverride?: () => number,

  /**
   * Sets the default activation model to use for the on-screen keyboard.  If not set, this
   * will default to "two-state" activation for the "anchored" and "floating" view styles
   * (conditioned on an HTMLElement instance) while using "simple" (one-state) activation
   * for the "inlined" style.
   */
  activator?: Activator<any>;

  /**
   * If set to `false`, hide animations will be disallowed.  Defaults to `true`.
   */
  allowHideAnimations?: boolean;

  /**
   * If set to 'true', will perform cache-busting behaviors for linked keyboard stylesheets.
   *
   * Defaults to 'false'.
   */
  doCacheBusting?: boolean;

  /**
   * A predictive-state management object that interfaces the predictive-text banner
   * with the active context.
   */
  predictionContextManager?: PredictionContext;
}