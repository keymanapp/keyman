/**
 * The basic interface that Keyman Engine for Web expects of its keyboard-selection UI modules.
 */
export interface UIModule {
  /**
   * The module's name (button, float, toggle, toolbar, touch)
   */
  get name(): string;

  /**
   * A method that may be called to have the UI module release resources and disconnect any
   * event handlers.
   */
  shutdown(): void;

  /**
   * The UI module's initialization method.  It is expected that the UI module will perform
   * its own setup and attach to KMW events for any data needed for its operation.
   */
  initialize(): void;
}