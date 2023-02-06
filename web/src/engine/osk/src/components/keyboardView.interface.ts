import OSKViewComponent from "./oskViewComponent.interface.js";

/**
 * An abstract representation for visualizations of the active keyboard within an
 * OSKManager / OSKView.  Most keyboards will default to use of a VisualKeyboard,
 * though some will use HelpPage for certain form factors.
 */
export default interface KeyboardView extends OSKViewComponent {
  readonly element: HTMLDivElement;

  /**
   * Evaluates code that must be run _after_ the KeyboardView has been inserted into
   * the DOM hierarchy.
   */
  postInsert(): void;

  /**
   * Code that updates the state of the KeyboardView whenever the OSK itself needs to be
   * refreshed or updated with new state information.
   */
  updateState(): void;
}