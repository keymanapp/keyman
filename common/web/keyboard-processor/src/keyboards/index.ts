// This file exists as a bundling intermediary that attempts to present all of
// keyboard-processor's offerings in the 'old', namespaced format - at least,
// as of the time that this submodule was converted to ES6 module use.

// Unfortunately, the declaration-bundling tool that works well for the modules...
// struggles a bit here.

export {
  ActiveKey,
  ActiveRow,
  ActiveLayer,
  ActiveLayout
} from "./activeLayout.js";

export {
  Layouts
} from "./defaultLayouts.js";

export { default as Keyboard} from "./keyboard.js";
export {
  LayoutState
} from "./keyboard.js";

