// References all utility includes from a single file, making import/export simple.

import deepCopy from "./deepCopy.js";
export { deepCopy };

import DeviceSpec from "./deviceSpec.js";
export { DeviceSpec };

/*
  // An example valid use, post-import:
  let testSpec = new DeviceSpec(DeviceSpec.Browser.Chrome,
                                DeviceSpec.FormFactor.Tablet,
                                DeviceSpec.OperatingSystem.Android,
                                true);
 */

import Version from "./version.js";
export { Version };

import globalObject from "./globalObject.js";
export { globalObject };

import extendString from "./kmwstring.js";
export { extendString };

// // Uncomment the following line and run the bundled output to verify successful
// // esbuild bundling of this submodule:
// console.log(Version.CURRENT.toString());