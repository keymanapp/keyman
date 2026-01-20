// References all utility includes from a single file, making import/export simple.

export { deepCopy } from "./deepCopy.js";

export { DeviceSpec, physicalKeyDeviceAlias } from "./deviceSpec.js";

/*
  // An example valid use, post-import:
  let testSpec = new DeviceSpec(DeviceSpec.Browser.Chrome,
                                DeviceSpec.FormFactor.Tablet,
                                DeviceSpec.OperatingSystem.Android,
                                true);
 */

export { Version } from "./version.js";

export { globalObject } from "./globalObject.js";

export * as KMWString from './kmwstring.js';

export { ManagedPromise } from "./managedPromise.js";
export { TimeoutPromise, timedPromise } from "./timeoutPromise.js";

export { PriorityQueue, QueueComparator } from "./priority-queue.js"

export { isEmptyTransform } from './isEmptyTransform.js';

// // Uncomment the following line and run the bundled output to verify successful
// // esbuild bundling of this submodule:
// console.log(Version.CURRENT.toString());
