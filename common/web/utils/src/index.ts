// References all utility includes from a single file, making import/export simple.

export { default as deepCopy } from "./deepCopy.js";

export { default as DeviceSpec, physicalKeyDeviceAlias } from "./deviceSpec.js";

/*
  // An example valid use, post-import:
  let testSpec = new DeviceSpec(DeviceSpec.Browser.Chrome,
                                DeviceSpec.FormFactor.Tablet,
                                DeviceSpec.OperatingSystem.Android,
                                true);
 */

export { default as Version } from "./version.js";

export { default as globalObject } from "./globalObject.js";

export { default as extendString } from "./kmwstring.js";

export { default as ManagedPromise } from "./managedPromise.js";
export { default as TimeoutPromise, timedPromise } from "./timeoutPromise.js";

export { default as PriorityQueue, QueueComparator } from "./priority-queue.js"

// // Uncomment the following line and run the bundled output to verify successful
// // esbuild bundling of this submodule:
// console.log(Version.CURRENT.toString());
