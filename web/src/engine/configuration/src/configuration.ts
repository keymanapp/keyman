import { physicalKeyDeviceAlias } from "keyman/engine/device-detect";
import { DeviceSpec, type SpacebarText } from "@keymanapp/keyboard-processor";

import { OptionSpec } from "./optionSpec.interface.js";
import PathConfiguration from "./pathConfiguration.js";

export default class Configuration {
  readonly paths: PathConfiguration;
  readonly activateFirstKeyboard: boolean;
  readonly defaultSpacebarText: SpacebarText;

  readonly hostDevice: DeviceSpec;
  readonly embeddingApp: string;

  // sourcePath:  see `var sPath =` in kmwbase.ts.  It is not obtainable headlessly.
  constructor(options: Required<OptionSpec>, device: DeviceSpec, sourcePath: string) {
    this.paths = new PathConfiguration(options, sourcePath);
    if(typeof options.setActiveOnRegister == 'boolean') {
      this.activateFirstKeyboard = options.setActiveOnRegister;
    } else if (typeof options.setActiveOnRegister == 'string') {
      let str = options.setActiveOnRegister.toLowerCase();
      this.activateFirstKeyboard = str === 'true';
    } else {
      this.activateFirstKeyboard = true;
    }

    this.defaultSpacebarText = options.spacebarText;
    this.hostDevice = device;

    if(options.embeddingApp) {
      this.embeddingApp = options.embeddingApp;
    }
  }

  get softDevice(): DeviceSpec {
    return this.hostDevice;
  }

  get hardDevice(): DeviceSpec {
    return physicalKeyDeviceAlias(this.hostDevice);
  }
}