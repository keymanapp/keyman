import { DeviceSpec, ManagedPromise, physicalKeyDeviceAlias, type SpacebarText } from "@keymanapp/keyboard-processor";

import { OptionSpec } from "./optionSpec.interface.js";
import PathConfiguration from "./pathConfiguration.js";

export default class Configuration {
  readonly hostDevice: DeviceSpec;
  readonly sourcePath: string;
  readonly deferForInitialization: ManagedPromise<void>;

  private _paths: PathConfiguration;
  private _activateFirstKeyboard: boolean;
  private _defaultSpacebarText: SpacebarText;
  private _embeddingApp: string;

  // sourcePath:  see `var sPath =` in kmwbase.ts.  It is not obtainable headlessly.
  constructor(device: DeviceSpec, sourcePath: string) {
    this.sourcePath = sourcePath;
    this.hostDevice = device;
    this.deferForInitialization = new ManagedPromise<void>();
  }

  initialize(options: Required<OptionSpec>) {
    this._paths = new PathConfiguration(options, this.sourcePath);
    if(typeof options.setActiveOnRegister == 'boolean') {
      this._activateFirstKeyboard = options.setActiveOnRegister;
    } else if (typeof options.setActiveOnRegister == 'string') {
      let str = options.setActiveOnRegister.toLowerCase();
      this._activateFirstKeyboard = str === 'true';
    } else {
      this._activateFirstKeyboard = true;
    }

    this._defaultSpacebarText = options.spacebarText;

    if(options.embeddingApp) {
      this._embeddingApp = options.embeddingApp;
    }

    this.deferForInitialization.resolve();
  }

  get paths() {
    return this._paths;
  }

  get activateFirstKeyboard() {
    return this._activateFirstKeyboard;
  }

  get defaultSpacebarText() {
    return this._defaultSpacebarText;
  }

  get embeddingApp() {
    return this._embeddingApp;
  }

  get softDevice(): DeviceSpec {
    return this.hostDevice;
  }

  get hardDevice(): DeviceSpec {
    return physicalKeyDeviceAlias(this.hostDevice);
  }
}