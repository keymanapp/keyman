import EventEmitter from "eventemitter3";

import { DeviceSpec, KeyboardProperties, ManagedPromise, physicalKeyDeviceAlias, SpacebarText } from "@keymanapp/keyboard-processor";
import { PathConfiguration, PathOptionDefaults, PathOptionSpec } from "keyman/engine/paths";
import { Device } from "keyman/engine/device-detect";
import { KeyboardStub } from "keyman/engine/package-cache";

interface EventMap {
  'spacebartext': (mode: SpacebarText) => void;
}

export class EngineConfiguration extends EventEmitter<EventMap> {
  // The app/webview path replaces this during init, but we expect to have something set for this
  // during engine construction, which occurs earlier.  So no `readonly`, sadly.
  //
  // May also be manipulated by Developer's debug-host?
  public hostDevice: DeviceSpec;
  readonly sourcePath: string;
  readonly deferForInitialization: ManagedPromise<void>;

  private _paths: PathConfiguration;
  private _activateFirstKeyboard: boolean;
  private _spacebarText: SpacebarText;
  private _stubNamespacer?: (KeyboardStub) => void;

  public applyCacheBusting: boolean = false;

  // sourcePath:  see `var sPath =` in kmwbase.ts.  It is not obtainable headlessly.
  constructor(sourcePath: string, device?: DeviceSpec) {
    super();

    if(!device) {
      const deviceDetector = new Device();
      deviceDetector.detect();

      device = deviceDetector.coreSpec;
    }

    this.sourcePath = sourcePath;
    this.hostDevice = device;
    this.deferForInitialization = new ManagedPromise<void>();
  }

  initialize(options: Required<InitOptionSpec>) {
    this._paths = new PathConfiguration(options, this.sourcePath);
    if(typeof options.setActiveOnRegister == 'boolean') {
      this._activateFirstKeyboard = options.setActiveOnRegister;
    } else if (typeof options.setActiveOnRegister == 'string') {
      let str = options.setActiveOnRegister.toLowerCase();
      this._activateFirstKeyboard = str === 'true';
    } else {
      this._activateFirstKeyboard = true;
    }

    this._spacebarText = options.spacebarText;

    // Make sure this is accessible to stubs for use in generating display names!
    KeyboardProperties.spacebarTextMode = () => this.spacebarText;

    this.deferForInitialization.resolve();
  }

  get paths() {
    return this._paths;
  }

  get activateFirstKeyboard() {
    return this._activateFirstKeyboard;
  }

  get spacebarText() {
    return this._spacebarText;
  }

  set spacebarText(value: SpacebarText) {
    if(this._spacebarText != value) {
      this._spacebarText = value;
      this.emit('spacebartext', value);
    }
  }

  get softDevice(): DeviceSpec {
    return this.hostDevice;
  }

  get hardDevice(): DeviceSpec {
    return physicalKeyDeviceAlias(this.hostDevice);
  }

  get stubNamespacer() {
    return this._stubNamespacer;
  }

  set stubNamespacer(functor: (stub: KeyboardStub) => void) {
    this._stubNamespacer = functor;
  }

  debugReport(): Record<string, any> {
    return {
      hostDevice: this.hostDevice,
      initialized: this.deferForInitialization.hasFinalized
    }
  }
}

export interface InitOptionSpec extends PathOptionSpec {
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

export const InitOptionDefaults: Required<InitOptionSpec> = {
  setActiveOnRegister: true,  // only needed for browser?
  spacebarText: SpacebarText.LANGUAGE_KEYBOARD,  // useful in both, for OSK config.
  ...PathOptionDefaults
}