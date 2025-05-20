import { KM_Core, km_core_keyboard } from 'keyman/engine/core-processor';

/**
 * Acts as a wrapper class for KMX(+) Keyman keyboards
 */
export class KMXKeyboard {
  private _state: km_core_state;

  constructor(private _keyboard: km_core_keyboard) {
    const environment_opts =
      [
        {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.PLATFORM,
          // TODO-web-core: Turn touch off for non-touch targets; read proper platform string from web
          value: "web iphone ipad androidphone androidtablet mobile touch hardware android phone"
        }, {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.BASELAYOUT,
          value: "kbdus.dll"
        }, {
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.BASELAYOUTALT,
          value: "us", // TODO-web-core: use proper base layout
        },{
          scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT,
          key: KM_CORE_KMX_ENV.SIMULATEALTGR,
          value: "0" // TODO-web-core: keyman_get_option_fromdconf(KEYMAN_DCONF_OPTIONS_SIMULATEALTGR) ? "1": "0"
        }
      ]
    const result = KM_Core.instance.state_create(_keyboard, environment_opts);
    if (result.status == KM_CORE_STATUS.OK) {
      this._state = result.object;
    }
  }

  shutdown() {
    if (this._state) {
      this._state.delete();
      this._state = null;
    }
    if (this._keyboard) {
      KM_Core.instance.keyboard_dispose(this._keyboard);
      this._keyboard = null;
    }
  }

  get isMnemonic(): boolean {
    return false;
  }

  get version(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const version = attrs.object.version_string;
    attrs.delete();
    return version;
  }

  get id(): string {
    const attrs = KM_Core.instance.keyboard_get_attrs(this._keyboard);
    const id = attrs.object.id;
    attrs.delete();
    return id;
  }

  get keyboard(): km_core_keyboard {
    return this._keyboard;
  }

  get state(): km_core_state {
    return this._state;
  }
}
