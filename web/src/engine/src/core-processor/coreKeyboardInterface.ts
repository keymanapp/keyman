/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KM_Core, km_core_option_item, KM_CORE_OPTION_SCOPE } from 'keyman/engine/core-adapter';
import { KeyboardMinimalInterface, Keyboard, VariableStoreSerializer, KMXKeyboard } from 'keyman/engine/keyboard';
import { toPrefixedKeyboardId } from 'keyman/engine/keyboard-storage';

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  private _activeKeyboard: Keyboard;

  public constructor(public readonly variableStoreSerializer: VariableStoreSerializer) {
    if (!variableStoreSerializer) {
      throw new Error('variableStoreSerializer is required');
    }
  }

  public get activeKeyboard(): Keyboard {
    return this._activeKeyboard;
  }
  public set activeKeyboard(keyboard: Keyboard) {
    this._activeKeyboard = keyboard;
    const options = this.loadSerializedOptions();

    if (options.length > 0) {
      KM_Core.instance.state_options_update(this.KmxKeyboard.state, options);
    }
  }

  private get KmxKeyboard(): KMXKeyboard {
    return this._activeKeyboard as KMXKeyboard;
  }

  private loadSerializedOptions(): km_core_option_item[] {
    const options: km_core_option_item[] = [];
    const attrs = KM_Core.instance.keyboard_get_attrs(this.KmxKeyboard.keyboard);
    const prefixedKeyboardId = toPrefixedKeyboardId(this.KmxKeyboard.id);
    const defaultOptions = attrs.object.default_options as km_core_option_item[];
    for (const defaultOption of defaultOptions) {
      if (defaultOption.scope == KM_CORE_OPTION_SCOPE.OPT_KEYBOARD) {
        const savedValue = this.variableStoreSerializer.loadStore(prefixedKeyboardId, defaultOption.key);
        if (savedValue !== undefined) {
          const item: km_core_option_item = {
            key: defaultOption.key,
            value: savedValue,
            scope: defaultOption.scope
          };
          options.push(item);
        }
      }
    }
    attrs.delete();
    return options;
  }
}
