/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KM_Core, km_core_option_item, KM_CORE_OPTION_SCOPE } from 'keyman/engine/core-adapter';
import { KeyboardMinimalInterface, VariableStoreSerializer, KMXKeyboard } from 'keyman/engine/keyboard';
import { toPrefixedKeyboardId } from 'keyman/engine/keyboard-storage';

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  private _activeKeyboard: KMXKeyboard;

  public constructor(public readonly variableStoreSerializer: VariableStoreSerializer) {
    if (!variableStoreSerializer) {
      throw new Error('variableStoreSerializer is required');
    }
  }

  public get activeKeyboard(): KMXKeyboard {
    return this._activeKeyboard;
  }
  public set activeKeyboard(keyboard: KMXKeyboard) {
    this._activeKeyboard = keyboard;
    const options = this.loadSerializedOptions();

    if (options.length > 0) {
      KM_Core.instance.state_options_update(keyboard.state, options);
    }
  }

  private loadSerializedOptions(): km_core_option_item[] {
    const options: km_core_option_item[] = [];
    const attrs = KM_Core.instance.keyboard_get_attrs(this.activeKeyboard.keyboard);
    const prefixedKeyboardId = toPrefixedKeyboardId(this.activeKeyboard.id);
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
