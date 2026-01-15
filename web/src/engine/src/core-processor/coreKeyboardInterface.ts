/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KM_Core, km_core_option_item, KM_CORE_OPTION_SCOPE } from 'keyman/engine/core-adapter';
import { KeyboardMinimalInterface, Keyboard, VariableStoreSerializer, KMXKeyboard } from 'keyman/engine/keyboard';
import { toPrefixedKeyboardId } from 'keyman/engine/keyboard-storage';

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  private _activeKeyboard: Keyboard;

  public constructor(public variableStoreSerializer?: VariableStoreSerializer) {
  }

  public get activeKeyboard(): Keyboard {
    return this._activeKeyboard;
  }
  public set activeKeyboard(keyboard: Keyboard) {
    this._activeKeyboard = keyboard;
    const stores = this.variableStoreSerializer.findStores(toPrefixedKeyboardId(keyboard.id));
    const options: km_core_option_item[] = [];
    for (const store of stores) {
      for (const key in store) {
        if (store.hasOwnProperty(key)) {
          const item: km_core_option_item = {
            key: key,
            value: store[key],
            scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD
          };
          options.push(item);
        }
      }
    }

    if (options.length > 0) {
      const kmxKeyboard = this._activeKeyboard as KMXKeyboard;
      KM_Core.instance.state_options_update(kmxKeyboard.state, options);
    }
  }

}
