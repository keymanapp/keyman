/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KM_Core, km_core_option_item } from 'keyman/engine/core-adapter';
import { KeyboardMinimalInterface, Keyboard, VariableStoreSerializer, KMXKeyboard } from 'keyman/engine/keyboard';

export class CoreKeyboardInterface implements KeyboardMinimalInterface {
  private _activeKeyboard: Keyboard;

  public constructor(public variableStoreSerializer: VariableStoreSerializer) {
  }

  public get activeKeyboard(): Keyboard {
    return this._activeKeyboard;
  }
  public set activeKeyboard(keyboard: Keyboard) {
    this._activeKeyboard = keyboard;
    const options = this.loadSerializedOptions(keyboard);

    if (options.length > 0) {
      const kmxKeyboard = this._activeKeyboard as KMXKeyboard;
      KM_Core.instance.state_options_update(kmxKeyboard.state, options);
    }
  }


  private loadSerializedOptions(keyboard: Keyboard): km_core_option_item[] {
    // TODO-WEB-CORE: use km_core_keyboard_get_attrs to get list of all variable
    // store names and then iterate through those rather than reading from
    // cookie props
    const options: km_core_option_item[] = [];
    /*const stores = this.variableStoreSerializer.findStores(toPrefixedKeyboardId(keyboard.id));
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
    }*/
    return options;
  }
}
