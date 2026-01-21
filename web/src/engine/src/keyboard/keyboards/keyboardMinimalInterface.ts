import { VariableStoreSerializer } from '../variableStore.js';
import { Keyboard } from './keyboardLoaderBase.js';

export interface KeyboardMinimalInterface {
  activeKeyboard: Keyboard;
  variableStoreSerializer: VariableStoreSerializer;

}