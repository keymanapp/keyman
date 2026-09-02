import { VariableStoreSerializer } from '../variableStore.js';
import { Keyboard } from './keyboard.js';

export interface KeyboardMinimalInterface {
  activeKeyboard: Keyboard;
  variableStoreSerializer: VariableStoreSerializer;

}