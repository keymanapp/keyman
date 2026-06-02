// TypeScript bindings for emscripten-generated code.  Automatically generated at compile time.
declare namespace RuntimeExports {
    /**
     * Given a pointer 'ptr' to a null-terminated UTF8-encoded string in the
     * emscripten HEAP, returns a copy of that string as a Javascript String object.
     *
     * @param {number} ptr
     * @param {number=} maxBytesToRead - An optional length that specifies the
     *   maximum number of bytes to read. You can omit this parameter to scan the
     *   string until the first 0 byte. If maxBytesToRead is passed, and the string
     *   at [ptr, ptr+maxBytesToReadr[ contains a null byte in the middle, then the
     *   string will cut short at that byte index (i.e. maxBytesToRead will not
     *   produce a string of exact length [ptr, ptr+maxBytesToRead[) N.B. mixing
     *   frequent uses of UTF8ToString() with and without maxBytesToRead may throw
     *   JS JIT optimizations off, so it is worth to consider consistently using one
     * @return {string}
     */
    function UTF8ToString(ptr: number, maxBytesToRead?: number): string;
    function stringToNewUTF8(str: any): any;
    let wasmExports: any;
    let HEAPF32: any;
    let HEAPF64: any;
    let HEAP_DATA_VIEW: any;
    let HEAP8: any;
    let HEAPU8: any;
    let HEAP16: any;
    let HEAPU16: any;
    let HEAP32: any;
    let HEAPU32: any;
    let HEAP64: any;
    let HEAPU64: any;
}
interface WasmModule {
}

type EmbindString = ArrayBuffer|Uint8Array|Uint8ClampedArray|Int8Array|string;
export interface km_core_keyboard {
  delete(): void;
}

export interface CoreKeyboardReturn {
  readonly object: km_core_keyboard;
  readonly status: number;
  delete(): void;
}

export interface CoreKeyboardAttrsReturn {
  readonly object: km_core_keyboard_attrs;
  readonly status: number;
  delete(): void;
}

export interface CoreContextReturn {
  readonly object: km_core_context_items;
  readonly status: number;
  delete(): void;
}

export interface km_core_keyboard_attrs {
  version_string: string;
  id: string;
  default_options: any;
  delete(): void;
}

export interface km_core_actions {
  do_alert: boolean;
  emit_keystroke: boolean;
  new_caps_lock_state: number;
  code_points_to_delete: number;
  output: string;
  deleted_context: string;
  persist_options: any;
  toString(): string;
  delete(): void;
}

export interface km_core_state {
  delete(): void;
}

export interface CoreStateReturn {
  readonly object: km_core_state;
  readonly status: number;
  delete(): void;
}

export interface km_core_context {
  delete(): void;
}

export interface km_core_context_items {
  data(): km_core_context_item;
  push_back(item: km_core_context_item): void;
  resize(size: number): void;
  size(): number;
  get(index: number): km_core_context_item;
  set(index: number, item: km_core_context_item): void;
  toString(): string;
  delete(): void;
}

export interface km_core_context_item {
  readonly type: number;
  character: number;
  marker: number;
  toString(): string;
  delete(): void;
}

export type km_core_attr = {
  max_context: number,
  current: number,
  revision: number,
  age: number,
  technology: number
};

export type km_core_option_item = {
  key: string,
  value: string,
  scope: number
};

interface EmbindModule {
  km_core_keyboard: {};
  CoreKeyboardReturn: {};
  CoreKeyboardAttrsReturn: {};
  CoreContextReturn: {};
  km_core_keyboard_attrs: {};
  km_core_actions: {};
  km_core_state: {};
  CoreStateReturn: {};
  km_core_context: {};
  km_core_context_items: {new(): km_core_context_items};
  km_core_context_item: {new(): km_core_context_item};
  create_end_context(): km_core_context_item;
  keyboard_get_attrs(keyboard: km_core_keyboard): CoreKeyboardAttrsReturn;
  state_clone(state: km_core_state): CoreStateReturn;
  state_context(state: km_core_state): km_core_context;
  state_get_actions(state: km_core_state): km_core_actions;
  context_get(context: km_core_context): CoreContextReturn;
  keyboard_dispose(keyboard: km_core_keyboard): void;
  state_dispose(state: km_core_state): void;
  process_event(state: km_core_state, vk: number, modifier_state: number, is_key_down: boolean, event_flags: number): number;
  state_context_clear(state: km_core_state): number;
  context_set(context: km_core_context, context_items: km_core_context_items): number;
  tmp_wasm_attributes(): km_core_attr;
  state_context_set_if_needed(state: km_core_state, application_context: string): number;
  state_context_debug(state: km_core_state, context_type: number): string;
  keyboard_load_from_blob(kb_name: EmbindString, blob: any): CoreKeyboardReturn;
  state_create(keyboard: km_core_keyboard, env: any): CoreStateReturn;
  state_options_update(state: km_core_state, new_options: any): number;
}

export type MainModule = WasmModule & typeof RuntimeExports & EmbindModule;
export default function MainModuleFactory (options?: unknown): Promise<MainModule>;
