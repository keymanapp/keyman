{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Interface for Keyman Core, matches keyboardprocessor.h
}
unit Keyman.System.KeymanCore;

interface

{$WARN SYMBOL_PLATFORM OFF}
{$ALIGN 8}

type
  uint8_t = Byte;
  uint16_t = Word;
  uint32_t = Cardinal;
  uintptr_t = NativeUInt;
  char16_t = Char;

  km_kbp_virtual_key = uint16_t;

  km_kbp_usv = uint32_t;  // UTF-32
  km_kbp_cp = WideChar;
  pkm_kbp_cp = ^km_kbp_cp;

  km_kbp_context = record end;
  pkm_kbp_context = ^km_kbp_context;

  km_kbp_keyboard = record end;
  pkm_kbp_keyboard = ^km_kbp_keyboard;

  km_kbp_state = record end;
  pkm_kbp_state = ^km_kbp_state;

  km_kbp_options = record end;
  pkm_kbp_options = ^km_kbp_options;

  km_kbp_path_name = PWideChar; // on Windows
  pkm_kbp_path_name = ^km_kbp_path_name;

type
  km_kbp_status = (
    KM_KBP_STATUS_OK = 0,
    KM_KBP_STATUS_NO_MEM = 1,
    KM_KBP_STATUS_IO_ERROR = 2,
    KM_KBP_STATUS_INVALID_ARGUMENT = 3,
    KM_KBP_STATUS_KEY_ERROR = 4,
    KM_KBP_STATUS_INSUFFICENT_BUFFER = 5,
    KM_KBP_STATUS_INVALID_UTF = 6,
    KM_KBP_STATUS_INVALID_KEYBOARD = 7
  );

type
  km_kbp_context_type = (
    KM_KBP_CT_END = 0,
    KM_KBP_CT_CHAR,
    KM_KBP_CT_MARKER
  );

  km_kbp_context_item = record
    _type: km_kbp_context_type;
{$IFDEF WIN64}
    _reserved: array[0..6] of uint8_t;
{$ELSE}
    _reserved: array[0..2] of uint8_t;
{$ENDIF}
  case integer of
    1: (character: km_kbp_usv);
    2: (marker: uint32_t);
  end;

  pkm_kbp_context_item = ^km_kbp_context_item;

const
  KM_KBP_CONTEXT_ITEM_END: km_kbp_context_item = (
    _type: KM_KBP_CT_END;
    _reserved: (0,0,0);
    character: 0
  );

const
  kmnkbp0 = 'kmnkbp0-0.dll';

function km_kbp_context_items_from_utf16(
  const text: pkm_kbp_cp;
  var out_ptr: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_context_items_from_utf8(
  const text: PAnsiChar;
  var out_ptr: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_context_items_to_utf16(
  const item: pkm_kbp_context_item;
  buf: pkm_kbp_cp;
  var buf_size: integer
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_context_items_to_utf8(
  const item: pkm_kbp_context_item;
  buf: pansichar;
  var buf_size: integer
): km_kbp_status; cdecl; external kmnkbp0 delayed;

procedure km_kbp_context_items_dispose(
  const context_items: km_kbp_context_item
); cdecl; external kmnkbp0 delayed;

function km_kbp_context_set(
  context: pkm_kbp_context;
  context_items: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_context_get(
  context: pkm_kbp_context;
  var context_items: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

procedure km_kbp_context_clear(
  context: pkm_kbp_context
); cdecl; external kmnkbp0 delayed;

function km_kbp_context_append(
  context: pkm_kbp_context;
  context_items: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_context_shrink(
  context: pkm_kbp_context;
  num: Integer;
  prefix: pkm_kbp_context_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;



type
  km_kbp_option_scope = (
    KM_KBP_OPT_UNKNOWN      = 0,
    KM_KBP_OPT_KEYBOARD     = 1,
    KM_KBP_OPT_ENVIRONMENT  = 2,
    KM_KBP_OPT_MAX_SCOPES
  );

  km_kbp_option_item = record
    key: pkm_kbp_cp;
    value: pkm_kbp_cp;
    scope: km_kbp_option_scope;
  end;

  pkm_kbp_option_item = ^km_kbp_option_item;

const
  KM_KBP_OPTIONS_END: km_kbp_option_item = (key: nil; value: nil; scope: KM_KBP_OPT_UNKNOWN);


type
  km_kbp_action_type = (
    KM_KBP_IT_END         = 0,  // Marks end of action items list.
    KM_KBP_IT_CHAR        = 1,  // A Unicode character has been generated.
    KM_KBP_IT_MARKER      = 2,  // Correlates to kmn's "deadkey" markers.
    KM_KBP_IT_ALERT       = 3,  // The keyboard has triggered a alert/beep/bell.
    KM_KBP_IT_BACK        = 4,  // Delete the codepoint preceding the insertion point.
    KM_KBP_IT_PERSIST_OPT = 5,  // The indicated option needs to be stored.
    KM_KBP_IT_EMIT_KEYSTROKE = 6,  // Emit the current keystroke to the application
    KM_KBP_IT_INVALIDATE_CONTEXT = 7,
            // The processor requests that the context buffer be cleared;
            // for applications where context is cached, this clears the context;
            // for applications where context is read from the focused text store,
            // the context is just re-read and markers flushed.
    KM_KBP_IT_MAX_TYPE_ID
  );

  km_kbp_backspace_type = (
    KM_KBP_BT_UNKNOWN    = 0,  // Used at beginning of context; user-initiated backspace
    KM_KBP_BT_CHAR       = 1,  // Deleting a character prior to insertion point
    KM_KBP_BT_MARKER     = 2,  // Deleting a marker prior to insertion point
    KM_KBP_BT_MAX_TYPE_ID
  );

  km_kbp_backspace_item = record
    expected_type: uint8_t;            /// TODO: km_kbp_backspace_type
    expected_value: uintptr_t;         /// used mainly in unit tests
  end;

  km_kbp_action_item = record
    _type: km_kbp_action_type;
{$IFDEF WIN64}
    _reserved: array[0..6] of uint8_t;
{$ELSE}
    _reserved: array[0..2] of uint8_t;
{$ENDIF}
  case Integer of
    0: (marker: uintptr_t);
    1: (option: pkm_kbp_option_item);
    2: (character: km_kbp_usv);
    3: (backspace: km_kbp_backspace_item)
  end;

  pkm_kbp_action_item = ^km_kbp_action_item;

function km_kbp_options_list_size(
  opts: pkm_kbp_option_item
): Integer; cdecl; external kmnkbp0 delayed;

function km_kbp_state_option_lookup(
  state: pkm_kbp_state;
  scope: km_kbp_option_scope;
  key: pkm_kbp_cp;
  var value: pkm_kbp_cp
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_state_options_update(
  state: pkm_kbp_state;
  new_opts: pkm_kbp_option_item
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_state_options_to_json(
  state: pkm_kbp_state;
  buf: PAnsiChar;
  var space: Integer
): km_kbp_status; cdecl; external kmnkbp0 delayed;

type
  km_kbp_keyboard_attrs = record
    version_string: pkm_kbp_cp;
    id: pkm_kbp_cp;
    folder_path: km_kbp_path_name;
    default_optons: pkm_kbp_option_item
  end;

  pkm_kbp_keyboard_attrs = ^km_kbp_keyboard_attrs;

function km_kbp_keyboard_load(
  kb_path: km_kbp_path_name;
  var keyboard: pkm_kbp_keyboard
): km_kbp_status; cdecl; external kmnkbp0 delayed;

procedure km_kbp_keyboard_dispose(
  keyboard: pkm_kbp_keyboard
); cdecl; external kmnkbp0 delayed;

function km_kbp_keyboard_get_attrs(
  keyboard: pkm_kbp_keyboard;
  var out: pkm_kbp_keyboard_attrs
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_state_create(
  keyboard: pkm_kbp_keyboard;
  env: pkm_kbp_option_item;
  var out: pkm_kbp_state
): km_kbp_status; cdecl; external kmnkbp0 delayed;

function km_kbp_state_clone(
  state: pkm_kbp_state;
  var out: pkm_kbp_state
): km_kbp_status; cdecl; external kmnkbp0 delayed;

procedure km_kbp_state_dispose(
  state: pkm_kbp_state
); cdecl; external kmnkbp0 delayed;

function km_kbp_state_context(
  state: pkm_kbp_state
): pkm_kbp_context; cdecl; external kmnkbp0 delayed;

function km_kbp_state_action_items(
  state: pkm_kbp_state;
  num_items: pinteger
): pkm_kbp_action_item; cdecl; external kmnkbp0 delayed;

function km_kbp_state_to_json(
  state: pkm_kbp_state;
  buf: PAnsiChar;
  space: pinteger
): km_kbp_status; cdecl; external kmnkbp0 delayed;

type
  km_kbp_attr = record
    max_context: Integer;   // Maximum context size supported by processor.
    current:     uint16_t;  // Current API number supported.
    revision:    uint16_t;  // Implementation number of current API.
    age:         uint16_t;  // current - age == Oldest API number supported.
    technology : uint16_t;  // A bit field specifiying which Keyboard
                            //  technologies the engine supports.
    vendor: pansichar       // Implementor of the processor.
  end;

  pkm_kbp_attr = ^km_kbp_attr;

  km_kbp_tech_value = (
    KM_KBP_TECH_UNSPECIFIED = 0,
    KM_KBP_TECH_MOCK        = 1 shl 0,
    KM_KBP_TECH_KMX         = 1 shl 1,
    KM_KBP_TECH_LDML        = 1 shl 2,
    KM_KBP_TECH_RUST_MOCK   = 1 shl 3
  );

function km_kbp_get_engine_attrs(
  state: pkm_kbp_state
): pkm_kbp_attr; cdecl; external kmnkbp0 delayed;

function
km_kbp_process_event(
  state: pkm_kbp_state;
  vk: km_kbp_virtual_key;
  modifier_state: uint16_t;
  is_key_down: uint8_t
): km_kbp_status; cdecl; external kmnkbp0 delayed;


// keyboardprocessor_vkeys.h

const
// enum km_kbp_modifier_state - matches Keyman32 shift states
  KM_KBP_MODIFIER_LCTRL       = 1 shl 0;
  KM_KBP_MODIFIER_RCTRL       = 1 shl 1;
  KM_KBP_MODIFIER_LALT        = 1 shl 2;
  KM_KBP_MODIFIER_RALT        = 1 shl 3;
  KM_KBP_MODIFIER_SHIFT       = 1 shl 4;
  KM_KBP_MODIFIER_CTRL        = 1 shl 5;
  KM_KBP_MODIFIER_ALT         = 1 shl 6;
  {
    KM_KBP_MODIFIER_META        = 1 shl 7,    // Either Meta-key flag (tentative).  Not usable by keyboards currently
                                             // Used internally (currently, only by KMW) to ensure Meta-key
                                             // shortcuts safely bypass rules
                                             // Meta key = Command key on macOS, Windows key on Windows
  }
  KM_KBP_MODIFIER_CAPS        = 1 shl 8;
  KM_KBP_MODIFIER_NOCAPS      = 1 shl 9;
  {
    KM_KBP_MODIFIER_NUMLOCK     = 1 << 10,
    KM_KBP_MODIFIER_NONUMLOCK   = 1 << 11,
    KM_KBP_MODIFIER_SCROLLOCK   = 1 << 12,
    KM_KBP_MODIFIER_NOSCROLLOCK = 1 << 13,
    KM_KBP_MODIFIER_VIRTUALKEY  = 1 << 14,
  }

const
// enum km_kbp_modifier_mask
  KM_KBP_MODIFIER_MASK_ALL         = $7f;
  KM_KBP_MODIFIER_MASK_ALT_GR_SIM  = KM_KBP_MODIFIER_LCTRL or KM_KBP_MODIFIER_LALT;
  KM_KBP_MODIFIER_MASK_CHIRAL      = $1f;
  KM_KBP_MODIFIER_MASK_IS_CHIRAL   = $0f;
  KM_KBP_MODIFIER_MASK_NON_CHIRAL  = $7f;
  KM_KBP_MODIFIER_MASK_CAPS        = $0300;
{KM_KBP_MODIFIER_MASK_NUMLOCK     = $0C00,
  KM_KBP_MODIFIER_MASK_SCROLLLOCK  = $3000,}

// These are Windows API VKEYs, using Keyman VKEY names.
{
const
//enum km_kpb_virtual_key {
  KM_KBP_VKEY__00 = 0;
  KM_KBP_VKEY_LBUTTON,
  KM_KBP_VKEY_RBUTTON,
  KM_KBP_VKEY_CANCEL,
  KM_KBP_VKEY_MBUTTON,
  KM_KBP_VKEY__05,
  KM_KBP_VKEY__06,
  KM_KBP_VKEY__07,
  KM_KBP_VKEY_BKSP,
  KM_KBP_VKEY_TAB,
  KM_KBP_VKEY__0A,
  KM_KBP_VKEY__0B,
  KM_KBP_VKEY_KP5,
  KM_KBP_VKEY_ENTER,
  KM_KBP_VKEY__0E,
  KM_KBP_VKEY__0F,
  KM_KBP_VKEY_SHIFT,
  KM_KBP_VKEY_CONTROL,
  KM_KBP_VKEY_ALT,
  KM_KBP_VKEY_PAUSE,
  KM_KBP_VKEY_CAPS,
  KM_KBP_VKEY__15,
  KM_KBP_VKEY__16,
  KM_KBP_VKEY__17,
  KM_KBP_VKEY__18,
  KM_KBP_VKEY__19,
  KM_KBP_VKEY__1A,
  KM_KBP_VKEY_ESC,
  KM_KBP_VKEY__1C,
  KM_KBP_VKEY__1D,
  KM_KBP_VKEY__1E,
  KM_KBP_VKEY__1F,
  KM_KBP_VKEY_SPACE,
  KM_KBP_VKEY_PGUP,
  KM_KBP_VKEY_PGDN,
  KM_KBP_VKEY_END,
  KM_KBP_VKEY_HOME,
  KM_KBP_VKEY_LEFT,
  KM_KBP_VKEY_UP,
  KM_KBP_VKEY_RIGHT,
  KM_KBP_VKEY_DOWN,
  KM_KBP_VKEY_SEL,
  KM_KBP_VKEY_PRINT,
  KM_KBP_VKEY_EXEC,
  KM_KBP_VKEY_PRTSCN,
  KM_KBP_VKEY_INS,
  KM_KBP_VKEY_DEL,
  KM_KBP_VKEY_HELP,
  KM_KBP_VKEY_0,
  KM_KBP_VKEY_1,
  KM_KBP_VKEY_2,
  KM_KBP_VKEY_3,
  KM_KBP_VKEY_4,
  KM_KBP_VKEY_5,
  KM_KBP_VKEY_6,
  KM_KBP_VKEY_7,
  KM_KBP_VKEY_8,
  KM_KBP_VKEY_9,
  KM_KBP_VKEY__3A,
  KM_KBP_VKEY__3B,
  KM_KBP_VKEY__3C,
  KM_KBP_VKEY__3D,
  KM_KBP_VKEY__3E,
  KM_KBP_VKEY__3F,
  KM_KBP_VKEY__40,
  KM_KBP_VKEY_A,
  KM_KBP_VKEY_B,
  KM_KBP_VKEY_C,
  KM_KBP_VKEY_D,
  KM_KBP_VKEY_E,
  KM_KBP_VKEY_F,
  KM_KBP_VKEY_G,
  KM_KBP_VKEY_H,
  KM_KBP_VKEY_I,
  KM_KBP_VKEY_J,
  KM_KBP_VKEY_K,
  KM_KBP_VKEY_L,
  KM_KBP_VKEY_M,
  KM_KBP_VKEY_N,
  KM_KBP_VKEY_O,
  KM_KBP_VKEY_P,
  KM_KBP_VKEY_Q,
  KM_KBP_VKEY_R,
  KM_KBP_VKEY_S,
  KM_KBP_VKEY_T,
  KM_KBP_VKEY_U,
  KM_KBP_VKEY_V,
  KM_KBP_VKEY_W,
  KM_KBP_VKEY_X,
  KM_KBP_VKEY_Y,
  KM_KBP_VKEY_Z,
  KM_KBP_VKEY__5B,
  KM_KBP_VKEY__5C,
  KM_KBP_VKEY__5D,
  KM_KBP_VKEY__5E,
  KM_KBP_VKEY__5F,
  KM_KBP_VKEY_NP0,
  KM_KBP_VKEY_NP1,
  KM_KBP_VKEY_NP2,
  KM_KBP_VKEY_NP3,
  KM_KBP_VKEY_NP4,
  KM_KBP_VKEY_NP5,
  KM_KBP_VKEY_NP6,
  KM_KBP_VKEY_NP7,
  KM_KBP_VKEY_NP8,
  KM_KBP_VKEY_NP9,
  KM_KBP_VKEY_NPSTAR,
  KM_KBP_VKEY_NPPLUS,
  KM_KBP_VKEY_SEPARATOR,
  KM_KBP_VKEY_NPMINUS,
  KM_KBP_VKEY_NPDOT,
  KM_KBP_VKEY_NPSLASH,
  KM_KBP_VKEY_F1,
  KM_KBP_VKEY_F2,
  KM_KBP_VKEY_F3,
  KM_KBP_VKEY_F4,
  KM_KBP_VKEY_F5,
  KM_KBP_VKEY_F6,
  KM_KBP_VKEY_F7,
  KM_KBP_VKEY_F8,
  KM_KBP_VKEY_F9,
  KM_KBP_VKEY_F10,
  KM_KBP_VKEY_F11,
  KM_KBP_VKEY_F12,
  KM_KBP_VKEY_F13,
  KM_KBP_VKEY_F14,
  KM_KBP_VKEY_F15,
  KM_KBP_VKEY_F16,
  KM_KBP_VKEY_F17,
  KM_KBP_VKEY_F18,
  KM_KBP_VKEY_F19,
  KM_KBP_VKEY_F20,
  KM_KBP_VKEY_F21,
  KM_KBP_VKEY_F22,
  KM_KBP_VKEY_F23,
  KM_KBP_VKEY_F24,
  KM_KBP_VKEY__88,
  KM_KBP_VKEY__89,
  KM_KBP_VKEY__8A,
  KM_KBP_VKEY__8B,
  KM_KBP_VKEY__8C,
  KM_KBP_VKEY__8D,
  KM_KBP_VKEY__8E,
  KM_KBP_VKEY__8F,
  KM_KBP_VKEY_NUMLOCK,
  KM_KBP_VKEY_SCROLL,
  KM_KBP_VKEY__92,
  KM_KBP_VKEY__93,
  KM_KBP_VKEY__94,
  KM_KBP_VKEY__95,
  KM_KBP_VKEY__96,
  KM_KBP_VKEY__97,
  KM_KBP_VKEY__98,
  KM_KBP_VKEY__99,
  KM_KBP_VKEY__9A,
  KM_KBP_VKEY__9B,
  KM_KBP_VKEY__9C,
  KM_KBP_VKEY__9D,
  KM_KBP_VKEY__9E,
  KM_KBP_VKEY__9F,
  KM_KBP_VKEY__A0,
  KM_KBP_VKEY__A1,
  KM_KBP_VKEY__A2,
  KM_KBP_VKEY__A3,
  KM_KBP_VKEY__A4,
  KM_KBP_VKEY__A5,
  KM_KBP_VKEY__A6,
  KM_KBP_VKEY__A7,
  KM_KBP_VKEY__A8,
  KM_KBP_VKEY__A9,
  KM_KBP_VKEY__AA,
  KM_KBP_VKEY__AB,
  KM_KBP_VKEY__AC,
  KM_KBP_VKEY__AD,
  KM_KBP_VKEY__AE,
  KM_KBP_VKEY__AF,
  KM_KBP_VKEY__B0,
  KM_KBP_VKEY__B1,
  KM_KBP_VKEY__B2,
  KM_KBP_VKEY__B3,
  KM_KBP_VKEY__B4,
  KM_KBP_VKEY__B5,
  KM_KBP_VKEY__B6,
  KM_KBP_VKEY__B7,
  KM_KBP_VKEY__B8,
  KM_KBP_VKEY__B9,
  KM_KBP_VKEY_COLON,
  KM_KBP_VKEY_EQUAL,
  KM_KBP_VKEY_COMMA,
  KM_KBP_VKEY_HYPHEN,
  KM_KBP_VKEY_PERIOD,
  KM_KBP_VKEY_SLASH,
  KM_KBP_VKEY_BKQUOTE,
  KM_KBP_VKEY__C1,
  KM_KBP_VKEY__C2,
  KM_KBP_VKEY__C3,
  KM_KBP_VKEY__C4,
  KM_KBP_VKEY__C5,
  KM_KBP_VKEY__C6,
  KM_KBP_VKEY__C7,
  KM_KBP_VKEY__C8,
  KM_KBP_VKEY__C9,
  KM_KBP_VKEY__CA,
  KM_KBP_VKEY__CB,
  KM_KBP_VKEY__CC,
  KM_KBP_VKEY__CD,
  KM_KBP_VKEY__CE,
  KM_KBP_VKEY__CF,
  KM_KBP_VKEY__D0,
  KM_KBP_VKEY__D1,
  KM_KBP_VKEY__D2,
  KM_KBP_VKEY__D3,
  KM_KBP_VKEY__D4,
  KM_KBP_VKEY__D5,
  KM_KBP_VKEY__D6,
  KM_KBP_VKEY__D7,
  KM_KBP_VKEY__D8,
  KM_KBP_VKEY__D9,
  KM_KBP_VKEY__DA,
  KM_KBP_VKEY_LBRKT,
  KM_KBP_VKEY_BKSLASH,
  KM_KBP_VKEY_RBRKT,
  KM_KBP_VKEY_QUOTE,
  KM_KBP_VKEY_oDF,
  KM_KBP_VKEY_oE0,
  KM_KBP_VKEY_oE1,
  KM_KBP_VKEY_oE2,  // 102nd key on European layouts
  KM_KBP_VKEY_oE3,
  KM_KBP_VKEY_oE4,
  KM_KBP_VKEY__E5,
  KM_KBP_VKEY_oE6,
  KM_KBP_VKEY__E7,
  KM_KBP_VKEY__E8,
  KM_KBP_VKEY_oE9,
  KM_KBP_VKEY_oEA,
  KM_KBP_VKEY_oEB,
  KM_KBP_VKEY_oEC,
  KM_KBP_VKEY_oED,
  KM_KBP_VKEY_oEE,
  KM_KBP_VKEY_oEF,
  KM_KBP_VKEY_oF0,
  KM_KBP_VKEY_oF1,
  KM_KBP_VKEY_oF2,
  KM_KBP_VKEY_oF3,
  KM_KBP_VKEY_oF4,
  KM_KBP_VKEY_oF5,
  KM_KBP_VKEY__F6,
  KM_KBP_VKEY__F7,
  KM_KBP_VKEY__F8,
  KM_KBP_VKEY__F9,
  KM_KBP_VKEY__FA,
  KM_KBP_VKEY__FB,
  KM_KBP_VKEY__FC,
  KM_KBP_VKEY__FD,
  KM_KBP_VKEY__FE,
  KM_KBP_VKEY__FF,
}

procedure _km_kbp_set_library_path(const path: string);

implementation

uses
  System.SysUtils,
  Winapi.Windows;

procedure _km_kbp_set_library_path(const path: string);
begin
  if LoadLibrary(PChar(path)) = 0 then
    RaiseLastOSError;
end;

end.
