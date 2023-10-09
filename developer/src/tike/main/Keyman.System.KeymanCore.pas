{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Interface for Keyman Core, matches keyman_core_api.h
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

  km_core_virtual_key = uint16_t;

  km_core_usv = uint32_t;  // UTF-32
  km_core_cp = WideChar;
  pkm_core_cp = ^km_core_cp;

  km_core_context = record end;
  pkm_core_context = ^km_core_context;

  km_core_keyboard = record end;
  pkm_core_keyboard = ^km_core_keyboard;

  km_core_state = record end;
  pkm_core_state = ^km_core_state;

  km_core_options = record end;
  pkm_core_options = ^km_core_options;

  km_core_path_name = PWideChar; // on Windows
  pkm_core_path_name = ^km_core_path_name;

type
  km_core_status = (
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
  km_core_context_type = (
    KM_KBP_CT_END = 0,
    KM_KBP_CT_CHAR,
    KM_KBP_CT_MARKER
  );

  km_core_context_item = record
    _type: km_core_context_type;
{$IFDEF WIN64}
    _reserved: array[0..6] of uint8_t;
{$ELSE}
    _reserved: array[0..2] of uint8_t;
{$ENDIF}
  case integer of
    1: (character: km_core_usv);
    2: (marker: uint32_t);
  end;

  pkm_core_context_item = ^km_core_context_item;

const
  KM_KBP_CONTEXT_ITEM_END: km_core_context_item = (
    _type: KM_KBP_CT_END;
    _reserved: (0,0,0);
    character: 0
  );

const
  kmnkbp0 = 'kmnkbp0-0.dll';

function km_core_context_items_from_utf16(
  const text: pkm_core_cp;
  var out_ptr: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_context_items_from_utf8(
  const text: PAnsiChar;
  var out_ptr: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_context_items_to_utf16(
  const item: pkm_core_context_item;
  buf: pkm_core_cp;
  var buf_size: integer
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_context_items_to_utf8(
  const item: pkm_core_context_item;
  buf: pansichar;
  var buf_size: integer
): km_core_status; cdecl; external kmnkbp0 delayed;

procedure km_core_context_items_dispose(
  const context_items: km_core_context_item
); cdecl; external kmnkbp0 delayed;

function km_core_context_set(
  context: pkm_core_context;
  context_items: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_context_get(
  context: pkm_core_context;
  var context_items: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;

procedure km_core_context_clear(
  context: pkm_core_context
); cdecl; external kmnkbp0 delayed;

function km_core_context_append(
  context: pkm_core_context;
  context_items: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_context_shrink(
  context: pkm_core_context;
  num: Integer;
  prefix: pkm_core_context_item
): km_core_status; cdecl; external kmnkbp0 delayed;



type
  km_core_option_scope = (
    KM_KBP_OPT_UNKNOWN      = 0,
    KM_KBP_OPT_KEYBOARD     = 1,
    KM_KBP_OPT_ENVIRONMENT  = 2,
    KM_KBP_OPT_MAX_SCOPES
  );

  km_core_option_item = record
    key: pkm_core_cp;
    value: pkm_core_cp;
    scope: km_core_option_scope;
  end;

  pkm_core_option_item = ^km_core_option_item;

const
  KM_KBP_OPTIONS_END: km_core_option_item = (key: nil; value: nil; scope: KM_KBP_OPT_UNKNOWN);


type
  km_core_action_type = (
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
    KM_KBP_IT_CAPSLOCK    = 8,  // Enable or disable capsLock
    KM_KBP_IT_MAX_TYPE_ID
  );

  km_core_backspace_type = (
    KM_KBP_BT_UNKNOWN    = 0,  // Used at beginning of context; user-initiated backspace
    KM_KBP_BT_CHAR       = 1,  // Deleting a character prior to insertion point
    KM_KBP_BT_MARKER     = 2,  // Deleting a marker prior to insertion point
    KM_KBP_BT_MAX_TYPE_ID
  );

  km_core_backspace_item = record
    expected_type: uint8_t;            /// TODO: km_core_backspace_type
    expected_value: uintptr_t;         /// used mainly in unit tests
  end;

  km_core_action_item = record
    _type: km_core_action_type;
{$IFDEF WIN64}
    _reserved: array[0..6] of uint8_t;
{$ELSE}
    _reserved: array[0..2] of uint8_t;
{$ENDIF}
  case Integer of
    0: (marker: uintptr_t);
    1: (option: pkm_core_option_item);
    2: (character: km_core_usv);
    3: (backspace: km_core_backspace_item);
    4: (capsLock: uint8_t);  // CAPSLOCK type, 1 to turn on, 0 to turn off
  end;

  pkm_core_action_item = ^km_core_action_item;

// These types are used only for debugging convenience
type
  km_core_action_item_array = array[0..100] of km_core_action_item;
  pkm_core_action_item_array = ^km_core_action_item_array;

function km_core_options_list_size(
  opts: pkm_core_option_item
): Integer; cdecl; external kmnkbp0 delayed;

function km_core_state_option_lookup(
  state: pkm_core_state;
  scope: km_core_option_scope;
  key: pkm_core_cp;
  var value: pkm_core_cp
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_state_options_update(
  state: pkm_core_state;
  new_opts: pkm_core_option_item
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_state_options_to_json(
  state: pkm_core_state;
  buf: PAnsiChar;
  var space: Integer
): km_core_status; cdecl; external kmnkbp0 delayed;

type
  km_core_keyboard_attrs = record
    version_string: pkm_core_cp;
    id: pkm_core_cp;
    folder_path: km_core_path_name;
    default_optons: pkm_core_option_item
  end;

  pkm_core_keyboard_attrs = ^km_core_keyboard_attrs;

function km_core_keyboard_load(
  kb_path: km_core_path_name;
  var keyboard: pkm_core_keyboard
): km_core_status; cdecl; external kmnkbp0 delayed;

procedure km_core_keyboard_dispose(
  keyboard: pkm_core_keyboard
); cdecl; external kmnkbp0 delayed;

function km_core_keyboard_get_attrs(
  keyboard: pkm_core_keyboard;
  var out: pkm_core_keyboard_attrs
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_state_create(
  keyboard: pkm_core_keyboard;
  env: pkm_core_option_item;
  var out: pkm_core_state
): km_core_status; cdecl; external kmnkbp0 delayed;

function km_core_state_clone(
  state: pkm_core_state;
  var out: pkm_core_state
): km_core_status; cdecl; external kmnkbp0 delayed;

procedure km_core_state_dispose(
  state: pkm_core_state
); cdecl; external kmnkbp0 delayed;

function km_core_state_context(
  state: pkm_core_state
): pkm_core_context; cdecl; external kmnkbp0 delayed;

function km_core_state_action_items(
  state: pkm_core_state;
  num_items: pinteger
): pkm_core_action_item; cdecl; external kmnkbp0 delayed;

function km_core_state_to_json(
  state: pkm_core_state;
  buf: PAnsiChar;
  space: pinteger
): km_core_status; cdecl; external kmnkbp0 delayed;

type
  km_core_attr = record
    max_context: Integer;   // Maximum context size supported by processor.
    current:     uint16_t;  // Current API number supported.
    revision:    uint16_t;  // Implementation number of current API.
    age:         uint16_t;  // current - age == Oldest API number supported.
    technology : uint16_t;  // A bit field specifiying which Keyboard
                            //  technologies the engine supports.
    vendor: pansichar       // Implementor of the processor.
  end;

  pkm_core_attr = ^km_core_attr;

  km_core_tech_value = (
    KM_KBP_TECH_UNSPECIFIED = 0,
    KM_KBP_TECH_MOCK        = 1 shl 0,
    KM_KBP_TECH_KMX         = 1 shl 1,
    KM_KBP_TECH_LDML        = 1 shl 2,
    KM_KBP_TECH_RUST_MOCK   = 1 shl 3
  );

function km_core_get_engine_attrs(
  state: pkm_core_state
): pkm_core_attr; cdecl; external kmnkbp0 delayed;

function
km_core_process_event(
  state: pkm_core_state;
  vk: km_core_virtual_key;
  modifier_state: uint16_t;
  is_key_down: uint8_t;
  event_flags: uint16_t
): km_core_status; cdecl; external kmnkbp0 delayed;

const
  KM_KBP_EVENT_FLAG_DEFAULT = 0; // default value: hardware
  KM_KBP_EVENT_FLAG_TOUCH = 1; // set if the event is touch, otherwise hardware


// keyman_core_api_vkeys.h

const
// enum km_core_modifier_state - matches Keyman32 shift states
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
// enum km_core_modifier_mask
  KM_KBP_MODIFIER_MASK_ALL         = $7f;
  KM_KBP_MODIFIER_MASK_ALT_GR_SIM  = KM_KBP_MODIFIER_LCTRL or KM_KBP_MODIFIER_LALT;
  KM_KBP_MODIFIER_MASK_CHIRAL      = $1f;
  KM_KBP_MODIFIER_MASK_IS_CHIRAL   = $0f;
  KM_KBP_MODIFIER_MASK_NON_CHIRAL  = $7f;
  KM_KBP_MODIFIER_MASK_CAPS        = $0300;
{KM_KBP_MODIFIER_MASK_NUMLOCK     = $0C00,
  KM_KBP_MODIFIER_MASK_SCROLLLOCK  = $3000,}

// These are Windows API VKEYs, using Keyman VKEY names.

const
// enum km_kpb_virtual_key
  KM_KBP_VKEY__00       = $00;
  KM_KBP_VKEY_LBUTTON   = $01;
  KM_KBP_VKEY_RBUTTON   = $02;
  KM_KBP_VKEY_CANCEL    = $03;
  KM_KBP_VKEY_MBUTTON   = $04;
  KM_KBP_VKEY__05   = $05;
  KM_KBP_VKEY__06   = $06;
  KM_KBP_VKEY__07   = $07;
  KM_KBP_VKEY_BKSP   = $08;
  KM_KBP_VKEY_TAB   = $09;
  KM_KBP_VKEY__0A   = $0A;
  KM_KBP_VKEY__0B   = $0B;
  KM_KBP_VKEY_KP5   = $0C;
  KM_KBP_VKEY_ENTER   = $0D;
  KM_KBP_VKEY__0E   = $0E;
  KM_KBP_VKEY__0F   = $0F;
  KM_KBP_VKEY_SHIFT   = $10;
  KM_KBP_VKEY_CONTROL   = $11;
  KM_KBP_VKEY_ALT   = $12;
  KM_KBP_VKEY_PAUSE   = $13;
  KM_KBP_VKEY_CAPS   = $14;
  KM_KBP_VKEY__15   = $15;
  KM_KBP_VKEY__16   = $16;
  KM_KBP_VKEY__17   = $17;
  KM_KBP_VKEY__18   = $18;
  KM_KBP_VKEY__19   = $19;
  KM_KBP_VKEY__1A   = $1A;
  KM_KBP_VKEY_ESC   = $1B;
  KM_KBP_VKEY__1C   = $1C;
  KM_KBP_VKEY__1D   = $1D;
  KM_KBP_VKEY__1E   = $1E;
  KM_KBP_VKEY__1F   = $1F;
  KM_KBP_VKEY_SPACE   = $20;
  KM_KBP_VKEY_PGUP   = $21;
  KM_KBP_VKEY_PGDN   = $22;
  KM_KBP_VKEY_END   = $23;
  KM_KBP_VKEY_HOME   = $24;
  KM_KBP_VKEY_LEFT   = $25;
  KM_KBP_VKEY_UP   = $26;
  KM_KBP_VKEY_RIGHT   = $27;
  KM_KBP_VKEY_DOWN   = $28;
  KM_KBP_VKEY_SEL   = $29;
  KM_KBP_VKEY_PRINT   = $2A;
  KM_KBP_VKEY_EXEC   = $2B;
  KM_KBP_VKEY_PRTSCN   = $2C;
  KM_KBP_VKEY_INS   = $2D;
  KM_KBP_VKEY_DEL   = $2E;
  KM_KBP_VKEY_HELP   = $2F;
  KM_KBP_VKEY_0   = $30;
  KM_KBP_VKEY_1   = $31;
  KM_KBP_VKEY_2   = $32;
  KM_KBP_VKEY_3   = $33;
  KM_KBP_VKEY_4   = $34;
  KM_KBP_VKEY_5   = $35;
  KM_KBP_VKEY_6   = $36;
  KM_KBP_VKEY_7   = $37;
  KM_KBP_VKEY_8   = $38;
  KM_KBP_VKEY_9   = $39;
  KM_KBP_VKEY__3A   = $3A;
  KM_KBP_VKEY__3B   = $3B;
  KM_KBP_VKEY__3C   = $3C;
  KM_KBP_VKEY__3D   = $3D;
  KM_KBP_VKEY__3E   = $3E;
  KM_KBP_VKEY__3F   = $3F;
  KM_KBP_VKEY__40   = $40;
  KM_KBP_VKEY_A   = $41;
  KM_KBP_VKEY_B   = $42;
  KM_KBP_VKEY_C   = $43;
  KM_KBP_VKEY_D   = $44;
  KM_KBP_VKEY_E   = $45;
  KM_KBP_VKEY_F   = $46;
  KM_KBP_VKEY_G   = $47;
  KM_KBP_VKEY_H   = $48;
  KM_KBP_VKEY_I   = $49;
  KM_KBP_VKEY_J   = $4A;
  KM_KBP_VKEY_K   = $4B;
  KM_KBP_VKEY_L   = $4C;
  KM_KBP_VKEY_M   = $4D;
  KM_KBP_VKEY_N   = $4E;
  KM_KBP_VKEY_O   = $4F;
  KM_KBP_VKEY_P   = $50;
  KM_KBP_VKEY_Q   = $51;
  KM_KBP_VKEY_R   = $52;
  KM_KBP_VKEY_S   = $53;
  KM_KBP_VKEY_T   = $54;
  KM_KBP_VKEY_U   = $55;
  KM_KBP_VKEY_V   = $56;
  KM_KBP_VKEY_W   = $57;
  KM_KBP_VKEY_X   = $58;
  KM_KBP_VKEY_Y   = $59;
  KM_KBP_VKEY_Z   = $5A;
  KM_KBP_VKEY__5B   = $5B;
  KM_KBP_VKEY__5C   = $5C;
  KM_KBP_VKEY__5D   = $5D;
  KM_KBP_VKEY__5E   = $5E;
  KM_KBP_VKEY__5F   = $5F;
  KM_KBP_VKEY_NP0   = $60;
  KM_KBP_VKEY_NP1   = $61;
  KM_KBP_VKEY_NP2   = $62;
  KM_KBP_VKEY_NP3   = $63;
  KM_KBP_VKEY_NP4   = $64;
  KM_KBP_VKEY_NP5   = $65;
  KM_KBP_VKEY_NP6   = $66;
  KM_KBP_VKEY_NP7   = $67;
  KM_KBP_VKEY_NP8   = $68;
  KM_KBP_VKEY_NP9   = $69;
  KM_KBP_VKEY_NPSTAR   = $6A;
  KM_KBP_VKEY_NPPLUS   = $6B;
  KM_KBP_VKEY_SEPARATOR   = $6C;
  KM_KBP_VKEY_NPMINUS   = $6D;
  KM_KBP_VKEY_NPDOT   = $6E;
  KM_KBP_VKEY_NPSLASH   = $6F;
  KM_KBP_VKEY_F1   = $70;
  KM_KBP_VKEY_F2   = $71;
  KM_KBP_VKEY_F3   = $72;
  KM_KBP_VKEY_F4   = $73;
  KM_KBP_VKEY_F5   = $74;
  KM_KBP_VKEY_F6   = $75;
  KM_KBP_VKEY_F7   = $76;
  KM_KBP_VKEY_F8   = $77;
  KM_KBP_VKEY_F9   = $78;
  KM_KBP_VKEY_F10   = $79;
  KM_KBP_VKEY_F11   = $7A;
  KM_KBP_VKEY_F12   = $7B;
  KM_KBP_VKEY_F13   = $7C;
  KM_KBP_VKEY_F14   = $7D;
  KM_KBP_VKEY_F15   = $7E;
  KM_KBP_VKEY_F16   = $7F;
  KM_KBP_VKEY_F17   = $80;
  KM_KBP_VKEY_F18   = $81;
  KM_KBP_VKEY_F19   = $82;
  KM_KBP_VKEY_F20   = $83;
  KM_KBP_VKEY_F21   = $84;
  KM_KBP_VKEY_F22   = $85;
  KM_KBP_VKEY_F23   = $86;
  KM_KBP_VKEY_F24   = $87;
  KM_KBP_VKEY__88   = $88;
  KM_KBP_VKEY__89   = $89;
  KM_KBP_VKEY__8A   = $8A;
  KM_KBP_VKEY__8B   = $8B;
  KM_KBP_VKEY__8C   = $8C;
  KM_KBP_VKEY__8D   = $8D;
  KM_KBP_VKEY__8E   = $8E;
  KM_KBP_VKEY__8F   = $8F;
  KM_KBP_VKEY_NUMLOCK   = $90;
  KM_KBP_VKEY_SCROLL   = $91;
  KM_KBP_VKEY__92   = $92;
  KM_KBP_VKEY__93   = $93;
  KM_KBP_VKEY__94   = $94;
  KM_KBP_VKEY__95   = $95;
  KM_KBP_VKEY__96   = $96;
  KM_KBP_VKEY__97   = $97;
  KM_KBP_VKEY__98   = $98;
  KM_KBP_VKEY__99   = $99;
  KM_KBP_VKEY__9A   = $9A;
  KM_KBP_VKEY__9B   = $9B;
  KM_KBP_VKEY__9C   = $9C;
  KM_KBP_VKEY__9D   = $9D;
  KM_KBP_VKEY__9E   = $9E;
  KM_KBP_VKEY__9F   = $9F;
  KM_KBP_VKEY__A0   = $A0;
  KM_KBP_VKEY__A1   = $A1;
  KM_KBP_VKEY__A2   = $A2;
  KM_KBP_VKEY__A3   = $A3;
  KM_KBP_VKEY__A4   = $A4;
  KM_KBP_VKEY__A5   = $A5;
  KM_KBP_VKEY__A6   = $A6;
  KM_KBP_VKEY__A7   = $A7;
  KM_KBP_VKEY__A8   = $A8;
  KM_KBP_VKEY__A9   = $A9;
  KM_KBP_VKEY__AA   = $AA;
  KM_KBP_VKEY__AB   = $AB;
  KM_KBP_VKEY__AC   = $AC;
  KM_KBP_VKEY__AD   = $AD;
  KM_KBP_VKEY__AE   = $AE;
  KM_KBP_VKEY__AF   = $AF;
  KM_KBP_VKEY__B0   = $B0;
  KM_KBP_VKEY__B1   = $B1;
  KM_KBP_VKEY__B2   = $B2;
  KM_KBP_VKEY__B3   = $B3;
  KM_KBP_VKEY__B4   = $B4;
  KM_KBP_VKEY__B5   = $B5;
  KM_KBP_VKEY__B6   = $B6;
  KM_KBP_VKEY__B7   = $B7;
  KM_KBP_VKEY__B8   = $B8;
  KM_KBP_VKEY__B9   = $B9;
  KM_KBP_VKEY_COLON   = $BA;
  KM_KBP_VKEY_EQUAL   = $BB;
  KM_KBP_VKEY_COMMA   = $BC;
  KM_KBP_VKEY_HYPHEN   = $BD;
  KM_KBP_VKEY_PERIOD   = $BE;
  KM_KBP_VKEY_SLASH   = $BF;
  KM_KBP_VKEY_BKQUOTE   = $C0;
  KM_KBP_VKEY__C1   = $C1;
  KM_KBP_VKEY__C2   = $C2;
  KM_KBP_VKEY__C3   = $C3;
  KM_KBP_VKEY__C4   = $C4;
  KM_KBP_VKEY__C5   = $C5;
  KM_KBP_VKEY__C6   = $C6;
  KM_KBP_VKEY__C7   = $C7;
  KM_KBP_VKEY__C8   = $C8;
  KM_KBP_VKEY__C9   = $C9;
  KM_KBP_VKEY__CA   = $CA;
  KM_KBP_VKEY__CB   = $CB;
  KM_KBP_VKEY__CC   = $CC;
  KM_KBP_VKEY__CD   = $CD;
  KM_KBP_VKEY__CE   = $CE;
  KM_KBP_VKEY__CF   = $CF;
  KM_KBP_VKEY__D0   = $D0;
  KM_KBP_VKEY__D1   = $D1;
  KM_KBP_VKEY__D2   = $D2;
  KM_KBP_VKEY__D3   = $D3;
  KM_KBP_VKEY__D4   = $D4;
  KM_KBP_VKEY__D5   = $D5;
  KM_KBP_VKEY__D6   = $D6;
  KM_KBP_VKEY__D7   = $D7;
  KM_KBP_VKEY__D8   = $D8;
  KM_KBP_VKEY__D9   = $D9;
  KM_KBP_VKEY__DA   = $DA;
  KM_KBP_VKEY_LBRKT   = $DB;
  KM_KBP_VKEY_BKSLASH   = $DC;
  KM_KBP_VKEY_RBRKT   = $DD;
  KM_KBP_VKEY_QUOTE   = $DE;
  KM_KBP_VKEY_oDF   = $DF;
  KM_KBP_VKEY_oE0   = $E0;
  KM_KBP_VKEY_oE1   = $E1;
  KM_KBP_VKEY_oE2   = $E2; // 102nd key on European layouts
  KM_KBP_VKEY_oE3   = $E3;
  KM_KBP_VKEY_oE4   = $E4;
  KM_KBP_VKEY__E5   = $E5;
  KM_KBP_VKEY_oE6   = $E6;
  KM_KBP_VKEY__E7   = $E7;
  KM_KBP_VKEY__E8   = $E8;
  KM_KBP_VKEY_oE9   = $E9;
  KM_KBP_VKEY_oEA   = $EA;
  KM_KBP_VKEY_oEB   = $EB;
  KM_KBP_VKEY_oEC   = $EC;
  KM_KBP_VKEY_oED   = $ED;
  KM_KBP_VKEY_oEE   = $EE;
  KM_KBP_VKEY_oEF   = $EF;
  KM_KBP_VKEY_oF0   = $F0;
  KM_KBP_VKEY_oF1   = $F1;
  KM_KBP_VKEY_oF2   = $F2;
  KM_KBP_VKEY_oF3   = $F3;
  KM_KBP_VKEY_oF4   = $F4;
  KM_KBP_VKEY_oF5   = $F5;
  KM_KBP_VKEY__F6   = $F6;
  KM_KBP_VKEY__F7   = $F7;
  KM_KBP_VKEY__F8   = $F8;
  KM_KBP_VKEY__F9   = $F9;
  KM_KBP_VKEY__FA   = $FA;
  KM_KBP_VKEY__FB   = $FB;
  KM_KBP_VKEY__FC   = $FC;
  KM_KBP_VKEY__FD   = $FD;
  KM_KBP_VKEY__FE   = $FE;
  KM_KBP_VKEY__FF   = $FF;

// keyman_core_api_consts.h
const
  // Defined environment options for KMX processor
  KM_KBP_KMX_ENV_PLATFORM                       = 'platform';
  KM_KBP_KMX_ENV_BASELAYOUT                     = 'baseLayout';
  KM_KBP_KMX_ENV_BASELAYOUTALT                  = 'baseLayoutAlt';
  KM_KBP_KMX_ENV_SIMULATEALTGR                  = 'simulateAltgr';
  KM_KBP_KMX_ENV_CAPSLOCK                       = 'capsLock';
  KM_KBP_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT = 'baseLayoutGivesCtrlRAltForRAlt';

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
