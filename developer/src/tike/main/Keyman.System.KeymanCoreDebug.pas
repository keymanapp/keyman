{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Debug interface for Keyman Core, matches keyman_core_api_debug.h
}
unit Keyman.System.KeymanCoreDebug;

interface

uses
  Keyman.System.KeymanCore;

{$WARN SYMBOL_PLATFORM OFF}
{$ALIGN 8}

///
/// The maximum size of context in km_core_cu units for a single debug
/// event. This is taken from MAXCONTEXT in keyman32 (Windows) and is purely
/// a convenience value. We can increase it if there is a demonstrated need.
///
const DEBUG_MAX_CONTEXT = 80;

///
/// The number of stores that can be processed in a rule. This is taken from
/// MAXSTOREOFFSETS in keyman32 (Windows) and is purely a convenience value.
/// We can increase it if there is a demonstrated need.
///
const DEBUG_MAX_STORE_OFFSETS = 20;
const DEBUG_STORE_OFFSETS_SIZE = (DEBUG_MAX_STORE_OFFSETS*2+1);

///
/// These modifier flags are used internally in the kmx engine, so will be
/// exposed only in debugging modifier states.
///
const KM_CORE_MODIFIER_VIRTUALKEY      = $4000;
const KM_CORE_MODIFIER_VIRTUALCHARKEY  = $8000;

///
/// Input key event data. The `character` member is derived from
/// a US English key event for vk + modifier_state, and is 0 if
/// the vk + modifier_state do not generate a character.
///
/// Used only in event type KM_CORE_DEBUG_BEGIN.
///
type

km_core_state_debug_key_info = record
  vk: uint16_t;
  modifier_state: uint16_t;
  character: char16_t;
end;

pkm_core_state_debug_key_info = ^km_core_state_debug_key_info;

km_core_state_debug_kmx_option_info = record
  store: Pointer;       // LPSTORE
  value: array[0..DEBUG_MAX_CONTEXT-1] of km_core_cu;  // value to be saved into the store
end;

pkm_core_state_debug_kmx_option_info = ^km_core_state_debug_kmx_option_info;

///
/// KMX processor data for each event. kmx_base.h defines the types that are
/// passed in here, read only. Warning: context may contain sentinel values
/// for markers, with the kmx UC_SENTINEL, CODE_CONTEXT, DEADKEY pattern.
///
/// The context value here will be an intermediate value, and may differ
/// from event to event as the context can be rewritten for each rule match.
///
/// Used in all event types except KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_END.
///

km_core_state_debug_kmx_info = record
  context: array [0..DEBUG_MAX_CONTEXT-1] of km_core_cu;     // The context matched by the rule (? may not need this?) // TODO: rename to context_matched
  group: Pointer;  // LPGROUP
  rule: Pointer;   // LPKEY
  store_offsets: array [0..DEBUG_STORE_OFFSETS_SIZE-1] of uint16_t;	// pairs--store, char position, terminated by 0xFFFF // TODO use a better structure here

  /// Track the actions index in the actions that will be returned to
  /// the debugger; the debugger uses this to determine when to
  /// execute the actions when single-stepping.
  first_action: uint16_t;
  option: km_core_state_debug_kmx_option_info;
end;

pkm_core_state_debug_kmx_info = ^km_core_state_debug_kmx_info;

///
/// A single debug event.
///
///

{$ALIGN 8}
km_core_state_debug_item = record
  _type: uint32_t; // TODO: _type as enum, 8 bit with padding? 32 bits is better optimized than 8 bits
  flags: uint32_t;
  key_info: km_core_state_debug_key_info;
  kmx_info: km_core_state_debug_kmx_info;
end;

pkm_core_state_debug_item = ^km_core_state_debug_item;

///
/// A single debug event.
///

km_core_debug_type = type uint32_t;

// These types are used only for debugging convenience
type
  km_core_state_debug_item_array = array[0..100] of km_core_state_debug_item;
  pkm_core_state_debug_item_array = ^km_core_state_debug_item_array;

const
  KM_CORE_DEBUG_BEGIN = 0;
  //KM_CORE_DEBUG_BEGIN_ANSI: km_core_debug_type = 1, // not supported; instead rewrite ansi keyboards to Unicode with mcompile
  KM_CORE_DEBUG_GROUP_ENTER = 2;
  KM_CORE_DEBUG_GROUP_EXIT = 3;
  KM_CORE_DEBUG_RULE_ENTER = 4;
  KM_CORE_DEBUG_RULE_EXIT = 5;
  KM_CORE_DEBUG_MATCH_ENTER = 6;
  KM_CORE_DEBUG_MATCH_EXIT = 7;
  KM_CORE_DEBUG_NOMATCH_ENTER = 8;
  KM_CORE_DEBUG_NOMATCH_EXIT = 9;
  KM_CORE_DEBUG_END = 10;
  KM_CORE_DEBUG_SET_OPTION = 11;

// Flags for KM_CORE_DEBUG_GROUP_EXIT
const KM_CORE_DEBUG_FLAG_RECURSIVE_OVERFLOW = $0001;
const KM_CORE_DEBUG_FLAG_NOMATCH            = $0002;

// Flags for KM_CORE_DEBUG_BEGIN
// TODO: do we need this at all?
const KM_CORE_DEBUG_FLAG_UNICODE            = $0001; // Always set

// Flags for KM_CORE_DEBUG_END
const KM_CORE_DEBUG_FLAG_OUTPUTKEYSTROKE    = $0001;

///
/// Enable or disable debug tracing
///
/// @param state     Pointer to initialized state
/// @param value     Set to 1 to enable debugging, 0 to disable
///
/// @returns   KM_CORE_STATUS_OK on success
///
function km_core_state_debug_set(
  state: pkm_core_state;
  value: integer
): km_core_status; cdecl; external keymancore delayed;

///
/// Get current debug tracing status
///
/// @param state     Pointer to initialized state
///
/// @returns   1 if debugging is enabled, 0 otherwise
///
function km_core_state_debug_get(
  state: pkm_core_state
): uint8_t; cdecl; external keymancore delayed;

///
/// Read current debug trace log
///
/// @param state     Pointer to initialized state
/// @param num_items Pointer to variable to receive size of list; may be
///                  nullptr if not required.
///
/// @returns   pointer to read only array of debug item events,
///            with last entry guaranteed to be KM_CORE_DEBUG_END.
///
function km_core_state_debug_items(
  state: pkm_core_state;
  num_items: PCardinal
): pkm_core_state_debug_item; cdecl; external keymancore delayed;

implementation

initialization
  // Assertions generated by debug_api.cpp for win32 x86.
  // Usage: debug-api.exe --print-sizeof

  //keyman_core_api.h:
  Assert(sizeof(km_core_context_item) = 8);
  Assert(sizeof(km_core_context_item) = 8);
  Assert(sizeof(km_core_action_item) = 12);
  Assert(sizeof(km_core_option_item) = 12);
  Assert(sizeof(km_core_keyboard_attrs) = 16);
  Assert(sizeof(km_core_attr) = 16);

  //keyman_core_api_debug.h:
  Assert(sizeof(km_core_state_debug_item) = 432);
  Assert(sizeof(km_core_state_debug_key_info) = 6);
  Assert(sizeof(km_core_state_debug_kmx_info) = 416);
  Assert(sizeof(km_core_state_debug_kmx_option_info) = 164);
  Assert(sizeof(km_core_debug_type) = 4);
end.
