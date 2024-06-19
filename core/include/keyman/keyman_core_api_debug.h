/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Keyboard Processor API - Debugger Interfaces
 *
 * The debugger interfaces are still very dependent on .kmx
 * objects.
 *
 * Note: this file is subject to change; the debugger
 *       interfaces are not stable across versions.
 *
 */

#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <keyman/keyman_core_api_bits.h>
#include <keyman/keyman_core_api_vkeys.h>

#if defined(__cplusplus)
extern "C"
{
#endif

/**
 * The maximum size of context in km_core_cu units for a single debug
 * event. This is taken from MAXCONTEXT in keyman32 (Windows) and is purely
 * a convenience value. We can increase it if there is a demonstrated need.
 */
#define DEBUG_MAX_CONTEXT 80

/**
 * The number of stores that can be processed in a rule. This is taken from
 * MAXSTOREOFFSETS in keyman32 (Windows) and is purely a convenience value.
 * We can increase it if there is a demonstrated need.
 */
#define DEBUG_MAX_STORE_OFFSETS 20
#define DEBUG_STORE_OFFSETS_SIZE (DEBUG_MAX_STORE_OFFSETS*2+1)

/**
 * These modifier flags are used internally in the kmx engine, so will be
 * exposed only in debugging modifier states.
 */
#define KM_CORE_MODIFIER_VIRTUALKEY      0x4000
#define KM_CORE_MODIFIER_VIRTUALCHARKEY  0x8000

/**
 * Input key event data. The `character` member is derived from
 * a US English key event for vk + modifier_state, and is 0 if
 * the vk + modifier_state do not generate a character.
 *
 * Used only in event type KM_CORE_DEBUG_BEGIN.
 */
typedef struct {
  uint16_t vk;
  uint16_t modifier_state;
  char16_t character;
} km_core_state_debug_key_info;

/**
 * Option event data.
 *
 * Used only in event type KM_CORE_DEBUG_SET_OPTION.
 */
typedef struct {
  void *store;      // LPSTORE
  km_core_cu value[DEBUG_MAX_CONTEXT];  // value to be saved into the store
} km_core_state_debug_kmx_option_info;

/**
 * KMX processor data for each event. kmx_base.h defines the types that are
 * passed in here, read only. Warning: context may contain sentinel values
 * for markers, with the kmx UC_SENTINEL, CODE_CONTEXT, DEADKEY pattern.
 *
 * The context value here will be an intermediate value, and may differ
 * from event to event as the context can be rewritten for each rule match.
 *
 * Used in all event types except KM_CORE_DEBUG_BEGIN, KM_CORE_DEBUG_END.
 */

typedef struct {
  km_core_cu context[DEBUG_MAX_CONTEXT];     // The context matched by the rule (? may not need this?) // TODO: rename to context_matched
  void *group;  // LPGROUP
  void *rule;   // LPKEY
  uint16_t store_offsets[DEBUG_STORE_OFFSETS_SIZE];	// pairs--store, char position, terminated by 0xFFFF // TODO use a better structure here

  /// Track the actions index in the actions that will be returned to
  /// the debugger; the debugger uses this to determine when to
  /// execute the actions when single-stepping.
  uint16_t first_action;
  km_core_state_debug_kmx_option_info option;
} km_core_state_debug_kmx_info;

/**
 * A single debug event.
 */
typedef struct {
  uint32_t  type; // 32 bits is better optimized than 8 bits
  uint32_t flags;
  km_core_state_debug_key_info key_info;
  km_core_state_debug_kmx_info kmx_info;
} km_core_state_debug_item;

/**
 * A single debug event.
 */
enum km_core_debug_type {
  KM_CORE_DEBUG_BEGIN = 0,
  //KM_CORE_DEBUG_BEGIN_ANSI = 1, // not supported; instead rewrite ansi keyboards to Unicode with mcompile
  KM_CORE_DEBUG_GROUP_ENTER = 2,
  KM_CORE_DEBUG_GROUP_EXIT = 3,
  KM_CORE_DEBUG_RULE_ENTER = 4,
  KM_CORE_DEBUG_RULE_EXIT = 5,
  KM_CORE_DEBUG_MATCH_ENTER = 6,
  KM_CORE_DEBUG_MATCH_EXIT = 7,
  KM_CORE_DEBUG_NOMATCH_ENTER = 8,
  KM_CORE_DEBUG_NOMATCH_EXIT = 9,
  KM_CORE_DEBUG_END = 10,
  KM_CORE_DEBUG_SET_OPTION = 11,
};

// Flags for KM_CORE_DEBUG_GROUP_EXIT
#define KM_CORE_DEBUG_FLAG_RECURSIVE_OVERFLOW  0x0001
#define KM_CORE_DEBUG_FLAG_NOMATCH             0x0002

// Flags for KM_CORE_DEBUG_BEGIN
// TODO: do we need this at all?
#define KM_CORE_DEBUG_FLAG_UNICODE             0x0001  // Always set

// Flags for KM_CORE_DEBUG_END
#define KM_CORE_DEBUG_FLAG_OUTPUTKEYSTROKE     0x0001

/**
 * Enable or disable debug tracing
 *
 * @param state     Pointer to initialized state
 * @param value     Set to 1 to enable debugging, 0 to disable
 *
 * @returns   KM_CORE_STATUS_OK on success
 */
KMN_API
km_core_status
km_core_state_debug_set(km_core_state *state, int value);

/**
 * Get current debug tracing status
 *
 * @param state     Pointer to initialized state
 *
 * @returns   1 if debugging is enabled, 0 otherwise
 */
KMN_API
uint8_t
km_core_state_debug_get(km_core_state const *state);

/**
 * Read current debug trace log
 *
 * @param state     Pointer to initialized state
 * @param num_items Pointer to variable to receive size of list; may be
 *                  nullptr if not required.
 *
 * @returns   pointer to read only array of debug item events,
 *            with last entry guaranteed to be KM_CORE_DEBUG_END.
 */
KMN_API
km_core_state_debug_item const *
km_core_state_debug_items(km_core_state const *state, size_t *num_items);

#if defined(__cplusplus)
} // extern "C"
#endif
