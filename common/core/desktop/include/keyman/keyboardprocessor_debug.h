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
#include <keyman/keyboardprocessor_bits.h>
#include <keyman/keyboardprocessor_vkeys.h>

/**
 * The maximum size of context in km_kbp_cp units for a single debug
 * event. This is taken from MAXCONTEXT in keyman32 (Windows) and is purely
 * a convenience value. We can increase it if there is a demonstrated need.
 */
#define DEBUG_MAX_CONTEXT 80

/**
 * Input key event data. The `character` member is derived from
 * a US English key event for vk + modifier_state, and is 0 if
 * the vk + modifier_state do not generate a character.
 *
 * Used only in event type KM_KBP_DEBUG_BEGIN.
 */
typedef struct {
  uint16_t vk;
  uint16_t modifier_state;
  char16_t character;
} km_kbp_state_debug_key_info;

/**
 * KMX processor data for each event. kmx_base.h defines the types that are
 * passed in here, read only. Warning: context may contain sentinel values
 * for markers, with the kmx UC_SENTINEL, CODE_CONTEXT, DEADKEY pattern.
 *
 * The context value here will be an intermediate value, and may differ
 * from event to event as the context can be rewritten for each rule match.
 *
 * Used in all event types except KM_KBP_DEBUG_BEGIN, KM_KBP_DEBUG_END.
 */
typedef struct {
  km_kbp_cp context[DEBUG_MAX_CONTEXT];
  void *group;  // LPGROUP
  void *store;  // LPSTORE
  void *rule;   // LPKEY
} km_kbp_state_debug_kmx_info;

/**
 * A single debug event.
 */
typedef struct {
  uint32_t  type; // 32 bits is better optimized than 8 bits
  uint32_t flags;
  union {
    km_kbp_state_debug_key_info key_info;
    km_kbp_state_debug_kmx_info kmx_info;
  };
} km_kbp_state_debug_item;

/**
 * A single debug event.
 */
enum km_kbp_debug_type {
  KM_KBP_DEBUG_BEGIN = 0,
  //KM_KBP_DEBUG_BEGIN_ANSI = 1, // not supported; instead rewrite ansi keyboards to Unicode with mcompile
  KM_KBP_DEBUG_GROUP_ENTER = 2,
  KM_KBP_DEBUG_GROUP_EXIT = 3,
  KM_KBP_DEBUG_RULE_ENTER = 4,
  KM_KBP_DEBUG_RULE_EXIT = 5,
  KM_KBP_DEBUG_MATCH_ENTER = 6,
  KM_KBP_DEBUG_MATCH_EXIT = 7,
  KM_KBP_DEBUG_NOMATCH_ENTER = 8,
  KM_KBP_DEBUG_NOMATCH_EXIT = 9,
  KM_KBP_DEBUG_END = 10,
};

// Flags for KM_KBP_DEBUG_GROUP_EXIT
#define KM_KBP_DEBUG_FLAG_RECURSIVE_OVERFLOW  0x0001
#define KM_KBP_DEBUG_FLAG_NOMATCH             0x0002

// Flags for KM_KBP_DEBUG_BEGIN
// TODO: do we need this at all?
#define KM_KBP_DEBUG_FLAG_UNICODE             0x0001  // Always set

// Flags for KM_KBP_DEBUG_END
#define KM_KBP_DEBUG_FLAG_OUTPUTKEYSTROKE     0x0001

/**
 * Enable or disable debug tracing
 *
 * @param state     Pointer to initialized state
 * @param value     Set to 1 to enable debugging, 0 to disable
 *
 * @returns   KM_KBP_STATUS_OK on success
 */
KMN_API
km_kbp_status
km_kbp_state_debug_set(km_kbp_state *state, int value);

/**
 * Get current debug tracing status
 *
 * @param state     Pointer to initialized state
 *
 * @returns   1 if debugging is enabled, 0 otherwise
 */
KMN_API
uint8_t
km_kbp_state_debug_get(km_kbp_state const *state);

/**
 * Read current debug trace log
 *
 * @param state     Pointer to initialized state
 * @param num_items Pointer to variable to receive size of list; may be
 *                  nullptr if not required.
 *
 * @returns   pointer to read only array of debug item events,
 *            with last entry guaranteed to be KM_KBP_DEBUG_END.
 */
KMN_API
km_kbp_state_debug_item const *
km_kbp_state_debug_items(km_kbp_state const *state, size_t *num_items);
