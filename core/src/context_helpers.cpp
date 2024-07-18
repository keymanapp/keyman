/*
  Copyright:    © 2018-2024 SIL International.
  Description:  Helper functions for context data type conversions
  Create Date:  18 Jan 2024
  Authors:      Marc Durdin
  History:      18 Jan 2024 - MCD - Refactor from km_core_state_context_set_if_needed.cpp
*/
#include <cassert>

#include "keyman_core.h"
#include "context.hpp"

using namespace km::core;

/**
 * Retrieves the context as a km_core_cu string, dropping markers
 */
km_core_cu* km::core::get_context_as_string(km_core_context *context) {
  assert(context != nullptr);
  if(context == nullptr) {
    return nullptr;
  }

  size_t buf_size = 0;
  km_core_context_item* context_items = nullptr;

  if(km_core_context_get(context, &context_items) != KM_CORE_STATUS_OK) {
    return nullptr;
  }

  if(context_items_to_utf16(context_items, nullptr, &buf_size) != KM_CORE_STATUS_OK) {
    km_core_context_items_dispose(context_items);
    return nullptr;
  }

  km_core_cu *app_context_string = new km_core_cu[buf_size];

  km_core_status status = context_items_to_utf16(context_items, app_context_string, &buf_size);
  km_core_context_items_dispose(context_items);

  if(status != KM_CORE_STATUS_OK) {
    return nullptr;
  }

  return app_context_string;
}

/**
 * Updates the context from the new_context km_core_cu string
 */
km_core_status km::core::set_context_from_string(km_core_context *context, km_core_cu const *new_context) {
  assert(context != nullptr);
  assert(new_context != nullptr);
  if(context == nullptr || new_context == nullptr) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  km_core_context_item* new_context_items = nullptr;

  km_core_status status = context_items_from_utf16(new_context, &new_context_items);
  if (status != KM_CORE_STATUS_OK) {
    return status;
  }

  km_core_context_set(context, new_context_items);
  km_core_context_items_dispose(new_context_items);

  return KM_CORE_STATUS_OK;
}
