/*
  Copyright:    © 2018 SIL International.
  Description:  Internal context class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_core_context_api.cpp
*/

#pragma once
#include <list>
#include <vector>
#include <cassert>
#include "keyman_core.h"

// Forward declarations
class json;

namespace km {
namespace core
{

// This will likely be replaced with a class implementing a more space
// efficient data structure such as a ring buffer or bounded queue.
class context: public std::list<km_core_context_item>
{
public:
  bool has_markers = true;
  void push_character(km_core_usv);
  void push_marker(uint32_t);
};


inline
void context::push_character(km_core_usv usv) {
  emplace_back(km_core_context_item { KM_CORE_CT_CHAR, {0,}, {usv} });
}


inline
void context::push_marker(uint32_t marker) {
  assert(has_markers);
  if(!has_markers) return;
  emplace_back(km_core_context_item { KM_CORE_CT_MARKER, {0,}, {marker} });
}

// Context helper functions

km_core_cu* get_context_as_string(km_core_context *context);
km_core_status set_context_from_string(km_core_context *context, km_core_cu const *new_context);

} // namespace core
} // namespace km

json & operator << (json &, km::core::context const &);
json & operator << (json &, km_core_context_item const &);


struct km_core_context : public km::core::context
{
};

// The following functions were public APIs in previous versions of Keyman Core.
// However, with the move of context caching responsibility to Core with 17.0,
// they have been moved to internal

/**
 * Convert a UTF16 encoded Unicode string into an array of `km_core_context_item`
 * structures. Allocates memory as needed.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be
 *           allocated for the output buffer.
 *         * `KM_CORE_STATUS_INVALID_UTF`: In the event the UTF16 string cannot
 *           be decoded because it contains unpaired surrogate codeunits.
 *
 * @param text    a pointer to a null terminated array of utf16 encoded data.
 * @param out_ptr a pointer to the result variable: A pointer to the start of
 *                the `km_core_context_item` array containing the representation
 *                of the input string. Terminated with a type of
 *                `KM_CORE_CT_END`. Must be disposed of with
 *                `km_core_context_items_dispose`.
 */
km_core_status
context_items_from_utf16(km_core_cu const *text,
                                km_core_context_item **out_ptr);

/**
 * Convert a context item array into a UTF-16 encoded string placing it into the
 * supplied buffer of specified size, and return the number of code units
 * actually used in the conversion. If null is passed as the buffer the number
 * of codeunits required is returned. Any markers in the context will not be
 * included in the output buffer.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large
 *           enough. `buf_size` will contain the space required. The contents
 *           of the buffer are undefined.
 *
 * @param context_items A pointer to the start of an array
 *                      `km_core_context_item`. Must be terminated with a type
 *                      of `KM_CORE_CT_END`.
 * @param buf           A pointer to the buffer to place the UTF-16 string into.
 *                      May be null to request size calculation.
 * @param buf_size      A pointer to the result variable: The size of the
 *                      supplied buffer in codeunits if `buf` is given. On
 *                      return will be the size required if `buf` is null.
 */
km_core_status
context_items_to_utf16(km_core_context_item const *item,
                              km_core_cu *buf,
                              size_t *buf_size);

/**
 * Convert a context item array into a UTF-8 encoded string placing it into the
 * supplied buffer of specified size, and return the number of code units
 * actually used in the conversion. If null is passed as the buffer the number
 * of codeunits required is returned. Any markers in the context will not be
 * included in the output buffer.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large
 *           enough. `buf_size` will contain the space required. The contents
 *           of the buffer are undefined.
 *
 * @param context_items A pointer to the start of an array
 *                      `km_core_context_item`. Must be terminated with a type
 *                      of `KM_CORE_CT_END`.
 * @param buf           A pointer to the buffer to place the UTF-8 string into.
 *                      May be null to request size calculation.
 * @param buf_size      A pointer to the result variable: The size of the
 *                      supplied buffer in codeunits if `buf` is given. On
 *                      return will be the size required if `buf` is null.
 */
km_core_status
context_items_to_utf8(km_core_context_item const *item,
                              char *buf,
                              size_t *buf_size);

/**
 * Convert a context item array into a UTF-32 encoded string placing it into
 * the supplied buffer of specified size, and return the number of codepoints
 * actually used in the conversion. If null is passed as the buffer the
 * number of codepoints required is returned. Any markers in the context will
 * not be included in the output buffer.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large
 *           enough. `buf_size` will contain the space required. The contents
 *           of the buffer are undefined.
 *
 * @param context_items A pointer to the start of an array
 *                      `km_core_context_item`. Must be terminated with a type
 *                      of `KM_CORE_CT_END`.
 * @param buf           A pointer to the buffer to place the UTF-32 string into.
 *                      May be null to request size calculation.
 * @param buf_size      A pointer to the result variable: The size of the
 *                      supplied buffer in codepoints if `buf` is given. On
 *                      return will be the size required if `buf` is null.
 */
km_core_status
context_items_to_utf32(km_core_context_item const *item,
                              km_core_usv *buf,
                              size_t *buf_size);

/**
 * Add more items to the end (insertion point) of the context. If these exceed
 * the maximum context length the same number of items will be dropped from the
 * beginning of the context.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be
 *           allocated to grow the context buffer internally.
 *
 * @param context       A pointer to an opaque context object.
 * @param context_items A pointer to the start of the `KM_CORE_CT_END`
 *                      terminated array of `km_core_context_item` to append.
 */
km_core_status
context_append(km_core_context *context,
                      km_core_context_item const *context_items);

/**
 * Insert items at the front of the context.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be
 *           allocated to grow the context buffer internally.
 *
 * @param context       A pointer to an opaque context object.
 * @param context_items A pointer to the start of the `KM_CORE_CT_END`
 *                      terminated array of `km_core_context_item` to prepend.
 * @param num           Maximum number of `km_core_context_item` elements to prepend.
 */
km_core_status
context_prepend(km_core_context *context, km_core_context_item const *context_items, size_t num = SIZE_MAX);

/**
 * Remove a specified number of items from the front or end of the context.
 *
 * @return km_core_status
 *         * `KM_CORE_STATUS_OK`: On success.
 *         * `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are
 *           null.
 *         * `KM_CORE_STATUS_NO_MEM`: in the event it cannot allocated enough
 *           memory to grow the context internally.
 *
 * @param context       A pointer to an opaque context object.
 * @param num           The number of items to remove from the context.
 * @param from_end      Whether to remove from the end or front of the context.
 */
km_core_status
context_shrink(km_core_context *context, size_t num, bool from_end = true);
