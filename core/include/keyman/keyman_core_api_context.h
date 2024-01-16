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

// Currently, the Core unit tests use private context APIs defined in
// keyman_core_api_context.h, which are unused by other consumers. We are
// hoping to remove these entirely in the future, so we restrict access
// by default with this macro. Keyman Core internally uses these functions
// #define _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

#if defined(__cplusplus)
extern "C"
{
#endif

typedef struct km_core_context     km_core_context;

// ----------------------------------------------------------------------------------
// Context APIs are now available only to the keyboard debugger and Core unit
// tests (17.0)
// ----------------------------------------------------------------------------------

/*
### Context
The context is the text prior to the insertion point (caret, cursor).
The context is constructed by the Platform layer, typically by interrogating the
Client Application.  The context will be updated by the engine for keystroke
events.  If the Platform layer code caches the context, the context should be
reset when a context state change is detected. Context state changes can occur
when the user uses the mouse to move the insertion point, uses cursor keys,
switches applications or input fields, or presses hotkeys such as Ctrl+N to
start a new document. The full set of context state change triggers is up to the
Platform layer.

Context can also contain positional Markers (also known as 'deadkeys' in kmn
keyboards), which are transitory state flags that are erased whenever a context
state change is detected. Markers are always controlled by the Engine.

Contexts are always owned by their state.  They may be set to a list of
context_items or interrogated for their current list of context items.
```c
*/
enum km_core_context_type {
  KM_CORE_CT_END,
  KM_CORE_CT_CHAR,
  KM_CORE_CT_MARKER
};

typedef struct {
  uint8_t   type;
  uint8_t   _reserved[3];
  union {
    km_core_usv  character;
    uint32_t    marker;
  };
} km_core_context_item;

#define KM_CORE_CONTEXT_ITEM_END {KM_CORE_CT_END, {0,}, {0,}}

/*
```
### `km_core_state_get_intermediate_context`
##### Description:
Get access to the state object's keyboard processor's intermediate context. This context
is used during an IMX callback, part way through processing a keystroke.
##### Return:
A pointer to an context item array. Must be disposed of by a call
to `km_core_context_items_dispose`.
##### Parameters:
- __state__: A pointer to the opaque state object to be queried.

```c
*/
KMN_API
km_core_status
km_core_state_get_intermediate_context(km_core_state *state, km_core_context_item ** context_items);

/*
```
### `km_core_context_items_dispose`
##### Description:
Free the allocated memory belonging to a `km_core_context_item` array previously
returned by `km_core_context_items_from_utf16` or `km_core_context_get`
##### Parameters:
- __context_items__: A pointer to the start of the `km_core_context_item` array
    to be disposed of.

```c
*/
KMN_API
void
km_core_context_items_dispose(km_core_context_item *context_items);

/**
 * Get access to the state object's cached context.
 * @param   state   A pointer to the opaque state object to be queried.
 * @returns   A pointer to an opaque context object. This pointer is valid for the
 *            lifetime of the state object. If null is passed in, then null is
 *            returned.
 */
KMN_API
km_core_context *
km_core_state_context(km_core_state const *state);

/**
 * Get access to the state object's application context.
 * @param   state   A pointer to the opaque state object to be queried.
 * @returns   A pointer to an opaque context object. This pointer is valid for the
 *            lifetime of the state object. If null is passed in, then null is
 *            returned.
 */
KMN_API
km_core_context *
km_core_state_app_context(km_core_state const *state);

/*
```
### `km_core_context_items_from_utf16`
##### Description:
Convert a UTF16 encoded Unicode string into an array of `km_core_context_item`
structures. Allocates memory as needed.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be allocated for the
  output buffer.
- `KM_CORE_STATUS_INVALID_UTF`: In the event the UTF16 string cannot be decoded
  because it contains unpaired surrogate codeunits.
##### Parameters:
- __text__: a pointer to a null terminated array of utf16 encoded data.
- __out_ptr__: a pointer to the result variable:
    A pointer to the start of the `km_core_context_item` array containing the
    representation of the input string.
    Terminated with a type of `KM_CORE_CT_END`. Must be disposed of with
    `km_core_context_items_dispose`.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_items_from_utf16(km_core_cp const *text,
                                km_core_context_item **out_ptr);

#endif

/*
```
### `km_core_context_items_from_utf8`
##### Description:
Convert an UTF8 encoded Unicode string into an array of `km_core_context_item`
structures. Allocates memory as needed.
##### Status:
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event it cannot allocate enough memory for the
  output buffer.
- `KM_CORE_STATUS_INVALID_UTF`: In the event the UTF8 string cannot be
decoded.
##### Parameters:
- __text__: a pointer to a null terminated array of utf8 encoded data.
- __out_ptr__: a pointer to the result variable:
    A pointer to the  start of the `km_core_context_item` array containing the
    representation of the input string.
    Terminated with a type of `KM_CORE_CT_END`.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_items_from_utf8(char const *text,
                                km_core_context_item **out_ptr);

#endif

/*
```
### `km_core_context_items_to_utf16`
##### Description:
Convert a context item array into a UTF-16 encoded string placing it into
the supplied buffer of specified size, and return the number of code units
actually used in the conversion. If null is passed as the buffer the
number of codeunits required is returned. Any markers in the context will
not be included in the output buffer.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large enough.
  `buf_size` will contain the space required. The contents of the buffer are
  undefined.
##### Parameters:
- __context_items__: A pointer to the start of an array `km_core_context_item`.
    Must be terminated with a type of `KM_CORE_CT_END`.
- __buf__: A pointer to the buffer to place the UTF-16 string into.
    May be null to request size calculation.
- __buf_size__: a pointer to the result variable:
    The size of the supplied buffer in codeunits if `buf` is given.
    On return will be the size required if `buf` is null.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_items_to_utf16(km_core_context_item const *item,
                              km_core_cp *buf,
                              size_t *buf_size);

#endif

/*
```
### `km_core_context_items_to_utf8`
##### Description:
Convert a context item array into a UTF-8 encoded string placing it into
the supplied buffer of specified size, and return the number of code units
actually used in the conversion. If null is passed as the buffer the
number of codeunits required is returned. Any markers in the context will
not be included in the output buffer.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large enough.
  `buf_size` will contain the space required. The contents of the buffer are
  undefined.
##### Parameters:
- __context_items__: A pointer to the start of an array `km_core_context_item`.
    Must be terminated with a type of `KM_CORE_CT_END`.
- __buf__: A pointer to the buffer to place the UTF-8 string into.
    May be null to request size calculation.
- __buf_size__: a pointer to the result variable:
    The size of the supplied buffer in codeunits if `buf` is given.
    On return will be the size required if `buf` is null.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_items_to_utf8(km_core_context_item const *item,
                              char *buf,
                              size_t *buf_size);

#endif

/*
```
### `km_core_context_items_to_utf32`
##### Description:
Convert a context item array into a UTF-32 encoded string placing it into
the supplied buffer of specified size, and return the number of codepoints
actually used in the conversion. If null is passed as the buffer the
number of codepoints required is returned. Any markers in the context will
not be included in the output buffer.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_INSUFFICENT_BUFFER`: If the buffer is not large enough.
  `buf_size` will contain the space required. The contents of the buffer are
  undefined.
##### Parameters:
- __context_items__: A pointer to the start of an array `km_core_context_item`.
    Must be terminated with a type of `KM_CORE_CT_END`.
- __buf__: A pointer to the buffer to place the UTF-32 string into.
    May be null to request size calculation.
- __buf_size__: a pointer to the result variable:
    The size of the supplied buffer in codepoints if `buf` is given.
    On return will be the size required if `buf` is null.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_items_to_utf32(km_core_context_item const *item,
                              km_core_usv *buf,
                              size_t *buf_size);

#endif

/*
```
### `km_core_context_set`
##### Description:
Replace the contents of the current context with a new sequence of
`km_core_context_item` entries.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be allocated to
  grow the context buffer internally.
##### Parameters:
- __context__: A pointer to an opaque context object
- __context_items__: A pointer to the start of the `km_core_context_item`
    array containing the new context. It must be terminated with an item
    of type `KM_CORE_CT_END`.

```c
*/
KMN_API
km_core_status
km_core_context_set(km_core_context *context,
                   km_core_context_item const *context_items);

/*
```
### `km_core_context_get`
##### Description:
Copies all items in the context into a new array and returns the new array.
This must be disposed of by caller using `km_core_context_items_dispose`.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be allocated for the
  output buffer.
##### Parameters:
- __context_items__: A pointer to the start of an array `km_core_context_item`.
- __out__: a pointer to the result variable:
    A pointer to the start of the `km_core_context_item` array containing a
    copy of the context. Terminated with a type of `KM_CORE_CT_END`. Must be
    disposed of with `km_core_context_items_dispose`.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_get(km_core_context const *context_items,
                   km_core_context_item **out);

#endif

/*
```
### `km_core_context_clear`
##### Description:
Removes all context_items from the internal array. If `context` is
null, has no effect.
##### Parameters:
- __context__: A pointer to an opaque context object

```c
*/
KMN_API
void
km_core_context_clear(km_core_context *);

/*
```
### `km_core_context_length`
##### Description:
Return the number of items in the context.
##### Return:
The number of items in the context, and will return 0 if passed a null `context`
pointer.
##### Parameters:
- __context__: A pointer to an opaque context object

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
size_t
km_core_context_length(km_core_context *);

#endif

/*
```
### `km_core_context_append`
##### Description:
Add more items to the end (insertion point) of the context. If these exceed the
maximum context length the same number of items will be dropped from the
beginning of the context.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: In the event not enough memory can be allocated to
  grow the context buffer internally.
##### Parameters:
- __context__: A pointer to an opaque context object.
- __context_items__: A pointer to the start of the `KM_CORE_CT_END` terminated
    array of `km_core_context_item` to append.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_append(km_core_context *context,
                      km_core_context_item const *context_items);

#endif

/*
```
### `km_core_context_shrink`
##### Description:
Remove a specified number of items from the end of the context, optionally
add up to the same number of the supplied items to the front of the context.
##### Return status:
- `KM_CORE_STATUS_OK`: On success.
- `KM_CORE_STATUS_INVALID_ARGUMENT`: If non-optional parameters are null.
- `KM_CORE_STATUS_NO_MEM`: in the event it cannot allocated enough memory to grow
  the context internally.
##### Parameters:
- __context__: A pointer to an opaque context object.
- __num__: The number of items to remove from the end of context.
- __context_items__: Pointer to the start of the `KM_CORE_CT_END` terminated
    array of `km_core_context_item` to add to the front. Up to `num` items will
    be prepended. This may be null if not required.

```c
*/

#ifdef _KM_CORE_ACCESS_PRIVATE_CONTEXT_API

KMN_API
km_core_status
km_core_context_shrink(km_core_context *context,
                      size_t num,
                      km_core_context_item const *prefix);

#endif

/*
```
### `km_core_context_item_list_size`
##### Description:
Return the length of a terminated `km_core_context_item` array.
##### Return:
The number of items in the list, not including terminating item,
or 0 if `context_items` is null.
##### Parameters:
- __context_items__: A pointer to a `KM_CORE_CT_END` terminated array of
    `km_core_context_item` values.

```c
*/
KMN_API
size_t
km_core_context_item_list_size(km_core_context_item const *context_items);

#if defined(__cplusplus)
} // extern "C"
#endif
