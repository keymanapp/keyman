/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Keyboard Processor API - Context Interfaces
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
returned by `km_core_context_items_from_utf16` or `context_get`
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
