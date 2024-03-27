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
// Context APIs are now available only to the keyboard debugger, IMX, and Core unit
// tests (17.0)
// ----------------------------------------------------------------------------------

/*
---
filename: context.md
title: Internal Context - Keyman Core API
---

The context is the text prior to the insertion point (caret, cursor). The
context is constructed by the Platform layer, typically by interrogating the
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

Core maintains and caches the context. Engine can update the context with
[`km_core_state_context_set_if_needed`] and [`km_core_state_context_clear`].
These two functions are available in keyman_core_api.h.

The Keyboard Debugger in Keyman Developer, and IMX in Keyman for Windows, make
use of the context functionality in this header, but these functions should not
be used in other places.

-------------------------------------------------------------------------------

# km\_core\_context\_type enum  {#km_core_context_type}

## Description

[`km_core_context_item.type`](#km_core_context_item) values which identify which
data value (if any) the context item carries.

## Specification

```c */
enum km_core_context_type {
  KM_CORE_CT_END,
  KM_CORE_CT_CHAR,
  KM_CORE_CT_MARKER
};
/*
```
## Values {#km_core_ct_type_values}

`KM_CORE_CT_END`
: A [`km_core_context_item`](#km_core_context_item) of this type marks the end of
an array of context items.

`KM_CORE_CT_CHAR`
: The context item contains a Unicode Scalar Value which must be accessed
through the [`km_core_context_item.character`](#km_core_context_item) union
member.

`KM_CORE_CT_MARKER`
: The context item contains a positional marker which must be accessed through
the [`km_core_context_item.marker`](#km_core_context_item) union member.

-------------------------------------------------------------------------------

# km\_core\_context\_item struct {#km_core_context_item}

## Description

A tagged union representing an element of context which can be either a Unicode
character or a positional marker.

## Specification
```c */
typedef struct {
  uint8_t   type;
  uint8_t   _reserved[3];
  union {
    km_core_usv  character;
    uint32_t    marker;
  };
} km_core_context_item;
/*
```
## Members

`type`
:Identifies the union member to access. A value of enum
[`km_core_context_type`](#km_core_context_type) values.

`_reserved`
:Space reserved for alignment purposes and possible future use.

`character`
:A Unicode Scalar Value.

`marker`
:A marker value, only meaningful to an engine.

-------------------------------------------------------------------------------

# KM\_CORE\_CONTEXT\_ITEM\_END macro

## Description

Convenience macro to declare a terminating entry in a
[`km_core_context_item`](#km_core_context_item) item array.

## Specification
```c */
#define KM_CORE_CONTEXT_ITEM_END {KM_CORE_CT_END, {0,}, {0,}}

/*
```
-------------------------------------------------------------------------------

# km_core_state_get_intermediate_context()

## Description

Get access to the state object's keyboard processor's intermediate context. This context
is used during an IMX callback, part way through processing a keystroke.

## Specification
```c */
KMN_API
km_core_status
km_core_state_get_intermediate_context(km_core_state *state, km_core_context_item ** context_items);

/*
```
## Parameters

`state`
: A pointer to the opaque state object to be queried.

## Returns

A pointer to an context item array. Must be disposed of by a call
to `km_core_context_items_dispose`.

-------------------------------------------------------------------------------

# km_core_context_items_dispose()

## Description

Free the allocated memory belonging to a `km_core_context_item` array previously
returned by `km_core_state_get_intermediate_context` (internally, also
`context_items_from_utf16` and `km_core_context_get`)

## Specification

```c
*/
KMN_API
void
km_core_context_items_dispose(km_core_context_item *context_items);

/*
```
## Parameters

`context_items`
: A pointer to the start of the `km_core_context_item` array
  to be disposed of.

-------------------------------------------------------------------------------

# km_core_state_context()

## Description

Get access to the state object's cached context.

## Specification
```c */
KMN_API
km_core_context *
km_core_state_context(km_core_state const *state);

/*
## Parameters

`state`
: A pointer to the opaque state object to be queried.

## Returns

A pointer to an opaque context object. This pointer is valid for the
lifetime of the state object. If null is passed in, then null is
returned.

-------------------------------------------------------------------------------

# km_core_state_app_context()

## Description

Get access to the state object's application context.

## Specification
```c */
KMN_API
km_core_context *
km_core_state_app_context(km_core_state const *state);

/*
```
## Parameters

`state`
: A pointer to the opaque state object to be queried.

## Returns

A pointer to an opaque context object. This pointer is valid for the
lifetime of the state object. If null is passed in, then null is
returned.

-------------------------------------------------------------------------------

# km_core_context_set()

## Description

Replace the contents of the current context with a new sequence of
`km_core_context_item` entries.

## Specification
```c */
KMN_API
km_core_status
km_core_context_set(km_core_context *context,
                   km_core_context_item const *context_items);

/*
```
## Parameters

`context`
: A pointer to an opaque context object

`context_items`
: A pointer to the start of the `km_core_context_item`
  array containing the new context. It must be terminated with an item
  of type `KM_CORE_CT_END`.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

`KM_CORE_STATUS_NO_MEM`
: In the event not enough memory can be allocated to
  grow the context buffer internally.

-------------------------------------------------------------------------------

# km_core_context_clear()

## Description

Removes all context_items from the internal array. If `context` is
null, has no effect.

## Specification
```c */
KMN_API
void
km_core_context_clear(km_core_context *);

/*
```
## Parameters

`context`
: A pointer to an opaque context object

-------------------------------------------------------------------------------

# km_core_context_item_list_size()

## Description

Return the length of a terminated `km_core_context_item` array.

## Specification
```c */
KMN_API
size_t
km_core_context_item_list_size(km_core_context_item const *context_items);

/*
```
## Parameters

`context_items`
: A pointer to a `KM_CORE_CT_END` terminated array of
  `km_core_context_item` values.

## Returns

The number of items in the list, not including terminating item,
or 0 if `context_items` is null.

-------------------------------------------------------------------------------

# km_core_context_get()

## Description

Copies all items in the context into a new array and returns the new array.
This must be disposed of by caller using `km_core_context_items_dispose`.

## Specification
```c */
KMN_API
km_core_status
km_core_context_get(km_core_context const *context,
                   km_core_context_item **out);

/*
```
## Parameters

`context`
: A pointer to an opaque context object

`out`
: pointer to the result variable: A pointer to the start
  of the `km_core_context_item` array containing a copy of
  the context. Terminated with a type of `KM_CORE_CT_END`.
  Must be disposed of with `km_core_context_items_dispose`.

## Returns

`KM_CORE_STATUS_OK`
: On success.

`KM_CORE_STATUS_INVALID_ARGUMENT`
: If non-optional parameters are null.

`KM_CORE_STATUS_NO_MEM`
: In the event not enough memory can be allocated for the
  output buffer.

-------------------------------------------------------------------------------

# km_core_context_length()

## Description

Return the number of items in the context.

## Specification
```c */
KMN_API
size_t
km_core_context_length(km_core_context *);

/*
```
## Parameters

`context`
: A pointer to an opaque context object

## Returns

The number of items in the context, and will return 0 if
passed a null `context` pointer.

$EOF
-------------------------------------------------------------------------------
*/

#if defined(__cplusplus)
} // extern "C"
#endif
