/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Tim Eves (TSE) on 2018-10-02
 *
 * Cross platform API C/C++ declarations for libkeymancore
 */
#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <keyman/keyman_core_api_bits.h>
#include <keyman/keyman_core_api_vkeys.h>
#include <keyman/keyman_core_api_version.h>

#if defined(__cplusplus)
extern "C"
{
#endif

/**
---
filename: index.md
title: Keyman Core API
---

## Overview
Keyman Core is the component of Keyman Engine that implements keyboarding rules.
It is platform independent and allows support for different keyboard formats to
be implemented within Keyman Engine. Eventually, Keyman Core will be used in
Keyman on all platforms. As of writing, Keyman for Linux, Keyman for Windows,
Keyman for macOS, and Keyman Developer use Keyman Core.

This is an internal API intended for use only within Keyman Engine.

## Reference

* [Background](background)
* [Changes from earlier versions](changes)
* [Keyboards](keyboards)
* [Options](options)
* [Processor](processor)
* [State and Actions](state)
* [Key Handling](keyhandling)
* [Building Keyman Core](building)

## Requirements
1. Cross platform.
2. Cross language.
3. Facilitate stateless operation of the Engine.
4. Keyboard format agnostic -- support both KMN and future LDML based keyboards.
5. Support querying Engine attributes.
6. Support querying Keyboard attributes.
7. Idempotent

## Design decisions in support of requirements
- Use C or C99 types and calling convention for the interface, it has the
  broadest language FFI support. [1,2]
- Have client (Platform layer) code load keyboards, manage & pass state. [3,4,7]
- Provide query calls to return static attributes data for keyboards and
  engine [5,6]
- Provide get/set calls for client accessible keyboard state information [3,4]


## Glossary
- __Platform layer:__
The code that consumes the Keyman Core API, and provides the
operating system-specific handling of keystroke events and integration with
applications.
- __Client Application:__
The application that has the focus and receives text events from the Platform
layer.
- __Context:__ Text preceding the insertion point
- __Marker:__ Positional state that can be placed in the Context.
- __Keyboard:__ A set of rules for execution by an Engine
- __Option:__ A variable in a dynamic or static key value store.
- __Processor:__
The component that implements this API and can parse and execute a particular
keyboard.
- __State:__ An object that holds internal state of the Processor for a given
insertion point
- __Action:__
A directive output by the processor detailing how the Platform layer should
transform the Client Application's text buffer. There may be several items
produced by a single keyboard event.
- __Keyboard Event:__
A virtual key event and modifier map received from the Platform layer to be
processed with the state object for this Client application.
- __Virtual Key:__
A code based on the US English layout, with values matching the Windows
virtual key codes. See `keyman_core_api_vkeys.h` for definitions.
- __Modifier Key:__
The set of Control, Shift, Alt, Caps Lock keys. On some platforms these may
have other names (e.g. Alt is called Option on macOS); other platform-specific
modifiers such as Windows key are excluded from this set. Some modifiers are
transient, such as Control, and others have long-lasting state, such as
Caps Lock.

- __See more in__   [Keyman Glossary](https://github.com/keymanapp/keyman/wiki/Keyman-glossary)

---
filename: building.md
title: How to build Keyman Core
---

## Prerequisites

### To build

*   Python 3
*   Meson build system.
*   C++17 or later compiler.

### Optional

*   [kmc](https://keyman.com/developer/download) (for testing)

## Installing Python3

### Linux

You will be able to install a python3 package in any reputable recent version of
linux using its package manager if it's not already installed.

### macOS

You can get the official installer from the official Python site:
[https://www.python.org/downloads/mac-osx](https://www.python.org/downloads/mac-osx/)

### Windows

You can get the official installer from the official Python site:
[https://www.python.org/downloads/windows](https://www.python.org/downloads/windows/)

## Installing Meson

Ensure you have Python3 correctly installed and can run the command `pip3`.

    $> python3 -m pip install meson

## Building

In your source directory do the following:

    $> cd core
    $> ./build.sh configure build test

## Note on kmc

kmc is node.js-based the command-line compiler from Keyman Developer, available
from [keyman.com](https://keyman.com/developer/) or on npm at
[@keymanapp/kmc](https://npmjs.com/package/@keymanapp/kmc).

### Windows

The search path can be edited through System settings / Advanced system settings
/ Environment Variables / User environment variables.

If you have Keyman Developer installed, kmc should already be on your path.
Otherwise, add the path where you extracted the kmcomp archive.

### Linux & MacOS

Install kmc from the NPM package.

---
filename: keyhandling.md
title: Keyhandling
---

# Key Handling

For each key press the processor will called twice, for the key down event
as well as for the key up event. Depending on the type of key pressed the
processor might handle the key itself or pass it on to the application.
The value of `emit_keystroke` in `km_core_actions` struct tells if the
processor handled the key (`emit_keystroke=0`) or not (`emit_keystroke=1`).
It is important that the same value gets set for both key down and key up
events, otherwise the application might miss some events which then looks
to the user like keys are stuck.

Usually the processor won't handle any frame keys and let the application
deal with it. This allows shortcut keys like <kbd>Ctrl</kbd>+<kbd>C</kbd>
to work. The only exception is the <kbd>Backspace</kbd> key which is
handled internally if enough context is available. Regular keys will be
handled by the processor.

Note that there is a difference between CLDR/LDML and KMN keyboards if
there are no rules/transforms defined for a key: KMN keyboards will
output the cap value of the key (e.g. pressing <kbd>a</kbd> will output
`a` if no rule is defined), whereas CLDR/LDML keyboards will suppress
any output for that key.

The following table lists the state of `km_core_actions.emit_keystroke`
on return of `km_core_process_event` when the following type of key is
pressed:

|Type                         |   KeyDown     |  KeyUp   |
|-----------------------------|---------------|----------|
|Key with rule                |    FALSE      |  FALSE   |
|Key w/o rule                 |    FALSE      |  FALSE   |
|Framekeys:                   |               |          |
|<kbd>Enter</kbd>             |    TRUE       |  TRUE    |
|<kbd>Backspace</kbd>¹        |    FALSE      |  FALSE   |
|<kbd>Backspace</kbd>²        |    TRUE       |  TRUE    |
|<kbd>Ctrl</kbd>+Key          |    TRUE       |  TRUE    |
|Modifier key <kbd>Shift</kbd>|    TRUE       |  TRUE    |
|Modifier key <kbd>Ctrl</kbd> |    TRUE       |  TRUE    |
|Modifier key <kbd>LAlt</kbd> |    TRUE       |  TRUE    |

1: context available<br/>
2: without or with empty context

---
filename: changes.md
title: Changes - Keyman Core API
---

## Changes between 18.0 and 19.0

* Removed deprecated `km_core_keyboard_attrs.folder_path`
* The JSON introspection APIs (which were not fully implemented),
  `km_core_state_options_to_json` and `km_core_state_to_json`, have been
  removed.

## Changes between 16.0 and 17.0

* The namespace identifier has changed from `km_kbp_` to `km_core_`.
* Most context APIs are now private, and [km_core_state_context_set_if_needed] is the
  primary context function. Private APIs are available in
  `keyman_core_api_context.h`.
* The action queue APIs are now private and deprecated. Instead, use
  [km_core_state_get_actions]. Private APIs are available in
  `keyman_core_api_actions.h`.
* Debug APIs are available in `keyman_core_api_debug.h`.

-------------------------------------------------------------------------------

---
filename: background.md
title: Background - Keyman Core API
---

## Namespace
All calls, types and enums are prefixed with the namespace identifier `km_core_`

## API idioms

### Error Handling

Error handling and success failure notification are communicated through a
general mechanism similar to COM’s `HRESULT` scheme (unlike COM, any non-zero
value is an error). Any functions that can fail will always return a status
value and all results are returned via outparams passed to the function.

### Passing variable length data out

Almost all calls marshalling variable length aggregate data in or out of an API
object take the form:

```c
km_core_status fn_name(object_ref, buffer_ptr, size_ptr)
```
where the `buffer_ptr` is nullable and all other arguments are required (will
result in an [`KM_CORE_STATUS_INVALID_ARGUMENT`](#km_core_status_codes)
status being returned if nulled). When `buffer_ptr` is `nullptr` or `0` the
function will place the size of the required buffer in the variable pointed to
by `size_ptr`.

### Resource management

Calls which result in the allocation of resources, regardless of resulting
ownership, are of the form:
```c
km_core_status fn_name(object_ref, handle_out_ptr)
```
where `handle_out_ptr` is a valid pointer to a caller allocated variable to hold
the resulting resource handle. This is often a reference to a created object.
Unless stated all arguments are required (will result in an
[`KM_CORE_STATUS_INVALID_ARGUMENT`](#km_core_status_codes) status being
returned if nulled).

All dispose calls are designed to accept `nullptr` or `0` as a valid value and
will do nothing in that event.

### Fixed size attribute access

For accessors to fixed size attributes of an object these will take the form:
```c
attr_value fn_name(object_ref)
```
`object_ref` is required to be valid and will result in a nonsense value being returned if `nullptr` or `0`.

### Versioning scheme

This follows the libtool interface versioning scheme of `current.age.revision`:

`current`

The most recent interface number that the engine implements.

`age`

How many interface numbers back from current the library implements. E.g. 5.2.0
would mean the library provides interface versions 3-5 and 5.0.0 would mean just
interface version 5 and nothing older.

`revision`

The implementation version of the current interface. This represents
improvements to the code that don't change the intended behaviour of the
interface such as bug fixes and optimisations.

For Linux and other OS which support this scheme the dynamic linker will
automatically choose the most updated version if more than one implementation is
available. For Windows or dynamic loaded shared objects on Linux you can use the
[km_core_get_engine_attrs] call and [Library version
macros](#lib-version-macros) to check the loaded DLL supplies the correct
interface.

-------------------------------------------------------------------------------
*/

// Note: `km_core_cu` and `km_core_usv` are defined in common/km_types.h, as the
//       underlying type may vary by platform, and the types are shared across
//       multiple projects.

/**
 * @name km_core_cu type
 *
 * Represents a UTF16 codepoint, most strings are passed as UTF16.
 *
 * ```c
 * typedef uint16_t/char16_t km_core_cu;
 * ```
 */

/**
 * @name km_core_usv type
 *
 * An integral type capable of holding a single Unicode Scalar Value, a decoded
 * UTF codepoint.
 *
 * ```c
 * typedef uint32_t/char32_t km_core_usv;
 * ```
 */

//-------------------------------------------------------------------------------

/**
 * @name km_core_virtual_key type
 *
 * An integral type capable of holding a platform specific virtual key code.
 */
typedef uint16_t    km_core_virtual_key;

/**
 * @name km_core_status type
 *
 * An integral 32 bit wide type capable of holding any valid status code as
 * defined by the `enum` [km_core_status_codes].
 */
typedef uint32_t    km_core_status;

//
// Opaque types for representing resources provided or created by the keyboard
// processor implementation.
//

/**
 * @name km_core_keyboard struct
 *
 * Represents a keyboard loaded from disk, that can be executed by the keyboard
 * processor to consume events, update state associated with an insertion point
 * and produce action items. A keyboard object may be referenced by any number
 * of state objects but must be disposed of after all state objects referencing
 * it have first been disposed of.
 */
typedef struct km_core_keyboard    km_core_keyboard;

/**
 * @name km_core_state struct
 *
 * Represents all state associated with an insertion point using a keyboard.
 * This tracks context, and current action items resulting from a processed
 * keyboard event. There can be many state objects using the same keyboard. A
 * state object may not live longer than the keyboard it manages state for.
 */
typedef struct km_core_state       km_core_state;

/**
 * @name km_core_options struct
 *
 * Represents a set of option items for environmental state and keyboard state.
 */
typedef struct km_core_options     km_core_options;

//
// Forward declarations
//
typedef struct km_core_option_item  km_core_option_item;

/**
 * @name km_core_keyboard_imx_platform callback function
 *
 * Callback function used to to access Input Method eXtension library functions
 * from Keyman Core
 */
typedef uint8_t (*km_core_keyboard_imx_platform)(km_core_state*, uint32_t, void*);

/**
 * @name km_core_status_codes enum
 *
 * An error code mechanism similar to COM's `HRESULT` scheme (unlike COM, any
 * non-zero value is an error).
 */
enum km_core_status_codes {
  // keep in sync with web/src/engine/src/core-adapter/KM_Core.ts
  // (see https://github.com/emscripten-core/emscripten/issues/18585)

  /** Success code. Call completed as documented. */
  KM_CORE_STATUS_OK = 0,

  /** The call failed to allocate memory during its execution, causing it to fail. */
  KM_CORE_STATUS_NO_MEM = 1,

  /** The call performed an I/O operation which failed, causing it to fail. */
  KM_CORE_STATUS_IO_ERROR = 2,

  /** The call detected one of its parameters was invalid or unsafe. */
  KM_CORE_STATUS_INVALID_ARGUMENT = 3,

  /** The provided key or index into a collection object was not present. */
  KM_CORE_STATUS_KEY_ERROR = 4,

  /** The provided buffer did not contain enough space to fully encode or copy
   *  the result of this call. */
  KM_CORE_STATUS_INSUFFICENT_BUFFER = 5,

  /** A malformed or partial UTF sequence prevented complete decoding of a
   *  unicode string. */
  KM_CORE_STATUS_INVALID_UTF = 6,

  /** An attempt to decode a keyboard file failed. */
  KM_CORE_STATUS_INVALID_KEYBOARD = 7,

  KM_CORE_STATUS_NOT_IMPLEMENTED = 8,

  /** This allows encapsulating a platform error code: the remaining 31 low bits
   *  are the error code returned by the OS for cases where the failure mode is
   *  platform specific. For HRESULT codes this only permits failure codes to be
   *  passed and not success codes. */
  KM_CORE_STATUS_OS_ERROR = 0x80000000
};

/**
 * @name km_core_attr struct
 *
 * A structure describing information about Keyman Core implementing this API.
 */
typedef struct {
  /** Maximum context size supported by processor in code points. */
  size_t      max_context;

  /** Current API number supported. */
  uint16_t    current;

  /** Implementation number of current API. */
  uint16_t    revision;

  /** current - age == Oldest API number supported. */
  uint16_t    age;

  /** A bit field of [km_core_tech_value] values, specifiying which Keyboard
   *  technologies the engine supports. */
  uint16_t    technology;

  /** A UTF-8 encoded string identifying the implementer of the processor. */
  char const *vendor;
} km_core_attr;

/**
 * @name km_core_tech_value enum
 *
 * Values for a bit field indicating which keyboarding technologies a keyboard
 * processor supports.
 */
enum km_core_tech_value {
  /** The keyboard processor implementation does not disclose which technologies
   *  it implements. */
  KM_CORE_TECH_UNSPECIFIED = 0,

  /** The keyboard processor implements a simple en-US keyboard for the purposes
   *  of testing the API. */
  KM_CORE_TECH_MOCK        = 1 << 0,

  /** The keyboard processor implements a Keyman KMX compatible engine. */
  KM_CORE_TECH_KMX         = 1 << 1,

  /** The keyboard processor implements a LDML capable processing engine. */
  KM_CORE_TECH_LDML        = 1 << 2
};


/**
 * @name km_core_get_engine_attrs function
 *
 * Get access processors attributes describing version and technology
 * implemented.
 *
 * @param    state  An opaque pointer to a [km_core_state].
 *
 * @returns  A pointer to a [km_core_attr] structure. Do not modify the contents
 *           of this structure.
 */
KMN_API
km_core_attr const *
km_core_get_engine_attrs(
  km_core_state const *state
);

/**
 * @name km_core_bool enum
 *
 * Defines a boolean state.
 */
typedef enum { KM_CORE_FALSE = 0, KM_CORE_TRUE = 1 } km_core_bool;

//-------------------------------------------------------------------------------

/**
---
filename: state.md
title: State and Actions - Keyman Core API
---

A State object maintains all per keyboard related state including context
and dynamic options ("option stores" in kmn format).

When a keystroke is processed by Keyman Core, Core provides back a set of actions
for the Platform layer to emit to the Client application. These actions are
owned by the state object.

-------------------------------------------------------------------------------
*/

/**
 * @name km_core_caps_state enum
 *
 * Describes a change to the hardware caps lock indicator state requested by
 * Keyman Core to the Platform layer.
 */
typedef enum {
  /** Caps lock state has not changed in this event. */
  KM_CORE_CAPS_UNCHANGED = -1,

  /** As a result of processing this event, the Platform layer should switch off
   *  Caps Lock on the hardware keyboard. */
  KM_CORE_CAPS_OFF = 0,

  /** As a result of processing this event, the Platform layer should switch on
   *  Caps Lock on the hardware keyboard.*/
  KM_CORE_CAPS_ON = 1
} km_core_caps_state;

/**
 * @name km_core_actions struct
 *
 * This structure provides the results of processing a key event to the Platform
 * layer and should be processed by the Platform layer to issue commands to the
 * os text services framework to transform the text store in the Client
 * Application, among other actions.
 *
 * This API replaces the Action items APIs, which are now deprecated and will be
 * removed in the future.
 */
typedef struct {
  /** Number of codepoints (not codeunits!) to delete from app context. */
  unsigned int code_points_to_delete;

  /** Null-term string of characters to insert into document. */
  const km_core_usv* output;

  /** List of options to persist, terminated with `KM_CORE_OPTIONS_END`. */
  km_core_option_item * persist_options;

  /** Issue a beep, 0 = no, 1 = yes. */
  km_core_bool do_alert;

  /** Emit the (unmodified) input keystroke to the application, 0 = no, 1 = yes.
   *  On most platforms this signals whether the processor handled the event (0)
   *  or not (1). See also [key handling](keyhandling). */
  km_core_bool emit_keystroke;

  /** -1=unchanged, 0=off, 1=on */
  km_core_caps_state new_caps_lock_state;

  /** Reference copy of actual UTF32 codepoints deleted from end of context
   *  (closest to caret) exactly `code_points_to_delete` in length (plus null
   *  terminator). Used to determine encoding conversion differences when
   *  deleting; only set when using [km_core_state_get_actions], otherwise
   *  `nullptr`. */
  const km_core_usv* deleted_context;
} km_core_actions;

/**
 * @name km_core_state_get_actions function
 *
 * Returns a pointer to an actions object which details all the actions that the
 * Platform layer must take after a keystroke. The `code_points_to_delete`
 * action must be performed before the `output` action, but the other actions
 * may be performed in any order.
 *
 * @param    state   An opaque pointer to a state object.
 * @returns  A pointer to a [km_core_actions] object. This data becomes invalid
 *           when the state object is destroyed, or after a call to
 *           [km_core_process_event]. Do not modify the contents of this data.
 */
KMN_API
km_core_actions const *
km_core_state_get_actions(
  km_core_state const *state
);

/**
 * @name km_core_context_status enum
 *
 * Return values for [km_core_state_context_set_if_needed].
 */
typedef enum {
  /** Cached context change was not needed. */
  KM_CORE_CONTEXT_STATUS_UNCHANGED = 0,

  /** Cached context was set to application context. */
  KM_CORE_CONTEXT_STATUS_UPDATED = 1,

  /** Application context was invalid, perhaps had unpaired surrogates, and so
   *  cached context was cleared instead. */
  KM_CORE_CONTEXT_STATUS_CLEARED = 2,

  /** Internal error. */
  KM_CORE_CONTEXT_STATUS_ERROR = 3,

  /** One or more parameters was null. */
  KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT = 4,
} km_core_context_status;

/**
 * @name km_core_state_context_set_if_needed function
 *
 * Sets the internal cached context for the state object, to the passed-in
 * application context string, if it differs from the codepoints in the
 * cached context. For the purposes of comparison, (1) cached markers are
 * ignored, (2) if the cached context is shorter than the application
 * context, it is considered identical, but (3) if the cached context is
 * longer, then it is considered different.
 *
 * If a difference is found, then the cached context will be set to the
 * application context, and thus any cached markers will be cleared.
 *
 * [km_core_state_context_set_if_needed] and [km_core_state_context_clear]
 * will replace most uses of the existing Core context APIs.
 *
 * @param   state                An opaque pointer to a state object.
 * @param   application_context  A pointer to an null-terminated array of utf16
 *                               encoded data representing the current context
 *                               from the application.
 * @returns A value from the [km_core_context_status] enum.
 */
KMN_API
km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cu const *application_context
);

/**
 *
 * @name km_core_state_context_clear function
 *
 * Clears the internal cached context for the state. This is the same as
 * `km_core_context_clear(km_core_state_context(&state))`.
 *
 * [km_core_state_context_set_if_needed] and [km_core_state_context_clear]
 * will replace most uses of the existing Core context APIs.
 *
 * @param   state                An opaque pointer to a state object.
 * @returns One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If any parameters are null.
 */
KMN_API
km_core_status
km_core_state_context_clear(
  km_core_state *state
);

//-------------------------------------------------------------------------------
/**
---
filename: options.md
title: Options - Keyman Core API
---

A state’s default options are set from the keyboard at creation time and the
environment. The Platform layer is then expected to apply any persisted
options it is maintaining.  Options are passed into and out of API functions as
simple C arrays of [km_core_option_item] terminated with a `KM_CORE_OPTIONS_END`
sentinel value. A state's options are exposed and manipulatable via the
[km_core_options] API. All option values are of type C string.

During processing when the Platform layer finds a PERSIST action type it should
store the updated option in the appropriate place, based on its scope.
For RESET the processor will apply the pristine value from the original scope,
the Platform layer should update that only if it manages a previously persisted
value.

-------------------------------------------------------------------------------
*/

/**
 * @name km_core_option_scope enum
 */
enum km_core_option_scope {
  /** An unknown option type. Reserved. */
  KM_CORE_OPT_UNKNOWN      = 0,

  /** An option that is defined for the currently active keyboard; not all
   *  processors support this type of option. These options are specific to the
   *  active keyboard. */
  KM_CORE_OPT_KEYBOARD     = 1,

  /** Properties of the current environment, often but not necessarily always
   *  read-only. */
  KM_CORE_OPT_ENVIRONMENT  = 2,
  KM_CORE_OPT_MAX_SCOPES
};

/**
 * @name km_core_option_item struct
 *
 * Defines a single option to be passed into the Keyman Core from the Platform
 * layer.
 */
struct km_core_option_item {
  /** Null-terminated string key for the option */
  km_core_cu const *   key;

  /** Null-terminated string value for the option */
  km_core_cu const *   value;

  /** Scope which an option belongs to, from [km_core_option_scope]. */
  uint8_t             scope;
};

#define KM_CORE_OPTIONS_END { 0, 0, 0 }

/**
 * @name km_core_options_list_size function
 *
 * Return the length of a terminated [km_core_option_item] array (options
 * list).
 *
 * @param opts   A pointer to a `KM_CORE_OPTIONS_END` terminated array of
 *               [km_core_option_item] values.
 * @returns      The number of items in the list, not including terminating
 *               item, or 0 if `opts` is null.
 */
KMN_API
size_t
km_core_options_list_size(km_core_option_item const *opts);

/**
 * @name km_core_state_option_lookup function
 *
 * Lookup an option based on its key, in an options list.
 *
 * @param   state   An opaque pointer to a state object.
 * @param   scope   Which key-value store to interrogate.
 * @param   key     A UTF-16 string that matches the key in the target
                    [km_core_option_item].
 * @param   value   A pointer to the result variable: A pointer to a UTF-16
 *                  string value owned by the state or keyboard object at the
 *                  time of the call. This pointer is only valid *until* the next
 *                  call to any function on this API and should be used
 *                  immediately.
 *
 * @returns One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If non-optional parameters are null, or if the scope is invalid.
 *
 *   `KM_CORE_STATUS_KEY_ERROR`
 *   : The key cannot be found.
 */
KMN_API
km_core_status
km_core_state_option_lookup(
  km_core_state const *state,
  uint8_t scope,
  km_core_cu const *key,
  km_core_cu const **value
);

/**
 * @name km_core_state_options_update function
 *
 * Adds or updates one or more options from a list of [km_core_option_item]s.
 *
 * @param state     An opaque pointer to a state object.
 * @param new_opts  An array of [km_core_option_item] objects to update or add.
 *                  Must be terminated with `KM_CORE_OPTIONS_END`.
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If non-optional parameters are null.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event an internal memory allocation fails.
 *
 *   `KM_CORE_STATUS_KEY_ERROR`
 *   : The key cannot be found.
 */
KMN_API
km_core_status
km_core_state_options_update(
  km_core_state *state,
  km_core_option_item const *new_opts
);

//-------------------------------------------------------------------------------
/**
---
filename: keyboards.md
title: Keyboards - Keyman Core API
---

A keyboard is a set of rules and transforms in a Processor specific format for
transforming key events into action items. The keyboard is parsed and loaded by
the processsor and made available in an immutable fashion for use with any number
of state objects.

-------------------------------------------------------------------------------
*/

/**
 * @name km_core_keyboard_attrs struct
 *
 * Provides read-only information about a keyboard.
 */
typedef struct {
  /** Processor specific version string. */
  km_core_cu const * version_string;

  /** Keyman keyboard ID string. */
  km_core_cu const * id;

  /** Set of default values for any options included in the keyboard. */
  km_core_option_item const * default_options;
} km_core_keyboard_attrs;


/**
 * @name km_core_keyboard_key struct
 *
 * Describes a single key and modifier combination that a keyboard handles, for
 * use by the Platform layer. This is used when the Platform layer must know in
 * advance which keys are used by a given keyboard.
 */
typedef struct {
  /** A virtual key. */
  km_core_virtual_key key;

  /** A [km_core_modifier_state] bitmask. */
  uint32_t modifier_flag;
} km_core_keyboard_key;

#define KM_CORE_KEYBOARD_KEY_LIST_END { 0, 0 }

/**
 * @name km_core_keyboard_imx struct
 *
 * Describes a single Input Method eXtension library and entry point.
 */
typedef struct {
  /** The fully-qualified path and filename of the dynamically loaded library
   *  file. */
  km_core_cu const * library_name;

  /** The entry point for the IMX. */
  km_core_cu const * function_name;

  /** unique identifier used to call this function */
  uint32_t imx_id;
} km_core_keyboard_imx;

#define KM_CORE_KEYBOARD_IMX_END { 0, 0, 0 }

/**
 * @name km_core_keyboard_load_from_blob function
 *
 * Parse and load keyboard from the supplied blob and a pointer to the loaded
 * keyboard into the out paramter.
 *
 * @param  kb_name    a string with the name of the keyboard.
 * @param  blob       a byte array containing the content of a KMX/KMX+ file.
 * @param  blob_size  a size_t variable with the size of the blob in bytes.
 * @param  keyboard   A pointer to result variable: A pointer to the opaque
 *                    keyboard object returned by the Processor. This memory
 *                    must be freed with a call to [km_core_keyboard_dispose].
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event an internal memory allocation fails.
 *
 *   `KM_CORE_STATUS_IO_ERROR`
 *   : In the event the keyboard file is unparseable for any reason
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : In the event `keyboard` is null.
 *
 *   `KM_CORE_STATUS_OS_ERROR`
 *   : Bit 31 (high bit) set, bits 0-30 are an OS-specific error code.
 */
KMN_API
km_core_status km_core_keyboard_load_from_blob(
  const km_core_path_name kb_name,
  const void* blob,
  const size_t blob_size,
  km_core_keyboard** keyboard
);

/**
 * @name km_core_keyboard_dispose function
 *
 * Free the allocated memory belonging to an opaque keyboard object previously
 * returned by [km_core_keyboard_load_from_blob].
 *
 * @param  keyboard  A pointer to the opaque keyboard object to be disposed of.
 */
KMN_API
void
km_core_keyboard_dispose(
  km_core_keyboard const* keyboard
);

/**
 * @name km_core_keyboard_get_attrs function
 *
 * Returns the const internal attributes of the keyboard. This structure is
 * valid for the lifetime of the opaque keyboard object. Do not modify the
 * returned data.
 *
 * @param  keyboard  A pointer to the opaque keyboard object to be queried.
 * @param  out       A pointer to the result: A pointer to a
 *                   [km_core_keyboard_attrs] structure.
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If non-optional parameters are null.
 */
KMN_API
km_core_status
km_core_keyboard_get_attrs(
  km_core_keyboard const *keyboard,
  km_core_keyboard_attrs const **out
);

/**
 * @name km_core_keyboard_get_key_list function
 *
 * Returns the unordered full set of modifier+virtual keys that are handled by
 * the keyboard. The matching dispose call needs to be called to free the
 * memory.
 *
 * @param keyboard   A pointer to the opaque keyboard object to be queried.
 * @param out        A pointer to an array of [km_core_keyboard_key] structures,
 *                   terminated by `KM_CORE_KEYBOARD_KEY_LIST_END`.
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If non-optional parameters are null.
 */
KMN_API
km_core_status
km_core_keyboard_get_key_list(
  km_core_keyboard const *keyboard,
  km_core_keyboard_key **out
);

/**
 * @name km_core_keyboard_key_list_dispose function
 *
 * Free the allocated memory belonging to a keyboard key list previously
 * returned by [km_core_keyboard_get_key_list].
 *
 * @param key_list  A pointer to the keyboard key list to be disposed of.
 */
KMN_API
void
km_core_keyboard_key_list_dispose(
  km_core_keyboard_key *key_list
);

/**
 * @name km_core_keyboard_get_imx_list function
 *
 * Returns the list of IMX libraries and function names that are referenced by
 * the keyboard. The matching dispose call needs to be called to free the
 * memory.
 *
 * @param  keyboard   A pointer to the keyboard
 * @param  imx_list   A pointer to a variable that will contain a pointer to
 *                    the IMX list.
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : If non-optional parameters are null.
 */
KMN_API
km_core_status
km_core_keyboard_get_imx_list(
  km_core_keyboard const *keyboard,
  km_core_keyboard_imx **imx_list
);

/**
 * @name km_core_keyboard_imx_list_dispose function
 *
 * Disposes of the IMX list.
 *
 * @param  imx_list   A pointer to the IMX list.
 */
KMN_API
void
km_core_keyboard_imx_list_dispose(
  km_core_keyboard_imx *imx_list
);

/**
 * @name km_core_state_imx_register_callback function
 *
 * Register the IMX callback endpoint for the client.
 *
 * @param  state            A pointer to the opaque state object
 * @param  imx_callback     Pointer to a function that implements the IMX
 *                          callback
 * @param  callback_object  An opaque pointer that can be used to pass context
 *                          information to the callback function, usually it is
 *                          a user-defined data structure.
 */
KMN_API
void
km_core_state_imx_register_callback(
  km_core_state *state,
  km_core_keyboard_imx_platform imx_callback,
  void *callback_object
);

/**
 * @name km_core_state_imx_deregister_callback function
 *
 * De-register IMX callback endpoint for the client.
 *
 * @param  state            A pointer to the opaque state object
 */
KMN_API
void
km_core_state_imx_deregister_callback(
  km_core_state *state
);

/**
 * @name km_core_state_create function
 *
 * Create a keyboard processor state object, maintaining state for the keyboard
 * in the environment passed.
 *
 * @param  keyboard  A pointer to the opaque keyboard object this object will
 *                   hold state for.
 * @param  env       The array of [km_core_option_item] key/value pairs used to
 *                   initialise the environment, terminated by
 *                   `KM_CORE_OPTIONS_END`.
 * @param  out       A pointer to result variable: A pointer to the opaque state
 *                   object returned by the Processor, initalised to maintain
 *                   state for `keyboard`. This must be disposed of by a call to
 *                   [km_core_state_dispose].
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event memory is unavailable to allocate a state object.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : In the event the `keyboard` or `out` pointer are null.
 */
KMN_API
km_core_status
km_core_state_create(
  km_core_keyboard const *keyboard,
  km_core_option_item const *env,
  km_core_state **out
);

/**
 * @name km_core_state_clone function
 *
 * Clone an existing opaque state object.
 *
 * @param  state  A pointer to the opaque state object to be cloned.
 * @param  out    A pointer to result variable: A pointer to the opaque state
 *                object returned by the Processor, cloned from the existing
 *                object `state`. This must be disposed of by a call to
 *                [km_core_state_dispose].
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event memory is unavailable to allocate a state object.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : In the event the `state` or `out` pointer are null.
 */
KMN_API
km_core_status
km_core_state_clone(
  km_core_state const *state,
  km_core_state **out
);

/**
 * @name km_core_state_dispose function
 *
 * Free the allocated resources belonging to a [km_core_state] object previously
 * returned by [km_core_state_create] or [km_core_state_clone]. After this all
 * pointers previously returned by any [km_core_state] family of calls will
 * become invalid.
 *
 * @param  state  A pointer to the opaque state object to be disposed.
 */
KMN_API
void
km_core_state_dispose(
  km_core_state const *state
);

/**
 * @name km_core_debug_context_type enum
 *
 *
 * As of version 17, the cached context is an internal property of the state,
 * not exposed to the consumer of the API -- apart from the Keyman Developer
 * Keyboard Debugger. However, for other debug purposes, it is helpful to be
 * able to examine the cached context, so a debug-formatted version of the
 * context is made available with  [km_core_state_context_debug]. This is not
 * intended to be parsed for reading the context for other purposes, and the
 * format may change.
 *
 * The three context types are: cached, intermediate, and app.
 */
typedef enum {
  /** the internal context used by Core, which may be normalized and may contain
   *  markers. This is set via [km_core_state_context_set_if_needed], and will
   *  be modified during keystroke event processing. */
  KM_CORE_DEBUG_CONTEXT_CACHED        = 0,

  /** internal context used by IMX, only valid during keystroke event
   *  processing. */
  KM_CORE_DEBUG_CONTEXT_INTERMEDIATE  = 1,

  /** an exact copy of the current context passed in to
   *  [km_core_state_context_set_if_needed], which is used to verify the precise
   *  text manipulations required when emitted changes. This input context is in
   *  "NFU" -- normalization form unknown, and may be mixed normalization so may
   *  require fixups when it is manipulated by keyboard processors that support
   *  normalization, such as the LDML keyboard processor. */
  KM_CORE_DEBUG_CONTEXT_APP           = 2
} km_core_debug_context_type;

/**
 * @name km_core_state_context_debug function
 *
 * Returns a debug formatted string of the context from the state.
 *
 * @param   state         A pointer to the opaque state object to be queried.
 * @param   context_type  The type of context to retrieve from the state.
 * @returns  A pointer to a [km_core_cu] UTF-16 string. Must be disposed of by a
 *           call to [km_core_cu_dispose].
 */
KMN_API
km_core_cu *
km_core_state_context_debug(
  const km_core_state *state,
  km_core_debug_context_type context_type
);

/**
 * @name km_core_cu_dispose function
 *
 * Free the allocated memory belonging to a [km_core_cu] array previously
 * returned by [km_core_state_context_debug]. May be `nullptr`.
 *
 * @param cp  A pointer to the start of the [km_core_cu] array to be disposed
 *            of.
 */
KMN_API
void
km_core_cu_dispose(
  km_core_cu *cp
);

//-------------------------------------------------------------------------------
/**
---
filename: processor.md
title: Processor - Keyman Core API
---
*/

/**
 * @name km_core_event_flags enum
 *
 * Bit flags to be used with the `event_flags` parameter of
 * [km_core_process_event].
 */
enum km_core_event_flags {
  /** default value: hardware */
  KM_CORE_EVENT_FLAG_DEFAULT = 0,

  /** set if the event is touch, otherwise hardware */
  KM_CORE_EVENT_FLAG_TOUCH = 1,
};

/**
 * @name km_core_process_event function
 *
 * Run the keyboard on an opaque state object with the provided virtual key and
 * modifer key state. Updates the state object as appropriate and fills out its
 * internal set of actions, which can be retrieved with
 * [km_core_state_get_actions].
 *
 * The state's actions will be cleared at the start of this call; options and
 * context in the state may also be modified.
 *
 * @param  state           A pointer to the opaque state object.
 * @param  vk              A virtual key to be processed.
 * @param  modifier_state  The combinations of modifier keys set at the time key
 *                         `vk` was pressed, bitmask from the
 *                         [km_core_modifier_state] enum.
 * @param  event_flags     Event level flags, see [km_core_event_flags]
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event memory is unavailable to allocate internal buffers.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : In the event the `state` pointer is null or an invalid virtual key or
 *     modifier state is passed.
 */
KMN_API
km_core_status
km_core_process_event(
  km_core_state const *state,
  km_core_virtual_key vk,
  uint16_t modifier_state,
  uint8_t is_key_down,
  uint16_t event_flags
);

/**
 * @name km_core_event function
 *
 * Tell the keyboard processor that an external event has occurred, such as a
 * keyboard being activated through the language switching UI.
 *
 * The keyboard processor may generate actions which should be processed by the
 * consumer of the API.
 *
 * The actions will be cleared at the start of this call; options and context in
 * the state may also be modified.
 *
 * @param  state  A pointer to the opaque state object.
 * @param  event  The event to be processed, from [km_core_event_code]
 *                enumeration
 * @param  data   Additional event-specific data. Currently unused, must be
 *                nullptr.
 * @returns  One of the following values:
 *
 *   `KM_CORE_STATUS_OK`
 *   : On success.
 *
 *   `KM_CORE_STATUS_NO_MEM`
 *   : In the event memory is unavailable to allocate internal buffers.
 *
 *   `KM_CORE_STATUS_INVALID_ARGUMENT`
 *   : In the event the `state` pointer is null or an invalid event or data is
 *     passed.
 */
KMN_API
km_core_status
km_core_event(
  km_core_state const *state,
  uint32_t event,
  void* data
);

/**
 * @name km_core_event_code enum
 *
 * Possible events to be passed into Keyman Core from the Platform layer.
 */
enum km_core_event_code {
  /** A keyboard has been activated by the user. The processor may use this
   *  event, for example, to switch caps lock state or provide other UX. */
  KM_CORE_EVENT_KEYBOARD_ACTIVATED = 1,
  //future: KM_CORE_EVENT_KEYBOARD_DEACTIVATED = 2,
};

/*
$EOF
*/

#if defined(__cplusplus)
} // extern "C"
#endif
