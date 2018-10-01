/*
# Keyman Keyboard Processor API

## Requirements
1. Cross platform.
2. Cross language.
3. Facilitate stateless operation of the Engine.
4. Keyboard format agnostic -- support both KMN and future LDML based keyboards.
5. Support querying Engine attributes.
6. Support querying Keyboard attributes.
7. Idempotent


## Glossary
- __Platform layer:__
the code that consumes the Keyman Keyboard Processor API, and provides the
operating system-specific handling of keystroke events and integration with
applications.
- __Client Application:__
the application that has the focus and receives text events from the Platform
layer.
- __Context:__ Text preceding the insertion point
- __Marker:__ Positional state that can be placed in the Context.
- __Keyboard:__ A set of rules for execution my an Engine
- __Option:__ A variable in a dynamic or static key value store.
- __Processor:__
The componennt that can parse, and execute a particular keyboard.
- __State:__ An object that hold internal state of the Processor for a given
insertion point
- __Action:__
A directive output by the processor detailing how the Platform layer should
transform the Client Application's text buffer.
- __(Keyboard) Event:__
A virtual key board event and modifier map recevied from the platform to be
processed with the state object for this Client application.


## Design decisions in support of requirements:
- Use C or C99 types and calling convention for the interface, it has the
  broadest language FFI support. [1,2]
- Have client (platform glue) code load keyboards, manage & pass state. [3,4,7]
- Provide query calls to return static attributes data for keyboards and
  engine [5,6]
- Provide get/set calls for client accessible keyboard state information [3,4]

### Open decisions:
Text encoding for passing string data:
- __UTF-8__
Fits in existing C style strings, good space for BMP characters, good for
interchange, but extra processing for per character access.
- __UTF-16__
Best space compromise for BMP characters, surrogates.
- __UTF-32__
Worst BMP space use, but no processing overhead for indexing, no
multibyte issues.
- __All of the above let the client specify__
Least attractive but we do have a fast UTF encoder/decoder available
from graphite.
- __Native C/C++ multibyte widechar support__
Used to have issues but might be worth investigation.

Naming style:
- __Underscore__: All_names_are_lower_case
- __Camelcase__: AllWordsAreCapitalised
- __Drooping Camelcase__: exceptTheFirst

UTF-16 has been picked as Windows Win32 uses UTF-16 for wide character strings
as does MacOS NString. For a naming style the Underscore sytle has been chosen
because it loosely matches the C/C++ stdlib conventions and is a common well
understood style.

## API
### Namespace
All calls, types and enums are prefixed with the namespace identifier `km_kbp_`
```c
*/
#include <stdint.h>
#include <stdlib.h>

#define KM_KBP_LIB_CURRENT  0
#define KM_KBP_LIB_AGE      0
#define KM_KBP_LIB_REVISION 0

#if defined(__cplusplus)
extern "C"
{
#endif
// Basic types
//
typedef uint16_t    km_kbp_cp;  // code point
typedef uint32_t    km_kbp_usv; // Unicode Scalar Value
typedef uint16_t    km_kbp_virtual_key;


/*```
### Error Handling
Error handling and success failure notification are communicated through a
general mechanism similar to COM’s `HRESULT` scheme. Any functions that can
fail will always return a status value and all results are returned via
outparams passed to the function.
```c
*/
typedef uint32_t km_kbp_status;
enum km_kbp_status_codes {
  KM_KBP_STATUS_OK = 0,
  KM_KBP_STATUS_NO_MEM = 1,
  KM_KBP_STATUS_IO_ERROR = 2,
  KM_KBP_STATUS_INVALID_ARGUMENT = 3,
  KM_KBP_STATUS_KEY_ERROR = 4,
  KM_KBP_STATUS_OS_ERROR = 0x80000000
};

/*
```
The final status code KM_KBP_STATUS_OS_ERROR is intended to allow encapsulating
a platform error code, the remaining 31 low bits are the error code returned by
the OS for case where the failure mode is platform specific. For HRESULT codes
this only permits failure codes to be passed.


### Context
The context is the text to the left of the insertion point (caret, cursor).
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

Contexts are always owned by their state.  They maybe set to a list of
context_items or interrogated for their current list of context items.
```c
*/
typedef struct km_kbp_context km_kpb_context;

enum km_kbp_context_type {
  KM_KBP_CT_END,
  KM_KBP_CT_CHAR,
  KM_KBP_CT_MARKER
};

typedef struct {
  uint8_t   type;
  uint8_t   reserved[3];
  union {
    km_kbp_usv  character;
    uint32_t    marker;
  };
} km_kbp_context_item;
/*
```
### `km_kbp_context_items_from_utf16`
##### Description:
Convert an UTF16 encoded Unicode string into an array of `km_kbp_context_item`
structures. Allocates memory as needed.
##### Status:
`KM_KBP_STATUS_NO_MEM`: In the event it cannot allocated enough memory for the
output buffer.
`KM_KBP_STATUS_INVALID_ARGUMENT`: In the event the UTF16 string cannot be
decoded.
##### Parameters:
- __text__: a pointer to a null terminated array of utf16 encoded data.
- __out_ptr__: a pointer to the result variable:
    A pointer to the  start of the `km_kbp_context_item` array containing the
    representation of the input string. Terminated with a type
    of `KM_KBP_CT_END`

```c
*/
km_kbp_status
km_kbp_context_items_from_utf16(km_kbp_cp const *text,
                                km_kbp_context_item **out_ptr);

/*
```
### `km_kbp_context_items_to_utf16`
##### Description:
Convert an context item array into an UTF-16 encoded string placing it into
the supplied buffer of specified size, and return the number codepoints
actually used in the conversion. If null is passed as the buffer the number
codeunits required is returned. This will strip markers from the
context during the conversion.
##### Return:
Number of code points needed.
##### Parameters:
- __context_items__: A pointer to the start of an array `km_kbp_context_item`.
- __buf__: A pointer to the buffer to place the UTF-16 string into, can be null.
- __buf_size__: The size of the supplied buffer.

```c
*/
size_t km_kbp_context_items_to_utf16(km_kbp_context_item const *,
                                     km_kbp_cp *buf, size_t buf_size);

/*
```
### `km_kbp_context_items_dispose`
##### Description:
Free the allocated memory belonging to a `km_kbp_context_item` array previously
returned by `km_kbp_context_items_from_utf16`.
##### Parameters:
- __context_items__: A pointer to the start of the `km_kbp_context_item` array
    to be disposed of.

```c
*/
void km_kbp_context_items_dispose(km_kbp_context_item *);

/*
```
### `km_kbp_context_set`
##### Description:
Replace the contents of the current context with a new sequence of
context_items
##### Status:
`KM_KBP_STATUS_NO_MEM`: in the event it cannot allocated enough memory to grow
the context internally.
##### Parameters:
- __context__: A pointer to an opaque context object
- __context_items__: A pointer to  the start of the `km_kbp_context_item` array
    containing the new context. It will be terminated with an item of type
    `KM_KBP_CT_END`.

```c
*/
km_kbp_status km_kbp_context_set(km_kbp_context *, km_kbp_context_item const *);

/*
```
### `km_kbp_context_get`
##### Description:
Get a pointer the the context's internal list of items.
##### Return:
A pointer to the start of the `km_kbp_context_item` array containing the
context's current items. It will be terminated with an item of type
`KM_KBP_CT_END`.
##### Parameters:
- __context__: A pointer to an opaque context object

```c
*/
km_kbp_context_item const * km_kbp_context_get(km_kbp_context *);

/*
```
### `km_kbp_context_clear`
##### Description:
Clear the context.
##### Parameters:
- __context__: A pointer to an opaque context object

```c
*/
void km_kbp_context_clear(km_kbp_context *);

/*
```
### `km_kbp_context_length`
##### Description:
Return the number of items in the context.
##### Return:
The number of items in the context
##### Parameters:
- __context__: A pointer to an opaque context object

```c
*/
size_t km_kbp_context_length(km_kbp_context *);

/*
```
### `km_kbp_context_append`
##### Description:
Add more items to the end (insertion point) of the context. If these exceed the
maximum context length the same number of items will be dropped from the
context's beginning.
##### Status:
`KM_KBP_STATUS_NO_MEM`: in the event it cannot allocated enough memory to grow
the context internally.
##### Parameters:
- __context__: A pointer to an opaque context object
- __context_items__: Pointer to the start of the `KM_KBP_CT_END` terminated array
    of `km_kbp_context_item`

```c
*/
km_kbp_status km_kbp_context_append(km_kbp_context *,
                                    km_kbp_context_item const *);

/*
```
### `km_kbp_context_shrink`
##### Description:
Remove a specified number of items from the end of the context, optionaly
add upto the same number of the supplied items to the front of the context.
##### Parameters:
- __context__: A pointer to an opaque context object.
- __num__: The number of items to remove from the end of context.
- __context_items__: Pointer to the start of the `KM_KBP_CT_END` terminated array
    of `km_kbp_context_item` to add to the front. Upto `num` items will be
    prepended.

```c
*/
void km_kbp_context_shrink(km_kbp_context *, size_t num,
                           km_kbp_context_item const* prefix);

/*
```
### Options
A state’s default options are set from the keyboard at creation time and the
environment. The Platform layer is then is expected to apply any persisted
options it is maintaining.  Options are passed into and out of API functions as
simple C arrays of `km_kbp_option` terminated with a `KM_KBP_OPTIONS_END`
sentinal value. A state's options are exposed and manipulatable via the
`km_kbp_option_set` API. All options are string values.

During processing when the glue code finds a PERSIST action type it should call
identify_option_src on the state to find out which store the option comes from,
read the updated value from the state’s option list and store the updated option
in the appropriate place. For RESET it should do the same but the read the
current default and update the options set.
```c
*/

typedef struct {
  char const *  key;
  char const *  value;
} km_kbp_option;

typedef struct km_kbp_option_set km_kbp_option_set;

#define KM_KBP_OPTIONS_END { 0, 0 }

/*
```
### `km_kbp_options_set_length`
##### Description:
Return the cardinality of a `km_kbp_option_set`.
##### Return:
The number of items in the supplied `km_kbp_option_set`
##### Parameters:
- __opts__: An opaque pointer to an `km_kbp_option_set`.

```c
*/
size_t km_kbp_options_set_size(km_kbp_option_set const *opts);

/*
```
### `km_kbp_options_set_lookup`
##### Description:
Lookup an option based on it's key, in an option set.
##### Return:
A pointer the `km_kbp_option` or `0` if not present.
##### Parameters:
- __opts__: An opaque pointer to an `km_kbp_option_set`.
- __key__: A C string that matches the key in the target `km_kbp_option`.

```c
*/
km_kbp_option const *km_kbp_options_set_lookup(km_kbp_option_set const *opts,
                                               const char *key);

/*
```
### `km_kbp_options_set_update`
##### Description:
Add or overwrite one or more options from a list of `km_kbp_option`s.
##### Status:
__KM_KBP_STATUS_NO_MEM__: In the event an internal memory allocation fails.
##### Parameters:
- __opts__: An opaque pointer to an `km_kbp_option_set`.
- __new_opts__: A C array of `km_kbp_option` objects to update or add. Must be
terminated with `KM_KBP_OPTIONS_END`.

```c
*/
km_kbp_status km_kbp_options_set_update(km_kbp_option_set * opts,
                                        km_kbp_option const *new_opts);

/*
```
### `km_kbp_options_set_to_json`
##### Description:
Export the contents of a `km_kbp_options_set` to a JSON formated document and
place it in the supplied buffer, reporting how much space was used. If null is
passed as the buffer the number of bytes required is returned. If there is
insufficent space to hold the document the contents of the buffer is undefined.
##### Status:
__KM_KBP_STATUS_NO_MEM__: In the event an internal memory allocation fails.
##### Parameters:
- __opts__: An opaque pointer to an `km_kbp_option_set`.
- __buf__: A pointer to the buffer to place the C string containing the JSON
document into, can be null.
- __space__: A pointer to a size_t variable. This variable must contain the
number of bytes available in the buffer pointed to by `buf`, unless `buf` is
null. On return it will hold how many bytes were used.

```c
*/
km_kbp_status km_kbp_options_set_to_json(km_kbp_option_set const * opts,
                                         char *buf,
                                         size_t *space);
/*
```
### Keyboards
```c
*/
typedef struct km_kbp_keyboard  km_kbp_keyboard;

typedef struct {
  char const *    version_string;
  char const *    id;
  char const *    folder_path;
  size_t          n_options;
  km_kbp_option   default_options[];
} km_kbp_keyboard_attrs;

/*
```
```c
*/
km_kbp_status km_kbp_keyboard_load(char const *kb_path,
                                   km_kbp_keyboard const **keyboard);

/*
```
```c
*/
void km_kbp_keyboard_dispose(km_kbp_keyboard const *);

/*
```
```c
*/
km_kbp_keyboard_attrs const *km_kbp_keyboard_get_attrs(km_kbp_keyboard const *);


/*
```
### Action Items
These provide the results of processing a key event to the glue code.
```c
*/
typedef struct {
  uint8_t   type;
  uint8_t   __reserved[3];
  union {
    km_kbp_virtual_key  vkey;       // VKEY types
    km_kbp_usv          character;  // CHAR type
    uintptr_t           marker;     // MARKER type
    char const *        options;    // OPT types
  };
} km_kbp_action_item;

enum km_kbp_action_type {
  KM_KBP_IT_END         = 0, // Marks end of action items list.
  KM_KBP_IT_VKEYDOWN    = 1,
  KM_KBP_IT_VKEYUP      = 2,
  KM_KBP_IT_VSHIFTDOWN  = 3,
  KM_KBP_IT_VSHIFTUP    = 4,
  KM_KBP_IT_CHAR        = 5,
  KM_KBP_IT_MARKER      = 6, // correlates to kmn's "deadkey" markers.
  KM_KBP_IT_BELL        = 7,
  KM_KBP_IT_BACK        = 8,
  KM_KBP_IT_PERSIST_OPT = 10,
  KM_KBP_IT_RESET_OPT   = 11,
  KM_KBP_IT_MAX_TYPE_ID
};


/*
```
### State
```c
*/
typedef struct km_kbp_state km_kbp_state;

enum km_kbp_state_flag {
  KM_KBP_FLAG_DEADKEY = 1,
  KM_KBP_FLAG_SURROGATE = 2
};


enum km_kbp_option_src {
  KM_KBP_OPT_KEYBOARD,
  KM_KBP_OPT_ENVIRONMENT
};


/*
```

```c
*/
km_kbp_status *km_kbp_state_create(km_kbp_keyboard const *,
                                  km_kbp_option const *env,
                                  km_kbp_state ** out);

/*
```

```c
*/
km_kbp_state *km_kbp_state_clone(km_kbp_state *);

/*
```

```c
*/
void km_kbp_state_dispose(km_kbp_state *);

/*
```

```c
*/
uint32_t km_kbp_state_flags(km_kbp_state *);

/*
```

```c
*/
km_kbp_context *km_kbp_state_context(km_kbp_state *);

/*
```

```c
*/
km_kbp_option const *km_kbp_state_environment(km_kbp_state const *);

/*
```

```c
*/
km_kbp_option_set *km_kpb_state_options(km_kbp_state const *);

/*
```

```c
*/
km_kbp_option_src km_kpb_state_identify_option_src(km_kbp_state const *,
                                                   km_kbp_option const *);

/*
```

```c
*/
km_kbp_action_item const * km_kbp_state_action_items(km_kbp_state const *,
                                                     size_t *num_items);
/*```
*/

/*
```
### Processor
```c
*/
enum km_kbp_attr {
  KM_KBP_CURRENT     = 0,  // Current API number supported.
  KM_KBP_REVISION    = 1,  // Implementation number of current API.
  KM_KBP_AGE         = 2,  // Oldest API number supported.
  KM_KBP_TECH        = 3,  // Keyboard specification language KMN or LDML.
  KM_KBP_MAX_CONTEXT = 4,  // Maximum context size supported by processor.
  KM_KBP_MAX_ATTR_ID
};
typedef uint16_t km_kbp_attrs[KM_KBP_MAX_ATTR_ID];

enum km_kbp_tech_value {
  KM_KBP_TECH_KMN  = 0,
  KM_KBP_TECH_LDML = 1
};


/*
```
```c
*/
km_kbp_attrs const *  km_kbp_get_engine_attrs();


/*
```
```c
*/
bool km_kbp_process_event(km_kbp_virtual_key vk, uint32_t modifier_state,
                          km_kbp_state *state);


#if defined(__cplusplus)
} // extern "C"
#endif
