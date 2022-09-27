# Using the build-utils.sh builder functions

The Keyman repository is standardising on bash scripts for builds. These may
call project-specific builders, such as `tsc` for Typescript projects, `meson`
for our cross-platform C++ projects, `xcodebuild` on macOS and iOS projects,
`gradle` for Android, `nmake` in our Windows builds, or worse, but each project
should also have a `build.sh` script in its root.

We have standardised on parameters and structure for `build.sh` scripts. The
objectives are:

1. to be consistent in use of script parameters across all platforms and
   projects
2. to be self-documenting in usage (`--help` should always tell you all you need
   to know)
3. for the scripts to be easily readable, coherent, and straightforward for
   anyone involved in the project to maintain.

* [Jump to API definitions](#builder-api-functions-and-variables)

---

# Anatomy of a build script

A build script is made up of three sections:

* [Prologue](#build-script-prologue)
* [Definition](#defining-build-script-parameters)
* [Processing actions](#build-script-actions)

# Build script prologue

A build script should always start with the following prologue:

```bash
#!/usr/bin/env bash
#
# <short description of the script purpose>

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/<relative-path-to-repo-root>/resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# any other includes, such as jq.inc.sh

################################ Main script ################################
```

This prologue ensures that we have a consistent environment. Explaining each section:

## Shebang

```bash
#!/usr/bin/env bash
```

We use the `/usr/bin/env` prefix to ensure that we get the right version of bash
on macOS (installed via homebrew, rather than the obsolete system-provided one).
This also works fine on Linux, git bash on Windows, and WSL.

## Bash options (`set -eu`)

We use `set -eu` throughout:

* `-e` to exit on any statement failure
* `-u` to abort on unset variable use (usually coming from typos)

## Standard build script include

```bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/<relative-path-to-repo-root>/resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE
```

This somewhat unwieldy incantation handles all our build environments, with
`greadlink` necessary on macOS (again installed with homebrew) due to the
included macOS `readlink` struggling with canonicalization of symbolic links.
The intent is to get a good solid consistent path for the script so that we can
safely include the build script, no matter what `pwd` is when the script is run.

The only modification permissible in this block is the
`<relative-path-to-repo-root>` text which will be a series of `../` paths taking
us to the repository root from the location of the script itself.

Inclusion of other scripts should be kept outside this standard build script
include section, as we may programatically update (a.ka. global
search-and-replace) this section in the future as required.

## Any other includes

Once `build-utils.sh` has been included, the variable `$REPO_ROOT` will be
available, so other include scripts should be sourced accordingly, for example:

```bash
. "$REPO_ROOT/resources/build/jq.inc.sh"
```

## Split

The comment line splitting the prologue from the body of the script is optional,
but makes the script easy to scan!

```bash
################################ Main script ################################
```

# Defining build script parameters

The build script should use the `builder` functions and variables to process its
command line and control its run.

Build scripts can define **targets**, **actions**, and **options**, which are
parameters passed in to the script when it is run by a user or called by
another script:

* **targets**: these are the expected outputs of the build script. A target is
  prefixed with a `:`, for example `:app`. If no target is defined for a script,
  then the default target `:project` is used.

* **actions**: these are the various actions that a build script can take, such
  as `clean`, or `build`. If no action is passed in to on a given script
  invocation, then the default action is `build` (unless the script defines an
  alternative default).

* **options**: these are possible additional options that can be passed to the
  script to modify the behavior of the script. All options should be prefixed
  with `--`, such as `--debug`, and a shorthand single letter form may also be
  optionally provided, such as `-d`.

  Note that when we call scripts from other scripts, particularly in CI, we
  should always use the longhand form; the shorthand form is for convenience on
  the command line only.

  Be judicious in use of options; a common one will be `--debug` to do a debug
  build, but overuse of options will make scripts hard to use.

  Options can be used to provide additional data, by including `=<varname>` in
  their definition. Otherwise, they are treated as a boolean.

The first step in your script is to describe the available parameters, using
[`builder_describe`], for example:

```bash
builder_describe \
  "Tests the build-utils.sh builder functions. This is merely an example." \
  clean \
  build \
  test \
  "install         Installs something on the local system" \
  :app \
  ":engine         The internal engine for the app" \
  "--power,-p      Use powerful mode" \
  "--zoom,-z       Use zoom mode" \
  "--feature=FOO   Enable feature foo"
```

After describing the available parameters, you need to pass the command line
parameters in for parsing and validation:

```bash
builder_parse "$@"
```

If any parameters are invalid, the script will be terminated by
[`builder_parse`](#builderparse-function) with an error and will print the
script usage help using [`builder_display_usage`](#builderdisplayusage-function).

# Build script actions

Then, check each of the potential actions, in the order that they should be run,
for example:

```bash
if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  npm run clean
  rm -f ./version.inc.ts
  builder_finish_action success clean
fi

if builder_start_action build; then
  npm run build -- $builder_verbose
  builder_finish_action success build
fi
```

Each step is run separately, is started with [`builder_start_action`], and
finishes with [`builder_finish_action`]. If a build step is complex, it may be
worthwhile splitting it into a separate function or even a separate script
include.

Use the longer form of `if ...; then` rather than the shorter `[ ... ] && `
pattern, for consistency and readability.

# Standard builder parameters

The following parameters are pre-defined and should not be overridden:

* `--help`, `-h`: displays help on using this script
* `--color`: forces on ANSI color output for the script
* `--no-color`: forces off ANSI color output for the script
* `--verbose`, `-v`: verbose mode, sets the [`$builder_verbose`] variable

# Builder API functions and variables


## `builder_check_color` function

If you wish to provide [formatting variables] in your [`builder_describe`] call, you
will need to use `builder_check_color` first. This function takes the same
parameters as [`builder_parse`].

### Usage

```bash
builder_check_color "$@"
builder_describe "sample" \
  "--ci    For use with action ${BUILDER_TERM_START}test${BUILDER_TERM_END} - emits CI-friendly test reports"
```


## `builder_describe` function

Describes a build script, defines available parameters and their meanings. Use
together with `builder_parse` to process input parameters.

### Usage

```bash
builder_describe description param_desc...
```

### Parameters

* `description`: A short description of what the script does
* `param_desc`:  Space separated name and description of parameter.

### Description

The `param_desc` parameter has two components: first, the parameter definition,
and second, an optional description for the parameter. The parameter definition
must not include any spaces, and the description, if included, must be preceded
by at least one space. This means that the parameters should be surrounded by
quote marks so that they are treated as a single parameter, for example:

```bash
builder_describe "Sample script" \
  ":app   the app" \
  configure \
  build \
  test \
  "--print-errors,-p    Print errors"
```

Or, a shorthand version for a simple script:

```bash
builder_describe "Build version module" clean configure build test
```

Each `param_desc` parameter defines a **target**, **action**, or **option**. All
parameters passed on the command line in a call to the script (prior to `--`,
see [`$builder_extra_params`] variable) must match one of the parameters defined
here.

**Targets** are defined by including a `:` prefix, for example:

```bash
builder_describe "Sample script" :engine ":proxy  the proxy module"
```

There are several predefined targets. These will not be available to users of
your script unless you include them in the `builder_describe` call, but when
used, they have default descriptions, which can be used instead of adding your
own in the call:
  * `:project`: `"this project"`
  * `:app`:     `"main app"`
  * `:engine`:  `"engine module"`
  * `:module`:  `"this module"`
  * `:tools`:   `"build tools for this project"`

**Actions** are defined as single words, for example:

```bash
builder_describe "Sample script build "install   installs app on local system"
```

There are several predefined actions. Again, these will not be available to
users unless you include them in the call, but they do have default
descriptions:
  * `clean`:     `"remove build/ folder and build artifacts"`
  * `configure`: `"install dependencies, e.g. npm"`
  * `build`:     `"build target(s)"`
  * `test`:      `"run automated tests"`

The default action will be `build`, unless overridden by using the `+` suffix on
a definition:

```bash
builder_describe "Testing script" clean test+
```

**Options** are defined by including a `--` prefix, for example:

```bash
builder_describe "Sample script" \
  --debug,-d \
  "--out-path,-o=OUT_PATH    Specify output path"
```

A shorthand form may optionally be provided by appending `,-x` to the parameter
definition, where `x` is a one letter shorthand form. Currently, shorthand forms
may not be combined when invoking the script -- each must be passed separately.
Ensure that you do not include a space after the comma.

By default, an option will be treated as a boolean. It can be tested with
[`builder_has_option`]. If you need to pass additional data, then the
`=<variable>` format specifies an environment variable where the additional data
will be stored. When using this format, it is good to use [`builder_has_option`]
to test for the presence of the parameter before attempting to use the variable.

**Note:** although the definition uses `=` to define the variable, when invoking
script, the value should be passed in as a separate parameter.

There is one option with a predefined description: `--debug`. When including
this, you should use `--debug,-d` to enable the shorthand form.

Note that you should not include any of the [standard builder parameters] here.


## `builder_display_usage` function

Prints the help for the script, constructed from the [`builder_describe`]
parameters, so must be called after `builder_describe`.

### Usage

```bash
builder_describe "sample" clean build test
builder_display_usage
```


## `$builder_extra_params` variable

If a build script needs to be able to pass arbitrary additional parameters onto
another tool, for example, to a test runner, then the `--` parameter can be
used, for example:

```bash
./build.sh test -- test-window-color --verbose
```

These two additional parameters will be available in the `$builder_extra_params`
array variable, which can then be used in a call to the tool, using the `${var[@]}`
array expansion format:

```bash
npm test -- "${builder_extra_params[@]}"
```


## `builder_finish_action` function

Finishes an action sequence. Should always be paired with [`builder_start_action`].

### Usage

```bash
if builder_start_action action:target; then
  # ... do the action
  if something_failed; then
    builder_finish_action "yeah, something failed" action:target
    exit 1
  fi
  builder_finish_action success action:target
fi
```

### Parameters

* **result**:  Result or message -- `success`, `failure`, or a more detailed
  failure message
* **action**:  Action to test
* **:target**: Target to test

These last two parameters can optionally be space separated.

### Description
In normal circumstances, `builder_finish_action` will then print a corresponding
message:

```
## [common/web/keyman-version] action:target completed successfully
```

When errors arise, a failure message will be printed, and the script will abort
with a non-zero exit code:

```
## [common/web/keyman-version] action:target failed with message: yeah, something failed
```


## `builder_has_action` function

This is similar to [`builder_start_action`], testing whether the script
invocation included a specific action, but does not start the action, and thus
does not print any messages to the console.

### Usage

```bash
if builder_has_action action:target; then
  # ...
fi
```

See [`builder_start_action`] for more details.


## `builder_has_option` function

Tests if an option has been passed in the script invocation. The option must be
defined in [`builder_describe`].

### Usage

```bash
if builder_has_option --option; then
  # ...
fi
```

### Parameters

* **--option**: The option to test. Must be the longhand form, and must be
  prefixed with `--`.

### Description

When testing for presence of options that take additional data, the additional
data variable will only be set if the option is passed in the script invocation.
So `builder_has_option` is a clean way to test for the presence of the option in
this case too:

```bash
builder_describe "Sample" "--path=OUT_PATH"
builder_parse "$@"

if builder_has_option --path; then
  echo "The output path is $OUT_PATH"
fi
```


## `builder_parse` function

Initializes a build.sh script, parses command line. Will abort the script if
invalid parameters are passed in. Use together with [`builder_describe`], which
sets up the possible command line parameters.

### Usage

```bash
builder_parse "$@"
```

### Description

Generally, you will always pass `"$@"` as the parameter for this call, to pass
all the command line parameters from the script invocation, with automatically
correct quoting and escaping.


## `builder_start_action` function

Starts an action and prints a message to the console, if the user has provided
the action in the script invocation.

### Usage

```bash
if builder_start_action action:target; then
  # ... do the action
fi

if builder_start_action action :target; then
  # ... do the action
fi

if builder_start_action action; then
  # ... do the action for default target (:project)
fi
```

### Parameters

* **action**  Action to test
* **:target** Target to test

These two parameters can optionally be space separated.

### Description

`builder_start_action` will only return `0` if the user passes that action as a
parameter when invoking the script. If the user has passed that action in, or
the action is the default (when no actions are provided), then the function will
also print a log message indicating that the action has started, for example:

```
## [common/web/keyman-version] build:project starting...
```


## `builder_use_color` function

This will normally be managed internally by build-utils, but can be manually
overridden with:

```bash
builder_use_color true
# or
builder_use_color false
```

## `$builder_verbose` variable

This standard variable will be set to `"--verbose"`, if the `--verbose` or `-v`
parameter is passed on the command line, and otherwise will be set to `""`.

### Usage

For example, can be used to pass `--verbose` to another app:

```bash
npm test -- $builder_verbose

# Can also be used like a standard option:
if builder_has_option --verbose; then
  # ...
fi
```

## Formatting variables

These helper variables define ANSI color escapes when running in color mode, and
resolve either to empty string (for `$COLOR_*`), or equivalent plain-text forms
(for `$BUILDER_TERM_*`) when running without color:

* `$BUILDER_TERM_START`: Use blue to start definition of a term for builder
  documentation (or `<` in plain-text)
* `$BUILDER_TERM_END`: Return to standard color to finish definition of a term
  for builder documentation (or `>` in plain-text)
* `$COLOR_RED`: Red (error)
* `$COLOR_GREEN`: Green (success)
* `$COLOR_BLUE`: Blue (heading / informational)
* `$COLOR_YELLOW`: Yellow (warning)
* `$COLOR_RESET`: Back to default (light grey/white)
* `$HEADING_SETMARK`: Add a setmark, e.g. with VSCode
  <https://code.visualstudio.com/updates/v1_69#_setmark-sequence-support>


[standard builder parameters]: #standard-builder-parameters
[`builder_check_color`]: #buildercheckcolor-function
[`builder_describe`]: #builderdescribe-function
[`builder_display_usage`]: #builderdisplayusage-function
[`$builder_extra_params`]: #builderextraparams-variable
[`builder_finish_action`]: #builderfinishaction-function
[`builder_has_action`]: #builderhasaction-function
[`builder_has_option`]: #builderhasoption-function
[`builder_parse`]: #builderparse-function
[`builder_start_action`]: #builderstartaction-function
[`builder_use_color`]: #builderusecolor-function
[`$builder_verbose`]: #builderverbose-variable
[formatting variables]: #formatting-variables
