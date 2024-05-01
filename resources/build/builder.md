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
   anyone involved in the project to maintain
4. for dependencies to be simple, but flexible

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
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/<relative-path-to-repo-root>/resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# . "$KEYMAN_ROOT/.../foo.inc.sh"     # any other includes, such as jq.inc.sh

# cd "$THIS_SCRIPT_PATH"              # optionally, run from script directory

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

Builder scripts will inherit `set -eu` from builder.inc.sh:

* `-e` to exit on any statement failure
* `-u` to abort on unset variable use (usually coming from typos)

## Standard build script include

```bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/<relative-path-to-repo-root>/resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE
```

This somewhat unwieldy incantation handles all our build environments.
The intent is to get a good solid consistent path for the script so that we can
safely include the build script, no matter what `pwd` is when the script is run.


The only modification permissible in this block is the
`<relative-path-to-repo-root>` text which will be a series of `../` paths taking
us to the repository root from the location of the script itself.

It is essential to make the include relative to the repo root, even for scripts
under the resources/ folder.  Doing this gives us significant performance
benefits.

Inclusion of other scripts should be kept outside this standard build script
include section, as we may programatically update (a.ka. global
search-and-replace) this section in the future as required.

## Any other includes

Once `build-utils.sh` has been included, the variable `$KEYMAN_ROOT` will be
available, so other include scripts should be sourced accordingly, for example:

```bash
. "$KEYMAN_ROOT/resources/build/jq.inc.sh"
```

## Setting path

Many scripts will be easier to code if they run from a consistent path. Because
build scripts should be invokable from any directory, you may wish to add the
following line here:

```bash
cd "$THIS_SCRIPT_PATH"
```

## Standard environment

`build-utils.sh` will prepend `$KEYMAN_ROOT/node_modules/.bin` to the `PATH`
variable to ensure that we run the correct versions of npm package commands, so
there is no need to hard-code path references or add script wrappers to
package.json (`npm run <script>`).

* `BUILDER_CONFIGURATION` will be set to `debug` if the `--debug` option is
  passed in, or `release` otherwise, which corresponds to the output folder
  names for many projects.

Other environment variables and paths will probably be added over time.

## Split

The comment line splitting the prologue from the body of the script is optional,
but makes the script easy to scan!

```bash
################################ Main script ################################
```

# Defining build script parameters

The build script should use the `builder` functions and variables to process its
command line and control its run.

Build scripts can define **targets**, **actions**, **options**, and
**dependencies**, which are parameters passed in to the script when it is run by
a user or called by another script:

* **targets**: these are the expected outputs of the build script. A target is
  prefixed with a `:`, for example `:app`. If no target is defined for a script,
  then the default target `:project` is used.

  If a folder exists with the same name as a target, then that automatically
  denotes the target as a "child project". This can simplify parent-child style
  scripts, using the [`builder_run_child_actions`] function.

  A child project with an alternate folder can also be specified by appending
  `=path` to the target definition, for example `:app=src/app`. Where possible,
  avoid differences in names of child projects and folders.

* **actions**: these are the various actions that a build script can take, such
  as `clean`, or `build`. If no action is passed in to on a given script
  invocation, then the default action is `build` (unless the script defines an
  alternative default).

* **options**: these are possible additional options that can be passed to the
  script to modify the behavior of the script. All options should be prefixed
  with `--`, such as `--option`, and a shorthand single letter form may also be
  optionally provided, such as `-o`.

  Note that when we call scripts from other scripts, particularly in CI, we
  should always use the longhand form; the shorthand form is for convenience on
  the command line only.

  Be judicious in use of options; overuse of options will make scripts hard to
  use.

  **Note:** `--debug` (or `-d`) is a standard option and should not be declared
  again. See [`builder_is_debug_build`] for more details on the `--debug` flag.

  Options can be used to provide additional data, by including `=<varname>` in
  their definition. Otherwise, they are treated as a boolean.

* **dependencies**: these are other builder scripts which must be configured and
  built before the actions in this script can continue. Only `configure` and
  `build` actions are ever passed to dependency scripts; these actions will
  execute by default, for all targets of the dependency script.  If you are
  working on code within a dependency, you are currently expected to rebuild and
  test that dependency locally.

  A module dependency is similar to, but not the same as, a child project. Child
  projects live in sub-folders of the parent project, whereas generally a
  dependency will be in another folder altogether.

  Module dependencies can be defined for all actions and targets, or may be
  limited to specific action and/or targets.

  A module dependency can be on a single target within a module, instead of all
  targets within the module.

  **Dependency definitions**

  It can be easy to confuse different dependency types!

  * An _external dependency_ is a dependency on a 3rd party component, which
    typically needs to be downloaded during the `configure` stage of a script.
  * An _internal dependency_ is a dependency within the current script
    itself, such as `build` being internally dependent on `configure`.
  * A dependency on another builder script, as described above, is called a
    _module dependency_.
  * _Child projects_ are not dependencies. But they can feel quite similar.

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
include. See also [`builder_run_action`].

Use the longer form of `if ...; then` rather than the shorter `[ ... ] && `
pattern, for consistency and readability.

## Standard build script actions

While no build script actions are pre-defined as such, there are a set of
standard actions that we should be using where possible. The actions should
be defined in the build script and 'actioned' in the order listed below.

* `clean`: clean all artifacts. Running `clean` should generally be similar to
  `git clean -fdx .`; the main difference is that user-created config files such
  as codesigning controls would be kept.
* `configure`: install _external dependencies_, create build scripts where tools
  require it.
* `build`: do the build. note: if _module dependency_ artifacts must be copied
  or transformed at any stage, this should be done in the `build` action.
* `test`: run automated unit tests. Some e2e tests may run here, so long as they
  have no UX impact -- i.e. we should not run e2e tests that take over the
  system keyboard or emit key events by default.
* `install`: install the built artifact on the local system for use.
* `publish`: publish the built artifacts to relevant repositories.

# Internal dependencies

All build scripts have a set of automatic internal dependencies:

* `build` depends on `configure`
* `test`, `install`, and `publish` depend on `build`

Internal dependencies will be added to the list of targets for the build if you
have described outputs for them, and the outputs do not exist, and the
dependency is required for one of the targets specified on the command line.

The build order of dependencies is determined by the order in which
[`builder_start_action`] is called in the script for each action.

You can also define your own internal dependencies with
[`builder_describe_internal_dependency`]. This allows you to define dependencies
across targets. Use this judiciously; for example, Keyman Core uses this to
build both x86_64 and arm64 targets, and test only the appropriate architecture
on macOS.

# Standard builder parameters

The following parameters are pre-defined and should not be overridden:

* `--help`, `-h`: displays help on using this script
* `--color`: forces on ANSI color output for the script
* `--no-color`: forces off ANSI color output for the script
* `--verbose`, `-v`: verbose mode, sets the [`$builder_verbose`] variable
* `--debug`, `-d`: debug build; see [`builder_is_debug_build`] for more detail

--------------------------------------------------------------------------------

# Builder API functions and variables

## `$builder_debug` variable

This standard variable will be set to `"--debug"`, if the `--debug` or `-d`
parameter is passed on the command line, and otherwise will be set to `""`.

### Usage

For example, can be used to pass `--debug` to another app:

```bash
npm test -- $builder_debug
```

--------------------------------------------------------------------------------

## `builder_describe` function

Describes a build script, defines available parameters and their meanings. Use
together with [`builder_parse`] to process input parameters.

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

Each `param_desc` parameter defines a **target**, **action**, **option**, or
**dependency**. All parameters passed on the command line in a call to the
script (prior to `--`, see [`$builder_extra_params`] variable) must match one of
the parameters defined here.

**Targets** are defined by including a `:` prefix, for example:

```bash
builder_describe "Sample script" :engine ":proxy  the proxy module"
```

There are several predefined targets. These will not be available to users of
your script unless you include them in the [`builder_describe`] call, but when
used, they have default descriptions, which can be used instead of adding your
own in the call:
  * `:project`: `"this project"`
  * `:app`:     `"main app"`
  * `:engine`:  `"engine module"`
  * `:module`:  `"this module"`
  * `:tools`:   `"build tools for this project"`

If a folder exists with the same name as a target, then that automatically
denotes the target as a "child project". This can simplify parent-child style
scripts, using the [`builder_run_child_actions`] function.

A child project with an alternate folder can also be specified by appending
`=path` to the target definition, for example `:app=src/app`. Where possible,
avoid differences in names of child projects and folders.

**Actions** are defined as single words, for example:

```bash
builder_describe "Sample script" build "install   installs app on local system"
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

**Options** are defined by including a `--` prefix.

Specification of options: `"--option[,-o][+][=var]   [One line description]"`

```bash
builder_describe "Sample script" \
  --option,-o \
  "--out-path,-o=OUT_PATH    Specify output path"
```

A shorthand form may optionally be provided by appending `,-x` to the parameter
definition, where `x` is a one letter shorthand form. Currently, shorthand forms
may not be combined when invoking the script -- each must be passed separately.
Ensure that you do not include a space after the comma.

If a `+` is appended (after the optional shorthand form, but before the
default), then the option will be passed to child scripts. All child scripts
_must_ accept this option, or they will fail. It is acceptable for the child
script to declare the option but ignore it. However, the option will _not_ be
passed to dependencies.

By default, an option will be treated as a boolean. It can be tested with
[`builder_has_option`]. If you need to pass additional data, then the
`=<variable>` format specifies an environment variable where the additional data
will be stored. When using this format, it is good to use [`builder_has_option`]
to test for the presence of the parameter before attempting to use the variable.

**Note:** although the definition uses `=` to define the variable, when invoking
script, the value should be passed in as a separate parameter.

There is one standard option: `--debug`. You should not include `--debug` in the
`builder_describe` call, as it is always available. See
[`builder_is_debug_build`] for more details.

Note that you should not include any of the [standard builder parameters] here.

**Dependencies** are defined with a `@` prefix, for example:

```bash
builder_describe "Sample script" \
  "@/core configure build" \
  configure \
  build
```

A dependency always starts with `@`. The path to the dependency will be relative
to the build script folder if the path does not start with `/`.  Otherwise, the path
to the dependency is interpreted relative to the root of the repository. It is an
error to specify a dependency outside the repo root.

A dependency definition can include a target for that dependency, for example,
`"@/core:arch"`. This would build only the ':arch' target for the core module.

Relative paths will be expanded to full paths, again, relative to the root of
the repository.

Dependencies may be limited to specific `action:target` pairs on the current
script. If not specified, dependencies will be built for all actions on all
targets. Either `action` or `:target` may be omitted, and multiple actions and
targets may be specified, space separated.

--------------------------------------------------------------------------------

## `builder_describe_internal_dependency` function

Define a local dependency between one action:target and another.

### Usage

```bash
builder_describe_internal_dependency action:target depaction:deptarget ...
```

### Parameters
  * **action:target**:        The action and target that has a dependency
  * **depaction:deptarget**:  The dependency action and target

### Example

```bash
builder_describe_internal_dependency \
  build:mac build:mac-x86_64 \
  build:mac build:mac-arm64
```

**Note:** actions and targets must be fully specified, and this _must_ be called
before either [`builder_describe_outputs`] or [`builder_parse`] in order for
dependencies to be resolved.

--------------------------------------------------------------------------------

## `builder_describe_outputs` function

Defines an output file or folder expected to be present after successful
completion of an action for a target. Used to skip actions for dependency
builds. If `:target` is not provided, assumes `:project`.

Relative paths are relative to script folder; absolute paths are relative to
repository root, not filesystem root.

### Usage

```bash
  builder_describe_outputs action:target filename [...]
```

### Parameters

* 1: `action[:target]`   action and/or target associated with file
* 2: `filename`          name of file or folder to check
* 3+: ... repeat previous arguments for additional outputs

### Example

```bash
  builder_describe_outputs \
    "configure" "/node_modules" \
    "build"     "build/index.js"
```

--------------------------------------------------------------------------------

## `builder_display_usage` function

Prints the help for the script, constructed from the [`builder_describe`]
parameters, so must be called after [`builder_describe`].

### Usage

```bash
builder_describe "sample" clean build test
builder_display_usage
```

--------------------------------------------------------------------------------

## `builder_echo` function

Wraps the `echo` command with color and a script identifier prefix.

### Usage

```bash
builder_echo [mode] message
```

### Parameters

Note: if only a single parameter passed, it will be the **message** parameter, and
mode will be `white`.

* **mode**: one of the following modes:
  * `success`: A message indicating success, represented with green text
  * `heading`: A heading, represented with blue text
  * `warning`: A warning message, represented with yellow text
  * `error`: An error message, represented with red text (consider [`builder_die`])
  * `debug`: A debug string, represented with teal text (consider [`builder_echo_debug`])

  Or color identifiers:
  * `white`: Normal white text, the default if **mode** is omitted
  * `grey`: Darker grey text
  * `green`: Equivalent to `success`
  * `blue`: Equivalent to `heading`
  * `yellow`: Equivalent to `warning`
  * `red`: Equivalent to `error`
  * `purple`: Purple text, generally reserved by Builder for `setmark` section
    headings
  * `brightwhite`: Bright white text, generally reserved by Builder for
    delineating current script messages
  * `teal`: Teal text, equivalent to `debug`, generally reserved for debugging
    messages

  The following modes are used mostly by Builder internally:
  * `setmark`: A marker for a section heading, represented with purple text

* **message**: a string (surround with quote marks)

### Description

The `builder_echo` command will emit a string, with the current script
identifier  at the start, optionally with color formatting (as long as the terminal
supports color).

```bash
builder_echo "this went well"
builder_echo error "this didn't go so well"
```

```
[this/script/identifier] this went well
[this/script/identifier] this didn't go so well
```

(Red text cannot be represented here!)

The current script identifier will be grey for dependency builds and bright
white for top-level builds and child builds.

--------------------------------------------------------------------------------

## `builder_echo_debug` function

Wraps the [`builder_echo`] command with debug mode and a `[DEBUG]` prefix.

### Usage

```bash
builder_echo_debug message
```

### Parameters

* **message**: The message to emit to the console

### Description

This function is used internally within Builder, but can also be used by
any builder scripts as required.

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

## `builder_is_debug_build` function

Returns `true` (aka 0) if the `--debug` standard option was passed in. This
should be used instead of `builder_has_option --debug`.

### Usage

```bash
if builder_is_debug_build; then
  ... # e.g. CONFIG=debug
fi
```

### Description

The `--debug` standard option is currently handled differently to other options.
It should never be declared in [`builder_describe`], because it is always
available anyway.

`--debug` is automatically passed to child scripts and dependency scripts.

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

## `builder_run_action` function

Wraps [`builder_start_action'] and [`builder_finish`] commands in a shorthand
style for single-command actions. Can be used together with a local function for
multi-command actions. Do be aware that this pseudo-closure style cannot be
mixed with operators such as `<`, `>`, `&&`, `;`, `()` and so on.

### Usage

```bash
  builder_run_action action[:target] command [command-params...]
```

### Parameters

* 1: `action[:target]`   name of action, and optionally also target, if target
                         excluded starts for all defined targets
* 2: command             command to run if action is started
* 3...: command-params   parameters for command

### Example

The following example shows a sensible pattern to use when you have a single
multi-command action and other single-command actions. If most of your actions
are multi-command, you may choose to use this approach, or stick with the
longhand form.

```bash
  function do_build() {
    mkdir -p build/cjs-src
    npm run build
  }

  builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_run_action configure    verify_npm_setup
  builder_run_action build        do_build
```

--------------------------------------------------------------------------------

## `builder_run_child_actions` function

Executes the specified actions on or all child targets, or on the specified
targets. A child target is any target which has a sub-folder of the same name as
the target. Like [`builder_start_action`], the actions will only actually be run
if they have been specified by the user on the command-line.

The child script will be called with the applicable action, for all targets. No
options apart from standard builder options are passed through.

### Usage

```bash
builder_run_child_actions action1 [...]
```

### Parameters

  1...: action[:target]   name of action:target to run

### Example

```bash
builder_run_child_actions configure build test install
```

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

## `builder_term` function

Emits the parameters passed to the function, wrapped with the helper function
`builder_term`, which wraps the passed string with `$BUILDER_TERM_START` and
`$BUILDER_TERM_END`, e.g.: `$(builder_term text)`.

### Usage

```bash
builder_describe "sample" \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports"
```

--------------------------------------------------------------------------------

## `builder_trim` function

Trims leading and following whitespace from the input parameters.

### Usage

```bash
  my_string="$(builder_trim "$my_string")"
```

### Parameters

* `my_string`    An input string

--------------------------------------------------------------------------------

## `builder_use_color` function

This will normally be managed internally by build-utils, but can be manually
overridden with:

```bash
builder_use_color true
# or
builder_use_color false
```

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

Note: it is often cleaner to use [`builder_echo`] than to use these variables directly.

Note: it is recommended that you use `$(builder_term text)` instead of
`${BUILDER_TERM_START}text${BUILDER_TERM_END}`.

[standard builder parameters]: #standard-builder-parameters
[`builder_describe`]: #builderdescribe-function
[`builder_describe_outputs`]: #builderdescribeoutputs-function
[`builder_describe_internal_dependency`]: #builderdescribeinternaldependency-function
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
[`builder_run_action`]: #builderrunaction-function
[`builder_run_child_actions`]: #builderrunchildactions-function
[`builder_echo`]: #builderecho-function
[`builder_die`]: #builderdie-function
[`builder_echo_debug`]: #builderechodebug-function
[`builder_is_debug_build`]: #builderisdebugbuild-function