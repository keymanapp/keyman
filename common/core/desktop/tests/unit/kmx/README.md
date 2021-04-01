# KMX unit tests

This directory contains unit tests that test the KMX processing.

The driver (`kmx.cpp`) loads and executes the tests. Each test consist of a
Keyman keyboard source file (`0*.kmn`) and the compiled KMX keyboard.
The source file contains rules and defines the setup, input and expected results.

## Defining tests in the `.kmn` file

The top of the file contains comments that define the setup, input and expected results.
Following the comments are the normal rules of a Keyman keyboard.

### Setup

- **description**: this is just for commenting what we test here

```text
c Description: Tests Caps Lock env set
```

- **option**: allows to put the system in a defined state. The example below turns
  caps-lock off.

```text
c option: &capsLock=0
```

- **context**: allows to setup a string which serves as context

```text
c context:
```

### Input

- **keys**: defines one or more virtual keys that will be processed one after the
  other

```text
c keys: [K_1][K_CAPS][K_2][SHIFT K_3][K_4][K_CAPS][K_5][K_CAPS][K_6]
```

### Expected Result

- **expected**: the resulting string of processing the input keys. In the example
  below each key press is expected to output the string `'pass.'`.

```text
c expected: pass.pass.pass.pass.pass.pass.
```

## Running the tests

All tests can be run at once with `meson test`.

Alternatively it's possible to run a single test with:

```bash
cd common/core/desktop
build/tests/unit/kmx/kmx 'tests/unit/kmx/038 - punctkeys.kmn' 'tests/unit/kmx/038 - punctkeys.kmx'
```
