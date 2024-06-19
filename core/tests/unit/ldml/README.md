# LDML unit tests

This directory contains unit tests that test the LDML keyboard processing.

The driver (`ldml.cpp`) loads and executes the tests. Each test consist of a
Keyman keyboard source file (`*.xml`) and the compiled KMXPlus keyboard.
The source file contains rules and defines the setup, input and expected results.

## Defining tests in the `.xml` file

Somewhere near the top of the file, an XML comment contains lines that
define the setup, input and expected results.

### Setup

- **description**: this is just for commenting what we test here

```text
@@description: Tests Caps Lock env set
```

- **capsLock**: allows to set the caps lock state. The example below turns
  caps-lock on at the start of the test. `[K_CAPS]` in the **keys** comment
  allows toggling of caps lock.

```text
@@capsLock: 1
```

- **context**: allows to setup a string which serves as context

```text
@@context:
```

### Input

- **keys**: defines one or more virtual keys that will be processed one after the
  other

```text
@@keys: [K_1][K_CAPS][K_2][SHIFT K_3][K_4][K_CAPS][K_5][K_CAPS][K_6]
```

### Expected Result

- **expected**: the resulting string of processing the input keys. In the example
  below each key press is expected to output the string `'pass.'`.

```text
@@expected: pass.pass.pass.pass.pass.pass.
```

## Running the tests

All tests can be run at once with `./build.sh --debug tests`.

Alternatively it's possible to run a single test with:

```bash
cd core
build/arch/debug/tests/unit/ldml/ldml 'tests/unit/ldml/000_test_zero.xml' 'tests/unit/ldml/000_test_zero.kmx'
```
