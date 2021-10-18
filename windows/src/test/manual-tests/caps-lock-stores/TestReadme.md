## Caps Lock
The test cases below expect the usage of the capslock.kmn keyboard in this directory. That keyboard outputs _pass_ or _fail_ if following the test cases.

### Prerequisites before each test

- System keyboard layout is **en-US**
- Install a keyboard that doesn't use any of the caps lock stores, e.g. `capslock.kmp`.
- CapsLock is currently **on**
- Currently active keyboard is the **capslock.kmp** keyboard

### Test cases

- **TEST_CAPSLOCK-1**: uppercase with virtual key
  - press and release `a`

  Expected result:
  - `pass.` (with other keyboards uppercase `A`)

- **TEST_CAPSLOCK-2**: lowercase with virtual key
  - press and hold 'Shift'
  - press and release `b`
  - release `Shift`

  Expected result:
  - `pass.` (with other keyboards lowercase `b`)

- **TEST_CAPSLOCK-3**: capslock ignored for numbers
  - press and hold 'Shift'
  - press and release `3`
  - release `Shift`

  Expected result:
  - `pass.` (with other keyboards `#`)

- **TEST_CAPSLOCK-4**: uppercase
  - press and release `c`

  Expected result:
  - `pass.` (with other keyboards uppercase `C`)

- **TEST_CAPSLOCK-5**: lowercase
  - press and hold 'Shift'
  - press and release `d`
  - release `Shift`

  Expected result:
  - `pass.` (with other keyboards lowercase `d`)

## CapsAlwaysOff

For these tests, use a keyboard with the `CapsAlwaysOff` store set. We call this keyboard _capsalwaysoff_ below.

Any keyboard with that store set will work; if you don't have one at hand you can use the caps_always_off.kmn keyboard. The *caps_always_off.kmp* keyboard will prevent switching caps lock on. As a sanity check to verify that Keyman is actually active, pressing the key `a` will output `ncaps_little_a`, and `Shift+a` will output `ncaps_shift_A`.

**Note:** When testing in a virtual machine, use an on-screen keyboard (in VirtualBox: Input/Keyboard/Soft Keyboard) and observe the caps lock indicator of the on-screen keyboard. Using the hardware keyboard might show side effects with caps lock.

### Prerequisites before each test

- Install a keyboard that has `CapsAlwaysOff` store set, e.g. `caps_always_off.kmp`.
- CapsLock is currently off
- Currently active keyboard is a non-Keyman keyboard

### Test cases

- **TEST_CAPSOFF-1**: sanity check
  - switch to capsalwaysoff keyboard
  - press and release `a`

  Expected result:
  - output: `ncaps_little_a`

- **TEST_CAPSOFF-2**: caps lock stays off
  - switch to capsalwaysoff keyboard
  - press and release `CapsLock` key
  - press and release `a`

  Expected result:
  - caps lock indicator stays turned off
  - output: `ncaps_little_a`

- **TEST_CAPSOFF-3**: no caps lock while holding capslock key
  - switch to capsalwaysoff keyboard
  - press and hold `CapsLock` key
  - press and release `a`
  - release `CapsLock` key

  Expected result:
  - output: `ncaps_little_a`

- **TEST_CAPSOFF-4**: no caps lock while holding capslock key
  - switch to capsalwaysoff keyboard
  - press and hold `CapsLock` key
  - press and hold `Shift` key
  - press and release `a`
  - release `CapsLock` and `Shift` keys

  Expected result:
  - output: `ncaps_shift_A`

- **TEST_CAPSOFF-5**: switching turns off caps lock
  - turn on caps lock
  - switch to capsalwaysoff keyboard
  - press and release `a`

  Expected result:
  - caps lock indicator turned off
  - output: `ncaps_little_a`
**Note:** Test caps off 5 using a soft keyboard provided in virtual box will fail to turn the cap lock indicator off. This because the soft keyboard does pre processing of the shift key and any key pressed and the shift key stroke does not get sent to the keyman engine.
## CapsOnOnly/ShiftFreesCaps

For these tests, use a keyboard with the `CapsOnOnly` and `ShiftFreesCaps` stores set. We call this keyboard _shift_frees_caps_ below.

Any keyboard with these stores set will work; if you don't have one at hand you can use the shift_frees_caps.kmp

The _shift_frees_caps.kmp_ keyboard will enable caps lock by pressing the `CapsLock` key, and will turn capslock off by pressing the `Shift` key. The keyboard outputs _pass_ or _fail_ if following the test cases.

**Note:** When testing in a virtual machine, use an on-screen keyboard (in VirtualBox: Input/Keyboard/Soft Keyboard) and observe the caps lock indicator of the on-screen keyboard. Using the hardware keyboard might show side effects with caps lock.

### Prerequisites before each test

- Install a keyboard that has the `CapsOnOnly` and `ShiftFreesCaps` stores set, e.g.
  `shift_frees_caps.kmp`.
- CapsLock is currently off
- Currently active keyboard is _shift_frees_caps_ keyboard

### Test cases

- **TEST_CAPSONLY-1**: no caps
  - press and release `1`

  Expected result:
  - output: `pass.`

- **TEST_CAPSONLY-2**: caps
  - press and release `CapsLock`
  - press and release `2`

  Expected result:
  - caps lock indicator turned on
  - output: `pass.`

- **TEST_CAPSONLY-3**: caps doesn't toggle
  - press and release `CapsLock`
  - press and release `CapsLock`
  - press and release `6`

  Expected result:
  - caps lock indicator turned on
  - output: `pass.`

- **TEST_CAPSONLY-4**: shift turns off
  - press and release `CapsLock`
  - press and hold `Shift`
  - press and release `3`
  - release `Shift`

  Expected result:
  - caps lock indicator turned off
  - output: `pass.`

- **TEST_CAPSONLY-5**: shift by itself turns off
  - press and release `CapsLock`
  - press and release `Shift`

  Expected result:
  - caps lock indicator turned off
  - (no output)
