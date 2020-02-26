ibus-keyman tests
=================

Setup
-----

You need pynput to generate keypresses

`pip3 install pynput`

These tests assume that the test kmx and kmn are located in
`~/.local/share/keyman/test_kmx`

You may like to link `common/core/desktop/tests/unit/kmx` to that location


Running tests
-------------

To run all the tests

`./runtests.sh`

To run an individual test <testname>

`python3 ./test_ibus_keyman.py "<testname>"`

If your keyboard isn't "us" then you need to set the keyboard before running the test

`setxkbmap us`

You may like to `setxkbmap -query|grep layout` before you run the test to find
out what to set it back to afterwards.

Test output
-----------

The tests produce `<testname>.in` and `<testname>.out` files
`<testname>.in` is the expected output
`<testname>.out` is the actual output

You may use `diff` or something else to compare them

