---
title: LDML Keyboard Debug Window
---

The LDML keyboard debug window is shown at the bottom of an LDML keyboard editor
when testing the keyboard. In version 17.0, the LDML keyboard debug window is
a simple test window, without support for interactive debugging, unlike the
Keyman keyboard debug window.

## Debugger input window

![Debug window - Debug State](../images/ui/frmDebug.png)

The debugger input window is used for typing input to test the keyboard.
In the top half of this window, input you type while testing your
keyboard will be displayed, exactly the same as in use, with one
exception: deadkeys will be shown visually with an
![OBJ](../images//ui/obj.gif) symbol.

The lower half of the window shows a grid of the characters to the virtual left
of the insertion point, or the selected characters if you make a selection.
Markers will be identified in the grid, but only by numeric value in version
17.0. The grid will show characters in right-to-left scripts in backing store
order, from left to right. If there are more characters in your text than can
fit on the screen, then only those that fit will be shown in the grid.