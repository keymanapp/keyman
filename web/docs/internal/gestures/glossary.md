# GestureRecognizer terms

Consider the path taken by one finger during a potential touch-based gesture input.  Suppose a user holds a button or key down for a period of time to activate a menu, then selecting an item from that menu.  Note the phrasing there - there are two separate 'stages', or 'components', to that input:
1. The user holds the button down for a period of time, without moving the input location.
    - After a set amount of time passes, the UI's state shifts, displaying a new menu corresponding to the held location and button.
2. Once the menu appears, the user then moves their finger to select an item from the menu.
    - Once an item is selected and the finger is lifted, the program should respond and act accordingly to the selected menu item.

These two items correspond to **gesture components**, corresponding to _segments_ of the overall touch input path.  These components are related and happen in sequence, hence use of the term **gesture sequence**.

**Touchpoint** refers to the location of the cursor (during mouse-based input) or of the finger (during touch) being observed.