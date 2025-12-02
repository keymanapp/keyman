---
title: KMManager.calculateKeyboardHeightFromTouch()
---

## Summary
The **calculateKeyboardHeightFromTouch()** method calculates the keyboard height from a touch Y coordinate, automatically applying minimum and maximum bounds.

## Syntax
```java
KMManager.calculateKeyboardHeightFromTouch(Context context, int touchY, int viewBottom)
```

### Parameters

`context`
: The context

`touchY`
: The Y coordinate of the touch event in screen coordinates (from event.getRawY())

`viewBottom`
: The bottom Y coordinate of the keyboard view in screen coordinates (typically: view.getLocationOnScreen()[1] + view.getHeight())

### Returns
Returns an `int` representing the calculated keyboard height in pixels, clamped to the valid range defined by `getKeyboardHeightMin()` and `getKeyboardHeightMax()`.

## Description
Use this method during interactive keyboard height adjustment (e.g., when implementing a draggable resize handle). The height is calculated as the distance from the touch point to the bottom of the screen, then automatically clamped to ensure it stays within valid bounds.

This method encapsulates the calculation logic and bounds checking, ensuring consistency across different implementations of keyboard height adjustment UI.

## Examples

### Example: Using `calculateKeyboardHeightFromTouch()` in a touch listener
The following script illustrates implementing a draggable keyboard resize handle:

```java
    View.OnTouchListener touchListener = new View.OnTouchListener() {
        @Override
        public boolean onTouch(View view, MotionEvent event) {
            switch (event.getAction()) {
                case MotionEvent.ACTION_MOVE:
                    // Get the keyboard view's bottom position on screen
                    int[] location = new int[2];
                    keyboardView.getLocationOnScreen(location);
                    int viewBottom = location[1] + keyboardView.getHeight();
                    int touchY = (int) event.getRawY();

                    // Calculate new height with automatic bounds checking
                    int newHeight = KMManager.calculateKeyboardHeightFromTouch(
                        context, touchY, viewBottom);
                    // newHeight is guaranteed to be within min/max bounds

                    // Update the keyboard preview
                    updateKeyboardPreview(newHeight);
                    break;
                case MotionEvent.ACTION_UP:
                    // Save the final height
                    KMManager.applyKeyboardHeight(context, newHeight);
                    break;
            }
            return true;
        }
    };

    // Attach listener to resize handle or keyboard view
    resizeHandle.setOnTouchListener(touchListener);
```

## See also
* [applyKeyboardHeight()](applyKeyboardHeight)
* [getKeyboardHeightMin()](getKeyboardHeightMin)
* [getKeyboardHeightMax()](getKeyboardHeightMax)
* [createKeyboardHeightString()](createKeyboardHeightString)
