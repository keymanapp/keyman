---
title: KMManager.createKeyboardHeightString()
---

## Summary
The **createKeyboardHeightString()** method creates a formatted string showing keyboard height percentages for both portrait and landscape orientations.

## Syntax
```java
KMManager.createKeyboardHeightString(Context context, String portraitLabel, String landscapeLabel)
```
or

```java
KMManager.createKeyboardHeightString(Context context, int liveHeight, int liveOrientation, String portraitLabel, String landscapeLabel)
```

### Parameters

`context`
: The context

`portraitLabel`
: Label for portrait orientation (e.g., "Portrait")

`landscapeLabel`
: Label for landscape orientation (e.g., "Landscape")

`liveHeight` _(Optional)_
: The current keyboard height in pixels (not yet saved to SharedPreferences). Used for displaying real-time percentages during keyboard height adjustment.

`liveOrientation` _(Optional)_
: The orientation for which `liveHeight` applies (Configuration.ORIENTATION_PORTRAIT or Configuration.ORIENTATION_LANDSCAPE)

### Returns
Returns a `String` in the format "100% Portrait | 100% Landscape"

## Description
Use this method to display the keyboard height percentages for both orientations. The first variant reads both values from saved preferences, while the second variant allows you to provide a live (unsaved) height for one orientation, useful during interactive height adjustment.

The percentage is calculated using Math.ceil() to ensure consistency with `getKeyboardHeightPercentage()`.

## Examples

### Example 1: Display saved percentages
The following script illustrates displaying saved keyboard heights:

```java
    // Display the saved keyboard height percentages
    String heightText = KMManager.createKeyboardHeightString(
        this,
        "Portrait",
        "Landscape"
    );
    // Result: "100% Portrait | 100% Landscape"
    textView.setText(heightText);
```

### Example 2: Display live percentage during drag
The following script shows how to display real-time percentages while the user adjusts keyboard height:

```java
    // User is dragging to adjust portrait height
    int currentOrientation = KMManager.getOrientation(this);
    String display = KMManager.createKeyboardHeightString(
        this,
        currentHeight,  // Live height being adjusted
        currentOrientation,
        "Portrait",
        "Landscape"
    );
    // Result: "145% Portrait | 100% Landscape"
    percentageDisplay.setText(display);
```

## See also
* [getKeyboardHeightPercentage()](getKeyboardHeightPercentage)
* [applyKeyboardHeight()](applyKeyboardHeight)
* [calculateKeyboardHeightFromTouch()](calculateKeyboardHeightFromTouch)
