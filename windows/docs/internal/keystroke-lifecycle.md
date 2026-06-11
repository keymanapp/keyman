

# Introduction
The three main parts that make up the lifecycle of a keystroke on the Windows platform.

1. Capture Key Stroke
2. Keystroke Processing against Keyboard rules
3. Apply Action Result

1 and 3 are processed in the Keyman Engine integrated to the Windows operating system. Where as the Keyman Core is the same core logic library used across all desktop platforms and soon to be web and mobile devices.



## Capture Keystroke
There are two main mechanism for capture the keystrokes one is the hook GetMessageProc the other is TIPTextService. There is another Hook LowLevelKeyboardProc used for the serial event server but it is out of scope for this document.

### kmnGetMessageProc
Not all key presses make it to the TSF TIP, especially modifiers. Therefore this hook ensures the Keyman engine receives all keystrokes. The relevant messages are  ((mp->message == WM_KEYDOWN || mp->message == WM_SYSKEYDOWN || mp->message == WM_KEYUP || mp->message == WM_SYSKEYUP)).

### CKMTipTextService

This is TextServicesFramework input method. This is where the bulk of the keyboard processing occurs.

Once the modifier state and the keystroke has been determined. The keystroke and modifier state is passed to Keyman core.

## Keyman Core - Keystroke Processing against Keyboard rules

The Keyman Core will process the keystroke with the current context against the keyboard rules and produce the output actions.

The Keyman Core contains Keyboard processor class in which instances implement the KMX, LDML protocol specification
The core allows for Keyboard layouts. The Keyboard layouts contain rules for interprets keystrokes,
determining the effects that should arise when evaluating the incoming keystroke with the
current context state.

The output of the Keyman Core will be a actions object with the required actions the Keyman Engine needs to apply to
the current Windows app.

## Apply Action Result
The actions that could be required are the following
Delete characters
Output characters
Emit Keystroke
Sound Bell
Invalidate Cache??

The Keyman Engine will then use the Windows api to apply the actions required.


The sequence diagram gives an example of a keystroke.


Note on context aware versus non complaint see for more details in this Link to previous section.
The difference for the sake of this explanation is that with Compliant apps the context can be sent and verified by the Keyman core with each keystroke. With non compliant apps the Keyman Core will keep track internally of the context. It will be invalidated as explained in linke to kb


## Glossary

Modifer Key: Caps, Shift, Alt, Ctrl


**Serial event server and Keystroke processing note.**
There is seralized event queue described in this blog post https://blog.keyman.com/2018/10/the-keyman-keyboard-input-pipeline/ It can be considered distinct part the rest of the key press life cycle. Infact the it can be turned on and off via feature flag. This explanation of the lifecycle talks as if the keystrokes come in order.
