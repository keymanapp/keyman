---
title: Step 7: Testing the Keyboard
---

# Compiling the Keyboard

Before we can test the keyboard, we must compile it. Choose
Keyboard, Compile
Keyboard or press <kbd>F7</kbd> to compile the
keyboard. The Message window will display the results of the
compilation; if you have no typing errors, the keyboard should compile
successfully.

If there are any mistakes, an error message will be displayed in the
Message window, which will tell you the line on which the error
occurred.

## Compiling the Keyboard

After compiling the keyboard successfully, we can start testing it.
Choose Debug,
Start Debugging or press
<kbd>F5</kbd> to begin testing. The Keyboard debug window
will appear.

Now we test that all the rules operate as expected. To test the rules,
we type the keystrokes that will give us the output; for example, we can
type a quote <kbd>'</kbd> followed by one of
<kbd>A</kbd>, <kbd>E</kbd>,
<kbd>I</kbd>, <kbd>O</kbd>,
<kbd>U</kbd>, or <kbd>Y</kbd> to test the
uppercase acute-accented vowels. Similarly we can test the other
accents, and C-cedilla (`Ç`) and the angled quotes. If the rules are
correct, all this should work as we wanted.

Testing the rules in isolation like this will show if the rules are
correct or not, but won't show other possible errors that might occur in
everyday usage of the keyboard. For example, look at what happens if you
type the following quote:

> `'Alors Alice demande, "Où est mon chat ?"'`

As you can see, it comes out incorrectly as:

> `Álors Alice demande, Öù est mon chat ?"'`

The problem occurs when we have a word in quotes that begins with a
vowel: the keyboard will convert it to an accented vowel. We will need
to come up with a solution to this problem.

-   [Continue with Step 8: Deadkeys](step-8)
-   [Back to Step 6: Stores, `any()`, and `index()`](step-6)