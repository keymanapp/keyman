There are four [problems we have identified in InDesign](https://github.com/keymanapp/keyman/issues/782) that negatively affect its
compatibility with input methods that use Apple's standard approach using InputMethodKit.

This is of particular concern to us because we have developed [Keyman](keyman.com), a very popular and
widely used input method that supports custom keyboards in hundreds of languages,
including many major languages whose complex scripts are best served by the powerful
options in Keyman as well as minority languages whose only viable option for text input
might be Keyman.

We have prepared a small sample app that easily illustrates these incompatibilities.
Like Keyman, the sample app uses the _third_ approach for receiving and processing text
events as described [here](https://developer.apple.com/documentation/inputmethodkit/imkserverinput?language=objc).

Note that our testing was done with InDesign CC 2018 version 13.1.

For each of the four problems, follow the testing steps described below to see the problem.

# Installation and Setup of TestInput sample app

1. Unzip the **InDesign IM Incompatibilities Test.zip** file
2. Copy TestInput.app into **~/Library/Input Methods**
3. Open **System Preferences**.
4. Open **Keyboard**.
5. Click the **Input Sources** tab.
6. Click **+** to add an input source.
7. In the list of languages, choose **Multiple Languages**.
8. In the righthand pane, you should see **TestInput** (its icon is a **9**). Select it. 
9. Click **Add**.
10. Ensure that the checkbox **Show input menu in menu bar** is selected.
11. Now the menu bar should include an icon for the input menu. If U.S. English is your default system input method, the icon will be an American flag.
12. Click the input menu icon to drop down that menu and confirm that **TestInput** appears as a choice. Select it.
13. The icon for the input menu should change to a **9**. Drop the menu again to confirm that there is now a new section in the menu called **TestInput**, which has three items.
14. Now you are ready to go on the next section to perform the steps to see the problems.

Note: _You can leave **TestInput** selected as the input source during the entire process, but if you need to type anything (e.g., to take notes) during the process, you will probably find it beneficial to switch back to your default system keyboard to do so, as **TestInput** does some funny things in response to several keys._

# Steps to Illustrate Compatibility Problems in InDesign

## 1. Actions associated with Submenus on the input menu do not fire

Steps to reproduce:

1. Activate InDesign.
2. For this test, it doesn't seem to matter what the context is in InDesign. We have tested on a blank page, in a text box, the **Get Started** welcome screen, and with the insertion point in a text box on the formatting toolbar.
3. Ensure that your system volume is turned up load enough for you to be able to hear system sounds.
4. _Optionally_, start Console and filter to show all messages from the **TestInput** process.
5. With **TestInput** selected as the active input source, drop down the input menu.
6. Select **Static Top-level Menu**. You should hear the _"Hero"_ sound. In the Console log, **Menu clicked - tag: 1** should appear.
7. Drop down the input menu again, point to **Static Drop-down Menu**, and then click **Static Submenu**. Nothing happens. _The expected result is that the "Glass" sound should be played and **Menu clicked - tag: 10** should be output to the Console log._
8. Drop down the input menu again, point to **Dynamic Drop-down Menu**, and then click **Submenu One**. Nothing happens. _The expected result is that the "Frog" sound should be played and **Menu clicked - tag: 100** should be output to the Console log._
9. For reference, you can repeat steps 5-8 with another program (e.g. TextEdit) active to observe the expected behavior.

## 2. InDesign text boxes only insert a single character in response to the insertText:replacementRange: method call.

Steps to reproduce:

1. Activate InDesign.
2. _Optionally_, in Console filter to show all messages from the **TestInput** process and containing the text _Attempting_.
3. In a text box, press the **~** key on the keyboard. A _t_ will be inserted. In the Console log, **Attempting to insert characters 'tilde' using insertText with undefined (default) replacement range.** should appear. _The expected result is that the entire word **tilde** should be inserted._
4. For reference, perform step 3 in another program or even in the Font Name control on the formatting toolbar in InDesign to observe the expected behavior.

## 3. InDesign text boxes do not provide correct implementations for some important methods in the IMKTextInput and/or NSTextInputClient protocols.

Note that although IMKTextInput is not properly documented on developer.apple.com, it is the (informal) protocol that the object returned from the IMKInputController **client** method is documented to implement. Descriptions of the protocol methods are contained in the public header file IMKInputSession.h. The NSTextInputClient protocol is a formal protocol that partially overlaps IMKTextInput. It is a replacement for the NSTextInput protocol (slated for deprecation), which is largely the same as IMKTextInput. In any case, for input methods such as Keyman to work well, these methods should be implemented properly:

* selectedRange
* attributedSubstringFromRange: (alternatively, attributedSubstringForProposedRange:actualRange:)
* insertText:replacementRange: (InDesign's implementation appears to ignore the range parameter.)

Steps to reproduce:

1. Activate InDesign.
2. _Optionally_, in Console filter to show all messages from the **TestInput** process.
3. In a text box, type **abc**
4. Press the **$** key on the keyboard. A _#_ will be appended to the existing text, giving **abc#**. _The expected result is that the final **c** should be deleted, leaving **ab**._
5. For reference, perform steps 2-3 in another program or even in the Font Name control on the formatting toolbar in InDesign to observe the expected behavior.

For the above test, the Console log will include several lines of diagnostic information. In the failure case, it will look something like the following:

	selectedRange.location not found
	selectedRange.length not found
	client length: 2147483647
	buffer = ""
	Attempting to delete the previous character by replacing it and the prior character with just the prior one. location = 9223372036854775807
	Operation might fail - client reports possible invalid location.
	attributedSubstringFromRange did not return a previous character. Trying attributedSubstringForProposedRange...
	Client does not respond to attributedSubstringForProposedRange:actualRange:. Using '#' instead

In the success case, it will look something like the following:

selectedRange.location = 2
selectedRange.length = 0
client length: 2
buffer = "ab"

## 4. InDesign text boxes appear to refresh or reset their interaction with the Input Method kit or operating system such that activateServer and deactiviateServer get called unnecessarily.

Keyman has a "legacy" mode it can use when working with some programs that fail to implement some of the aforementioned protocol methods properly. We tried using that approach with InDesign, but this exposed this final defect. If the previous issue is properly dealt with, then this problem probably won't matter to Keyman. Still, it does seem to be a bug and it could adversely affect other input methods.

Steps to reproduce:

1. Activate InDesign.
2. _Optionally_, in Console filter to show all messages from the **TestInput** process.
3. In a text box, type **abc**
4. Press the **9** key on the keyboard. The _bc_ will be deleted, leaving just **a**. _The expected result is that the **bc** should be replaced with **xyz*, leaving **axyz**._
5. For reference, perform steps 2-3 in another program or even in the Font Name control on the formatting toolbar in InDesign to observe the expected behavior.

For the above test, the Console log will include several lines of diagnostic information. In the failure case, it will look something like the following:

	Attempting to simulate delete (back) twice - followed later by a 'xyz'
	Operation might fail - client reports possible invalid location.
	*** deactivateServer ***
	sender: ...
	*** activateServer ***
	sender: ...
	*** deactivateServer ***
	sender: ...
	...

In the success case, it will look something like the following:

	Attempting to simulate delete (back) twice - followed later by a 'xyz'
	NSEvent: type=KeyDown loc=(0,0) time=38336.0 flags=0 win=0x0 winNum=0 ctxt=0x0 chars="\^H" unmodchars="\^H" repeat=0 keyCode=51
	*** logContext ***
	client: ...
	selectedRange.location = 3
	selectedRange.length = 0
	client length: 3
	buffer = "abc"
	Processing a delete.
	NSEvent: type=KeyDown loc=(0,0) time=38336.0 flags=0 win=0x0 winNum=0 ctxt=0x0 chars="\^H" unmodchars="\^H" repeat=0 keyCode=51
	*** logContext ***
	client: ...
	selectedRange.location = 2
	selectedRange.length = 0
	client length: 2
	buffer = "ab"
	Processing a delete.
	NSEvent: type=KeyDown loc=(0,0) time=38336.1 flags=0 win=0x0 winNum=0 ctxt=0x0 chars="" unmodchars="" repeat=0 keyCode=255
	*** logContext ***
	client: ...
	selectedRange.location = 1
	selectedRange.length = 0
	client length: 1
	buffer = "a"
	Processing the special 0xFF
	inserting text from composed buffer: xyz
	...
	selectedRange.location not found
	selectedRange.length = 0
	client length: 4
	*** logContext ***
	client: ...
	selectedRange.location = 4
	selectedRange.length = 0
	client length: 4
	buffer = "axyz"
	...
