We have identified a [problem in PowerPoint](https://github.com/keymanapp/keyman/issues/687) that negatively affects its
compatibility with input methods that add custom menu items to the input menu (e.g., [NumberInput](https://developer.apple.com/library/content/samplecode/NumberInput_IMKit_Sample/Introduction/Intro.html), a sample IM developed by Apple).

This is of particular concern to us because we have developed [Keyman](keyman.com), a very popular and widely used input method that supports custom keyboards in hundreds of languages, including many major languages whose complex scripts are best served by the powerful options in Keyman as well as minority languages whose only viable option for text input might be Keyman.

We have prepared a small sample app that easily illustrates the problem.

# Installation and Setup of TestInput sample app

1. Unzip the **PowerPoint IM Incompatibilities Test.zip** file
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

Note: _If you need to type anything (e.g., to take notes) during the testing process, you will probably find it beneficial to switch back to your default system keyboard to do so, as **TestInput** does some funny things in response to several keys._

# Steps to Illustrate Compatibility Problem in PowerPoint

1. Activate PowerPoint.
2. Ensure that your system volume is turned up load enough for you to be able to hear system sounds.
3. _Optionally_, start Console and filter to show all messages from the **TestInput** process.
4. With **TestInput** selected as the active input source, drop down the input menu.
5. Select **Static Top-level Menu**. Nothing happens. _The expected result is that the "Hero" sound should be played and **Menu clicked - tag: 1** should be output to the Console log._
6. Drop down the input menu again, point to **Static Drop-down Menu**, and then click **Static Submenu**. Nothing happens. _The expected result is that the "Glass" sound should be played and **Menu clicked - tag: 10** should be output to the Console log._
7. Drop down the input menu again, point to **Dynamic Drop-down Menu**, and then click **Submenu One**. Nothing happens. _The expected result is that the "Frog" sound should be played and **Menu clicked - tag: 100** should be output to the Console log._
8. For reference, you can repeat steps 4-7 in another program to observe the expected behavior. You can also test in other contexts within PowerPoint (e.g., with a slide selected or with the insertion point in the font selection control in the formatting toolbar) to see that it behaves properly there.

Note that our testing was done with PowerPoint version 16.13 (180513).
