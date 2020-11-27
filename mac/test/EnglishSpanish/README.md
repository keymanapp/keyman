# EnglishSpanish test keyboard

This keyboard is used only to test the crash reporting.

1. In Keyman, turn on verbose logging.
2. Optionally, in Console filter all messages from the **Keyman** process having the text **Sentry**
3. Select a keyboard called **EnglishSpanish** (see /mac/test/EnglishSpanish folder in the source repo)
4. Open an editable file in Xcode.
5. Type **Sentry force now**
   - With each keystroke, Console messages should indicate that the character is being added to the Easter egg string.
   - When the final **w** is typed, a Console message should indicate: **Forcing crash now!**
   - After some delay (you may need to switch Keyman off and on again in order for the report to upload), the new crash should appear in the console on sentry.keyman.com.