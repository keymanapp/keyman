/*
 
 File:TestInputController.m
 
 Abstract: Test input controller class.
 
 Version: 1.0
 
 Copyright (C) 2007 Apple Inc. All Rights Reserved.
 
 */
#import "TestInputController.h"
#import "TestInputMethodAppDelegate.h"
#include <Carbon/Carbon.h> /* For kVK_ constants, and TIS functions. */

@implementation TestInputController

/*
 There are the three approaches to implementing an inut method:
 
 1.  Support keybinding.
 In this approach the system takes each keydown and trys to map the keydown to an action method that the input method has implemented.  If an action is found the system calls didCommandBySelector:client:.  If no action method is found inputText:client: is called.  An input method choosing this approach should implement
 -(BOOL)inputText:(NSString*)string client:(id)sender;
 -(BOOL)didCommandBySelector:(SEL)aSelector client:(id)sender;
 
 2. Receive all key events without the keybinding, but do "unpack" the relevant text data.
 Key events are broken down into the Unicodes, the key code that generated them, and modifier flags.  This data is then sent to the input method's inputText:key:modifiers:client: method.  For this approach implement:
 -(BOOL)inputText:(NSString*)string key:(NSInteger)keyCode modifiers:(NSUInteger)flags client:(id)sender;
 
 3. Receive events directly from the Text Services Manager as NSEvent objects.  For this approach implement:
 -(BOOL)handleEvent:(NSEvent*)event client:(id)sender;
 
 Approach #3 is the one used by Keyman, so that is what this TestInputController class also does.
 */

/*
 Testing procedure:
 SETUP STEPS
 -----------
 1. Clean/Build TestInput project
 2. If a previous version of TestInput is in ~/Library/Input Methods:
    a. Open Keyboard Preferences and click Input Methods tab. Remove TestInput if installed.
    b. Use Activity Monitor to kill the TestInput process if necessary.
 3. Open the location where TestInput was built and copy it to ~/Library/Input Methods.
 4. Add the TestInput input method (on Keyboard Preferences/Input Methods tab) and set it as the default IM for the system.
 4. Start Console.
 5. In the "Search" box on the toolbar, filter to show only messages from the TestInput process.
 6. To watch the messages as they appear in console, it will be helpful to arrange it to occupy one half of the screen,
    with the application or website filling the other half.
 
 TESTING STEPS
 -------------
 1. Activate the application or website you want to test (e.g. LibreOffice Vanilla, Google Docs, TextWrangler, search field in Facebook in Chrome, etc.)
 2. Type "abc"
    Verify that the typed characters are displayed.
 3. Type "1"
    Verify that the letter c has been replaced by the number 1
 4. Type "2"
    Verify that the letter b has been replaced by the number 2
 5. Type "3"
    Verify that the a21 has been replaced by 123 and that the middle digit (2) is selected.
 6. Press the Delete key
    Verify that the selected 2 is deleted and that the insertion point is left between the remaining digits: 1|3
 */

-(BOOL)handleEvent:(NSEvent*)event client:(id)sender;
{
    // Return YES to indicate the the key input was received and dealt with.  Key processing will not continue in that
    // case. In other words the system will not deliver a key down event to the application.
    // Returning NO means the original key down will be passed on to the client.
    NSLog(@"%@", event);
    
    if (event == nil || sender == nil)
        return NO;
    
//This doesn't seem to get fired reliably.
//    if (event.type == NSLeftMouseDown) {
//        [self performSelector:@selector(logContext:) withObject:sender afterDelay:0.25];
//        return NO;
//    }
    
    id inputClient = [self client];
    
    if (![inputClient supportsUnicode])
        return NO;
    
    NSUInteger location = [self logContext:inputClient];
    
    if ([[event characters] isEqual: @"1"])
    {
        NSLog(@"Attempting to replace previous character with a 1");
        
        if (location == 0 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        [inputClient setMarkedText:@"1" selectionRange:NSMakeRange(location - 1, 1) replacementRange:NSMakeRange(location - 1, 1)];
        [inputClient insertText:@"1" replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"2"])
    {
        NSLog(@"Attempting to replace previous two characters with 2c");
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        // This command causes a "beep" when executed in Terminal
        // [inputClient insertText:@"2" replacementRange:NSMakeRange(location - 2, 1)];
        // This approach works in TextWrangler, but in many other apps, it leaves the
        // 2c underlined, and then subsequent typing replaces the underlined text.
        [inputClient setMarkedText:@"2c" selectionRange:NSMakeRange(location - 2, 2) replacementRange:NSMakeRange(location - 2, 2)];
//         [self setComposedBuffer:@"2c"];
       // [inputClient insertText:@"2" replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"3"])
    {
        NSLog(@"Attempting to replace previous three characters with 123");
        
        if (location < 3 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        NSString *preChar = [_originalBuffer substringFromIndex:[_originalBuffer length] - 3];
        if (![preChar isEqual: @"abc"])
            NSLog(@"If exact testing protocol is followed, preceding characters should be \"abc\" instead of %@", preChar);
        
        // This works in TextEdit, but in TextWrangler, we get 123123. And in Chrome, it
        // just puts 123 out at the current insertion point.
        [inputClient setMarkedText:@"123" selectionRange:NSMakeRange(location - 3, 3) replacementRange:NSMakeRange(location - 3, 3)];
        [inputClient insertText:@"123" replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
    
        return YES;
    }
    
    if ([[event characters] isEqual: @"4"])
    {
        NSLog(@"Attempting to replace previous two characters with 2c");
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        // In TextWrangler, this approach results in the extra space being added before the 2c.
        [inputClient setMarkedText:@" " selectionRange:NSMakeRange(location - 2, 1) replacementRange:NSMakeRange(location - 2, 2)];
        [inputClient setMarkedText:@"2c" selectionRange:NSMakeRange(NSNotFound, NSNotFound) replacementRange:NSMakeRange(location - 1, 1)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"~"])
    {
        NSLog(@"Attempting to insert characters 'tilde' using insertText with undefined (default) replacement range.");
        
        [inputClient insertText:@"tilde" replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"$"])
    {
        NSLog(@"Attempting to delete the previous character by replacing it and the prior character with just the prior one. location = %lu", location);
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        // This works in TextEdit, but not in TextWrangler. In Chrome, it just tacks the prior character on
        // at the current insertion point.
        NSString *preChar = @"=";
        if ([inputClient respondsToSelector:@selector(attributedSubstringFromRange:)]) {
            preChar = [[inputClient attributedSubstringFromRange:NSMakeRange(location - 2, 1)] string];
            if (!preChar || [preChar isEqualToString:@""]) {
                NSLog(@"attributedSubstringFromRange did not return a previous character. Trying attributedSubstringForProposedRange...");
                if ([inputClient respondsToSelector:@selector(attributedSubstringForProposedRange:actualRange:)]) {
                    NSRange returnedRange;
                    preChar = [[inputClient attributedSubstringForProposedRange:NSMakeRange(location - 2, 1) actualRange:&returnedRange] string];
                    if (!preChar || [preChar isEqualToString:@""]) {
                        NSLog(@"Previous character could NOT be retrieved using attributedSubstringFromRange or attributedSubstringForProposedRange. Using '&' instead");
                        preChar = @"&";
                    }
                }
                else {
                    NSLog(@"Client does not respond to attributedSubstringForProposedRange:actualRange:. Using '#' instead");
                    preChar = @"#";
                }
            }
            else
                NSLog(@"Previous character retrieved = %@", preChar);
        }
        else
            NSLog(@"Client does not respond to attributedSubstringFromRange:. Using '=' instead");
        [inputClient insertText:preChar replacementRange:NSMakeRange(location - 2, 2)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"5"])
    {
        NSLog(@"Attempting to replace previous two characters with 2c");
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        [inputClient setMarkedText:@"2c" selectionRange:NSMakeRange(location - 2, 2) replacementRange:NSMakeRange(location - 2, 2)];
        [inputClient insertText:@"2c" replacementRange:NSMakeRange(location - 2, 2)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"6"])
    {
        NSLog(@"Attempting to replace previous two characters with 2c");
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        [inputClient setMarkedText:@"2c" selectionRange:NSMakeRange(location - 2, 2) replacementRange:NSMakeRange(location - 2, 2)];
        [inputClient insertText:@"2c" replacementRange:NSMakeRange(location, 0)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"7"])
    {
        NSLog(@"Attempting to replace previous two characters with 2c - This approach fails");
        
        if (location < 2 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        [inputClient setMarkedText:@"2c" selectionRange:NSMakeRange(location - 2, 2) replacementRange:NSMakeRange(location - 2, 2)];
        [inputClient insertText:@"" replacementRange:NSMakeRange(location, 0)];
        
        return YES;
    }
    
    if ([[event characters] isEqual: @"8"])
    {
        NSLog(@"Attempting to replace previous character with an 8");
        
        if (location == 0 || location == NSNotFound)
        {
            NSLog(@"Operation might fail - client reports possible invalid location.");
            location = 3;
        }
        
        [inputClient setMarkedText:@"8" selectionRange:NSMakeRange(location - 1, 1) replacementRange:NSMakeRange(location - 1, 1)];
        [inputClient insertText:@"8" replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
        
        return YES;
    }
    
    // This version tested and works in Atom and in Google Docs in Safari
    if ([[event characters] isEqual: @"9"])
    {
        // deleteBack
        NSLog(@"Attempting to simulate delete (back) twice - followed later by a 'xyz'");

        if (location < 1 || location == NSNotFound)
            NSLog(@"Operation might fail - client reports possible invalid location.");

        [self setComposedBuffer:@"xyz"];
        _numberOfDeletesExpected = 2;

        CGEventRef ev;
        CGEventSourceRef source = CGEventCreateSourceFromEvent([event CGEvent]);

        for (int i = 0; i < 2; i++)
        {
            //delete-back down
            ev = CGEventCreateKeyboardEvent (source, kVK_Delete, true);
            CGEventPost(kCGHIDEventTap, ev);
            CFRelease(ev);

            //delete-back up
            ev = CGEventCreateKeyboardEvent (source, kVK_Delete, false);
            CGEventPost(kCGHIDEventTap, ev);
            CFRelease(ev);
        }

        CFRelease(source);

        return YES;
    }
    
// This version works in Atom, but not in Google Docs in Safari. Rather than counting down the
// expected deletes and only posting the 0xFF after the deletes are all done, it posts them all
// up-front, but using the "source" created from the existing event. I don't know why that's
// enough to get them to post in the correct order (to the correct source) in Atom, but not in Safari.
//    if ([[event characters] isEqual: @"9"])
//    {
//        // deleteBack
//        NSLog(@"Attempting to simulate delete (back) twice - followed later by a 'xyz'");
//
//        if (location < 1 || location == NSNotFound)
//            NSLog(@"Operation might fail - client reports possible invalid location.");
//
//        [self setComposedBuffer:@"xyz"];
//        //_numberOfDeletesExpected = 2;
//
//        ProcessSerialNumber psn;
//        GetFrontProcess(&psn);
//
//        CGEventRef ev;
//        CGEventSourceRef source = CGEventCreateSourceFromEvent([event CGEvent]);
//
//        for (int i = 0; i < 2; i++)
//        {
//            //delete-back down
//            ev = CGEventCreateKeyboardEvent (source, kVK_Delete, true);
//            CGEventPostToPSN(&psn, ev);
//            CFRelease(ev);
//
//            //delete-back up
//            ev = CGEventCreateKeyboardEvent (source, kVK_Delete, false);
//            CGEventPostToPSN(&psn, ev);
//            CFRelease(ev);
//        }
//
//        ev = CGEventCreateKeyboardEvent(source, (CGKeyCode)0xFF, true);
//        CGEventPostToPSN(&psn, ev);
//        CFRelease(ev);
//        CFRelease(source);
//
//        return YES;
//    }
    
    if ([[event characters] isEqual: @"0"])
    {
        // deleteBack
        NSLog(@"Attempting to simulate a delete (back)");
        
        ProcessSerialNumber psn;
        GetFrontProcess(&psn);
        
        CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(NULL, kVK_Delete, true);
        CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(NULL, kVK_Delete, false);
        
        CGEventPostToPSN(&psn, keyDownEvent);
        CGEventPostToPSN(&psn, keyUpEvent);
        
        CFRelease(keyDownEvent);
        CFRelease(keyUpEvent);
        
        return YES;
        
//        // deleteBack
//        NSLog(@"Attempting to select previous 2 characters for replacement, followed by posting 0xFF to trigger insertion.");
//
//        if (location == 0 || location == NSNotFound)
//        {
//            NSLog(@"Operation might fail - client reports possible invalid location.");
//            location = 2;
//        }
//        else if (location < 2)
//        {
//            return NO;
//        }
//
//        [self setComposedBuffer:@"%^&"];
//
//        [sender setMarkedText:@"%^&" selectionRange:NSMakeRange(location - 2, 3) replacementRange:NSMakeRange(location - 2, 2)];
        
//        ProcessSerialNumber psn;
//        GetFrontProcess(&psn);
//        CGEventRef keyDownEvent = CGEventCreateKeyboardEvent( NULL, (CGKeyCode)0xFF, true);
//        CGEventPostToPSN(&psn, keyDownEvent);
//        CFRelease(keyDownEvent);
        
        return YES;
    }
    
    if (event.type == NSKeyDown && event.keyCode == kVK_Delete)
    {
        NSLog(@"Processing a delete.");
        
        if (_numberOfDeletesExpected > 0)
            if (--_numberOfDeletesExpected == 0)
            {
                ProcessSerialNumber psn;
                GetFrontProcess(&psn);
                
                CGEventRef ev = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)0xFF, true);
                CGEventPostToPSN(&psn, ev);
                CFRelease(ev);
            }
        
        return NO;
    }
    
    if (event.type == NSKeyDown && event.keyCode == 0xFF)
    {
        NSLog(@"Processing the special 0xFF");
        
        NSString* text = [self composedBuffer];
        
        if (text != nil && [text length] > 0)
        {
            NSLog(@"inserting text from composed buffer: %@", text);
            
            [inputClient insertText:text replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
            
            [self setComposedBuffer:@""];
            
            return YES;
        }
    }
    
    return NO;
}

/* Returns string representation of key, if it is printable.
 * Ownership follows the Create Rule; that is, it is the caller's
 * responsibility to release the returned object. */
CFStringRef createStringForKey(CGKeyCode keyCode)
{
    TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardInputSource();
    CFDataRef layoutData =
    TISGetInputSourceProperty(currentKeyboard,
                              kTISPropertyUnicodeKeyLayoutData);
    const UCKeyboardLayout *keyboardLayout =
    (const UCKeyboardLayout *)CFDataGetBytePtr(layoutData);
    
    UInt32 keysDown = 0;
    UniChar chars[4];
    UniCharCount realLength;
    
    UCKeyTranslate(keyboardLayout,
                   keyCode,
                   kUCKeyActionDisplay,
                   0,
                   LMGetKbdType(),
                   kUCKeyTranslateNoDeadKeysBit,
                   &keysDown,
                   sizeof(chars) / sizeof(chars[0]),
                   &realLength,
                   chars);
    CFRelease(currentKeyboard);
    
    return CFStringCreateWithCharacters(kCFAllocatorDefault, chars, 1);
}

/* Returns key code for given character via the above function, or UINT16_MAX
 * on error. */
CGKeyCode keyCodeForChar(const char c)
{
    static CFMutableDictionaryRef charToCodeDict = NULL;
    UniChar character = c;
    CFStringRef charStr = NULL;

    /* Generate table of keycodes and characters. */
    if (charToCodeDict == NULL) {
        size_t i;
        charToCodeDict = CFDictionaryCreateMutable(kCFAllocatorDefault,
                                                   128,
                                                   &kCFCopyStringDictionaryKeyCallBacks,
                                                   NULL);
        if (charToCodeDict == NULL) return UINT16_MAX;

        /* Loop through every keycode (0 - 127) to find its current mapping. */
        for (i = 0; i < 128; ++i) {
            CFStringRef string = createStringForKey((CGKeyCode)i);
            if (string != NULL) {
                CFDictionaryAddValue(charToCodeDict, string, (const void *)i);
                CFRelease(string);
            }
        }
    }

    charStr = CFStringCreateWithCharacters(kCFAllocatorDefault, &character, 1);

    /* Our values may be NULL (0), so we need to use this function.
     * Use a pointer-sized variable to receive the value, since CFDictionary
     * stores values as void pointers (64-bit on modern systems), then cast
     * to CGKeyCode (uint16_t). */
    const void *value = NULL;
    CGKeyCode code;
    if (CFDictionaryGetValueIfPresent(charToCodeDict, charStr, &value)) {
        code = (CGKeyCode)(uintptr_t)value;
    } else {
        code = UINT16_MAX;
    }

    CFRelease(charStr);
    return code;
}

// Return the composed buffer.  If it is NIL create it.
-(NSMutableString*)composedBuffer;
{
    if ( _composedBuffer == nil ) {
        _composedBuffer = [[NSMutableString alloc] init];
    }
    return _composedBuffer;
}

// Change the composed buffer.
-(void)setComposedBuffer:(NSString*)string
{
    NSMutableString*        buffer = [self composedBuffer];
    [buffer setString:string];
}

// Get the original buffer.
-(NSMutableString*)originalBuffer
{
    if ( _originalBuffer == nil ) {
        _originalBuffer = [[NSMutableString alloc] init];
    }
    return _originalBuffer;
}

//// Add newly input text to the original buffer.
//-(void)originalBufferAppend:(NSString*)string client:(id)sender
//{
//    NSMutableString* buffer = [self originalBuffer];
//    [buffer appendString: string];
//    _insertionIndex++;
//    [sender setMarkedText:buffer selectionRange:NSMakeRange(0, [buffer length]) replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
//}

// Change the original buffer.
-(void)setOriginalBuffer:(NSString*)string
{
    NSMutableString* buffer = [self originalBuffer];
    [buffer setString:string];
    NSLog(@"buffer = \"%@\"", buffer);
}

- (NSUInteger)logContext:(id)client {
    NSLog(@"*** logContext ***");
    NSLog(@"client: %@ / %@", client, [client uniqueClientIdentifierString]);
    NSRange selRange = [client selectedRange];
    if (selRange.location == NSNotFound)
        NSLog(@"selectedRange.location not found");
    else
        NSLog(@"selectedRange.location = %lu", (unsigned long)selRange.location);
    if (selRange.length == NSNotFound)
        NSLog(@"selectedRange.length not found");
    else
        NSLog(@"selectedRange.length = %lu", (unsigned long)selRange.length);

    NSUInteger len = [client length];
    NSLog(@"client length: %lu", len);

    NSRange actualRange;
    
    if (selRange.location != NSNotFound && selRange.location > 0 && selRange.location != len)
    {
        if (selRange.location > len)
            NSLog(@"location > len probably means client is misreporting length.");
        else
        {
            NSString *followingText = [client stringFromRange:NSMakeRange(selRange.location, len - selRange.location) actualRange:&actualRange];
            NSLog(@"followingText = %@", followingText);
        }
        len = selRange.location;
    }

    NSUInteger start = 0;
    if (len > 15)
    {
        start = len - 15;
        len = 15;
    }
    //NSString *preBuffer = [[client attributedSubstringFromRange:NSMakeRange(0, len)] string];
    NSString *preBuffer = [client stringFromRange:NSMakeRange(start, len) actualRange:&actualRange];
    if (preBuffer)
        [self setOriginalBuffer:preBuffer];
    
    return selRange.location;
}

- (void)activateServer:(id)sender {
    NSLog(@"*** activateServer ***");
    NSLog(@"sender: %@", sender);
    
    [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
    [self performSelector:@selector(logContext:) withObject:sender afterDelay:0.25];
}

- (TestInputMethodAppDelegate *)AppDelegate {
    return (TestInputMethodAppDelegate *)[NSApp delegate];
}

-(NSMenu*)menu
{
    return [self.AppDelegate menu];
}

- (void)menuAction:(id)sender {
    NSMenuItem *mItem = [sender objectForKey:kIMKCommandMenuItemName];
    NSInteger itag = mItem.tag;
    NSLog(@"Menu clicked - tag: %lu", itag);
    [[NSSound soundNamed:(itag < 100 ? (itag < 10 ? @"Hero" : @"Glass") : @"Frog")] play];
}

- (void)deactivateServer:(id)sender {
    NSLog(@"*** deactivateServer ***");
    NSLog(@"sender: %@", sender);
}

@end
