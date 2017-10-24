//
//  KMInputController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputController.h"

@interface KMInputController ()
@property (nonatomic, strong) NSMutableDictionary *kbData;
//@property (nonatomic, strong) NSDictionary *kmModes;
@property (assign) BOOL willDeleteNullChar;
@end

@implementation KMInputController

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithServer:(IMKServer *)server delegate:(id)delegate client:(id)inputClient {
    self = [super initWithServer:server delegate:delegate client:inputClient];
    if (self) {
        self.AppDelegate.inputController = self;
        if (self.AppDelegate.kvk != nil && self.AppDelegate.alwaysShowOSK)
            [self.AppDelegate showOSK];
    }
    
    return self;
}

- (NSUInteger)recognizedEvents:(id)sender {
    return (NSKeyDownMask | NSLeftMouseDownMask);
}

- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
    if ([self.AppDelegate debugMode])
        NSLog(@"Event = %@", event);
    
    if (event == nil || sender == nil)
        return NO;
    
    // OSK key feedback from hardware keyboard is disabled
    /*if (event.type == NSKeyDown)
        [self.AppDelegate handleKeyEvent:event];*/
    
    if (self.kmx == nil)
        return NO;
    
    if (event.type == NSLeftMouseDown) {
        [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
        return NO;
    }
    else if ((event.modifierFlags & NSEventModifierFlagCommand) == NSEventModifierFlagCommand)
        return NO; // We ignore any Command-key events.
    
    BOOL handled = NO;
    NSArray *actions = nil;
    if (!self.willDeleteNullChar)
        actions = [self.kme processEvent:event];
    
    if ([self.AppDelegate debugMode])
        NSLog(@"actions = %@", actions);
    for (NSDictionary *action in actions) {
        NSString *actionType = [[action allKeys] objectAtIndex:0];
        if ([actionType isEqualToString:Q_STR]) {
            NSString *output = [action objectForKey:actionType];
            if (self.useNullChar) {
                NSUInteger nc = [self.contextBuffer deleteLastNullChars];
                if (nc > 0) {
                    NSRange selRange = [sender selectedRange];
                    NSUInteger pos = selRange.location;
                    if (pos >= nc && pos != NSNotFound) {
                        [sender insertText:output replacementRange:NSMakeRange(pos - nc, nc)];
                    }
                }
                else {
                    [sender insertText:output replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
                }
            }
            else {
                [sender insertText:output replacementRange:NSMakeRange(NSNotFound, NSNotFound)];
            }
            
            [self.contextBuffer appendString:output];
        }
        else if ([actionType isEqualToString:Q_BACK]) {
            [self.contextBuffer deleteLastNullChars];
            NSUInteger dk = [self.contextBuffer deleteLastDeadkeys];
            NSInteger n = [[action objectForKey:actionType] integerValue] - dk;
            NSUInteger dc = [[self.contextBuffer lastNChars:n] deadKeyCount];
            n -= dc;
            
            BOOL shouldAppendNullChar = NO;
            if (n > 0) {
                if ([self.AppDelegate debugMode])
                    NSLog(@"Q_BACK");
                NSRange selRange = [sender selectedRange];
                NSInteger pos = selRange.location;
                if ([self.AppDelegate debugMode])
                    NSLog(@"pos = %lu", pos);
                if (pos >= n && pos != NSNotFound) {
                    if (self.useNullChar) {
                        if ((pos - (n+1)) >= 0) {
                            NSString *preChar = [[sender attributedSubstringFromRange:NSMakeRange(pos - (n+1), 1)] string];
                            [sender insertText:preChar replacementRange:NSMakeRange(pos - (n+1), n+1)];
                        }
                        else {
                            [sender insertText:@" " replacementRange:NSMakeRange(pos - n, n)];
                            shouldAppendNullChar = YES;
                        }
                    }
                    else {
                        [sender setMarkedText:@"" selectionRange:NSMakeRange(0, 0) replacementRange:NSMakeRange(pos - n, n)];
                    }
                }
            }
            
            [self.contextBuffer deleteLastNChars:n+dc];
            if (shouldAppendNullChar)
                [self.contextBuffer appendNullChar];
        }
        else if ([actionType isEqualToString:Q_DEADKEY]) {
            [self.contextBuffer deleteLastNullChars];
            NSUInteger x = [[action objectForKey:actionType] unsignedIntegerValue];
            [self.contextBuffer appendDeadkey:x];
        }
        else if ([actionType isEqualToString:Q_NUL]) {
            continue;
        }
        else if ([actionType isEqualToString:Q_RETURN]) {
            return YES;
        }
        else if ([actionType isEqualToString:Q_BEEP]) {
            [[NSSound soundNamed:@"Tink"] play];
        }
        
        handled = YES;
    }
    
    // Apply context changes if not handled
    if (!handled) {
        if (self.useNullChar) {
            NSUInteger nc = [self.contextBuffer deleteLastNullChars];
            if (nc > 0) {
                for (int i = 0; i < nc; i++) {
                    self.willDeleteNullChar = YES;
                    [self deleteBack];
                }
                
                [self sendEvent:event];
                return YES;
            }
        }
        
        BOOL didUpdateContext = NO;
        unsigned short keyCode = event.keyCode;
        if (keyCode <= 0x33) { // Main keys
            if (keyCode == 0x24) // Enter
                [self.contextBuffer appendString:@"\n"];
            else if (keyCode == 0x33) { // Backspace
                if (!self.willDeleteNullChar) {
                    [self.contextBuffer deleteLastDeadkeys];
                    [self.contextBuffer deleteLastNChars:1];
                    [self.contextBuffer deleteLastDeadkeys];
                }
                
                self.willDeleteNullChar = NO;
            }
            else
                [self.contextBuffer appendString:event.characters];
        }
        else {
            unichar ch = [event.characters characterAtIndex:0];
            if (ch >= 0x2A && ch <= 0x39) // Numpad char range
                [self.contextBuffer appendString:event.characters];
            else if (keyCode == 0x4C) // Enter (Numpad)
                [self.contextBuffer appendString:@"\n"];
            else if (keyCode >= 0x7B && keyCode <= 0x7E) {
                // Arrow keys
                [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
                didUpdateContext = YES;
            }
            else if (keyCode == 0x73 || keyCode == 0x77 || keyCode == 0x74 || keyCode == 0x79) {
                // Home, End, Page Up, Page Down
                [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
                didUpdateContext = YES;
            }
            else {
                // Other keys
            }
        }
        
        if (!didUpdateContext)
            [self.kme setContextBuffer:self.contextBuffer];
    }
    
    if ([self.AppDelegate debugMode]) {
        NSLog(@"handledEvent: %@", handled?@"YES":@"NO");
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"***");
    }
    
    return handled;
}

- (void)activateServer:(id)sender {
    [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
    [self performSelector:@selector(updateContextBuffer:) withObject:sender afterDelay:0.25];
}

- (void)deactivateServer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** deactivateServer ***");
        NSLog(@"sender: %@", sender);
        NSLog(@"***");
    }
}

/*
- (NSDictionary *)modes:(id)sender {
    if ([self.AppDelegate debugMode])
        NSLog(@"*** Modes ***");
    if (_kmModes == nil) {
        NSDictionary *amhMode = [[NSDictionary alloc] initWithObjectsAndKeys:@"keyman.png", kTSInputModeAlternateMenuIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeIsVisibleKey,
                                 @"A", kTSInputModeKeyEquivalentKey,
                                 [NSNumber numberWithInteger:4608], kTSInputModeKeyEquivalentModifiersKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 @"keyman.png", kTSInputModeMenuIconFileKey,
                                 @"keyman.png", kTSInputModePaletteIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModePrimaryInScriptKey,
                                 @"smUnicodeScript", kTSInputModeScriptKey,
                                 @"amh", @"TISIntendedLanguage", nil];
        
        NSDictionary *hinMode = [[NSDictionary alloc] initWithObjectsAndKeys:@"keyman.png", kTSInputModeAlternateMenuIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeIsVisibleKey,
                                 @"H", kTSInputModeKeyEquivalentKey,
                                 [NSNumber numberWithInteger:4608], kTSInputModeKeyEquivalentModifiersKey,
                                 [NSNumber numberWithBool:YES], kTSInputModeDefaultStateKey,
                                 @"keyman.png", kTSInputModeMenuIconFileKey,
                                 @"keyman.png", kTSInputModePaletteIconFileKey,
                                 [NSNumber numberWithBool:YES], kTSInputModePrimaryInScriptKey,
                                 @"smUnicodeScript", kTSInputModeScriptKey,
                                 @"hin", @"TISIntendedLanguage", nil];
        
        NSDictionary *modeList = [[NSDictionary alloc] initWithObjectsAndKeys:amhMode, @"com.apple.inputmethod.amh", hinMode, @"com.apple.inputmethod.hin", nil];
        NSArray *modeOrder = [[NSArray alloc] initWithObjects:@"com.apple.inputmethod.amh", @"com.apple.inputmethod.hin", nil];
        _kmModes = [[NSDictionary alloc] initWithObjectsAndKeys:modeList, kTSInputModeListKey,
                               modeOrder, kTSVisibleInputModeOrderedArrayKey, nil];
    }

    return _kmModes;
}
*/

- (NSMenu *)menu {
    return self.AppDelegate.menu;
}

- (KMEngine *)kme {
    return self.AppDelegate.kme;
}

- (KMXFile *)kmx {
    return self.AppDelegate.kmx;
}

- (NSMutableString *)contextBuffer {
    return self.AppDelegate.contextBuffer;
}

- (BOOL)useNullChar {
    return self.AppDelegate.useNullChar;
}

- (void)updateContextBuffer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** updateContextBuffer ***");
        NSLog(@"sender: %@", sender);
    }
    
    NSRange selRange = [sender selectedRange];
    NSUInteger len = [sender length];
    if (selRange.location != NSNotFound && selRange.location > 0)
        len = selRange.location;
    if ([self.AppDelegate debugMode])
        NSLog(@"selRange.location: %lu", (unsigned long)selRange.location);
    NSString *preBuffer = [[sender attributedSubstringFromRange:NSMakeRange(0, len)] string];
    if ([self.AppDelegate debugMode])
        NSLog(@"preBuffer = \"%@\"", preBuffer);
    [self.AppDelegate setContextBuffer:preBuffer.length?[NSMutableString stringWithString:preBuffer]:nil];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?[self.contextBuffer codeString]:@"{empty}");
        NSLog(@"***");
    }
}

- (void)clearContextBuffer {
    if ([self.AppDelegate debugMode])
        NSLog(@"*** clearContextBuffer ***");
    [self.AppDelegate setContextBuffer:nil];
    if ([self.AppDelegate debugMode]) {
        NSLog(@"contextBuffer = \"%@\"", self.contextBuffer.length?self.contextBuffer:@"{empty}");
        NSLog(@"***");
    }
}

- (void)menuAction:(id)sender {
    NSMenuItem *mItem = [sender objectForKey:kIMKCommandMenuItemName];
    NSInteger itag = mItem.tag;
    if (itag == 2) {
        [self showPreferences:sender];
    }
    else if (itag == 3) {
        [self.AppDelegate showOSK];
    }
    else if (itag == 4) {
        [self.AppDelegate showAboutWindow];
    }
    else if (itag >= 1000) {
        NSMenuItem *keyboards = [self.AppDelegate.menu itemWithTag:1];
        for (NSMenuItem *item in keyboards.submenu.itemArray) {
            if (item.tag == itag)
                [item setState:NSOnState];
            else
                [item setState:NSOffState];
        }
        
        NSString *path = [self.AppDelegate.activeKeyboards objectAtIndex:itag%1000];
        KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
        [self.AppDelegate setKmx:kmx];
        KVKFile *kvk = nil;
        NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
        NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
        if (kvkFilename != nil) {
            NSString *kvkFilePath = [self.AppDelegate kvkFilePathFromFilename:kvkFilename];
            if (kvkFilePath != nil)
                kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
        }
        [self.AppDelegate setKvk:kvk];
        [self.AppDelegate setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
        [self.AppDelegate setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
        [self.AppDelegate setContextBuffer:nil];
        [self.AppDelegate setSelectedKeyboard:path];
        if (kvk != nil && self.AppDelegate.alwaysShowOSK)
            [self.AppDelegate showOSK];
    }
}

- (void)sendEvent:(NSEvent *)event {
    ProcessSerialNumber psn;
    GetFrontProcess(&psn);
    
    CGEventSourceRef source = CGEventCreateSourceFromEvent([event CGEvent]);
    CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, event.keyCode, true);
    CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(source, event.keyCode, false);
    
    CGEventPostToPSN(&psn, keyDownEvent);
    CGEventPostToPSN(&psn, keyUpEvent);
    
    CFRelease(source);
    CFRelease(keyDownEvent);
    CFRelease(keyUpEvent);
}

- (void)deleteBack {
    ProcessSerialNumber psn;
    GetFrontProcess(&psn);

    CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)0x33, true);
    CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)0x33, false);
    
    CGEventPostToPSN(&psn, keyDownEvent);
    CGEventPostToPSN(&psn, keyUpEvent);
    
    CFRelease(keyDownEvent);
    CFRelease(keyUpEvent);
}

@end
