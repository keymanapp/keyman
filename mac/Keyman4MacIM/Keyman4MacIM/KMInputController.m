//
//  KMInputController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputController.h"
#import "KMInputMethodEventHandler.h"
#import "KMInputMethodBrowserClientEventHandler.h"
#import "KMInputMethodSafariClientEventHandler.h"
#import "KMOSVersion.h"
#include <Carbon/Carbon.h> /* For kVK_ constants. */

@implementation KMInputController

KMInputMethodEventHandler* _eventHandler;
NSMutableArray *servers;

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (id)initWithServer:(IMKServer *)server delegate:(id)delegate client:(id)inputClient
{
    if ([self.AppDelegate debugMode]) {
        NSLog(@"Initializing Keyman Input Method with server: %@", server);
    }
    
    self = [super initWithServer:server delegate:delegate client:inputClient];
    if (self) {
        servers = [[NSMutableArray alloc] initWithCapacity:2];
        self.AppDelegate.inputController = self;
        if (self.AppDelegate.kvk != nil && self.AppDelegate.alwaysShowOSK) {
            [self.AppDelegate showOSK];
        }
    }
    
    return self;
}

- (NSUInteger)recognizedEvents:(id)sender {
    return NSEventMaskKeyDown | NSEventMaskFlagsChanged;
}

- (BOOL)handleEvent:(NSEvent *)event client:(id)sender {
    if ([self.AppDelegate debugMode])
        NSLog(@"handleEvent: event = %@", event);
    
    if (event == nil || sender == nil || self.kmx == nil || _eventHandler == nil) {
        if ([self.AppDelegate debugMode])
            NSLog(@"handleEvent: not handling event");
        return NO; // Not sure this can ever happen.
    }
    
    return [_eventHandler handleEvent:event client:sender];
}

// Passthrough from the app delegate low level event hook
// to the input method event handler for Delete Back. 
- (BOOL)handleDeleteBackLowLevel:(NSEvent *)event {
    if(_eventHandler != nil) {
        return [_eventHandler handleDeleteBackLowLevel:event];
    }

    return NO;
}

- (void)activateServer:(id)sender {
    @synchronized(servers) {
        [sender overrideKeyboardWithKeyboardNamed:@"com.apple.keylayout.US"];
        
        [self.AppDelegate wakeUpWith:sender];
        [servers addObject:sender];
        
        if (_eventHandler != nil) {
            [_eventHandler deactivate];
        }
        
        NSRunningApplication *currApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
        NSString *clientAppId = [currApp bundleIdentifier];
        if ([self.AppDelegate debugMode]) {
            NSLog(@"New active app %@", clientAppId);
            NSLog(@"sender %@", sender);
        }
        
        // Most things in Safari work well using the normal way, but Google Docs doesn't.
        if ([clientAppId isEqual: @"com.apple.Safari"]) {
            _eventHandler = [[KMInputMethodSafariClientEventHandler alloc] init];
        }
        else if ([clientAppId isEqual: @"org.mozilla.firefox"] ||
            [clientAppId isEqual: @"com.google.Chrome"]) {
            _eventHandler = [[KMInputMethodBrowserClientEventHandler alloc] init];
        }
        else {
            // We cache the client for use with events sourced from the low level tap
            // where we don't necessarily have any access to the current client.
            _eventHandler = [[KMInputMethodEventHandler alloc] initWithClient:clientAppId client:sender];
        }
    }
}

- (void)deactivateServer:(id)sender {
    if ([self.AppDelegate debugMode]) {
        NSLog(@"*** deactivateServer ***");
        NSLog(@"sender: %@", sender);
        NSLog(@"***");
    }
    @synchronized(servers) {
        for (int i = 0; i < servers.count; i++) {
            if (servers[i] == sender) {
                [servers removeObjectAtIndex:i];
                break;
            }
        }
        if (servers.count == 0) {
            if ([self.AppDelegate debugMode]) {
                NSLog(@"No known active server for Keyman IM. Starting countdown to sleep...");
            }
            [self performSelector:@selector(timerAction:) withObject:sender afterDelay:0.7];
        }
    }
}

- (void)timerAction:(id)lastServer {
    @synchronized(servers) {
        if (servers.count == 0) {
            if (_eventHandler != nil) {
                [_eventHandler deactivate];
                _eventHandler = nil;
            }
            [self.AppDelegate sleepFollowingDeactivationOfServer:lastServer];
        }
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

- (KMXFile *)kmx {
    return self.AppDelegate.kmx;
}


- (void)menuAction:(id)sender {
    NSMenuItem *mItem = [sender objectForKey:kIMKCommandMenuItemName];
    NSInteger itag = mItem.tag;
    if ([self.AppDelegate debugMode])
        NSLog(@"Keyman menu clicked - tag: %lu", itag);
    if (itag == 2) {
        // Using `showConfigurationWindow` instead of `showPreferences:` because `showPreferences:` is missing in
        // High Sierra (10.13.1 - 10.13.3). See: https://bugreport.apple.com/web/?problemID=35422518
        // rrb: where Apple's API is broken (10.13.1-10.13.3) call our workaround, otherwise, call showPreferences
        u_int16_t systemVersion = [KMOSVersion SystemVersion];
        if ([KMOSVersion Version_10_13_1] <= systemVersion && systemVersion <= [KMOSVersion Version_10_13_3]) // between 10.13.1 and 10.13.3 inclusive
        {
            NSLog(@"Input Menu: calling workaround instead of showPreferences (sys ver %x)", systemVersion);
            [self.AppDelegate showConfigurationWindow]; // call our workaround
        }
        else
        {
            NSLog(@"Input Menu: calling Apple's showPreferences (sys ver %x)", systemVersion);
            [self showPreferences:sender]; // call Apple API
        }
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
        NSString *keyboardName = [kmxInfo objectForKey:kKMKeyboardNameKey];
        if ([self.AppDelegate debugMode])
            NSLog(@"Selected keyboard from menu: %@", keyboardName);
        [self.AppDelegate setKeyboardName:keyboardName];
        [self.AppDelegate setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
        [self.AppDelegate setContextBuffer:nil];
        [self.AppDelegate setSelectedKeyboard:path];
        [self.AppDelegate loadSavedStores];
        if (kvk != nil && self.AppDelegate.alwaysShowOSK)
            [self.AppDelegate showOSK];
    }
}
@end
