//
//  KMInputMethodBrowserClientEventHandler.m
//  Keyman
//
//  Created by tom on 1/5/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "KMInputMethodBrowserClientEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import "KMInputMethodBrowserClientEventHandlerProtected.h"

@implementation KMInputMethodBrowserClientEventHandler

// Because Google Docs can't report its context in any of the browsers (Safari, Chrome, Firefox), we want to
// try to detect it so each browser can respond appropriatey.
NSUInteger _failuresToRetrieveExpectedContext;

- (instancetype)init {
    return [self initWithLegacyMode:YES clientSelectionCanChangeUnexpectedly:YES];
}

- (instancetype)initWithLegacyMode:(BOOL)legacy clientSelectionCanChangeUnexpectedly:(BOOL) flagClientSelectionCanChangeUnexpectedly {
    self = [super initWithLegacyMode:legacy clientSelectionCanChangeUnexpectedly: flagClientSelectionCanChangeUnexpectedly];
    if (self) {
        _failuresToRetrieveExpectedContext = 0;
    }
    return self;
}

- (void)checkContextIn:(id) client {
    if (!self.willDeleteNullChar && !self.contextOutOfDate && _failuresToRetrieveExpectedContext < 3) {
        if (self.AppDelegate.debugMode) {
            NSLog(@"Checking to see if we're in Google Docs (or some other site that can't give context).");
        }
        NSUInteger bufferLength = self.contextBuffer.length;
        if (bufferLength) {
            NSUInteger location = [client selectedRange].location;
            
            if (location != NSNotFound && location > 0) {
                NSString *clientContext = [[client attributedSubstringFromRange:NSMakeRange(0, location)] string];
                if (clientContext == nil || !clientContext.length ||
                    [clientContext characterAtIndex:clientContext.length - 1] !=
                    [self.contextBuffer characterAtIndex:bufferLength - 1]) {
                    _failuresToRetrieveExpectedContext++;
                }
                else {
                    if ([self AppDelegate].debugMode) {
                        NSLog(@"We got what we were expecting from the client. We can stop checking.");
                    }
                    _failuresToRetrieveExpectedContext = NSUIntegerMax;
                }
            }
            else {
                _failuresToRetrieveExpectedContext++;
            }
            
            if (_failuresToRetrieveExpectedContext == 3)
            {
                if ([self AppDelegate].debugMode) {
                    NSLog(@"Detected Google Docs or some other editor that can't provide context.");
                }
                _failuresToRetrieveExpectedContext = NSUIntegerMax;
                [self setInGoogleDocs];
            }
        }
    }
}

- (void)setInGoogleDocs {
    if (self.clientSelectionCanChangeUnexpectedly) {
        self.cannnotTrustSelectionLength = YES;
        self.clientSelectionCanChangeUnexpectedly = NO; // This isn't true (it can change unexpectedly), but we can't get the context, so we pretend/hope it won't.
    }
}
@end
