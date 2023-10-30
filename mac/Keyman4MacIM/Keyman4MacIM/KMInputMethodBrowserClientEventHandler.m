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

// Because Google Docs can't report its context in any of the browsers (Safari, Chrome, Firefox) and Word sometimes
// has trouble doing this in Chrome, we want to try to detect it so each browser can respond appropriately.
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
        if (self.appDelegate.debugMode) {
            NSLog(@"Checking to see if we're in Google Docs (or some other site that can't give context).");
        }
        NSUInteger bufferLength = self.contextBuffer.length;
        if (bufferLength) {
            NSUInteger location = [client selectedRange].location;
            
            if (location != NSNotFound && location > 0) {
                if ([self appDelegate].debugMode)
                    NSLog(@"Trying to get context up to location %lu", location);
                NSString* clientContext = [self getLimitedContextFrom:client at:location];
                if (clientContext == nil)
                {
                    // Client is failing to provide useful response to attributedSubstringFromRange.
                    // Word (in MS Live) occasionally does this.
                    [self setInSiteThatDoesNotGiveContext];
                }
                else if (!clientContext.length) {
                    if ([self appDelegate].debugMode) {
                        NSLog(@"Expected context = '%@'", self.contextBuffer);
                        NSLog(@"Actual clientContext was empty");
                    }
                    _failuresToRetrieveExpectedContext++;
                }
                else
                {
                    unichar lastCodepointExpected = [self.contextBuffer characterAtIndex:bufferLength - 1];
                    unichar lastCodepointInClient = [clientContext characterAtIndex:clientContext.length - 1];
                    if (lastCodepointExpected != lastCodepointInClient) {
                        if ([self appDelegate].debugMode) {
                            NSLog(@"Expected context = '%@'", self.contextBuffer);
                            NSLog(@"Actual clientContext = '%@'", (clientContext == nil ? @"{nil}" : clientContext));
                            NSLog(@"Last character expected (in contextBuffer) = '%lu'", (unsigned long)lastCodepointExpected);
                            NSLog(@"Last character in clientContext = '%lu'", (unsigned long)lastCodepointInClient);
                        }
                        // MS Word converts/reports plain spaces (32) as non-breaking spaces (160). If we
                        // get this kind of mismatch, we don't want to count this as a definite match, but
                        // we also don't want to count it as a failure to match. So just wait for a more
                        // reliable character to test.
                        if (lastCodepointInClient != 160 || lastCodepointExpected != 32) {
                            _failuresToRetrieveExpectedContext++;
                        }
                    }
                    else {
                        if ([self appDelegate].debugMode) {
                            NSLog(@"We got what we were expecting from the client. We can stop checking.");
                        }
                        _failuresToRetrieveExpectedContext = NSUIntegerMax;
                    }
                }
            }
            else {
                if ([self appDelegate].debugMode) {
                    NSLog(@"bufferLength is %lu, but location was %lu.", bufferLength, location);
                }
                _failuresToRetrieveExpectedContext++;
            }
            
            if (_failuresToRetrieveExpectedContext == 3)
                [self setInSiteThatDoesNotGiveContext];
        }
    }
}

- (void)setInSiteThatDoesNotGiveContext {
    if ([self appDelegate].debugMode) {
        NSLog(@"Detected some editor (e.g., Google Docs) that can't provide context.");
    }
    _failuresToRetrieveExpectedContext = NSUIntegerMax;
    
    if (self.clientSelectionCanChangeUnexpectedly) {
        self.clientCanProvideSelectionInfo = Unreliable;
        self.clientSelectionCanChangeUnexpectedly = NO; // This isn't true (it can change unexpectedly), but we can't get the context, so we pretend/hope it won't.
    }
}
@end
