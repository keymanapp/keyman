//
//  KMInputMethodChromeClientEventHandler.m
//  Keyman
//
//  Created by tom on 1/9/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "KMInputMethodChromeClientEventHandler.h"
#import "KMInputMethodEventHandlerProtected.h"
#import "KMInputMethodBrowserClientEventHandlerProtected.h"

@implementation KMInputMethodChromeClientEventHandler

// Because Google Docs can't report its context, if we detect that we are in Google Docs, we change to
// NOT assume that it needs to re-get the context every time around (which means that if the user
// does mouse-click somewhere else, it could lead to bad behaviour).
BOOL _insertCharactersIndividually;

- (instancetype)init {    
    self = [super init];
    if (self) {
        _insertCharactersIndividually = NO;
    }
    return self;
}

- (void)setInGoogleDocs {
    [super setInGoogleDocs];
    // Google docs in Chrome allows only a single character at a time :-(
    _insertCharactersIndividually = YES;
}

- (void)insertPendingBufferTextIn:(id)client {
    
    NSString* remainingText = @"";
    if (!_insertCharactersIndividually) {
        NSUInteger length = [self pendingBuffer].length;
        if (length > 1) {
            if ([self.AppDelegate debugMode])
                NSLog(@"Using special Google Docs in Chrome logic (length = %lu)", length);
            remainingText = [self.pendingBuffer substringFromIndex:1];
            [self.pendingBuffer deleteLastNChars:length - 1];
        }
    }

    [super insertPendingBufferTextIn:client];
    
    if ([remainingText length] > 0) {
        // Reset the pending buffer to contain remaining characters and issue call to come back for more...
        [self setPendingBuffer:remainingText];
        [self performSelector:@selector(initiatePendingBufferProcessing:) withObject:client afterDelay:0.1];
    }
}
@end
