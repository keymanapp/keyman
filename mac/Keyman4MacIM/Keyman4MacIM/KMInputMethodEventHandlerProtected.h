//
//  KMInputMethodEventHandlerProtected.h
//  Keyman4MacIM
//
//  Created by tom on 1/5/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef KMInputMethodEventHandlerProtected_h
#define KMInputMethodEventHandlerProtected_h

typedef NS_ENUM(NSInteger, ClientCapability) {
    Unknown,
    Yes,
    No,
    Unreliable
};

@interface KMInputMethodEventHandler ()
//@property (nonatomic) NSMutableDictionary *kbData;
//@property (nonatomic) NSDictionary *kmModes;
@property (assign) BOOL willDeleteNullChar;
@property (assign) BOOL contextOutOfDate;
// This flag indicates which mode is being used for replacing already typed text when composing characters
// (i.e., when not using deadkeys). Some apps (and some javascript-based websites, such as Google Docs) do
// not properly deal with calls to insertText calls that replace a range of characters. So for these "legacy"
// apps, we post one or more deletes (i.e., backspace), followed by a special code that tells us we're now
// ready to insert the composed text.
@property BOOL legacyMode;
// Some clients (e.g. Chrome) handle the mouse down events before we get a crack at them. For such apps that
// are able to report their current selection location (LibreOffice can't even do that!), we can do some
// checking at the start of the event processing to see if we're probably still in the same place where we
// left off previously.
@property (assign) BOOL clientSelectionCanChangeUnexpectedly; // REVIEW: Maybe we can get notification from these clients by handling mouseDownOnCharacterIndex.
@property (assign) ClientCapability clientCanProvideSelectionInfo;

@property id senderForDeleteBack;
@property BOOL ignoreNextDeleteBackHighLevel;

- (instancetype)initWithLegacyMode:(BOOL)legacy clientSelectionCanChangeUnexpectedly:(BOOL) flagClientSelectionCanChangeUnexpectedly;
- (void)handleCommand:(NSEvent *)event;
// This just sets the legacyMode property to true and spits out a debug message to that effect.
- (void)switchToLegacyMode;
- (void)checkContextIn:(id)client;
- (void)replaceExistingSelectionIn:(id)client with:(NSString *) text;
- (void)insertPendingBufferTextIn:(id)client;
- (KMInputMethodAppDelegate *)appDelegate;
- (NSMutableString *)contextBuffer;
- (NSString *)getLimitedContextFrom:(id)sender at:(NSUInteger) len;
// Return the pending buffer.  If it is NIL create it.
-(NSMutableString*)pendingBuffer;
-(void)setPendingBuffer:(NSString*)string;
- (void)initiatePendingBufferProcessing:(id)sender;
@end

#endif /* KMInputMethodEventHandlerProtected_h */
