//
//  TestAppDelegate.h
//  Keyman4MacIM
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef TestAppDelegate_h
#define TestAppDelegate_h
#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

typedef void(^PostEventCallback)(CGEventRef eventToPost);

@interface TestAppDelegate : NSObject<NSApplicationDelegate>

@property (nonatomic, strong) KMEngine *kme;
@property (nonatomic, strong) KMXFile *kmx;
@property (nonatomic, strong) NSMutableString *contextBuffer;
@property (nonatomic, assign) CFMachPortRef lowLevelEventTap; // Always nil for tests
@property (nonatomic, assign) BOOL contextChangingEventDetected;
@property (nonatomic, assign) CGKeyCode virtualKeyPosted;

// Helper method
-(NSEvent *)keyStrokeEventForCharacter:(NSString *)character keyCode:(unsigned short) keyCode;

// Override of "normal" production processing
- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent;
@end


#endif /* TestAppDelegate_h */
