//
//  OSKView.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef OSKView_h
#define OSKView_h

#import "KVKFile.h"
#import "KMkeyViewProtocol.h"

@interface OSKView : NSView <KMkeyView>

@property (weak, nonatomic) KVKFile *kvk;

+ (BOOL)isOskKeyDownEvent:(CGEventRef)event;
+ (NSEventModifierFlags)extractModifierFlagsFromOskEvent:(CGEventRef)event;
- (void)handleKeyEvent:(NSEvent *)event;
- (void)setShiftState:(BOOL)shiftState;
- (void)setOskShiftState:(BOOL)oskShiftState;
- (void)setAltState:(BOOL)altState;
- (void)setOskAltState:(BOOL)oskAltState;
- (void)setCtrlState:(BOOL)ctrlState;
- (void)setOskCtrlState:(BOOL)oskCtrlState;
- (void)resetOSK;
- (void)resizeOSKLayout;
- (int64_t)createOskEventUserData;

@end

#endif /* OSKView_h */
