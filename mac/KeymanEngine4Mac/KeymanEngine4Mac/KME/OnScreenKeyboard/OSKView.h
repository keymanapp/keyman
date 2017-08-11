//
//  OSKView.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KVKFile.h"

@interface OSKView : NSView

@property (weak, nonatomic) KVKFile *kvk;

- (void)handleKeyEvent:(NSEvent *)event;
- (void)setShiftState:(BOOL)shiftState;
- (void)setOskShiftState:(BOOL)oskShiftState;
- (void)setAltState:(BOOL)altState;
- (void)setOskAltState:(BOOL)oskAltState;
- (void)setCtrlState:(BOOL)ctrlState;
- (void)setOskCtrlState:(BOOL)oskCtrlState;
- (void)resetOSK;
- (void)resizeOSKLayout;

@end
