//
//  KMAboutBGView.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/11/2015.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMAboutBGView.h"

@implementation KMAboutBGView

- (void)drawRect:(NSRect)rect {
    [[NSColor whiteColor] setFill];
    NSRectFillUsingOperation(rect, NSCompositeSourceOver);
}

- (BOOL)mouseDownCanMoveWindow {
    return YES;
}

@end
