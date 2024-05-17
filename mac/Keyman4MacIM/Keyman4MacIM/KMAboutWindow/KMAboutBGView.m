//
//  KMAboutBGView.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/11/2015.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMAboutBGView.h"

@implementation KMAboutBGView

// draw white behind the graphic at the top of the view,
// draw the theme's background color behind the rest of the view
// e.g., rect origin=(0,0) size=(318,450)
//  topImgView origin=(15,252) size=(58,400)
//  topFill origin=(0,252) size=(66,450)
//  botFill origin=(0,0) size=(252,450)
- (void)drawRect:(NSRect)rect {
    NSRect topFill, botFill;
    topFill = rect;
    botFill = rect;
//    NSLog(@"rect origin x %f y %f  size h %f w %f", rect.origin.x, rect.origin.y, rect.size.height, rect.size.width);
    NSView *topImgView = [self viewWithTag:1];
//    NSLog(@"topImgView origin x %f y %f  size h %f w %f before", topImgView.frame.origin.x, topImgView.frame.origin.y, topImgView.frame.size.height, topImgView.frame.size.width);
    NSInteger imgOriginHeightDelta = topImgView.frame.origin.y - rect.origin.y;
    topFill.origin.y += imgOriginHeightDelta;
    topFill.size.height -= imgOriginHeightDelta;
    botFill.size.height = imgOriginHeightDelta;
//    NSLog(@"topFill origin x %f y %f  size h %f w %f after raising to imgHeight %ld", topFill.origin.x, topFill.origin.y, topFill.size.height, topFill.size.width, imgOriginHeightDelta);
//    NSLog(@"botFill origin x %f y %f  size h %f w %f after reducing by imgHeight %ld", botFill.origin.x, botFill.origin.y, botFill.size.height, botFill.size.width, imgOriginHeightDelta);
    [[NSColor whiteColor] setFill];
    NSRectFillUsingOperation(topFill, NSCompositingOperationSourceOver);
    [[NSColor windowBackgroundColor] setFill];
    NSRectFillUsingOperation(botFill, NSCompositingOperationSourceOver);
}

- (BOOL)mouseDownCanMoveWindow {
    return YES;
}

@end
