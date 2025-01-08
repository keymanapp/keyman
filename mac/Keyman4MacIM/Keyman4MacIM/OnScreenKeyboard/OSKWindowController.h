//
//  OSKWindowController.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 30/04/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

@interface OSKWindowController : NSWindowController <NSWindowDelegate>

@property (nonatomic, weak) IBOutlet OSKView *oskView;

- (void)prepareToShowOsk;
- (void)resetOSK;
- (NSEventModifierFlags)getOskEventModifierFlags;

@end
