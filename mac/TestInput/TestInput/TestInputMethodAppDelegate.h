//
//  TestInputMethodAppDelegate.h
//
//  Copyright (c) 2018 SIL International. All rights reserved.
//

#ifndef TestInputMethodAppDelegate_h
#define TestInputMethodAppDelegate_h

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import "TestInputController.h"

@interface TestInputMethodAppDelegate : NSObject {
    IBOutlet NSMenu *_menu;
}

- (NSMenu *)menu;

@end

#endif /* TestInputMethodAppDelegate_h */
