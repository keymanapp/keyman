//
//  TestInputMethodAppDelegate.m
//  Copyright (c) 2018 SIL International. All rights reserved.
//

#import "TestInputMethodAppDelegate.h"

@implementation TestInputMethodAppDelegate

+ (TestInputMethodAppDelegate *)AppDelegate {
    return (TestInputMethodAppDelegate *)[NSApp delegate];
}

- (NSMenu *)menu {
    return _menu;
}

- (void)awakeFromNib {
    if (!_menu)
        NSLog(@"No main menu!");
    NSMenuItem *staticMenuItem1 = [_menu itemWithTag:1];
    if (staticMenuItem1) {
        NSLog(@"Setting action for menu 1");
        [staticMenuItem1 setAction:@selector(menuAction:)];
    }
    
    NSMenuItem* staticDropdownMenuItem = [_menu itemWithTag:2];
    if (staticDropdownMenuItem) {
        NSLog(@"Looking for dropdown menu on menu 2");
        NSMenu* dropDownMenu = [staticDropdownMenuItem submenu];
        if (dropDownMenu) {
            NSLog(@"Looking for menu 10");
            NSMenuItem* staticSubMenu = [dropDownMenu itemWithTag:10];
            if ( staticSubMenu ) {
                NSLog(@"Setting action for static submenu item");
                [staticSubMenu setAction:@selector(menuAction:)];
            }
        }
    }
    
    NSMenuItem* dynamicDropdownMenuItem = [_menu itemWithTag:3];
    if (dynamicDropdownMenuItem) {
        NSLog(@"Looking for dropdown menu on menu 3");
        NSMenu* dropDownMenu = [dynamicDropdownMenuItem submenu];
        if (dropDownMenu) {
            NSMenuItem *dynamicSubMenuItem = [[NSMenuItem alloc] initWithTitle:@"Submenu One" action:@selector(menuAction:) keyEquivalent:@""];
            [dynamicSubMenuItem setTag:100];
            NSLog(@"Adding dynamic submenu item 100");
            [dropDownMenu addItem:dynamicSubMenuItem];
        }
    }
}
@end
