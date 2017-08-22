//
//  main.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 28/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <InputMethodKit/InputMethodKit.h>

const NSString *kConnectionName = @"Keyman_Input_Connection";
IMKServer *server;

int main(int argc, const char * argv[]) {
    NSString *identifier;
    @autoreleasepool {
        identifier = [[NSBundle mainBundle] bundleIdentifier];
        server = [[IMKServer alloc] initWithName:(NSString *)kConnectionName bundleIdentifier:identifier];
        [NSBundle loadNibNamed:@"MainMenu" owner:[NSApplication sharedApplication]];
        [[NSApplication sharedApplication] run];
    }
    return 0;
}
