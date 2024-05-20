//
//  main.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 28/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <InputMethodKit/InputMethodKit.h>
#import <os/log.h>

const NSString *kConnectionName = @"Keyman_Input_Connection";
IMKServer *server;

int main(int argc, const char * argv[]) {
  NSString *identifier;
  os_log_t configLog = os_log_create("org.sil.keyman", "startup");
  
  @autoreleasepool {
    identifier = [[NSBundle mainBundle] bundleIdentifier];
    server = [[IMKServer alloc] initWithName:(NSString *)kConnectionName bundleIdentifier:identifier];
    
    BOOL didLoadNib = [[NSBundle mainBundle] loadNibNamed:@"MainMenu" owner:[NSApplication sharedApplication] topLevelObjects: nil];
    
    os_log_with_type(configLog, OS_LOG_TYPE_DEBUG, "main Did load MainMenu nib: %@", didLoadNib?@"YES":@"NO");
    
    [[NSApplication sharedApplication] run];
  }
  return 0;
}
