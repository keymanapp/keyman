//
//  KMConfigurationWindowController.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 24/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KMInputMethodAppDelegate.h"
#import "KMConfigColumn1CellView.h"
#import "KMConfigColumn3CellView.h"
#import "KMPackage.h"

@interface KMConfigurationWindowController : NSWindowController <NSTableViewDelegate, NSTableViewDataSource>

- (void)handleRequestToInstallPackage:(KMPackage *) package;

@end
