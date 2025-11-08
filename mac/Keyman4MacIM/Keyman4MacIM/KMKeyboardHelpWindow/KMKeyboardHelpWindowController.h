//
//  KMKeyboardHelpWindowController.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 13/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface KMKeyboardHelpWindowController : NSWindowController<WKNavigationDelegate>

@property (nonatomic, strong) NSString *packagePath;

@end
