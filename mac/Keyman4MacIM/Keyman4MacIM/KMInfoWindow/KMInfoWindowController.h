//
//  KMInfoWindowController.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface KMInfoWindowController : NSWindowController <NSTabViewDelegate, WKNavigationDelegate>

@property (nonatomic, strong) NSString *packagePath;

@end
