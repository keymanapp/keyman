//
//  KMDownloadKBWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 18/05/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMDownloadKBWindowController.h"
#import "KMInputMethodAppDelegate.h"

@interface KMDownloadKBWindowController ()
@property (nonatomic, weak) IBOutlet WebView *webView;
@end

@implementation KMDownloadKBWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
    [super windowDidLoad];
    
    [self.webView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
    [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"https://secure.tavultesoft.com/prog/macosx/10/downloadkeyboards/"]]];
}

@end
