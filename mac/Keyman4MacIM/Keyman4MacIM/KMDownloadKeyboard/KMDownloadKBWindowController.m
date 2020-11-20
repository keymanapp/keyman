//
//  KMDownloadKBWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 18/05/2015.
//  Copyright (c) 2018 SIL International. All rights reserved.
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
    [self.webView setGroupName:@"KMDownloadKB"];
    [self.webView setPolicyDelegate:(id<WebPolicyDelegate>)self];
    
    NSString *version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleShortVersionString"];
    KeymanVersionInfo keymanVersionInfo = [[self AppDelegate] versionInfo];
    NSString *url = [NSString stringWithFormat:@"https://%@/go/macos/14.0/download-keyboards/?version=%@", keymanVersionInfo.keymanCom, version];
    if (self.AppDelegate.debugMode)
        NSLog(@"KMDownloadKBWindowController opening url = %@, version = '%@'", url, version);
    [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
}

- (void)webView:(WebView *)webView decidePolicyForNavigationAction:(NSDictionary *)actionInformation request:(NSURLRequest *)request frame:(WebFrame *)frame decisionListener:(id<WebPolicyDecisionListener>)listener {
    NSString* url = [[request URL] description];
    if (self.AppDelegate.debugMode)
        NSLog(@"decidePolicyForNavigationAction, navigating to %@", url);
    
    if ([request.URL.absoluteString startsWith:@"keyman:"]) {
        if ([request.URL.absoluteString startsWith:@"keyman:link?url="])
        {
            [listener ignore];
            url = [request.URL.absoluteString substringFromIndex:[@"keyman:link?url=" length]];
            if (self.AppDelegate.debugMode)
                NSLog(@"Opening URL in default browser: %@", url);
            [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:url]];
        }
        else {
            if (self.AppDelegate.debugMode)
                NSLog(@"Farming out download to app delegate.");
            [listener ignore];
            [self.AppDelegate processURL:url];
        }
    }
    else
    {
        if (self.AppDelegate.debugMode)
            NSLog(@"Normal navigation in webview.");
        [listener use];
    }
}
@end
