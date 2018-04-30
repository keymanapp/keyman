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
    NSString *url = [NSString stringWithFormat:@"https://keyman.com/go/macos/10.0/download-keyboards/?version=%@", version];
    NSLog(@"KMDownloadKBWindowController opening url = %@, version = '%@'", url, version);
    [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];

// REVIEW: Compare this implementation to the one in KMKeyboardHelpWindowController. Should we
// use a consistent prefix?
- (void) webView:(WebView*) sender decidePolicyForNavigationAction:(NSDictionary*) actionInformation request:(NSURLRequest*) request frame:(WebFrame*) frame decisionListener:(id <WebPolicyDecisionListener>) listener {
    NSString* url = [[request URL] description];
    NSLog(@"decidePolicyForNavigationAction, navigating to %@", url);
    if ([request.URL.absoluteString startsWith:@"keyman:link?url="])
    {
        [listener ignore];
        url = [request.URL.absoluteString substringFromIndex:[@"keyman:link?url=" length]];
        [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:url]];
    }
    else
    {
        [listener use];
    }
}

@end
