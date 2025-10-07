//
//  KMDownloadKBWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 18/05/2015.
//  Copyright (c) 2018 SIL International. All rights reserved.
//

#import "KMDownloadKBWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMLogs.h"
@import Sentry;

@interface KMDownloadKBWindowController ()
@property (nonatomic, strong) IBOutlet WKWebView *webView;
@end

@implementation KMDownloadKBWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
  [super windowDidLoad];
  
  os_log_debug([KMLogs uiLog], "KMDownloadKBWindowController windowDidLoad webview: %{public}@", self.webView);
  self.webView.navigationDelegate = self;
  
  NSString *version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleShortVersionString"];
  os_log_debug([KMLogs uiLog], "KMDownloadKBWindowController windowDidLoad version: %{public}@", version);
  KeymanVersionInfo keymanVersionInfo = [[self AppDelegate] versionInfo];
  NSString *url = [NSString stringWithFormat:@"https://%@/go/macos/14.0/download-keyboards/?version=%@", keymanVersionInfo.keymanCom, version];
  
  os_log_debug([KMLogs uiLog], "KMDownloadKBWindowController opening url = %{public}@, version = '%{public}@'", url, version);
  
  [self.webView loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
}

- (void)webView:(WKWebView *)webView decidePolicyForNavigationAction:(WKNavigationAction *)navigationAction decisionHandler:(void (^)(WKNavigationActionPolicy))decisionHandler {
  NSURL *url = navigationAction.request.URL;
  os_log_debug([KMLogs uiLog], "decidePolicyForNavigationAction with WKNavigationAction, navigating to %{public}@", url);
  
  NSArray<NSString *> *callStackSymbols = [NSThread callStackSymbols];
  NSString *stackTraceString = [callStackSymbols componentsJoinedByString:@"\n"];
  os_log_debug([KMLogs uiLog], "stack trace:\n%{public}@", stackTraceString);
  
  // The pattern for matching links matches work in #3602
  NSString* urlPathMatchKeyboardsInstall = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards/install/([^?/]+)(?:\\?(.+))?$";
  // e.g. https://keyman.com/keyboards/install/foo
  NSString* urlPathMatchKeyboardsRoot = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards([/?].*)?$";
  // http://keyman.com.local/keyboards/foo
  NSString* urlPathMatchKeyboardsGo = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/go/macos/[^/]+/download-keyboards";
  // https://keyman-staging.com/go/macos/14.0/download-keyboards?version=14.0.146.0
  
  NSError* error;
  NSRegularExpression* regexInstall = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsInstall options: 0 error: &error];
  NSRegularExpression* regexRoot = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsRoot options: 0 error: &error];
  NSRegularExpression* regexGo = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsGo options: 0 error: &error];
  
  NSString* urlString = [navigationAction.request.URL absoluteString];
  NSRange range = NSMakeRange(0, urlString.length);
  
  NSArray* matchesInstall = [regexInstall matchesInString:urlString options:0 range:range];
  
  if(matchesInstall.count > 0) {
    os_log_debug([KMLogs uiLog], "Delegating download to app delegate.");
    decisionHandler(WKNavigationActionPolicyCancel);
    NSTextCheckingResult* match = (NSTextCheckingResult*) matchesInstall[0];
    NSString* matchKeyboardId = [urlString substringWithRange:[match rangeAtIndex:1]];
    // Install keyboard link
    [self.AppDelegate downloadKeyboardFromKeyboardId:matchKeyboardId];
  }
  else if([regexRoot numberOfMatchesInString:urlString options:0 range:range] > 0 ||
          [regexGo numberOfMatchesInString:urlString options:0 range:range] > 0) {
    // allow https://keyman.com/keyboards* to go through
    // allow https://keyman.com/go/macos/download-keyboards to go through
    os_log_debug([KMLogs uiLog], "Accepting link in this browser.");
    decisionHandler(WKNavigationActionPolicyAllow);
  }
  else if([urlString startsWith:@"keyman:"]) {
    if ([urlString startsWith:@"keyman:link?url="])
    {
      os_log_debug([KMLogs uiLog], "Opening keyman:link URL in default browser: %{public}@", url);
      decisionHandler(WKNavigationActionPolicyCancel);
      urlString = [navigationAction.request.URL.absoluteString substringFromIndex:[@"keyman:link?url=" length]];
      [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:urlString]];
    }
    else {
      os_log_debug([KMLogs uiLog], "Delegating download to app delegate.");
      decisionHandler(WKNavigationActionPolicyCancel);
      [self.AppDelegate processURL:urlString];
    }
  }
  else
  {
    // Open in external browser
    os_log_debug([KMLogs uiLog], "Opening URL in default browser: %{public}@", url);
    decisionHandler(WKNavigationActionPolicyCancel);
    [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:urlString]];
  }
}

- (void) webView:(WKWebView *) webView
didReceiveServerRedirectForProvisionalNavigation:(WKNavigation *) navigation {
  os_log_debug([KMLogs uiLog], "KMDownloadKBWindowController didReceiveServerRedirectForProvisionalNavigation");
}

- (void) webView:(WKWebView *) webView
didFailProvisionalNavigation:(WKNavigation *) navigation
       withError:(NSError *) error {
  os_log_error([KMLogs uiLog], "KMDownloadKBWindowController didFailProvisionalNavigation, error: %{public}@", error);
  [SentrySDK captureError:(error)];
}

- (void) webView:(WKWebView *) webView
didFailNavigation:(WKNavigation *) navigation
       withError:(NSError *) error {
  os_log_error([KMLogs uiLog], "KMDownloadKBWindowController didFailNavigation, error: %{public}@", error);
  [SentrySDK captureError:(error)];
}

@end
