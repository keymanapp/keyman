//
//  KMKeyboardHelpWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 13/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMKeyboardHelpWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMPackageInfo.h"
#import <WebKit/WebKit.h>
#import "KMLogs.h"
@import Sentry;

@interface KMKeyboardHelpWindowController ()
@property (nonatomic, strong) IBOutlet WKWebView *welcomeWebView;
@property (nonatomic, strong) KMPackageInfo *packageInfo;
@end

@implementation KMKeyboardHelpWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
  [super windowDidLoad];
  [self.window setTitle:@"Keyman"];
  self.welcomeWebView.navigationDelegate = self;
}

- (void)setPackagePath:(NSString *)packagePath {
  _packagePath = packagePath;
  _packageInfo = nil;
  if (packagePath != nil && packagePath.length) {
    @try {
      NSString *title = self.packageInfo.packageName;
      if (title.length)
        [self.window setTitle:title];
    }
    @catch (NSException *e) {
      os_log_error([KMLogs dataLog], "Error = %{public}@", e.description);
    }
    
    NSString *welcomeFile = [packagePath stringByAppendingPathComponent:@"welcome.htm"];
    if ([[NSFileManager defaultManager] fileExistsAtPath:welcomeFile]) {
      [self.welcomeWebView loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:welcomeFile]]];
    }
    else {
      [self.welcomeWebView loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"about:blank"]]];
    }
  }
}

- (KMPackageInfo*)packageInfo {
  if(_packageInfo == nil) {
    _packageInfo = [self.AppDelegate loadPackageInfo:self.packagePath];
  }
  
  return _packageInfo;
}

- (IBAction)okAction:(id)sender {
  [self close];
}

- (IBAction)printAction:(id)sender {
  NSPrintInfo *printInfo = [NSPrintInfo sharedPrintInfo];
  [printInfo setOrientation:NSPaperOrientationPortrait];
  NSPrintOperation *printOperation = [self.welcomeWebView printOperationWithPrintInfo:printInfo];
  
  [printOperation runOperationModalForWindow:self.window
                                    delegate:nil
                              didRunSelector:nil
                                 contextInfo:nil];
}

- (void)webView:(WKWebView *)webView decidePolicyForNavigationAction:(WKNavigationAction *)navigationAction decisionHandler:(void (^)(WKNavigationActionPolicy))decisionHandler {

  os_log_debug([KMLogs uiLog], "KMKeyboardHelpWindowController decidePolicyForNavigationAction, navigating to %{public}@", navigationAction.request.URL);

  if (navigationAction.navigationType == WebNavigationTypeLinkClicked && ![navigationAction.request.URL isFileURL]) {
    NSString *urlStr = [navigationAction.request.URL absoluteString];
    if ([urlStr startsWith:@"link:"]) {
      urlStr = [urlStr substringFromIndex:5];
    }
    
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:urlStr]];
  }
  else
    decisionHandler(WKNavigationActionPolicyAllow);
}

- (void) webView:(WKWebView *) webView
didReceiveServerRedirectForProvisionalNavigation:(WKNavigation *) navigation {
  os_log_debug([KMLogs uiLog], "KMKeyboardHelpWindowController didReceiveServerRedirectForProvisionalNavigation");
}

- (void) webView:(WKWebView *) webView
didFailProvisionalNavigation:(WKNavigation *) navigation
       withError:(NSError *) error {
  os_log_error([KMLogs uiLog], "KMKeyboardHelpWindowController didFailProvisionalNavigation, error: %{public}@", error);
  [SentrySDK captureError:(error)];
}

- (void) webView:(WKWebView *) webView
didFailNavigation:(WKNavigation *) navigation
       withError:(NSError *) error {
  os_log_error([KMLogs uiLog], "KMKeyboardHelpWindowController didFailNavigation, error: %{public}@", error);
  [SentrySDK captureError:(error)];
}

@end
