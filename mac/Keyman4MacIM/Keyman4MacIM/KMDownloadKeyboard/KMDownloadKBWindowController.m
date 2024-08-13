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
  NSString *percentEncodedVersion = [version stringByAddingPercentEncodingWithAllowedCharacters:[NSCharacterSet URLQueryAllowedCharacterSet]];

  KeymanVersionInfo keymanVersionInfo = [[self AppDelegate] versionInfo];
  NSString *urlString = [NSString stringWithFormat:@"https://%@/go/macos/14.0/download-keyboards/?version=%@", keymanVersionInfo.keymanCom, percentEncodedVersion];
  os_log_debug([KMLogs uiLog], "KMDownloadKBWindowController opening url = %{public}@, version = '%{public}@', percentEncodedVersion = '%{public}@'",
               urlString, version, percentEncodedVersion);
  
  NSURL * downloadsUrl = [NSURL URLWithString:urlString];
  os_log_debug([KMLogs uiLog], "download site url = '%{public}@'", downloadsUrl.absoluteString);

  [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:downloadsUrl]];
}

- (void)webView:(WebView *)sender decidePolicyForNewWindowAction:(NSDictionary *)actionInformation request:(NSURLRequest *)request newFrameName:(NSString *)frameName decisionListener:(id<WebPolicyDecisionListener>)listener {
  NSString* url = [[request URL] absoluteString];
  os_log_debug([KMLogs uiLog], "decidePolicyForNewWindowAction, url = %{public}@", url);
  [[NSWorkspace sharedWorkspace] openURL:[actionInformation objectForKey:WebActionOriginalURLKey]];
  [listener ignore];
}

- (void)webView:(WebView *)webView decidePolicyForNavigationAction:(NSDictionary *)actionInformation request:(NSURLRequest *)request frame:(WebFrame *)frame decisionListener:(id<WebPolicyDecisionListener>)listener {
  os_log_debug([KMLogs uiLog], "decidePolicyForNavigationAction, request = %{public}@", request);
  NSString* url = [[request URL] absoluteString];
  os_log_debug([KMLogs uiLog], "decidePolicyForNavigationAction, navigating to %{public}@", url);

  // The pattern for matching links matches work in #3602
  NSString* urlPathMatchKeyboardsInstall = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards/install/([^?/]+)(?:\\?(.+))?$";
  // e.g. https://keyman.com/keyboards/install/foo
  NSString* urlPathMatchKeyboardsRoot = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/keyboards([/?].*)?$";
  // http://keyman.com.local/keyboards/foo
  NSString* urlPathMatchKeyboardsGo = @"^http(?:s)?://keyman(?:-staging)?\\.com(?:\\.local)?/go/macos/[^/]+/download-keyboards";
  // https://keyman-staging.com/go/macos/14.0/download-keyboards?version=14.0.146.0
  NSRange range = NSMakeRange(0, url.length);
  
  NSError* error;
  NSRegularExpression* regexInstall = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsInstall options: 0 error: &error];
  NSRegularExpression* regexRoot = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsRoot options: 0 error: &error];
  NSRegularExpression* regexGo = [NSRegularExpression regularExpressionWithPattern: urlPathMatchKeyboardsGo options: 0 error: &error];
  
  NSArray* matchesInstall = [regexInstall matchesInString:url options:0 range:range];
  
  if(matchesInstall.count > 0) {
    os_log_debug([KMLogs uiLog], "Delegating download to app delegate.");
    [listener ignore];
    NSTextCheckingResult* match = (NSTextCheckingResult*) matchesInstall[0];
    NSString* matchKeyboardId = [url substringWithRange:[match rangeAtIndex:1]];
    // Install keyboard link
    [self.AppDelegate downloadKeyboardFromKeyboardId:matchKeyboardId];
    //[self.AppDelegate processURL:url];
  }
  else if([regexRoot numberOfMatchesInString:url options:0 range:range] > 0 ||
          [regexGo numberOfMatchesInString:url options:0 range:range] > 0) {
    // allow https://keyman.com/keyboards* to go through
    // allow https://keyman.com/go/macos/download-keyboards to go through
    os_log_debug([KMLogs uiLog], "Accepting link in this browser.");
    [listener use];
  }
  else if([url startsWith:@"keyman:"]) {
    if ([url startsWith:@"keyman:link?url="])
    {
      os_log_debug([KMLogs uiLog], "Opening keyman:link URL in default browser: %{public}@", url);
      [listener ignore];
      url = [request.URL.absoluteString substringFromIndex:[@"keyman:link?url=" length]];
      [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:url]];
    }
    else {
      os_log_debug([KMLogs uiLog], "Delegating download to app delegate.");
      [listener ignore];
      [self.AppDelegate processURL:url];
    }
  }
  else
  {
    // Open in external browser
    os_log_debug([KMLogs uiLog], "Opening URL in default browser: %{public}@", url);
    [listener ignore];
    [[NSWorkspace sharedWorkspace] openURL: [[NSURL alloc] initWithString:url]];
  }
}
@end
