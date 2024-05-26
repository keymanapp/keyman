//
//  KMInfoWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInfoWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import "KMPackageInfo.h"
#import "KMKeyboardInfo.h"
#import <WebKit/WebKit.h>

@interface KMInfoWindowController ()
@property (nonatomic, weak) IBOutlet NSImageView *imageView;
@property (nonatomic, weak) IBOutlet NSTabView *tabView;
@property (nonatomic, weak) IBOutlet WebView *detailsView;
@property (nonatomic, weak) IBOutlet WebView *readmeView;
@property (nonatomic, strong) KMPackageInfo *packageInfo;
@property (nonatomic, strong) NSTabViewItem *readMeTab;
@end

@implementation KMInfoWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
  [super windowDidLoad];
  
  [self.tabView setDelegate:self];
  [self.detailsView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
  [self.detailsView setPolicyDelegate:(id<WebPolicyDelegate>)self];
  [self.readmeView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
  [self.readmeView setPolicyDelegate:(id<WebPolicyDelegate>)self];
}

// TODO: rename, refactor, reset PackageInfo directly rather than making getter do it?
- (void)setPackagePath:(NSString *)packagePath {
  _packagePath = packagePath;
  _packageInfo = nil;
  [self.tabView selectTabViewItemAtIndex:0];
  if (packagePath != nil && packagePath.length) {
    NSString *imgName = self.packageInfo.graphicFilename;
    if (imgName != nil) {
      NSString *imgFile = [packagePath stringByAppendingPathComponent:imgName];
      NSImage *img = [[NSImage alloc] initWithContentsOfFile:imgFile];
      [self.imageView setImage:img];
    }
    else {
      [self.imageView setImage:[NSImage imageNamed:@"SideImage.bmp"]];
    }
    
    NSString *detailsHtml = [self detailsHtml];
    if (detailsHtml != nil)
      [self.detailsView.mainFrame loadHTMLString:detailsHtml baseURL:[[NSBundle mainBundle] resourceURL]];
    else
      [self.detailsView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"about:blank"]]];
    
    NSString *readmeFilename = self.packageInfo.readmeFilename;
    if (readmeFilename != nil) {
      [self.readmeView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:[packagePath stringByAppendingPathComponent:readmeFilename]]]];
      if (_readMeTab != nil && ![self.tabView.tabViewItems containsObject:_readMeTab]) {
        [self.tabView addTabViewItem:_readMeTab];
        _readMeTab = nil;
      }
    }
    else {
      [self.readmeView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"about:blank"]]];
      if ([self.tabView.tabViewItems count] > 1) {
        _readMeTab = [self.tabView tabViewItemAtIndex:1];
        [self.tabView removeTabViewItem:_readMeTab];
      }
    }
  }
}

- (BOOL)tabView:(NSTabView *)tabView shouldSelectTabViewItem:(NSTabViewItem *)tabViewItem {
  return YES;
}

// TODO: refactor: any reason for this to be HTML? hard to read stringWithFormat applied to template with 16 arguments
- (NSString *)detailsHtml {
  
  NSString *errorString = NSLocalizedString(@"message-keyboard-file-unreadable", nil);
  
  @try {
    NSString *htmlFormat =
    @"<html>"
    "<head>"
    "<meta http-equiv='content-type' content='text/html; charset=UTF-8'>"
    "<title>Keyman</title>"
    "<style>"
    "div.header {background-color:#CC3846; height:30px;}"
    "div.body {padding:10px;}"
    "img.icon {width:20px; height:20px; padding-top:5px; padding-left:10px; vertical-align:middle;}"
    "p.header {display:inline; color:white; font-family:arial; font-weight:bold; font-size:16px; padding-left:10px; vertical-align:bottom;}"
    "p.title {display:inline; font-family:arial; font-weight:bold; font-size:14px;}"
    "p.body {display:inline; font-family:arial; font-size:14px;}"
    "#qrcode-container {display:block; width: 128px; height: 166px; padding: 8px; float: right; background: white; border: solid 1px black}"
    "#qrcode-caption {font-family: arial; font-size: 11px; margin-top: 4px; text-align: center}"
    "</style>"
    "<script src='qrcode.min.js'></script>"
    "</head>"
    "<body style='margin:0px'>"
    "<div class='header'><img src='info.png' class='icon'><p class='header'>%@</p></div>"
    "<div class='body'>"
    "<div id='qrcode-container'>"
    "<div id='qrcode'></div>"
    "<script>new QRCode(document.getElementById('qrcode'), {text:'%@',width:128,height:128})</script>"
    "<div id='qrcode-caption'>Scan this code to load this keyboard on another device or "
    "<a href='%@'>share online</a></div>"
    "</div>"
    "<p class='title'>%@</p><br>"
    "%@"
    "<br>"
    "<p class='title'>%@</p><br>"
    "%@"
    "<br>"
    "<p class='title'>%@ </p><p class='body'>%@</p><br>"
    "<p class='title'>%@ </p><p class='body'>%@</p><br>"
    "<p class='title'>%@ </p><p class='body'>%@</p><br>"
    "<p class='title'>%@ </p><p class='body'>%@</p>"
    "</div>"
    "</body>"
    "</html>";
    
    NSString *pBodyFormat = @"<p class='body'>%@</p><br>";
    NSMutableString *keyboardString = [NSMutableString stringWithString:@""];
    
    if (self.packageInfo.keyboards.count) {
      for (KMKeyboardInfo *keyboard in self.packageInfo.keyboards) {
        [keyboardString appendString:[NSString stringWithFormat:pBodyFormat, keyboard.name]];
      }
    } else {
      keyboardString = [NSMutableString stringWithString:@"<p class='body'><none></p><br>"];
    }
    
    NSArray *fonts = self.packageInfo.fonts;
    NSMutableString *fontsStr = [NSMutableString stringWithString:@""];
    
    if (fonts != nil && fonts.count) {
      for (NSString *font in fonts) {
        [fontsStr appendString:[NSString stringWithFormat:pBodyFormat, font]];
      }
    }
    else {
      fontsStr = [NSMutableString stringWithString:@"<p class='body'><none></p><br>"];
    }
    
    NSString *linkFormat = @"<a href='%@'>%@</a>";
    NSString *packageId = [self.packagePath lastPathComponent];
    
    KeymanVersionInfo keymanVersionInfo = [[self AppDelegate] versionInfo];
    NSString *shareUrl = [NSString stringWithFormat:@"https://%@/go/keyboard/%@/share", keymanVersionInfo.keymanCom, packageId];
    
    NSString *name = self.packageInfo.packageName;
    if (name == nil)
      name = @"Unknown Keyboard Package";
    
    NSString *version = self.packageInfo.packageVersion;
    if (version == nil)
      version = @"";
    
    NSString *authorV1 = self.packageInfo.authorName;
    NSString *authorV2 = self.packageInfo.authorUrl;
    NSString *author = @"";
    if (authorV1.length && authorV2.length)
      author = [NSString stringWithFormat:linkFormat, authorV2, authorV1];
    else if (authorV1.length)
      author = authorV1;
    else if (authorV2.length)
      author = [NSString stringWithFormat:linkFormat, authorV2, authorV2];
    
    NSString *website = @"";
    if (self.packageInfo.website.length)
      website = [NSString stringWithFormat:linkFormat, self.packageInfo.website, self.packageInfo.website];
    
    NSString *copyright = @"";
    if (self.packageInfo.copyright.length)
      copyright = self.packageInfo.copyright;
    
    // get localized label strings
    NSString *keyboardsLabel = NSLocalizedString(@"keyboards-label", nil);
    NSString *fontsLabel = NSLocalizedString(@"fonts-label", nil);
    NSString *packageVersionLabel = NSLocalizedString(@"package-version-label", nil);
    NSString *authorLabel = NSLocalizedString(@"author-label", nil);
    NSString *websiteLabel = NSLocalizedString(@"website-label", nil);
    NSString *copyrightLabel = NSLocalizedString(@"copyright-label", nil);
    
    
    NSString *htmlStr = [NSString stringWithFormat:htmlFormat, name, shareUrl, shareUrl,
                         keyboardsLabel, keyboardString, fontsLabel, fontsStr,
                         packageVersionLabel, version, authorLabel, author,
                         websiteLabel, website, copyrightLabel, copyright];
    
    return htmlStr;
  }
  @catch (NSException *e) {
    NSLog(@"Error = %@", e.description);
    return nil;
  }
}

- (KMPackageInfo *)packageInfo {
  if(_packageInfo == nil) {
    _packageInfo = [self.AppDelegate loadPackageInfo:self.packagePath];
  }
  
  return _packageInfo;
}

- (void)webView:(WebView *)webView decidePolicyForNavigationAction:(NSDictionary *)actionInformation request:(NSURLRequest *)request frame:(WebFrame *)frame decisionListener:(id<WebPolicyDecisionListener>)listener {
  NSNumber *navType = [actionInformation objectForKey:WebActionNavigationTypeKey];
  if (navType.integerValue == WebNavigationTypeLinkClicked && ![request.URL isFileURL]) {
    NSString *urlStr = [[request URL] absoluteString];
    if ([urlStr startsWith:@"link:"])
      urlStr = [urlStr substringFromIndex:5];
    
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:urlStr]];
  }
  else
    [listener use];
}

@end
