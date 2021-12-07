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
    // TODO: rename
    [self.window setTitle:@"Keyboard/Package Info"];
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

- (NSString *)detailsHtml {
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
        "<p class='title'>Keyboard Layouts:</p><br>"
        "%@"
        "<br>"
        "<p class='title'>Fonts:</p><br>"
        "%@"
        "<br>"
        "<p class='title'>Package version: </p><p class='body'>%@</p><br>"
        "<p class='title'>Author: </p><p class='body'>%@</p><br>"
        "<p class='title'>Website: </p><p class='body'>%@</p><br>"
        "<p class='title'>Copyright: </p><p class='body'>%@</p>"
        "</div>"
        "</body>"
        "</html>";
        
        NSString *pBodyFormat = @"<p class='body'>%@</p><br>";
        NSMutableString *kbsStr = [NSMutableString stringWithString:@""];
        
        // TODO: getting keyboards from folder/kmx rather than inf
        NSArray *kbs = [[self AppDelegate] keyboardNamesFromFolder:self.packagePath];
        if (kbs != nil && kbs.count) {
            for (NSArray *kbName in kbs) {
                [kbsStr appendString:[NSString stringWithFormat:pBodyFormat, kbName]];
            }
        }
        // TODO: need to provide above as fallback in case no json???
/*        else {
            // This was the old logic (prior to fixing issue #994). Keeping as fallback, though
            // it's unlikely to be useful/needed.
            kbs = [self.keyboardInfo objectForKey:kKeyboard];
        
            if (kbs != nil && kbs.count) {
                for (NSArray *kb in kbs) {
                    [kbsStr appendString:[NSString stringWithFormat:pBodyFormat, [kb objectAtIndex:0]]];
                }
            }
            else {
                kbsStr = [NSMutableString stringWithString:@"<p class='body'><none></p><br>"];
            }
        }
*/
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
        /*
        NSString *websiteV1 = [[self.keyboardInfo objectForKey:kWebSite] objectAtIndex:0];
        NSString *websiteV2 = [[self.keyboardInfo objectForKey:kWebSite] objectAtIndex:1];
        NSString *website = @"";
        if (websiteV1.length && websiteV2.length)
            website = [NSString stringWithFormat:linkFormat, websiteV2, websiteV1];
        else if (websiteV1.length)
            website = websiteV1;
        else if (websiteV2.length)
            website = [NSString stringWithFormat:linkFormat, websiteV2, websiteV2];
        */
        NSString *website = @"";
        if (self.packageInfo.website.length)
            website = [NSString stringWithFormat:linkFormat, self.packageInfo.website, self.packageInfo.website];

        /*
        NSString *copyrightV1 = [[self.keyboardInfo objectForKey:kCopyright] objectAtIndex:0];
        NSString *copyrightV2 = [[self.keyboardInfo objectForKey:kCopyright] objectAtIndex:1];
        NSString *copyright = @"";
        if (copyrightV1.length && copyrightV2.length)
            copyright = [NSString stringWithFormat:linkFormat, copyrightV2, copyrightV1];
        else if (copyrightV1.length)
            copyright = copyrightV1;
        else if (copyrightV2.length)
            copyright = [NSString stringWithFormat:linkFormat, copyrightV2, copyrightV2];
        */
        
        NSString *copyright = @"";
        if (self.packageInfo.copyright.length)
            copyright = self.packageInfo.copyright;
        
        NSString *htmlStr = [NSString stringWithFormat:htmlFormat, name, shareUrl, shareUrl,
                             kbsStr, fontsStr, version, author, website, copyright];
        
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
