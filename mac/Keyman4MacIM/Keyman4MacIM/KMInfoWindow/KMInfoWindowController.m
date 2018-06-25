//
//  KMInfoWindowController.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInfoWindowController.h"
#import "KMInputMethodAppDelegate.h"
#import <WebKit/WebKit.h>

@interface KMInfoWindowController ()
@property (nonatomic, weak) IBOutlet NSImageView *imageView;
@property (nonatomic, weak) IBOutlet NSTabView *tabView;
@property (nonatomic, weak) IBOutlet WebView *detailsView;
@property (nonatomic, weak) IBOutlet WebView *readmeView;
@property (nonatomic, strong) NSDictionary *infoDict;
@property (nonatomic, strong) NSTabViewItem *readMeTab;
@end

@implementation KMInfoWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
    [super windowDidLoad];
    [self.window setTitle:@"Keyboard/Package Info"];
    [self.tabView setDelegate:self];
    [self.detailsView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
    [self.detailsView setPolicyDelegate:(id<WebPolicyDelegate>)self];
    [self.readmeView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
    [self.readmeView setPolicyDelegate:(id<WebPolicyDelegate>)self];
}

- (void)setPackagePath:(NSString *)packagePath {
    _packagePath = packagePath;
    _infoDict = nil;
    [self.tabView selectTabViewItemAtIndex:0];
    if (packagePath != nil && packagePath.length) {
        NSString *imgName = [self.infoDict objectForKey:kGraphicFile];
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
        
        NSString *readMeFile = [self.infoDict objectForKey:kReadMeFile];
        if (readMeFile != nil) {
            [self.readmeView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:[packagePath stringByAppendingPathComponent:readMeFile]]]];
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
        "</style>"
        "</head>"
        "<body style='margin:0px'>"
        "<div class='header'><img src='info.png' class='icon'><p class='header'>%@</p></div>"
        "<div class='body'>"
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
        NSArray *kbs = [[self AppDelegate] keyboardNamesFromFolder:self.packagePath];
        if (kbs != nil && kbs.count) {
            for (NSArray *kbName in kbs) {
                [kbsStr appendString:[NSString stringWithFormat:pBodyFormat, kbName]];
            }
        }
        else {
            // This was the old logic (prior to fixing issue #994). Keeping as fallback, though
            // it's unlikely to be useful/needed.
            kbs = [self.infoDict objectForKey:kKeyboard];
        
            if (kbs != nil && kbs.count) {
                for (NSArray *kb in kbs) {
                    [kbsStr appendString:[NSString stringWithFormat:pBodyFormat, [kb objectAtIndex:0]]];
                }
            }
            else {
                kbsStr = [NSMutableString stringWithString:@"<p class='body'><none></p><br>"];
            }
        }
        
        NSArray *fonts = [self.infoDict objectForKey:kFont];
        NSMutableString *fontsStr = [NSMutableString stringWithString:@""];
        if (fonts != nil && fonts.count) {
            for (NSArray *font in fonts) {
                [fontsStr appendString:[NSString stringWithFormat:pBodyFormat, [font objectAtIndex:0]]];
            }
        }
        else {
            fontsStr = [NSMutableString stringWithString:@"<p class='body'><none></p><br>"];
        }
        
        NSString *linkFormat = @"<a href='%@'>%@</a>";
        NSString *name = [[self.infoDict objectForKey:kName] objectAtIndex:0];
        if (name == nil)
            name = @"Unknown Keyboard Package";
        
        NSString *version = [[self.infoDict objectForKey:kVersion] objectAtIndex:0];
        if (version == nil)
            version = @"";
        
        NSString *authorV1 = [[self.infoDict objectForKey:kAuthor] objectAtIndex:0];
        NSString *authorV2 = [[self.infoDict objectForKey:kAuthor] objectAtIndex:1];
        NSString *author = @"";
        if (authorV1.length && authorV2.length)
            author = [NSString stringWithFormat:linkFormat, authorV2, authorV1];
        else if (authorV1.length)
            author = authorV1;
        else if (authorV2.length)
            author = [NSString stringWithFormat:linkFormat, authorV2, authorV2];
        
        NSString *websiteV1 = [[self.infoDict objectForKey:kWebSite] objectAtIndex:0];
        NSString *websiteV2 = [[self.infoDict objectForKey:kWebSite] objectAtIndex:1];
        NSString *website = @"";
        if (websiteV1.length && websiteV2.length)
            website = [NSString stringWithFormat:linkFormat, websiteV2, websiteV1];
        else if (websiteV1.length)
            website = websiteV1;
        else if (websiteV2.length)
            website = [NSString stringWithFormat:linkFormat, websiteV2, websiteV2];
        
        NSString *copyrightV1 = [[self.infoDict objectForKey:kCopyright] objectAtIndex:0];
        NSString *copyrightV2 = [[self.infoDict objectForKey:kCopyright] objectAtIndex:1];
        NSString *copyright = @"";
        if (copyrightV1.length && copyrightV2.length)
            copyright = [NSString stringWithFormat:linkFormat, copyrightV2, copyrightV1];
        else if (copyrightV1.length)
            copyright = copyrightV1;
        else if (copyrightV2.length)
            copyright = [NSString stringWithFormat:linkFormat, copyrightV2, copyrightV2];
        
        NSString *htmlStr = [NSString stringWithFormat:htmlFormat, name, kbsStr, fontsStr, version, author, website, copyright];
        
        return htmlStr;
    }
    @catch (NSException *e) {
        NSLog(@"Error = %@", e.description);
        return nil;
    }
}

- (NSDictionary *)infoDict {
    if (_infoDict == nil) {
        NSString *infoFile = [self.packagePath stringByAppendingPathComponent:@"kmp.inf"];
        _infoDict = [self.AppDelegate infoDictionaryFromFile:infoFile];
    }
    
    return _infoDict;
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
