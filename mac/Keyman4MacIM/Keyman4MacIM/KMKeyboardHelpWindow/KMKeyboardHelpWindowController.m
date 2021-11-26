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

@interface KMKeyboardHelpWindowController ()
@property (nonatomic, weak) IBOutlet WebView *welcomeView;
@property (nonatomic, strong) KMPackageInfo *packageInfo;
@end

@implementation KMKeyboardHelpWindowController

- (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

- (void)windowDidLoad {
    [super windowDidLoad];
    [self.window setTitle:@"Keyman"];
    [self.welcomeView setFrameLoadDelegate:(id<WebFrameLoadDelegate>)self];
    [self.welcomeView setPolicyDelegate:(id<WebPolicyDelegate>)self];
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
            NSLog(@"Error = %@", e.description);
        }
        
        NSString *welcomeFile = [packagePath stringByAppendingPathComponent:@"welcome.htm"];
        if ([[NSFileManager defaultManager] fileExistsAtPath:welcomeFile])
            [self.welcomeView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:welcomeFile]]];
        else
            [self.welcomeView.mainFrame loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"about:blank"]]];
    }
}

- (KMPackageInfo*)packageInfo {
    if(_packageInfo == nil) {
        _packageInfo = [self.AppDelegate loadPackageInfo:self.packagePath];
        
        NSLog(@"SGS2021 packageInfo.packageName = %@", _packageInfo.packageName);
        NSLog(@"SGS2021 packageInfo.packageVersion = %@", _packageInfo.packageVersion);
        NSLog(@"SGS2021 packageInfo.authorName = %@", _packageInfo.authorName);
        NSLog(@"SGS2021 packageInfo.authorUrl = %@", _packageInfo.authorUrl);
        NSLog(@"SGS2021 packageInfo.copyright = %@", _packageInfo.copyright);
        NSLog(@"SGS2021 packageInfo.readmeFilename = %@", _packageInfo.readmeFilename);

    }

    /*
    if(_keyboardInfo == nil) {
        KMPackageInfo *packageInfo = [self.AppDelegate loadPackageInfo:self.packagePath];
    
        NSMutableDictionary *infoDict = [NSMutableDictionary dictionaryWithCapacity:0];
        [infoDict setObject:@[packageInfo.packageName, @""] forKey:@"Name"];
        [infoDict setObject:packageInfo.readmeFilename forKey:@"ReadMeFile"];

        _keyboardInfo = infoDict;
        _keyboardInfo = [self.AppDelegate loadPackageInfo:self.packagePath];
    }

    if (_keyboardInfo == nil) {
        NSString *jsonFilename = [self.packagePath stringByAppendingPathComponent:@"kmp.json"];
        NSLog(@"SGS2021 KMKeyboardHelpWindowController loading keyboard info from json file: %@", jsonFilename);
        _keyboardInfo = [self.AppDelegate loadKeyboardInfoFromJsonFile:jsonFilename];

        NSDictionary *info = _keyboardInfo[@"info"];
        NSDictionary *nameMap = info[@"name"];
        NSLog(@"SGS2021 package name = %@", nameMap[@"description"]);
    }
    
    if (_keyboardInfo == nil) {
        NSString *infoFile = [self.packagePath stringByAppendingPathComponent:@"kmp.inf"];
        NSLog(@"SGS2021 KMKeyboardHelpWindowController loading keyboard info from %@", infoFile);
        _keyboardInfo = [self.AppDelegate keyboardInfoFromInfFile:infoFile];
    }
    */
    
    return _packageInfo;
}

- (IBAction)okAction:(id)sender {
    [self close];
}

- (IBAction)printAction:(id)sender {
    NSPrintInfo *printInfo = [NSPrintInfo sharedPrintInfo];
    [printInfo setOrientation:NSPaperOrientationPortrait];
    NSPrintOperation *printOperation = [[self.welcomeView.mainFrame frameView] printOperationWithPrintInfo:printInfo];
    [printOperation runOperationModalForWindow:self.window
                                      delegate:nil
                                didRunSelector:nil
                                   contextInfo:nil];
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
