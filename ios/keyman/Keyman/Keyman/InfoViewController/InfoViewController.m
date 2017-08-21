//
//  InfoViewController.m
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "InfoViewController.h"
#import "AppDelegate.h"
#import "Keyman.h"
#import "Reachability.h"

@interface InfoViewController ()
    @property (strong, nonatomic) Reachability *networkReachable;
@end

@implementation InfoViewController
@synthesize webView, networkReachable;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    if ([self respondsToSelector:@selector(setExtendedLayoutIncludesOpaqueBars:)])
        [self setExtendedLayoutIncludesOpaqueBars:YES];
    
    self.webView.delegate = self;
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(checkNetworkStatus:) name:kReachabilityChangedNotification object:nil];
	networkReachable = [Reachability reachabilityWithHostName:@"www.keyman.com"];
	[networkReachable startNotifier];
}

- (void)viewDidUnload {
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    self.webView = nil;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)checkNetworkStatus:(NSNotification *)notification {
	NetworkStatus networkStatus = [networkReachable currentReachabilityStatus];
	switch (networkStatus)
	{
		case NotReachable:
		{
			//NSLog(@"Network is Off");
            [self loadFromLocal];
			break;
		}
		default:
		{
			//NSLog(@"Network is On");
            [self loadFromServer];
			break;
		}
	}
}

- (void)loadFromLocal {
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"info" ofType:@"html" inDirectory:nil];
    [webView loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:filePath]]];
}

- (void)loadFromServer {
    NSString *currentKbID = [[[KMManager sharedInstance] currentKeyboardInfo] objectForKey:kKeymanKeyboardIdKey];
    if (currentKbID == nil)
        currentKbID = kKeymanDefaultKeyboardID;
    
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSMutableString *installedKbs = [NSMutableString stringWithString:@""];
    NSString *kbID;
    if (userKeyboards != nil) {
        for (NSDictionary *kbInfo in userKeyboards) {
            kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
            if ([installedKbs rangeOfString:kbID].location == NSNotFound) {
                [installedKbs appendString:[NSString stringWithFormat:@"%@,", kbID]];
            }
        }
    }
    
    NSInteger lastIndex = [installedKbs length] - 1;
    if (lastIndex > 0)
        [installedKbs deleteCharactersInRange:NSMakeRange(lastIndex, 1)];
    
    if ([installedKbs isEqualToString:@""])
        installedKbs = [NSMutableString stringWithString:currentKbID];
    
    NSString *urlStr = [NSString stringWithFormat:@"http://keyman.com/iphone-and-ipad/app/?active=%@&installed=%@", currentKbID, installedKbs];
    NSURL *url = [NSURL URLWithString:urlStr];
    [webView loadRequest:[NSURLRequest requestWithURL:url]];
}

- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error {
    //NSLog([NSString stringWithFormat:@"Error : %@", error]);
}

- (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType {
    if (navigationType == UIWebViewNavigationTypeLinkClicked)
    {
        [[UIApplication sharedApplication] openURL:[request URL]];
        return NO;
    }

    return YES;
}

- (void)reloadKeymanHelp {
    [self checkNetworkStatus:nil];
}

@end
