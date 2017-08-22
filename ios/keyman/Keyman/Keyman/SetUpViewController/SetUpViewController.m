//
//  SetUpViewController.m
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 14/10/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "SetUpViewController.h"
#import "AppDelegate.h"
#import "Keyman.h"
#import "Reachability.h"

@interface SetUpViewController ()
@property (strong, nonatomic) Reachability *networkReachable;
@end

@implementation SetUpViewController
@synthesize webView, networkReachable;

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view from its nib.
    
    UINavigationBar *navBar = (UINavigationBar *)[self.view viewWithTag:786586];
    UIBarButtonItem *doneButton = navBar.topItem.rightBarButtonItem;
    [doneButton setTarget:self];
    [doneButton setAction:@selector(dismissView)];
    
    self.webView.delegate = self;
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(checkNetworkStatus:) name:kReachabilityChangedNotification object:nil];
    networkReachable = [Reachability reachabilityWithHostName:@"www.keyman.com"];
    [networkReachable startNotifier];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)dismissView {
    [self dismissViewControllerAnimated:YES completion:nil];
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
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"setup" ofType:@"html" inDirectory:nil];
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
    
    NSString *urlStr = [NSString stringWithFormat:@"http://keyman.com/iphone-and-ipad/app/systemkeyboard/index.php?active=%@&installed=%@", currentKbID, installedKbs];
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

@end
