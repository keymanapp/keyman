//
//  ViewController.m
//  FirstVoices
//
//  Created by Serkan Kurt on 17/11/2015.
//  Copyright © 2015 FirstVoices. All rights reserved.
//

#import "ViewController.h"
#import "KeymanEngine/KeymanEngine.h"
#import "FVShared.h"

@interface ViewController ()
@property (nonatomic, weak) IBOutlet UIWebView *webView;
@property (nonatomic, weak) IBOutlet UIView *statusBarBackground;
@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.

    [KMManager setApplicationGroupIdentifier: FVGroupID];
    
    #ifdef DEBUG
    [[KMManager sharedInstance] setDebugPrintingOn:YES];
    #endif
    
    NSArray *kbPaths = [[NSBundle mainBundle] pathsForResourcesOfType:@"js" inDirectory:nil];
    for (NSString *kbPath in kbPaths)
        [[KMManager sharedInstance] preloadLanguageFileAtPath:kbPath shouldOverwrite:YES];
    
    self.webView.delegate = self;
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"setup" ofType:@"html" inDirectory:nil];
    [self.webView loadRequest:[NSURLRequest requestWithURL:[NSURL fileURLWithPath:filePath]]];
}

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    [self.navigationController setNavigationBarHidden:YES];
    [self setStatusBarBackgroundWithOrientation:[[UIApplication sharedApplication] statusBarOrientation]];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    [self setStatusBarBackgroundWithOrientation:toInterfaceOrientation];
}

- (void)setStatusBarBackgroundWithOrientation:(UIInterfaceOrientation)orientation {
    if (UIInterfaceOrientationIsPortrait(orientation))
        [self.statusBarBackground setHidden:NO];
    else
        [self.statusBarBackground setHidden:([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)];
}

- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error {
    //NSLog([NSString stringWithFormat:@"Error : %@", error]);
}

- (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType {
    NSString *fragment = [[request URL] fragment];
    if (fragment != nil && [fragment rangeOfString:@"showKeyboards"].location != NSNotFound) {
        [self performSegueWithIdentifier:@"Keyboards" sender:nil];
        return NO;
    }
    
    if (navigationType == UIWebViewNavigationTypeLinkClicked) {
        [[UIApplication sharedApplication] openURL:[request URL]];
        return NO;
    }
    
    return YES;
}

@end
