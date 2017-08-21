//
//  InfoViewController.h
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@class Reachability;

@interface InfoViewController : UIViewController <UIWebViewDelegate> {
    UIWebView *webView;
}

@property (strong, nonatomic) IBOutlet UIWebView *webView;

- (void)reloadKeymanHelp;

@end
