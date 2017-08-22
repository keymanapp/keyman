//
//  KMWebBrowserViewController.h
//  Keyman
//
//  Created by Serkan Kurt on 9/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KMWebBrowserViewController : UIViewController <UIWebViewDelegate>

@property (strong, nonatomic) IBOutlet UIWebView *webView;
@property (strong, nonatomic) NSString *fontFamily;

@end
