//
//  SetUpViewController.h
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 14/10/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SetUpViewController : UIViewController <UIWebViewDelegate> {
    UIWebView *webView;
}

@property (strong, nonatomic) IBOutlet UIWebView *webView;

@end
