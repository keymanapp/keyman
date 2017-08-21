//
//  KMBookmarksViewController.h
//  Keyman
//
//  Created by Serkan Kurt on 19/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "KMWebBrowserViewController.h"

@interface KMBookmarksViewController : UIViewController <UITableViewDelegate, UITableViewDataSource>

@property (weak, nonatomic) KMWebBrowserViewController *webBrowser;

@end
