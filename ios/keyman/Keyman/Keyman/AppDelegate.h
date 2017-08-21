//
//  AppDelegate.h
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MainViewController;

@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;
@property (strong, nonatomic) UIWindow *overlayWindow;
@property (strong, nonatomic) UINavigationController *navigationController;
@property (strong, nonatomic) MainViewController *viewController;

+ (NSUserDefaults *)activeUserDefaults;
+ (BOOL)isKeymanEnabledSystemWide;
+ (CGFloat)statusBarHeight;

@end
