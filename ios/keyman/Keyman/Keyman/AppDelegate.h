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

@property (strong, nonatomic) UIWindow * _Nullable window;
@property (strong, nonatomic) UIWindow * _Nonnull overlayWindow;
@property (strong, nonatomic) UINavigationController * _Nullable navigationController;
@property (strong, nonatomic) MainViewController * _Nullable viewController;

+ (NSUserDefaults * _Nonnull)activeUserDefaults;
+ (BOOL)isKeymanEnabledSystemWide;
+ (CGFloat)statusBarHeight;

@end
