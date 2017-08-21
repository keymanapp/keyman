//
//  AppDelegate.m
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "AppDelegate.h"
#import "MainViewController.h"

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    [NSThread sleepForTimeInterval:1.0];
    
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    [self overlayWindow];
    
    // Override point for customization after application launch.
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        self.viewController = [[MainViewController alloc] initWithNibName:@"MainViewController_iPhone" bundle:nil];
    }
    else {
        self.viewController = [[MainViewController alloc] initWithNibName:@"MainViewController_iPad" bundle:nil];
    }
    
    self.navigationController = [[UINavigationController alloc] initWithRootViewController:self.viewController];
    [self.navigationController.navigationBar setTranslucent:NO]; // Navigation bar became translucent by default in iOS7 SDK, however we don't want it to be translucent.
    if ([[[UIDevice currentDevice] systemVersion] intValue] < 7 && [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        [self.navigationController.navigationBar setBarStyle:UIBarStyleBlack];
        [[UINavigationBar appearance] setTintColor:[UIColor colorWithRed:167.0f/255.0f green:172.0f/255.0f blue:178.0f/255.0f alpha:1.0]];
    }
    
    self.window.rootViewController = self.navigationController;
    [self.window makeKeyAndVisible];
    return YES;
}

- (BOOL)application:(UIApplication *)application openURL:(NSURL *)url sourceApplication:(NSString *)sourceApplication annotation:(id)annotation {
    [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanLaunchedFromUrlNotification
                                                        object:self
                                                      userInfo:[NSDictionary dictionaryWithObjectsAndKeys:
                                                                url, kKeymanUrlKey,
                                                                nil]];
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    _overlayWindow = nil;
    [[KMManager sharedInstance] unregisterCustomFonts];
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    [userData setObject:[self.viewController textView].text forKey:kKeymanUserTextKey];
    [userData setObject:[NSString stringWithFormat:@"%f", [self.viewController textSize]] forKey:kKeymanUserTextSizeKey];
    [userData synchronize];
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
    [self performSelector:@selector(registerCustomFonts) withObject:nil afterDelay:1.0];
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application {
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

- (UIWindow *)overlayWindow {
    if (_overlayWindow == nil) {
        _overlayWindow = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
        [_overlayWindow setRootViewController:[[UIViewController alloc] init]];
        [_overlayWindow setAutoresizingMask:UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight];
        [_overlayWindow setAutoresizesSubviews:YES];
        [_overlayWindow setBackgroundColor:[UIColor colorWithWhite:0.0 alpha:0.75]];
        [_overlayWindow setWindowLevel:UIWindowLevelStatusBar+1];
        [_overlayWindow setUserInteractionEnabled:YES];
        [_overlayWindow setHidden:YES];
    }
    
    // Workaround to display overlay window above keyboard
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 9) {
        NSArray *windows = [[UIApplication sharedApplication] windows];
        UIWindow *lastWindow = (UIWindow *)[windows lastObject];
        [_overlayWindow setWindowLevel:lastWindow.windowLevel+1];
    }
    
    return _overlayWindow;
}

- (void)registerCustomFonts {
    [[KMManager sharedInstance] registerCustomFonts];
}

+ (NSUserDefaults *)activeUserDefaults {
    NSUserDefaults *userDefaults = nil;
    if ([[NSUserDefaults standardUserDefaults] respondsToSelector:@selector(initWithSuiteName:)])
        userDefaults = [[NSUserDefaults alloc] initWithSuiteName:@"group.KM4I"];
    
    if (userDefaults == nil)
        userDefaults = [NSUserDefaults standardUserDefaults];
    
    return userDefaults;
}

+ (BOOL)isKeymanEnabledSystemWide {
    NSArray *keyboards = [[[NSUserDefaults standardUserDefaults] dictionaryRepresentation] objectForKey:@"AppleKeyboards"];
    for (NSString *keyboard in keyboards) {
        if ([keyboard isEqualToString:@"Tavultesoft.Keyman.SWKeyboard"])
            return YES;
    }
    
    return NO;
}

+ (CGFloat)statusBarHeight {
    CGFloat height = 0;
    CGRect statusBarFrame = [[UIApplication sharedApplication] statusBarFrame];
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 8) {
        height = CGRectGetHeight(statusBarFrame);
    }
    else {
        if (UIInterfaceOrientationIsPortrait(orientation))
            height = CGRectGetHeight(statusBarFrame);
        else
            height = CGRectGetWidth(statusBarFrame);
    }
    
    return height;
}

@end
