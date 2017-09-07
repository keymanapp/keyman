//
//  MainViewController.m
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "MainViewController.h"
#import "AppDelegate.h"
#import "KMDropDownListView.h"
#import "UIImage+Helpers.h"
#import "CoreText/CTFontManager.h"
#import <QuartzCore/QuartzCore.h>
#import <Keyman-Swift.h>

// Internal strings
NSString *const baseUri = @"http://r.keymanweb.com/20/fonts/get_mobileconfig.php?id=";
NSString *const kKeymanProfileKey = @"profile";
NSString *const kKeymanCheckedProfilesKey = @"CheckedProfiles";

// External strings
NSString *const kKeymanUserTextKey = @"UserText";
NSString *const kKeymanUserTextSizeKey = @"UserTextSize";
#ifdef FREE
NSString *const kKeymanDontShowGetStartedKey = @"DontShowGetStarted";
#else
NSString *const kKeymanDontShowGetStartedKey = @"DontShowGetStarted-Pro";
#endif
NSString *const kKeymanLaunchedFromUrlNotification = @"kKeymanLaunchedFromUrlNotification";
NSString *const kKeymanUrlKey = @"url";

CGFloat const minTextSize = 9.0;
CGFloat const maxTextSize = 72.0;

NSInteger const getStartedViewTag = 7183;
NSInteger const activityIndicatorViewTag = 6573;
NSInteger const dropDownListTag = 686876;

NSUInteger const minIOSVersion4FontInstall = 7;

@interface MainViewController ()

@property (strong, nonatomic) GetStartedViewController *getStartedVC;
@property (strong, nonatomic) InfoViewController *infoView;
@property (strong, nonatomic) UIPopoverController *popover;
@property (strong, nonatomic) UIActionSheet *actionSheet;
@property (strong, nonatomic) UISlider *textSizeController;

@property (strong, nonatomic) UIBarButtonItem *moreButton;
@property (strong, nonatomic) UIBarButtonItem *infoButton;
@property (strong, nonatomic) UIBarButtonItem *getStartedButton;
@property (strong, nonatomic) UIBarButtonItem *trashButton;
@property (strong, nonatomic) UIBarButtonItem *textSizeButton;
@property (strong, nonatomic) UIBarButtonItem *browserButton;
@property (strong, nonatomic) UIBarButtonItem *actionButton;

//@property (strong, nonatomic) NSMutableDictionary *profileNamesByKeyboardID;
@property (strong, nonatomic) NSMutableArray *profiles2Install;
@property (strong, nonatomic) NSMutableArray *checkedProfiles;
@property (strong, nonatomic) NSString *profileName;
@property (strong, nonatomic) NSURL *launchUrl;
@property (strong, nonatomic) NSDictionary *keyboard2Download;
@property (strong, nonatomic) NSDictionary *customKeyboard2Download;
@property (assign, nonatomic) BOOL wasKeyboardVisible;

@end

@implementation MainViewController
@synthesize textView, textSize, infoView, popover, actionSheet, textSizeController;
@synthesize moreButton, getStartedButton, infoButton, trashButton;
@synthesize textSizeButton, actionButton, browserButton;
@synthesize profiles2Install, checkedProfiles, profileName, launchUrl;
@synthesize keyboard2Download, customKeyboard2Download, wasKeyboardVisible;

- (AppDelegate *)appDelegate {
    return (AppDelegate *)[[UIApplication sharedApplication] delegate];
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil]) {
        // Setup Notifications
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillShow:) name:UIKeyboardWillShowNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDidShow:) name:UIKeyboardWillShowNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillHide:) name:UIKeyboardWillHideNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardLoaded:) name:kKeymanKeyboardLoadedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(languagesUpdated:) name:kKeymanLanguagesUpdatedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(languagesDownloadFailed:) name:kKeymanLanguagesDownloadFailedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardPickerDismissed:) name:kKeymanKeyboardPickerDismissedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadStarted:) name:kKeymanKeyboardDownloadStartedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFinished:) name:kKeymanKeyboardDownloadCompletedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFailed:) name:kKeymanKeyboardDownloadFailedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardRemoved:) name:kKeymanKeyboardRemovedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(launchedFromUrl:) name:kKeymanLaunchedFromUrlNotification object:nil];
        //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(registeredFontsChanged:) name:(__bridge NSString *)kCTFontManagerRegisteredFontsChangedNotification object:nil];
        
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];

    if ([self respondsToSelector:@selector(setExtendedLayoutIncludesOpaqueBars:)])
        [self setExtendedLayoutIncludesOpaqueBars:YES];
    if ([self respondsToSelector:@selector(setAutomaticallyAdjustsScrollViewInsets:)])
        [self setAutomaticallyAdjustsScrollViewInsets:NO];
    
    iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    NSArray *systemFonts = [NSArray arrayWithArray:[UIFont familyNames]];
    
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    if (iosVersion >= 8) {
        screenWidth = screenRect.size.width;
        screenHeight = screenRect.size.height;
    }
    else {
        if (UIInterfaceOrientationIsPortrait(orientation)) {
            screenWidth = screenRect.size.width;
            screenHeight = screenRect.size.height;
        }
        else {
            screenWidth = screenRect.size.height;
            screenHeight = screenRect.size.width;
        }
    }
    
    // Setup Keyman Manager & fetch keyboards list
    updateStatus = 0;
    [KMManager setApplicationGroupIdentifier:@"group.KM4I"];
    
    #ifdef DEBUG
    [[KMManager sharedInstance] setDebugPrintingOn:YES];
    #endif
    
    #ifdef FREE
    [[KMManager sharedInstance] setCanRemoveDefaultKeyboard:NO];
    #else
    [[KMManager sharedInstance] setCanRemoveDefaultKeyboard:YES];
    #endif
    [[KMManager sharedInstance] fetchKeyboardsList];
    
    UIColor *bgColor = [UIColor colorWithRed:1.0 green:1.0 blue:207.0/255.0 alpha:1.0];
    [self.view setBackgroundColor:bgColor];
    
    // Check for configuration profiles/fonts to install for iOS 7+
    if (iosVersion >= minIOSVersion4FontInstall) {
        NSMutableDictionary *kmFonts = [NSMutableDictionary dictionaryWithDictionary:[[KMManager sharedInstance] keymanFonts]];
        [kmFonts removeObjectForKey:@"keymanweb-osk.ttf"];
        NSArray *allKeys = [NSArray arrayWithArray:[kmFonts allKeys]];
        NSUInteger length = [allKeys count];
        NSMutableDictionary *profilesByFontName = [NSMutableDictionary dictionaryWithCapacity:length];
        for (int i = 0; i < length; i++) {
            NSString *key = [allKeys objectAtIndex:i];
            NSString *value = [[kmFonts objectForKey:key] objectForKey:kKeymanFontNameKey];
            NSUInteger index = [key rangeOfString:@"." options:NSBackwardsSearch].location;
            NSString *type = [key substringFromIndex:index];
            [profilesByFontName setObject:[key stringByReplacingOccurrencesOfString:type withString:@".mobileconfig"] forKey:value];
        }
        
        NSMutableArray *customFonts = [NSMutableArray arrayWithArray:[UIFont familyNames]];
        [customFonts removeObjectsInArray:systemFonts];
        [customFonts removeObjectsInArray:@[@"KeymanwebOsk"]];
        
        NSUserDefaults *userData = [AppDelegate activeUserDefaults];
        checkedProfiles = [[userData objectForKey:kKeymanCheckedProfilesKey] mutableCopy];
        if (checkedProfiles == nil)
            checkedProfiles = [NSMutableArray arrayWithCapacity:0];
        
        profiles2Install = [NSMutableArray arrayWithCapacity:[customFonts count]];
        for (NSString *name in customFonts) {
            [profiles2Install addObject:[profilesByFontName objectForKey:[[UIFont fontNamesForFamilyName:name] objectAtIndex:0]]];
        }
    }
    
    // Setup NavigationBar
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        CGFloat size = screenRect.size.height > screenRect.size.width?screenRect.size.height:screenRect.size.width;
        if (size > 568.0) {
            // Navbar for iPhone 6 & 6 Plus
            UIImage *bgImg = nil;
            if (UIInterfaceOrientationIsPortrait(orientation))
                bgImg = [[UIImage imageNamed:@"navbar-Portrait.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            else
                bgImg = [[UIImage imageNamed:@"navbar-Landscape-568h.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            
            [self.navigationController.navigationBar setBackgroundImage:bgImg
                                                          forBarMetrics:UIBarMetricsDefault];
            //[self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Landscape-568h.png"] forBarMetrics:UIBarMetricsCompact];
        }
        else if (size == 568) {
            // Navbar for iPhone and iPod Touch with 4" Display
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Landscape-568h.png"] forBarMetrics:UIBarMetricsCompact];
        }
        else if (size < 568.0) {
            // Navbar for iPhone and iPod Touch with 3.5" Display
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Landscape.png"] forBarMetrics:UIBarMetricsCompact];
        }
    }
    else {
        // Navbar for iPad
        if (UIInterfaceOrientationIsPortrait(orientation))
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
        else
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Landscape.png"] forBarMetrics:UIBarMetricsDefault];
    }
    
    // Setup Keyman TextView
    textSize = 16.0;
    textView = [[KMTextView alloc] initWithFrame:screenRect];
    [textView setKeymanDelegate:self];
    [textView setViewController:self];
    [textView setBackgroundColor:bgColor];
    [textView setScrollEnabled:YES];
    [textView setUserInteractionEnabled:YES];
    textView.font = [textView.font fontWithSize:textSize];
    [self.view addSubview:textView];
    [textView setEditable:NO];
    
    // Setup Info View
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        infoView = [[InfoViewController alloc] initWithNibName:@"InfoViewController_iPhone" bundle:nil];
    }
    else {
        infoView = [[InfoViewController alloc] initWithNibName:@"InfoViewController_iPad" bundle:nil];
        textSize *= 2;
        textView.font = [textView.font fontWithSize:textSize];
    }
    
    infoView.view.hidden = YES;
    [infoView.view setBackgroundColor:bgColor];
    [self.view addSubview:infoView.view];
    
    [self resizeViewsWithKeyboardVisible:[textView isFirstResponder]];
    
    // Setup Text Size Controller
    textSizeController = [[UISlider alloc] init];
    [textSizeController setFrame:CGRectZero];
    [textSizeController setMinimumValue:0.0];
    [textSizeController setMaximumValue:(maxTextSize - minTextSize)];
    [textSizeController setValue:(textSize - minTextSize)];
    UIImage *textSizeUp = [[UIImage imageNamed:@"textsize_up.png"] scaleToSize:CGSizeMake(20, 20)];
    [textSizeController setMaximumValueImage:textSizeUp];
    UIImage *textSizeDown = [[UIImage imageNamed:@"textsize_up.png"] scaleToSize:CGSizeMake(15, 15)];
    [textSizeController setMinimumValueImage:textSizeDown];
    [textSizeController addTarget:self action:@selector(sliderValueChanged:) forControlEvents:UIControlEventValueChanged];
    
    [self setNavBarButtons];
}

- (void)viewWillDisappear:(BOOL)animated {
    [super viewWillDisappear:animated];
    wasKeyboardVisible = [textView isFirstResponder];
}

- (void)setNavBarButtons {
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    UIBarButtonItem *fixedSpace = [[UIBarButtonItem alloc]
                                   initWithBarButtonSystemItem:UIBarButtonSystemItemFixedSpace
                                   target:nil
                                   action:nil];
    
    CGFloat imageScaleF = 0.9;
    moreButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"more.png"]
                                  highlightedImage:[UIImage imageNamed:@"more-selected.png"]
                                        imageScale:imageScaleF
                                            action:@selector(showDropDownMenu:)
                                       orientation:orientation];
    moreButton.title = @"More";
    
    infoButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"724-info.png"]
                                  highlightedImage:[UIImage imageNamed:@"724-info-selected.png"]
                                        imageScale:imageScaleF
                                            action:@selector(infoButtonClick:)
                                       orientation:orientation];
    infoButton.title = @"Info";

    getStartedButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"887-notepad.png"]
                                        highlightedImage:[UIImage imageNamed:@"887-notepad-selected.png"]
                                              imageScale:imageScaleF
                                                  action:@selector(showGetStartedView:)
                                             orientation:orientation];
    getStartedButton.title = @"Get Started";
    
    trashButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"711-trash.png"]
                                   highlightedImage:[UIImage imageNamed:@"711-trash-selected.png"]
                                         imageScale:imageScaleF
                                             action:@selector(trashButtonClick:)
                                        orientation:orientation];
    trashButton.title = @"Clear Text";
    
    textSizeButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"textsize.png"]
                                      highlightedImage:[UIImage imageNamed:@"textsize_selected.png"]
                                            imageScale:imageScaleF
                                                action:@selector(textSizeButtonClick:)
                                           orientation:orientation];
    textSizeButton.title = @"Text Size";
    
    #ifndef FREE
    browserButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"786-browser.png"]
                                     highlightedImage:[UIImage imageNamed:@"786-browser-selected.png"]
                                           imageScale:1.0
                                               action:@selector(showKMWebBrowserView:)
                                          orientation:orientation];
    browserButton.title = @"Browser";
    #endif
    
    actionButton = [self createNavBarButtonWithImage:[UIImage imageNamed:@"702-share.png"]
                                    highlightedImage:[UIImage imageNamed:@"702-share-selected.png"]
                                          imageScale:imageScaleF
                                              action:@selector(actionButtonClick:)
                                         orientation:orientation];
    actionButton.title = @"Share";
    
    CGFloat scaleF = ([UIScreen mainScreen].scale == 2.0)?2.0:1.5;
    scaleF *= ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?1.0:2.0;
    fixedSpace.width = 10*scaleF;
    
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        #ifdef FREE
        [[self navigationItem] setRightBarButtonItems:@[moreButton, fixedSpace, textSizeButton, fixedSpace, actionButton]];
        #else
        [[self navigationItem] setRightBarButtonItems:@[moreButton, fixedSpace, browserButton, fixedSpace, actionButton]];
        #endif
    }
    else {
        #ifdef FREE
        [[self navigationItem] setRightBarButtonItems:@[infoButton, fixedSpace, getStartedButton, fixedSpace, trashButton, fixedSpace, textSizeButton, fixedSpace, actionButton]];
        #else
        [[self navigationItem] setRightBarButtonItems:@[infoButton, fixedSpace, getStartedButton, fixedSpace, trashButton, fixedSpace, textSizeButton, fixedSpace, browserButton, fixedSpace, actionButton]];
        #endif
    }
}

- (UIBarButtonItem *)createNavBarButtonWithImage:(UIImage *)image highlightedImage:(UIImage *)imageHighlighted imageScale:(CGFloat)scaleF action:(SEL)selector orientation:(UIInterfaceOrientation)orientation {
    UIImage *icon = [image scaleToSize:CGSizeMake(image.size.width*scaleF, image.size.height*scaleF)];
    UIImage *iconHighlighted = [imageHighlighted scaleToSize:CGSizeMake(imageHighlighted.size.width*scaleF, imageHighlighted.size.height*scaleF)];
    
    if ([UIImage instancesRespondToSelector:@selector(imageWithRenderingMode:)]) { // This fixes icon display issues for iOS 7 SDK
        icon = [icon imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal];
        iconHighlighted = [iconHighlighted imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal];
    }
    
    CGFloat vAdjPort = -2.6;
    CGFloat vAdjLscpe = -1.6;
    if ([UIScreen mainScreen].scale == 2.0)
        vAdjPort = -3.6;
    
    CGFloat vAdj = UIInterfaceOrientationIsPortrait(orientation)?vAdjPort:vAdjLscpe;
    CGFloat navBarHeight = [self navBarHeight];
    CGFloat y = (navBarHeight - icon.size.height)/2.0 + vAdj;
    
    UIButton *customButton = [UIButton buttonWithType:UIButtonTypeCustom];
    [customButton setImage:icon forState:UIControlStateNormal];
    [customButton setImage:iconHighlighted forState:UIControlStateHighlighted];
    [customButton setFrame:CGRectMake(0.0, y, icon.size.width, icon.size.height)];
    [customButton addTarget:self action:selector
                       forControlEvents:UIControlEventTouchUpInside];
    UIView *containerView = [[UIView alloc] initWithFrame:CGRectMake(0.0, 0.0, icon.size.width, navBarHeight)];
    [containerView setBackgroundColor:[UIColor clearColor]];
    [containerView addSubview:customButton];
    UIBarButtonItem *navbarButton = [[UIBarButtonItem alloc] initWithCustomView:containerView];
    return navbarButton;
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];

    if (wasKeyboardVisible)
        [textView becomeFirstResponder];
    
    if (![textView isEditable])
        [self showActivityIndicator];
    else if ([self shouldShowGetStarted])
        [self performSelector:@selector(showGetStartedView:) withObject:nil afterDelay:1.0];
}

- (void)viewDidUnload {
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    self.textView = nil;
    self.infoView = nil;
    self.popover = nil;
    self.actionSheet = nil;
    self.textSizeController = nil;
}

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    UIInterfaceOrientation orientation = toInterfaceOrientation;
    for (UIView *view in [self.overlayWindow subviews]) {
        [UIView animateWithDuration:duration
                              delay:0.0
                            options:(UIViewAnimationOptionTransitionNone)
                         animations:^{[view setTransform:[self transformForOrientation:orientation]];}
                         completion:nil];
    }
    
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        CGRect screenRect = [[UIScreen mainScreen] bounds];
        CGFloat size = screenRect.size.height > screenRect.size.width?screenRect.size.height:screenRect.size.width;
        if (size > 568.0) {
            // Navbar for iPhone 6 & 6 Plus
            UIImage *bgImg = nil;
            if (UIInterfaceOrientationIsPortrait(orientation))
                bgImg = [[UIImage imageNamed:@"navbar-Portrait.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            else
                bgImg = [[UIImage imageNamed:@"navbar-Landscape-568h.png"]
                         resizableImageWithCapInsets:UIEdgeInsetsMake(0, 0, 0, 0)
                         resizingMode:UIImageResizingModeStretch];
            
            [self.navigationController.navigationBar setBackgroundImage:bgImg
                                                          forBarMetrics:UIBarMetricsDefault];
        }
    }
    else {
        if (UIInterfaceOrientationIsPortrait(orientation))
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Portrait.png"] forBarMetrics:UIBarMetricsDefault];
        else
            [self.navigationController.navigationBar setBackgroundImage:[UIImage imageNamed:@"navbar-Landscape.png"] forBarMetrics:UIBarMetricsDefault];
    }
    
    [popover dismissPopoverAnimated:NO];
    [self dismissDropDownMenu];
    [actionSheet dismissWithClickedButtonIndex:actionSheet.cancelButtonIndex animated:NO];
}

- (void)didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation {
    if ([infoView.view isHidden])
        [self setNavBarButtons];
    [self resizeViewsWithKeyboardVisible:[self.textView isFirstResponder]];
}

- (void)resizeViewsWithKeyboardVisible:(BOOL)keyboardVisible {
    // Resize textView and infoView
    CGRect mainScreen = [[UIScreen mainScreen] bounds];
    CGFloat margin = 2.0;
    CGFloat width, height;
    CGFloat barHeights = [AppDelegate statusBarHeight] + [self navBarHeight];
    CGFloat kbHeight = keyboardVisible?CGRectGetHeight(textView.inputView.frame):0;

    if (iosVersion >= 8) {
        width = mainScreen.size.width;
        height = mainScreen.size.height - barHeights - kbHeight;
    }
    else {
        UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
        if (UIInterfaceOrientationIsPortrait(orientation)) {
            width = CGRectGetWidth(mainScreen);
            height = CGRectGetHeight(mainScreen) - barHeights - kbHeight;
        }
        else {
            width = CGRectGetHeight(mainScreen);
            height = CGRectGetWidth(mainScreen) - barHeights - kbHeight;
        }
    }
    
    [self.textView setFrame:CGRectMake(margin, ((iosVersion >= 7)?barHeights:0) + margin, width - 2*margin, height - 2*margin)];
    [self.infoView.view setFrame:CGRectMake(0, (iosVersion >= 7)?barHeights:0, width, height + kbHeight)];
}

- (CGFloat)navBarWidth {
    CGRect navBarFrame = self.navigationController.navigationBar.frame;
    return CGRectGetWidth(navBarFrame);
}

- (CGFloat)navBarHeight {
    CGRect navBarFrame = self.navigationController.navigationBar.frame;
    return CGRectGetHeight(navBarFrame);
}

#pragma mark - Keyboard notifications

- (void)keyboardWillShow:(NSNotification *)notification {
    [self dismissDropDownMenu];
    [self resizeViewsWithKeyboardVisible:YES];
}

- (void)keyboardDidShow:(NSNotification *)notification {
    // Workaround to display overlay window above keyboard
    if (iosVersion >= 9) {
        NSArray *windows = [[UIApplication sharedApplication] windows];
        UIWindow *lastWindow = (UIWindow *)[windows lastObject];
        [self.overlayWindow setWindowLevel:lastWindow.windowLevel+1];
    }
}

- (void)keyboardWillHide:(NSNotification *)notification {
    [self resizeViewsWithKeyboardVisible:NO];
}

#pragma mark - Keyman notifications

- (void)keyboardLoaded:(NSNotification *)notification {
    [self startTimer];
}

- (void)keyboardChanged:(NSNotification *)notification {
    BOOL listCheck = YES;
    if (didDownload) {
        listCheck = NO;
        didDownload = NO;
    }
    
    if (iosVersion >= minIOSVersion4FontInstall) {
        NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
        NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
        NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
        [self checkProfileForKeyboardID:kbID languageID:langID doListCheck:listCheck];
    }
}

- (void)keyboardDownloadStarted:(NSNotification *)notification {
    if (self.launchUrl != nil) {
        [self showActivityIndicator];
    }
}

- (void)keyboardDownloadFinished:(NSNotification *)notification {
    didDownload = YES;
    if (self.launchUrl != nil) {
        NSUserDefaults *userData = [AppDelegate activeUserDefaults];
        NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
        if (userKeyboards == nil || ![userKeyboards count]) {
            BOOL isRTL = [kKeymanDefaultKeyboardRTL isEqualToString:@"Y"]?YES:NO;
            [[KMManager sharedInstance] addKeyboardWithID:kKeymanDefaultKeyboardID languageID:kKeymanDefaultLanguageID keyboardName:kKeymanDefaultKeyboardName languageName:kKeymanDefaultLanguageName isRTL:isRTL isCustom:NO font:kKeymanDefaultKeyboardFont oskFont:nil];
        }
        
        [self performSelector:@selector(dismissActivityIndicator) withObject:nil afterDelay:1.0];
        NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
        NSString *kbID = nil;
        NSString *langID = nil;
        if ([kbInfo isKindOfClass:[NSDictionary class]]) {
            kbID = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardIdKey];
            langID = [(NSDictionary *)kbInfo objectForKey:kKeymanLanguageIdKey];
            if ([self.launchUrl.query rangeOfString:@"url="].location == NSNotFound) {
                NSString *dictKey = [NSString stringWithFormat:@"%@_%@", langID, kbID];
                NSDictionary *kbDict = [[[KMManager sharedInstance] keyboardsDictionary] objectForKey:dictKey];
                if (kbDict != nil) {
                    NSString *kbName = [kbDict objectForKey:kKeymanKeyboardNameKey];
                    NSString *langName = [kbDict objectForKey:kKeymanLanguageNameKey];
                    BOOL isRTL = [[kbDict objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
                    NSString *font = [kbDict objectForKey:kKeymanFontKey];
                    NSString *oskFont = [kbDict objectForKey:kKeymanOskFontKey];
                    [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:NO font:font oskFont:oskFont];
                    [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
                }
            }
            else {
                NSString *kbName = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardNameKey];
                NSString *langName = [(NSDictionary *)kbInfo objectForKey:kKeymanLanguageNameKey];
                BOOL isRTL = [[(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
                BOOL isCustom = [[(NSDictionary *)kbInfo objectForKey:kKeymanCustomKeyboardKey] isEqualToString:@"Y"]?YES:NO;
                NSString *font = [(NSDictionary *)kbInfo objectForKey:kKeymanFontKey];
                NSString *oskFont = [(NSDictionary *)kbInfo objectForKey:kKeymanOskFontKey];
                [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:isCustom font:font oskFont:oskFont];
                [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
            }
        }
        else {
            for (NSDictionary *info in (NSArray *)kbInfo) {
                kbID = [info objectForKey:kKeymanKeyboardIdKey];
                langID = [info objectForKey:kKeymanLanguageIdKey];
                NSString *kbName = [info objectForKey:kKeymanKeyboardNameKey];
                NSString *langName = [info objectForKey:kKeymanLanguageNameKey];
                BOOL isRTL = [[info objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
                BOOL isCustom = [[info objectForKey:kKeymanCustomKeyboardKey] isEqualToString:@"Y"]?YES:NO;
                NSString *font = [info objectForKey:kKeymanFontKey];
                NSString *oskFont = [info objectForKey:kKeymanOskFontKey];
                [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:isCustom font:font oskFont:oskFont];
                [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
            }
        }
        
        self.launchUrl = nil;
    }
}

- (void)keyboardDownloadFailed:(NSNotification *)notification {
    if (self.launchUrl != nil) {
        [self performSelector:@selector(dismissActivityIndicator) withObject:nil afterDelay:1.0];
        NSError *error = [[notification userInfo] objectForKey:NSUnderlyingErrorKey];
        [self showAlertWithTitle:@"Keyboard Download Error"
                         message:error.localizedDescription
               cancelButtonTitle:@"OK"
               otherButtonTitles:nil
                             tag:-1];

        self.launchUrl = nil;
    }
}

- (void)languagesUpdated:(NSNotification *)notification {
    updateStatus = 1;
}

- (void)languagesDownloadFailed:(NSNotification *)notification {
    updateStatus = -1;
}

- (void)keyboardPickerDismissed:(NSNotification *)notification {
    [textView becomeFirstResponder];
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad) {
        [self performSelector:@selector(showGetStartedView) withObject:nil afterDelay:1.0];
    }
}

- (void)showGetStartedView {
    if ([self shouldShowGetStarted])
        [self showGetStartedView:nil];
}

- (void)launchedFromUrl:(NSNotification *)notification {
    NSURL *url = [[notification userInfo] objectForKey:kKeymanUrlKey];
    if ([url query] != nil) {
        self.launchUrl = url;
        if (updateStatus > 0) {
            [self performActionFromUrl:self.launchUrl];
        }
    }
    else {
        self.launchUrl = nil;
    }
}

- (void)keyboardRemoved:(NSNotification *)notification {
    if (iosVersion >= minIOSVersion4FontInstall) {
        NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
        NSString *profile = [self profileNameWithJSONFont:[kbInfo objectForKey:kKeymanFontKey]];
        if (profile != nil) {
            [self.profiles2Install removeObjectsInArray:@[profile]];
            [self.profiles2Install addObject:profile];
            
            [self.checkedProfiles removeObjectsInArray:@[profile]];
            NSUserDefaults *userData = [AppDelegate activeUserDefaults];
            [userData setObject:self.checkedProfiles forKey:kKeymanCheckedProfilesKey];
            [userData synchronize];
        }
    }
}

/*
- (void)registeredFontsChanged:(NSNotification *)notification {
    NSLog(@"registeredFontsChanged: %@", notification);
    NSLog(@"%@", [UIFont familyNames]);
}
*/

- (void)infoButtonClick:(id)sender {
    [UIView setAnimationDelegate:self];
    SEL animSelector = NSSelectorFromString(@"animationDidStop:finished:context:");
    [UIView setAnimationDidStopSelector:animSelector];
    [UIView beginAnimations:nil context:nil];
    [UIView setAnimationDuration:0.75];
    [UIView setAnimationTransition:((textView.hidden == NO)?UIViewAnimationTransitionFlipFromRight:UIViewAnimationTransitionFlipFromLeft) forView:self.view cache:YES];
    
    if (textView.hidden == NO) {
        textView.hidden = YES;
        infoView.view.hidden = NO;
        [infoView reloadKeymanHelp];
    }
    else {
        textView.hidden = NO;
        infoView.view.hidden = YES;
    }
    
    [UIView commitAnimations];
    
    if (textView.hidden == NO) {
        [[self navigationItem] setTitleView:nil];
        [[self navigationItem] setRightBarButtonItem:nil];
        [self setNavBarButtons];
        if (wasKeyboardVisible)
            [self performSelector:@selector(showKeyboard) withObject:nil afterDelay:0.75];
        if ([self shouldShowGetStarted])
            [self performSelector:@selector(showGetStartedView:) withObject:nil afterDelay:0.75];
    }
    else {
        [self dismissDropDownMenu];
        [popover dismissPopoverAnimated:NO];
        [actionSheet dismissWithClickedButtonIndex:actionSheet.cancelButtonIndex animated:NO];
        wasKeyboardVisible = [textView isFirstResponder];
        [textView dismissKeyboard];
        
        [[self navigationItem] setRightBarButtonItems:nil];
        CGFloat vAdjPort = -2.6;
        CGFloat vAdjLscpe = -1.6;
        if ([UIScreen mainScreen].scale == 2.0)
            vAdjPort = -3.6;
        
        UIBarButtonItem *infoDoneButton = [[UIBarButtonItem alloc] initWithTitle:@"Done" style:UIBarButtonItemStylePlain target:self action:@selector(infoButtonClick:)];
        [infoDoneButton setBackgroundVerticalPositionAdjustment:vAdjPort forBarMetrics:UIBarMetricsDefault];
        [infoDoneButton setBackgroundVerticalPositionAdjustment:vAdjLscpe forBarMetrics:UIBarMetricsCompact];
        [[self navigationItem] setRightBarButtonItem:infoDoneButton];
        UILabel *versionLabel = [[UILabel alloc] init];
        NSString *version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleShortVersionString"];
        NSString *build = [[[NSBundle mainBundle] infoDictionary] objectForKey:(NSString *)kCFBundleVersionKey];
        [versionLabel setText:[NSString stringWithFormat:@"Version: %@ (build %@)", version, build]];
        [versionLabel setFont:[UIFont systemFontOfSize:9.0]];
        [versionLabel setTextAlignment:NSTextAlignmentRight];
        [versionLabel sizeToFit];
        CGRect frame = versionLabel.frame;
        [versionLabel setFrame:CGRectInset(frame, -8, 0)];
        [versionLabel setBackgroundColor:[UIColor clearColor]];
        [[self navigationItem] setTitleView:versionLabel];
    }
}

- (void)actionButtonClick:(id)sender {
    [self dismissDropDownMenu];
    [popover dismissPopoverAnimated:NO];
    [actionSheet dismissWithClickedButtonIndex:actionSheet.cancelButtonIndex animated:NO];
    
    if (NSClassFromString(@"UIActivityViewController")) {
        NSString *lrm = @"\u200e";
        NSString *rlm = @"\u200f";
        NSMutableString *mText = [textView.text mutableCopy];
        if ([mText rangeOfString:lrm].location == 0 || [mText rangeOfString:rlm].location == 0) {
            [mText deleteCharactersInRange:NSMakeRange(0, 1)];
        }
        
        ActivityItemProvider *activityProvider = [[ActivityItemProvider alloc]
            initWithText: mText font: textView.font];
        
        UISimpleTextPrintFormatter *printText = [[UISimpleTextPrintFormatter alloc] initWithText:textView.text];
        printText.font = textView.font;
        
        UIActivityViewController *activityVC = [[UIActivityViewController alloc] initWithActivityItems:@[activityProvider, printText] applicationActivities:nil];
        activityVC.excludedActivityTypes = [[NSArray alloc] initWithObjects:UIActivityTypeAssignToContact, UIActivityTypePostToWeibo, UIActivityTypeSaveToCameraRoll, nil];
        #ifdef FREE
        [activityVC setCompletionHandler:^(NSString *activityType, BOOL completed) {
            [textView setUserInteractionEnabled:YES];
        }];
        #else
        [activityVC setCompletionWithItemsHandler:^(NSString *activityType, BOOL completed, NSArray *returnedItems, NSError *activityError) {
            [textView setUserInteractionEnabled:YES];
        }];
        #endif
        
        [textView setUserInteractionEnabled:NO];
        if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
            [self presentViewController:activityVC animated:YES completion:nil];
        }
        else {
            popover = [[UIPopoverController alloc] initWithContentViewController:activityVC];
            [popover presentPopoverFromBarButtonItem:[[self.navigationItem rightBarButtonItems] objectAtIndex:8] permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];
        }
    }
}

- (void)textSizeButtonClick:(id)sender {
    [self dismissDropDownMenu];
    [popover dismissPopoverAnimated:NO];
    [actionSheet dismissWithClickedButtonIndex:actionSheet.cancelButtonIndex animated:NO];
    
    if (iosVersion >= 8) {
        UIViewController *textSizeVC = [[UIViewController alloc] init];
        textSizeVC.modalPresentationStyle = UIModalPresentationOverCurrentContext;
        [textSizeVC.view setBackgroundColor:[UIColor clearColor]];
        
        CGRect mframe = textSizeVC.view.frame;
        CGFloat w = 304;
        CGFloat h = 142;
        CGRect sframe = CGRectMake(mframe.size.width/2.0 - w/2.0, mframe.size.height, w, h);
        CGRect eframe = sframe;
        eframe.origin.y -= h + 8;
        UIView *containerView = [[UIView alloc] initWithFrame:sframe];
        containerView.autoresizingMask = UIViewAutoresizingFlexibleTopMargin|UIViewAutoresizingFlexibleLeftMargin|UIViewAutoresizingFlexibleRightMargin;
        [containerView setTag:1];
        [containerView setBackgroundColor:[UIColor whiteColor]];
        [containerView.layer setCornerRadius:4.0];
        
        UIButton *doneButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
        [doneButton addTarget:self action:@selector(dismissTextSizeVC:)
         
             forControlEvents:UIControlEventTouchUpInside];
        [doneButton setBackgroundColor:[UIColor whiteColor]];
        [doneButton setTitle:@"Done" forState:UIControlStateNormal];
        [doneButton.titleLabel setFont:[doneButton.titleLabel.font fontWithSize:21.0]];
        [doneButton setTitleColor:[UIColor colorWithRed:0.0 green:0.5 blue:1.0 alpha:1.0] forState:UIControlStateNormal];
        [doneButton.layer setCornerRadius:4.0];
        CGRect bframe = CGRectMake(0, h - 42, sframe.size.width, 42);
        [doneButton setFrame:bframe];
        [containerView addSubview:doneButton];
        
        CGRect lframe = CGRectMake(0, bframe.origin.y - 1, sframe.size.width, 1);
        UIView *borderLine = [[UIView alloc] initWithFrame:lframe];
        CGFloat c = 230.0/255.0;
        [borderLine setBackgroundColor:[UIColor colorWithRed:c green:c blue:c alpha:1.0]];
        [containerView addSubview:borderLine];
        
        CGRect tframe = CGRectMake(0, 0, bframe.size.width, 40);
        UILabel *textSizeTitle = [[UILabel alloc] initWithFrame:tframe];
        [textSizeTitle setTag:2];
        [textSizeTitle setBackgroundColor:[UIColor clearColor]];
        [textSizeTitle setText:[NSString stringWithFormat:@"Text size: %d", (int)textSize]];
        [textSizeTitle setTextAlignment:NSTextAlignmentCenter];
        [textSizeTitle setTextColor:[UIColor grayColor]];
        [textSizeTitle setFont:[textSizeTitle.font fontWithSize:14.0]];
        [containerView addSubview:textSizeTitle];
        
        CGRect sliderFrame = CGRectMake(12, 45, bframe.size.width - 24, 50);
        [textSizeController setFrame:sliderFrame];
        [containerView addSubview:textSizeController];
        
        [textSizeVC.view addSubview:containerView];
        if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
            wasKeyboardVisible = [textView isFirstResponder];
            [textView dismissKeyboard];
            [self presentViewController:textSizeVC animated:NO completion:^{
                [UIView animateWithDuration:0.25
                                      delay:0.0
                                    options:UIViewAnimationOptionCurveEaseIn
                                 animations:^{
                                     [containerView setFrame:eframe];
                                     [textSizeVC.view setBackgroundColor:[UIColor colorWithRed:0.0 green:0.0 blue:0.0 alpha:0.4]];
                                 }
                                 completion:nil
                 ];
            }];
        }
        else {
            UIViewController *vc = [[UIViewController alloc] init];
            [vc setModalPresentationStyle:UIModalPresentationPopover];
            [vc.popoverPresentationController setBarButtonItem:[[self.navigationItem rightBarButtonItems] objectAtIndex:6]];
            [vc setView:containerView];
            [vc setPreferredContentSize:containerView.frame.size];
            [self presentViewController:vc animated:YES completion:nil];
        }
    }
    else {
        wasKeyboardVisible = [textView isFirstResponder];
        [textView dismissKeyboard];
        actionSheet = [[UIActionSheet alloc] initWithTitle:@"" delegate:self
                                     cancelButtonTitle:@"Done"
                                destructiveButtonTitle:nil
                                     otherButtonTitles:@"", nil];
        [actionSheet setTitle:[NSString stringWithFormat:@"Text size: %d", (int)textSize]];
        [actionSheet addSubview:textSizeController];
        [actionSheet setTag:0];
        [actionSheet showFromBarButtonItem:[[self.navigationItem rightBarButtonItems] objectAtIndex:6] animated:YES];
    }
}

- (void)dismissTextSizeVC:(id)sender {
    UIViewController *presentedVC = [self presentedViewController];
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        UIView *containerView = [presentedVC.view viewWithTag:1];
        CGRect sframe = containerView.frame;
        CGRect eframe = sframe;
        eframe.origin.y += sframe.size.height + 8;
        [UIView animateWithDuration:0.25
                              delay:0.0
                            options:UIViewAnimationOptionCurveEaseIn
                         animations:^{
                             [containerView setFrame:eframe];
                             [presentedVC.view setBackgroundColor:[UIColor clearColor]];
                         }
                         completion:^(BOOL finished){
                             [presentedVC dismissViewControllerAnimated:NO completion:^{
                                 if (wasKeyboardVisible)
                                     [textView becomeFirstResponder];
                             }];
                         }
         ];
    }
    else
        [presentedVC dismissViewControllerAnimated:YES completion:nil];
}

- (void)trashButtonClick:(id)sender {
    [self dismissDropDownMenu];
    if ([UIAlertController class]) {
        UIAlertController *alert = [UIAlertController
                                    alertControllerWithTitle:nil
                                    message:nil
                                    preferredStyle:UIAlertControllerStyleActionSheet];
        
        UIAlertAction *clear = [UIAlertAction
                                actionWithTitle:@"Clear Text"
                                style:UIAlertActionStyleDestructive
                                handler:^(UIAlertAction *action) {
                                    [textView setText:@""];
                                }];
        UIAlertAction *cancel = [UIAlertAction
                                 actionWithTitle:@"Cancel"
                                 style:UIAlertActionStyleDefault
                                 handler:^(UIAlertAction *action) {
                                 }];
        [alert addAction:clear];
        [alert addAction:cancel];
        [alert.popoverPresentationController setBarButtonItem:[[self.navigationItem rightBarButtonItems] objectAtIndex:4]];
        [self presentViewController:alert animated:YES completion:nil];
    }
    else {
        [popover dismissPopoverAnimated:NO];
        [actionSheet dismissWithClickedButtonIndex:actionSheet.cancelButtonIndex animated:NO];
        wasKeyboardVisible = [textView isFirstResponder];
        [textView dismissKeyboard];
        actionSheet = [[UIActionSheet alloc] initWithTitle:nil
                                                  delegate:self
                                         cancelButtonTitle:@"Cancel"
                                    destructiveButtonTitle:@"Clear Text"
                                         otherButtonTitles:nil];
        [actionSheet setTag:1];
        [actionSheet showFromBarButtonItem:[[self.navigationItem rightBarButtonItems] objectAtIndex:4] animated:YES];
    }
}

- (void)actionSheet:(UIActionSheet *)actionSheet clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (wasKeyboardVisible)
        [textView becomeFirstResponder];
    
    if (self.actionSheet.tag == 0) {
        
    }
    else if (self.actionSheet.tag == 1) {
        if (buttonIndex == 0) {
            [textView setText:@""];
        }
    }
}

- (void)willPresentActionSheet:(UIActionSheet *)actionSheet {
    if (self.actionSheet.tag == 0) {
        UIButton *button = nil;
        CGRect sliderFrame = CGRectZero;
        for (UIView *view in self.actionSheet.subviews)
        {
            if ([view isKindOfClass:[UIButton class]])
            {
                button = (UIButton *)view;
                button.enabled = NO;
                button.hidden = YES;
                sliderFrame = button.frame;
                break;
            }
        }
        
        sliderFrame.origin.x += 12;
        sliderFrame.size.width -= 24;
        [textSizeController setFrame:sliderFrame];
    }
}

- (UIWindow *)overlayWindow {
    return [self appDelegate].overlayWindow;
}

- (void)showActivityIndicator {
    if (![self.overlayWindow isHidden] && [self.overlayWindow viewWithTag:activityIndicatorViewTag] != nil)
        return;
    
    [self dismissGetStartedView:nil];
    [self.overlayWindow setHidden:NO];
    UIActivityIndicatorView *activityView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleWhiteLarge];
    UIView *containerView = [[UIView alloc] initWithFrame:CGRectInset(activityView.bounds, -10.0f, -10.0f)];
    containerView.backgroundColor = [UIColor colorWithWhite:0.5f alpha:0.8f];
    containerView.layer.cornerRadius = 6.0f;
    containerView.center = self.overlayWindow.center;
    containerView.tag = activityIndicatorViewTag;
    containerView.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin;
    
    activityView.center = CGPointMake(containerView.bounds.size.width*0.5f, containerView.bounds.size.height*0.5f);
    [activityView startAnimating];
    
    [containerView addSubview:activityView];
    [self.overlayWindow addSubview:containerView];
}

- (void)dismissActivityIndicator {
    [[self.overlayWindow viewWithTag:activityIndicatorViewTag] removeFromSuperview];
    [self.overlayWindow setHidden:YES];
}

- (void)sliderValueChanged:(UISlider *)sender {
    textSize = roundf(sender.value + minTextSize);
    textView.font = [textView.font fontWithSize:textSize];
    if (iosVersion >= 8) {
        UILabel *textSizeTitle = (UILabel *)[[self presentedViewController].view viewWithTag:2];
        [textSizeTitle setText:[NSString stringWithFormat:@"Text size: %d", (int)textSize]];
    }
    else
        [actionSheet setTitle:[NSString stringWithFormat:@"Text size: %d", (int)textSize]];
}
/*
- (UIImage *)resizeImage:(UIImage *)image scaledToSize:(CGSize)newSize {
    UIGraphicsBeginImageContextWithOptions(newSize, NO, 0.0);
    [image drawInRect:CGRectMake(0, 0, newSize.width, newSize.height)];
    UIImage *newImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return newImage;
}*/

- (CGRect)rectForBarButtonItem:(UIBarButtonItem *)barButtonItem {
    UIView *view = [barButtonItem valueForKey:@"view"];
    return view?view.frame:CGRectZero;
}

- (void)startTimer {
    if (loadTimer == nil) {
        loadTimer = [NSTimer scheduledTimerWithTimeInterval:1.0f
                                                     target:self
                                                   selector:@selector(timerAction)
                                                   userInfo:nil
                                                    repeats:YES];
    }
}

- (void)stopTimer {
    if (loadTimer != nil) {
        [loadTimer invalidate];
        loadTimer = nil;
    }
}

- (void)timerAction {
    if (updateStatus == 1) {
        [self stopTimer];
        
        /*
        if (iosVersion >= minIOSVersion4FontInstall) {
            [self createProfileNamesByKeyboardID];
        }*/
        
        [self dismissActivityIndicator];
        
        [textView setEditable:YES];
        [textView becomeFirstResponder];
        
        if (self.launchUrl != nil) {
            [self performActionFromUrl:self.launchUrl];
        }
        else {
            [self loadSavedUserText];
            if ([self shouldShowGetStarted])
                [self performSelector:@selector(showGetStartedView:) withObject:nil afterDelay:1.0];
        }
    }
    else if (updateStatus == -1) {
        [self stopTimer];
        [self dismissActivityIndicator];
        [self loadSavedUserText];
        [textView setEditable:YES];
        [textView becomeFirstResponder];
        if ([self shouldShowGetStarted])
            [self performSelector:@selector(showGetStartedView:) withObject:nil afterDelay:1.0];
    }
}

- (void)resetTextViewCursor {
    [textView setSelectedRange:NSMakeRange(0,0)];
    CGFloat size = textView.font.pointSize;
    [textView setFont:[[textView font] fontWithSize:(size+5)]];
    [textView setFont:[[textView font] fontWithSize:(size-5)]];
}

- (void)loadSavedUserText {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    NSString *userText = [userData objectForKey:kKeymanUserTextKey];
    NSString *userTextSize = [userData objectForKey:kKeymanUserTextSizeKey];
    
    if (userText != nil && [userText length] > 0)
        [textView setText:userText];
    
    if (userTextSize != nil && [userTextSize length] > 0) {
        textSize = [userTextSize floatValue];
        textView.font = [textView.font fontWithSize:textSize];
        [textSizeController setValue:(textSize - minTextSize)];
    }
}

- (void)performActionFromUrl:(NSURL *)url {
    if ([url query] == nil) {
        self.launchUrl = nil;
        return;
    }
    
    NSString *langID = nil;
    NSString *kbID = nil;
    NSURL *jsonUrl = nil;
    NSString *direct = nil;
    if ([[url lastPathComponent] isEqualToString:@"open"]) {
        NSString *query = [url query];
        if ([query rangeOfString:@"url="].location != NSNotFound) {
            // Download and set custom keyboard
            NSArray *params = [[url query] componentsSeparatedByString:@"&"];
            if ([params count] >= 1) {
                for (NSString *value in params) {
                    NSUInteger index = -1;
                    if ((index = [value rangeOfString:@"url="].location) != NSNotFound) {
                        jsonUrl = [NSURL URLWithString:[NSString stringWithString:[value substringFromIndex:index+4]]];
                    }
                    else if ((index = [value rangeOfString:@"direct="].location) != NSNotFound) {
                        direct = [NSString stringWithString:[value substringFromIndex:index+7]];
                    }
                    else if ((index = [value rangeOfString:@"keyboard="].location) != NSNotFound) {
                        kbID = [NSString stringWithString:[value substringFromIndex:index+9]];
                    }
                    else if ((index = [value rangeOfString:@"language="].location) != NSNotFound) {
                        langID = [NSString stringWithString:[value substringFromIndex:index+9]];
                    }
                }
            }
            
            /* Disabled to allow re-download of custom keyboards
            if (kbID != nil && langID != nil) {
                NSInteger index = [[KMManager sharedInstance] indexForUserKeyboardWithID:kbID languageID:langID];
                if (index >= 0) {
                    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
                    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
                    NSDictionary *kbInfo = [userKeyboards objectAtIndex:index];
                    NSString *kbName = [kbInfo objectForKey:kKeymanKeyboardNameKey];
                    NSString *langName = [kbInfo objectForKey:kKeymanLanguageNameKey];
                    NSString *font = [kbInfo objectForKey:kKeymanFontKey];
                    [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font];
                    self.launchUrl = nil;
                    return;
                }
            }*/
            
            if (jsonUrl != nil && [[jsonUrl absoluteString] length]) {
                [[KMManager sharedInstance] dismissKeyboardPicker:self];
                if (![self.infoView.view isHidden])
                    [self performSelector:@selector(infoButtonClick:) withObject:nil];
                
                BOOL isDirect = NO;
                if (direct != nil && [direct isEqualToString:@"true"])
                    isDirect = YES;
                
                self.customKeyboard2Download = [NSDictionary dictionaryWithObjectsAndKeys:
                                                jsonUrl, @"url",
                                                [NSNumber numberWithBool:isDirect], @"direct",
                                                nil];
                NSString *title = [NSString stringWithFormat:@"Custom Keyboard: %@", [jsonUrl lastPathComponent]];
                [self showAlertWithTitle:title
                                 message:@"Would you like to install this keyboard?"
                       cancelButtonTitle:@"Cancel"
                       otherButtonTitles:@"Install"
                                     tag:2];
            }
            else {
                [self showAlertWithTitle:@"Custom Keyboard"
                                 message:@"The keyboard could not be installed: Invalid Url"
                       cancelButtonTitle:@"OK"
                       otherButtonTitles:nil
                                     tag:-1];
                
                self.launchUrl = nil;
            }
        }
        else {
            // Query should include keyboard and language IDs to set the keyboard (first download if not available)
            NSArray *params = [[url query] componentsSeparatedByString:@"&"];
            if ([params count] >= 2) {
                for (NSString *value in params) {
                    NSUInteger index = -1;
                    if ((index = [value rangeOfString:@"keyboard="].location) != NSNotFound) {
                        kbID = [NSString stringWithString:[value substringFromIndex:index+9]];
                    }
                    else if ((index = [value rangeOfString:@"language="].location) != NSNotFound) {
                        langID = [NSString stringWithString:[value substringFromIndex:index+9]];
                    }
                }
            }
            
            if (kbID != nil && langID != nil) {
                NSString *dictKey = [NSString stringWithFormat:@"%@_%@", langID, kbID];
                NSDictionary *kbDict = [[[KMManager sharedInstance] keyboardsDictionary] objectForKey:dictKey];
                NSString *kbName = [kbDict objectForKey:kKeymanKeyboardNameKey];
                NSString *langName = [kbDict objectForKey:kKeymanLanguageNameKey];
                BOOL isRTL = [[kbDict objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
                NSString *font = [kbDict objectForKey:kKeymanFontKey];
                NSString *oskFont = [kbDict objectForKey:kKeymanOskFontKey];
                
                if ([[KMManager sharedInstance] stateForKeyboardWithID:kbID] == kKMKeyboardStateNeedsDownload) {
                    self.keyboard2Download = [NSDictionary dictionaryWithObjectsAndKeys:
                                              langID, kKeymanLanguageIdKey,
                                              kbID, kKeymanKeyboardIdKey,
                                              nil];
                    [self showAlertWithTitle:[NSString stringWithFormat:@"%@: %@", langName, kbName]
                                     message:@"Would you like to install this keyboard?"
                           cancelButtonTitle:@"Cancel"
                           otherButtonTitles:@"Install"
                                         tag:0];
                }
                else {
                    [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:NO font:font oskFont:oskFont];
                    [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
                }
            }
            else {
                self.launchUrl = nil;
            }
        }
    }
}

/*
- (void)createProfileNamesByKeyboardID {
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSLog(@"userKeyboards = %@", userKeyboards);
    
    NSArray *languages = [[KMManager sharedInstance] languages];
    self.profileNamesByKeyboardID = [NSMutableDictionary dictionaryWithCapacity:0];
    
    for (NSDictionary *langDict in languages) {
        NSArray *keyboards = [langDict objectForKey:kKeymanLanguageKeyboardsKey];
        for (NSDictionary *kbDict in keyboards) {
            NSString *kbID = [kbDict objectForKey:kKeymanIdKey];
            NSDictionary *fontDict = [kbDict objectForKey:kKeymanFontKey];
            if (fontDict != nil && [self.profileNamesByKeyboardID objectForKey:kbID] == nil) {
                id fontSource = [fontDict objectForKey:kKeymanFontSourceKey];
                if ([fontSource isKindOfClass:[NSArray class]]) {
                    for (NSString *value in fontSource) {
                        if ([value rangeOfString:@".mobileconfig"].location != NSNotFound) {
                            [self.profileNamesByKeyboardID setObject:[NSDictionary dictionaryWithObjectsAndKeys:value, kKeymanProfileKey, nil] forKey:kbID];
                            break;
                        }
                    }
                }
            }
        }
    }
}*/

- (NSDictionary *)dictionaryWithKeyboardID:(NSString *)kbID languageID:(NSString *)langID {
    NSInteger index = [[KMManager sharedInstance] indexForUserKeyboardWithID:kbID
                                                                  languageID:langID];
    if (index < 0)
        return nil;
    
    NSArray *userKeyboards = [[AppDelegate activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
    return [userKeyboards objectAtIndex:index];
}

- (NSString *)profileNameWithKeyboardID:(NSString *)kbID languageID:(NSString *)langID {
    NSDictionary *kbDict = [self dictionaryWithKeyboardID:kbID languageID:langID];
    if (kbDict == nil)
        return nil;
    
    return [self profileNameWithJSONFont:[kbDict objectForKey:kKeymanFontKey]];
}

- (NSString *)profileNameWithJSONFont:(NSString *)jsonFont {
    NSDictionary *fontDict = [NSJSONSerialization JSONObjectWithData:[jsonFont dataUsingEncoding:NSUTF8StringEncoding] options:NSJSONReadingMutableContainers error:nil];
    if (fontDict == nil)
        return nil;
    
    id fontSource = [fontDict objectForKey:kKeymanFontFilesKey];
    NSString *profile = nil;
    if ([fontSource isKindOfClass:[NSArray class]]) {
        for (NSString *value in fontSource) {
            if ([value rangeOfString:@".mobileconfig"].location != NSNotFound) {
                profile = [NSString stringWithString:value];
            }
        }
    }
    
    return profile;
}

- (void)checkProfileForKeyboardID:(NSString *)kbID languageID:(NSString *)langID doListCheck:(BOOL)doListCheck {
    if ([kbID isEqualToString:kKeymanDefaultKeyboardID] && [langID isEqualToString:kKeymanDefaultLanguageID])
        return;
    
    NSString *profile = [self profileNameWithKeyboardID:kbID languageID:langID];
    if (profile != nil) {
        BOOL doInstall = NO;
        if (doListCheck) {
            NSMutableArray *checkList = [NSMutableArray arrayWithArray:self.profiles2Install];
            [checkList removeObjectsInArray:self.checkedProfiles];
            for (NSString *value in checkList) {
                if ([profile isEqualToString:value]) {
                    doInstall = YES;
                    break;
                }
            }
        }
        else {
            doInstall = YES;
        }
        
        if (doInstall) {
            profileName = profile;
            NSDictionary *kbDict = [self dictionaryWithKeyboardID:kbID languageID:langID];
            NSString *languageName = [kbDict objectForKey:kKeymanLanguageNameKey];
            NSString *title = [NSString stringWithFormat:@"%@ Font", languageName];
            NSString *msg = [NSString stringWithFormat:@"Touch Install to make %@ display correctly in all your apps", languageName];
            [self showAlertWithTitle:title
                             message:msg
                   cancelButtonTitle:@"Cancel"
                   otherButtonTitles:@"Install"
                                 tag:1];
        }
        else {
            profileName = nil;
        }
    }
}

- (void)showKeyboard {
    [textView becomeFirstResponder];
}

- (void)showAlertWithTitle:(NSString *)title message:(NSString *)msg cancelButtonTitle:(NSString *)cbTitle otherButtonTitles:(NSString *)obTitles tag:(NSInteger)tag {
    /*
    if ([UIAlertController class]) {
        UIAlertController *alert = [UIAlertController alertControllerWithTitle:title
                                                                       message:msg
                                                                preferredStyle:UIAlertControllerStyleAlert];
        
        //UIAlertController* __weak weakAlert = alert;
        UIAlertAction *cancel = [UIAlertAction actionWithTitle:cbTitle
                                                         style:UIAlertActionStyleCancel
                                                       handler:^(UIAlertAction *action) {
                                                           if (profileName != nil && tag == 1) {
                                                               [checkedProfiles addObject:profileName];
                                                               NSUserDefaults *userData = [AppDelegate activeUserDefaults];
                                                               [userData setObject:self.checkedProfiles forKey:kKeymanCheckedProfilesKey];
                                                               [userData synchronize];
                                                               profileName = nil;
                                                           }
                                                       }];
        [alert addAction:cancel];
        
        if (obTitles != nil) {
            UIAlertAction *other = [UIAlertAction actionWithTitle:obTitles
                                                            style:UIAlertActionStyleDefault
                                                          handler:^(UIAlertAction *action) {
                                                              [self performAlertButtonClickWithTag:tag];
                                                          }];
            [alert addAction:other];
        }
        
        UIViewController *vc = [self.navigationController presentedViewController];
        [vc presentViewController:alert animated:YES completion:nil];
    }
    else {
        UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:title
                                                            message:msg
                                                           delegate:self
                                                  cancelButtonTitle:cbTitle
                                                  otherButtonTitles:obTitles, nil];
        alertView.tag = tag;
        [alertView show];
    }
    */
    
    [self dismissGetStartedView:nil];
    UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:title
                                                        message:msg
                                                       delegate:self
                                              cancelButtonTitle:cbTitle
                                              otherButtonTitles:obTitles, nil];
    alertView.tag = tag;
    [alertView show];
}

- (void)performAlertButtonClickWithTag:(NSInteger)tag {
    if (tag == 0) {
        NSString *langID = [self.keyboard2Download objectForKey:kKeymanLanguageIdKey];
        NSString *kbID = [self.keyboard2Download objectForKey:kKeymanKeyboardIdKey];
        [[KMManager sharedInstance] downloadKeyboardWithID:kbID languageID:langID isUpdate:NO];
    }
    else if (tag == 1) {
        if (profileName != nil) {
            [checkedProfiles addObject:profileName];
            NSUserDefaults *userData = [AppDelegate activeUserDefaults];
            [userData setObject:self.checkedProfiles forKey:kKeymanCheckedProfilesKey];
            [userData synchronize];
            
            [[UIApplication sharedApplication] openURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@%@", baseUri, profileName]]];
            
            profileName = nil;
        }
    }
    else if (tag == 2) {
        NSURL *jsonUrl = [self.customKeyboard2Download objectForKey:@"url"];
        BOOL isDirect = [[self.customKeyboard2Download objectForKey:@"direct"] boolValue];
        [[KMManager sharedInstance] downloadKeyboardFromUrl:jsonUrl isDirect:isDirect];
    }
}

#pragma mark - Alert view delegate

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (buttonIndex == alertView.cancelButtonIndex) {
        if (profileName != nil && alertView.tag == 1) {
            [checkedProfiles addObject:profileName];
            NSUserDefaults *userData = [AppDelegate activeUserDefaults];
            [userData setObject:self.checkedProfiles forKey:kKeymanCheckedProfilesKey];
            [userData synchronize];
            profileName = nil;
        }
        
        if (alertView.tag == 2) {
            if ([self shouldShowGetStarted])
                [self showGetStartedView:nil];
        }
        
        return;
    }
    
    [self performAlertButtonClickWithTag:alertView.tag];
}

#pragma mark - DropDown menu methods

- (void)showDropDownMenu:(id)sender {
    if ([self dismissDropDownMenu])
        return;

    wasKeyboardVisible = [textView isFirstResponder];
    [textView dismissKeyboard];
    
    CGFloat w = 160;
    CGFloat h = 44;
    CGFloat x = [self navBarWidth] - w;
    CGFloat y = [AppDelegate statusBarHeight] + [self navBarHeight];
    #ifdef FREE
    NSArray *listItems = [NSArray arrayWithObjects:trashButton, getStartedButton, infoButton, nil];
    #else
    NSArray *listItems = [NSArray arrayWithObjects:textSizeButton, trashButton, getStartedButton, infoButton, nil];
    #endif
    KMDropDownListView *dropDownList = [[KMDropDownListView alloc] initWithListItems:listItems
                                                                            itemSize:CGSizeMake(w, h)
                                                                            position:CGPointMake(x, y)];
    dropDownList.tag = dropDownListTag;
    [self.navigationController.view addSubview:dropDownList];
    //[self.view addSubview:dropDownList];
}

- (BOOL)dismissDropDownMenu {
    UIView *view = [self.navigationController.view viewWithTag:dropDownListTag];
    if (view != nil && wasKeyboardVisible)
        [textView becomeFirstResponder];
    
    [view removeFromSuperview];
    return (view == nil)?NO:YES;
}

#pragma mark - Get started view methods

- (void)showGetStartedView:(id)sender {
    if (![self.overlayWindow isHidden] && [self.overlayWindow viewWithTag:getStartedViewTag] != nil)
        return;
    
    [popover dismissPopoverAnimated:NO];
    [self dismissDropDownMenu];
    [self.overlayWindow setHidden:NO];
    self.getStartedVC = [[GetStartedViewController alloc] init];
    [self.getStartedVC setMainViewController:self];
    UIView *containerView = self.getStartedVC.view;
    UINavigationBar *navBar = (UINavigationBar *)[containerView viewWithTag:786586];
    UIBarButtonItem *doneButton = navBar.topItem.rightBarButtonItem;
    [doneButton setTarget:self];
    [doneButton setAction:@selector(dismissGetStartedView:)];

    containerView.frame = CGRectInset(containerView.frame, 15, 140);
    containerView.backgroundColor = [UIColor whiteColor];
    containerView.layer.cornerRadius = 6.0f;
    containerView.center = self.overlayWindow.center;
    containerView.tag = getStartedViewTag;
    containerView.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin;
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    [containerView setTransform:[self transformForOrientation:orientation]];
    [self.overlayWindow addSubview:containerView];
}

- (void)dismissGetStartedView:(id)sender {
    [[self.overlayWindow viewWithTag:getStartedViewTag] removeFromSuperview];
    [self.overlayWindow setHidden:YES];
}

- (BOOL)shouldShowGetStarted {
    // Do not display "Get started" when MainView is not visible
    if (self.navigationController.visibleViewController != self)
        return NO;
    
    BOOL shouldShowGetStarted = NO;    
    NSUserDefaults *userData = [AppDelegate activeUserDefaults];
    BOOL dontShowGetStarted = [userData boolForKey:kKeymanDontShowGetStartedKey];
    if (!dontShowGetStarted) {
        NSArray *userKbs = [userData objectForKey:kKeymanUserKeyboardsListKey];
        if ([userKbs count] < 2) {
            NSDictionary *firstKB = [userKbs objectAtIndex:0];
            if (firstKB != nil) {
                NSString *kbID = [firstKB objectForKey:kKeymanKeyboardIdKey];
                NSString *langID = [firstKB objectForKey:kKeymanLanguageIdKey];
                if ([kbID isEqualToString:kKeymanDefaultKeyboardID] && [langID isEqualToString:langID])
                    shouldShowGetStarted = YES;
                else
                    shouldShowGetStarted = NO;
            }
            else
                shouldShowGetStarted = YES;
        }
            
        if (iosVersion >= 8) {
            if (![AppDelegate isKeymanEnabledSystemWide])
                shouldShowGetStarted = YES;
        }
    }
    
    return shouldShowGetStarted;
}


#ifndef FREE
#pragma mark - KMWebBrowser view methods

- (void)showKMWebBrowserView:(id)sender {
    [popover dismissPopoverAnimated:NO];
    [self dismissDropDownMenu];
    WebBrowserViewController *webBrowserVC = [[WebBrowserViewController alloc] init];
    [webBrowserVC setFontFamily:[[textView font] fontName]];
    [self presentViewController:webBrowserVC animated:YES completion:nil];
}
#endif

#define DegreesToRadians(degrees) (degrees * M_PI / 180)

- (CGAffineTransform)transformForOrientation:(UIInterfaceOrientation)orientation {
    if (iosVersion >= 8)
        return CGAffineTransformIdentity;
    
    switch (orientation) {
        case UIInterfaceOrientationLandscapeLeft:
            return CGAffineTransformMakeRotation(-DegreesToRadians(90));
        case UIInterfaceOrientationLandscapeRight:
            return CGAffineTransformMakeRotation(DegreesToRadians(90));
        case UIInterfaceOrientationPortraitUpsideDown:
            return CGAffineTransformMakeRotation(DegreesToRadians(180));
        case UIInterfaceOrientationPortrait:
        default:
            return CGAffineTransformMakeRotation(DegreesToRadians(0));
    }
}

/*
- (UIImage *)navBarBgImage {
    CGRect rect = CGRectMake(0, 0, 320, 44);
    UIGraphicsBeginImageContext(rect.size);
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetFillColorWithColor(context, [UIColor redColor].CGColor);
    CGContextFillRect(context, rect);
    UIImage *bgImg = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsPopContext();
    UIGraphicsEndImageContext();
    return bgImg;
}*/

@end
