//
//  MainViewController.h
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 13/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "Keyman.h"

extern NSString *const kKeymanUserTextKey;
extern NSString *const kKeymanUserTextSizeKey;
extern NSString *const kKeymanDontShowGetStartedKey;
extern NSString *const kKeymanLaunchedFromUrlNotification;
extern NSString *const kKeymanUrlKey;

@interface MainViewController : UIViewController <KMTextViewDelegate, UIActionSheetDelegate, UIAlertViewDelegate> {
    NSUInteger iosVersion;
    CGFloat textSize;
    CGFloat screenWidth;
    CGFloat screenHeight;
    CGFloat portLeftMargin;
    CGFloat portRightMargin;
    CGFloat lscpeLeftMargin;
    CGFloat lscpeRightMargin;
    NSTimer *loadTimer;
    NSInteger updateStatus;
    BOOL didDownload;
}

//@property (strong, nonatomic) UIWindow *overlayWindow;
@property (strong, nonatomic) KMTextView *textView;
@property CGFloat textSize;

- (void)infoButtonClick:(id)sender;
- (void)dismissGetStartedView:(id)sender;

@end
