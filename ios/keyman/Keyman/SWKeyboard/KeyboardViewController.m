//
//  KeyboardViewController.m
//  Keyman for iPhone and iPad: System-wide Keyboard
//
//  Created by Serkan Kurt on 18/09/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardViewController.h"

@interface KeyboardViewController ()
@property (strong, nonatomic) UIView *overlayView;
@property (strong, nonatomic) NSString *displayMessage;
@end

@implementation KeyboardViewController

- (void)updateViewConstraints {
    [super updateViewConstraints];
    
    // Add custom view sizing constraints here
    if (self.view.frame.size.width == 0 || self.view.frame.size.height == 0)
        return;
    
    [self setupTopBarImage:[KMInputViewController isPortrait]];
}

- (void)viewDidLoad {
    [KMManager setApplicationGroupIdentifier:@"group.KM4I"];
    
    #ifdef DEBUG
    [[KMManager sharedInstance] setDebugPrintingOn:YES];
    #endif
    
    [super viewDidLoad];
    
    [self setupTopBarImage:[KMInputViewController isPortrait]];
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    
    /*
    BOOL hasFullAccess = [self hasFullAccess];
    if (purchased) {
        if (hasFullAccess) {
            self.displayMessage = nil;
            [self.overlayView removeFromSuperview];
            self.overlayView = nil;
            [[self webView] setUserInteractionEnabled:YES];
        }
        else {
            self.displayMessage = @"'Allow Full Access' should be turned on for Keyman system-wide keyboard! See info page in Keyman app for more details.";
            [self performSelector:@selector(showOverlayViewWithOkButton) withObject:nil afterDelay:0.25];
        }
    }
    else {
        UIView *webView = [self webView];
        [webView setUserInteractionEnabled:NO];
        if (hasFullAccess) {
            self.displayMessage = @"Keyman System-wide Keyboard can be purchased through the Keyman app";
            [self performSelector:@selector(showOverlayViewWithNextButton) withObject:nil afterDelay:0.25];
        }
        else {
            self.displayMessage = @"'Allow Full Access' should be turned on for Keyman system-wide keyboard! See info page in Keyman app for more details.";
            [self performSelector:@selector(showOverlayViewWithNextButton) withObject:nil afterDelay:0.25];
        }
    }*/
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated
}

- (void)textWillChange:(id<UITextInput>)textInput {
    [super textWillChange:textInput];
    // The app is about to change the document's contents. Perform any preparation here.
}

- (void)textDidChange:(id<UITextInput>)textInput {
    [super textDidChange:textInput];
    // The app has just changed the document's contents, the document context has been updated.
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    [super willRotateToInterfaceOrientation:toInterfaceOrientation duration:duration];
    [self setupTopBarImage:UIInterfaceOrientationIsPortrait(toInterfaceOrientation)];
}

- (void)setupTopBarImage:(BOOL)isPortrait {
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        CGFloat size = screenRect.size.height > screenRect.size.width?screenRect.size.height:screenRect.size.width;
        if (size >= 568.0) {
            // TopBarImage for iPhone and iPod Touch with 4"+ Display
            if (isPortrait)
                [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Portrait.png"]];
            else
                [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Landscape-568h.png"]];
        }
        else if (size < 568.0) {
            // TopBarImage for iPhone and iPod Touch with 3.5" Display
            if (isPortrait)
                [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Portrait.png"]];
            else
                [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Landscape.png"]];
        }
    }
    else {
        // TopBarImage for iPad
        if (isPortrait)
            [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Portrait.png"]];
        else
            [self.topBarImageView setImage:[UIImage imageNamed:@"banner-Landscape.png"]];
    }
}

/*
- (void)showOverlayViewWithNextButton {
    UIView *webView = [self webView];
    [self.overlayView removeFromSuperview];
    self.overlayView = [[UIView alloc] initWithFrame:webView.frame];
    self.overlayView.autoresizesSubviews = YES;
    self.overlayView.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight;
    [self.overlayView setBackgroundColor:[UIColor colorWithWhite:0.0 alpha:0.8]];
    UILabel *msgLabel = [[UILabel alloc] initWithFrame:CGRectInset(webView.frame, 10, 0)];
    msgLabel.backgroundColor = [UIColor clearColor];
    msgLabel.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight;
    msgLabel.textColor = [UIColor whiteColor];
    msgLabel.text = self.displayMessage;
    msgLabel.font = [msgLabel.font fontWithSize:18.0];
    msgLabel.shadowColor = [UIColor lightTextColor];
    msgLabel.shadowOffset = CGSizeMake(1, 1);
    msgLabel.lineBreakMode = NSLineBreakByWordWrapping;
    msgLabel.numberOfLines = 0;
    msgLabel.textAlignment = NSTextAlignmentCenter;
    msgLabel.center = webView.center;
    
    UIButton *nextKeyboardButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    nextKeyboardButton.backgroundColor = [UIColor whiteColor];
    [nextKeyboardButton setTitle:@"  Next Keyboard  " forState:UIControlStateNormal];
    [nextKeyboardButton sizeToFit];
    [nextKeyboardButton addTarget:self action:@selector(advanceToNextInputMode) forControlEvents:UIControlEventTouchUpInside];
    [nextKeyboardButton.layer setCornerRadius:5.0];
    [nextKeyboardButton setTranslatesAutoresizingMaskIntoConstraints:NO];
    
    [self.overlayView addSubview:msgLabel];
    [self.overlayView addSubview:nextKeyboardButton];
    [self.view addSubview:self.overlayView];
    CGRect sFrame = self.overlayView.frame;
    CGRect eFrame = sFrame;
    sFrame.origin.y += sFrame.size.height;
    [self.overlayView setFrame:sFrame];
    [UIView animateWithDuration:0.25
                          delay:0.0
                        options:UIViewAnimationOptionCurveEaseIn
                     animations:^{
                         [self.overlayView setFrame:eFrame];
                     }
                     completion:nil
     ];
    
    NSLayoutConstraint *nextKeyboardButtonCenterConstraint = [NSLayoutConstraint constraintWithItem:nextKeyboardButton attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:self.overlayView attribute:NSLayoutAttributeCenterX multiplier:1.0 constant:0.0];
    NSLayoutConstraint *nextKeyboardButtonBottomConstraint = [NSLayoutConstraint constraintWithItem:nextKeyboardButton attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:self.overlayView attribute:NSLayoutAttributeBottom multiplier:1.0 constant:-15.0];
    [self.overlayView addConstraints:@[nextKeyboardButtonCenterConstraint, nextKeyboardButtonBottomConstraint]];
}

- (void)showOverlayViewWithOkButton {
    UIView *webView = [self webView];
    [self.overlayView removeFromSuperview];
    self.overlayView = [[UIView alloc] initWithFrame:webView.frame];
    self.overlayView.autoresizesSubviews = YES;
    self.overlayView.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight;
    [self.overlayView setBackgroundColor:[UIColor colorWithWhite:0.0 alpha:0.8]];
    UILabel *msgLabel = [[UILabel alloc] initWithFrame:CGRectInset(webView.frame, 10, 0)];
    msgLabel.backgroundColor = [UIColor clearColor];
    msgLabel.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight;
    msgLabel.textColor = [UIColor whiteColor];
    msgLabel.text = self.displayMessage;
    msgLabel.font = [msgLabel.font fontWithSize:18.0];
    msgLabel.shadowColor = [UIColor lightTextColor];
    msgLabel.shadowOffset = CGSizeMake(1, 1);
    msgLabel.lineBreakMode = NSLineBreakByWordWrapping;
    msgLabel.numberOfLines = 0;
    msgLabel.textAlignment = NSTextAlignmentCenter;
    msgLabel.center = webView.center;
    
    UIButton *nextKeyboardButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    nextKeyboardButton.backgroundColor = [UIColor whiteColor];
    [nextKeyboardButton setTitle:@"  OK  " forState:UIControlStateNormal];
    [nextKeyboardButton sizeToFit];
    [nextKeyboardButton addTarget:self action:@selector(dismissOverlayView) forControlEvents:UIControlEventTouchUpInside];
    [nextKeyboardButton.layer setCornerRadius:5.0];
    [nextKeyboardButton setTranslatesAutoresizingMaskIntoConstraints:NO];
    
    [self.overlayView addSubview:msgLabel];
    [self.overlayView addSubview:nextKeyboardButton];
    [self.view addSubview:self.overlayView];
    CGRect sFrame = self.overlayView.frame;
    CGRect eFrame = sFrame;
    sFrame.origin.y += sFrame.size.height;
    [self.overlayView setFrame:sFrame];
    [UIView animateWithDuration:0.25
                          delay:0.0
                        options:UIViewAnimationOptionCurveEaseIn
                     animations:^{
                         [self.overlayView setFrame:eFrame];
                     }
                     completion:nil
     ];
    
    NSLayoutConstraint *nextKeyboardButtonCenterConstraint = [NSLayoutConstraint constraintWithItem:nextKeyboardButton attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:self.overlayView attribute:NSLayoutAttributeCenterX multiplier:1.0 constant:0.0];
    NSLayoutConstraint *nextKeyboardButtonBottomConstraint = [NSLayoutConstraint constraintWithItem:nextKeyboardButton attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:self.overlayView attribute:NSLayoutAttributeBottom multiplier:1.0 constant:-15.0];
    [self.overlayView addConstraints:@[nextKeyboardButtonCenterConstraint, nextKeyboardButtonBottomConstraint]];
}

- (void)dismissOverlayView {
    CGRect eFrame = self.overlayView.frame;
    eFrame.origin.y += eFrame.size.height;
    [UIView animateWithDuration:0.25
                          delay:0.0
                        options:UIViewAnimationOptionCurveEaseIn
                     animations:^{
                         [self.overlayView setFrame:eFrame];
                     }
                     completion:^(BOOL finished){
                         self.displayMessage = nil;
                         [self.overlayView removeFromSuperview];
                         self.overlayView = nil;
                         [[self webView] setUserInteractionEnabled:YES];
                     }
     ];
}
*/
/*
- (UIView *)webView {
    UIView *webView = nil;
    for (UIView *v in self.view.subviews) {
        if ([v isKindOfClass:[UIWebView class]] || [v isKindOfClass:[WKWebView class]]) {
            webView = v;
            break;
        }
    }
    
    return webView;
}*/

@end
