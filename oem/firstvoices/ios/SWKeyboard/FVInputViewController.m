//
//  FVInputViewController.m
//  SWKeyboard
//
//  Created by Serkan Kurt on 18/11/2015.
//  Copyright © 2015 FirstVoices. All rights reserved.
//

#import "FVInputViewController.h"
#import "FVShared.h"

@interface FVInputViewController ()
@property (nonatomic, strong) UILabel *titleLabel;
@end

@implementation FVInputViewController

- (void)updateViewConstraints {
    [super updateViewConstraints];
    
    // Add custom view sizing constraints here
    if (self.view.frame.size.width == 0 || self.view.frame.size.height == 0)
        return;
    
    [self setupTopBarImage:[KMInputViewController isPortrait]];
    [self resizeTitleLabel];
}

- (void)viewDidLoad {
    [KMManager setKeymanLicense:@"FirstVoices" key:@"a016449b6b29ebb661d31179b279fef4"];
    [KMManager setApplicationGroupIdentifier:FVGroupID];
    
    #ifdef DEBUG
    [[KMManager sharedInstance] setDebugPrintingOn:YES];
    #endif
    
    [super viewDidLoad];
    
    [self setGlobeKeyTapBehaviour:GKTapSwitchToNextKeyboard];
    [self setMenuBehaviour:MenuShowAlways];
    [self setMenuCloseButtonTitle:@"Switch to other keyboard"];
    
    [self setupTopBarImage:[KMInputViewController isPortrait]];
    
    _titleLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    [_titleLabel setTextColor:[UIColor colorWithRed:170.0/255.0 green:18.0/255.0 blue:37.0/255.0 alpha:1.0]];
    [_titleLabel setText:@"FirstVoices"];
    
    [self resizeTitleLabel];
    [self.topBarImageView addSubview:_titleLabel];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated
}

- (void)textWillChange:(id<UITextInput>)textInput {
    // The app is about to change the document's contents. Perform any preparation here.
}

- (void)textDidChange:(id<UITextInput>)textInput {
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

- (void)resizeTitleLabel {
    [_titleLabel sizeToFit];
    CGRect frame = _titleLabel.frame;
    frame.origin.x = 10;
    frame.size.height = self.topBarImageView.frame.size.height;
    [_titleLabel setFrame:frame];
}

@end
