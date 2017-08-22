//
//  KeyboardViewController.m
//  SWKeyboard
//
//  Created by Serkan Kurt on 7/04/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardViewController.h"

@interface KeyboardViewController ()

@end

@implementation KeyboardViewController

- (void)updateViewConstraints {
    [super updateViewConstraints];
    
    // Add custom view sizing constraints here
}

- (void)viewDidLoad {
    [KMManager setApplicationGroupIdentifier:@"group.KMSample"]; // TO DO: Replace with your application group id
    [[KMManager sharedInstance] setDebugPrintingOn:YES]; // TO DO: Disable before release
    [super viewDidLoad];
    
    [self.topBarImageView setBackgroundColor:[UIColor colorWithRed:1.0 green:96.0/255.0 blue:0.0 alpha:1.0]];
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectZero];
    
    [label setText:[NSString stringWithFormat:@" %@", [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleDisplayName"]]];
    [label sizeToFit];
    [label setTextColor:[UIColor whiteColor]];
    [self.topBarImageView addSubview:label];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated
}

- (void)textWillChange:(id<UITextInput>)textInput {
    // The app is about to change the document's contents. Perform any preparation here.
}

- (void)textDidChange:(id<UITextInput>)textInput {
    [super textDidChange:textInput];
    // The app has just changed the document's contents, the document context has been updated.
}

@end
