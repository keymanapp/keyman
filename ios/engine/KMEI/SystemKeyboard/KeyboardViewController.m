//
//  KeyboardViewController.m
//  SystemKeyboard
//
//  Created by Serkan Kurt on 20/06/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyboardViewController.h"
#import "KeymanEngine.h"

@interface KeyboardViewController ()

@end

@implementation KeyboardViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Perform custom initialization work here
    }
    return self;
}

- (void)updateViewConstraints {
    [super updateViewConstraints];
    // Add custom view sizing constraints here
}

- (void)viewDidLoad {
    [KMManager setApplicationGroupIdentifier:@"group.KMEI"];
    [KMManager sharedInstance].debugPrintingOn = YES;
    [super viewDidLoad];
    // Perform custom UI setup here
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated
}

@end
