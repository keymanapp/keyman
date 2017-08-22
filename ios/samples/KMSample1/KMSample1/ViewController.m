//
//  ViewController.m
//  KMSample1
//
//  Created by Serkan Kurt on 7/04/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "ViewController.h"

@interface ViewController ()
@property (strong, nonatomic) IBOutlet KMTextView *textView;
@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
    
    [[KMManager sharedInstance] setDebugPrintingOn:YES];
    [[KMManager sharedInstance] setKeymanHelpOn:NO];
    
    [[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"tamil99m-1.1" ofType:@"js"] shouldOverwrite:YES];
    [[KMManager sharedInstance] preloadFontFileAtPath:[[NSBundle mainBundle] pathForResource:@"aava1" ofType:@"ttf"] shouldOverwrite:YES];
    [[KMManager sharedInstance] registerCustomFonts];
    
    //[[NSUserDefaults standardUserDefaults] removeObjectForKey:kKeymanUserKeyboardsListKey];
    [[KMManager sharedInstance] addKeyboardWithID:kKeymanDefaultKeyboardID languageID:kKeymanDefaultLanguageID keyboardName:kKeymanDefaultKeyboardName languageName:kKeymanDefaultLanguageName isRTL:NO isCustom:NO font:kKeymanDefaultKeyboardFont oskFont:nil];
    [[KMManager sharedInstance] addKeyboardWithID:@"tamil99m" languageID:@"tam" keyboardName:@"Tamil 99M" languageName:@"Tamil" isRTL:NO isCustom:YES font:@"aava1.ttf" oskFont:nil];
    //[[KMManager sharedInstance] setCanAddNewKeyboards:NO];
    
    [self.textView setKeymanDelegate:self];
    [self.textView setViewController:self];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
