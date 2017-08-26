//
//  KMInputViewController.m
//  Keyman Engine
//
//  Created by Serkan Kurt on 20/06/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputViewController.h"
#import "KMManager+Internal.h"
#import <AudioToolbox/AudioToolbox.h>

@interface KMInputViewController ()
@property (weak, nonatomic) UIView *kmInputView;
@property (strong, nonatomic) NSArray<__kindof NSLayoutConstraint *> *bView_constraint_H;
@property (strong, nonatomic) NSArray<__kindof NSLayoutConstraint *> *bView_constraint_POS_HR;
@property (strong, nonatomic) UIView *containerView;
@property (strong, nonatomic) NSArray<__kindof NSLayoutConstraint *> *cView_constraint_H;
@property (strong, nonatomic) NSArray<__kindof NSLayoutConstraint *> *cView_constraint_POS_HR;
@property (strong, nonatomic) NSLayoutConstraint *heightConstraint;
//@property (strong, nonatomic) UIButton *debugButton;
@property (assign) BOOL isTopBarEnabled;
@end

@implementation KMInputViewController
@synthesize bView_constraint_H, bView_constraint_POS_HR;
@synthesize cView_constraint_H, cView_constraint_POS_HR;

- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
        self.isInputClickSoundEnabled = YES;
        self.isTopBarEnabled = [[KMManager sharedInstance] isSystemKeyboardTopBarEnabled];
        self.kmInputView = [KMManager inputView];
        //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardLoaded:) name:kKeymanKeyboardLoadedNotification object:nil];
    }
    return self;
}

- (void)updateViewConstraints {
    // Add custom view sizing constraints here
    if (self.view.frame.size.width == 0 || self.view.frame.size.height == 0) {
        [super updateViewConstraints];
        return;
    }
    
    [[KMManager sharedInstance] updateViewConstraints];
    //NSDictionary *viewsDict = @{@"bView":topBarImageView, @"cView":containerView};
    
    NSLayoutConstraint *bViewConstraintH = (NSLayoutConstraint *)[bView_constraint_H objectAtIndex:0];
    bViewConstraintH.constant = (unsigned long)(self.isTopBarEnabled?[[self class] topBarHeight]:0);
    
    /*
    [self.view removeConstraints:bView_constraint_H];
    NSString *vFormat = [NSString stringWithFormat:@"V:[bView(%lu)]", (unsigned long)(self.isTopBarEnabled?[[self class] topBarHeight]:0)];
    bView_constraint_H = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                 options:0
                                                                 metrics:nil
                                                                   views:viewsDict];
    [self.view addConstraints:bView_constraint_H];*/
  
    for ( __kindof NSLayoutConstraint * bConstraint in bView_constraint_H) {
      if (![self.view.constraints containsObject:bConstraint])
        [self.view addConstraint:bConstraint];
    }
  
    CGSize size = self.kmInputView.frame.size;
    NSLayoutConstraint *cViewConstraintH = (NSLayoutConstraint *)[cView_constraint_H objectAtIndex:0];
    cViewConstraintH.constant = (int)size.height%1000;
    /*
    [self.view removeConstraints:cView_constraint_H];
    CGSize size = self.kmInputView.frame.size;
    vFormat = [NSString stringWithFormat:@"V:[cView(%d)]", (int)size.height%1000];
    cView_constraint_H = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                           options:0
                                                           metrics:nil
                                                             views:viewsDict];
    [self.view addConstraints:cView_constraint_H];*/
    for ( __kindof NSLayoutConstraint * cConstraint in cView_constraint_H) {
      if (![self.view.constraints containsObject:cConstraint])
        [self.view addConstraint:cConstraint];
    }
  
    CGFloat expandedHeight = [self expandedHeight];
    if (![self.view.constraints containsObject:self.heightConstraint]) {
        self.heightConstraint.constant = expandedHeight;
        [self.view addConstraint:self.heightConstraint];
    }
    else if (self.heightConstraint.constant != expandedHeight) {
        self.heightConstraint.constant = expandedHeight;
    }

    [super updateViewConstraints];
}

- (void)viewDidLoad {
    [super viewDidLoad];
    
    UIColor *bgColor = [UIColor colorWithRed:210.0/255.0 green:214.0/255.0 blue:220.0/255.0 alpha:1.0];
    self.view.backgroundColor = bgColor;
    
    [[KMManager sharedInstance] inputViewDidLoad];
    [[KMManager sharedInstance] setInputDelegate:self];
    
    [self.topBarImageView removeFromSuperview];
    _topBarImageView = [UIImageView new];
    self.topBarImageView.translatesAutoresizingMaskIntoConstraints = NO;
    self.topBarImageView.backgroundColor = [UIColor grayColor];
    [self.view addSubview:self.topBarImageView];
    
    /*
    if ([[KMManager sharedInstance] debugPrintingOn]) {
        _debugButton = [UIButton buttonWithType:UIButtonTypeSystem];
        self.debugButton.translatesAutoresizingMaskIntoConstraints = NO;
        [self.debugButton addTarget:self action:@selector(viewDebugLog:) forControlEvents:UIControlEventTouchUpInside];
        [self.debugButton setTitle:@"\u24B9" forState:UIControlStateNormal];
        [self.debugButton sizeToFit];
        [self.view addSubview:self.debugButton];
    }*/
    
    [self.containerView.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
    [self.containerView removeFromSuperview];
    _containerView = [UIView new];
    self.containerView.translatesAutoresizingMaskIntoConstraints = NO;
    self.containerView.backgroundColor = bgColor;
    [self.containerView addSubview:self.kmInputView];
    [self.view addSubview:self.containerView];
    
    //[self setConstraints]; /**/
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    [self setConstraints]; /**/
    [self.inputView setNeedsUpdateConstraints]; /**/
    //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(subKeysMenuWillShow:) name:kKeymanSubKeysMenuWillShowNotification object:nil];
    //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(subKeysMenuDismissed:) name:kKeymanSubKeysMenuDismissedNotification object:nil];
}

- (void)viewDidDisappear:(BOOL)animated {
    [super viewDidDisappear:animated];
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    [[KMManager sharedInstance] inputViewWillRotateToInterfaceOrientation:toInterfaceOrientation duration:duration];
}

- (void)dealloc {
    //[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)textDidChange:(id<UITextInput>)textInput {
    NSString *cxBeforeInput;
    if ([self.textDocumentProxy documentContextBeforeInput] != nil)
        cxBeforeInput = [NSString stringWithString:[self.textDocumentProxy documentContextBeforeInput]];
    else
        cxBeforeInput = @"";
    
    NSString *cxAfterInput;
    if ([self.textDocumentProxy documentContextAfterInput] != nil)
        cxAfterInput = [NSString stringWithString:[self.textDocumentProxy documentContextAfterInput]];
    else
        cxAfterInput = @"";
    
    NSString *context = [NSString stringWithFormat:@"%@%@", cxBeforeInput, cxAfterInput];
    NSRange range = [context rangeOfString:cxBeforeInput];
    NSRange newRange;
    if (range.location != NSNotFound)
        newRange = NSMakeRange(range.location + range.length, 0);
    else
        newRange = NSMakeRange(0, 0);
    
    [KMManager setKMText:context];
    [KMManager setKMSelectionRange:newRange manually:NO];
    
    /*
    if (textInput != nil) {
        [KMManager setKMText:[textInput textInRange:[textInput textRangeFromPosition:[textInput beginningOfDocument] toPosition:[textInput endOfDocument]]]];
        UITextRange *textRange = textInput.selectedTextRange;
        NSRange newRange = NSMakeRange([textInput offsetFromPosition:[textInput beginningOfDocument] toPosition:textRange.start], [textInput offsetFromPosition:textRange.start toPosition:textRange.end]);
        [KMManager setKMSelectionRange:newRange manually:NO];
    }
    */
}

- (void)selectionDidChange:(id<UITextInput>)textInput {
    /*
    if (textInput != nil) {
        UITextRange *textRange = textInput.selectedTextRange;
        NSRange newRange = NSMakeRange([textInput offsetFromPosition:[textInput beginningOfDocument] toPosition:textRange.start], [textInput offsetFromPosition:textRange.start toPosition:textRange.end]);
        [KMManager setKMSelectionRange:newRange manually:NO];
    }
    */
}

#pragma mark - KMWebViewDelegate

- (void)updatedFragment:(NSString *)fragment {
    if (fragment == nil || [fragment length] == 0)
        return;
    
    if ([fragment rangeOfString:@"insertText"].location != NSNotFound) {
        if ([[KMManager sharedInstance] isSubKeysMenuVisible])
            return;
        
        if (self.isInputClickSoundEnabled) {
            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
                AudioServicesPlaySystemSound(0x450);
            });
            self.isInputClickSoundEnabled = NO; // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
            [self performSelector:@selector(enableInputClickSound) withObject:nil afterDelay:0.1f];
        }
        
        NSRange range1 = [fragment rangeOfString:@"+dn="];
        NSRange range2 = [fragment rangeOfString:@"+s="];
        range1.location += range1.length;
        range1.length = range2.location - range1.location;
        NSInteger dn = [[fragment substringWithRange:range1] integerValue];
        
        range2.location += range2.length;
        range2.length = [fragment length] - range2.location;
        NSString *s = [fragment substringWithRange:range2];

        NSMutableString *text = nil;
        NSArray *unicodes = nil;
        if ([s length])
            unicodes = [s componentsSeparatedByString: @","];
        NSScanner *scanner = nil;
        for (int i=0; i<[unicodes count]; i++) {
            scanner = [NSScanner scannerWithString:[unicodes objectAtIndex:i]];
            unsigned int unicode;
            [scanner scanHexInt:&unicode];
            if(text == nil)
                text = [NSMutableString stringWithFormat:@"%C", (unsigned short) unicode];
            else
                [text appendString:[NSString stringWithFormat:@"%C", (unsigned short) unicode]];
        }
        
        if (text == nil)
            text = [NSMutableString stringWithString:@""];
        
        if (dn <= 0) {
            [self.textDocumentProxy insertText:text];
        }
        else {
            if (![text length]) {
                while (dn) {
                    NSString *xContext = [[self.textDocumentProxy documentContextBeforeInput] copy];
                    [self.textDocumentProxy deleteBackward];
                    NSString *nContext = [[self.textDocumentProxy documentContextBeforeInput] copy];
                    NSInteger dif = xContext.length - nContext.length;
                    if (dif > 1) {
                        unichar c = [xContext characterAtIndex:xContext.length - 1];
                        if (![self isSurrogate:c] && xContext.length >= 1)
                            [self.textDocumentProxy insertText:[xContext substringWithRange:NSMakeRange(nContext.length, dif - 1)]];
                    }
                    
                    dn--;
                }
            }
            else {
                while (dn) {
                    NSString *xContext = [[self.textDocumentProxy documentContextBeforeInput] copy];
                    [self.textDocumentProxy deleteBackward];
                    NSString *nContext = [[self.textDocumentProxy documentContextBeforeInput] copy];
                    NSInteger dif = xContext.length - nContext.length;
                    if (dif > 1) {
                        unichar c = [xContext characterAtIndex:xContext.length - 1];
                        if (![self isSurrogate:c] && xContext.length >= 1)
                            [self.textDocumentProxy insertText:[xContext substringWithRange:NSMakeRange(nContext.length, dif - 1)]];
                    }
                    
                    dn--;
                }
                
                [self.textDocumentProxy insertText:text];
            }
        }
    }
    else if ([fragment rangeOfString:@"menuKeyUp"].location != NSNotFound) {
        if ([[KMManager sharedInstance] isKeyboardMenuVisible])
            return;
        
        if (self.globeKeyTapBehaviour == GKTapSwitchToNextKeyboard) {
            NSInteger nextIndex = [[KMManager sharedInstance] switchToNextKeyboard];
            if (nextIndex < 0 || nextIndex == 0)
                [self advanceToNextInputMode];
        }
        else if (self.globeKeyTapBehaviour == GKTapSwitchToNextInputMethod) {
            [self advanceToNextInputMode];
        }
    }
    else if ([fragment rangeOfString:@"showKeyboardMenu"].location != NSNotFound) {
        if (self.menuBehaviour == MenuShowAlways) {
            [[KMManager sharedInstance] showKeyboardMenu:self closeButtonTitle:self.menuCloseButtonTitle];
        }
        else if (self.menuBehaviour == MenuShowIfMultipleKeyboards) {
            if ([self keyboardListCount] > 1) {
                [[KMManager sharedInstance] showKeyboardMenu:self closeButtonTitle:self.menuCloseButtonTitle];
            }
        }
    }
    else if ([fragment rangeOfString:@"hideKeyboard"].location != NSNotFound) {
        [self dismissKeyboard];
    }
}

- (void)setConstraints {
    NSDictionary *viewsDict = @{@"bView":self.topBarImageView, @"cView":self.containerView};
    NSString *vFormat = [NSString stringWithFormat:@"V:[bView(%lu)]", (unsigned long)(self.isTopBarEnabled?[[self class] topBarHeight]:0)];
    bView_constraint_H = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                 options:0
                                                                 metrics:nil
                                                                   views:viewsDict];
    
    CGFloat screenWidth = [UIScreen mainScreen].bounds.size.width;
    vFormat = [NSString stringWithFormat:@"H:[bView(>=%lu)]", (unsigned long)screenWidth];
    NSArray *bView_constraint_V = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                          options:0
                                                                          metrics:nil
                                                                            views:viewsDict];
    
    vFormat = @"V:|-0-[bView]";
    NSArray *bView_constraint_POS_V = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                              options:0
                                                                              metrics:nil
                                                                                views:viewsDict];
    vFormat = @"H:|-0-[bView]";
    NSArray *bView_constraint_POS_HL = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                               options:0
                                                                               metrics:nil
                                                                                 views:viewsDict];
    vFormat = @"H:[bView]-0-|";
    bView_constraint_POS_HR = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                               options:0
                                                                               metrics:nil
                                                                                 views:viewsDict];
    [self.view addConstraints:bView_constraint_V];
    [self.view addConstraints:bView_constraint_H];
    [self.view addConstraints:bView_constraint_POS_V];
    [self.view addConstraints:bView_constraint_POS_HL];
    
    CGSize size = self.kmInputView.frame.size;
    vFormat = [NSString stringWithFormat:@"V:[cView(%d)]", (int)size.height%1000]; // note this appears to relate to the rotation fix bug; may not work on iPads as 1000 is not enough
    cView_constraint_H = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                 options:0
                                                                 metrics:nil
                                                                   views:viewsDict];
    
    vFormat = [NSString stringWithFormat:@"H:[cView(>=%lu)]", (unsigned long)screenWidth];
    NSArray *cView_constraint_V = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                          options:0
                                                                          metrics:nil
                                                                            views:viewsDict];
    
    vFormat = @"V:[cView]-0-|";
    NSArray *cView_constraint_POS_V = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                              options:0
                                                                              metrics:nil
                                                                                views:viewsDict];
    vFormat = @"H:|-0-[cView]";
    NSArray *cView_constraint_POS_HL = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                               options:0
                                                                               metrics:nil
                                                                                 views:viewsDict];
    vFormat = @"H:[cView]-0-|";
    cView_constraint_POS_HR = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                               options:0
                                                                               metrics:nil
                                                                                 views:viewsDict];
    [self.view addConstraints:cView_constraint_V];
    [self.view addConstraints:cView_constraint_H];
    [self.view addConstraints:cView_constraint_POS_V];
    [self.view addConstraints:cView_constraint_POS_HL];
    
    /*
    if (self.debugButton != nil) {
        vFormat = @"H:[dButton]-10-|";
        NSArray *dButton_constraint_POS_HR = [NSLayoutConstraint constraintsWithVisualFormat:vFormat
                                                                                     options:0
                                                                                     metrics:nil
                                                                                       views:@{@"dButton":self.debugButton}];
        [self.view addConstraints:dButton_constraint_POS_HR];
        [self.view addConstraint:[NSLayoutConstraint constraintWithItem:self.debugButton
                                                              attribute:NSLayoutAttributeCenterY
                                                              relatedBy:NSLayoutRelationEqual
                                                                 toItem:self.topBarImageView
                                                              attribute:NSLayoutAttributeCenterY
                                                             multiplier:1.0
                                                               constant:0.0]];
    }*/
    
    self.heightConstraint = [NSLayoutConstraint constraintWithItem:self.view
                                                         attribute:NSLayoutAttributeHeight
                                                         relatedBy:NSLayoutRelationEqual
                                                            toItem:nil
                                                         attribute:NSLayoutAttributeNotAnAttribute
                                                        multiplier:0.0
                                                          constant:[self expandedHeight]];
    [self.heightConstraint setPriority:999];
}

/*
- (void)advanceToNextInputMode {
    self.startIndex = [self getStartIndex];
    [super advanceToNextInputMode];
}

- (void)keyboardLoaded:(NSNotification *)notification {
    self.startIndex = [self getStartIndex];
}

- (NSUInteger)getStartIndex {
    NSUInteger startIndex = 0;
    NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
    NSMutableArray *keyboardList = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
    for (int i = 0; i < keyboardList.count; i++) {
        NSString *languageID = [[keyboardList objectAtIndex:i] objectForKey:kKeymanLanguageIdKey];
        NSString *keyboardID = [[keyboardList objectAtIndex:i] objectForKey:kKeymanKeyboardIdKey];
        if ([[KMManager sharedInstance].languageID isEqualToString:languageID] && [[KMManager sharedInstance].keyboardID isEqualToString:keyboardID]) {
            startIndex = i;
            break;
        }
    }
    
    return startIndex;
}
*/

- (NSUInteger)keyboardListCount {
    NSUserDefaults *userData = [[KMManager sharedInstance] activeUserDefaults];
    NSArray *keyboardList = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    return keyboardList.count;
}

+ (NSUInteger)topBarHeight {
    UIUserInterfaceIdiom uiIdiom = [[UIDevice currentDevice] userInterfaceIdiom];
    if ([[self class] isPortrait])
        return (uiIdiom == UIUserInterfaceIdiomPhone?41:41);
    else
        return (uiIdiom == UIUserInterfaceIdiomPhone?34:39);
}

- (CGFloat)expandedHeight {    
    CGSize size = self.kmInputView.frame.size;
    return (int)size.height%1000 + (self.isTopBarEnabled?[[self class] topBarHeight]:0);
}

- (BOOL)hasFullAccess {
    return [[KMManager sharedInstance] canAccessSharedContainer];
}

+ (BOOL)isPortrait {
    if ([UIScreen mainScreen].bounds.size.width < [UIScreen mainScreen].bounds.size.height)
        return YES;
    else
        return NO;
}

- (void)enableInputClickSound {
    self.isInputClickSoundEnabled = YES;
}

/*
- (void)viewDebugLog:(id)sender {
    UIPasteboard *pb = [UIPasteboard generalPasteboard];
    [pb setString:[KMManager sharedInstance].debugLog];
}*/

- (BOOL)isSurrogate:(unichar)c {
    if (c >= 0xD800 && c <= 0xDFFF)
        return YES;
    
    return NO;
}

@end
