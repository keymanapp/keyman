//
//  KMTextField.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-05-16.
//  Updated by Serkan Kurt on 17/05/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMTextField.h"
#import "KMManager+Internal.h"
#import <AudioToolbox/AudioToolbox.h>

@interface KMTextField ()
	@property (nonatomic, strong) KMTextFieldDelegateProxy *delegateProxy;
	@property (nonatomic, strong) NSTimer *selectionPollingTimer;
	@property (nonatomic, assign) NSInteger lastSelectionPosition;
	@property (nonatomic, assign) NSInteger lastSelectionLength;
    @property (nonatomic, assign) BOOL shouldUpdateKMText;
@end

@implementation KMTextField

@synthesize delegateProxy = delegateProxy_;
@synthesize selectionPollingTimer = selectionPollingTimer_;
@synthesize lastSelectionPosition = lastSelectionPosition_;
@synthesize lastSelectionLength = lastSelectionLength_;
@synthesize viewController;
@synthesize shouldSetCustomFontOnKeyboardChange;
@synthesize isInputClickSoundEnabled;
@synthesize shouldUpdateKMText;

#pragma mark - Object Admin

- (void)dealloc {
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[self.selectionPollingTimer invalidate];
	self.delegate = nil;
}

- (id)init {
    return [self initWithFrame:CGRectZero];
}

- (id)initWithFrame:(CGRect)frame {
	shouldSetCustomFontOnKeyboardChange = YES;
    isInputClickSoundEnabled = YES;
    if (self = [super initWithFrame:frame]) {
		self.delegateProxy = [[KMTextFieldDelegateProxy alloc] init];
		self.delegate = self.delegateProxy;

        if ([self respondsToSelector:@selector(inputAssistantItem)]) {
            UITextInputAssistantItem *inputAssistantItem = [self inputAssistantItem];
            inputAssistantItem.leadingBarButtonGroups = @[];
            inputAssistantItem.trailingBarButtonGroups = @[];
        }
        
		[KMManager sharedInstance]; // Preload webview keyboard
		
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(textfieldTextDidChange:) name:UITextFieldTextDidChangeNotification object:self];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
		
        self.lastSelectionPosition = 0;
		self.lastSelectionLength = 0;

		NSString *minVersionString = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"MinimumOSVersion"];
		if ([minVersionString hasPrefix:@"4"] && [KMManager sharedInstance].debugPrintingOn)
            [[KMManager sharedInstance] KMLog:@"KMTextField requires iOS 5.0 onwards. Try KMTextView instead or change your deployment target." checkDebugPrinting:NO];
    }
    
    return self;
}

#pragma mark - Class Overrides

- (UIView *)inputView {
	[KMManager sharedInstance].webDelegate = self;
	return [KMManager inputView];
}

// Prevent the delegate being set to anything but self
- (void)setDelegate:(id<UITextFieldDelegate>)delegate {
	// Allow clearing of delegate
	if (!delegate) {
        [super setDelegate:nil];
    } else {
        if (delegate != self.delegateProxy)
            [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextField: 0x%x  Trying to set KMTextField's delegate directly. Use -[setKeymanDelegate:] instead.", (uint)self] checkDebugPrinting:YES];
        [super setDelegate:self.delegateProxy];
    }
}

#pragma mark - Public Methods

- (void)setKeymanDelegate:(id<KMTextFieldDelegate>)keymanDelegate {
	self.delegateProxy.keymanDelegate = keymanDelegate;
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextField: 0x%x  keymanDelegate set to: %@", (uint)self, keymanDelegate] checkDebugPrinting:YES];
}

- (void)dismissKeyboard {
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextField: 0x%x  Dismissing keyboard. Was first responder:%@", (uint)self, [self isFirstResponder]?@"YES":@"NO"] checkDebugPrinting:YES];
    
	[self resignFirstResponder];
    [[KMManager inputView] endEditing:YES];
}

- (void)setText:(NSString *)text {
    if (text == nil)
        [super setText:@""];
    else
        [super setText:text];
    
	[KMManager setKMText:self.text];
	UITextRange *textRange = self.selectedTextRange;
	NSRange newRange = NSMakeRange([self offsetFromPosition:[self beginningOfDocument] toPosition:textRange.start], [self offsetFromPosition:textRange.start toPosition:textRange.end]);
	[KMManager setKMSelectionRange:newRange manually:NO];
}

#pragma mark - KMWebViewDelegate

- (void)updatedFragment:(NSString *)fragment {
    if (fragment && [fragment rangeOfString:@"insertText"].location != NSNotFound) {
        if ([[KMManager sharedInstance] isSubKeysMenuVisible])
            return;
        
        if (isInputClickSoundEnabled) {
            AudioServicesPlaySystemSound(0x450);
            isInputClickSoundEnabled = NO; // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
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
        
        NSMutableString *mText = nil;
        NSArray *unicodes = nil;
        if ([s length])
            unicodes = [s componentsSeparatedByString: @","];
        NSScanner *scanner = nil;
        for(int i=0; i<[unicodes count]; i++) {
            scanner = [NSScanner scannerWithString:[unicodes objectAtIndex:i]];
            unsigned int unicode;
            [scanner scanHexInt:&unicode];
            if(mText == nil)
                mText = [NSMutableString stringWithFormat:@"%C", (unsigned short) unicode];
            else
                [mText appendString:[NSString stringWithFormat:@"%C", (unsigned short) unicode]];
        }
        
        if (mText == nil)
            mText = [NSMutableString stringWithString:@""];
        NSString *text = [NSString stringWithString:mText];
        mText = nil;
        
        UITextRange *textRange = self.selectedTextRange;
        NSRange selRange = NSMakeRange([self offsetFromPosition:[self beginningOfDocument] toPosition:textRange.start], [self offsetFromPosition:textRange.start toPosition:textRange.end]);
        if (dn <= 0) {
            if(selRange.length == 0)
                [self insertText:text];
            else {
                NSString *t = self.text;
                NSString *r = [t stringByReplacingCharactersInRange:selRange withString:text];
                self.text = r;
            }
        }
        else {
            if (![s length]) {
                for (int i=0;i<dn;i++)
                    [self deleteBackward];
            }
            else {
                if(selRange.length == 0) {
                    for (int i=0;i<dn;i++)
                        [self deleteBackward];
                    [self insertText:text];
                }
                else {
                    NSString *t = self.text;
                    NSString *r = [t stringByReplacingCharactersInRange:selRange withString:text];
                    self.text = r;
                }
            }
        }
	}
    else if (fragment && [fragment rangeOfString:@"hideKeyboard"].location != NSNotFound) {
        [self dismissKeyboard];
	}
    else if (fragment && [fragment rangeOfString:@"menuKeyUp"].location != NSNotFound) {
        if (viewController != nil)
            [[KMManager sharedInstance] showKeyboardPickerInViewController:viewController shouldAddKeyboard:NO];
        else
            [[KMManager sharedInstance] switchToNextKeyboard];
	}
}

#pragma mark - UITextFieldDelegate Hooks

- (void)setSelectedTextRange:(UITextRange *)selectedTextRange {
    [super setSelectedTextRange:selectedTextRange];
    NSRange newRange = NSMakeRange([self offsetFromPosition:[self beginningOfDocument] toPosition:selectedTextRange.start], [self offsetFromPosition:selectedTextRange.start toPosition:selectedTextRange.end]);
    [KMManager setKMSelectionRange:newRange manually:NO];
}

- (BOOL)textField:(UITextField *)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString *)string
{
    shouldUpdateKMText = YES; // Enable text update to catch copy/paste operations
    return YES;
}

- (void)textfieldTextDidChange:(NSNotification *)notification {
    if (shouldUpdateKMText) { // Catches copy/paste operations
        [KMManager setKMText:self.text];
        UITextRange *textRange = self.selectedTextRange;
        NSRange newRange = NSMakeRange([self offsetFromPosition:[self beginningOfDocument] toPosition:textRange.start], [self offsetFromPosition:textRange.start toPosition:textRange.end]);
        [KMManager setKMSelectionRange:newRange manually:NO];
        shouldUpdateKMText = NO;
    }
}

- (BOOL)textFieldShouldClear:(KMTextField *)textField {
	if (textField == self)
		[KMManager clearText];
    
	return YES;
}

- (BOOL)textFieldShouldBeginEditing:(UITextField *)textField {
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion < 7)
        return YES;
    
    NSString *lrm = @"\u200e";
    NSString *rlm = @"\u202e";
    NSString *keyboardID = [KMManager sharedInstance].keyboardID;
    NSString *languageID = [KMManager sharedInstance].languageID;
    UITextWritingDirection textWD = [self baseWritingDirectionForPosition:[self beginningOfDocument] inDirection:UITextStorageDirectionForward];
    if ([[KMManager sharedInstance] isRTLKeyboardWithID:keyboardID languageID:languageID]) {
        if (textWD != UITextWritingDirectionRightToLeft) {
            if ([self.text rangeOfString:lrm].location == 0) {
                NSMutableString *mText = [self.text mutableCopy];
                [mText deleteCharactersInRange:NSMakeRange(0, 1)];
                [self setText:[NSString stringWithString:mText]];
                mText = nil;
            }
            
            if (![self.text length] || [self.text rangeOfString:rlm].location != 0)
                [self setText:[NSString stringWithFormat:@"%@%@", rlm, self.text]];
            [self makeTextWritingDirectionRightToLeft:nil];
        }
    }
    else {
        if (textWD != UITextWritingDirectionLeftToRight) {
            if ([self.text rangeOfString:rlm].location == 0) {
                NSMutableString *mText = [self.text mutableCopy];
                [mText deleteCharactersInRange:NSMakeRange(0, 1)];
                [self setText:[NSString stringWithString:mText]];
                mText = nil;
            }
            
            if (![self.text length] || [self.text rangeOfString:lrm].location != 0)
                [self setText:[NSString stringWithFormat:@"%@%@", lrm, self.text]];
            [self makeTextWritingDirectionLeftToRight:nil];
        }
    }
    
    return YES;
}

- (void)textFieldDidBeginEditing:(KMTextField *)textField {
	[KMManager sharedInstance].webDelegate = self;
    
    NSString *keyboardID = [[KMManager sharedInstance] keyboardID];
    NSString *languageID = [[KMManager sharedInstance] languageID];
    NSString *fontName = [[KMManager sharedInstance] fontNameForKeyboardWithID:keyboardID languageID:languageID];
    CGFloat fontSize = [self.font pointSize];
    if (fontName != nil)
        [self setFont:[UIFont fontWithName:fontName size:fontSize]];
    else
        [self setFont:[UIFont systemFontOfSize:fontSize]];
    
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"TextField setFont: %@", [self font].familyName] checkDebugPrinting:YES];
    
	// copy this textField's text to the webview
	[KMManager setKMText:self.text];
	UITextRange *textRange = self.selectedTextRange;
	NSRange newRange = NSMakeRange([self offsetFromPosition:[self beginningOfDocument] toPosition:textRange.start], [self offsetFromPosition:textRange.start toPosition:textRange.end]);
	[KMManager setKMSelectionRange:newRange manually:NO];
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextField: 0x%x  Became first responder. Value: %@", (uint)self, [self.text length]?self.text:@"<blank>"] checkDebugPrinting:YES];
}

- (BOOL)textFieldShouldEndEditing:(KMTextField *)textField {
	if (textField == self)
		[self resignFirstResponder];
    
	return YES;
}

#pragma mark - Keyman notifications

- (void)keyboardChanged:(NSNotification *)notification {
    if (!self.shouldSetCustomFontOnKeyboardChange)
        return;
    
    NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
    NSString *keyboardID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
    NSString *languageID = [kbInfo objectForKey:kKeymanLanguageIdKey];
    NSString *fontName = [[KMManager sharedInstance] fontNameForKeyboardWithID:keyboardID languageID:languageID];
    CGFloat fontSize = [self.font pointSize];
    if (fontName != nil)
        [self setFont:[UIFont fontWithName:fontName size:fontSize]];
    else
        [self setFont:[UIFont systemFontOfSize:fontSize]];
    
    if ([self isFirstResponder]) {
        [self resignFirstResponder];
        [self becomeFirstResponder];
    }
    
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"TextField setFont: %@", [self font].familyName] checkDebugPrinting:YES];
}

#pragma mark - Private Methods

- (void)enableInputClickSound {
    isInputClickSoundEnabled = YES;
}

@end
