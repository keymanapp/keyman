//
//  KMTextView.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 25/03/12.
//  Updated by Serkan Kurt on 17/05/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMTextView.h"
#import "KMManager+Internal.h"
#import <AudioToolbox/AudioToolbox.h>

@interface KMTextView ()
	@property (nonatomic, strong) KMTextViewDelegateProxy *delegateProxy;
    @property (nonatomic, assign) BOOL shouldUpdateKMText;
@end

@implementation KMTextView

@synthesize delegateProxy = delegateProxy_;
@synthesize viewController;
@synthesize shouldSetCustomFontOnKeyboardChange;
@synthesize isInputClickSoundEnabled;
@synthesize shouldUpdateKMText;

#pragma mark - Object Admin

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
	self.delegate = nil;
}

- (id)init {
    return [self initWithFrame:CGRectZero];
}

- (id)initWithFrame:(CGRect)frame {
	shouldSetCustomFontOnKeyboardChange = YES;
    isInputClickSoundEnabled = YES;
    if (self = [super initWithFrame:frame]) {
		self.delegateProxy = [[KMTextViewDelegateProxy alloc] init];
		self.delegate = self.delegateProxy;
        
        if ([self respondsToSelector:@selector(inputAssistantItem)]) {
            UITextInputAssistantItem *inputAssistantItem = [self inputAssistantItem];
            inputAssistantItem.leadingBarButtonGroups = @[];
            inputAssistantItem.trailingBarButtonGroups = @[];
        }
        
		[KMManager sharedInstance]; // Preload webview keyboard
        
        [self setText:@" "]; // Workaround to reset cursor position (for IOS 7 bug)
        [self setText:@""];
        
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
    }
    
    return self;
}

#pragma mark - Class Overrides

- (UIView *)inputView {
	[KMManager sharedInstance].webDelegate = self;
	return [KMManager inputView];
}

// Prevent the delegate being set to anything but self
- (void)setDelegate:(id<UITextViewDelegate>)delegate {
	// Allow clearing of delegate
	if (!delegate) {
        [super setDelegate:nil];
    } else {
        if (delegate != self.delegateProxy)
            [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextView: 0x%x  Trying to set KMTextView's delegate directly. Use -[setKeymanDelegate:] instead.", (uint)self] checkDebugPrinting:YES];
        [super setDelegate:self.delegateProxy];
    }
}

#pragma mark - Public Methods

- (void)setKeymanDelegate:(id<KMTextViewDelegate>)keymanDelegate {
	self.delegateProxy.keymanDelegate = keymanDelegate;
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextView: 0x%x  keymanDelegate set to: %@", (uint)self, keymanDelegate] checkDebugPrinting:YES];
}

- (void)dismissKeyboard {
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextView: 0x%x  Dismissing keyboard. Was first responder:%@", (uint)self, [self isFirstResponder]?@"YES":@"NO"] checkDebugPrinting:YES];
    
	[self resignFirstResponder];
    [[KMManager inputView] endEditing:YES];
}

- (void)setText:(NSString *)text {	
    if (text == nil)
        [super setText:@""];
    else
        [super setText:text];
    
    [KMManager setKMText:self.text];
    [KMManager setKMSelectionRange:self.selectedRange manually:NO];
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
        
        NSMutableString *text = nil;
        NSArray *unicodes = nil;
        if ([s length])
            unicodes = [s componentsSeparatedByString: @","];
        
        NSScanner *scanner = nil;
        for(int i=0; i<[unicodes count]; i++) {
            scanner = [NSScanner scannerWithString:[unicodes objectAtIndex:i]];
            unsigned int unicode;
            [scanner scanHexInt:&unicode];
            if(text == nil)
                text = [NSMutableString stringWithFormat:@"%C", (unsigned short) unicode];
            else
                [text appendString:[NSMutableString stringWithFormat:@"%C", (unsigned short) unicode]];
        }
        
        if (text == nil)
            text = [NSMutableString stringWithString:@""];
        
        NSRange selRange = [self selectedRange];
        if (dn <= 0) {
            if(selRange.length == 0)
                [self insertText:text];
            else
                self.text = [self.text stringByReplacingCharactersInRange:selRange withString:text];
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
                else
                    self.text = [self.text stringByReplacingCharactersInRange:selRange withString:text];
            }
        }

        // Workaround for iOS 7 UITextView scroll bug
        int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
        if (iosVersion >= 7)
            [self performSelector:@selector(scrollToShowSelection:) withObject:self afterDelay:0.1]; // Smaller delays are unreliable.
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

#pragma mark - UITextViewDelegate Hooks

- (void)setSelectedTextRange:(UITextRange *)selectedTextRange {
    [super setSelectedTextRange:selectedTextRange];
    [KMManager setKMSelectionRange:self.selectedRange manually:NO];
}

- (void)textViewDidChangeSelection:(KMTextView *)textView {
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion < 7)
        return;
        
    // Workaround for iOS 7 UITextView scroll bug
    [self scrollToCarret:textView];
    [self scrollRangeToVisible:textView.selectedRange];
}

- (BOOL)textViewShouldBeginEditing:(UITextView *)textView {
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion < 7)
        return YES;
    
    NSString *lrm = @"\u200e";
    NSString *rlm = @"\u200f"; // or @"\u202e"
    NSString *keyboardID = [KMManager sharedInstance].keyboardID;
    NSString *languageID = [KMManager sharedInstance].languageID;
    UITextWritingDirection textWD = [self baseWritingDirectionForPosition:[self beginningOfDocument] inDirection:UITextStorageDirectionForward];
    if ([[KMManager sharedInstance] isRTLKeyboardWithID:keyboardID languageID:languageID]) {
        if (textWD != UITextWritingDirectionRightToLeft) {
            if ([self.text rangeOfString:lrm].location == 0) {
                NSMutableString *mText = [self.text mutableCopy];
                [mText deleteCharactersInRange:NSMakeRange(0, 1)];
                [self setText:mText];
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
                [self setText:mText];
            }
            
            if (![self.text length] || [self.text rangeOfString:lrm].location != 0)
                [self setText:[NSString stringWithFormat:@"%@%@", lrm, self.text]];
            [self makeTextWritingDirectionLeftToRight:nil];
        }
    }

    return YES;
}

- (void)textViewDidBeginEditing:(KMTextView *)textView {
	[KMManager sharedInstance].webDelegate = self;
    NSString *keyboardID = [[KMManager sharedInstance] keyboardID];
    NSString *languageID = [[KMManager sharedInstance] languageID];
    NSString *fontName = [[KMManager sharedInstance] fontNameForKeyboardWithID:keyboardID languageID:languageID];
    CGFloat fontSize = [self.font pointSize];
    if (fontName != nil)
        [self setFont:[UIFont fontWithName:fontName size:fontSize]];
    else
        [self setFont:[UIFont systemFontOfSize:fontSize]];
    
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"TextView setFont: %@", [self font].familyName] checkDebugPrinting:YES];
    
	// copy this textView's text to the webview
	[KMManager setKMText:self.text];
    [KMManager setKMSelectionRange:self.selectedRange manually:NO];
	[[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"KMTextView: 0x%x  Became first responder. Value: %@", (uint)self, [self.text length]?self.text:@"<blank>"] checkDebugPrinting:YES];
}

- (BOOL)textView:(UITextView *)textView shouldChangeTextInRange:(NSRange)range replacementText:(NSString *)text {
    shouldUpdateKMText = YES; // Enable text update to catch copy/paste operations
    return YES;
}

// *** This method is not called in response to programmatically initiated changes on iOS6.x ***
- (void)textViewDidChange:(KMTextView *)textView {
    if (shouldUpdateKMText) { // Catches copy/paste operations
        [KMManager setKMText:textView.text];
        [KMManager setKMSelectionRange:textView.selectedRange manually:NO];
        shouldUpdateKMText = NO;
    }
}

- (BOOL)textViewShouldEndEditing:(KMTextView *)textView {
	if (textView == self)
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
    
    [[KMManager sharedInstance] KMLog:[NSString stringWithFormat:@"TextView setFont: %@", [self font].familyName] checkDebugPrinting:YES];
}

#pragma mark iOS 7 TextView Scroll bug fix

- (void)scrollToCarret:(UITextView *)textView {
    CGRect caretRect = [textView caretRectForPosition:textView.selectedTextRange.end];
    caretRect.size.height += textView.textContainerInset.bottom;
    [textView scrollRectToVisible:caretRect animated:NO];
}

- (void)scrollToShowSelection:(UITextView *)textView {
	if (textView.selectedRange.location < textView.text.length)
		return;
    
	CGPoint bottomOffset = CGPointMake(0, textView.contentSize.height - textView.bounds.size.height);
    if (bottomOffset.y < 0)
        bottomOffset.y = 0;
	[textView setContentOffset:bottomOffset animated:YES];
}

#pragma mark - Private Methods

- (void)setWritingDirection {

}

- (void)enableInputClickSound {
    isInputClickSoundEnabled = YES;
}

@end
