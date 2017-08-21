//
//  MainViewController.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 1/10/12.
//  Updated by Serkan Kurt on 17/05/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "MainViewController.h"
#import "KMManager.h"
#import "KMKeyboardPickerBarButtonItem.h"
#import "KMKeyboardPickerButton.h"
#import <QuartzCore/QuartzCore.h>

@implementation MainViewController

@synthesize textView1 = textView1_;
@synthesize textView2 = textView2_;
@synthesize textView3 = textView3_;

#pragma mark - Object Admin

- (void)dealloc {
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
	if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil]) {
		
		self.title = @"Keyman Demo";
		//[[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"gwunicode_amharic" ofType:@"js"] shouldOverwrite:YES];
        //[[KMManager sharedInstance] preloadFontFileAtPath:[[NSBundle mainBundle] pathForResource:@"gs_geezscript" ofType:@"ttf"] shouldOverwrite:YES];
        //[[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"european2-1.0" ofType:@"js"] shouldOverwrite:YES];
        //[[KMManager sharedInstance] preloadFontFileAtPath:[[NSBundle mainBundle] pathForResource:@"DejaVuSans" ofType:@"ttf"] shouldOverwrite:YES];
        //[[KMManager sharedInstance] registerCustomFonts];
        //[[KMManager sharedInstance] addKeyboardWithID:@"gwunicode_amharic" languageID:@"amh" keyboardName:@"Unicode" languageName:@"Amharic" font:@"gs_geezscript.ttf"];
        //[[KMManager sharedInstance] addKeyboardWithID:@"european2" languageID:@"eng" keyboardName:@"EuroLatin" languageName:@"English" keyboardVersion:@"1.0" font:@"DejaVuSans.ttf"];
        
        /*
        [[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"inuktitut_pirurvik-1.0" ofType:@"js"] shouldOverwrite:YES];
        [[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"inuktitut_naqittaut-1.0" ofType:@"js"] shouldOverwrite:YES];
        [[KMManager sharedInstance] preloadLanguageFileAtPath:[[NSBundle mainBundle] pathForResource:@"inuktitut_latin-1.0" ofType:@"js"] shouldOverwrite:YES];
        
        [[NSUserDefaults standardUserDefaults] removeObjectForKey:kKeymanUserKeyboardsListKey];
        [[KMManager sharedInstance] addKeyboardWithID:@"inuktitut_pirurvik" languageID:@"ike" keyboardName:@"Pirurvik" languageName:@"Inuktitut" isRTL:NO isCustom:YES font:nil];
        [[KMManager sharedInstance] addKeyboardWithID:@"inuktitut_naqittaut" languageID:@"ike" keyboardName:@"Naqittaut" languageName:@"Inuktitut" isRTL:NO isCustom:YES font:nil];
        [[KMManager sharedInstance] addKeyboardWithID:@"inuktitut_latin" languageID:@"ike" keyboardName:@"Latin" languageName:@"Inuktitut" isRTL:NO isCustom:YES font:nil];
        */
        
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(resizeViewForKeyboardNotification:) name:UIKeyboardWillShowNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(resizeViewForKeyboardNotification:) name:UIKeyboardWillHideNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadStarted:) name:kKeymanKeyboardDownloadStartedNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloaded:) name:kKeymanKeyboardDownloadCompletedNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardDownloadFailed:) name:kKeymanKeyboardDownloadFailedNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(languagesUpdated:) name:kKeymanLanguagesUpdatedNotification object:nil];
        
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardLoaded:) name:kKeymanKeyboardLoadedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardChanged:) name:kKeymanKeyboardChangedNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardPickerDismissed:) name:kKeymanKeyboardPickerDismissedNotification object:nil];
	}
	return self;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Release any cached data, images, etc that aren't in use.
}


#pragma mark - View lifecycle

- (void)viewDidLoad {
	[super viewDidLoad];
    //[[KMManager sharedInstance] fetchKeyboardsList];
	self.view.backgroundColor = [UIColor grayColor];
	
	self.navigationItem.rightBarButtonItem = [KMKeyboardPickerBarButtonItem buttonWithPresentingViewController:self];
	
	UIView *contentView = [self contentView];
	contentView.frame = self.view.bounds;
	[self.view addSubview:contentView];
}

- (void)viewDidUnload {
    [super viewDidUnload];
	[self.textView1 removeFromSuperview];
	self.textView1 = nil;
	[self.textView2 removeFromSuperview];
	self.textView2 = nil;
	[self.textView3 removeFromSuperview];
	self.textView3 = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
	if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
	    return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
	} else {
	    return YES;
	}
}


#pragma mark - Helper Methods

- (UIScrollView *)contentView {
	const CGFloat contentWidth = 320.0f; // arbitrary non-zero width for now (it will get auto-resized)
	const CGFloat margin = 10.0f;
	
	UIScrollView* contentView = [[UIScrollView alloc] initWithFrame:CGRectMake(0.0f, 0.0f, contentWidth, 1.0f)];
	
	// Labels
	UILabel *label = [[UILabel alloc] init];
	label.text = @"Keyman Text Views/Fields:";
	label.numberOfLines = 0;
	label.lineBreakMode = NSLineBreakByWordWrapping;
	label.frame = CGRectMake(margin, margin, contentWidth - margin*2.0f, 40.0f);
	label.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	label.backgroundColor = [UIColor clearColor];
	label.textColor = [UIColor whiteColor];
	[contentView addSubview:label];
	
	label = [[UILabel alloc] init];
	label.text = @"Normal Text View:";
	label.numberOfLines = 0;
	label.lineBreakMode = NSLineBreakByWordWrapping;
	label.frame = CGRectMake(margin, margin, contentWidth - margin*2.0f, 40.0f);
	label.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	label.backgroundColor = [UIColor clearColor];
	label.textColor = [UIColor whiteColor];
	[contentView addSubview:label];
	
	// Text Views/Fields
	self.textView1 = [self autoreleasedTextViewOfClass:[KMTextView class]];
//	self.textView2 = [self autoreleasedTextViewOfClass:[KMTextView class]];
	self.textView2 = [self autoreleasedTextViewOfClass:[KMTextField class]];
	self.textView3 = [self autoreleasedTextViewOfClass:[UITextView class]];
	
	self.textView1.frame = CGRectMake(margin,
									  CGRectGetMaxY(label.frame) + 5.0f,
									  CGRectGetWidth(contentView.frame) - margin*2.0f,
									  80.0f);
	self.textView2.frame = CGRectMake(margin,
									  CGRectGetMaxY(self.textView1.frame) + margin,
									  CGRectGetWidth(contentView.frame) - margin*2.0f,
									  30.0f);
	
	label.frame = CGRectMake(margin,
							 CGRectGetMaxY(self.textView2.frame) + margin,
							 contentWidth - margin*2.0f,
							 40.0f);
	self.textView3.frame = CGRectMake(margin,
									  CGRectGetMaxY(label.frame) + margin,
									  contentWidth - margin*2.0f,
									  40.0f);
    
	self.textView1.viewController = self;
    self.textView2.viewController = self;
	[contentView addSubview:self.textView1];
	[contentView addSubview:self.textView2];
	[contentView addSubview:self.textView3];
	
	// Button - keyboard picker
	KMKeyboardPickerButton* kbButton = [KMKeyboardPickerButton buttonWithPresentingViewController:self];
	kbButton.center = CGPointMake(contentWidth / 2.0f, CGRectGetMaxY(self.textView3.frame) + 30.0f);
	kbButton.frame = CGRectIntegral(kbButton.frame);
	kbButton.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin;
	[contentView addSubview:kbButton];
	
	// Button - close keyboard
	UIButton* dismissButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
	[dismissButton setTitle:@"Close Keyboard" forState:UIControlStateNormal];
	[dismissButton addTarget:self action:@selector(dismissKeyboard) forControlEvents:UIControlEventTouchUpInside];
	[dismissButton sizeToFit];
	dismissButton.center = CGPointMake(contentWidth / 2.0f, CGRectGetMaxY(kbButton.frame) + 30.0f);
	dismissButton.frame = CGRectIntegral(dismissButton.frame);
	dismissButton.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin;
	[contentView addSubview:dismissButton];
	
	
	// Button - download custom keyboard
	UIButton *downloadButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
	[downloadButton setTitle:@"Download custom keyboard" forState:UIControlStateNormal];
	[downloadButton addTarget:self action:@selector(downloadButtonTapped:) forControlEvents:UIControlEventTouchUpInside];
	[downloadButton sizeToFit];
	downloadButton.center = CGPointMake(contentWidth/2.0f, CGRectGetMaxY(dismissButton.frame) + 30.0f);
	downloadButton.frame = CGRectIntegral(downloadButton.frame);
	downloadButton.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin;
	[contentView addSubview:downloadButton];
	
	// Content view setup
	contentView.frame = CGRectMake(0.0f, 0.0f, contentWidth, CGRectGetMaxY(downloadButton.frame) + margin);
	contentView.contentSize = contentView.frame.size;
	
	contentView.backgroundColor = [UIColor grayColor];
	contentView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	
	return contentView;
}

- (void)dismissKeyboard {
	[self.textView1 dismissKeyboard];
	[self.textView2 dismissKeyboard];
	[self.textView3 resignFirstResponder];
}

- (id)autoreleasedTextViewOfClass:(Class)theClass {
    id textView = [[theClass alloc] init];
    [textView setFont:[UIFont systemFontOfSize:20.0f]];
	[textView setAutoresizingMask:UIViewAutoresizingFlexibleWidth];
	[[textView layer] setCornerRadius:6.0f];
	
	if ([textView isKindOfClass:[KMTextView class]]) {
		[textView setKeymanDelegate:self];
	} else if ([textView isKindOfClass:[KMTextField class]]) {
		[textView setKeymanDelegate:self];
		[textView setPlaceholder:@"text field"];
		[textView setBorderStyle:UITextBorderStyleRoundedRect];
		[textView setClearButtonMode:UITextFieldViewModeWhileEditing];
	}
	
    return textView;
}

- (void)resizeViewForKeyboardNotification:(NSNotification *)notification {
	NSValue* endFrame = [[notification userInfo] objectForKey: UIKeyboardFrameEndUserInfoKey];
	NSNumber* duration = [[notification userInfo] objectForKey:UIKeyboardAnimationDurationUserInfoKey];
	NSNumber* curve = [[notification userInfo] objectForKey:UIKeyboardAnimationCurveUserInfoKey];
	
	static const CGFloat statusBarHeight = 20.0f;
	static const CGFloat landscapeNavBarHeight = 33.0f;
	
	if (endFrame && duration) {
		CGRect keyboardFrame = [endFrame CGRectValue];
		UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
		CGRect oldFrame = self.view.frame;
		CGRect newFrame = oldFrame;
		
		switch (orientation) {
			case UIInterfaceOrientationPortrait:
				newFrame = CGRectMake(CGRectGetMinX(oldFrame),
									  CGRectGetMinY(oldFrame),
									  CGRectGetWidth(oldFrame),
									  CGRectGetMinY(keyboardFrame) - CGRectGetMinY(self.view.superview.frame));
				break;
			case UIInterfaceOrientationLandscapeLeft:
				newFrame = CGRectMake(0.0f,
									  0.0f,
									  CGRectGetHeight(keyboardFrame),
									  CGRectGetMinX(keyboardFrame) - landscapeNavBarHeight - statusBarHeight);
				break;
			case UIInterfaceOrientationLandscapeRight:
				newFrame = CGRectMake(0.0f,
									  0.0f,
									  CGRectGetHeight(keyboardFrame),
									  [[UIApplication sharedApplication] keyWindow].frame.size.width - landscapeNavBarHeight - statusBarHeight - CGRectGetMaxX(keyboardFrame));
				break;
			case UIInterfaceOrientationPortraitUpsideDown:
			default:
				break;
		}
        
		[UIView animateWithDuration:[duration floatValue]
							  delay:0.0f
							options:[curve intValue]
						 animations:^{
							 self.view.frame = newFrame;
						 }
						 completion:nil];
	}
}

- (void)downloadButtonTapped:(UIButton *)sender {
    [self downloadCustomKeyboard];
}

#pragma mark - Responding to Keyman notifications

- (void)keyboardDownloadStarted:(NSNotification *)notification {
    NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
    NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
    NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
    NSString *kbName = [kbInfo objectForKey:kKeymanKeyboardNameKey];
    NSString *langName = [kbInfo objectForKey:kKeymanLanguageNameKey];
    if (kbID != nil && langID != nil && kbName != nil && langName != nil) {
        [self showActivityIndicator];
    }
}

- (void)keyboardDownloaded:(NSNotification *)notification {
	// This is an example of responding to a Keyman event.
	//   - for a list of all events, see KMManager.h
    
    NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
    NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
    NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
    NSString *kbName = [kbInfo objectForKey:kKeymanKeyboardNameKey];
    NSString *langName = [kbInfo objectForKey:kKeymanLanguageNameKey];
    BOOL isRTL = [[kbInfo objectForKey:kKeymanKeyboardRTLKey] isEqualToString:@"Y"]?YES:NO;
    BOOL isCustom = [[kbInfo objectForKey:kKeymanCustomKeyboardKey] isEqualToString:@"Y"]?YES:NO;
    NSString *font = [kbInfo objectForKey:kKeymanFontKey];
    NSString *oskFont = [kbInfo objectForKey:kKeymanOskFontKey];

    if (kbID != nil && langID != nil && kbName != nil && langName != nil) {
        [[KMManager sharedInstance] addKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName isRTL:isRTL isCustom:isCustom font:font oskFont:oskFont];
        [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
        [self performSelector:@selector(dismissActivityIndicator) withObject:nil afterDelay:1.0];
    }
}

- (void)keyboardDownloadFailed:(NSNotification *)notification {
    NSDictionary *kbInfo = [[notification userInfo] objectForKey:kKeymanKeyboardInfoKey];
    NSString *kbID = [kbInfo objectForKey:kKeymanKeyboardIdKey];
    NSString *langID = [kbInfo objectForKey:kKeymanLanguageIdKey];
    NSString *kbName = [kbInfo objectForKey:kKeymanKeyboardNameKey];
    NSString *langName = [kbInfo objectForKey:kKeymanLanguageNameKey];
    if (kbID != nil && langID != nil && kbName != nil && langName != nil) {
        NSError *error = [[notification userInfo] objectForKey:NSUnderlyingErrorKey];
        if (![[error localizedDescription] isEqualToString:@"Download queue is busy"]) {
            [self performSelector:@selector(dismissActivityIndicator) withObject:nil afterDelay:1.0];
            [self performSelector:@selector(showAlert:) withObject:[error localizedDescription] afterDelay:1.1];
        }
        else
            [self showAlert:[error localizedDescription]];
    }
}

- (void)languagesUpdated:(NSNotification *)notification {
    //
}

- (void)keyboardLoaded:(NSNotification *)notification {
    //
}

- (void)keyboardChanged:(NSNotification *)notification {
    //
}

- (void)keyboardPickerDismissed:(NSNotification *)notification {
    [textView1_ becomeFirstResponder];
}

- (void)downloadCustomKeyboard {
    //[[UIApplication sharedApplication] openURL:[NSURL URLWithString:@"app-settings:General"]];
    //[[KMManager sharedInstance] downloadKeyboardFromUrl:[NSURL URLWithString:@"http://ossa.tavultesoft.local/emote/emote.json"] isDirect:YES];
    //[[KMManager sharedInstance] downloadKeyboardFromUrl:[NSURL URLWithString:@"http://r.keymanweb.com/test/emote/emote.json"] isDirect:NO];
    //[[KMManager sharedInstance] downloadKeyboardFromUrl:[NSURL URLWithString:@"http://www.keymangreek.gr/greekez-1.2.json"] isDirect:NO];
    //[[KMManager sharedInstance] downloadKeyboardFromUrl:[NSURL URLWithString:@"http://192.168.20.208/install/sinhala.json"] isDirect:YES];
    [[KMManager sharedInstance] downloadKeyboardFromUrl:[NSURL URLWithString:@"https://sites.google.com/site/hebrewsoftware/files/yiddish_zc-1.0.json"] isDirect:NO];
    //NSString *jsonStr = @"{\"options\" : {\"device\" : \"any\",\"keyboardBaseUri\" : \"http://www.keymangreek.gr/\",\"fontBaseUri\" : \"http://www.keymangreek.gr/\"},\"keyboard\" : {\"font\" : {\"family\" : \"Galatia SIL\",\"source\" : [\"GalSILR.ttf\"]},\"oskFont\" : {\"family\" : \"Galatia SIL\",\"source\" : [\"GalSILR.ttf\"]},\"languages\" : [{\"id\" : \"ell\",\"name\" : \"Greek\"}],\"id\" : \"greekez\",\"name\" : \"Polytonic automated\",\"filename\" : \"greekez-1.2.js\",\"version\" : \"1.2\",\"lastModified\" : \"2014-11-28T12:18:19.275+02:00\"}}";
    //NSString *jsonStr2 = @"{\"options\" :{ \"device\" : \"any\", \"keyboardBaseUri\" : \"https://sites.google.com/site/hebrewsoftware/files/\", \"fontBaseUri\" : \"https://sites.google.com/site/hebrewsoftware/files/\" }, \"keyboard\" : { \"font\" : {}, \"oskfont\" : {}, \"languages\" : [{ \"id\" : \"heb\", \"name\" : \"Hebrew\" }], \"id\" : \"yiddish_zc\", \"name\" : \"Yiddish-ZC\", \"filename\" : \"yiddish_zc-1.0.js\", \"version\" : \"1.0\", \"lastModified\" : \"2015-07-17T10:52:19.536-07:00\" } }";
    //NSDictionary *jsonDict = [NSJSONSerialization JSONObjectWithData:[jsonStr2 dataUsingEncoding:NSUTF8StringEncoding] options:NSJSONReadingMutableContainers error:nil];
    //[[KMManager sharedInstance] downloadKeyboardFromDictionary:jsonDict];
    //[[KMManager sharedInstance] showKeyboardPickerInViewController:self shouldAddKeyboard:YES];
}

- (void)showActivityIndicator {
    if ([[self parentViewController].view viewWithTag:-1] != nil)
         return;
    UIActivityIndicatorView *activityView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleWhiteLarge];
    UIView *containerView = [[UIView alloc] initWithFrame:CGRectInset(activityView.bounds, -10.0f, -10.0f)];
    containerView.backgroundColor = [UIColor colorWithRed:0.0 green:1.0 blue:0.0 alpha:0.75];
    containerView.layer.cornerRadius = 6.0f;
    containerView.center = self.view.center;
    containerView.tag = -1;
    containerView.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin |  UIViewAutoresizingFlexibleBottomMargin;
    
    activityView.center = CGPointMake(containerView.bounds.size.width*0.5f, containerView.bounds.size.height*0.5f);
    [activityView startAnimating];
    
    [containerView addSubview:activityView];
    [[self parentViewController].view addSubview:containerView];
}

- (void)dismissActivityIndicator {
    [[[self parentViewController].view viewWithTag:-1] removeFromSuperview];
}

- (void)showAlert:(NSString *)message {
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:[NSString stringWithFormat:@"Keyboard Download Error"]
                                                    message:message
                                                   delegate:self
                                          cancelButtonTitle:@"OK"
                                          otherButtonTitles:nil];
    [alert show];
}

@end
